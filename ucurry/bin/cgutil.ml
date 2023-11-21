module L = Llvm
module A = Ast
module S = Sast
module StringMap = Map.Make (String)
open Util

type def = Sast.sdef
type literal = Sast.svalue
type type_env = A.typ StringMap.t

exception UNIMPLEMENTED of string

(* get the ltype for a corresponding ast type *)
let rec ltype_of_type (ty_map : L.lltype StringMap.t) (llmodule : L.llmodule)
    (context : L.llcontext) (ty : Ast.typ) =
  let rec ltype_of = function
    | A.INT_TY -> L.i32_type context
    | A.BOOL_TY -> L.i1_type context
    | A.STRING_TY -> L.pointer_type (L.i8_type context)
    | A.UNIT_TY -> L.i1_type context
    | A.FUNCTION_TY (arg, ret) ->
        L.function_type (ltype_of ret) [| ltype_of arg |]
        (* confirm on the function type  *)
    | A.CONSTRUCTOR_TY name -> StringMap.find name ty_map
    | A.TUPLE_TY taus ->
        let taus =
          Array.init (List.length taus) (fun i -> ltype_of (List.nth taus i))
        in
        L.struct_type context taus
    | A.LIST_TY subtype as l -> (
        (* TODO: make sure the name of the struct type does not produce conflict *)
        let ty_name = Ast.string_of_typ l in
        match L.type_by_name llmodule ty_name with
        | Some ty -> L.pointer_type ty
        | None ->
            let list_ty = L.named_struct_type context ty_name in
            let hd_ty = ltype_of subtype in
            let tl_ty = L.pointer_type list_ty in
            let _ = L.struct_set_body list_ty [| hd_ty; tl_ty |] false in
            L.pointer_type list_ty)
  in
  ltype_of ty

let build_datatype (context : L.llcontext) (llmodule : L.llmodule)
    (map : L.lltype StringMap.t) : def -> L.lltype StringMap.t = function
  | Sast.SDatatype (CONSTRUCTOR_TY con_name, cons) ->
      let names, taus = List.split cons in
      let subtypesList = List.map (ltype_of_type map llmodule context) taus in
      let newmap =
        StringMap.add_seq (List.to_seq @@ (List.combine names) subtypesList) map
      in
      let newContype =
        L.struct_type context (list_to_arr (L.i32_type context :: subtypesList))
      in
      StringMap.add con_name newContype newmap
  | _ -> map

let rec build_literal builder (ty_map : L.lltype StringMap.t)
    (context : L.llcontext) (llmodule : L.llmodule)
    (string_pool : L.llvalue StringMap.t) (ty : Ast.typ) (v : literal) =
  let i32_t = L.i32_type context and i1_t = L.i1_type context in
  let rec to_lit ty = function
    | S.Construct ((con_name, i), value) ->
        let field_v = to_lit ty value (* TODO: ty is unknown, need to be added*)
        and con_v =
          L.build_alloca (StringMap.find con_name ty_map) con_name builder
        and tag_v = L.const_int i32_t i in
        ignore (set_data_field field_v i con_v builder);
        ignore (set_data_field tag_v 0 con_v builder);
        L.build_load con_v "temp" builder
    | S.INT i -> L.const_int i32_t i
    | S.STRING s -> StringMap.find s string_pool
    | S.BOOL b -> L.const_int i1_t (if b then 1 else 0)
    | S.EMPTYLIST ->
        let list_ptr_ty = ltype_of_type ty_map llmodule context ty in
        let list_ty = L.element_type list_ptr_ty in
        let list_ptr = L.build_alloca list_ty "empty_list_address" builder in
        let null_list = L.const_null list_ty in
        ignore (L.build_store null_list list_ptr builder);
        list_ptr
    | S.LIST (hd, tl) ->
        (* TODO: currently too many allocation *)
        let subty = Util.list_subtype ty in
        let list_ty =
          L.element_type (ltype_of_type ty_map llmodule context ty)
        in
        let hd_v = to_lit subty hd in
        let tl_ptr = to_lit ty tl in
        let list_ptr = L.build_alloca list_ty "list_ptr" builder in
        ignore (set_data_field hd_v 0 list_ptr builder);
        ignore (set_data_field tl_ptr 1 list_ptr builder);
        list_ptr
    | S.TUPLE _ -> raise (UNIMPLEMENTED "tuple literal")
    | S.INF_LIST _ -> raise (UNIMPLEMENTED "inf list")
    | S.UNIT -> L.const_int i1_t 0
  in
  to_lit ty v
