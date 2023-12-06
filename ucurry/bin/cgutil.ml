module L = Llvm
module A = Ast
module S = Sast
module C = Cast
module StringMap = Map.Make (String)
open Util

type def = Cast.def
type literal = Sast.svalue
type type_env = A.typ StringMap.t

exception UNIMPLEMENTED of string

(* get the ltype for a corresponding ast type *)
let ltype_of_type (ty_map : L.lltype StringMap.t) (llmodule : L.llmodule)
    (context : L.llcontext) (ty : Ast.typ) =
  let void_ptr = L.pointer_type (L.i8_type context) in
  let rec ltype_of = function
    | A.INT_TY -> L.i32_type context
    | A.BOOL_TY -> L.i1_type context
    | A.STRING_TY -> L.pointer_type (L.i8_type context)
    | A.UNIT_TY -> L.i1_type context
    | A.FUNCTION_TY (_, _) as ft ->
        (* Should return closure ptr type *)
        let formal_types = Util.get_formalty ft in
        let retty = Util.get_retty ft in
        let formal_lltypes = void_ptr :: List.map ltype_of formal_types in
        let ftype =
          L.function_type (ltype_of retty) (Array.of_list formal_lltypes)
        in
        let cl_struct_type =
          L.struct_type context [| L.pointer_type ftype; void_ptr |]
        in
        L.pointer_type cl_struct_type
    | A.CONSTRUCTOR_TY (name, _) -> L.pointer_type (StringMap.find name ty_map)
    | A.TUPLE_TY taus ->
        let taus = Array.of_list (List.map ltype_of taus) in
        L.pointer_type (L.struct_type context taus)
    | A.LIST_TY subtype as l -> (
        (* the lltype will be a pointer to the struct that stores a cons cell *)
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
(*
   let get_named_structptr_type
       (context : L.llcontext)
       (llmodule : L.llmodule)
       (map: L.lltype StringMap.t)
       (name: string)
       (field_types: A.typ list) :L.lltype StringMap.t * L.lltype =
       let struct_ty = L.named_struct_type context name in
       let map' = StringMap.add name struct_ty map in
       let field_lltypes = List.map (ltype_of_type map' llmodule context) field_types in
       let _ = L.struct_set_body struct_ty (Array.of_list field_lltypes) false in
       (map', L.pointer_type struct_ty) *)
(* 
let build_struct (context : L.llcontext) (llmodule : L.llmodule)
    (builder : L.llbuilder) (map : L.lltype StringMap.t) (name : string)
    (taus : A.typ list) (elements : L.llvalue list) : L.llvalue =
  let element_types = List.map (ltype_of_type map llmodule context) taus in
  let struct_type = L.struct_type context (Array.of_list element_types) in
  let str = L.build_malloc struct_type name builder in
  ignore
    (Util.map_i (fun v i -> Util.set_data_field v i str builder) 0 elements);
  str *)

(* given the program return a map that maps the datatype name to its lltype *)
let build_datatypes (context : L.llcontext) (llmodule : L.llmodule)
    (program : def list) : L.lltype StringMap.t =
  let add_datatype map = function
    | Cast.Datatype (CONSTRUCTOR_TY (dt_name, _), cons) -> 
        (* Define dt struct type to allow for recursive adt *)
        let dt_struct_type = L.named_struct_type context dt_name in 
        let map' = StringMap.add dt_name dt_struct_type map in 

        (* Get the arg types of each value constructor *)
        let vcon_names, arg_taus = List.split cons in 
        let fieldTypes = List.map (ltype_of_type map' llmodule context) arg_taus in 
        let map'' = StringMap.add_seq 
                      (List.to_seq @@ (List.combine vcon_names) fieldTypes) 
                      map' in 
        
        (* Fully define the datatype struct type: the first field is a tag *)
        let _ =
          L.struct_set_body dt_struct_type
            (list_to_arr (L.i32_type context :: fieldTypes))
            false
        in
        map'' 
    (* | Cast.Datatype (CONSTRUCTOR_TY datatype_name, cons) ->
        let datatype_struct_type = L.named_struct_type context datatype_name in
        let map' = StringMap.add datatype_name datatype_struct_type map in

        (* Get the type of the arguments of each value constructor *)
        let vcon_names, arg_taus = List.split cons in
        let get_field_type = function
          | [] -> L.i1_type context
          | [ t ] -> ltype_of_type map' llmodule context t
          | ts ->
              L.pointer_type
                (L.struct_type context
                   (Array.of_list
                      (List.map (ltype_of_type map' llmodule context) ts)))
        in
        let subtypesList = List.map get_field_type arg_taus in
        let newmap =
          StringMap.add_seq
            (List.to_seq @@ (List.combine vcon_names) subtypesList)
            map'
        in

        (* Define the datatype struct type *)
        let _ =
          L.struct_set_body datatype_struct_type
            (list_to_arr (L.i32_type context :: subtypesList))
            false
        in
        (* StringMap.add datatype_name newContype newmap *)
        newmap *)
    | _ -> map
  in
  List.fold_left add_datatype StringMap.empty program

(* build a llvm value from a literal value  *)
let build_literal builder (ty_map : L.lltype StringMap.t)
    (context : L.llcontext) (llmodule : L.llmodule)
    (string_pool : L.llvalue StringMap.t) (ty : Ast.typ) (v : literal) =
  let i32_t = L.i32_type context and i1_t = L.i1_type context in
  let rec to_lit ty = function
    (* | S.Construct ((con_name, i, inner_ty), value) ->
        let field_v = to_lit inner_ty value
        and con_v =
          L.build_malloc (StringMap.find con_name ty_map) con_name builder
        and tag_v = L.const_int i32_t i in
        ignore (set_data_field field_v i con_v builder);
        ignore (set_data_field tag_v 0 con_v builder);
        con_v *)
    | S.INT i -> L.const_int i32_t i
    | S.STRING s -> StringMap.find s string_pool
    | S.BOOL b -> L.const_int i1_t (if b then 1 else 0)
    | S.EMPTYLIST _ ->
        let list_ptr_ty = ltype_of_type ty_map llmodule context ty in
        L.const_null list_ptr_ty
    | S.LIST (hd, tl) ->
        let subty = Util.list_subtype ty in
        let list_ty =
          L.element_type (ltype_of_type ty_map llmodule context ty)
        in
        let hd_v = to_lit subty hd in
        let tl_ptr = to_lit ty tl in
        let list_ptr = L.build_malloc list_ty "list_ptr" builder in
        ignore (set_data_field hd_v 0 list_ptr builder);
        ignore (set_data_field tl_ptr 1 list_ptr builder);
        list_ptr
    (* | S.TUPLE vs -> (
        match ty with
        | TUPLE_TY taus ->
            let tuple_ptr_ty = ltype_of_type ty_map llmodule context ty in
            let tuple_ty = L.element_type tuple_ptr_ty in
            let tuple_ptr = L.build_malloc tuple_ty "tuple address" builder in
            let inner_values = List.map2 to_lit taus vs in
            let set_feild value index =
              Util.set_data_field value index tuple_ptr builder
            in
            ignore (Util.map_i set_feild 0 inner_values);
            tuple_ptr
        | _ -> raise (Impossible "tuple must have tuple type")) *)
    | S.INF_LIST _ -> raise (UNIMPLEMENTED "inf list")
    | S.UNIT -> L.const_int i1_t 0
  in
  to_lit ty v

(* return the formating string for a A.typ  *)
let ty_fmt_string ty (builder : L.llbuilder) : L.llvalue =
  let rec string_matcher = function
    | A.INT_TY -> "%d"
    | A.BOOL_TY -> "%d"
    | A.STRING_TY -> "%s"
    | A.LIST_TY subtau -> "[ " ^ string_matcher subtau ^ "..]"
    | A.TUPLE_TY taus ->
        "(" ^ String.concat ", " (List.map string_matcher taus) ^ ")"
    | A.CONSTRUCTOR_TY (s, _) -> s
    | A.UNIT_TY -> " "
    | A.FUNCTION_TY _ -> raise (Impossible "function cannot be printed")
  in
  L.build_global_stringptr (string_matcher ty) "fmt" builder

let build_string_pool (program : C.program) (builder : L.llbuilder) :
    L.llvalue StringMap.t =
  let rec mk_expr_string_pool builder pool (_, sx) =
    let rec mk_value_string_pool (v_pool : L.llvalue StringMap.t) (v : S.svalue)
        =
      match v with
      | S.STRING s -> (
          match StringMap.find_opt s v_pool with
          | Some _ -> v_pool
          | None ->
              StringMap.add s
                (L.build_global_stringptr s "strlit" builder)
                v_pool)
      | S.LIST (hd, tl) ->
          let v_pool' = mk_value_string_pool v_pool hd in
          mk_value_string_pool v_pool' tl
      (* | S.Construct (_, v) -> mk_value_string_pool v_pool v *)
      (* | S.TUPLE vs -> List.fold_left mk_value_string_pool v_pool vs *)
      | S.BOOL _ -> v_pool
      | S.EMPTYLIST _ -> v_pool
      | S.INF_LIST _ -> v_pool
      | S.INT _ -> v_pool
      | S.UNIT -> v_pool
    in
    match sx with
    | C.Literal l -> mk_value_string_pool pool l
    | C.Var _ -> pool
    | C.Assign (_, sexpr) -> mk_expr_string_pool builder pool sexpr
    | C.Apply (func, args) ->
        let pool' = mk_expr_string_pool builder pool func in
        List.fold_left (mk_expr_string_pool builder) pool' args
    | C.If (condition, texpr, eexpr) ->
        let pool' = mk_expr_string_pool builder pool condition in
        let pool'' = mk_expr_string_pool builder pool' texpr in
        mk_expr_string_pool builder pool'' eexpr
    | C.Let (bindings, sexpr) ->
        let _, es = List.split bindings in
        let pool' = List.fold_left (mk_expr_string_pool builder) pool es in
        mk_expr_string_pool builder pool' sexpr
    | C.Begin sexprs -> List.fold_left (mk_expr_string_pool builder) pool sexprs
    | C.Binop (operand1, _, operand2) ->
        let pool' = mk_expr_string_pool builder pool operand1 in
        mk_expr_string_pool builder pool' operand2
    | C.Unop (_, operand) -> mk_expr_string_pool builder pool operand
    | C.Closure ((_, sexpr), cap) ->
        let pool' = mk_expr_string_pool builder pool sexpr in
        let pool'' = List.fold_left (mk_expr_string_pool builder) pool' cap in
        pool''
    | C.Construct (_, arg) ->
       mk_expr_string_pool builder pool arg
    | C.Case (scrutinee, patterns) ->
        let _, es = List.split patterns in
        let pool' = mk_expr_string_pool builder pool scrutinee in
        List.fold_left (mk_expr_string_pool builder) pool' es
    | C.Tuple ses -> 
        List.fold_left (mk_expr_string_pool builder) pool ses
    | C.At (sexpr, _) -> mk_expr_string_pool builder pool sexpr
    | C.Noexpr -> pool
    | C.Captured _ -> pool
  in
  let mk_defs_string_pool builder pool sdef =
    match sdef with
    | C.Datatype _ -> pool
    | C.Val (_, sexpr) -> mk_expr_string_pool builder pool sexpr
    | C.Function (_, _, ((_, se), _)) -> mk_expr_string_pool builder pool se
    | C.Exp sexpr -> mk_expr_string_pool builder pool sexpr
    | C.CheckTypeError _ -> pool
  in
  List.fold_left (mk_defs_string_pool builder) StringMap.empty program
