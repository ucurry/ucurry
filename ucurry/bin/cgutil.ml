open Typing
module L = Llvm
module A = Ast
module S = Sast
module C = Cast
module StringMap = Map.Make (String)
open Util

type def = Cast.def
type literal = Sast.svalue
type type_env = typ StringMap.t

exception UNIMPLEMENTED of string

(* get the ltype for a corresponding ast type *)
let ltype_of_type (ty_map : L.lltype StringMap.t) (llmodule : L.llmodule)
    (context : L.llcontext) (ty : typ) =
  let void_ptr = L.pointer_type (L.i8_type context) in
  let rec ltype_of = function
    | INT_TY -> L.i32_type context
    | BOOL_TY -> L.i1_type context
    | STRING_TY -> L.pointer_type (L.i8_type context)
    | UNIT_TY -> L.i1_type context
    | FUNCTION_TY (_, _) as ft ->
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
    | CONSTRUCTOR_TY name -> L.pointer_type (StringMap.find name ty_map)
    | TUPLE_TY taus ->
        let taus = Array.of_list (List.map ltype_of taus) in
        L.pointer_type (L.struct_type context taus)
    | LIST_TY subtype as l -> (
        (* the lltype will be a pointer to the struct that stores a cons cell *)
        let ty_name = string_of_typ l in
        match L.type_by_name llmodule ty_name with
        | Some ty -> L.pointer_type ty
        | None ->
            let list_ty = L.named_struct_type context ty_name in
            let hd_ty = ltype_of subtype in
            let tl_ty = L.pointer_type list_ty in
            let _ = L.struct_set_body list_ty [| hd_ty; tl_ty |] false in
            L.pointer_type list_ty)
    | ANY_TY -> L.i32_type context
  in
  ltype_of ty

(* given the program return a map that maps the datatype name to its lltype *)
let build_datatypes (context : L.llcontext) (llmodule : L.llmodule)
    (program : def list) : L.lltype StringMap.t =
  let add_datatype map = function
    | Cast.Datatype (CONSTRUCTOR_TY dt_name, cons) ->
        (* Define dt struct type to allow for recursive adt *)
        let dt_struct_type = L.named_struct_type context dt_name in
        let map' = StringMap.add dt_name dt_struct_type map in
        (* Get the arg types of each value constructor *)
        let vcon_names, arg_taus = List.split cons in
        let fieldTypes =
          List.map (ltype_of_type map' llmodule context) arg_taus
        in
        let map'' =
          StringMap.add_seq
            (List.to_seq @@ (List.combine vcon_names) fieldTypes)
            map'
        in
        (* Fully define the datatype struct type: the first field is a tag *)
        let _ =
          L.struct_set_body dt_struct_type
            (list_to_arr (L.pointer_type (L.i8_type context) :: fieldTypes))
            false
        in
        map''
    | _ -> map
  in
  List.fold_left add_datatype StringMap.empty program

(* build a llvm value from a literal value  *)
let build_literal (context : L.llcontext) (string_pool : L.llvalue StringMap.t)
    (v : literal) =
  let i32_t = L.i32_type context and i1_t = L.i1_type context in
  let to_lit = function
    | S.INT i -> L.const_int i32_t i
    | S.STRING s -> StringMap.find s string_pool
    | S.BOOL b -> L.const_int i1_t (if b then 1 else 0)
    | S.INF_LIST _ -> raise (UNIMPLEMENTED "inf list")
    | S.UNIT -> L.const_int i1_t 0
  in
  to_lit v

(* return the formating string for a A.typ  *)
let ty_fmt_string ty (builder : L.llbuilder) : L.llvalue =
  let rec string_matcher = function
    | INT_TY -> "%d"
    | BOOL_TY -> "%d"
    | STRING_TY -> "%s"
    | LIST_TY subtau -> "[ " ^ string_matcher subtau ^ "..]"
    | TUPLE_TY taus ->
        "(" ^ String.concat ", " (List.map string_matcher taus) ^ ")"
    | CONSTRUCTOR_TY s -> s
    | UNIT_TY -> " "
    | FUNCTION_TY _ -> raise (Impossible "function cannot be printed")
    | ANY_TY -> "%d"
  in
  L.build_global_stringptr (string_matcher ty) "fmt" builder

let build_string_pool (program : C.program) (builder : L.llbuilder) :
    L.llvalue StringMap.t =
  let rec mk_expr_string_pool builder pool (_, sx) =
    let mk_value_string_pool (v_pool : L.llvalue StringMap.t) (v : S.svalue) =
      match v with
      | S.STRING s -> (
          match StringMap.find_opt s v_pool with
          | Some _ -> v_pool
          | None ->
              StringMap.add s
                (L.build_global_stringptr s "strlit" builder)
                v_pool)
      (* | S.LIST (hd, tl) ->
          let v_pool' = mk_value_string_pool v_pool hd in
          mk_value_string_pool v_pool' tl *)
      | S.BOOL _ -> v_pool
      (* | S.EMPTYLIST _ -> v_pool *)
      | S.INF_LIST _ -> v_pool
      | S.INT _ -> v_pool
      | S.UNIT -> v_pool
    in
    match sx with
    | C.Literal l -> mk_value_string_pool pool l
    | C.Var _ -> pool
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
    | C.Construct (_, arg) -> mk_expr_string_pool builder pool arg
    | C.Tuple ses -> List.fold_left (mk_expr_string_pool builder) pool ses
    | C.EmptyList _ -> pool
    | C.List (hd, tl) ->
        let pool' = mk_expr_string_pool builder pool hd in
        mk_expr_string_pool builder pool' tl
    | C.At (sexpr, _) -> mk_expr_string_pool builder pool sexpr
    | C.GetField (sexpr, _) -> mk_expr_string_pool builder pool sexpr
    | C.GetTag sexpr -> mk_expr_string_pool builder pool sexpr
    | C.Noexpr -> pool
    | C.Captured _ -> pool
    | C.Nomatch -> pool
  in
  let mk_defs_string_pool builder pool sdef =
    match sdef with
    | C.Datatype (_, vcons) ->
        let names, _ = List.split vcons in
        List.fold_left
          (fun pool' str ->
            StringMap.add str
              (L.build_global_stringptr str "vcon_name" builder)
              pool')
          pool names
    | C.Val (_, sexpr) -> mk_expr_string_pool builder pool sexpr
    | C.Function (_, _, ((_, se), _)) -> mk_expr_string_pool builder pool se
    | C.Exp sexpr -> mk_expr_string_pool builder pool sexpr
    | C.CheckTypeError _ -> pool
  in
  List.fold_left (mk_defs_string_pool builder) StringMap.empty program
