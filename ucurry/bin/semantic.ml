open Ast
module StringMap = Map.Make (String)

exception TypeError of string

type type_env = typ StringMap.t

let curry f a b = f (a, b)
let pair_with a b = (b, a)
let o f g x = f (g x)
let getOp default = function Some a -> a | None -> default

let findType (name : string) (env : 'a StringMap.t) =
  try StringMap.find name env
  with Not_found -> raise (TypeError ("name " ^ name ^ "unbound"))

let findFunctionType (name : string) (env : type_env) =
  match findType name env with
  | FUNCTION_TY (argty, retty) -> (argty, retty)
  | _ -> raise (TypeError ("name " ^ name ^ " is not a function type"))

let mustAvailable (env : 'a StringMap.t) (name : string) =
  match StringMap.find_opt name env with
  | Some _ -> raise (TypeError ("name " ^ name ^ " has been defined"))
  | None -> name

let bindUnique (name : string) (value : 'a) (env : 'a StringMap.t) =
  StringMap.add (mustAvailable env name) value env

let bindAll (keys : string list) (vals : 'a list) (map : 'a StringMap.t) =
  List.fold_right2 StringMap.add keys vals map

let bindAllUnique (keys : string list) (vals : 'a list) (map : 'a StringMap.t) =
  List.fold_right2 bindUnique keys vals map

let bindAllPairs (map : 'a StringMap.t) (pairs : (string * 'a) list) =
  let names, values = List.split pairs in
  bindAll names values map

let subtypeOfList tau =
  match tau with
  | LIST_TY tau1 -> tau1
  | _ -> raise (TypeError ("expected list type but got " ^ string_of_typ tau))

let rec eqType = function
  | INT_TY, INT_TY -> true
  | STRING_TY, STRING_TY -> true
  | UNIT_TY, UNIT_TY -> true
  | BOOL_TY, BOOL_TY -> true
  | LIST_TY UNIT_TY, LIST_TY _ ->
      true
      (* TODO: HACK all empty list is of type LIST UNIT, which can be matched to any LIST TYPE  *)
  | LIST_TY _, LIST_TY UNIT_TY -> true
  | LIST_TY tau1, LIST_TY tau2 -> eqType (tau1, tau2)
  | FUNCTION_TY (tau1, tau2), FUNCTION_TY (tau1', tau2') ->
      eqType (tau1, tau1') && eqType (tau2, tau2')
  | CONSTRUCTOR_TY n1, CONSTRUCTOR_TY n2 -> String.equal n1 n2
  | TUPLE_TY tys1, TUPLE_TY tys2 -> List.for_all2 (curry eqType) tys1 tys2
  | _ -> false

let rec legalPattern (ty_env : type_env) (tau : typ) (pat : pattern) =
  match pat with
  | VAR_PAT x -> [ (x, tau) ]
  | WILDCARD -> []
  | CON_PAT (name, None) ->
      let exp_tau, ret_tau = findFunctionType name ty_env in
      if eqType (tau, ret_tau) && eqType (exp_tau, UNIT_TY) then []
      else
        raise
          (TypeError
             ("invalid pattern matching for pattern " ^ string_of_pattern pat
            ^ " expected " ^ string_of_typ tau ^ " get " ^ string_of_typ ret_tau
             ))
  | CON_PAT (name, Some pats) ->
      let exp_tau, ret_tau = findFunctionType name ty_env in
      if eqType (tau, ret_tau) then
        match (exp_tau, pats) with
        | TUPLE_TY types, _ ->
            List.concat (List.map2 (legalPattern ty_env) types pats)
        | tau, [ pat ] -> legalPattern ty_env tau pat
        | _ -> raise (TypeError "illy formed type constructor")
      else raise (TypeError "ill formed type ")
  | NIL ->
      let _ = subtypeOfList tau in
      []
  | CONCELL (s1, s2) ->
      let subty = subtypeOfList tau in
      [ (s1, subty); (s2, LIST_TY subty) ]

let eqTypes = List.for_all2 (curry eqType)

let rec typ_of (ty_env : type_env) (exp : Ast.expr) =
  let rec ty = function
    | Literal l ->
        let rec lit_ty = function
          | INT _ -> INT_TY
          | STRING _ -> STRING_TY
          | BOOL _ -> BOOL_TY
          | UNIT -> UNIT_TY
          | LIST [] ->
              LIST_TY UNIT_TY
              (* TODO: decide on the type for empty list, or whether we allow for type variable *)
          | LIST (hd :: tl) -> (
              match (lit_ty hd, lit_ty (LIST tl)) with
              | tau1, LIST_TY UNIT_TY -> LIST_TY tau1
              | tau1, LIST_TY tau2 ->
                  if eqType (tau1, tau2) then LIST_TY tau2
                  else
                    raise (TypeError "list contain elements of different types")
              | _ -> raise (TypeError "cons called on non-list"))
          | TUPLE lits -> TUPLE_TY (List.map lit_ty lits)
          | INF_LIST _ -> LIST_TY INT_TY
          | Construct (name, value) ->
              let exp_tau, ret_tau = findFunctionType name ty_env in
              if eqType (exp_tau, ty (Literal value)) then ret_tau
              else raise (TypeError "type error in construct")
        in
        lit_ty l
    | Var x -> findType x ty_env
    | Assign (x, e) ->
        let var_ty = findType x ty_env in
        if eqType (var_ty, ty e) then var_ty
        else raise (TypeError "assigned unmatched type")
    | Apply (e, es) -> (
        match (ty e, List.map ty es) with
        | FUNCTION_TY (tau1, tau2), types ->
            let rec matchfun tau types =
              match (tau, types) with
              | _, [] -> tau
              | FUNCTION_TY (tau1, tau2), hd :: tl ->
                  if eqType (tau1, hd) then matchfun tau2 tl
                  else raise (TypeError "argument type not matched")
              | _ -> raise (TypeError "apply non-function")
            in
            matchfun (FUNCTION_TY (tau1, tau2)) types
        | _, _ -> raise (TypeError "applied a non-function"))
    | If (e1, e2, e3) -> (
        match (ty e1, ty e2, ty e3) with
        | BOOL_TY, tau1, tau2 ->
            if eqType (tau1, tau2) then tau1
            else raise (TypeError "if branches contain different types")
        | _ -> raise (TypeError "if condition contains non-boolean types"))
    | Let (bindings, e) ->
        let vars, es = List.split bindings in
        let types, names = List.split vars in
        let newEnv = bindAll names types ty_env in
        let sameTypes = eqTypes types (List.map ty es) in
        if sameTypes then typ_of newEnv e
        else raise (TypeError "binding types are not annotated correctly")
    | Begin [] -> UNIT_TY
    | Begin es ->
        let types = List.map ty es in
        o List.hd List.rev types
    | Binop (e1, b, e2) as exp -> (
        let tau1 = ty e1 in
        let tau2 = ty e2 in
        let same = eqType (tau1, tau2) in
        match b with
        | (Add | Sub | Mult | Div | Mod) when same && eqType (tau1, INT_TY) ->
            INT_TY
        | (Geq | Less | Leq | Greater) when same && eqType (tau1, INT_TY) ->
            BOOL_TY
        | (And | Or) when same && eqType (tau1, BOOL_TY) -> BOOL_TY
        | (Equal | Neq) when same -> BOOL_TY
        | Cons when eqType (LIST_TY tau1, tau2) -> LIST_TY tau1
        | _ ->
            raise (TypeError ("type error in expression " ^ string_of_expr exp))
        )
    | Unop (u, e2) -> (
        let tau1 = ty e2 in
        match (u, tau1) with
        | Neg, INT_TY -> INT_TY
        | Not, BOOL_TY -> BOOL_TY
        | Hd, LIST_TY tau1 -> tau1
        | Tl, LIST_TY tau1 -> LIST_TY tau1
        | Print, _ -> UNIT_TY
        | Println, _ -> UNIT_TY
        | _ -> raise (TypeError "type error in unoary operaion"))
    | Lambda (ty, formals, body) ->
        let rec check_lambda tau formals env =
          match (tau, formals) with
          | _, [] ->
              if eqType (tau, typ_of env body) then ty
              else raise (TypeError "lambda type unmatch")
          | FUNCTION_TY (tau1, tau2), hd :: tl ->
              check_lambda tau2 tl (StringMap.add hd tau1 env)
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda ty formals ty_env
    | Case (exp, cases) ->
        let scrutinee = ty exp in
        let patterns, es = List.split cases in
        let bindings = List.map (legalPattern ty_env scrutinee) patterns in
        let newEnvs = List.map (bindAllPairs ty_env) bindings in
        let taus = List.map2 typ_of newEnvs es in
        let allSame, tau =
          List.fold_left
            (fun (same, tau) tau' -> (same && eqType (tau, tau'), tau))
            (true, List.hd taus)
            (List.tl taus)
        in
        if allSame then tau else raise (TypeError "ill typed case expression ")
    | Noexpr -> UNIT_TY
  in
  ty exp

(* type_def : def -> type_env -> type_env *)
let rec type_def (def : Ast.def) (ty_env : type_env) =
  let ty = function
    | Function (tau, funname, args, body) ->
        let tau' =
          typ_of (bindUnique funname tau ty_env) (Lambda (tau, args, body))
        in
        if eqType (tau', tau) then bindUnique funname tau ty_env
        else raise (TypeError "invalid type in function definition")
    | Datatype (name, val_cons) ->
        let new_tau = CONSTRUCTOR_TY name in
        let vcons, argtaus_op = List.split val_cons in
        (* treat each vcon as a function name *)
        let function_taus =
          List.map
            (fun argtau_op -> FUNCTION_TY ((getOp UNIT_TY) argtau_op, new_tau))
            argtaus_op
        in
        bindAllUnique vcons function_taus ty_env
    | Variable (tau, name, e) ->
        if eqType (tau, typ_of ty_env e) then bindUnique name tau ty_env
        else raise (TypeError ("type mismatch in variable definition " ^ name))
    | Exp e ->
        let _ = typ_of ty_env e in
        ty_env
    | CheckTypeError d -> (
        try
          let _ = type_def d ty_env in
          raise (TypeError "supposed to raise type error")
        with TypeError _ -> ty_env)
  in

  ty def

let typecheck (defs : Ast.program) =
  List.fold_left (fun ty_env def -> type_def def ty_env) StringMap.empty defs

(* TODO:
   1. function type - do we support partial application?
   2. should value construct only takes in value or expression
*)
