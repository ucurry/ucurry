open Ast
module S = Sast
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
  | VAR_PAT x -> [ (x, tau) ] (*Did we test this case?*)
  | WILDCARD -> []
  | CON_PAT (name, None) ->
      let exp_tau, ret_tau = findFunctionType name ty_env in
      if eqType (tau, ret_tau) && eqType (exp_tau, UNIT_TY) then []
      else
        raise
          (TypeError
             ("invalid pattern matching for pattern " ^ string_of_pattern pat
            ^ " expected " ^ string_of_typ tau ^ ", got "
            ^ string_of_typ ret_tau))
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

let rec typ_of (ty_env : type_env) (exp : Ast.expr) : S.sexpr * typ =
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
              if eqType (exp_tau, lit_ty value) then ret_tau
              else raise (TypeError "type error in construct")
        in
        let literal_type = lit_ty l in
        (S.SLiteral (literal_type, l), literal_type)
    | Var x ->
        let var_type = findType x ty_env in
        (S.SVar (var_type, x), var_type)
    | Assign (x, e) ->
        let var_type = findType x ty_env in
        let sexpr, expr_type = ty e in
        if eqType (var_type, expr_type) then (S.SAssign (x, sexpr), var_type)
        else raise (TypeError "assigned unmatched type")
    | Apply (e, es) ->
        let sexpr, expr_type = ty e in
        let formals, formal_types = List.split (List.map ty es) in
        let rec matchfun = function
          | ret_tau, [] -> ret_tau
          | FUNCTION_TY (tau1, tau2), hd :: tl ->
              if eqType (tau1, hd) then matchfun (tau2, tl)
              else raise (TypeError "argument type not matched")
          | _ -> raise (TypeError "apply non-function")
        in
        let function_type = matchfun (expr_type, formal_types) in
        (S.SApply (sexpr, formals), function_type)
    | If (e1, e2, e3) -> (
        match (ty e1, ty e2, ty e3) with
        | (se1, BOOL_TY), (se2, tau1), (se3, tau2) ->
            if eqType (tau1, tau2) then (S.SIf (se1, se2, se3), tau1)
            else raise (TypeError "if branches contain different types")
        | _ -> raise (TypeError "if condition contains non-boolean types"))
    | Let (bindings, e) ->
        let vars, es = List.split bindings in
        let types, names = List.split vars in
        let newEnv = bindAll names types ty_env in
        let sexprs, e_types = List.split (List.map ty es) in
        let sameTypes = eqTypes types e_types in
        if sameTypes then
          let sexpr, e_type = typ_of newEnv e in
          (S.SLet (List.combine vars sexprs, sexpr), e_type)
        else raise (TypeError "binding types are not annotated correctly")
    | Begin [] -> (S.SBegin [], UNIT_TY)
    | Begin es ->
        let es, types = List.split (List.map ty es) in
        (SBegin es, o List.hd List.rev types)
    | Binop (e1, b, e2) as exp -> (
        let se1, tau1 = ty e1 in
        let se2, tau2 = ty e2 in
        let same = eqType (tau1, tau2) in
        match b with
        | (Add | Sub | Mult | Div | Mod) when same && eqType (tau1, INT_TY) ->
            (S.SBinop (INT_TY, se1, b, se2), INT_TY)
        | (Geq | Less | Leq | Greater) when same && eqType (tau1, INT_TY) ->
            (S.SBinop (BOOL_TY, se1, b, se2), BOOL_TY)
        | (And | Or) when same && eqType (tau1, BOOL_TY) ->
            (S.SBinop (BOOL_TY, se1, b, se2), BOOL_TY)
        | (Equal | Neq) when same -> (S.SBinop (BOOL_TY, se1, b, se2), BOOL_TY)
        | Cons when eqType (LIST_TY tau1, tau2) ->
            (S.SBinop (tau2, se1, b, se2), tau2)
        | _ ->
            raise (TypeError ("type error in expression " ^ string_of_expr exp))
        )
    | Unop (u, e) -> (
        let se, tau = ty e in
        match (u, tau) with
        | Neg, INT_TY -> (S.SUnop (INT_TY, u, se), INT_TY)
        | Not, BOOL_TY -> (S.SUnop (BOOL_TY, u, se), BOOL_TY)
        | Hd, LIST_TY tau1 -> (S.SUnop (tau1, u, se), tau1)
        | Tl, LIST_TY tau1 -> (S.SUnop (LIST_TY tau1, u, se), LIST_TY tau1)
        | Print, _ -> (S.SUnop (UNIT_TY, u, se), UNIT_TY)
        | Println, _ -> (S.SUnop (UNIT_TY, u, se), UNIT_TY)
        | _ -> raise (TypeError "type error in unoary operaion"))
    | Lambda (ty, formals, body) ->
        let rec check_lambda tau formals env =
          match (tau, formals) with
          | _, [] ->
              let se, tau' = typ_of env body in
              if eqType (tau, tau') then (S.SLambda (ty, formals, se), ty)
              else raise (TypeError "lambda type unmatch")
          | FUNCTION_TY (tau1, tau2), hd :: tl ->
              check_lambda tau2 tl (StringMap.add hd tau1 env)
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda ty formals ty_env
    | Case (exp, cases) ->
        let se, scrutinee_type = ty exp in
        let patterns, es = List.split cases in
        let bindings = List.map (legalPattern ty_env scrutinee_type) patterns in
        let newEnvs = List.map (bindAllPairs ty_env) bindings in
        let ses, taus = List.split (List.map2 typ_of newEnvs es) in
        let scases = List.combine patterns ses in
        let allSame, exptau =
          List.fold_left
            (fun (same, tau) tau' -> (same && eqType (tau, tau'), tau))
            (true, List.hd taus)
            (List.tl taus)
        in
        if allSame then (S.SCase (se, scases), exptau)
        else raise (TypeError "ill typed case expression ")
    | Noexpr -> (S.SNoexpr, UNIT_TY)
  in
  ty exp

(* type_def : def -> type_env -> type_env *)
let rec type_def (def : Ast.def) (ty_env : type_env) : S.sdef * type_env =
  let ty = function
    | Function (tau, funname, args, body) ->
        let se, tau' =
          typ_of (bindUnique funname tau ty_env) (Lambda (tau, args, body))
        in
        if eqType (tau', tau) then
          (S.SFunction (tau, funname, args, se), bindUnique funname tau ty_env)
        else raise (TypeError "invalid type in function definition")
    | Datatype (tau, val_cons) ->
        let vcons, argtaus_op = List.split val_cons in
        (* treat each vcon as a function name *)
        let function_taus =
          List.map
            (fun argtau_op -> FUNCTION_TY ((getOp UNIT_TY) argtau_op, tau))
            argtaus_op
        in
        (S.SDatatype (tau, val_cons), bindAllUnique vcons function_taus ty_env)
    | Variable (tau, name, e) ->
        let se, tau' = typ_of ty_env e in
        if eqType (tau, tau') then
          (S.SVariable (tau, name, se), bindUnique name tau ty_env)
        else raise (TypeError ("type mismatch in variable definition " ^ name))
    | Exp e ->
        let se, _ = typ_of ty_env e in
        (S.SExp se, ty_env)
    | CheckTypeError d -> (
        try
          let _ = type_def d ty_env in
          raise (TypeError "supposed to raise type error")
        with TypeError _ -> (S.SCheckTypeError d, ty_env))
  in
  ty def

let typecheck (defs : Ast.program) : S.sprogram =
  let sdefs, _ =
    List.fold_left
      (fun (sdefs, ty_env) def ->
        let sdef, ty_env' = type_def def ty_env in
        (sdef :: sdefs, ty_env'))
      ([], StringMap.empty) defs
  in
  sdefs
(* TODO:
   1. function type - do we support partial application?
   2. should value construct only takes in value or expression
*)
