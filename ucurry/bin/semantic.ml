open Util
module A = Ast
module S = Sast
module StringMap = Map.Make (String)

exception TypeError of string

type typ = A.typ
type type_env = typ StringMap.t
type vcon_env = (string * int * Ast.typ) StringMap.t

let nth (l : 'a list) (n : int) =
  try List.nth l n
  with Failure _ | Invalid_argument _ ->
    raise (TypeError "access out of the bound")

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
  | A.LIST_TY tau1 -> tau1
  | _ -> raise (TypeError ("expected list type but got " ^ A.string_of_typ tau))

let rec remove_unit_tau tau1 tau2 =
  match (tau1, tau2) with
  | A.LIST_TY A.UNIT_TY, A.LIST_TY _ -> (tau2, tau2)
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> (tau1, tau1)
  | _, _ -> (tau1, tau2)

let rec eqType = function
  | A.INT_TY, A.INT_TY -> true
  | A.STRING_TY, A.STRING_TY -> true
  | A.UNIT_TY, A.UNIT_TY -> true
  | A.BOOL_TY, A.BOOL_TY -> true
  | A.LIST_TY A.UNIT_TY, A.LIST_TY _ ->
      true
      (* TODO: HACK all empty list is of type LIST UNIT, which can be matched to any LIST TYPE  *)
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> true
  | A.LIST_TY tau1, A.LIST_TY tau2 -> eqType (tau1, tau2)
  | A.FUNCTION_TY (tau1, tau2), FUNCTION_TY (tau1', tau2') ->
      eqType (tau1, tau1') && eqType (tau2, tau2')
  | A.CONSTRUCTOR_TY n1, A.CONSTRUCTOR_TY n2 -> String.equal n1 n2
  | A.TUPLE_TY tys1, A.TUPLE_TY tys2 -> List.for_all2 (curry eqType) tys1 tys2
  | _ -> false

let rec legalPattern (ty_env : type_env) (tau : typ) (pat : A.pattern) =
  match pat with
  | A.VAR_PAT x -> [ (x, tau) ] (*Did we test this case?*)
  | A.WILDCARD -> []
  | A.CON_PAT (name, []) ->
      let exp_tau, ret_tau = findFunctionType name ty_env in
      if eqType (tau, ret_tau) && eqType (exp_tau, UNIT_TY) then []
      else
        raise
          (TypeError
             ("invalid pattern matching for pattern " ^ A.string_of_pattern pat
            ^ " expected " ^ A.string_of_typ tau ^ ", got "
            ^ A.string_of_typ ret_tau))
  | A.CON_PAT (name, pats) ->
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

let rec to_svalue (vcon_map : vcon_env) (v : A.value) : S.svalue =
  let valof = to_svalue vcon_map in
  match v with
  | A.INT i -> S.INT i
  | A.EMPTYLIST -> S.EMPTYLIST
  | A.LIST (hd, tl) -> S.LIST (valof hd, valof tl)
  | A.TUPLE xs -> S.TUPLE (List.map valof xs)
  | A.STRING s -> S.STRING s
  | A.BOOL b -> S.BOOL b
  | A.Construct (s, v) -> S.Construct (findType s vcon_map, valof v)
  | A.UNIT -> S.UNIT
  | A.INF_LIST i -> S.INF_LIST i

let rec to_spattern (vcon_map : vcon_env) (c : A.pattern) =
  let pattern_of = to_spattern vcon_map in
  match c with
  | A.VAR_PAT s -> S.VAR_PAT s
  | A.CON_PAT (name, ps) ->
      S.CON_PAT (mid @@ findType name vcon_map, List.map pattern_of ps)
  | A.WILDCARD -> S.WILDCARD
  | A.CONCELL (x, xs) -> S.CONCELL (x, xs)
  | A.NIL -> S.NIL

let rec typ_of (vcon_map : vcon_env) (ty_env : type_env) (exp : Ast.expr) :
    S.sexpr * typ =
  let rec ty = function
    | A.Literal l ->
        let rec lit_ty = function
          | A.INT _ -> A.INT_TY
          | A.STRING _ -> A.STRING_TY
          | A.BOOL _ -> A.BOOL_TY
          | A.UNIT -> A.UNIT_TY
          | A.EMPTYLIST ->
              A.LIST_TY A.UNIT_TY
              (* TODO: decide on the type for empty list, or whether we allow for type variable *)
          | A.LIST (x, A.EMPTYLIST) ->
              LIST_TY (lit_ty x)
              (* TODO: decide on the type for empty list, or whether we allow for type variable *)
          | A.LIST (x, xs) -> (
              match (lit_ty x, lit_ty xs) with
              | tau, LIST_TY tau2 ->
                  if eqType (tau, tau2) then LIST_TY tau2
                  else raise (TypeError "list contain different types")
              | tau1, tau2 ->
                  raise
                    (TypeError
                       ("wrong list type " ^ A.string_of_typ tau1 ^ " "
                      ^ A.string_of_typ tau2)))
          | A.TUPLE lits -> A.TUPLE_TY (List.map lit_ty lits)
          | A.INF_LIST _ -> A.LIST_TY INT_TY
          | A.Construct (name, value) ->
              let exp_tau, ret_tau = findFunctionType name ty_env in
              if eqType (exp_tau, lit_ty value) then ret_tau
              else raise (TypeError "type error in construct")
        in
        let literal_type = lit_ty l in
        ((literal_type, S.SLiteral (to_svalue vcon_map l)), literal_type)
    | A.Var x ->
        let var_type = findType x ty_env in
        ((var_type, S.SVar x), var_type)
    | A.Assign (x, e) ->
        let var_type = findType x ty_env in
        let sexpr, expr_type = ty e in
        if eqType (var_type, expr_type) then
          ((var_type, S.SAssign (x, sexpr)), var_type)
        else raise (TypeError "assigned unmatched type")
    | A.Apply (e, es) ->
        let sexpr, expr_type = ty e in
        (* let formals, formal_types = List.split (List.map ty es) in *)
        (* removed the unit from argument *)
        let formals, formal_types =
          List.split
            (match List.map ty es with
            | (_, A.UNIT_TY) :: rest -> rest
            | formals' -> formals')
        in
        let rec matchfun = function
          | A.FUNCTION_TY (A.UNIT_TY, tau), [] -> tau
          | ret_tau, [] -> ret_tau
          | A.FUNCTION_TY (tau1, tau2), hd :: tl ->
              if eqType (tau1, hd) then matchfun (tau2, tl)
              else raise (TypeError "argument type not matched")
          | _ -> raise (TypeError "apply non-function")
        in
        let ret_ty = matchfun (expr_type, formal_types) in
        ((ret_ty, S.SApply (sexpr, formals)), ret_ty)
    | A.If (e1, e2, e3) -> (
        match (ty e1, ty e2, ty e3) with
        | (se1, BOOL_TY), (se2, tau2), (se3, tau3) ->
            let _, innerSe2 = se2
            and _, innerSe3 = se3
            and tau2', tau3' = remove_unit_tau tau2 tau3 in
            if eqType (tau2', tau3') then
              ((tau2', S.SIf (se1, (tau2', innerSe2), (tau3', innerSe3))), tau3')
            else raise (TypeError "if branches contain different types")
        | _ -> raise (TypeError "if condition contains non-boolean types"))
    | A.Let (bindings, e) ->
        let vars, es = List.split bindings in
        let dec_taus, names = List.split vars in
        let newEnv = bindAll names dec_taus ty_env in
        let sexprs, stys = List.split (List.map ty es) in
        let _, inner_e = List.split sexprs in
        let sameTypes = eqTypes dec_taus stys in
        if sameTypes then
          let sexpr, e_type = typ_of vcon_map newEnv e in
          ( ( e_type,
              S.SLet (List.combine vars (List.combine dec_taus inner_e), sexpr)
            ),
            e_type )
        else raise (TypeError "binding types are not annotated correctly")
    | A.Begin [] -> ((UNIT_TY, S.SBegin []), UNIT_TY)
    | A.Begin es ->
        let es, types = List.split (List.map ty es) in
        let final_ty = o List.hd List.rev types in
        ((final_ty, SBegin es), final_ty)
    | A.Binop (e1, b, e2) as exp -> (
        let se1, tau1 = ty e1 in
        let se2, tau2 = ty e2 in
        let same = eqType (tau1, tau2) in
        match b with
        | (Add | Sub | Mult | Div | Mod) when same && eqType (tau1, INT_TY) ->
            ((INT_TY, S.SBinop (se1, b, se2)), INT_TY)
        | (Geq | Less | Leq | Greater) when same && eqType (tau1, INT_TY) ->
            ((BOOL_TY, S.SBinop (se1, b, se2)), BOOL_TY)
        | (And | Or) when same && eqType (tau1, BOOL_TY) ->
            ((BOOL_TY, S.SBinop (se1, b, se2)), BOOL_TY)
        | (Equal | Neq) when same -> ((BOOL_TY, S.SBinop (se1, b, se2)), BOOL_TY)
        | Cons when eqType (LIST_TY tau1, tau2) ->
            let _, tl_val = se2 in
            let _, tl_tau = remove_unit_tau (LIST_TY tau1) tau2 in
            ((tl_tau, S.SBinop (se1, b, (tl_tau, tl_val))), tl_tau)
        | _ ->
            raise
              (TypeError ("type error in expression " ^ A.string_of_expr exp)))
    | A.Unop (u, e) -> (
        let se, tau = ty e in
        match (u, tau) with
        | Neg, INT_TY -> ((INT_TY, S.SUnop (u, se)), INT_TY)
        | Not, BOOL_TY -> ((BOOL_TY, S.SUnop (u, se)), BOOL_TY)
        | Hd, LIST_TY tau1 -> ((tau1, S.SUnop (u, se)), tau1)
        | Tl, LIST_TY tau1 -> ((LIST_TY tau1, S.SUnop (u, se)), LIST_TY tau1)
        | Print, STRING_TY
        | Println, STRING_TY
        | Print, INT_TY
        | Println, INT_TY ->
            ((UNIT_TY, S.SUnop (u, se)), UNIT_TY)
        | Print, BOOL_TY | Println, BOOL_TY ->
            ty
              (If
                 ( Binop (e, Equal, Literal (BOOL true)),
                   Unop (u, Literal (STRING "true")),
                   Unop (u, Literal (STRING "false")) ))
        | IsNull, LIST_TY _ -> ((BOOL_TY, S.SUnop (u, se)), BOOL_TY)
        | _ -> raise (TypeError "type error in unoary operaion"))
    | A.Lambda (ty, formals, body) ->
        let rec check_lambda tau fs env =
          match (tau, fs) with
          | A.FUNCTION_TY (A.UNIT_TY, ret_tau), [] ->
              let se, tau' = typ_of vcon_map env body in
              if eqType (ret_tau, tau') then ((ty, S.SLambda (formals, se)), ty)
              else raise (TypeError "lambda type unmatch")
          | _, [] ->
              let se, tau' = typ_of vcon_map env body in
              if eqType (tau, tau') then ((ty, S.SLambda (formals, se)), ty)
              else raise (TypeError "lambda type unmatch")
          | A.FUNCTION_TY (tau1, tau2), hd :: tl ->
              check_lambda tau2 tl (StringMap.add hd tau1 env)
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda ty formals ty_env
    | A.Case (exp, cases) ->
        let scrutinee, scrutinee_type = ty exp in
        let patterns, es = List.split cases in
        let spatterns = List.map (to_spattern vcon_map) patterns in
        let bindings = List.map (legalPattern ty_env scrutinee_type) patterns in
        let newEnvs = List.map (bindAllPairs ty_env) bindings in
        let ses, taus = List.split (List.map2 (typ_of vcon_map) newEnvs es) in
        let scases = List.combine spatterns ses in
        let allSame, ret_tau =
          List.fold_left
            (fun (same, tau) tau' -> (same && eqType (tau, tau'), tau))
            (true, List.hd taus)
            (List.tl taus)
        in
        if allSame then
          let default =
            (* (A.LIST_TY A.INT_TY, S.SLiteral (S.EMPTYLIST)) *)
            (* (A.INT_TY, S.SLiteral (S.INT 0)) *)
            ( ret_tau,
              S.SUnop
                ( A.Println,
                  (A.STRING_TY, S.SLiteral (S.STRING "pattern not matched")) )
            )
          in
          (Caseconvert.case_convert ret_tau scrutinee scases default, ret_tau)
        else raise (TypeError "ill typed case expression ")
    | A.At (e, i) -> (
        let se, tau = ty e in
        match tau with
        | TUPLE_TY ts -> ((nth ts i, S.SAt (se, i)), nth ts i)
        | _ -> raise (TypeError "access field from non tuple value"))
    | A.Noexpr -> ((UNIT_TY, S.SNoexpr), UNIT_TY)
  in
  ty exp

(* type_def : def -> type_env -> type_env *)
let rec type_def (def : Ast.def) (ty_env : type_env) (vcon_map : vcon_env) :
    S.sdef * type_env =
  let ty = function
    | A.Function (tau, funname, args, body) ->
        let new_env = bindUnique funname tau ty_env in
        let se, tau' = typ_of vcon_map new_env (Lambda (tau, args, body)) in
        if eqType (tau', tau) then (S.SFunction (funname, se), new_env)
        else raise (TypeError "invalid type in function definition")
    | A.Datatype (tau, val_cons) ->
        let vcons, argtaus = List.split val_cons in
        (* treat each vcon as a function name *)
        let function_taus =
          List.map (fun argtau -> A.FUNCTION_TY (argtau, tau)) argtaus
        in
        (S.SDatatype (tau, val_cons), bindAllUnique vcons function_taus ty_env)
    | A.Variable (tau, name, e) ->
        let (_, e), tau' = typ_of vcon_map ty_env e in
        if eqType (tau, tau') then
          (S.SVal (tau, name, (tau, e)), bindUnique name tau ty_env)
        else raise (TypeError ("type mismatch in variable definition " ^ name))
    | Exp e ->
        let se, _ = typ_of vcon_map ty_env e in
        (S.SExp se, ty_env)
    | CheckTypeError d -> (
        try
          let _ = type_def d ty_env vcon_map in
          raise (TypeError "supposed to raise type error")
        with TypeError _ -> (S.SExp (UNIT_TY, S.SNoexpr), ty_env))
  in
  ty def

let typecheck (defs : Ast.program) : S.sprogram * type_env =
  let add_vcons (vcon_env : vcon_env) (def : Ast.def) =
    match def with
    | A.Datatype (CONSTRUCTOR_TY con_name, cons) ->
        let add_vcon (name, typ) idx map =
          StringMap.add name (con_name, idx, typ) map
        in
        fold_left_i add_vcon 1 vcon_env cons
    | _ -> vcon_env
  in
  let vcon_map = List.fold_left add_vcons StringMap.empty defs in
  let sdefs, global_env =
    List.fold_left
      (fun (sdefs, ty_env) def ->
        let sdef, ty_env' = type_def def ty_env vcon_map in
        (sdef :: sdefs, ty_env'))
      ([], StringMap.empty) defs
  in
  (List.rev sdefs, global_env)
(* TODO:
   1. function type - do we support partial application?
   2. should value construct only takes in value or expression
*)

let match_fail =
  ( A.UNIT_TY,
    S.SUnop
      ( Ast.Println,
        (A.STRING_TY, S.SLiteral (S.STRING "no pattern being matched")) ) )
