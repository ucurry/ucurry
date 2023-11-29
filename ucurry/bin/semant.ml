module A = Ast
module S = Sast
module StringMap = Map.Make (String)
module U = Util

type typ = A.typ
type type_env = typ StringMap.t
type vcon_env = (string * int * typ) StringMap.t

exception TypeError of string

let default_case tau =
  let rec get_value tau' =
    match tau' with
    | A.INT_TY -> S.INT 0
    | A.STRING_TY -> S.STRING "case not matched"
    | A.BOOL_TY -> S.BOOL false
    | A.LIST_TY tau -> S.LIST (get_value tau, S.EMPTYLIST)
    | A.TUPLE_TY taus -> S.TUPLE (List.map get_value taus)
    | _ -> UNIT (* HACK : when case is unmatched , should thrown exception *)
  in
  (tau, S.SLiteral (get_value tau))

(* get the nth element from a list  *)
let nth (l : 'a list) (n : int) =
  try List.nth l n
  with Failure _ | Invalid_argument _ ->
    raise (TypeError "access out of the bound")

let get_ft = function
  | A.FUNCTION_TY (formalty, retty) -> (formalty, retty)
  | _ -> raise (TypeError "not function type")

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

(* return the final types if tau1 and tau2 can be the same, else raise type error  *)
let rec get_checked_types tau1 tau2 =
  match (tau1, tau2) with
  | A.INT_TY, A.INT_TY -> tau1
  | A.STRING_TY, A.STRING_TY -> tau1
  | A.UNIT_TY, A.UNIT_TY -> tau1
  | A.BOOL_TY, A.BOOL_TY -> tau1
  | A.LIST_TY A.UNIT_TY, A.LIST_TY _ -> tau2
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> tau1
  | A.LIST_TY tau1, A.LIST_TY tau2 ->
      let tau = get_checked_types tau1 tau2 in
      A.LIST_TY tau
  | A.FUNCTION_TY (arg_tau1, ret_tau1), A.FUNCTION_TY (arg_tau2, ret_tau2) ->
      let arg_tau = get_checked_types arg_tau1 arg_tau2
      and ret_tau = get_checked_types ret_tau1 ret_tau2 in
      A.FUNCTION_TY (arg_tau, ret_tau)
  | A.CONSTRUCTOR_TY n1, A.CONSTRUCTOR_TY n2 ->
      if String.equal n1 n2 then tau1
      else raise (TypeError "failed to check equal type")
  | A.TUPLE_TY tys1, A.TUPLE_TY tys2 ->
      let tys = List.map2 get_checked_types tys1 tys2 in
      A.TUPLE_TY tys
  | tau1, tau2 ->
      raise
        (TypeError
           ("failed to check eqaul type between " ^ A.string_of_typ tau1
          ^ " and " ^ A.string_of_typ tau2))

let rec eqType tau1 tau2 =
  match (tau1, tau2) with
  | A.INT_TY, A.INT_TY -> true
  | A.STRING_TY, A.STRING_TY -> true
  | A.UNIT_TY, A.UNIT_TY -> true
  | A.BOOL_TY, A.BOOL_TY -> true
  | A.LIST_TY A.UNIT_TY, A.LIST_TY _ ->
      true
      (* TODO: HACK all empty list is of type LIST UNIT, which can be matched to any LIST TYPE  *)
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> true
  | A.LIST_TY tau1, A.LIST_TY tau2 -> eqType tau1 tau2
  | A.FUNCTION_TY (tau1, tau2), FUNCTION_TY (tau1', tau2') ->
      eqType tau1 tau1' && eqType tau2 tau2'
  | A.CONSTRUCTOR_TY n1, A.CONSTRUCTOR_TY n2 -> String.equal n1 n2
  | A.TUPLE_TY tys1, A.TUPLE_TY tys2 -> List.for_all2 eqType tys1 tys2
  | _ -> false

let rec bs_from_legal_pat (ty_env : type_env) (tau : typ) (pat : A.pattern) :
    (string * typ) list =
  match pat with
  | A.VAR_PAT x -> [ (x, tau) ]
  | A.WILDCARD -> []
  | A.CON_PAT (name, []) ->
      let _, ret_tau = findFunctionType name ty_env in
      let _ = get_checked_types tau ret_tau in
      []
  | A.CON_PAT (name, pats) -> (
      let exp_tau, ret_tau = findFunctionType name ty_env in
      let _ = get_checked_types tau ret_tau in
      match (exp_tau, pats) with
      | TUPLE_TY types, _ ->
          List.concat (List.map2 (bs_from_legal_pat ty_env) types pats)
      | tau, [ pat ] -> bs_from_legal_pat ty_env tau pat
      | _ -> raise (TypeError "illy formed type constructor"))
  | NIL ->
      let _ = subtypeOfList tau in
      []
  | CONCELL (s1, s2) ->
      let subty = subtypeOfList tau in
      [ (s1, subty); (s2, LIST_TY subty) ]

let rec to_spattern (vcon_map : vcon_env) (c : A.pattern) =
  let pattern_of = to_spattern vcon_map in
  match c with
  | A.VAR_PAT s -> S.VAR_PAT s
  | A.CON_PAT (name, ps) ->
      S.CON_PAT (U.mid @@ findType name vcon_map, List.map pattern_of ps)
  | A.WILDCARD -> S.WILDCARD
  | A.CONCELL (x, xs) -> S.CONCELL (x, xs)
  | A.NIL -> S.NIL

let rec typ_of (vcon_map : vcon_env) (ty_env : type_env) (exp : Ast.expr) :
    typ * S.sx =
  let rec ty = function
    | A.Literal l ->
        let rec lit_ty = function
          | A.INT i -> (A.INT_TY, S.INT i)
          | A.STRING s -> (A.STRING_TY, S.STRING s)
          | A.EMPTYLIST -> (A.LIST_TY A.UNIT_TY, S.EMPTYLIST)
          | A.LIST (hd, tl) ->
              let hd_tau, hd_val = lit_ty hd and tl_tau, tl_val = lit_ty tl in
              let list_tau = get_checked_types (A.LIST_TY hd_tau) tl_tau in
              (list_tau, S.LIST (hd_val, tl_val))
          | A.TUPLE xs ->
              let taus, vals = List.split (List.map lit_ty xs) in
              (A.TUPLE_TY taus, S.TUPLE vals)
          | A.BOOL b -> (A.BOOL_TY, S.BOOL b)
          | A.Construct (s, v) ->
              let exp_tau, ret_tau = findFunctionType s ty_env in
              let tau, v' = lit_ty v in
              ignore (get_checked_types exp_tau tau);
              (ret_tau, S.Construct (findType s vcon_map, v'))
          | A.UNIT -> (A.UNIT_TY, S.UNIT)
          | A.INF_LIST i -> (A.LIST_TY A.INT_TY, S.INF_LIST i)
        in
        let tau, lit = lit_ty l in
        (tau, S.SLiteral lit)
    | A.Var x -> (findType x ty_env, S.SVar x)
    | A.Assign (x, e) ->
        let var_ty = findType x ty_env and assign_ty, se = ty e in
        let final_tau = get_checked_types var_ty assign_ty in
        (final_tau, S.SAssign (x, (final_tau, se)))
    | A.Apply (e, [ arg ]) ->
        (* base case: type checks *)
        let ft, fe = ty e in
        let formalty, retty = get_ft ft in
        let argty, arge = ty arg in
        let final_arg_tau = get_checked_types formalty argty in
        (retty, S.SApply ((ft, fe), [ (final_arg_tau, arge) ]))
    | A.Apply (e, es) ->
        (* make nested apply to conform to the one-arg apply form *)
        ty
          (List.fold_left
             (fun acc_apply arg -> A.Apply (acc_apply, [ arg ]))
             e es)
    | A.If (cond, e1, e2) ->
        let cond_tau, cond_e = ty cond
        and e1_tau, se1 = ty e1
        and e2_tau, se2 = ty e2 in
        let branch_tau = get_checked_types e1_tau e2_tau
        and cond_tau' = get_checked_types cond_tau A.BOOL_TY in
        ( branch_tau,
          S.SIf ((cond_tau', cond_e), (branch_tau, se1), (branch_tau, se2)) )
    | A.Let (bindings, e) ->
        let vars, es = List.split bindings in
        let dec_taus, names = List.split vars in
        let newEnv = bindAll names dec_taus ty_env in
        let taus, ses = List.split (List.map ty es) in
        let final_taus = List.map2 get_checked_types dec_taus taus in
        let bind_ses = List.combine vars @@ List.combine final_taus ses in
        let body_tau, body_es = typ_of vcon_map newEnv e in
        (body_tau, S.SLet (bind_ses, (body_tau, body_es)))
    | A.Begin [] -> (A.UNIT_TY, S.SBegin [])
    | A.Begin es ->
        let ses = List.map ty es in
        (U.o U.fst (U.o List.hd List.rev) ses, S.SBegin ses)
    | A.Binop (e1, b, e2) as exp -> (
        let tau1, se1 = ty e1 in
        let tau2, se2 = ty e2 in
        let same = eqType tau1 tau2 in
        match b with
        | (Add | Sub | Mult | Div | Mod) when same && eqType tau1 INT_TY ->
            (INT_TY, S.SBinop ((tau1, se1), b, (tau2, se2)))
        | (Geq | Less | Leq | Greater) when same && eqType tau1 INT_TY ->
            (BOOL_TY, S.SBinop ((tau1, se1), b, (tau2, se2)))
        | (And | Or) when same && eqType tau1 BOOL_TY ->
            (BOOL_TY, S.SBinop ((tau1, se1), b, (tau2, se2)))
        | (Equal | Neq) when same ->
            (BOOL_TY, S.SBinop ((tau1, se1), b, (tau2, se2)))
        | Cons ->
            let final_tau = get_checked_types (LIST_TY tau1) tau2 in
            (final_tau, S.SBinop ((tau1, se1), b, (final_tau, se2)))
        | _ ->
            raise
              (TypeError ("type error in expression " ^ A.string_of_expr exp)))
    | A.Unop (u, e) -> (
        let tau, se = ty e in
        match (u, tau) with
        | Neg, A.INT_TY -> (A.INT_TY, S.SUnop (u, (tau, se)))
        | Not, A.BOOL_TY -> (A.BOOL_TY, S.SUnop (u, (tau, se)))
        | Hd, LIST_TY tau1 -> (tau1, S.SUnop (u, (tau, se)))
        | Tl, LIST_TY _ -> (tau, S.SUnop (u, (tau, se)))
        | Print, STRING_TY
        | Println, STRING_TY
        | Print, INT_TY
        | Println, INT_TY ->
            (INT_TY, S.SUnop (u, (tau, se)))
        | Print, BOOL_TY | Println, BOOL_TY ->
            ty
              (If
                 ( Binop (e, Equal, Literal (BOOL true)),
                   Unop (u, Literal (STRING "true")),
                   Unop (u, Literal (STRING "false")) ))
        | IsNull, LIST_TY _ -> (BOOL_TY, S.SUnop (u, (tau, se)))
        | _ -> raise (TypeError "type error in unoary operaion"))
    | A.Lambda (lambda_tau, formals, body) ->
        let rec check_lambda tau fs env =
          match (tau, fs) with
          | _, [] ->
              let tau', se = typ_of vcon_map env body in
              let final_tau = get_checked_types tau' tau in
              (final_tau, se)
          | A.FUNCTION_TY (tau1, tau2), hd :: [] ->
              let new_env = StringMap.add hd tau1 env in
              let tau', se = typ_of vcon_map new_env body in
              let final_tau = get_checked_types tau' tau2 in
              (tau, S.SLambda ([ hd ], (final_tau, se)))
          | A.FUNCTION_TY (tau1, tau2), hd :: tl ->
              (* make nested lambda to conform to one-arg function form *)
              ( tau,
                S.SLambda
                  ([ hd ], check_lambda tau2 tl (StringMap.add hd tau1 env)) )
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda lambda_tau formals ty_env
    | A.At (e, i) -> (
        let tau, se = ty e in
        match tau with
        | TUPLE_TY ts -> (nth ts i, S.SAt ((tau, se), i))
        | _ -> raise (TypeError "access field from non-typle value"))
    | A.Noexpr -> (A.UNIT_TY, S.SNoexpr)
    | A.Case (scrutinee, cases) ->
        let scrutinee_sexp = ty scrutinee in
        let scrutinee_type, _ = scrutinee_sexp in
        let patterns, es = List.split cases in
        let spatterns = List.map (to_spattern vcon_map) patterns in
        let bindings =
          List.map (bs_from_legal_pat ty_env scrutinee_type) patterns
        in
        let new_envs = List.map (bindAllPairs ty_env) bindings in
        let taus, case_exps =
          List.split (List.map2 (typ_of vcon_map) new_envs es)
        in
        let scases = List.combine spatterns case_exps in
        let final_tau =
          List.fold_left get_checked_types (List.hd taus) (List.tl taus)
        in
        let final_scases =
          List.map (fun (pat, se) -> (pat, (final_tau, se))) scases
        in
        let default = default_case final_tau in
        Caseconvert.case_convert final_tau scrutinee_sexp final_scases default
  in
  ty exp

let rec typ_def (def : A.def) (ty_env : type_env) (vcon_map : vcon_env) :
    S.sdef * type_env =
  let ty = function
    | A.Function (tau, funname, args, body) ->
        let new_env = bindUnique funname tau ty_env in
        let tau', sx = typ_of vcon_map new_env (Lambda (tau, args, body)) in
        let final_tau = get_checked_types tau tau' in
        let match_retrun = function
          | S.SLambda body -> (S.SFunction (final_tau, funname, body), new_env)
          | _ -> (S.SVal (final_tau, funname, (final_tau, sx)), new_env)
        in
        match_retrun sx
    | A.Datatype (tau, val_cons) ->
        let vcons, argtaus = List.split val_cons in
        let func_taus =
          List.map (fun argtau -> A.FUNCTION_TY (argtau, tau)) argtaus
        in
        (S.SDatatype (tau, val_cons), bindAllUnique vcons func_taus ty_env)
    | A.Variable (tau, name, e) ->
        let tau', se = typ_of vcon_map ty_env e in
        let var_tau = get_checked_types tau tau' in
        (S.SVal (var_tau, name, (var_tau, se)), bindUnique name tau ty_env)
    | A.Exp e ->
        let tau, e' = typ_of vcon_map ty_env e in
        (S.SExp (tau, e'), ty_env)
    | A.CheckTypeError d -> (
        try
          let _ = typ_def d ty_env vcon_map in
          raise (TypeError "supposed to raise type error")
        with TypeError _ -> (S.SExp (UNIT_TY, S.SLiteral S.UNIT), ty_env))
  in
  ty def

let semant_check (defs : A.def list) : S.sprogram * type_env =
  let add_vcons (vcon_env : vcon_env) (def : Ast.def) =
    match def with
    | A.Datatype (CONSTRUCTOR_TY con_name, cons) ->
        let add_vcon (name, typ) idx map =
          StringMap.add name (con_name, idx, typ) map
        in
        U.fold_left_i add_vcon 1 vcon_env cons
    | _ -> vcon_env
  in
  let vcon_map = List.fold_left add_vcons StringMap.empty defs in
  let sdefs, global_env =
    List.fold_left
      (fun (sdefs, ty_env) def ->
        let sdef, ty_env' = typ_def def ty_env vcon_map in
        (sdef :: sdefs, ty_env'))
      ([], StringMap.empty) defs
  in
  (List.rev sdefs, global_env)
