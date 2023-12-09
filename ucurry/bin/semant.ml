open Typing
module A = Ast
module S = Sast
module U = Util
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
open SemantUtil

type semant_envs = {
  type_env : S.type_env;
  vcon_env : S.vcon_env;
  vcon_sets : S.vcon_sets;
}

let new_binds_from_legal_pat (type_env : S.type_env) (scrutinee_tau : typ)
    (pat : A.pattern) : typ StringMap.t =
  let rec get_binds tau p =
    match p with
    | A.PATS ps -> (
        match tau with
        | TUPLE_TY taus ->
            let new_envs =
              try List.map2 get_binds taus ps
              with Invalid_argument _ ->
                raise (TypeError ("illegal pattern " ^ A.string_of_pattern pat))
            and combine_unique_env =
              StringMap.union (fun _ _ _ ->
                  raise (TypeError "cannot have duplicated name in pattern"))
            in
            List.fold_left combine_unique_env StringMap.empty new_envs
        | _ ->
            raise
              (TypeError ("cannot pattern match on" ^ Ast.string_of_pattern p)))
    | A.VAR_PAT x -> StringMap.add x tau StringMap.empty
    | A.CON_PAT (name, p) ->
        let arg_tau, ret_tau = findFunctionType name type_env in
        let _ = get_checked_types tau ret_tau in
        get_binds arg_tau p
    | A.WILDCARD -> StringMap.empty
    (* | NIL ->
           let _ = subtypeOfList tau in
           StringMap.empty
       | CONCELL (s1, s2) ->
           let subty = subtypeOfList tau in
           bindAllUnique [ s1; s2 ] [ subty; LIST_TY subty ] StringMap.empty *)
  in
  get_binds scrutinee_tau pat

(* TODO: commented out to use string to compare pattern instead of index *)
(* let rec to_spattern (vcon_env : S.vcon_env) (c : A.pattern) =
   let pattern_of = to_spattern vcon_env in
   match c with
   | A.PATS ps -> S.PATTERNS (List.map (to_spattern vcon_env) ps)
   | A.VAR_PAT s -> S.VAR_PAT s
   | A.CON_PAT (name, ps) ->
       S.CON_PAT (U.mid @@ findType name vcon_env, pattern_of ps)
   | A.WILDCARD -> S.WILDCARD
   (* | A.CONCELL (x, xs) -> S.CONCELL (x, xs)
   | A.NIL -> S.NIL *) *)

let rec typ_of (vcon_env : S.vcon_env) (vcon_sets : S.vcon_sets)
    (type_env : S.type_env) (exp : Ast.expr) : typ * S.sx =
  let rec ty = function
    | A.Literal l ->
        let rec lit_ty = function
          | A.INT i -> (INT_TY, S.INT i)
          | A.STRING s -> (STRING_TY, S.STRING s)
          | A.EMPTYLIST tau -> (LIST_TY tau, S.EMPTYLIST tau)
          | A.LIST (hd, tl) ->
              let hd_tau, hd_val = lit_ty hd and tl_tau, tl_val = lit_ty tl in
              let list_tau = get_checked_types (LIST_TY hd_tau) tl_tau in
              (list_tau, S.LIST (hd_val, tl_val))
          | A.BOOL b -> (BOOL_TY, S.BOOL b)
          | A.UNIT -> (UNIT_TY, S.UNIT)
          | A.INF_LIST i -> (LIST_TY INT_TY, S.INF_LIST i)
        in
        let tau, lit = lit_ty l in
        (tau, S.SLiteral lit)
    | A.Var x -> (findType x type_env, S.SVar x)
    | A.Assign (x, e) ->
        let var_ty = findType x type_env and assign_ty, se = ty e in
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
        and cond_tau' = get_checked_types cond_tau BOOL_TY in
        ( branch_tau,
          S.SIf ((cond_tau', cond_e), (branch_tau, se1), (branch_tau, se2)) )
    | A.Let (bindings, e) ->
        let vars, es = List.split bindings in
        let taus, ses = List.split (List.map ty es) in
        let newEnv = bindAll vars taus type_env in
        let bind_ses = List.combine vars @@ List.combine taus ses in
        let body_tau, body_es = typ_of vcon_env vcon_sets newEnv e in
        (body_tau, S.SLet (bind_ses, (body_tau, body_es)))
    | A.Begin [] -> (UNIT_TY, S.SBegin [])
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
        | Neg, INT_TY -> (INT_TY, S.SUnop (u, (tau, se)))
        | Not, BOOL_TY -> (BOOL_TY, S.SUnop (u, (tau, se)))
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
        | _ -> raise (TypeError "type error in unary operaion"))
    | A.Lambda (lambda_tau, formals, body) ->
        let rec check_lambda tau fs env =
          match (tau, fs) with
          | _, [] ->
              let tau', se = typ_of vcon_env vcon_sets env body in
              let final_tau = get_checked_types tau' tau in
              (final_tau, se)
          | FUNCTION_TY (tau1, tau2), hd :: [] ->
              let new_env = StringMap.add hd tau1 env in
              let tau', se = typ_of vcon_env vcon_sets new_env body in
              let final_tau = get_checked_types tau' tau2 in
              (tau, S.SLambda ([ hd ], (final_tau, se)))
          | FUNCTION_TY (tau1, tau2), hd :: tl ->
              (* make nested lambda to conform to one-arg function form *)
              ( tau,
                S.SLambda
                  ([ hd ], check_lambda tau2 tl (StringMap.add hd tau1 env)) )
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda lambda_tau formals type_env
    | A.At (e, i) -> (
        let tau, se = ty e in
        match tau with
        | TUPLE_TY ts -> (nth ts i, S.SAt ((tau, se), i))
        | _ -> raise (TypeError "access field from non-tuple value"))
    | A.Noexpr -> (UNIT_TY, S.SNoexpr)
    | A.Case (scrutinee, cases) ->
        let tau, _ = ty scrutinee in
        let pats, _ = List.split cases in
        let _ = Patconvert.legal_pats tau pats vcon_env in
        let desugared = Patconvert.case_convert scrutinee cases vcon_env tau in
        (* print_string (A.string_of_expr exp); *)
        (* print_newline(); *)
        (* (ANY_TY, S.SNoexpr) *)
        (* TODO: placeholder *)
        ty desugared
    | A.Construct (vcon_name, arg) ->
        let dt_name, vcon_id, formal_tau = StringMap.find vcon_name vcon_env in
        let arg_tau, sarg = ty arg in
        let _ = SemantUtil.eqType arg_tau formal_tau in
        ( CONSTRUCTOR_TY dt_name,
          S.SConstruct ((vcon_id, vcon_name), (arg_tau, sarg)) )
    | A.Tuple es ->
        let ses = List.map ty es in
        let taus, _ = List.split ses in
        (TUPLE_TY taus, S.STuple ses)
    | A.Thunk body ->
        let tau, e = ty body in
        (FUNCTION_TY (UNIT_TY, tau), S.SLambda ([ "unit" ], (tau, e)))
    | A.GetTag e ->
        let tau, e' = ty e in
        ignore
          (match tau with
          | CONSTRUCTOR_TY _ -> 1
          | _ -> raise (TypeError "not a datatype"));
        (STRING_TY, S.SGetTag (tau, e'))
    | A.GetField (e, vcon_name) -> (
        let tau, e' = ty e in
        match tau with
        | CONSTRUCTOR_TY _ ->
            let _, vcon_id, formal_tau = StringMap.find vcon_name vcon_env in
            (formal_tau, S.SGetField ((tau, e'), vcon_id))
        | _ -> raise (TypeError "not a datatype"))
  in

  ty exp

let rec typ_def (def : A.def) (semant_envs : semant_envs) : S.sdef * S.type_env
    =
  let { type_env; vcon_env; vcon_sets } = semant_envs in
  let ty = function
    | A.Function (tau, funname, args, body) ->
        let new_env = bindUnique funname tau type_env in
        let tau', sx =
          typ_of vcon_env vcon_sets new_env (Lambda (tau, args, body))
        in
        let final_tau = get_checked_types tau tau' in
        let match_retrun = function
          | S.SLambda body -> (S.SFunction (final_tau, funname, body), new_env)
          | _ -> (S.SVal (funname, (final_tau, sx)), new_env)
        in
        match_retrun sx
    | A.Datatype (tau, val_cons) ->
        let vcons, argtaus = List.split val_cons in
        let func_taus =
          List.map (fun argtau -> FUNCTION_TY (argtau, tau)) argtaus
        in
        (S.SDatatype (tau, val_cons), bindAllUnique vcons func_taus type_env)
    | A.Variable (tau, name, e) ->
        let tau', se = typ_of vcon_env vcon_sets type_env e in
        let var_tau = get_checked_types tau tau' in
        (S.SVal (name, (var_tau, se)), bindUnique name tau type_env)
    | A.Exp e ->
        let tau, e' = typ_of vcon_env vcon_sets type_env e in
        (S.SExp (tau, e'), type_env)
    | A.CheckTypeError d -> (
        try
          ignore (typ_def d semant_envs);
          failwith "suppposed to raise type error "
        with TypeError _ -> (S.SExp (UNIT_TY, S.SLiteral S.UNIT), type_env))
  in
  ty def

let semant_check (defs : A.def list) : S.sprogram * S.type_env =
  let add_vcons (vcon_env, vcon_sets) (def : Ast.def) =
    match def with
    | A.Datatype (CONSTRUCTOR_TY con_name, cons) ->
        let con_names, _ = List.split cons in
        let add_vcon (name, typ) idx map =
          StringMap.add name (con_name, idx, typ) map
        in
        let new_vcon_env = U.fold_left_i add_vcon 1 vcon_env cons in
        let vcon_set =
          List.fold_left (U.flip StringSet.add) StringSet.empty con_names
        in
        ignore
          (match StringMap.find_opt con_name vcon_sets with
          | Some _ -> raise (TypeError ("name " ^ con_name ^ " has been taken"))
          | None -> ());
        let new_vcon_sets = StringMap.add con_name vcon_set vcon_sets in
        (new_vcon_env, new_vcon_sets)
    | _ -> (vcon_env, vcon_sets)
  in

  let vcon_env, vcon_sets =
    List.fold_left add_vcons (StringMap.empty, StringMap.empty) defs
  in
  let sdefs, global_env =
    List.fold_left
      (fun (sdefs, type_env) def ->
        let sdef, type_env' = typ_def def { type_env; vcon_env; vcon_sets } in
        (sdef :: sdefs, type_env'))
      ([], StringMap.empty) defs
  in
  (List.rev sdefs, global_env)
