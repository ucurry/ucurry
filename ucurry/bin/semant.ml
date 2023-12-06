module A = Ast
module S = Sast
module U = Util
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
open SemantUtil

type semant_envs = {
  type_env : S.type_env;
  vcon_env : S.vcon_env;
      (* used to replace value constructor with
         (datatype name, index, argument type) *)
  vcon_sets : S.vcon_sets;
      (* stores the datatype and its set of value constructor
         for checking ehaustive case matching *)
}

let default_case tau =
  let rec get_value tau' =
    match tau' with
    | A.INT_TY -> S.INT 0
    | A.STRING_TY -> S.STRING "case not matched"
    | A.BOOL_TY -> S.BOOL false
    | A.LIST_TY tau ->
        S.LIST (get_value tau, S.EMPTYLIST UNIT_TY) (* TODO: not sure *)
    (* | A.TUPLE_TY taus -> S.TUPLE (List.map get_value taus) *)
    | _ -> UNIT (* HACK : when case is unmatched , should thrown exception *)
  in
  (tau, S.SLiteral (get_value tau))

let new_binds_from_legal_pat (vcon_env : S.vcon_env) (scrutinee_tau : S.typ)
    (pat : A.pattern) : Ast.typ StringMap.t =
  let rec get_binds type_env tau p =
    match p with
    (* | A.PATTERNS ps -> (
        match tau with
        | A.TUPLE_TY taus ->
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
              (TypeError ("cannot pattern match on" ^ Ast.string_of_pattern p))) *)
    | A.VAR_PAT x ->
        (* StringMap.add x tau type_env *)
        bindUnique x tau type_env
        (* StringMap.add x (A.FUNCTION_TY (A.UNIT_TY, tau)) StringMap.empty *)
    | A.CON_PAT (name, ps) ->
        let datatype_name, _, formal_taus = StringMap.find name vcon_env in
        let _ = get_checked_types tau (CONSTRUCTOR_TY datatype_name) in
        List.fold_left2
          (fun new_env t p -> get_binds new_env t p)
          type_env formal_taus ps
        (* let arg_tau, ret_tau = findFunctionType name type_env in
           let _ = get_checked_types tau ret_tau in
           get_binds arg_tau p *)
    | A.WILDCARD -> type_env
    (* | NIL ->
           let _ = subtypeOfList tau in
           StringMap.empty
       | CONCELL (s1, s2) ->
           let subty = subtypeOfList tau in
           bindAllUnique [ s1; s2 ]
             [
               A.FUNCTION_TY (A.UNIT_TY, subty);
               A.FUNCTION_TY (A.UNIT_TY, LIST_TY subty);
             ]
             StringMap.empty *)
  in
  get_binds StringMap.empty scrutinee_tau pat

let rec to_spattern (vcon_env : S.vcon_env) (c : A.pattern) =
  let pattern_of = to_spattern vcon_env in
  match c with
  (* | A.PATTERNS ps -> S.PATTERNS (List.map (to_spattern vcon_env) ps) *)
  | A.VAR_PAT s -> S.VAR_PAT s
  | A.CON_PAT (name, ps) ->
      S.CON_PAT (U.mid @@ findType name vcon_env, List.map pattern_of ps)
  | A.WILDCARD -> S.WILDCARD
(* | A.CONCELL (x, xs) -> S.CONCELL (x, xs)
   | A.NIL -> S.NIL *)

let rec typ_of (vcon_env : S.vcon_env) (vcon_sets : S.vcon_sets)
    (type_env : S.type_env) (exp : Ast.expr) : S.typ * S.sx =
  let rec ty = function
    | A.Literal l ->
        let rec lit_ty = function
          | A.INT i -> (A.INT_TY, S.INT i)
          | A.STRING s -> (A.STRING_TY, S.STRING s)
          | A.EMPTYLIST t -> (A.LIST_TY t, S.EMPTYLIST t)
          | A.LIST (hd, tl) ->
              let hd_tau, hd_val = lit_ty hd and tl_tau, tl_val = lit_ty tl in
              let list_tau = get_checked_types (A.LIST_TY hd_tau) tl_tau in
              (list_tau, S.LIST (hd_val, tl_val))
          (* | A.TUPLE xs ->
              let taus, vals = List.split (List.map lit_ty xs) in
              (A.TUPLE_TY taus, S.TUPLE vals) *)
          | A.BOOL b -> (A.BOOL_TY, S.BOOL b)
          (* | A.Construct (s, v) ->
              let exp_tau, ret_tau = findFunctionType s type_env in
              let tau, v' = lit_ty v in
              ignore (get_checked_types exp_tau tau);
              (ret_tau, S.Construct (findType s vcon_env, v')) *)
          | A.UNIT -> (A.UNIT_TY, S.UNIT)
          | A.INF_LIST i -> (A.LIST_TY A.INT_TY, S.INF_LIST i)
        in
        let tau, lit = lit_ty l in
        (tau, S.SLiteral lit)
    | A.Var x -> (findType x type_env, S.SVar x)
    | A.Assign (x, e) ->
        let var_ty = findType x type_env and assign_ty, se = ty e in
        let final_tau = get_checked_types var_ty assign_ty in
        (final_tau, S.SAssign (x, (final_tau, se)))
    | A.Apply (_, []) ->
        raise (TypeError "apply no-arg function")
        (* let ty, e' = ty e in
           ty, S.SApply ((ty, e'),[]) *)
    | A.Apply (e, [ arg ]) ->
        (* base case: type checks *)
        let ft, fe = ty e in
        let formalty, retty = get_ft ft in
        let argty, arge = ty arg in
        let final_arg_tau = get_checked_types formalty argty in
        (retty, S.SApply ((ft, fe), [ (final_arg_tau, arge) ]))
    | A.Apply (e, es) ->
        raise
          (U.Impossible
             ("can only take 1 arg received: "
             ^ string_of_int (List.length es)
             ^ "in expression" ^ A.string_of_expr e))
        (* make nested apply to conform to the one-arg apply form *)
        (* ty
           (List.fold_left
              (fun acc_apply arg -> A.Apply (acc_apply, [ arg ]))
              e es) *)
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
        let taus, ses = List.split (List.map ty es) in
        let newEnv = bindAll vars taus type_env in
        let bind_ses = List.combine vars @@ List.combine taus ses in
        let body_tau, body_es = typ_of vcon_env vcon_sets newEnv e in
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
        | _ -> raise (TypeError "type error in unary operaion"))
    | A.Lambda (lambda_tau, formals, body) ->
        let check_lambda tau fs env =
          match (tau, fs) with
          | _, [] ->
              let tau', se = typ_of vcon_env vcon_sets env body in
              let final_tau = get_checked_types tau' tau in
              (final_tau, se)
          | A.FUNCTION_TY (tau1, tau2), hd :: [] ->
              let new_env = StringMap.add hd tau1 env in
              let tau', se = typ_of vcon_env vcon_sets new_env body in
              let final_tau = get_checked_types tau' tau2 in
              (tau, S.SLambda ([ hd ], (final_tau, se)))
          | A.FUNCTION_TY (_, _), hd :: tl ->
              raise
                (TypeError
                   ("Expected 1 arg but received " ^ string_of_int
                   @@ List.length (hd :: tl)))
              (* make nested lambda to conform to one-arg function form *)
              (* ( tau,
                 S.SLambda
                   ([ hd ], check_lambda tau2 tl (StringMap.add hd tau1 env)) ) *)
          | _ -> raise (TypeError "lambda type unmatch")
        in
        check_lambda lambda_tau formals type_env
    | A.Tuple es ->
        let ses = List.map ty es in 
        let taus, _ = List.split ses in 
        (TUPLE_TY taus, S.STuple ses)
    | A.At (e, i) -> (
        let tau, se = ty e in
        match tau with
        | TUPLE_TY ts -> (nth ts i, S.SAt ((tau, se), i))
        | _ -> raise (TypeError "access field from non-tuple value"))
    | A.Thunk exp ->
        let tau, e = ty exp in
        (A.FUNCTION_TY (A.UNIT_TY, tau), S.SLambda ([ "unit" ], (tau, e)))
    | A.Noexpr -> (A.UNIT_TY, S.SNoexpr)
    | A.Construct (vcon_name, args) ->
        let datatype_name, vcon_id, formal_taus =
          StringMap.find vcon_name vcon_env
        in
        let sargs = List.map ty args in
        let arg_taus, _ = List.split sargs in
        let _ =
          List.map
            (fun (t1, t2) -> eqType t1 t2)
            (List.combine formal_taus arg_taus)
        in
        ( A.CONSTRUCTOR_TY datatype_name,
          S.SConstruct ((datatype_name, vcon_id), sargs) )
    | A.Case (scrutinee, cases) ->
        let scrutinee_sexp = ty scrutinee in
        let scrutinee_type, _ = scrutinee_sexp in
        let patterns, es = List.split cases in
        (* TODO: check if pattern matching is exhaustive *)
        (* let _ =
             Caseconvert.is_exhaustive vcon_sets type_env scrutinee_type patterns
           in *)
        let spatterns = List.map (to_spattern vcon_env) patterns in
        let case_envs =
          List.map (new_binds_from_legal_pat vcon_env scrutinee_type) patterns
        in
        let new_envs =
          List.map (StringMap.union (fun _ _ v2 -> Some v2) type_env) case_envs
        in
        let taus, case_exps =
          List.split (List.map2 (typ_of vcon_env vcon_sets) new_envs es)
        in
        let scases = List.combine spatterns case_exps in
        let final_tau =
          List.fold_left get_checked_types (List.hd taus) (List.tl taus)
        in
        let final_scases =
          List.map (fun (pat, se) -> (pat, (final_tau, se))) scases
        in
        (final_tau, S.SCase (scrutinee_sexp, final_scases))
    (* let default = default_case final_tau in
       Caseconvert.case_convert final_tau scrutinee_sexp final_scases default *)
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
        (S.SDatatype (tau, val_cons), type_env)
        (* let vcons, argtaus = List.split val_cons in
           let func_taus =
             List.map (fun argtau -> A.FUNCTION_TY (argtau, tau)) argtaus
           in
           (S.SDatatype (tau, val_cons), bindAllUnique vcons func_taus type_env) *)
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
