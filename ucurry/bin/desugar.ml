open Typing
module SU = SemantUtil
module U = Util
module A = Ast
module S = Sast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type semant_envs = {
  type_env : S.type_env;
  vcon_env : S.vcon_env;
  vcon_sets : S.vcon_sets;
}

let rec typ_of (vcon_env : S.vcon_env) (vcon_sets : S.vcon_sets)
    (type_env : S.type_env) (exp : Ast.expr) : typ * Ast.expr =
  let rec ty exp =
    match exp with
    | A.Literal l ->
        let lit_ty = function
          | A.INT _ -> INT_TY
          | A.STRING _ -> STRING_TY
          | A.BOOL _ -> BOOL_TY
          | A.UNIT -> UNIT_TY
        in
        (lit_ty l, exp)
    | A.Var x -> (SU.findType x type_env, exp)
    | A.Apply (e, [ arg ]) ->
        let ft, e' = ty e in
        let formalty, retty = SU.get_ft ft in
        let argty, arg' = ty arg in
        let _ = SU.get_checked_types formalty argty in
        (retty, A.Apply (e', [ arg' ]))
    | A.Apply (e, es) ->
        ty
          (List.fold_left
             (fun acc_apply arg -> A.Apply (acc_apply, [ arg ]))
             e es)
    | A.If (cond, e1, e2) ->
        let (cond_tau, cond'), (then_tau, then'), (else_tau, else') =
          (ty cond, ty e1, ty e2)
        in
        let _ = SU.get_checked_types BOOL_TY cond_tau in
        let branch_tau = SU.get_checked_types then_tau else_tau in
        (branch_tau, A.If (cond', then', else'))
    | A.Let (bindings, e) ->
        let check_e (_, ei) = ty ei in
        let vars, _ = List.split bindings in
        let taus, es' = List.split @@ List.map check_e bindings in
        let new_env = SU.bindAll vars taus type_env in
        let tau, e' = typ_of vcon_env vcon_sets new_env e in
        (tau, A.Let (List.combine vars es', e'))
    | A.Letrec (bindings, e) ->
        (* enforce the bindings are all lambdas *)
        let check_letrec_binding (env, new_es) (n, ei) =
          match ei with
          | A.Lambda (t, _, _) ->
              let new_env = StringMap.add n t env in
              let tau, e' = typ_of vcon_env vcon_sets new_env ei in
              let _ = SU.get_checked_types t tau in
              (new_env, e' :: new_es)
          | _ ->
              raise
                (SU.TypeError
                   ("Letrec binds name " ^ n ^ " to non-lambda expression"))
        in
        let vars, _ = List.split bindings in
        let final_env, new_es =
          List.fold_left check_letrec_binding (type_env, []) bindings
        in
        let tau, e' = typ_of vcon_env vcon_sets final_env e in
        (tau, A.Letrec (List.combine vars (List.rev new_es), e'))
    | A.Begin [] -> (UNIT_TY, exp)
    | A.Begin es ->
        let taus, es' = List.split (List.map ty es) in
        (List.hd (List.rev taus), A.Begin es')
    | A.Binop (e1, b, e2) ->
        let (tau1, e1'), (tau2, e2') = (ty e1, ty e2) in
        let same = SU.eqType tau1 tau2 in
        ( (match b with
          | (Add | Sub | Mult | Div | Mod) when same && SU.eqType tau1 INT_TY ->
              INT_TY
          | (Geq | Less | Leq | Greater) when same && SU.eqType tau1 INT_TY ->
              BOOL_TY
          | (And | Or) when same && SU.eqType tau1 BOOL_TY -> BOOL_TY
          | (Equal | Neq) when same -> BOOL_TY
          | _ ->
              raise
                (SU.TypeError
                   ("type error in expression " ^ A.string_of_expr exp))),
          A.Binop (e1', b, e2') )
    | A.Unop (u, e) ->
        let tau, e' = ty e in
        ( (match (u, tau) with
          | Neg, INT_TY -> INT_TY
          | Not, BOOL_TY -> BOOL_TY
          | Hd, LIST_TY tau -> tau
          | Tl, LIST_TY t -> LIST_TY t
          | Print, STRING_TY
          | Println, STRING_TY
          | Print, INT_TY
          | Println, INT_TY ->
              INT_TY
          | Print, BOOL_TY | Println, BOOL_TY -> INT_TY
          | IsNull, LIST_TY _ -> BOOL_TY
          | _ -> raise (SU.TypeError "type error in unary operaion")),
          A.Unop (u, e') )
    | A.Lambda (lambda_tau, formals, body) ->
        let rec check_lambda tau fs env =
          match (tau, fs) with
          | _, [] ->
              let tau', body' = typ_of vcon_env vcon_sets env body in
              let final_tau = SU.get_checked_types tau' tau in
              (final_tau, body')
          | FUNCTION_TY (tau1, tau2), hd :: [] ->
              let new_env = StringMap.add hd tau1 env in
              let tau', body' = typ_of vcon_env vcon_sets new_env body in
              let _ = SU.get_checked_types tau' tau2 in
              (tau, A.Lambda (tau, [ hd ], body'))
          | FUNCTION_TY (tau1, tau2), hd :: tl ->
              let tau', body =
                check_lambda tau2 tl (StringMap.add hd tau1 env)
              in
              (tau, A.Lambda (tau', [ hd ], body))
          | _ -> raise (SU.TypeError "lambda type unmatch")
        in
        check_lambda lambda_tau formals type_env
    | A.At (e, i) -> (
        let tau, e' = ty e in
        match tau with
        | TUPLE_TY ts -> (SU.nth ts i, A.At (e', i))
        | _ ->
            raise
              (SU.TypeError
                 ("access field from non-tuple value" ^ A.string_of_expr e)))
    | A.Noexpr -> (UNIT_TY, exp)
    | A.Construct (vcon_name, arg) ->
        let dt_name, _, formal_tau = SU.findType vcon_name vcon_env in
        let arg_tau, arg' = ty arg in
        let _ = SU.get_checked_types arg_tau formal_tau in
        (CONSTRUCTOR_TY dt_name, A.Construct (vcon_name, arg'))
    | A.Tuple es ->
        let taus, es' = List.split @@ List.map ty es in
        (TUPLE_TY taus, A.Tuple es')
    | A.Thunk body ->
        let tau, body' = ty body in
        (FUNCTION_TY (UNIT_TY, tau), A.Thunk body')
    | A.GetTag e -> (
        let tau, e' = ty e in
        match tau with
        | CONSTRUCTOR_TY _ -> (STRING_TY, A.GetTag e')
        | _ -> raise (SU.TypeError ("get field on  " ^ string_of_typ tau)))
    | A.GetField (e, vcon_name) -> (
        let tau, e' = ty e in
        match tau with
        | CONSTRUCTOR_TY _ ->
            let _, _, formal_tau = SU.findType vcon_name vcon_env in
            (formal_tau, A.GetField (e', vcon_name))
        | _ -> raise (SU.TypeError ""))
    | A.Case (scrutinee, cases) ->
        let tau, _ = ty scrutinee in
        let pats, _ = List.split cases in
        let _ = Patconvert.legal_pats tau pats vcon_env in
        let desugared =
          Patconvert.case_convert scrutinee cases vcon_env vcon_sets tau
        in
        ty desugared
    | A.EmptyList tau -> (LIST_TY tau, exp)
    | A.List (hd, tl) -> (
        let hd_tau, hd' = ty hd and tl_tau, tl' = ty tl in
        match tl_tau with
        | LIST_TY UNIT_TY -> (LIST_TY hd_tau, A.List (hd', tl'))
        | LIST_TY sub_tau ->
            (LIST_TY (SU.get_checked_types hd_tau sub_tau), A.List (hd', tl'))
        | _ -> raise (SU.TypeError "tail is not a list"))
    | A.NoMatch -> (ANY_TY, A.NoMatch)
  in
  ty exp

let rec typ_def (def : A.def) (semant_envs : semant_envs) : A.def * S.type_env =
  let { type_env; vcon_env; vcon_sets } = semant_envs in
  let ty = function
    | A.Function (tau, funname, args, body) ->
        let new_env = SU.bindUnique funname tau type_env in
        let tau', lambda' =
          typ_of vcon_env vcon_sets new_env (A.Lambda (tau, args, body))
        in
        let final_tau = SU.get_checked_types tau tau' in
        let match_retrun = function
          | A.Lambda (tau', formals', body) ->
              (A.Function (tau', funname, formals', body), new_env)
          | e -> (A.Variable (final_tau, funname, e), new_env)
        in
        match_retrun lambda'
    | A.Datatype _ -> (def, type_env)
    | A.Variable (tau, name, e) ->
        let tau', e' = typ_of vcon_env vcon_sets type_env e in
        let _ = SU.get_checked_types tau tau' in
        (A.Variable (tau', name, e'), SU.bindUnique name tau type_env)
    | A.Exp e ->
        let _, e' = typ_of vcon_env vcon_sets type_env e in
        (A.Exp e', type_env)
    | A.CheckTypeError d -> (
        try
          ignore (typ_def d semant_envs);
          failwith "suppposed to raise type error "
        with SU.TypeError _ -> (A.Exp (A.Literal A.UNIT), type_env))
  in
  ty def

let desugar (defs : A.program) : A.program =
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
          | Some _ ->
              raise (SU.TypeError ("name " ^ con_name ^ " has been taken"))
          | None -> ());
        let new_vcon_sets = StringMap.add con_name vcon_set vcon_sets in
        (new_vcon_env, new_vcon_sets)
    | _ -> (vcon_env, vcon_sets)
  in
  let vcon_env, vcon_sets =
    List.fold_left add_vcons (StringMap.empty, StringMap.empty) defs
  in
  let sdefs, _ =
    List.fold_left
      (fun (sdefs, type_env) def ->
        let sdef, type_env' = typ_def def { type_env; vcon_env; vcon_sets } in
        (sdef :: sdefs, type_env'))
      ([], StringMap.empty) defs
  in
  List.rev sdefs
