module S = Sast
module A = Ast
module L = Last

exception LAZY_NOT_YET_IMPLEMENTED of string

let rec transform_funty args ty =
  match (args, ty) with
  | [], ty -> ty
  | _ :: xs, A.FUNCTION_TY (arg_tau, ret_tau) ->
      A.FUNCTION_TY
        (A.FUNCTION_TY (UNIT_TY, arg_tau), transform_funty xs ret_tau)
  | _ -> failwith "argument list and function type does not match"

let rec lazyExpWith ((ty, exp) : S.sexpr) : L.sexpr =
  let to_thunk ((t, _) as thunk : S.sexpr) : L.sexpr =
    (A.FUNCTION_TY (A.UNIT_TY, t), L.Lambda ([], lazyExpWith thunk))
  in
  match exp with
  | S.SLiteral l -> (ty, L.Literal l)
  | S.SVar v -> (ty, L.Apply ((FUNCTION_TY (UNIT_TY, ty), L.Var v), []))
  | S.SAssign (v, e) -> (ty, L.Assign (v, to_thunk e))
  | S.SUnop (unop, e) -> (ty, L.Unop (unop, lazyExpWith e))
  | S.SBinop (e1, binop, e2) ->
      (ty, L.Binop (lazyExpWith e1, binop, lazyExpWith e2))
  | S.SBegin es -> (ty, L.Begin (List.map lazyExpWith es))
  | S.SIf (e1, e2, e3) ->
      (ty, L.If (lazyExpWith e1, lazyExpWith e2, lazyExpWith e3))
  | S.SLet (bs, e) ->
      let vars, es = List.split bs in
      let lazy_bs = List.combine vars (List.map to_thunk es) in
      (ty, L.Let (lazy_bs, lazyExpWith e))
  | S.SCase (scrutinee, cases) ->
      let scrutinee' = lazyExpWith scrutinee in
      let ps, cs = List.split cases in
      let cs' = List.map lazyExpWith cs in
      (ty, L.Case (scrutinee', List.combine ps cs'))
  | S.SApply (e, args) ->
      let lazy_args : L.thunk list = List.map to_thunk args in
      (ty, L.Apply (lazyExpWith e, lazy_args))
      (* (match e with
         | A.FUNCTION_TY _ as fun_ty , S.SVar name -> (ty, L.Apply ((fun_ty,(L.Var name)), lazy_args))
         | _ -> (ty, L.Apply (lazyExpWith e, lazy_args))) *)
  | S.SLambda (args, e) ->
      let e' = L.Lambda (args, lazyExpWith e)
      and ty' = transform_funty args ty in
      (ty', e')
  | S.SNoexpr -> (ty, L.Noexpr)
  | S.SAt (e, i) -> (ty, L.At (lazyExpWith e, i))

let lazyDef (def : S.sdef) : L.def =
  match def with
  | S.SExp e -> L.Exp (lazyExpWith e)
  | S.SFunction (fname, slambda) -> L.Function (fname, lazyExpWith slambda)
  | S.SVal (tau, name, e) ->
      let body =
        lazyExpWith (A.FUNCTION_TY (A.UNIT_TY, tau), S.SLambda ([], e))
      in
      L.Function (name, body)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Def not implemented")
