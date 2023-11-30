module S = Sast
module A = Ast
module L = Last

exception LAZY_NOT_YET_IMPLEMENTED of string

let rec transform_funty (funty : A.typ) : A.typ =
  match funty with
  | A.FUNCTION_TY (argty, retty) ->
      A.FUNCTION_TY (A.FUNCTION_TY (UNIT_TY, argty), transform_funty retty)
  | _ -> funty


let rec to_thunk ((t, _) as exp : S.sexpr) : L.sexpr =
  (A.FUNCTION_TY (A.UNIT_TY, t), L.Lambda ([ "unit_ph" ], lazyExpWith exp))
and lazyExpWith ((ty, exp) : S.sexpr) : L.sexpr =
  match exp with
  | S.SLiteral l -> (ty, L.Literal l)
  | S.SVar v ->
      ( ty,
        L.Apply
          ( (A.FUNCTION_TY (A.UNIT_TY, ty), L.Var v),
            [ (A.UNIT_TY, L.Literal UNIT) ] ) )
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
      (*TODO dc reason: I'm not sure if we allow literal expression as scrutinee, if we do, then we need to put literals into a lambda as well*)
      let ps, cs = List.split cases in
      let cs' = List.map lazyExpWith cs in
      (ty, L.Case (scrutinee', List.combine ps cs'))
  | S.SApply (e, args) ->
      let lazy_args : L.thunk list = List.map to_thunk args in
      (ty, L.Apply (lazyExpWith e, lazy_args))
  | S.SLambda (args, e) ->
      let e' = L.Lambda (args, lazyExpWith e) and ty' = transform_funty ty in
      (ty', e')
  | S.SNoexpr -> (ty, L.Noexpr)
  | S.SAt (e, i) -> (ty, L.At (lazyExpWith e, i))

let lazyDef (def : S.sdef) : L.def =
  match def with
  | S.SExp e -> L.Exp (lazyExpWith e)
  | S.SVal (_, name, e) ->
     (*TODO: maybe assert here that the type in e is the same as tau, as we have 2 places to retrieve typ, maybe assert it to a single point of truth is safer*)
      let (ty, _) as le = to_thunk e in
      L.Function (ty, name, le)
  | S.SFunction (funty, name, body) ->
      let ((ty, _) as lambda) = lazyExpWith (funty, S.SLambda body) in
      L.Function (ty, name, lambda)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Def not implemented")
