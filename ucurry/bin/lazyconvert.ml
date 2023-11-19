module S = Sast
module A = Ast
module L = Last

exception LAZY_NOT_YET_IMPLEMENTED of string
let rec lazyExpWith ((ty, tope) : S.sexpr) : L.sexpr =
  let rec lazyExp (exp : S.sx) : L.expr =

    let to_thunk ((t, e) as thunk: S.sexpr) : L.sexpr = 
      (A.FUNCTION_TY (A.UNIT_TY, t), L.Lambda ([], lazyExpWith thunk))
    in

    match exp with
    | S.SLiteral l -> L.Literal l
    | S.SVar v -> L.Apply ((FUNCTION_TY (UNIT_TY, ty), L.Var v), [])
    | S.SAssign (v, e) -> L.Assign (v, to_thunk e)
    | S.SUnop (unop, e) -> L.Unop (unop, lazyExpWith e)
    | S.SBinop (e1, binop, e2) -> L.Binop (lazyExpWith e1, binop, lazyExpWith e2)
    | S.SBegin es -> L.Begin (List.map lazyExpWith es)
    | S.SIf (e1, e2, e3) -> L.If (lazyExpWith e1, lazyExpWith e2, lazyExpWith e3)
    | S.SLet (bs, e) ->
      let (vars, es) = List.split bs  in 
      let lazy_bs = List.combine vars (List.map to_thunk es) in 
        L.Let (lazy_bs, lazyExpWith e)
    | S.SCase (scrutinee, cases) ->
        let scrutinee' = lazyExpWith scrutinee in
        let ps, cs = List.split cases in
        let cs' = List.map lazyExpWith cs in
        L.Case (scrutinee', List.combine ps cs')
    | S.SApply (e, args) ->
        let lazy_args : L.thunk list = List.map to_thunk args in
        L.Apply (lazyExpWith e, lazy_args)
    | S.SLambda (args, e) -> L.Lambda (args, lazyExpWith e)
    | S.SNoexpr -> L.Noexpr
  in
  (ty, lazyExp tope)

let rec lazyDef (def : S.sdef) : L.def =
  match def with
  | S.SExp e -> L.Exp (lazyExpWith e)
  | S.SFunction (fname, slambda) ->  L.Function (fname, lazyExpWith slambda)
  | S.SVal (tau, name, e) -> 
    let body = lazyExpWith (A.FUNCTION_TY (tau, A.UNIT_TY), (S.SLambda ([], e)))  in 
     L.Function (name, body)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Def not implemented")
