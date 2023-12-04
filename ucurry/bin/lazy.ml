module A = Ast

let rec to_thunk_ty (ty : A.typ) : A.typ =
  A.FUNCTION_TY (A.UNIT_TY, transform_ty ty)

and transform_ty ty =
  match ty with
  | A.FUNCTION_TY (argty, retty) ->
      A.FUNCTION_TY (to_thunk_ty argty, transform_ty retty)
  | _ -> ty

let unitv = A.Literal A.UNIT

let rec lazy_expr (exp : A.expr) : A.expr =
  let to_thunk e = A.Thunk (lazy_expr e) in
  match exp with
  | A.Literal _ -> exp
  | A.Var n -> A.Apply (A.Var n, [ unitv ])
  | A.Assign (n, e) -> (A.Begin [A.Assign (n, to_thunk e); A.Apply (A.Var n, [unitv])])
  | A.Apply (e, es) ->
      let lazy_fun = lazy_expr e in
      let lazy_args = List.map to_thunk es in
      A.Apply (lazy_fun, lazy_args)
  | A.If (e1, e2, e3) -> A.If (lazy_expr e1, lazy_expr e2, lazy_expr e3)
  | A.Let (bindings, body) ->
      let thunk_bindings = List.map (fun (n, e) -> (n, to_thunk e)) bindings in
      A.Let (thunk_bindings, lazy_expr body)
  | A.Lambda (typ, args, body) ->
      let typ' = transform_ty typ in
      A.Lambda (typ', args, lazy_expr body)
  | A.Begin es -> A.Begin (List.map lazy_expr es)
  | A.Binop (e1, bp, e2) -> A.Binop (lazy_expr e1, bp, lazy_expr e2)
  | A.Unop (up, e) -> A.Unop (up, lazy_expr e)
  | A.Case (scrutinee, cases) ->
      let pat, es = List.split cases in
      let new_cases = List.combine pat (List.map lazy_expr es) in
      A.Case (lazy_expr scrutinee, new_cases)
  | A.At (e, idx) -> A.At (lazy_expr e, idx)
  | A.Noexpr -> A.Noexpr
  | A.Thunk _ -> failwith "Illegal thunk"

let rec lazy_def def =
  match def with
  | A.Function (ty, funname, args, body) ->
      let lazy_tau = to_thunk_ty ty in
      A.Function
        (lazy_tau, funname, [ "unit" ], lazy_expr (A.Lambda (ty, args, body)))
  | A.Exp e -> A.Exp (lazy_expr e)
  | A.CheckTypeError e -> A.CheckTypeError (lazy_def e)
  | A.Variable (ty, name, e) ->
      let lazy_tau = to_thunk_ty ty in
      A.Variable (lazy_tau, name, A.Lambda (lazy_tau, [ "unit" ], lazy_expr e))
  | A.Datatype (_, _) -> def

let lazy_convert (program : A.def list) : A.def list = List.map lazy_def program
