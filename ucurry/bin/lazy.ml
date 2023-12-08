open Typing
module P = Past

let rec to_thunk_ty (ty : typ) : typ = FUNCTION_TY (UNIT_TY, transform_ty ty)
and transform_ty ty =
  match ty with
  | FUNCTION_TY (argty, retty) ->
      FUNCTION_TY (to_thunk_ty argty, transform_ty retty)
  | TUPLE_TY taus -> TUPLE_TY (List.map to_thunk_ty taus)
  | _ -> ty

let unitv = P.Literal Ast.UNIT

let rec lazy_expr (exp : P.expr) : P.expr =
  let to_thunk e = P.Thunk (lazy_expr e) in
  match exp with
  | P.Literal _ -> exp
  | P.Var n -> P.Apply (P.Var n, [ unitv ])
  | P.Assign (n, e) ->
      P.Begin [ P.Assign (n, to_thunk e); P.Apply (P.Var n, [ unitv ]) ]
  | P.Apply (e, es) ->
      let lazy_fun = lazy_expr e in
      let lazy_args = List.map to_thunk es in
      P.Apply (lazy_fun, lazy_args)
  | P.If (e1, e2, e3) -> P.If (lazy_expr e1, lazy_expr e2, lazy_expr e3)
  | P.Let (bindings, body) ->
      let thunk_bindings = List.map (fun (n, e) -> (n, to_thunk e)) bindings in
      P.Let (thunk_bindings, lazy_expr body)
  | P.Lambda (typ, args, body) ->
      let typ' = transform_ty typ in
      P.Lambda (typ', args, lazy_expr body)
  | P.Begin es -> P.Begin (List.map lazy_expr es)
  | P.Binop (e1, bp, e2) -> P.Binop (lazy_expr e1, bp, lazy_expr e2)
  | P.Unop (up, e) -> P.Unop (up, lazy_expr e)
  | P.Construct (vcon_name, arg) -> P.Construct (vcon_name, to_thunk arg)
  | P.Tuple es -> P.Tuple (List.map to_thunk es)
  | P.At (e, idx) -> P.Apply (P.At (lazy_expr e, idx), [ unitv ])
  | P.Noexpr -> P.Noexpr
  | P.GetTag e -> P.GetTag (lazy_expr e)
  | P.GetField (e, i) -> P.Apply (P.GetField (lazy_expr e, i), [ unitv ])
  | P.Thunk _ -> failwith "Illegal thunk"
  | P.Nomatch -> P.Nomatch

let rec lazy_def def =
  match def with
  | P.Function (ty, funname, args, body) ->
      let lazy_tau = to_thunk_ty ty in
      P.Function
        (lazy_tau, funname, [ "unit" ], lazy_expr (P.Lambda (ty, args, body)))
  | P.Exp e -> P.Exp (lazy_expr e)
  | P.CheckTypeError e -> P.CheckTypeError (lazy_def e)
  | P.Variable (ty, name, e) ->
      let lazy_tau = to_thunk_ty ty in
      P.Variable (lazy_tau, name, P.Lambda (lazy_tau, [ "unit" ], lazy_expr e))
  | P.Datatype (t, vcon_list) ->
      let vcon_names, arg_taus = List.split vcon_list in
      let new_arg_taus = List.map to_thunk_ty arg_taus in
      let new_vcon_list = List.combine vcon_names new_arg_taus in
      P.Datatype (t, new_vcon_list)

let lazy_convert (program : P.def list) : P.def list = List.map lazy_def program
