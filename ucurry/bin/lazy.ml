(* Lazy, delay name binding by putting into a lambda expression,
   and force evaluation in VAR *)
(* Authors: Stephanie Xu, Vivian Li *)

open Typing
module A = Ast

let rec to_thunk_ty (ty : typ) : typ = THUNK_TY (FUNCTION_TY (UNIT_TY, transform_ty ty))
and to_fun_ty (ty: typ) : typ = FUNCTION_TY (UNIT_TY, transform_ty ty)

(* let rec to_thunk_ty (ty : typ) : typ = FUNCTION_TY (UNIT_TY, transform_ty ty) *)

and transform_ty ty =
  match ty with
  | FUNCTION_TY (argty, retty) ->
      FUNCTION_TY (to_thunk_ty argty, transform_ty retty)
  | TUPLE_TY taus -> TUPLE_TY (List.map to_thunk_ty taus)
  | LIST_TY taus -> LIST_TY (transform_ty taus)
  | _ -> ty

let unitv = A.Literal Ast.UNIT

let rec lazy_expr (exp : A.expr) : A.expr =
  let to_thunk e = A.Thunk (lazy_expr e) in (* would turn into Thunk (() -> e) in Semant *)
  let force_eval e = 
    let else_exp = A.Apply (A.GetClosure e, [unitv]) in else_exp
    (* A.Apply (A.GetClosure e, [unitv]) in 
                   else_exp  *)
    (* let else_exp = A.Begin [A.SetEvaled e; A.SetValue (e, A.Apply (A.GetClosure e, [(A.Literal A.UNIT)]))] in 
                   A.If (A.GetEvaled e, A.GetValue e, else_exp)  *)
  in 
  match exp with
  | A.Literal _ -> exp
  | A.Var n -> force_eval (A.Var n)
  | A.Apply (e, es) ->
      let lazy_fun = lazy_expr e in
      let lazy_args = List.map to_thunk es in
      A.Apply (lazy_fun, lazy_args)
  | A.If (e1, e2, e3) -> A.If (lazy_expr e1, lazy_expr e2, lazy_expr e3)
  | A.Let (bindings, body) ->
      let thunk_bindings = List.map (fun (n, e) -> (n, to_thunk e)) bindings in
      A.Let (thunk_bindings, lazy_expr body)
  | A.Letrec (bindings, body) ->
      let thunk_bindings = List.map (fun (n, e) -> (n, to_thunk e)) bindings in
      A.Letrec (thunk_bindings, lazy_expr body)
  | A.Lambda (typ, args, body) ->
      let typ' = transform_ty typ in
      A.Lambda (typ', args, lazy_expr body)
  | A.Begin es -> A.Begin (List.map lazy_expr es)
  | A.Binop (e1, bp, e2) -> A.Binop (lazy_expr e1, bp, lazy_expr e2)
  | A.Unop (up, e) -> A.Unop (up, lazy_expr e)
  | A.Construct (vcon_name, arg) -> A.Construct (vcon_name, to_thunk arg)
  | A.Tuple es -> A.Tuple (List.map to_thunk es)
  | A.List (hd, tl) -> A.List (lazy_expr hd, lazy_expr tl)
  | A.EmptyList tau -> A.EmptyList tau
  | A.At (e, idx) -> force_eval (A.At (lazy_expr e, idx))
  | A.Noexpr -> A.Noexpr
  | A.GetTag e -> A.GetTag (lazy_expr e)
  | A.GetField (e, i) -> force_eval (A.GetField (lazy_expr e, i))
  | A.Case (scrutinee, cases) ->
      let scrutinee' = lazy_expr scrutinee and ps, cs' = List.split cases in
      let cs' = List.combine ps (List.map lazy_expr cs') in
      A.Case (scrutinee', cs')
  | A.NoMatch -> A.NoMatch
  | A.Thunk e -> A.Thunk (lazy_expr e)
  | A.Force _ -> failwith "Illegal force"
  | _ -> failwith "Illegal getValue, getEvalued or EvalFun"

let rec lazy_def def =
  match def with
  | A.Function (ty, funname, args, body) -> (* The body has not yet wrapped with thunk => will be done in codegen *)
      let lazy_tau = to_thunk_ty ty in
      A.Function
        (lazy_tau, funname, [ "unit" ], lazy_expr (A.Lambda (ty, args, body)))
  | A.Exp e -> A.Exp (lazy_expr e)
  | A.CheckTypeError e -> A.CheckTypeError (lazy_def e)
  | A.Variable (ty, name, e) ->
      let lazy_tau = to_thunk_ty ty in
      (* A.Variable (lazy_tau, name, A.Thunk (A.Lambda (to_fun_ty ty , [ "unit" ], lazy_expr e))) *)
      A.Variable (lazy_tau, name, A.Thunk (lazy_expr e))
  | A.Datatype (t, vcon_list) ->
      let vcon_names, arg_taus = List.split vcon_list in
      let new_arg_taus = List.map to_thunk_ty arg_taus in
      let new_vcon_list = List.combine vcon_names new_arg_taus in
      A.Datatype (t, new_vcon_list)

let lazy_convert (program : A.def list) : A.def list = List.map lazy_def program
