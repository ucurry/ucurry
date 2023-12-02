module A = Ast

let rec curry_expr (exp : A.expr) : A.expr =
  match exp with
  | A.Literal _ -> exp
  | A.Var _ -> exp
  | A.Assign (name, e) -> A.Assign (name, curry_expr e)
  | A.Apply (f, [ arg ]) -> A.Apply (curry_expr f, [ curry_expr arg ])
  | A.Apply (f, args) ->
      List.fold_left (fun f' arg -> A.Apply (f', [ curry_expr arg ])) (curry_expr f) args
  | A.If (e1, e2, e3) -> A.If (curry_expr e1, curry_expr e2, curry_expr e3)
  | A.Let (bs, body) ->
      let vars, binds = List.split bs in
      let new_bs = List.combine vars (List.map curry_expr binds) in
      A.Let (new_bs, curry_expr body)
  | A.Begin es -> A.Begin (List.map curry_expr es)
  | A.Binop (e1, bop, e2) -> A.Binop (curry_expr e1, bop, curry_expr e2)
  | A.Unop (unop, e) -> A.Unop (unop, curry_expr e)
  | A.Lambda (tau, [], body) ->  A.Lambda (tau, [], curry_expr body)
  | A.Lambda (tau, [ arg ], body) -> A.Lambda (tau, [ arg ], curry_expr body)
  | A.Lambda (typ, a :: args, body) ->
      let _, retty = SemantUtil.get_ft typ in
      A.Lambda (typ, [ a ], curry_expr @@ A.Lambda (retty, args, curry_expr body))
  | A.Case (scrutinee, cases) -> 
    let ps , es = List.split cases in 
    let new_cases = List.combine ps (List.map curry_expr es)  in 
    A.Case (curry_expr scrutinee, new_cases)
  | A.At (e, idx) -> A.At (curry_expr e, idx)
  | A.Noexpr -> A.Noexpr
  | _ -> failwith "no match in curry pass"
  

let curry (program : A.program) : A.program =
  let rec curry_def (def : A.def) : A.def =
    match def with
    | A.Exp e -> A.Exp (curry_expr e)
    | A.Function (ty, funname, [], body) -> A.Function (ty, funname, [], curry_expr body)
    | A.Function (ty, funname, [arg], body) -> A.Function (ty, funname, [arg], curry_expr body)
    | A.Function (ty, funname, a :: args, body) ->
        let _, retty = SemantUtil.get_ft ty in
        A.Function (ty, funname, [ a ], curry_expr @@ A.Lambda (retty, args, body))
    | A.Variable (ty, name, e) -> A.Variable (ty, name, curry_expr e)
    | A.CheckTypeError d -> A.CheckTypeError (curry_def d)
    | A.Datatype _ -> def
  in
  List.map curry_def program
