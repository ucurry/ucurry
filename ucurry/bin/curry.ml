module  A  =  Ast
let rec curry_expr (exp: A.expr) : A.expr = 
  match exp with
  | A.Apply ( _ , _::[]) -> exp
  | A.Apply (f , args) -> List.fold_left (fun f' arg -> A.Apply (f', [arg])) f args 
  | A.Lambda (_, _::[], _) -> exp 
  | A.Lambda (typ, a :: args, body) -> 
    let (_, retty) = SemantUtil.get_ft typ in 
    A.Lambda (typ, [a], curry_expr @@ A.Lambda (retty, args, body)) 
  | _ -> exp  

let curry (program: A.program) : A.program =  
  let curry_def (def: A.def) : A.def = 
    match def with
    | A.Exp e -> A.Exp (curry_expr e)
    | _ -> def 
  in List.map curry_def program


