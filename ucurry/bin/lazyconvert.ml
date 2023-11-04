module A = Ast

module L = Last

let rec lazyExp (exp: A.expr ): L.expr =  match exp with
| A.Literal v -> L.Literal v  
| A.Unop (unop, e) -> L.Unop (unop, lazyExp e)
| _ -> raise (Failure "LazyExp not implemented")

let rec lazyDef (def: A.def ): L.def =  match def with
| Exp (e) -> L.Exp (lazyExp e)
(* | Function (tau, name, names, e) -> raise (Failure "Lazy Function not implemented") *)
| _ -> raise (Failure " LazyDef not implemented")