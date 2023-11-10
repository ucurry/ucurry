module A = Ast
module L = Last

exception LAZY_NOT_YET_IMPLEMENTED of string

let rec lazyExp (exp : A.expr) : L.expr =
  match exp with
  | A.Literal l -> L.Literal l
  (* | A.Var v -> L.Lambda  *)
  | A.Unop (unop, e) -> L.Unop (unop, lazyExp e)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Exp not implemented")

let rec lazyDef (def : A.def) : L.def =
  match def with
  | Exp e -> L.Exp (lazyExp e)
  (* | Function (tau, name, names, e) -> raise (Failure "Lazy Function not implemented") *)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Def not implemented")
