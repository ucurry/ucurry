module S = Sast
module A = Ast
module L = Last

exception LAZY_NOT_YET_IMPLEMENTED of string

let rec lazyExpWith ((ty, tope) : S.sexpr) : L.sexpr =
  let rec lazyExp (exp : S.sx) : L.expr =
    match exp with
    | S.SLiteral l -> L.Literal l
    (* | A.Var v -> L.Lambda  *)
    | S.SUnop (unop, e) -> L.Unop (unop, lazyExpWith e)
    | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Exp not implemented")
  in 
(ty, lazyExp tope)

let lazyDef (def : S.sdef) : L.def =
  match def with
  | S.SExp e -> L.Exp (lazyExpWith e)
  (* | Function (tau, name, names, e) -> raise (Failure "Lazy Function not implemented") *)
  | _ -> raise (LAZY_NOT_YET_IMPLEMENTED "Def not implemented")
