module A = Ast
module S = Sast

type sexpr = A.typ * expr

and expr =
  | Literal of S.svalue
  | Var of string
  | Assign of string * thunk
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of ((A.typ * string) * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Case of sexpr * case_expr list
  | Lambda of lambda
  | At of sexpr * int
  | Noexpr

and case_expr = S.pattern * sexpr
and lambda = string list * sexpr
and thunk = sexpr (* which will be a Lambda (ty, lambda) form *)

and def =
  | Function of A.typ * string * sexpr
    (* TODO: lazy-convert would eventually convert all top-level Val to Val(ty, name, slambda) *)
  | Datatype of A.typ * A.constructor list
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * A.typ option

type program = def list
