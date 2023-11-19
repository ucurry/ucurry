module A = Ast

type sexpr = A.typ * expr

and expr =
  | Literal of value
  | Assign of string * thunk
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of ((A.typ * string) * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Case of sexpr * case_expr list
  | Lambda of lambda
  | Noexpr

and value = A.value
and case_expr = A.pattern * sexpr

and lambda =
  A.typ * string list * sexpr (* TODO: need to change lambda argument type  *)

and thunk = lambda

and def =
  | Function of string * lambda
  | Datatype of A.typ * A.constructor list
  | Variable of A.typ * string * thunk (* typ will need to be THUNK_TY *)
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * A.typ option

type program = def list
