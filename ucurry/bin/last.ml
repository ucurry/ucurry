module A = Ast 

type expr = 
  | Literal of value 
  | Assign of string * thunk 
  | Apply of expr * thunk list
  | If of expr * expr * expr
  | Let of ((A.typ * string) * thunk) list * expr 
  | Begin of expr list
  | Binop of expr * A.binop * expr
  | Unop of A.uop * expr
  | Case of expr * case_expr list
  | Lambda of lambda
  | Noexpr
  and value = A.value
  and case_expr = A.pattern * expr
  and lambda = A.typ * string list * expr (* TODO: need to change lambda argument type  *)
  and thunk = lambda

  and def = 
  | Function of string * lambda
  | Datatype of string * A.constructor list
  | Variable of A.typ * string * thunk (* typ will need to be THUNK_TY *)
  | Exp of expr
  | CheckTypeError of def 
and constructor = string * A.typ option

type program = def list
  