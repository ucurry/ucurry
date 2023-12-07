module A = Ast
type binop  = A.binop
type uop  = A.uop
type typ = A.typ 
type value = A.value

type expr =
  | Literal of value
  | Var of string
  | Assign of string * expr
  | Apply of expr * expr list
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * string list * expr
  | Thunk of expr
  | Construct of (string * int) * expr (* !! *)
  | Tuple of expr list (* !! *)
  | At of expr * int
  | GetTag of expr (* return a string *)
  | GetField of expr * int (* return the field value of the value constructor *)
  | Noexpr

type def =
  | Function of typ * string * string list * expr
  | Datatype of typ * constructor list (* !! *)
  | Variable of typ * string * expr
  | Exp of expr
  | CheckTypeError of def

and constructor = string * typ (* !! *)