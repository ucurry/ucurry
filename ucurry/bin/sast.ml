open Ast

type sexpr = typ * sx

and sx =
  | SLiteral of value
  | SVar of string
  | SAssign of string * sexpr
  | SApply of sexpr * sexpr list
  | SIf of sexpr * sexpr * sexpr
  | SLet of ((typ * string) * sexpr) list * sexpr
  | SBegin of sexpr list
  | SBinop of sexpr * binop * sexpr
  | SUnop of uop * sexpr
  | SLambda of lambda
  | SCase of sexpr * scase_expr list
  | SNoexpr

and scase_expr = pattern * sexpr
and lambda = string list * sexpr
type sdef =
  | SFunction of string * sexpr (* This sexpr will be a Lambda expression *)
  | SDatatype of typ * constructor list
  | SVal of typ * string * sexpr
  | SExp of sexpr
  | SCheckTypeError of def

type sprogram = sdef list
