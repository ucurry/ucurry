open Ast

type sexpr =
  | SLiteral of typ * value
  | SVar of typ * string
  | SAssign of typ * string * sexpr
  | SApply of typ * sexpr * sexpr list
  | SIf of  typ * sexpr * sexpr * sexpr
  | SLet of typ * ((typ * string) * sexpr) list * sexpr
  | SBegin of typ * sexpr list
  | SBinop of typ * sexpr * binop * sexpr
  | SUnop of typ * uop * sexpr
  | SLambda of typ * string list * sexpr
  | SCase of typ *sexpr * scase_expr list
  | SNoexpr

and scase_expr = pattern * sexpr

type sdef =
  | SFunction of typ * string * string list * sexpr
  | SDatatype of typ * constructor list
  | SVariable of typ * string * sexpr
  | SExp of sexpr
  | SCheckTypeError of def

type sprogram = sdef list