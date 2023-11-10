open Ast


type sexpr = typ * expr
and sx = 
  | SLiteral of typ * value