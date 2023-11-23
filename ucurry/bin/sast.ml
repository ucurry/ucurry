module A = Ast

type typ = A.typ

type sexpr = typ * sx

and sx =
  | SLiteral of svalue
  | SVar of string
  | SAssign of string * sexpr
  | SApply of sexpr * sexpr list
  | SIf of sexpr * sexpr * sexpr
  | SLet of ((typ * string) * sexpr) list * sexpr
  | SBegin of sexpr list
  | SBinop of sexpr * A.binop * sexpr
  | SUnop of A.uop * sexpr
  | SLambda of lambda
  | SCase of sexpr * scase_expr list
  | SAt of sexpr * int
  | SNoexpr
and scase_expr = pattern * sexpr
and lambda = string list * sexpr
and pattern =
  | VAR_PAT of string
  | CON_PAT of int * pattern list
  | WILDCARD
  | CONCELL of string * string
  | NIL

(* data constructor name becomes index *)
and svalue =
  | Construct of (string * int * Ast.typ) * svalue (* NOTE: add additional information 
                                                            on the datatyp AST node *)
  | INT of int
  | STRING of string
  | BOOL of bool
  | EMPTYLIST
  | LIST of svalue * svalue
  | TUPLE of svalue list
  | INF_LIST of int
  | UNIT

type sdef =
  | SFunction of string * sexpr (* This sexpr will be a Lambda expression *)
  | SDatatype of typ * constructor list
  | SVal of typ * string * sexpr
  | SExp of sexpr
  | SCheckTypeError of sdef

and constructor = string * typ (* TODO: consider changing into int * typ  *)

type sprogram = sdef list
