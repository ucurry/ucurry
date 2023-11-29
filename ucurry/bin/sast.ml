module A = Ast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type typ = A.typ
type type_env = typ StringMap.t
type vcon_env = (string * int * typ) StringMap.t
type vcon_sets = StringSet.t StringMap.t

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
  | PATTERNS of pattern list
  | VAR_PAT of string
  | CON_PAT of int * pattern
  | WILDCARD
  | CONCELL of string * string
  | NIL

(* data constructor name becomes index *)
and svalue =
  | Construct of (string * int * Ast.typ) * svalue
    (* NOTE: add additional information
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
  | SDatatype of typ * constructor list
  | SVal of typ * string * sexpr
  | SFunction of typ * string * lambda
  | SExp of sexpr
  | SCheckTypeError of sdef

and constructor = string * typ

type sprogram = sdef list
