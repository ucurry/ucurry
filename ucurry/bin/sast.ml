(* Semantic AST: AST with type information  *)
(* Authors: Stephanie Xu, Vivian Li, Matt Zhou *)


open Typing
module A = Ast
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type type_env = typ StringMap.t
type vcon_env = (string * int * typ) StringMap.t
type vcon_sets = StringSet.t StringMap.t

type sexpr = typ * sx

and sx =
  | SLiteral of svalue
  | SVar of name
  | SApply of sexpr * sexpr list
  | SIf of sexpr * sexpr * sexpr
  | SLet of (name * sexpr) list * sexpr
  | SLetrec of (name * sexpr) list * sexpr
  | SBegin of sexpr list
  | SBinop of sexpr * A.binop * sexpr
  | SUnop of A.uop * sexpr
  | SLambda of lambda
  | SConstruct of (vcon_id * vcon_name) * sexpr
  | STuple of sexpr list
  | SAt of sexpr * int
  | SGetTag of sexpr
  | SGetField of sexpr * int
  | SEmptyList of typ
  | SList of sexpr * sexpr
  | SNoexpr
  | SNomatch
  | SThunk of sexpr (* a lambda expression for thunk_fun *)
  | SForce of sexpr (* a expression of THUNK_TY t type*)

and scase_expr = A.pattern * sexpr
and lambda = string list * sexpr
and svalue = INT of int | STRING of string | BOOL of bool | UNIT

type sdef =
  | SDatatype of typ * constructor list
  | SVal of string * sexpr
  | SFunction of typ * string * lambda
  | SExp of sexpr
  | SCheckTypeError of sdef

and constructor = string * typ

type sprogram = sdef list

(* Pretty Print *)
let string_of_literal : svalue -> string = function
  | INT l -> string_of_int l
  | STRING l -> "\"" ^ l ^ "\""
  | BOOL l -> string_of_bool l
  | UNIT -> "()"
