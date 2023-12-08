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
  | SAssign of name * sexpr
  | SApply of sexpr * sexpr list
  | SIf of sexpr * sexpr * sexpr
  | SLet of (name * sexpr) list * sexpr
  | SBegin of sexpr list
  | SBinop of sexpr * A.binop * sexpr
  | SUnop of A.uop * sexpr
  | SLambda of lambda
  | SConstruct of (vcon_id * vcon_name) * sexpr
  | STuple of sexpr list (* TODO: redundant type annotation?? *)
  | SAt of sexpr * int
  | SGetTag of sexpr
  | SGetField of sexpr * int
  | SNoexpr
  | SNomatch

(* and scase_expr = pattern * sexpr *)
and lambda = string list * sexpr
(*
   and pattern =
     (* | PATTERNS of pattern list *)
     | PATS of pattern list
     | VAR_PAT of string
     | CON_PAT of int * pattern
     | WILDCARD
   (* | CONCELL of string * string
      | NIL *) *)

(* data constructor name becomes index *)
and svalue =
  (* | Construct of (string * int * Ast.typ) * svalue *)
  (* NOTE: add additional information
           on the datatyp AST node *)
  | INT of int
  | STRING of string
  | BOOL of bool
  | EMPTYLIST of typ
  | LIST of svalue * svalue
  (* | TUPLE of svalue list *)
  | INF_LIST of int
  | UNIT

type sdef =
  | SDatatype of typ * constructor list
  | SVal of string * sexpr
  | SFunction of typ * string * lambda
  | SExp of sexpr
  | SCheckTypeError of sdef

and constructor = string * typ

type sprogram = sdef list

(* Pretty Print *)
let rec string_of_literal : svalue -> string = function
  | INT l -> string_of_int l
  | STRING l -> "\"" ^ l ^ "\""
  | BOOL l -> string_of_bool l
  | EMPTYLIST t -> "[" ^ string_of_typ t ^ "]"
  | LIST (x, xs) ->
      let rec listString (x, xs) =
        match (x, xs) with
        | x, EMPTYLIST _ -> string_of_literal x
        | x, LIST (y, ys) -> string_of_literal x ^ "," ^ listString (y, ys)
        | _ -> raise (Invalid_argument "should not be reached")
      in
      "[" ^ listString (x, xs) ^ "]"
  (* | TUPLE l -> "(" ^ String.concat ", " (List.map string_of_literal l) ^ ")" *)
  | UNIT -> "()"
  | INF_LIST n -> "[" ^ string_of_int n ^ "..]"
(* | Construct ((s, _, _), e) -> "(" ^ s ^ " " ^ string_of_literal e ^ ")" *)
