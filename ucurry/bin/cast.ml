(* Abstract Syntax Tree and functions for printing it *)

(* Copied from ast.ml: A.typ, A.unop, A.binop,  *)
module A = Ast

type sexpr = A.typ * expr

and expr =
  | Literal of A.value
  | Var of string
  | Assign of string * thunk
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of ((A.typ * string) * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Captured of int
  | Closure of (string list * sexpr) * sexpr list (* (lambda, captured list) *)
  | Case of sexpr * case_expr list
  | Noexpr

and case_expr = A.pattern * sexpr
and thunk = sexpr (* which will be a Closure form *)

and def =
  | Function of string * sexpr 
  | Datatype of A.typ * A.constructor list
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * A.typ option

type program = def list

(* Pretty-printing functions *)

let rec string_of_typ = function
  | A.INT_TY -> "int"
  | A.STRING_TY -> "string"
  | A.BOOL_TY -> "bool"
  | A.LIST_TY typ -> string_of_typ typ ^ " list"
  | A.UNIT_TY -> "unit"
  | A.FUNCTION_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | A.CONSTRUCTOR_TY s -> s
  | A.TUPLE_TY typs ->
      "(" ^ String.concat " * " (List.map string_of_typ typs) ^ ")"


let rec string_of_sexpr ((ty, tope) : sexpr) = 
let rec string_of_expr (exp : expr) =
  let flat_string_of_exp = function
    | Literal l -> A.string_of_literal l
    (* | Var s -> s *)
    (* | Assign (v, e) -> v ^ " = " ^ string_of_expr e *)
    (* | Apply (e, el) ->
        "(" ^ string_of_expr e ^ " "
        ^ String.concat " " (List.map string_of_expr el)
        ^ ")" *)
    | Begin el ->
        "(begin " ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    (* | Binop (e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2 *)
    | Unop (o, e) -> A.string_of_uop o ^ string_of_sexpr e
    (* | Lambda (t, vl, e) ->
        "\\(" ^ string_of_typ t ^ ")" ^ String.concat " " vl ^ " -> " ^ string_of_expr e *)
    (* | Case (e, cel) ->
        "(case " ^ string_of_expr e ^ " of\n\t"
        ^ "  " ^ String.concat " \n\t| " (List.map (fun (pat, exp) -> string_of_pattern pat ^ " => " ^ string_of_expr exp) cel)
        ^ ")" *)
    (* | Let (vl, e) ->
        "let " ^ String.concat ", " (List.map (fun ((t, v), e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_expr e) vl) ^ " in " ^ string_of_expr e *)
    | Noexpr -> ""
    | _ -> raise (Failure "String_of_expr Not implemented For Most Cases")
  in
  match exp with Noexpr -> "" | _ -> "(" ^ flat_string_of_exp exp ^ ")"
in
string_of_typ ty ^ string_of_expr tope

let string_of_def = function
  | Exp se -> string_of_sexpr se
  | _ -> raise (Failure "String_of_def Not implemented For Most Cases")

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
