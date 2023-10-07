(* Abstract Syntax Tree and functions for printing it *)

(* TODO: do we want to convert top-level function to global variable binding to a lambda? if so, where should we do this *)
type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | Cons 

type uop = Neg | Not | Hd | Tl

(* type -> int | string | boolean | type list | type * type | type -> type *)
type typ =
  | INT_TY
  | STRING_TY
  | BOOL_TY
  | LIST_TY
  | UNIT_TY
  | PRODUCT_TY of typ * typ
  | FUNCTION_TY of typ * typ
  | CONSTRUCTOR_TY of string


type literal =
  | INT of int
  | STRING of string
  | BOOL of bool
  | LIST of literal list 
(* | Tuple of literal * literal *)

(* pattern -> variable-name | constructor-name { literal | variable-name } | _  *)
type pattern =
  | VAR_PAT of string
  | LIT_PAT of literal
  | CON_PAT of string * pattern list
  | WILDCARD

type expr =
  | Literal of literal
  | Var of string
  | Assign of string * expr
  | Apply of string * expr list
  | If of expr * expr * expr
  | Let of (typ * string * expr) list  * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * string list * expr
  | Construct of string * expr list
  | Case of expr * case_expr

and case_expr = pattern * expr

type def =
  | Function of typ * string * string list * expr
  | Datatype of string * constructor list
  | Variable of typ * string * expr
  | Exp of expr

and constructor = ValCon of string * typ list

type program = def list

(* Pretty-printing functions *)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Cons -> "::"

let string_of_uop = function Neg -> "-" | Not -> "!" | Hd -> "hd" | Tl -> "tl"

(* let rec string_of_listLiteral = function
  | EmptyList -> "[]"
  | Pair (l, EmptyList) -> "[" ^ string_of_literal l ^ "]"
  | Pair (l, l') ->
      "[" ^ string_of_literal l ^ "; " ^ string_of_listLiteral l' ^ "]" *)

and string_of_literal = function
  | INT l -> string_of_int l
  | STRING l -> l
  | BOOL l -> string_of_bool l
  | _ -> "boooom"
  (* | LIST l -> string_of_listLiteral l *)
(* | Tuple(l) -> string_of_tupleLiteral l *)

let rec string_of_pattern = function
  | VAR_PAT s -> s
  | LIT_PAT l -> string_of_literal l
  | CON_PAT (c, pl) ->
      c ^ "(" ^ String.concat ", " (List.map string_of_pattern pl) ^ ")"
  | WILDCARD -> "_"

let rec string_of_typ = function
  | INT_TY -> "int"
  | STRING_TY -> "string"
  | BOOL_TY -> "bool"
  | LIST_TY -> "list"
  | UNIT_TY -> "unit"
  | PRODUCT_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " * " ^ string_of_typ t2 ^ ")"
  | FUNCTION_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | CONSTRUCTOR_TY s -> s


let string_of_variable (t, s) = string_of_typ t ^ " " ^ s

let string_of_constructor _ = ""

let rec string_of_expr = function
  | Literal l -> string_of_literal l
  | Var s -> s
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | _ -> "GHE(GDSJHJUFGUOSEHA)"


let string_of_def = function
  (* | Function (v, vl, e) ->
      "let " ^ string_of_variable v ^ " " ^ String.concat " " vl ^ " = "
      ^ string_of_expr e
  | Datatype (c, cl) ->
      "type " ^ c ^ " = " ^ String.concat " | " (List.map string_of_constructor cl)
  | Variable (v, e) -> "let " ^ string_of_variable v ^ " = " ^ string_of_expr e *)
  | Exp e -> string_of_expr e
  | _ -> "We only pretty print expressions right now\n"

let string_of_program defs = 
  let () = print_string "Printing Roundtrip\n" in
  String.concat "\n" (List.map string_of_def defs)
(* | _ -> "go fuck yourself we haven't started yet\n" *)

(* let rec string_of_expr = function
  | Literal l -> string_of_literal l
  | Var s -> s
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | Apply (e, el) ->
      string_of_expr e ^ "("
      ^ String.concat ", " (List.map string_of_expr el)
      ^ ")"
  | If (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else "
      ^ string_of_expr e3
  | Let (ves, e2) ->
      "let " ^ v ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Begin el ->
      "begin " ^ String.concat " " (List.map string_of_expr el) ^ " end"
  | Binop (e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Unop (o, e) -> string_of_uop o ^ string_of_expr e
  | Lambda (v, e1, e2) ->
      "fun " ^ v ^ " -> " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Construct (c, el) ->
      c ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Case (e, (p, e1)) ->
      "case " ^ string_of_expr e ^ " of " ^ string_of_pattern p ^ " -> "
      ^ string_of_expr e1 *)

(* let rec string_of_typ ty =
  match ty with
  | INT_TY -> "int"
  | STRING_TY -> "string"
  | BOOL_TY -> "bool"
  | LIST_TY -> "list"
  | PRODUCT_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " * " ^ string_of_typ t2 ^ ")"
  | FUNCTION_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")" *)

(*
   let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

   let string_of_fdecl fdecl =
     string_of_typ fdecl.typ ^ " " ^
     fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
     ")\n{\n" ^
     String.concat "" (List.map string_of_vdecl fdecl.locals) ^
     String.concat "" (List.map string_of_stmt fdecl.body) ^
     "}\n" *)

(* let string_of_program (vars, funcs) =
   String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
   String.concat "\n" (List.map string_of_fdecl funcs) *)
