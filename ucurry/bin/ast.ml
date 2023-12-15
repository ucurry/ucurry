(* Abstract Syntax Tree and functions for printing it *)
(* Authors: Alex Bai, Stephanie Xu, Vivian Li, Matt Zhou *)

open Typing

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

type uop =
  | Neg
  | Not
  | Hd
  | Tl
  | Print
  | Println
  | IsNull
  | GetField
  | GetPattern

type pattern =
  | VAR_PAT of name
  | CON_PAT of vcon_name * pattern
  | PATS of pattern list
  | CONCELL of pattern * pattern
  | WILDCARD
  | NIL

type expr =
  | Literal of value
  | Var of name
  | Apply of expr * expr list
  | If of expr * expr * expr
  | Let of (name * expr) list * expr
  | Letrec of (name * expr) list * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * arg_name list * expr
  | Thunk of expr
  | Force of expr 
  | Case of expr * case_expr list
  | At of expr * int
  | List of expr * expr
  | EmptyList of typ
  | Tuple of expr list
  | Construct of vcon_name * expr
  | GetTag of expr (* Should only appear from after Desugar (case convert) *)
  | GetField of
      expr
      * vcon_name (* Should only appear from after Desugar (case convert) *)
  | Noexpr
  | NoMatch

and value = INT of int | STRING of string | BOOL of bool | UNIT
and case_expr = pattern * expr

type def =
  | Function of typ * name * arg_name list * expr
  | Datatype of typ * constructor list (* !! *)
  | Variable of typ * name * expr
  | Exp of expr
  | CheckTypeError of def

and constructor = vcon_name * typ (* !! *)

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
  | And -> "and"
  | Or -> "or"

let string_of_uop = function
  | Neg -> "~"
  | Not -> "not"
  | Hd -> "hd"
  | Tl -> "tl"
  | Print -> "print"
  | Println -> "println"
  | IsNull -> "null?"
  | _ -> "internal primitive"

let rec string_of_pattern = function
  | VAR_PAT s -> s
  | CON_PAT (c, p) -> (
      match p with PATS [] -> c | _ -> c ^ " " ^ string_of_pattern p)
  | PATS [] -> "()"
  | PATS ps -> "(" ^ String.concat ", " (List.map string_of_pattern ps) ^ ")"
  | WILDCARD -> "_"
  | NIL -> "[]"
  | CONCELL (hd, tl) ->
      "(" ^ string_of_pattern hd ^ "::" ^ string_of_pattern tl ^ ")"

let string_of_variable (t, s) = string_of_typ t ^ " " ^ s

let rec string_of_expr exp =
  let flat_string_of_exp = function
    | Literal l -> string_of_literal l
    | Var s -> s
    | Apply (e, el) ->
        "(" ^ string_of_expr e ^ " "
        ^ String.concat " " (List.map string_of_expr el)
        ^ ")"
    | If (e1, e2, e3) ->
        "if " ^ string_of_expr e1 ^ " \n then " ^ string_of_expr e2
        ^ " \n else " ^ string_of_expr e3
    | Begin el ->
        "(begin " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Binop (e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
    | Unop (o, e) -> string_of_uop o ^ " " ^ string_of_expr e
    | Lambda (t, vl, e) ->
        "\\(" ^ string_of_typ t ^ ")" ^ String.concat " " vl ^ " -> "
        ^ string_of_expr e
    | Construct (n, e) -> n ^ " " ^ string_of_expr e
    | Case (e, cel) ->
        "(case " ^ string_of_expr e ^ " of\n\t" ^ "  "
        ^ String.concat " \n\t| "
            (List.map
               (fun (pat, exp) ->
                 string_of_pattern pat ^ " => " ^ string_of_expr exp)
               cel)
        ^ ")"
    | Let (vl, e) ->
        "let "
        ^ String.concat ", "
            (List.map (fun (v, e) -> v ^ " = " ^ string_of_expr e) vl)
        ^ " in " ^ string_of_expr e
    | Letrec (vl, e) ->
        "letrec "
        ^ String.concat ", "
            (List.map (fun (v, e) -> v ^ " = " ^ string_of_expr e) vl)
        ^ " in " ^ string_of_expr e
    | Tuple es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
    | At (e, i) -> string_of_expr e ^ "." ^ string_of_int i
    | Noexpr -> ""
    | Thunk e -> "(THUNK: " ^ string_of_expr e ^ ")"
    | GetField (e, v) -> string_of_expr e ^ "@" ^ v
    | GetTag e -> string_of_expr e ^ ".T"
    | EmptyList t -> "[" ^ string_of_typ t ^ "]"
    | List (x, xs) -> string_of_expr x ^ " :: " ^ string_of_expr xs
    | NoMatch -> "no match"
    | Force e -> "(Force " ^ string_of_expr e ^ ")"
  in

  match exp with Noexpr -> "" 
  | _ -> flat_string_of_exp exp 
  (* | _ -> "(" ^ flat_string_of_exp exp ^ ")" *)

and string_of_literal = function
  | INT l -> string_of_int l
  | STRING l -> "\"" ^ l ^ "\""
  | BOOL l -> string_of_bool l
  | UNIT -> "()"

let string_of_constructor = function
  | c, UNIT_TY -> c
  | c, t -> c ^ " of " ^ string_of_typ t

let rec string_of_def = function
  | Function (ty, f, args, e) ->
      "fun : " ^ string_of_typ ty ^ ":\n" ^ f ^ " " ^ String.concat " " args
      ^ " = " ^ string_of_expr e ^ ";"
  | Datatype (ty, cls) ->
      "datatype " ^ string_of_typ ty ^ " = "
      ^ String.concat " | " (List.map string_of_constructor cls)
      ^ ";"
  | Exp e -> string_of_expr e ^ ";"
  | Variable (ty, name, e)  ->
      string_of_typ ty ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";"
  | CheckTypeError e -> "check_type_error " ^ string_of_def e

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
