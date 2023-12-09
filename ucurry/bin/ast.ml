(* Abstract Syntax Tree and functions for printing it *)
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
  | Cons

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
  | CON_PAT of vcon_name * pattern (* !! *)
  | PATS of pattern list
  | WILDCARD

type expr =
  | Literal of value
  | Var of name
  | Assign of string * expr
  | Apply of expr * expr list
  | If of expr * expr * expr
  | Let of (name * expr) list * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * arg_name list * expr
  | Thunk of expr
  | Construct of vcon_name * expr (* !! *)
  | Case of expr * case_expr list
  | Tuple of expr list (* !! *)
  | At of expr * int
  | GetTag of expr (* Only for testing - need to be deleted; return a string *)
  | GetField of expr * vcon_name
    (* Only for testing - need to be deleted; return the field value of the value constructor *)
  | Noexpr

and value =
  | INT of int
  | STRING of string
  | BOOL of bool
  | EMPTYLIST of typ
  | LIST of value * value
  (* | TUPLE of value list *)
  | INF_LIST of int
  | UNIT

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
  | Cons -> "::"

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
      match p with PATS [] -> c | _ -> c ^ "(" ^ string_of_pattern p ^ ")")
  | PATS [] -> ""
  | PATS ps -> String.concat ", " (List.map string_of_pattern ps)
  | WILDCARD -> "_"
(*
   let rec string_of_typ = function
     | INT_TY -> "int"
     | STRING_TY -> "string"
     | BOOL_TY -> "bool"
     | LIST_TY typ -> string_of_typ typ ^ " list"
     | UNIT_TY -> "unit"
     | FUNCTION_TY (t1, t2) ->
         "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
     | CONSTRUCTOR_TY dt -> dt
     | TUPLE_TY typs ->
         "(" ^ String.concat " * " (List.map string_of_typ typs) ^ ")" *)

let string_of_variable (t, s) = string_of_typ t ^ " " ^ s

let rec string_of_expr exp =
  let flat_string_of_exp = function
    | Literal l -> string_of_literal l
    | Var s -> s
    | Assign (v, e) -> v ^ " = " ^ string_of_expr e
    | Apply (e, el) ->
        "(" ^ string_of_expr e ^ " "
        ^ String.concat " " (List.map string_of_expr el)
        ^ ")"
    | If (e1, e2, e3) ->
        "if " ^ string_of_expr e1 ^ " \n then " ^ string_of_expr e2 ^ " \n else "
        ^ string_of_expr e3
    | Begin el ->
        "(begin " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Binop (e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
    | Unop (o, e) -> string_of_uop o ^ string_of_expr e
    | Lambda (t, vl, e) ->
        "\\(" ^ string_of_typ t ^ ")" ^ String.concat " " vl ^ " -> "
        ^ string_of_expr e
    | Construct (n, e) -> n ^ string_of_expr e
    (* | Construct (n, []) -> n
       | Construct (n, es) ->
           n ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")" *)
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
    | Tuple es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
    | At (e, i) -> string_of_expr e ^ "." ^ string_of_int i
    | Noexpr -> ""
    | Thunk e -> "THUNK: " ^ string_of_expr e
    | GetField (e, v) -> string_of_expr e ^ "@" ^ v
    | GetTag e -> string_of_expr e ^ ".T"
  in

  match exp with Noexpr -> "" | _ -> "(" ^ flat_string_of_exp exp ^ ")"

and string_of_literal = function
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
(* | Construct (c, e) -> "(" ^ c ^ " " ^ string_of_literal e ^ ")" *)

let string_of_constructor = function
  | c, UNIT_TY -> c
  | c, t -> c ^ " of " ^ string_of_typ t
(* | c, [] -> c
   | c, ts -> c ^ " of " ^ String.concat " * " (List.map string_of_typ ts) *)

let rec string_of_def = function
  | Function (ty, f, args, e) ->
      "fun : " ^ string_of_typ ty ^ ":\n" ^ f ^ " " ^ String.concat " " args
      ^ " = " ^ string_of_expr e ^ ";"
  | Datatype (ty, cls) ->
      "datatype " ^ string_of_typ ty ^ " = "
      ^ String.concat " | " (List.map string_of_constructor cls)
      ^ ";"
  | Exp e -> string_of_expr e ^ ";"
  | Variable (ty, name, e) ->
      string_of_typ ty ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";"
  | CheckTypeError e -> "check_type_error " ^ string_of_def e

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
