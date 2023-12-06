(* Abstract Syntax Tree and functions for printing it *)

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

type typ =
  | INT_TY
  | STRING_TY
  | BOOL_TY
  | LIST_TY of typ
  | UNIT_TY
  | FUNCTION_TY of typ * typ
  | CONSTRUCTOR_TY of string
  | TUPLE_TY of typ list

type pattern =
  | VAR_PAT of string
  | CON_PAT of string * pattern list (* !! *)
  | WILDCARD

type expr =
  | Literal of value
  | Var of string
  | Assign of string * expr
  | Apply of expr * expr list
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * string list * expr
  | Thunk of expr
  | Construct of string * expr list (* !! *)
  | Case of expr * case_expr list
  | Tuple of expr list (* !! *)
  | At of expr * int
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
  | Function of typ * string * string list * expr
  | Datatype of typ * constructor list (* !! *)
  | Variable of typ * string * expr
  | Exp of expr
  | CheckTypeError of def

and constructor = string * typ list (* !! *)

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
  | CON_PAT (c, []) -> c
  | CON_PAT (c, ps) ->
      c ^ " (" ^ String.concat ", " (List.map string_of_pattern ps) ^ ")"
  | WILDCARD -> "_"

let rec string_of_typ = function
  | INT_TY -> "int"
  | STRING_TY -> "string"
  | BOOL_TY -> "bool"
  | LIST_TY typ -> string_of_typ typ ^ " list"
  | UNIT_TY -> "unit"
  | FUNCTION_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | CONSTRUCTOR_TY s -> s
  | TUPLE_TY typs ->
      "(" ^ String.concat " * " (List.map string_of_typ typs) ^ ")"

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
        "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else "
        ^ string_of_expr e3
    | Begin el ->
        "(begin " ^ String.concat ", " (List.map string_of_expr el) ^ ")"
    | Binop (e1, o, e2) ->
        string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
    | Unop (o, e) -> string_of_uop o ^ string_of_expr e
    | Lambda (t, vl, e) ->
        "\\(" ^ string_of_typ t ^ ")" ^ String.concat " " vl ^ " -> "
        ^ string_of_expr e
    | Construct (n, []) -> n
    | Construct (n, es) ->
        n ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
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
  | c, [] -> c
  | c, ts -> c ^ " of " ^ String.concat " * " (List.map string_of_typ ts)

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
