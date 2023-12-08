open Typing
module A = Ast

type binop = A.binop
type uop = A.uop
type value = A.value

type expr =
  | Literal of value
  | Var of name
  | Assign of name * expr
  | Apply of expr * expr list
  | If of expr * expr * expr
  | Let of (name * expr) list * expr
  | Begin of expr list
  | Binop of expr * binop * expr
  | Unop of uop * expr
  | Lambda of typ * arg_name list * expr
  | Thunk of expr
  | Construct of vcon_name * expr
  | Tuple of expr list (* !! *)
  | At of expr * int
  | GetTag of expr (* return a string *)
  | GetField of
      expr * vcon_name (* return the field value of the value constructor *)
  | Noexpr
  | Nomatch

type def =
  | Function of typ * name * arg_name list * expr
  | Datatype of typ * constructor list (* !! *)
  | Variable of typ * name * expr
  | Exp of expr
  | CheckTypeError of def

and constructor = vcon_name * typ (* !! *)

let rec string_of_expr exp =
  let flat_string_of_exp = function
    | Literal l -> A.string_of_literal l
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
        string_of_expr e1 ^ " " ^ A.string_of_binop o ^ " " ^ string_of_expr e2
    | Unop (o, e) -> A.string_of_uop o ^ string_of_expr e
    | Lambda (t, vl, e) ->
        "\\(" ^ string_of_typ t ^ ")" ^ String.concat " " vl ^ " -> "
        ^ string_of_expr e
    | Construct (n, e) -> n ^ string_of_expr e
    | Let (vl, e) ->
        "let "
        ^ String.concat ", "
            (List.map (fun (v, e) -> v ^ " = " ^ string_of_expr e) vl)
        ^ " in " ^ string_of_expr e
    | Tuple es -> "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
    | At (e, i) -> string_of_expr e ^ "." ^ string_of_int i
    | Noexpr -> ""
    | Nomatch -> "No match"
    | Thunk e -> "THUNK: " ^ string_of_expr e
    | GetField (e, name) -> string_of_expr e ^ "@" ^ name
    | GetTag e -> string_of_expr e ^ ".T"
  in

  match exp with Noexpr -> "" | _ -> "(" ^ flat_string_of_exp exp ^ ")"

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
  | Variable (ty, name, e) ->
      string_of_typ ty ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";"
  | CheckTypeError e -> "check_type_error " ^ string_of_def e

let string_of_program defs =
    String.concat "\n" (List.map string_of_def defs)
