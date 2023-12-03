(* Abstract Syntax Tree and functions for printing it *)

module A = Ast
module S = Sast

type sexpr = A.typ * expr

and expr =
  | Literal of S.svalue
  | Var of string
  | Assign of string * thunk
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of (string * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Captured of int
  | Closure of closure
  | Case of sexpr * case_expr list
  | At of sexpr * int
  | Noexpr

and case_expr = S.pattern * sexpr
and closure = (string list * sexpr) * sexpr list (* (lambda, captured list) *)
and thunk = sexpr (* which will be a Closure form *)

and def =
  | Val of string * sexpr
  | Function of A.typ * string * closure
  | Datatype of A.typ * A.constructor list
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * A.typ option

type program = def list

(* Pretty-printing functions *)

let rec string_of_closure (((args, body), captured) : closure) : string =
  let args = "(" ^ String.concat ", " args ^ ", closure)" in
  let body_string = string_of_sexpr "\t" body in
  let captured_vars =
    List.fold_left
      (fun acc cap -> acc ^ " " ^ string_of_sexpr "" cap)
      "" captured
  in
  "\n(mkClosure " ^ args ^ "\n" ^ "\t" ^ body_string ^ "\n" ^ "["
  ^ captured_vars ^ "]" ^ ")"

and string_of_sexpr (delim : string) ((ty, expr) : sexpr) : string =
  let string_of_expr (exp : expr) : string =
    match exp with
    | Literal l -> S.string_of_literal l
    | Var x -> x
    | Assign (name, thunk) -> name ^ " = " ^ string_of_sexpr delim thunk
    | Apply (sexpr, tlist) ->
        let expr_string = string_of_sexpr delim sexpr in
        let args_string =
          List.fold_left
            (fun acc thunk -> acc ^ " " ^ string_of_sexpr delim thunk)
            "" tlist
        in
        "APP(" ^ expr_string ^ args_string ^ ")"
    | If (e1, e2, e3) ->
        "If " ^ string_of_sexpr delim e1 ^ " then \n" ^ delim
        ^ string_of_sexpr delim e2 ^ " else \n" ^ delim
        ^ string_of_sexpr delim e3
    | Let (vl, e) ->
        "let "
        ^ String.concat ", "
            (List.map
               (fun (name, thunk) -> name ^ " = " ^ string_of_sexpr delim thunk)
               vl)
        ^ " in \n" ^ delim ^ string_of_sexpr delim e
    | Begin el ->
        "(begin "
        ^ String.concat (", " ^ delim) (List.map (string_of_sexpr delim) el)
        ^ ")"
    | Binop (e1, o, e2) ->
        string_of_sexpr delim e1 ^ " " ^ Ast.string_of_binop o ^ " "
        ^ string_of_sexpr delim e2
    | Unop (o, e) -> A.string_of_uop o ^ " " ^ string_of_sexpr delim e
    | Captured i -> "Captured " ^ string_of_int i
    | Closure cl -> string_of_closure cl
    | Case _ -> failwith "String_of_expr Not implemented for case"
    | At _ -> failwith "String_of_exor Not implemented for at"
    | Noexpr -> ""
  in
  "(" ^ A.string_of_typ ty ^ "," ^ string_of_expr expr ^ ")"

let string_of_def = function
  | Val (name, e) -> name ^ " " ^ string_of_sexpr "" e
  | Function (ty, name, body) ->
      "fun: " ^ Ast.string_of_typ ty ^ ":\n" ^ name ^ " "
      ^ string_of_closure body
  | Datatype _ -> failwith "String_of_def Not implemented datatype"
  | Exp se -> string_of_sexpr "" se
  | CheckTypeError _ -> failwith "String_of_def Not implemented checktypeerror"

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
