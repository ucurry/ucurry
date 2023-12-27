(* Closures Abstract Syntax Tree and functions for printing it *)
(* Authors: Stephanie Xu, Vivian Li, Matt Zhou *)

open Typing
module A = Ast
module S = Sast

type sexpr = typ * expr

and expr =
  | Literal of S.svalue
  | Var of string
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of (string * thunk) list * sexpr
  | Letrec of (string * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Captured of int
  | Closure of closure
  | Construct of (vcon_id * vcon_name) * sexpr
  | Tuple of sexpr list
  | At of sexpr * int
  | List of sexpr * sexpr
  | EmptyList of typ
  | GetTag of sexpr
  | GetField of sexpr * int
  | Noexpr
  | Nomatch
  | Thunk of sexpr
  | Force of sexpr
  | GetEvaled of sexpr
  | GetValue of sexpr 
  | GetClosure of sexpr
  | SetEvaled of sexpr
  | SetValue of sexpr * sexpr

(* and case_expr = S.pattern * sexpr *)
and closure = (string list * sexpr) * sexpr list (* (lambda, captured list) *)
and thunk = sexpr (* which will be a Closure form *)

and def =
  | Val of string * sexpr
  | Function of typ * string * closure
  | Datatype of typ * A.constructor list
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * typ

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
    | Letrec (vl, e) ->
        "letrec "
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
    | Construct ((_, vcon_name), arg) ->
        "(" ^ vcon_name ^ " " ^ string_of_sexpr delim arg ^ ")"
    | Tuple _ -> failwith "String_of_expr Not implemented for Tuple"
    | At _ -> failwith "String_of_exor Not implemented for at"
    | GetField (e, i) -> string_of_sexpr delim e ^ "@" ^ string_of_int i
    | GetTag e -> string_of_typ ty ^ " " ^ string_of_sexpr delim e
    | Noexpr -> ""
    | Nomatch -> "No match"
    | List (_, _) ->
        failwith "string_of_expr not implemented for non-empty list"
    | EmptyList _ -> "[]"
    | Thunk t -> "(Thunk: " ^ string_of_sexpr delim t ^ ")"
    | Force e -> string_of_sexpr delim e ^ ".force"
    | GetEvaled e -> "(GetEvaled " ^ string_of_sexpr delim  e ^ ")"
    | GetValue e -> "(GetValue " ^ string_of_sexpr delim  e ^ ")"
    | GetClosure e -> "(GetClosure " ^ string_of_sexpr delim  e ^ ")"
    | SetEvaled e -> "(SetEvaled " ^ string_of_sexpr delim  e ^ ")"
    | SetValue (e1, e2) -> "(SetValue " ^ string_of_sexpr delim  e1 ^ " " ^ string_of_sexpr delim  e2 ^ ")"
    | _ -> failwith "cast pp not implemented for forceeval"
  in
  "(" ^ string_of_typ ty ^ "," ^ string_of_expr expr ^ ")"

let string_of_def = function
  | Val (name, e) -> name ^ " " ^ string_of_sexpr "" e
  | Function (ty, name, body) ->
      "fun: " ^ string_of_typ ty ^ ":\n" ^ name ^ " " ^ string_of_closure body
  | Datatype (ty, cls) ->
      "datatype " ^ string_of_typ ty ^ " = "
      ^ String.concat " | " (List.map A.string_of_constructor cls)
      ^ ";"
  | Exp se -> string_of_sexpr "" se
  | CheckTypeError _ -> failwith "String_of_def Not implemented checktypeerror"

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
