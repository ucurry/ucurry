(* Abstract Syntax Tree and functions for printing it *)

module A = Ast

type sexpr = A.typ * expr

and expr =
  | Literal of Sast.svalue
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

and case_expr = Sast.pattern * sexpr
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

let rec string_of_literal : Sast.svalue -> string = function
  | Sast.INT l -> string_of_int l
  | Sast.STRING l -> "\"" ^ l ^ "\""
  | Sast.BOOL l -> string_of_bool l
  | Sast.EMPTYLIST -> "[]"
  | Sast.LIST (x, xs) ->
      let rec listString (x, xs) =
        match (x, xs) with
        | x, Sast.EMPTYLIST -> string_of_literal x
        | x, Sast.LIST (y, ys) -> string_of_literal x ^ "," ^ listString (y, ys)
        | _ -> raise (Invalid_argument "should not be reached")
      in
      "[" ^ listString (x, xs) ^ "]"
  | Sast.TUPLE l ->
      "(" ^ String.concat ", " (List.map string_of_literal l) ^ ")"
  | Sast.UNIT -> "()"
  | Sast.INF_LIST n -> "[" ^ string_of_int n ^ "..]"
  | Sast.Construct ((s, _, _), e) -> "(" ^ s ^ " " ^ string_of_literal e ^ ")"

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
      | Literal l -> string_of_literal l
      | Assign _ -> "assign"
      | Var x -> x
      | Apply _ -> "apply"
      | Begin el ->
          "(begin " ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | Binop (e1, o, e2) ->
          string_of_sexpr e1 ^ " " ^ Ast.string_of_binop o ^ " "
          ^ string_of_sexpr e2
      | Unop (o, e) -> A.string_of_uop o ^ " " ^ string_of_sexpr e
      | If (e1, e2, e3) ->
          "if " ^ string_of_sexpr e1 ^ " then \n" ^ string_of_sexpr e2
          ^ " else \n" ^ string_of_sexpr e3
      | Closure ((_, (_, e)), _) -> string_of_expr e
      | Let (vl, e) ->
          "let "
          ^ String.concat ", "
              (List.map
                 (fun (v, e) ->
                    v ^ " = " ^ string_of_sexpr e)
                 vl)
          ^ " in \n" ^ string_of_sexpr e
      | Noexpr -> ""
      | _ -> raise (Failure "String_of_expr Not implemented For Most Cases")
    in
    match exp with Noexpr -> "" | _ -> "(" ^ flat_string_of_exp exp ^ ")"
  in
  string_of_typ ty ^ " " ^ string_of_expr tope

let string_of_def = function
  | Exp se -> string_of_sexpr se
  | Val ( name, e) ->
       name ^ " " ^ string_of_sexpr e
  | _ -> raise (Failure "String_of_def Not implemented For Most Cases")

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)
