module A = Ast
module S = Sast

type sexpr = A.typ * expr

and expr =
  | Literal of S.svalue
  | Var of string
  | Assign of string * thunk
  | Apply of sexpr * thunk list
  | If of sexpr * sexpr * sexpr
  | Let of ((A.typ * string) * thunk) list * sexpr
  | Begin of sexpr list
  | Binop of sexpr * A.binop * sexpr
  | Unop of A.uop * sexpr
  | Case of sexpr * case_expr list
  | Lambda of lambda
  | At of sexpr * int
  | Noexpr

and case_expr = S.pattern * sexpr
and lambda = string list * sexpr
and thunk = sexpr (* which will be a Lambda (ty, lambda) form *)

and def =
  | Function of A.typ * string * sexpr
    (* TODO: lazy-convert would eventually convert all top-level Val to Val(ty, name, slambda) *)
  | Datatype of A.typ * A.constructor list
  | Exp of sexpr
  | CheckTypeError of def

and constructor = string * A.typ option

type program = def list

(* Pretty-printing functions *)
let rec string_of_lambdabody (args, body) =
    "(" ^ String.concat ", " args ^ ") " ^ string_of_sexpr "\t" body
and string_of_sexpr (delim: string) ((ty, expr) : sexpr) : string = 
  let typ_string =  A.string_of_typ ty ^ ":" in
  match expr with 
    | Literal l -> typ_string ^ S.string_of_literal l
    | Var x -> typ_string ^ x
    | Assign (name, thunk) -> typ_string ^ name ^ " = " ^ string_of_sexpr delim thunk 
    | Apply (sexpr, tlist) ->
        let expr_string = string_of_sexpr delim sexpr in
        let args_string =
          List.fold_left
            (fun acc thunk -> acc ^ " " ^ string_of_sexpr delim thunk)
            "" tlist
        in
        typ_string ^ "APP{(" ^ expr_string ^ ")" ^ args_string ^ "}"
    | If (e1, e2, e3) ->
      typ_string ^ "If " ^ string_of_sexpr delim e1 ^ " then \n" ^ delim ^ string_of_sexpr delim e2
        ^ " else \n" ^ delim ^ string_of_sexpr delim e3
    | Let (vl, e) ->
      typ_string ^ "let "
        ^ String.concat ", "
            (List.map
               (fun ((t, v), e) ->
                 A.string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr delim e)
               vl)
        ^ " in \n" ^ delim ^ string_of_sexpr delim e
    | Begin el ->
      typ_string ^ "(begin " ^ String.concat (", " ^ delim) (List.map (string_of_sexpr delim) el) ^ ")"
    | Binop (e1, o, e2) ->
      typ_string ^ string_of_sexpr delim e1 ^ " " ^ Ast.string_of_binop o ^ " "
        ^ string_of_sexpr delim e2
    | Unop (o, e) -> typ_string ^ A.string_of_uop o ^ " " ^ string_of_sexpr delim e
    | Lambda l -> "\\" ^ A.string_of_typ ty ^ " " ^ string_of_lambdabody l
    | Case _ -> failwith "String_of_expr Not implemented for case"
    | At _ -> failwith "String_of_exor Not implemented for at"
    | Noexpr -> ""

let string_of_def = function
  | Function (ty, name, (_, Lambda(l))) ->
      "fun: " ^ Ast.string_of_typ ty ^ ":\n" ^ name ^ " "
      ^ string_of_lambdabody l
  | Datatype _ -> failwith "String_of_def Not implemented datatype"
  | Exp se -> string_of_sexpr "" se
  | CheckTypeError _ -> failwith "String_of_def Not implemented checktypeerror"
  | _ -> failwith "impossible string_of_def last"

let string_of_program defs = String.concat "\n" (List.map string_of_def defs)