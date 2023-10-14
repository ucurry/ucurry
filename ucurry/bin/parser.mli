
(* The type of tokens. *)

type token = 
  | WILDCARD
  | UNITTYPE
  | UNIT
  | TL
  | TIMES
  | THEN
  | SUB
  | STRTYPE
  | STRINGLIT of (string)
  | SEMI
  | RBRACKET
  | RBRACE
  | OR
  | OF
  | NOT
  | NEQ
  | NEG
  | NAME of (string)
  | MOD
  | LISTTYPE
  | LET
  | LESS
  | LEQ
  | LBRACKET
  | LBRACE
  | LAMBDA
  | INTTYPE
  | INTEGER of (int)
  | IN
  | IF
  | HD
  | GREATER
  | GEQ
  | FUNCTION
  | EQUAL
  | EOF
  | ELSE
  | DOUBLEARROW
  | DIVIDE
  | DATATYPE
  | CONS
  | COMMA
  | COLON
  | CASE
  | CAPNAME of (string)
  | BOOLTYPE
  | BOOL of (bool)
  | BEGIN
  | BAR
  | ASN
  | ARROW
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val program: Lexing.position -> (Ast.program) MenhirInterpreter.checkpoint
  
end
