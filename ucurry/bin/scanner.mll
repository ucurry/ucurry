(* Ocamllex scanner for uCurry *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let char = ['a' - 'z' 'A' - 'Z']
let string = char+

rule token = parse 
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "--" { comment lexbuf }
| "->" {ARROW}
| ':'  {COLON}
| ';'  { SEMI }
| "==" {EQUAL}
| '='  {ASN}
| "int" {INTTYPE}
| '<'      { LESS }
| '>'      { GREATER }
| "<="     { LEQ }
| ">="     { GEQ }
| "!="     { NEQ }
| eof      { EOF }
| digits as lxm { INTEGER(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { NAME(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse 
    ['\r' '\n'] { token lexbuf}
| _ { comment lexbuf }