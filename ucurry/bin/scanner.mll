(* Ocamllex scanner for uCurry *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let char = ['a' - 'z' 'A' - 'Z']
let string = char+
let simple_char = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let escape_char = ['t' 'r' 'n' '\'' '\"' '\\']

rule token = parse 
  [' ' '\t' '\r' '\n'] { token lexbuf }
| eof                  { EOF }

(* delimiters *)
| "--"       { comment lexbuf }
| "->"       { ARROW }
| "=>"       { DOUBLEARROW }
| ';'        { SEMI }
| ','        { COMMA }
| ':'        { COLON }
| '('        { LBRACE }
| ')'        { RBRACE }
| '['        { LBRACKET }
| ']'        { RBRACKET }
| '|'        { BAR }
| ".."       { DOTS }
| "."        { DOT }

(* keyword *)
| "check_type_error" { CHECK_TYPE_ERROR } 
| "fun"      { FUNCTION }
| "\\"       { LAMBDA }
| "datatype" { DATATYPE }
| "if"       { IF }
| "then"     { THEN }
| "else"     { ELSE }
| "let"      { LET }
| "begin"    { BEGIN }
| "in"       { IN }
| "case"     { CASE }
| "of"       { OF }
| '_'        { WILDCARD }

(* typ *)  
| "int"      { INTTYPE }
| "string"   { STRTYPE }
| "bool"     { BOOLTYPE }
| "list"     { LISTTYPE }
| "unit"     { UNITTYPE }

(* binop *)  
| '='        { ASN }
| '+'        { ADD }
| "-"        { SUB }
| "*"        { TIMES }    
| "/"        { DIVIDE }
| "%"        { MOD }
| "=="       { EQUAL }
| "!="       { NEQ }
| '<'        { LESS }
| "<="       { LEQ }
| '>'        { GREATER }
| ">="       { GEQ }
| "and"      { AND }
| "or"       { OR }
| "::"       { CONS }

(* unop *)  
| "hd"       { HD }
| "tl"       { TL }
| "not"      { NOT }
| "~"        { NEG }
| "print"    { PRINT }
| "println"  { PRINTLN }
| "null?"    { ISNULL }


(* literal   *)
| "true"     { BOOL(true) }
| "false"    { BOOL(false) }
| "()"       { UNIT }
| digits as lxm{ INTEGER(int_of_string lxm) }
| '\"'((simple_char | '\\'escape_char)* as lxm)'\"' { STRINGLIT(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z']* as lxm { CAPNAME(lxm)  }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm{ NAME(lxm) }
| _ as char{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse 
  ['\r' '\n'] { token lexbuf }
| eof         { EOF }
| _           { comment lexbuf }
