%{
    open Ast 
%}

// delimiters
%token ARROW DOUBLEARROW SEMI COMMA COLON LBRACE RBRACE LBRACKET RBRACKET BAR 
// keyword
%token FUNCTION LAMBDA DATATYPE IF THEN ELSE LET BEGIN IN CASE OF WILDCARD
// type
%token INTTYPE STRTYPE LISTTYPE BOOLTYPE UNITTYPE 
// binop 
%token ASN ADD SUB TIMES DIVIDE MOD EQUAL NEQ LESS LEQ GREATER GEQ AND OR CONS
// unop 
%token HD TL NEG NOT 

// primitive literal 
%token <string> CAPNAME
%token <string> NAME 
%token <string> STRINGLIT
%token <int> INTEGER
%token <bool> BOOL 
%token UNIT
%token EOF 

%start program 
%type <Ast.program> program

%%
program:
    defs EOF { List.rev $1 }

defs: // TODO: reverse the list of definition at the end  
    /* nothing */            { [] }
    | defs fundef SEMI       { $2 :: $1 }
    | defs vardef SEMI       { $2 :: $1 }
    | defs datatypedef SEMI  { $2 :: $1 }
    | defs exp SEMI          { (Exp $2) :: $1 }

fundef:
    FUNCTION COLON funtype COLON NAME formals ASN exp
    { Function ($3, $5, List.rev $6, $8) }
    
vardef: 
    typ NAME ASN exp { Variable ($1, $2, $4) }

datatypedef:
    DATATYPE CAPNAME ASN constructor_list { Datatype ($2, List.rev $4) }

constructor_list:
                 { [] }
  | constructor  { [$1] }
  | constructor_list BAR constructor { $3 :: $1 }

constructor:
    CAPNAME        { ValCon ($1, None) }
  | CAPNAME OF typ { ValCon ($1, Some $3) }

exp:
    literal                   { Literal $1 }
  | NAME                      { Var $1 }
  | NAME ASN exp              { Assign ($1, $3) }
  | LBRACE exp args RBRACE    { Apply ($2, List.rev $3) }
  | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }
  | LET bindings IN exp       { Let (List.rev $2, $4) }
  | BEGIN exp_list            { Begin (List.rev $2) }
  | binop                     { $1 }
  | unop                      { $1 }
  | lambda                    { $1 }
  | CAPNAME exp_opt           { Construct ($1, $2) }
  | CASE exp OF case_exp_list { Case ($2, List.rev $4) }

exp_opt:
    /* nothing */ { Noexpr }
  | exp           { $1 }

case_exp_list:
    pattern DOUBLEARROW exp { [($1, $3)] }
  | case_exp_list BAR pattern DOUBLEARROW exp { ($3, $5) :: $1 }

pattern:
    NAME             { VAR_PAT $1 }
  | literal          { LIT_PAT $1 }
  | CAPNAME          { CON_PAT ($1, None) }
  | CAPNAME pattern  { CON_PAT ($1, Some $2) } // currently our parser doesn't support tuple type, thus a value constructor can at most take in one arg
  | WILDCARD         { WILDCARD }

lambda:
  LAMBDA LBRACE funtype RBRACE formals ARROW exp { Lambda($3, $5, $7) }

bindings:
    typ NAME ASN exp          { [($1, $2, $4)] }
  | bindings COMMA typ NAME ASN exp { ($3, $4, $6):: $1 }


formals: // functions have to have arguments
    NAME         { [$1] }
  | formals NAME { $2 :: $1 }

funtype:
    typ ARROW typ { FUNCTION_TY ($1, $3) } 

typ:
    INTTYPE  { INT_TY }  
  | STRTYPE  { STRING_TY }  
  | LISTTYPE { BOOL_TY }  
  | BOOLTYPE { LIST_TY }  
  | UNITTYPE { UNIT_TY }
  | CAPNAME  { CONSTRUCTOR_TY $1}
  | funtype  { $1 }

literal:
    STRINGLIT                      { STRING $1 } 
  | INTEGER                        { INT $1 }
  | BOOL                           { BOOL $1 }
  | LBRACKET literal_list RBRACKET { LIST (List.rev $2) } 
  | UNIT                           { UNIT }

literal_list:
                               { [] }
  | literal                    { [$1] }
  | literal_list COMMA literal { $3 :: $1}

exp_list: 
    exp { [$1] }
  | exp_list COMMA exp  { $3 :: $1 }

args:
    exp { [$1] }
  | args exp { $2 :: $1 }

binop:
  | exp ADD   exp       { Binop ($1, Add,   $3) }
  | exp SUB    exp      { Binop ($1, Sub,   $3) }
  | exp TIMES  exp      { Binop ($1, Mult,  $3) }
  | exp DIVIDE exp      { Binop ($1, Div,   $3) }
  | exp MOD    exp      { Binop ($1, Mod,   $3) }
  | exp EQUAL  exp      { Binop ($1, Equal, $3) }
  | exp NEQ    exp      { Binop ($1, Neq,   $3) }
  | exp LESS  exp       { Binop ($1, Less,  $3) }
  | exp LEQ    exp      { Binop ($1, Leq,   $3) }
  | exp GREATER  exp    { Binop ($1, Greater, $3) }
  | exp GEQ    exp      { Binop ($1, Geq,   $3) }
  | exp AND    exp      { Binop ($1, And,   $3) }
  | exp OR     exp      { Binop ($1, Or,    $3) }
  | exp CONS   exp      { Binop ($1, Cons,  $3) }

unop:
  | HD exp              { Unop (Hd, $2) }
  | TL exp              { Unop (Tl, $2) }
  | NEG  exp            { Unop(Neg, $2)}
  | NOT  exp            { Unop (Not, $2) }
