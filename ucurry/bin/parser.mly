%{
    open Ast 
%}

// delimiters
%token ARROW DOUBLEARROW SEMI COMMA COLON LBRACE RBRACE LBRACKET RBRACKET BAR DOTS
// keyword
%token FUNCTION LAMBDA DATATYPE IF THEN ELSE LET BEGIN IN CASE OF WILDCARD CHECK_TYPE_ERROR 
// type
%token INTTYPE STRTYPE LISTTYPE BOOLTYPE UNITTYPE 
// binop 
%token ASN ADD SUB TIMES DIVIDE MOD EQUAL NEQ LESS LEQ GREATER GEQ AND OR CONS 
// unop 
%token HD TL NEG NOT

// primitive value 
%token <string> CAPNAME
%token <string> NAME 
%token <string> STRINGLIT
%token <int> INTEGER
%token <bool> BOOL 
%token UNIT
%token EOF 


%nonassoc IN ELSE 
%right ARROW
%right ASN 
%left OR 
%left AND 
%left CONS 
%left NEQ EQUAL LESS LEQ GREATER GEQ 
%left ADD SUB  
%left TIMES DIVIDE MOD 
%right HD TL NEG NOT 
%left LISTTYPE


%start program 
%type <Ast.program> program

%%
program:
    defs EOF { List.rev $1 }



defs:   
    /* nothing */            { [] }
    | defs def SEMI       { $2 :: $1 }

def: 
  | fundef {$1}
  | vardef {$1}
  | datatypedef {$1}
  | exp {(Exp $1)}
  | CHECK_TYPE_ERROR def { CheckTypeError $2}

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
    CAPNAME        { ($1, None) }
  | CAPNAME OF typ { ($1, Some $3) }

exp:
    LBRACE exp RBRACE         { $2 }
  | value                   { Literal $1 }
  | NAME                      { Var $1 }
  | NAME ASN exp              { Assign ($1, $3) }
  | LBRACE exp args RBRACE    { Apply ($2, List.rev $3) }
  | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }
  | LET bindings IN exp       { Let (List.rev $2, $4) }
  | LBRACE BEGIN exp_list RBRACE        { Begin (List.rev $3) }
  | binop                     { $1 }
  | unop                      { $1 }
  | lambda                    { $1 }
  // fix: adding brackets to value constructors avoids shift/reduce conflict
  | LBRACE CASE exp OF case_exp_list RBRACE { Case ($3, List.rev $5) }


exp_opt:
    /* nothing */ { Noexpr }
  | exp           { $1 }

case_exp_list:
    pattern DOUBLEARROW exp { [($1, $3)] }
  | case_exp_list BAR pattern DOUBLEARROW exp { ($3, $5) :: $1 }

pattern:
    NAME                { VAR_PAT $1 }
  | CAPNAME             { CON_PAT ($1, None) }
  | CAPNAME pattern     { CON_PAT ($1, Some [$2])}
  | CAPNAME LBRACE pattern_tuple RBRACE { CON_PAT ($1, Some (List.rev $3)) } // currently our parser doesn't support tuple type, thus a value constructor can at most take in one arg
  | WILDCARD            { WILDCARD }
  | LBRACKET RBRACKET   { NIL }
  | NAME CONS NAME      { CONCELL ($1, $3) }

pattern_tuple:
    pattern COMMA pattern       { [$3; $1] }
  | pattern_tuple COMMA pattern { $3 :: $1 }

lambda:
  LAMBDA LBRACE funtype RBRACE formals ARROW exp { Lambda($3, List.rev $5, $7) }

bindings:
    typ NAME ASN exp          { [(($1, $2), $4)] }
  | bindings COMMA typ NAME ASN exp { (($3, $4), $6):: $1 }


formals: // functions have to have arguments
  | UNIT         { [] }
  | NAME         { [$1] }
  | formals NAME { $2 :: $1 }

funtype:
   | LBRACE funtype RBRACE { $2 } 
   | typ ARROW typ { FUNCTION_TY ($1, $3) } 

tupletype:
    LBRACE typelist RBRACE      { TUPLE_TY (List.rev $2) }

typelist:
     typ TIMES typ      { [$3 ; $1] }
   | typelist TIMES typ { $3 :: $1 }

typ:
    INTTYPE             { INT_TY }  
  | STRTYPE             { STRING_TY }  
  | BOOLTYPE            { BOOL_TY }  
  | UNITTYPE            { UNIT_TY }
  | CAPNAME             { CONSTRUCTOR_TY $1}
  | typ LISTTYPE        { LIST_TY $1 }  
  | funtype             { $1 }
  | tupletype           { $1 }

value:
    STRINGLIT                      { STRING $1 } 
  | INTEGER                        { INT $1 }
  | BOOL                           { BOOL $1 }
  | LBRACKET literal_list RBRACKET { LIST (List.rev $2) } 
  | LBRACE literal_tuple RBRACE     { TUPLE (List.rev $2)}
  | UNIT                           { UNIT }
  | LBRACKET INTEGER DOTS RBRACKET { INF_LIST $2 }
  | LBRACE CAPNAME  exp_opt RBRACE { Construct ($2, $3) }

literal_list:
                               { [] }
  | value                    { [$1] }
  | literal_list COMMA value { $3 :: $1}

literal_tuple:
    value  COMMA value     { [$3; $1] }
  | literal_tuple COMMA value { $3 :: $1 }

exp_list: 
    exp { [$1] }
  | exp_list COMMA exp  { $3 :: $1 }

args:
  | exp { [$1] }
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
