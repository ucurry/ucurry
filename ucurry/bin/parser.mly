%{
    open Ast 
%}

%token SEMI COMMA COLON LBRACE RBRACE LBRACKET RBRACKET ARROW BAR QUOTE
// keyword
%token FUNCTION LAMBDA DATATYPE IF THEN ELSE LET BEGIN 
// type
%token INTTYPE STRTYPE LISTTYPE BOOLTYPE UNITTYPE 
// binop 
%token ASN ADD SUB TIMES DIVIDE MOD EQUAL NEQ LESS LEQ GREATER GEQ AND OR CONS
// unop 
%token HD TL NEG NOT 

// primitive literal 
%token <string> CONSTRUCTOR // TODO: enforce captilization 
%token <string> NAME 
%token <string> STRINGLIT
%token <int> INTEGER
%token <bool> BOOL 
%token EOF 

%start program 
%type <Ast.program> program

// TODO: add tokens , token name should be consistent in the scanner 


%%
program:
    defs EOF { List.rev $1 }

defs: // TODO: reverse the list of definition at the end  
    /* nothing */   { [] }
    | defs fundef SEMI   { $2 :: $1 }
    // | defs datatypedef SEMI  { $2 :: $1 }
    | defs vardef SEMI  { $2 :: $1 }
    | defs exp SEMI     { (Exp $2) :: $1 }

fundef:
    NAME COLON funtype COLON formals ASN exp SEMI 
    { Function ($3, $1, List.rev $5 , $7) }

formals:
    NAME { [$1] }
  | formals NAME { $2 :: $1 }
    
// datatypedef: 
    // DATATYPE CONSTRUCTOR ASN constuctorlist { Datatype ($2, $4) }

vardef: 
    typ NAME ASN exp
    { Variable ($1, $2, $4) }

funtype:
    typ ARROW typ { FUNCTION_TY ($1, $3) } 

typ:
    INTTYPE  { INT_TY }  
  | STRTYPE  { STRING_TY }  
  | LISTTYPE { BOOL_TY }  
  | BOOLTYPE { LIST_TY }  
  | UNITTYPE { UNIT_TY } 

literal:
    QUOTE STRINGLIT QUOTE  { STRING $2 } 
  | INTEGER             { INT $1 }
  | BOOL                { BOOL $1 }
  | LBRACKET literal_list RBRACKET { LIST (List.rev $2) } 

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

exp:
    literal             {Literal $1 }
  | NAME                { Var $1 }
  | NAME ASN exp        { Assign ($1, $3) }
  | NAME args           { Apply ($1, List.rev $2) }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
//   // | LET 
  | BEGIN exp_list      { Begin (List.rev $2) }
  | exp ADD   exp       { Binop($1, Add,   $3)   }
  | exp SUB    exp      { Binop($1, Sub,   $3)   }
  | exp TIMES  exp      { Binop($1, Mult,  $3)   }
  | exp DIVIDE exp      { Binop($1, Div,   $3)   }
  | exp EQUAL  exp      { Binop($1, Equal, $3)   }
  | exp NEQ    exp      { Binop($1, Neq,   $3)   }
  | exp LEQ    exp      { Binop($1, Leq,   $3)   }
  | exp GEQ    exp      { Binop($1, Geq,   $3)   }
  | exp AND    exp      { Binop($1, And,   $3)   }
  | exp OR     exp      { Binop($1, Or,    $3)   }
  | exp CONS   exp      { Binop($1, Cons,  $3)   }
  | HD exp              { Unop(Hd, $2) }
  | TL exp              { Unop(Tl, $2) }
  | NEG  exp            { Unop(Neg, $2)}
  | NOT  exp            { Unop(Not, $2) }

    