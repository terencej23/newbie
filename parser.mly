%{open Ast 
module StringMap = Map.Make(String) %}

%token SEMI COLON LPAREN RPAREN LBRACK RBRACK COMMA 
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT UNION
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token RETURN DEFINE FUNCTION IF ELIF ELSE WHILE 
%token BREAK 
%token INT FLOAT VOID STRING BOOL ARRAY
%token <int> NUMLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
/*%nonassoc ID */
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left TIMES DIVIDE MOD UNION 
%left LBRACK 
%right NOT NEG 

%start program
%type <Ast.program> program

%%

program:
    decls EOF { $1 }

decls: 
    /* nothing */ { [], [] }
    | decls vinit { ($2 :: fst $1), snd $1 } 
    | decls fdecl { fst $1, ($2 :: snd $1) } 

fdecl:
    DEFINE ID LPAREN params_opt RPAREN stmt_list COLON
    { { fname = $2; 
        formals = $4 ; 
        body = List.rev $6 } }
 

params_opt:
    /* nothing */ {[]}
    | param_list { List.rev $1 }

param_list:
    ID { [$1] } 
    | param_list COMMA ID { $3 :: $1 } 

stmt_list:
    /* nothing */ { [] }
    | stmt_list stmt { $2 :: $1 }

stmt:
    | expr_stmt{ $1 }
    | select_stmt { $1 } 
    | assign_stmt {  $1 } 
    /*| compound_stmt { $1 }*/
    /*| iteration_stmt { $1 }*/
    | jump_stmt { $1 }

expr_stmt:
    expr SEMI { Expr $1 } 

select_stmt: 
    IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
    | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }

assign_stmt:
    ID ASSIGN expr SEMI { Assign($1, $3) }
    | ID LBRACK expr RBRACK ASSIGN expr SEMI { SetElementAssign($1, $3, $6)}


/*compound_stmt:
    LBRACE stmt_list RBRACE { Block(List.rev $2) }*/

/*iteration_stmt: 
    WHILE LPAREN expr RPAREN stmt { While($3, $5) }
    | LPAREN constraints_list expr_opt RPAREN stmt 
    { Iter( List.rev($2), $3, $5)  }*/
 
/*expr_opt:
     nothing  { Noexpr }
    | expr { $1 }*/


jump_stmt:
    | BREAK SEMI { Break }
    | RETURN expr SEMI { Return($2) }

expr: 
    literals           { $1 }
    | expr PLUS   expr { Binop($1, Add,   $3) } 
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr MOD    expr { Binop($1, Mod,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) } 
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | expr UNION  expr { Binop($1, Union, $3) }
    | MINUS expr %prec NEG { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
    | LPAREN expr RPAREN { $2 }


literals:
    NUMLIT { NumLit($1) }
    | STRLIT { StrLit($1)}
    | TRUE {BoolLit(true) }
    | FALSE { BoolLit(false) }
    | ID { Id($1) }


/*set: 
    LBRACK expr_list_opt RBRACK { Set($2) }
    | ID LBRACK expr RBRACK { SetAccess($1, $3) }
    | ID LBRACK expr COLON expr RBRACK { Slice($1, $3, $5) }*/
    

/*expr_list_opt:
     nothing  { [] }
    | expr_list { List.rev $1 }

expr_list:
    expr { [$1] }
    | expr_list COMMA expr { $3 :: $1 }*/

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

vinit:
   ID ASSIGN expr SEMI { ($1, $3) }