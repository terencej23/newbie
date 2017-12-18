%{
  open Ast 
%}

/* TODO: lists, iterations, breaks */

%token LPAREN RPAREN COMMA ATTR
%token NEWLINE INDENT DEDENT
%token RETURN DEF FUNC WITH NO PARAMS IF ELSE FOR WHILE EACH IN 
%token BREAK
%token PLUS MINUS NEG MULT DIVIDE MOD EQUALS ASSIGN TO
%token GT LT GEQ LEQ AND OR NOT TRUE FALSE
%token EOF

%token <int>    INTLIT
%token <float>  FLOATLIT
%token <string> STRLIT
%token <string> ID

%nonassoc NOELSE 
%nonassoc ELSE
%left OR
%left AND
%left EQUALS
%left LT GT LEQ GEQ
%left PLUS MINUS
%left MULT DIVIDE MOD
%right NOT NEG

%start program
%type <Ast.program> program

%%


program:
    decls EOF               { $1 }
  | NEWLINE decls           { $2 }

decls:
    /* nothing */     { [], [] }
  | decls vinit       { ($2 :: fst $1), snd $1 } 
  | decls fdecl       { fst $1, ($2 :: snd $1) }

fdecl:
    fdecl_params      { $1 }
  | fdecl_noparams    { $1 }

fdecl_params:
    DEF FUNC ID WITH PARAMS LPAREN params_opt RPAREN NEWLINE INDENT stmt_list DEDENT
    {{
      fname = $3 ;
      formals = $7 ;
      body = List.rev $11 ;
    }}

fdecl_noparams:
  DEF FUNC ID WITH NO PARAMS NEWLINE INDENT stmt_list DEDENT
  {{
    fname = $3 ;
    formals = [] ;
    body = List.rev $9
  }}

params_opt:
    /* nothing */   { [] }
  | param_list      { List.rev $1 }

param_list:
    ID                    { [$1] }
  | param_list COMMA ID   { $3 :: $1 } 

iteration_stmt:
  WHILE LPAREN expr RPAREN NEWLINE compound_stmt  { While($3, $6) }

stmt_list:
    /* nothing */   { [] }
  | stmt_list stmt  { $2 :: $1 }

stmt:
    expr_stmt     { $1 }
  | select_stmt   { $1 }
  | assign_stmt   { $1 }
  | compound_stmt { $1 }
  | jump_stmt     { $1 }
  | iteration_stmt{ $1 }

expr_stmt:
    expr NEWLINE  { Expr $1 }

select_stmt:
    IF LPAREN expr RPAREN NEWLINE compound_stmt %prec NOELSE         
      { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN NEWLINE compound_stmt ELSE NEWLINE compound_stmt
      { If($3, $6, $9) }

assign_stmt:
    ASSIGN ID TO expr NEWLINE       { Assign($2, $4) } 

compound_stmt:
    INDENT stmt_list DEDENT         { Block(List.rev $2)}

jump_stmt:
  | BREAK NEWLINE { Break }
  | RETURN expr NEWLINE     { Return($2) }

expr:
    literals                      { $1 }
  | expr PLUS expr                { Binop($1, Add, $3) }
  | expr MINUS expr               { Binop($1, Sub, $3) }
  | expr MULT expr                { Binop($1, Mult, $3) }
  | expr DIVIDE expr              { Binop($1, Div, $3) }
  | expr MOD expr                 { Binop($1, Mod, $3) }
  | expr EQUALS expr              { Binop($1, Eq, $3) }
  | expr LT expr                  { Binop($1, Lt, $3) }
  | expr GT expr                  { Binop($1, Gt, $3) }
  | expr LEQ expr                 { Binop($1, Leq, $3) }
  | expr GEQ expr                 { Binop($1, Geq, $3) }
  | expr AND expr                 { Binop($1, And, $3) }
  | expr OR expr                  { Binop($1, Or, $3) }
  | MINUS expr %prec NEG          { Unop(Neg, $2) }
  | NOT expr                      { Unop(Not, $2) }
  | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }
  | LPAREN expr RPAREN            { $2 }

literals:
    INTLIT      { IntLit($1) }
  | FLOATLIT    { FloatLit($1) }
  | STRLIT      { StrLit($1) }
  | TRUE        { BoolLit(true) }
  | FALSE       { BoolLit(false) }
  | ID          { Id($1) }

vinit:
    ASSIGN ID TO expr NEWLINE   { ($2, $4) }

actuals_opt:
    /* nothing */       { [] }
  | actuals_list        { List.rev $1 }

actuals_list:
    expr                    { [$1] } 
  | actuals_list COMMA expr { $3 :: $1 }
