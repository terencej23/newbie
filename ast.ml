type binop = Add | Sub | Div | Mul | Or | And | Leq | Geq | Eq 
type unop = Neg | Neq

type expression =
    | Str_lit of string
    | Num_lit of float
    | Bool_lit of bool
    | Binop of expression * binop * expression
    | Unop of unop * expression
    | Id of string 
    | Call of string * expression list    

type stmt = 
    | If of expression * stmt * stmt
    | While of expression * stmt
    | For of expression * expression * expression
    | Expr of expression
    | Return of expression 
    | Assign of string * expression 

type fun_decl =  {
    fname : string
    formals : string list 
    body : stmt list
}

type global =  string * expr 
type program = global list * fun_decl list
