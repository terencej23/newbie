type binop = Add | Sub | Div | Mod | Mult | Or | And | Lt | Leq | Gt | Geq | Eq 
type unop = Neg | Not

type expr =
    | StrLit of string
    | IntLit of int
    | FloatLit of float
    | BoolLit of bool
    | Binop of expr * binop * expr
    | Unop of unop * expr
    | Id of string 
    | Call of string * expr list

type stmt = 
    | Block of stmt list
    | If of expr * stmt * stmt
    | While of expr * stmt
    | For of expr * expr * expr
    | Expr of expr
    | Return of expr 
    | Assign of string * expr 

type fun_decl =  {
  fname : string;
  formals : string list;
  body : stmt list;
}

type global =  string * expr 
type program = global list * fun_decl list
