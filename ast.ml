type binop = Add | Sub | Div | Mod | Mult | Or | And | Lt | Leq | Gt | Geq | Eq 
type typ = Int | Void | String | Float | Bool 
type unop = Neg | Not
type datatype = Datatype of typ

type expr =
    | StrLit of string
    | IntLit of int
    | FloatLit of float
    | BoolLit of bool
    | Binop of expr * binop * expr
    | Unop of unop * expr
    | Id of string 
    | Call of string * expr list
    | Noexpr

type stmt = 
    | Block of stmt list
    | If of expr * stmt * stmt
    | While of expr * stmt
(*  | For of expr * expr * expr  *)
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


(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l)               -> string_of_int l
  | FloatLit(l)             -> string_of_float l
  | StrLit(s)               -> s
  | BoolLit(true)           -> "true"
  | BoolLit(false)          -> "false"
  | Id(s)                   -> s
  | Binop(e1, o, e2)        -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e)              -> string_of_uop o ^ string_of_expr e
  | Call(f, el)             -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr                  -> "noexpr"

let rec string_of_stmt = function
    Block(stmts)                -> "Block{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)                  -> string_of_expr expr ^ "\n"
  | Return(expr)                -> "return " ^ string_of_expr expr ^ "\n";
  | If(e, s, Block([]))         -> "if (" ^ string_of_expr e ^ ")\n" ^ 
        string_of_stmt s
  | If(e, s1, s2)               ->  "if (" ^ string_of_expr e ^ ")\n" ^
                                    string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s)                 -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Assign(st, e)               -> st ^ " = " ^ string_of_expr e ^ "\n" 

let string_of_vinit (s, e) = "set" ^ s ^ "to" ^ string_of_expr e ^ "\n"


