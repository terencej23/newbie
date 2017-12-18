type binop = Add | Sub | Div | Mod | Mult | Or | And | Lt | Leq | Gt | Geq | Eq | Neq
type typ = Int | Void | String | Float | Bool 
type unop = Neg | Not
type datatype = Datatype of typ | Listtype of typ

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
  (* list *) 
  | List of expr list
  | ListAccess of string * expr
  | ListPop of string
  | ListPush of string * expr
  | ListSize of string
  | ListSlice of string * expr * expr

type stmt = 
  | Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
(*  | For of expr * expr * expr  *)
(*  | Iter of expr * expr *)
  | Break
  | Expr of expr
  | Return of expr 
  | Assign of string * expr 
  (* list *) 
  | ListReplace of string * expr * expr

type fdecl =  {
  fname : string;
  formals : string list;
  body : stmt list;
}

type global =  string * expr 
type program = global list * fdecl list


(* Pretty-printing functions *)

let string_of_op = function
    Add   -> Printf.sprintf "+"
  | Sub   -> Printf.sprintf "-"
  | Mult  -> Printf.sprintf "*"
  | Div   -> Printf.sprintf "/"
  | Mod   -> Printf.sprintf "%%"
  | Eq    -> Printf.sprintf "="
  | Neq   -> Printf.sprintf "!="
  | Lt    -> Printf.sprintf "<"
  | Leq   -> Printf.sprintf "<="
  | Gt    -> Printf.sprintf ">"
  | Geq   -> Printf.sprintf ">="
  | And   -> Printf.sprintf "and"
  | Or    -> Printf.sprintf "or"

let string_of_uop = function
    Neg   -> Printf.sprintf "-"
  | Not   -> Printf.sprintf "!"

let rec string_of_expr = function
    IntLit(d)               -> Printf.sprintf "%d" d
  | FloatLit(f)             -> Printf.sprintf "%f" f
  | StrLit(s)               -> Printf.sprintf "\"%s\"" s
  | BoolLit(true)           -> Printf.sprintf "true"
  | BoolLit(false)          -> Printf.sprintf "false"
  | Id(s)                   -> Printf.sprintf "%s" s
  | Binop(e1, o, e2)        -> Printf.sprintf "%s %s %s" 
                                (string_of_expr e1) (string_of_op o) (string_of_expr e2)
  | Unop(o, e)              -> Printf.sprintf "%s %s"
                                (string_of_uop o) (string_of_expr e)
  | Call(f, e)              -> Printf.sprintf "%s(%s)"
                                f (String.concat ", " (List.map string_of_expr e))
  | Noexpr                  -> Printf.sprintf "noexpr"
  (* list *)
  | List(e_l)               -> Printf.sprintf "[%s]"
                                (String.concat ", " (List.map string_of_expr e_l))
  | ListAccess(s, e)        -> Printf.sprintf "%s[%s]" s (string_of_expr e)
  | ListPop(s)              -> Printf.sprintf "%s.pop()" s
  | ListPush(s, e)          -> Printf.sprintf "%s.append(%s)" s (string_of_expr e)
  | ListSize(s)             -> Printf.sprintf "%s.size()" s
  | ListSlice(s, e1, e2)    -> Printf.sprintf "%s[%s:%s]" 
                                s (string_of_expr e1) (string_of_expr e2)

let rec string_of_stmt = function
    Block(s)                    -> Printf.sprintf "%s" 
                                    (String.concat "\n\t" (List.map string_of_stmt s))
  | Expr(e)                     -> Printf.sprintf "%s"
                                    (string_of_expr e)
  | Return(e)                   -> Printf.sprintf "return %s" 
                                    (string_of_expr e)
  | If(e, s, Block([]))         -> Printf.sprintf "if (%s)\n\t%s"
                                    (string_of_expr e) (string_of_stmt s)
  | If(e, s1, s2)               -> Printf.sprintf "if (%s)\n\t%s\nelse\n\t%s"
                                    (string_of_expr e) (string_of_stmt s1) (string_of_stmt s2)
  | While(e, s)                 -> Printf.sprintf "while (%s)\n\t%s" 
                                    (string_of_expr e) (string_of_stmt s)
  | Assign(s, e)                -> Printf.sprintf "set %s to %s"
                                    s (string_of_expr e)
  | ListReplace(s, e1, e2)      -> Printf.sprintf "set %s[%s] to %s" 
                                    s (string_of_expr e1) (string_of_expr e2)
  | Break                       -> Printf.sprintf "break\n;"

let string_of_assign (s, e) = Printf.sprintf "set %s to %s" s (string_of_expr e)

let string_of_fdecl fdecl = Printf.sprintf "define function %s with params (%s)\n\t%s"
  (fdecl.fname)
  (String.concat ", "   (List.map (fun x -> x) fdecl.formals))
  (String.concat "\n\t" (List.map string_of_stmt fdecl.body))

let string_of_program (vars, funcs) = Printf.sprintf "%s\n\n%s"
(String.concat "\n" (List.map string_of_assign vars))
(String.concat "\n" (List.map string_of_fdecl funcs))
