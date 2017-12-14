open Ast

type sexpr =
    SStrLit of string * datatype
  | SIntLit of int * datatype
  | SFloatLit of float * datatype
  | SBoolLit of bool * datatype
  | SBinop of sexpr * binop * sexpr * datatype
  | SUnop of unop * sexpr * datatype
  | SId of string * datatype
  | SCall of string * sexpr list * datatype
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SIf of sexpr * sstmt * sstmt
(* | SWhile of sexpr * sstmt *)
(* | SFor of sexpr * expr * expr *)
  | SAssign of string * sexpr * datatype
  | SExpr of sexpr * datatype
  | SReturn of sexpr * datatype

type sfdecl = {
  styp: datatype;
  sfname: string;
  slocals: (string * datatype) list;
  sformals: (string * datatype) list;
  sbody: sstmt list;
}

type sglobal = string * sexpr * datatype
type sprogram = sglobal list * sfdecl list


(* Pretty-printing functions *)

let sstring_of_op = function
    Add   -> Printf.sprintf "+"
  | Sub   -> Printf.sprintf "-"
  | Mult  -> Printf.sprintf "*"
  | Div   -> Printf.sprintf "/"
  | Mod   -> Printf.sprintf "%%"
  | Eq    -> Printf.sprintf "="
  | Lt    -> Printf.sprintf "<"
  | Leq   -> Printf.sprintf "<="
  | Gt    -> Printf.sprintf ">"
  | Geq   -> Printf.sprintf ">="
  | And   -> Printf.sprintf "and"
  | Or    -> Printf.sprintf "or"

let sstring_of_uop = function
    Neg   -> Printf.sprintf "-"
  | Not   -> Printf.sprintf "!"

let rec sstring_of_typ = function
    Datatype(String)        -> Printf.sprintf "str"
  | Datatype(Int)           -> Printf.sprintf "int"
  | Datatype(Float)         -> Printf.sprintf "float"
  | Datatype(Bool)          -> Printf.sprintf "bool"
  | Datatype(Void)          -> Printf.sprintf "void"

let rec sstring_of_expr = function
    SIntLit(d, _)            -> Printf.sprintf "%d" d
  | SFloatLit(f, _)          -> Printf.sprintf "%f" f
  | SStrLit(s, _)            -> Printf.sprintf "\"%s\"" s
  | SBoolLit(true, _)        -> Printf.sprintf "true"
  | SBoolLit(false, _)       -> Printf.sprintf "false"
  | SId(s, _)                -> Printf.sprintf "%s" s
  | SBinop(se1, so, se2, _)     -> Printf.sprintf "%s %s %s" 
                                (sstring_of_expr se1) (sstring_of_op so) (sstring_of_expr se2)
  | SUnop(so, se, _)           -> Printf.sprintf "%s %s"
                                (sstring_of_uop so) (sstring_of_expr se)
  | SCall(s, se, _)           -> Printf.sprintf "%s(%s)"
                                s (String.concat ", " (List.map sstring_of_expr se))
  | SNoexpr                  -> Printf.sprintf "noexpr"

let rec sstring_of_stmt = function
    SBlock(ss)                   -> Printf.sprintf "%s" 
                                    (String.concat "\n\t" (List.map sstring_of_stmt ss))
  | SExpr(se, _)                 -> Printf.sprintf "%s"
                                    (sstring_of_expr se)
  | SReturn(se, _)               -> Printf.sprintf "return %s" 
                                    (sstring_of_expr se)
  | SIf(se, ss, SBlock([]))       -> Printf.sprintf "if (%s)\n\t%s"
                                    (sstring_of_expr se) (sstring_of_stmt ss)
  | SIf(se, ss1, ss2)              -> Printf.sprintf "if (%s)\n\t%s\nelse\n\t%s"
                                    (sstring_of_expr se) (sstring_of_stmt ss1) (sstring_of_stmt ss2)
(* | SWhile(se, ss)                -> Printf.sprintf "while (%s)\n\t%s"
                                    (sstring_of_expr se) (sstring_of_stmt ss) *)
  | SAssign(ss, se, _)            -> Printf.sprintf "set %s to %s"
                                     ss (sstring_of_expr se)

let sstring_of_vinit (s, se, _) = Printf.sprintf "set %s to %s" s (sstring_of_expr se)

let sstring_of_fdecl sfdecl = Printf.sprintf "define function %s with parameters (%s) -> %s\n\t%s"
  (sfdecl.sfname)
  (String.concat ", "   (List.map (fun (name, typ) -> Printf.sprintf "<%s> %s" (sstring_of_typ typ) name) sfdecl.sformals))
  (sstring_of_typ sfdecl.styp)
  (String.concat "\n\t" (List.map sstring_of_stmt sfdecl.sbody))

let sstring_of_program (vars, funcs) = Printf.sprintf "%s\n\n%s"
(String.concat "\n" (List.map sstring_of_vinit vars))
(String.concat "\n" (List.map sstring_of_fdecl funcs))
