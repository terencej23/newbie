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

let string_of_sop = function
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

let string_of_suop = function
    Neg   -> Printf.sprintf "-"
  | Not   -> Printf.sprintf "!"

let rec string_of_typ = function
    Datatype(String)        -> Printf.sprintf "str"
  | Datatype(Int)           -> Printf.sprintf "int"
  | Datatype(Float)         -> Printf.sprintf "float"
  | Datatype(Bool)          -> Printf.sprintf "bool"
  | Datatype(Void)          -> Printf.sprintf "void"

let rec string_of_sexpr = function
    SIntLit(d, _)             -> Printf.sprintf "%d" d
  | SFloatLit(f, _)           -> Printf.sprintf "%f" f
  | SStrLit(s, _)             -> Printf.sprintf "\"%s\"" s
  | SBoolLit(true, _)         -> Printf.sprintf "true"
  | SBoolLit(false, _)        -> Printf.sprintf "false"
  | SId(s, _)                 -> Printf.sprintf "%s" s
  | SBinop(se1, so, se2, _)   -> Printf.sprintf "%s %s %s" 
                                 (string_of_sexpr se1) (string_of_sop so) (string_of_sexpr se2)
  | SUnop(so, se, _)          -> Printf.sprintf "%s %s"
                                 (string_of_suop so) (string_of_sexpr se)
  | SCall(s, se, _)           -> Printf.sprintf "%s(%s)"
                                 s (String.concat ", " (List.map string_of_sexpr se))
  | SNoexpr                   -> Printf.sprintf "noexpr"

let rec string_of_sstmt = function
    SBlock(ss)                  -> Printf.sprintf "%s" 
                                   (String.concat "\n\t" (List.map string_of_sstmt ss))
  | SExpr(se, _)                -> Printf.sprintf "%s"
                                   (string_of_sexpr se)
  | SReturn(se, _)              -> Printf.sprintf "return %s" 
                                   (string_of_sexpr se)
  | SIf(se, ss, SBlock([]))     -> Printf.sprintf "if (%s)\n\t%s"
                                   (string_of_sexpr se) (string_of_sstmt ss)
  | SIf(se, ss1, ss2)           -> Printf.sprintf "if (%s)\n\t%s\nelse\n\t%s"
                                   (string_of_sexpr se) (string_of_sstmt ss1) (string_of_sstmt ss2)
(* | SWhile(se, ss)                -> Printf.sprintf "while (%s)\n\t%s"
                                    (sstring_of_expr se) (sstring_of_stmt ss) *)
  | SAssign(ss, se, _)          -> Printf.sprintf "set %s to %s"
                                   ss (string_of_sexpr se)

let string_of_sassign (s, se, _) = Printf.sprintf "set %s to %s" s (string_of_sexpr se)

let string_of_sfdecl sfdecl = Printf.sprintf "define function %s with params (%s) -> %s\n\t%s"
  (sfdecl.sfname)
  (String.concat ", "   (List.map (fun (name, typ) -> Printf.sprintf "<%s> %s" (string_of_typ typ) name) sfdecl.sformals))
  (string_of_typ sfdecl.styp)
  (String.concat "\n\t" (List.map string_of_sstmt sfdecl.sbody))

let string_of_sprogram (vars, funcs) = Printf.sprintf "%s\n\n%s"
(String.concat "\n" (List.map string_of_sassign vars))
(String.concat "\n" (List.map string_of_sfdecl funcs))
