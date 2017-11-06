
{
  module L = Lexing
  module B = Buffer

  (* TODO: incomplete - move into ocamllyac parser module *)
  type token =
      STR of string
    | INT of int
    | FLOAT of float
    | ID of string
    | EQUALS
    | PLUS
    | MINUS
    | MULT
    | DIVIDE
    | EOF

  let get = L.lexeme
  let sprintf = Printf.sprintf

  (* TODO: move to separate module for err reporting ~begin *)
  let pos_ lexbuf = 
    let p = lexbuf.L.lex_curr_p in
      (* filename:line:col *)
      sprintf "%s:%d:%d" p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

  let set_fname fname lexbuf =
    (
      lexbuf.L.lex_curr_p <- { lexbuf.L.lex_curr_p with L.pos_fname = fname } ;
      lexbuf
    )

  exception Error of string
  let err lexbuf format =
    Printf.ksprintf (fun msg -> raise (Error((pos fname)^" "^msg)) format

  (* ~end *)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = (alpha | '_') (alpha | digit | '_')*

let nl = '\n' | ('\r' '\n')
let tab = ['t']
let ws = [' ']

(* TODO: pass in stack arg for tracking indentation *)
rule token = parse
    ws+               { token lexbuf }
  | tab               { (* TODO: track for INDENT/DEDENT TOKEN *) token lexbuf }
  | nl                { L.new_line ; token lexbuf } 
  | digit+            { INT(int_of_string (get lexbuf)) }
  | digit+ '.' digit* { FLOAT(float_of_string (get lexbuf)) }
  | id                { ID(get lexbuf) }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { MULT }
  | '/'               { DIVIDE }
  | '='               { EQUALS }
  (* TODO: special handling of str *)
  (* TODO: special handling of multiline comments *)
  | eof               { EOF } 
  | _                 { error lexbuf "unrecognized char '%s'" (get lexbuf) }

{

}