
{
  module L = Lexing
  module B = Buffer

  (* TODO: incomplete - move into ocamllyac parser module *)
  type tokens =

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
    Printf.ksprintf (function msg -> raise (Error((pos fname)^" "^msg)) format

  (* ~end *)
}

{

}