
{
  module L = Lexing
  module B = Buffer

  (* TODO: incomplete - move into ocamllyac parser module *)
  type token =
      STR of string
    | INT of int
    | FLOAT of float
    | ID of string
    | EQUAL
    | PLUS
    | MINUS
    | MULT
    | DIVIDE
    | EOF

  let get = L.lexeme
  let sprintf = Printf.sprintf

  (* TODO: move to separate module for err reporting ~begin *)
  let pos lexbuf = 
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
    Printf.ksprintf (fun msg -> raise (Error((pos lexbuf)^" "^msg))) format

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
  | '='               { EQUAL }
  (* TODO: special handling of str *)
  (* TODO: special handling of multiline comments *)
  | eof               { EOF } 
  | _                 { err lexbuf "unrecognized char '%s'" (get lexbuf) }

{
  (* TODO: port to debugging module *)
  let to_string = function
      STR(str)    -> sprintf "STR(%s)" (str) (* TODO: escape str *)
    | INT(num)    -> sprintf "INT(%d)" num
    | FLOAT(num)  -> sprintf "FLOAT(%f)" num
    | ID(str)     -> sprintf "ID(%s)" str
    | EQUAL       -> sprintf "EQUALS"
    | PLUS        -> sprintf "PLUS"
    | MINUS       -> sprintf "MINUS"
    | MULT        -> sprintf "MULT"
    | DIVIDE      -> sprintf "DIVIDE"
    | EOF         -> sprintf "EOF"

  let main () = 
    let lexbuf = set_fname "stdin" (L.from_channel stdin) in
    let rec loop list = function
        EOF       -> to_string EOF :: list |> List.rev
      | input     -> loop (to_string input :: list) (token lexbuf)
    in 
      loop [] (token lexbuf)
      |> String.concat " "
      |> print_endline

  let _ = Printexc.print main ()
}