
{
  module L = Lexing
  module B = Buffer

  let buf_size = 100 (* default buffer size *)

  (* TODO: incomplete - move into ocamllyac parser module *)
  type token =
      STR of string
    | INT of int
    | FLOAT of float
    | ID of string
    | ATTR
    | NUM_TYPE
    | STR_TYPE
    | ASSIGN
    | DEF
    | WITH
    | PARAMS
    | TO
    | EQUALS
    | GT
    | LT
    | PLUS
    | MINUS
    | MULT
    | DIVIDE
    | IF
    | ELSE
    | TRUE
    | FALSE
    | FOR
    | WHILE
    | EACH
    | IN
    | AND
    | OR
    | NOT
    | RETURN
    | COMMA
    | LPAREN
    | RPAREN
    | INDENT
    | DEDENT
    | EOF

  (* TODO: move to separate module for err reporting ~begin *)
  let pos lexbuf = 
    let p = lexbuf.L.lex_curr_p in
      (* filename:line:col *)
      Printf.sprintf "%s:%d:%d" p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

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
let tab = '\t'
let ws = [' ' '\t'] 

(* TODO: pass in stack arg for tracking indentation *)
rule token = parse
    ws+                       { token lexbuf }
  | nl tab* as delimit        { (* TODO: track INDENT/DEDENT TOKEN *)
                                L.new_line lexbuf ; token lexbuf
                              }  
  | "if"                      { IF }
  | "else"                    { ELSE }
  | "true"                    { TRUE }
  | "false"                   { FALSE }
  | "for"                     { FOR }
  | "while"                   { WHILE }
  | "each"                    { EACH }
  | "in"                      { IN }
  | "and"                     { AND }
  | "or"                      { OR }
  | "not"                     { NOT }
  | "return"                  { RETURN }
  | "set"                     { ASSIGN }
  | "define"                  { DEF }
  | "with"                    { WITH }
  | "params"                  { PARAMS }
  | "to"                      { TO }
  | ','                       { COMMA }
  | '('                       { LPAREN }
  | ')'                       { RPAREN }
  | ('+' | "plus")            { PLUS }
  | ('-' | "minus")           { MINUS }
  | ('*' | "times")           { MULT }
  | ('/' | "divided by")      { DIVIDE }
  | ('=' | "equals")          { EQUALS }
  | ('>' | "greater than")    { GT }
  | ('<' | "less than")       { LT }
  | '"'                       { STR(str (B.create buf_size) lexbuf) }
  (* TODO: special handling of list *)
  (* TODO: special handling of (multiline) comments *)
  | "num"                     { NUM_TYPE }
  | "str"                     { STR_TYPE }
  | digit+ as num             { INT(int_of_string num) }
  | digit+ '.' digit* as num  { FLOAT(float_of_string num) }
  | "'s"                      { ATTR }
  | id as ident               { ID(ident) }
  | eof                       { EOF } 
  | _ as char                 { err lexbuf "unrecognized char '%c'" char }

and str buf = parse
  | [^ '"' '\r' '\n' '\\' ]+ as text  
                              { B.add_string buf text ; str buf lexbuf }
  | nl as newline             { B.add_string buf newline ; L.new_line lexbuf ; str buf lexbuf }
  | ('\\' '"')                { B.add_char buf '"' ; str buf lexbuf }
  | ('\\' 'n')                { B.add_char buf '\n' ; str buf lexbuf }
  | ('\\' 'r')                { B.add_char buf '\r' ; str buf lexbuf }
  | ('\\' 't')                { B.add_char buf '\t' ; str buf lexbuf }
  | '\\'                      { B.add_char buf '\\' ; str buf lexbuf }
  | '"'                       { B.contents buf } (* return *)
  | eof                       { err lexbuf "eof within str" }
  | _ as char                 { err lexbuf "unrecognized char '%c'" char }

{
  (* TODO: port to debugging module *)
  let to_string = function
      STR(str)    -> Printf.sprintf "STR(%s)" (String.escaped str)
    | INT(num)    -> Printf.sprintf "INT(%d)" num
    | FLOAT(num)  -> Printf.sprintf "FLOAT(%f)" num
    | ID(str)     -> Printf.sprintf "ID(%s)" str
    | ATTR        -> Printf.sprintf "ATTR" 
    | NUM_TYPE    -> Printf.sprintf "NUM_TYPE"
    | STR_TYPE    -> Printf.sprintf "STR_TYPE"
    | ASSIGN      -> Printf.sprintf "ASSIGN"
    | DEF         -> Printf.sprintf "DEF"
    | WITH        -> Printf.sprintf "WITH"
    | PARAMS      -> Printf.sprintf "PARAMS"
    | EQUALS      -> Printf.sprintf "EQUAL"
    | GT          -> Printf.sprintf "GT"
    | LT          -> Printf.sprintf "LT"
    | PLUS        -> Printf.sprintf "PLUS"
    | MINUS       -> Printf.sprintf "MINUS"
    | MULT        -> Printf.sprintf "MULT"
    | DIVIDE      -> Printf.sprintf "DIVIDE"
    | IF          -> Printf.sprintf "IF"
    | TRUE        -> Printf.sprintf "TRUE"
    | FALSE       -> Printf.sprintf "FALSE"
    | ELSE        -> Printf.sprintf "ELSE"
    | FOR         -> Printf.sprintf "FOR"
    | WHILE       -> Printf.sprintf "WHILE"
    | IN          -> Printf.sprintf "IN"
    | EACH        -> Printf.sprintf "EACH"
    | TO          -> Printf.sprintf "TO"
    | AND         -> Printf.sprintf "AND"
    | OR          -> Printf.sprintf "OR"
    | NOT         -> Printf.sprintf "NOT"
    | RETURN      -> Printf.sprintf "RETURN"
    | COMMA       -> Printf.sprintf "COMMA"
    | LPAREN      -> Printf.sprintf "LPAREN"
    | RPAREN      -> Printf.sprintf "RPAREN"
    | INDENT      -> Printf.sprintf "INDENT"
    | DEDENT      -> Printf.sprintf "DEDENT"
    | EOF         -> Printf.sprintf "EOF"

  let main () = 
    let (channel, fname) = 
      if Array.length Sys.argv > 1 then
        ((open_in Sys.argv.(1)), Sys.argv.(1))
      else
        (stdin, "stdin")
    in
    let lexbuf = set_fname fname (L.from_channel channel) in
    let rec loop list = function
        EOF       -> to_string EOF :: list |> List.rev
      | input     -> loop (to_string input :: list) (token lexbuf)
    in 
      loop [] (token lexbuf)
      |> String.concat " "
      |> print_endline

  let _ = Printexc.print main ()
}
