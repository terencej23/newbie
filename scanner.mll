
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
    | NUM_
    | STR_
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

  (* TODO: generation of INDENT/DEDENT tokens - move module ~begin *)
  let indent_stack = Stack.create ()
  let () = Stack.push 0 indent_stack 

  let de_indent_gen str token_stream stack = 
    let char_count str target = (* count target char occurences in str *)
      let explode str = (* explode str into list of char *)
        let rec helper i lst =
          if i < 0 then lst else helper(i-1) (str.[i] :: lst)
        in helper (String.length str - 1) []
      in
      let l = explode str in
      List.fold_left (fun acc elem -> if (elem = target) then (acc+1) else acc) 0 l
    in
    let curr_indent = char_count str '\t' in (* TODO: make tab or spaces *)
    let prev_indent = Stack.top stack in
    let cmp_tabs top curr = (* compare stack peek with with current num -> # tokens to gen and of ? type *)
      if (curr != top) then
        if (curr > top) then
          let () = Stack.push curr stack in
          1
        else
          let rec dedent_track count check = (* give int < 0 -> # of DEDENT TOKENS TO GENERATE *)
            if (Stack.is_empty stack) then
              raise (Failure("unexpected indentation"))
            else
              let popped = Stack.pop stack in
              if (check = popped) then
                count
              else
                dedent_track (count-1) popped
          in
          dedent_track (-1) (Stack.top stack)
      else
        0   
    in
    let to_generate = cmp_tabs prev_indent curr_indent in
    let rec token_gen stream = function (* add appropriate tokens to stream based on int arg -> updated stream *)
        0             -> stream
      | _ as int_     -> 
        if (int_ > 0) then
          token_gen (INDENT :: stream) (int_ - 1) 
        else
          token_gen (DEDENT :: stream) (int_ + 1)
    in 
    token_gen token_stream to_generate 

    let eof_dedent token_stream stack = (* add DEDENT tokens to stream at eof *)
      let peek = Stack.top stack in
      let rec token_gen stream = function
          0     -> EOF :: stream 
        | _     -> 
          let _ = Stack.pop stack in
          token_gen (DEDENT :: stream) (Stack.top stack)
      in
      token_gen token_stream peek

  (* ~end *)

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = (alpha | '_') (alpha | digit | '_')*

let nl = '\n' | ('\r' '\n')
let tab = '\t'
let ws = [' ' '\t'] 

(* TODO: pass in stack arg for tracking indentation *)
rule token stream = parse
    nl tab* as delimit        { L.new_line lexbuf ; 
                                let toks = de_indent_gen delimit stream indent_stack in 
                                token toks lexbuf }  
  | ws+                       { token stream lexbuf }
  | "if"                      { let toks = IF     :: stream in token toks lexbuf }
  | "else"                    { let toks = ELSE   :: stream in token toks lexbuf }
  | "true"                    { let toks = TRUE   :: stream in token toks lexbuf }
  | "false"                   { let toks = FALSE  :: stream in token toks lexbuf }
  | "for"                     { let toks = FOR    :: stream in token toks lexbuf }
  | "while"                   { let toks = WHILE  :: stream in token toks lexbuf }
  | "each"                    { let toks = EACH   :: stream in token toks lexbuf }
  | "in"                      { let toks = IN     :: stream in token toks lexbuf }
  | "and"                     { let toks = AND    :: stream in token toks lexbuf }
  | "or"                      { let toks = OR     :: stream in token toks lexbuf }
  | "not"                     { let toks = NOT    :: stream in token toks lexbuf }
  | "return"                  { let toks = RETURN :: stream in token toks lexbuf }
  | "set"                     { let toks = ASSIGN :: stream in token toks lexbuf }
  | "define"                  { let toks = DEF    :: stream in token toks lexbuf }
  | "with"                    { let toks = WITH   :: stream in token toks lexbuf }
  | "params"                  { let toks = PARAMS :: stream in token toks lexbuf }
  | "to"                      { let toks = TO     :: stream in token toks lexbuf }
  | ','                       { let toks = COMMA  :: stream in token toks lexbuf }
  | '('                       { let toks = LPAREN :: stream in token toks lexbuf }
  | ')'                       { let toks = RPAREN :: stream in token toks lexbuf }
  | ('+' | "plus")            { let toks = PLUS   :: stream in token toks lexbuf }
  | ('-' | "minus")           { let toks = MINUS  :: stream in token toks lexbuf }
  | ('*' | "times")           { let toks = MULT   :: stream in token toks lexbuf }
  | ('/' | "divided by")      { let toks = DIVIDE :: stream in token toks lexbuf }
  | ('=' | "equals")          { let toks = EQUALS :: stream in token toks lexbuf }
  | ('>' | "greater than")    { let toks = GT     :: stream in token toks lexbuf }
  | ('<' | "less than")       { let toks = LT     :: stream in token toks lexbuf }
  | '"'                       { let toks = STR(str (B.create buf_size) lexbuf) :: stream in token toks lexbuf }
(* TODO: special handling of list *)
(* TODO: special handling of (multiline) comments *)
  | "num"                     { let toks = NUM_   :: stream in token toks lexbuf }
  | "str"                     { let toks = STR_   :: stream in token toks lexbuf }
  | digit+ as num             { let toks = INT(int_of_string num) :: stream in token toks lexbuf }
  | digit+ '.' digit* as num  { let toks = FLOAT(float_of_string num) :: stream in token toks lexbuf }
  | "'s"                      { let toks = ATTR   :: stream in token toks lexbuf }
  | id as ident               { let toks = ID(ident) :: stream in token toks lexbuf }
  | eof                       { eof_dedent stream indent_stack } 
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
    | NUM_        -> Printf.sprintf "NUM_TYPE"
    | STR_        -> Printf.sprintf "STR_TYPE"
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
    let token_list = List.rev (token [] lexbuf) in
    let () = List.iter (fun tok -> Printf.printf "%s " (to_string tok)) token_list in
    let () = print_endline "" in
    token_list

  let _ = Printexc.print main ()
}
