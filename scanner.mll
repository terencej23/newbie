{
  module L = Lexing
  module B = Buffer
  module E = Exceptions
  open Parser

  let buf_size = 100 (* default buffer size *) 

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
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = (alpha | '_') (alpha | digit | '_')*

let nl = '\n' | ('\r' '\n')
let tab = '\t'
let ws = [' ' '\t'] 

(* TODO: pass in stack arg for tracking indentation *)
rule token stream = parse
    (nl+ tab*)+ as delimit    { 
                                L.new_line lexbuf ; 
                                let toks = de_indent_gen delimit (NEWLINE :: stream) indent_stack in 
                                token toks lexbuf
                              }  
  | ws+                       { token stream lexbuf }
  | "if"                      { let toks = IF     :: stream in token toks lexbuf }
  | "break"                   { let toks = BREAK  :: stream in token toks lexbuf }
  | "else"                    { let toks = ELSE   :: stream in token toks lexbuf }
  | "true"                    { let toks = TRUE   :: stream in token toks lexbuf }
  | "false"                   { let toks = FALSE  :: stream in token toks lexbuf }
  | "for"                     { let toks = FOR    :: stream in token toks lexbuf }
  | "while"                   { let toks = WHILE  :: stream in token toks lexbuf }
  | "each"                    { let toks = EACH   :: stream in token toks lexbuf }
  | "in"                      { let toks = IN     :: stream in token toks lexbuf }
  | "and"                     { let toks = AND    :: stream in token toks lexbuf }
  | "or"                      { let toks = OR     :: stream in token toks lexbuf }
  | "no"                      { let toks = NO     :: stream in token toks lexbuf }
  | "not"                     { let toks = NOT    :: stream in token toks lexbuf }
  | "function"                { let toks = FUNC   :: stream in token toks lexbuf }
  | "return"                  { let toks = RETURN :: stream in token toks lexbuf }
  | "set"                     { let toks = ASSIGN :: stream in token toks lexbuf }
  | "define"                  { let toks = DEF    :: stream in token toks lexbuf }
  | "with"                    { let toks = WITH   :: stream in token toks lexbuf }
  | "params"                  { let toks = PARAMS :: stream in token toks lexbuf }
  | "to"                      { let toks = TO     :: stream in token toks lexbuf }
  | '['                       { let toks = LBRACK :: stream in token toks lexbuf }
  | ']'                       { let toks = RBRACK :: stream in token toks lexbuf }
  | ':'                       { let toks = COLON  :: stream in token toks lexbuf }
  | ','                       { let toks = COMMA  :: stream in token toks lexbuf }
  | '('                       { let toks = LPAREN :: stream in token toks lexbuf }
  | ')'                       { let toks = RPAREN :: stream in token toks lexbuf }
  | ('+' | "plus")            { let toks = PLUS   :: stream in token toks lexbuf }
  | ('-' | "minus")           { let toks = MINUS  :: stream in token toks lexbuf }
  | ('*' | "times")           { let toks = MULT   :: stream in token toks lexbuf }
  | ('/' | "divided by")      { let toks = DIVIDE :: stream in token toks lexbuf }
  | ('%' | "modulo")          { let toks = MOD    :: stream in token toks lexbuf }
  | ('=' | "equals")          { let toks = EQUALS :: stream in token toks lexbuf }
  | ('>' | "greater than")    { let toks = GT     :: stream in token toks lexbuf }
  | ('<' | "less than")       { let toks = LT     :: stream in token toks lexbuf }
  | (">=" | "greater than or equal to") 
                              { let toks = GEQ    :: stream in token toks lexbuf }
  | ("<=" | "less than or equal to") 
                              { let toks = LEQ    :: stream in token toks lexbuf }
  | '"'                       { let toks = STRLIT(str (B.create buf_size) lexbuf) :: stream in token toks lexbuf }
  | '#'                       { comment stream lexbuf }
  | "/*"                      { multi_comment stream lexbuf } 
  | digit+ as num             { let toks = INTLIT(int_of_string num) :: stream in token toks lexbuf }
  | digit+ '.' digit* as num  { let toks = FLOATLIT(float_of_string num) :: stream in token toks lexbuf }
  | "'s" | '.'                { let toks = ATTR   :: stream in token toks lexbuf }
  | "size"                    { let toks = SIZE   :: stream in token toks lexbuf }
  | "append" | "push"         { let toks = PUSH   :: stream in token toks lexbuf }
  | "pop"                     { let toks = POP    :: stream in token toks lexbuf }
  | id as ident               { let toks = ID(ident) :: stream in token toks lexbuf }
  | eof                       { eof_dedent stream indent_stack } 
  | _ as char                 { raise (E.SyntaxError((Printf.sprintf "unrecognized char '%c'" char))) }

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
  | eof                       { raise (E.SyntaxError("eof within str")) }
  | _ as char                 { raise (E.SyntaxError((Printf.sprintf "unrecognized char '%c'" char))) }

and comment stream = parse
  | nl+                       { token stream lexbuf }
  | eof                       { eof_dedent stream indent_stack } 
  | _                         { comment stream lexbuf }

and multi_comment stream = parse
  | "*/"                      { token stream lexbuf }
  | eof                       { raise (E.SyntaxError("eof within comment")) }
  | _                         { multi_comment stream lexbuf }

{

 (* pretty printing functions *) 
 
 let string_of_token = function
    STRLIT(str)     -> Printf.sprintf "STRLIT(%s)" (String.escaped str)
  | INTLIT(num)     -> Printf.sprintf "INTLIT(%d)" num
  | FLOATLIT(num)   -> Printf.sprintf "FLOATLIT(%f)" num
  | ID(str)         -> Printf.sprintf "ID(%s)" str
  | ATTR            -> Printf.sprintf "ATTR"
  | ASSIGN          -> Printf.sprintf "ASSIGN"
  | DEF             -> Printf.sprintf "DEF"
  | WITH            -> Printf.sprintf "WITH"
  | PARAMS          -> Printf.sprintf "PARAMS"
  | EQUALS          -> Printf.sprintf "EQUAL"
  | GT              -> Printf.sprintf "GT"
  | LT              -> Printf.sprintf "LT"
  | GEQ             -> Printf.sprintf "GEQ"
  | LEQ             -> Printf.sprintf "LEQ"
  | PLUS            -> Printf.sprintf "PLUS"
  | MINUS           -> Printf.sprintf "MINUS"
  | NEG             -> Printf.sprintf "NEG"
  | MULT            -> Printf.sprintf "MULT"
  | DIVIDE          -> Printf.sprintf "DIVIDE"
  | MOD             -> Printf.sprintf "MOD"
  | IF              -> Printf.sprintf "IF"
  | ELSE            -> Printf.sprintf "ELSE"
  | TRUE            -> Printf.sprintf "TRUE"
  | FALSE           -> Printf.sprintf "FALSE"
  | FOR             -> Printf.sprintf "FOR"
  | WHILE           -> Printf.sprintf "WHILE"
  | IN              -> Printf.sprintf "IN"
  | EACH            -> Printf.sprintf "EACH"
  | TO              -> Printf.sprintf "TO"
  | AND             -> Printf.sprintf "AND"
  | OR              -> Printf.sprintf "OR"
  | NOT             -> Printf.sprintf "NOT"
  | NO              -> Printf.sprintf "NO"
  | FUNC            -> Printf.sprintf "FUNC"
  | RETURN          -> Printf.sprintf "RETURN"
  | COMMA           -> Printf.sprintf "COMMA"
  | COLON           -> Printf.sprintf "COLON"
  | LBRACK          -> Printf.sprintf "LBRACK"
  | RBRACK          -> Printf.sprintf "RBRACK"
  | SIZE            -> Printf.sprintf "SIZE"
  | PUSH            -> Printf.sprintf "PUSH"
  | POP             -> Printf.sprintf "POP"
  | LPAREN          -> Printf.sprintf "LPAREN"
  | RPAREN          -> Printf.sprintf "RPAREN"
  | INDENT          -> Printf.sprintf "INDENT"
  | DEDENT          -> Printf.sprintf "DEDENT"
  | NEWLINE         -> Printf.sprintf "NEWLINE"
  | EOF             -> Printf.sprintf "EOF"
  | BREAK           -> Printf.sprintf "BREAK"

let string_of_tokens tokens = 
  List.map (fun tok -> string_of_token tok) tokens
    |> String.concat " "

}