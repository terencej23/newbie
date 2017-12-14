(* top-level of newbie compiler *)

type actions = TOKEN | AST | SAST | (* LLVIM_IR | COMPILE | *)  DEFAULT

let main () =
  let is_tag str =
    String.get str 0 = '-'
  in
  let action = 
    if (Array.length Sys.argv = 3 && is_tag Sys.argv.(1)) then                  (* tag arg *)
      List.assoc Sys.argv.(1) [
          ("-t", TOKEN)     ; (* output tokens only *)
          ("-a", AST)       ; (* output ast only*)
          ("-s", SAST)      ; (* output sast only *)
        (* 
          ("-l", LLVIM_IR)  ; (* generate, do NOT check *)
          ("-c", COMPILE)     (* generate, check LLVM IR *)
         *)
      ]
    else if (Array.length Sys.argv = 2 && not (is_tag Sys.argv.(1))) then       (* no tag *)
      DEFAULT
    else                                                                        (* error *)
      raise (Failure("invalid format ./newbie [-tag] path_to_file"))
  in
  let get_channel_from = function
      DEFAULT     -> open_in (Sys.argv.(1))
    | _           -> open_in (Sys.argv.(2))
  in
  let lexbuf = Lexing.from_channel (get_channel_from action) in
  let tokens = List.rev (Scanner.token [] lexbuf) in 
  let cache =                                                                   (* finesse *)
    let l = ref [] in
    fun lexbuf ->
      match !l with
      | hd::tl  -> l := tl ; hd 
      | []      -> 
        match (tokens) with
        | hd::tl  -> l := tl ; hd 
        | []      -> failwith "sheesh!"
  in
  let gen_ast = Parser.program cache lexbuf in
  let gen_sast = Semant.check gen_ast in
  match action with
      TOKEN           -> print_endline (Scanner.string_of_tokens tokens)
    | AST             -> print_endline (Ast.string_of_program gen_ast)
    | SAST            -> print_endline (Sast.string_of_sprogram gen_sast)
    (* | LLVIM_IR        -> print_endline (Llvm.string_of_llmodule (Codegen.translate gen_ast)) (* TODO: make gen_sast *) *)
    (* | COMPILE         -> let m = Codegen.translate gen_ast in
         Llvm_analysis.assert_valid_module m; print_string (Llvm.string_of_llmodule m) *)
    | DEFAULT         -> () 
                        (* 
                          print_endline (Ast.string_of_program gen_ast) ;
                          print_endline (Sast.string_of_sprogram gen_sast)
                         *)

let _ = Printexc.print main ()
