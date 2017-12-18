(* top-level of newbie compiler *)

type actions = TOKEN  | AST | SAST | LLVIM_IR | COMPILE | DEFAULT

let lli_exec = "/usr/local/opt/llvm/bin/lli"

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
          ("-l", LLVIM_IR)  ; (* generate, do NOT check *)
          ("-c", COMPILE)     (* generate, check LLVM IR *)
      ]
    else if (Array.length Sys.argv = 2 && not (is_tag Sys.argv.(1))) then       (* no tag *)
      DEFAULT
    else                                                                        (* error *)
      (raise (Exceptions.InvalidExecFormat("invalid format ./newbie [-t] path_to_file")))
  in
  let fname = 
    match action with
      DEFAULT     -> Sys.argv.(1)
    | _           -> Sys.argv.(2)
  in
  let lexbuf = Lexing.from_channel (open_in fname) in
  let tokens = List.rev (Scanner.token [] lexbuf) in 
  let cache =                                                                   (* finesse *)
    let l = ref [] in
    fun lexbuf ->
      match !l with
      | hd::tl  -> l := tl ; hd 
      | []      -> 
        match (tokens) with
        | hd::tl  -> l := tl ; hd 
        | []      -> (raise (Exceptions.MalformedTokens))
  in
  let gen_ast = Parser.program cache lexbuf in
  let gen_sast = Semant.check gen_ast in
  let the_module = Codegen.translate gen_sast in
  match action with
    TOKEN           -> print_endline (Scanner.string_of_tokens tokens)
  | AST             -> print_endline (Ast.string_of_program gen_ast)
  | SAST            -> print_endline (Sast.string_of_sprogram gen_sast)
  | LLVIM_IR        -> print_endline (Llvm.string_of_llmodule the_module)
  | COMPILE         -> Llvm_analysis.assert_valid_module the_module ; 
                       print_string (Llvm.string_of_llmodule the_module)
  | DEFAULT         -> Llvm_analysis.assert_valid_module the_module ; 
      let llvm_code = Llvm.string_of_llmodule the_module in
      let ll_fname =
        let basename = Filename.basename fname in
        (Filename.remove_extension basename) ^ ".ll"
      in
      let outFile = open_out (ll_fname) in
      Printf.fprintf outFile "%s\n" (llvm_code) ; close_out outFile ;           (* write out to file *)
      ignore(Sys.command (Printf.sprintf "%s %s" lli_exec ll_fname))            (* run llvm interpreter - set var in makefile *)

let _ = Printexc.print main ()
