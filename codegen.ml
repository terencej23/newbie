
module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module  context "Newbie"

      and i32_t  = L.i32_type     context
      and i1_t   = L.i1_type      context
      and void_t = L.void_type    context
      and str_t  = L.pointer_type   (L.i8_type context) 
    in

        let global_vars = ref (StringMap.empty) in

    let ltype_of_typ = function
        A.Datatype(A.Int)   ->  i32_t
      | A.Datatype(A.Bool)  ->  i1_t
      | A.Datatype(A.Void)  ->  void_t
      | A.Datatype(A.String)  ->  str_t
    in

    (* Declare print *)
    let print_t = L.function_type void_t [| str_t |] in
    let print_func = L.declare_function "print" print_t the_module in

    (* Format strings for printing *) 
    let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in

  (*   let rec add_terminal builder f =
        match L.block_terminator (L.insertion_block builder) with
        Some _  -> ()
        | None    -> ignore (f builder) *)

     (* Lookup gives llvm for variable *)
   (*  and lookup n  = try StringMap.find n !local_vars
        with Not_found ->   StringMap.find n !global_vars  
  in *)

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls = 
    let function_decl m fdecl = 
      let name = fdecl.S.sfname
      and formal_types = 
      Array.of_list(List.map (fun (_) -> void_t) fdecl.S.sformals) 
      in
      let ftype = L.function_type void_t formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in

    let rec expr builder = function 
        S.SBoolLit(b, _)  -> L.const_int i1_t (if b then 1 else 0)
      | S.SStrLit (s, _)  -> L.build_global_stringptr s "string" builder
      | S.SNoexpr    -> L.const_int i32_t 0
      | S.SIntLit (i, _)  -> L.const_int i32_t i
      | S.SCall("print", [e], _) -> 
        L.build_call print_func [| str_format_str builder; (expr builder e)|]
        "print" builder
     | S.SId (s, _)    -> L.build_load (lookup s) s builder
    (* Lookup gives llvm for variable *)
    and lookup n = StringMap.find n !global_vars
    in


  (* Declare each global variable; remember its value in a map *)
    	
    let _global_vars =
        let (f, _) = StringMap.find "main" function_decls in
        let builder = L.builder_at_end context (L.entry_block f) in
        let global_var m (n, e, _) = 
            let init = expr builder e 
            in StringMap.add n (L.define_global n init the_module) m in
        List.fold_left global_var StringMap.empty globals 
    in global_vars := _global_vars;      
	
    print_endline "\n\nTEST\n\n";

the_module