
module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =

	let context = L.global_context () in
	let the_module = L.create_module 	context "Newbie"

  		and i32_t  = L.i32_type  		context
  		and i1_t   = L.i1_type   		context
  		and void_t = L.void_type 		context
  		and str_t  = L.pointer_type 	(L.i8_type context) 
  	in

  	let ltype_of_typ = function
      	  A.Int 	-> 	i32_t
    	| A.Bool 	-> 	i1_t
    	| A.Void 	-> 	void_t
    	| A.String 	->	str_t
    in
<<<<<<< HEAD

    (* Declare print *)
    let print_t = L.function_type void_t [| str_t |] in
  	let print_func = L.declare_function "print" print_t the_module in

  	(* Format strings for printing *) 
    let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in

  (*   let rec add_terminal builder f =
        match L.block_terminator (L.insertion_block builder) with
	      Some _ 	-> ()
        | None 		-> ignore (f builder) *)

     (* Lookup gives llvm for variable *)
   (*  and lookup n  = try StringMap.find n !local_vars
        with Not_found ->   StringMap.find n !global_vars  
	in *)
	let global_vars = ref (StringMap.empty) in

	(* Define each function (arguments and return type) so we can call it *)
	let function_decls = 
		let function_decl m fdecl = 
			let name = fdecl.A.fname
			and formal_types = 
			Array.of_list(List.map (fun (_) -> void_t) fdecl.A.formals) 
			in
			let ftype = L.function_type void_t formal_types in
			StringMap.add name (L.define_function name ftype the_module, fdecl) m
		in
		List.fold_left function_decl StringMap.empty functions
	in

    let rec expr builder = function 
    	  A.BoolLit(b)	-> L.const_int i1_t (if b then 1 else 0)
    	| A.StrLit (s) 	-> L.build_global_stringptr s "string" builder
    	| A.Noexpr 		-> L.const_int i32_t 0
    	| A.IntLit (i) 	-> L.const_int i32_t i
    	| A.Call("print", [e]) -> 
    		L.build_call print_func [| str_format_str builder; (expr builder e)|]
    		"print" builder
    	| A.Id (s) 		-> L.build_load (lookup s) s builder
    
    (* Lookup gives llvm for variable *)
    and lookup n = StringMap.find n !global_vars
=======
    List.fold_left function_decl StringMap.empty functions
  in

  let rec add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
      Some(_)   -> ()
    | None      -> ignore (f builder)

  and expr builder = function
      A.StrLit(s)   -> L.build_global_stringptr s "string" builder
    | A.Noexpr      -> L.const_int i32_t 0
    | A.Id(id)      -> L.build_load (lookup id) id builder
    | A.IntLit(i)   -> L.const_int i32_t i
    | A.FloatLit(f) -> L.const_float float_t f 
    | A.BoolLit(b)  -> L.const_int i1_t (if b then 1 else 0)
    | A.Binop(e1, op, e2)     -> L.const_int i32_t 0
    | A.Unop(op, e) -> L.const_int i32_t 0
    (* print str *)
    | A.Call("print", [e]) ->
        L.build_call print_func [| str_format_str builder ; (expr builder e) |]
        "print" builder
    | A.Call(f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = "" in
        L.build_call fdef (Array.of_list actuals) result builder

  and stmt builder =
    let (the_function, _) = StringMap.find !current_f.A.fname function_decls in 
    function
      A.Block sl     -> List.fold_left stmt builder sl ;
    | A.Expr e       -> ignore (expr builder e) ; builder
    | A.If (predicate, then_stmt, else_stmt)   -> 
      let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
           add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
(*  | A.While (predicate, body)  -> 
      let pred_bb = L.append_block context "while" the_function in

          let body_bb = L.append_block context "while_body" the_function in

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in

          let merge_bb = L.append_block context "merge" the_function in

            br_block  := merge_bb; 

            ignore(L.build_br pred_bb builder);

            add_terminal (stmt (L.builder_at_end context body_bb) body) 
                (L.build_br pred_bb);

          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
*)
    | A.Assign (s, e) -> 
                (match e with
                | _ -> 
                        (ignore(let e' = expr builder e in 
                        (L.build_store e' (lookup s) builder)); builder))
    | A.Return (e)   -> ignore (match e with 
        _ -> L.build_ret_void builder); builder
  
  (*  give llvm for variable *)
  and lookup name = 
    try StringMap.find name !local_vars
    with Not_found  -> StringMap.find name !global_vars  
  in

  (* TODO: declaration of global vars *)

  (* declaration of local vars *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    current_f := fdecl ;

    (* construct the function's locals - formal params and locally declared vars *)  
    let _local_vars =
      let add_formal map (name, typ) v =
        L.set_value_name name v ;

        let local = L.build_alloca (ltype_of_typ typ) name builder in
        ignore(L.build_store v local builder); StringMap.add name local map
      in

      let add_local map (name, typ) = 
        let local_var = L.build_alloca (ltype_of_typ typ) name builder in
        StringMap.add name local_var map
      in
      let sformals = List.rev(List.fold_left2 
        (fun l name typ -> (name, typ) :: l) [] fdecl.A.formals [])
      in
      let formals = 
        List.fold_left2 add_formal StringMap.empty 
          sformals (Array.to_list (L.params the_function))
      in 
      List.fold_left add_local formals [] (* TODO: add semantically checked locals *)
>>>>>>> 5312a0a64a46e6f1147b9302283780086333ab00
    in


	(* Declare each global variable; remember its value in a map *)
    	
	let _global_vars = 
		let (f, _) = StringMap.find "main" function_decls in
		let builder = L.builder_at_end context (L.entry_block f) in
		let global_var m (n, e, _) = 
			let init = expr builder e in
			StringMap.add n (L.define_global n init the_module) m
		in
		List.fold_left global_var StringMap.empty globals
	in
	global_vars := _global_vars;

	(* Declare each local variable; remember its value in a map *)

	
    print_endline "\n\nTEST\n\n";

the_module