module L = Llvm
module A = Ast

module StringMap = Map.Make(String)


let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Newbie"

    and i32_t = L.i32_type context
    and i8_t = L.i8_type context
    and void_t = L.void_type context in

  let global_vars = ref (StringMap.empty) in
  let local_vars = ref (StringMap.empty) in
  let current_f = ref (List.hd functions) in

  (* format str for printing *)
  let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in


  (* declare built-in c functions *)
  let print_t = L.var_arg_function_type i32_t  [| L.pointer_type i8_t |] in
  let print_func = L.declare_function "print" print_t the_module in


  (* get the llvm type of datatype *)
  let ltype_of_typ = function
    A.Void    -> void_t 

  in

  (* map of each function declared in file *)
  let function_decls = 
    let function_decl map fdecl =
      let name = fdecl.A.fname in
      let ftype = void_t in (* TODO: non-void *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) map
    in
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
    let (the_function, _) = StringMap.find !current_f.fname function_decls in 
    function
      A.Block(sl)     -> List.fold_left stmt builder sl ;
    | A.Expr(e)       -> ignore (expr builder e) ; builder
    | A.Return(e)     -> ignore (L.build_ret_void builder) ; builder
  
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

(*
    let _local_vars =
      let add_formal map (name, typ) val =
        L.set_value_name name val ;

        let local = L.build_alloca (ltype_of_typ typ) name builder in
        ignore(L.build_store val local builder); StringMap.add name local map

      in
      let add_local map (name, typ) = 
        let local_var = L.build_alloca (ltype_of_typ typ) name builder in
        StringMap.add name local_var map
      in
      let formals = 
        List.fold_left2 add_formal StringMap.empty 
          fdecl.formals (Array.to_list (L.params the_function)) in
      in 
      List.fold_left add_local formals [] (* TODO: add semantically checked locals *)
    in
    local_vars := _local_vars ;

*)

    (* build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.body) in

    (* add return if nonexistent *)
    add_terminal builder (L.build_ret_void)

  in 
  List.iter build_function_body functions ;
  the_module