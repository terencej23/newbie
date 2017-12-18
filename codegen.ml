module L = Llvm
module A = Ast
module S = Sast
module E = Exceptions
module Semant = Semant

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module  context "Newbie"

      and i32_t  = L.i32_type     context
      and i1_t   = L.i1_type      context
      and void_t = L.void_type    context
      and float_t = L.double_type context 
      and str_t  = L.pointer_type   (L.i8_type context) 
    in

    let global_vars = ref (StringMap.empty) in
    let current_f = ref (List.hd functions) in
    let local_vars = ref (StringMap.empty) in 

    let ltype_of_typ = function
        A.Datatype(A.Int)   ->  i32_t
      | A.Datatype(A.Bool)  ->  i1_t
      | A.Datatype(A.Void)  ->  void_t
      | A.Datatype(A.String)  ->  str_t
      | A.Datatype(A.Float) -> float_t 
    in

    (* Declare print *)
    let print_t = L.var_arg_function_type i32_t [| str_t |] in
    let print_func = L.declare_function "printf" print_t the_module in

    (* Format strings for printing *) 
    let str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder 
    and float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder
    and int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in


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


    let rec add_terminal builder f =
        match L.block_terminator (L.insertion_block builder) with
        Some _  -> ()
        | None    -> ignore (f builder)

    and expr builder = function 
        S.SBoolLit(b, _)  -> L.const_int i1_t (if b then 1 else 0)
      | S.SStrLit (s, _)  -> L.build_global_stringptr s "string" builder
      | S.SNoexpr    -> L.const_int i32_t 0
      | S.SFloatLit(f, _) -> L.const_float float_t f 
      | S.SIntLit (i, _)  -> L.const_int i32_t i
      | S.SCall("printstr", [e], _) -> 
        L.build_call print_func [| str_format_str builder; (expr builder e)|]
        "printf" builder
      | S.SCall("printint", [e], _) -> 
        L.build_call print_func [| int_format_str builder; (expr builder e)|]
        "printf" builder
      | S.SCall("printfloat", [e], _) -> 
        L.build_call print_func [| float_format_str builder; (expr builder e)|]
        "printf" builder
      | S.SCall("printbool", [e], _) -> 
        L.build_call print_func [| int_format_str builder; (expr builder e)|]
        "printf" builder
      | S.SCall(f, act, _ ) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = (match fdecl.S.styp with 
                                 A.Datatype(A.Void) -> ""
                                 | _ -> f ^ "_result") in
            L.build_call fdef (Array.of_list actuals) result builder
      | S.SUnop(op, e, _) -> 
          let e' = expr builder e in
          let llvm_build = function
            | A.Neg       -> L.build_neg e' "tmp" builder
            | A.Not       -> L.build_not e' "tmp" builder
          in
          llvm_build op
      | S.SId (s, _)    -> L.build_load (lookup s) s builder
      | S.SBinop (e1, op, e2, _) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
            let typ = Semant.sexpr_to_type e1 in 
            (match typ with 
                A.Datatype(A.Int) |  A.Datatype(A.Bool) ->  (match op with
                A.Add     -> L.build_add
              | A.Sub     -> L.build_sub
              | A.Mult    -> L.build_mul
              | A.Div     -> L.build_sdiv
              | A.Mod     -> L.build_srem 
              | A.Eq   -> L.build_icmp L.Icmp.Eq
              | A.Lt    -> L.build_icmp L.Icmp.Slt
              | A.Leq     -> L.build_icmp L.Icmp.Sle
              | A.Gt -> L.build_icmp L.Icmp.Sgt
              | A.Geq     -> L.build_icmp L.Icmp.Sge
              | A.And     -> L.build_and
              | A.Or      -> L.build_or
              (* | A.Neq     -> L.build_icmp L.Icmp.Ne *)
              | _         -> raise E.InvalidBinaryOperation 
              ) e1' e2' "tmp" builder
              | A.Datatype(A.Float) -> (match op with
              A.Add     ->   L.build_fadd
              | A.Sub     -> L.build_fsub
              | A.Mult    -> L.build_fmul
              | A.Div     -> L.build_fdiv
              | A.Mod     -> L.build_frem
              | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
              | A.Lt    -> L.build_fcmp L.Fcmp.Ult
              (* | A.Neq     -> L.build_fcmp L.Fcmp.One *)
              | A.Leq     -> L.build_fcmp L.Fcmp.Ole
              | A.Gt -> L.build_fcmp L.Fcmp.Ogt
              | A.Geq     -> L.build_fcmp L.Fcmp.Oge
              | _ -> raise E.InvalidBinaryOperation 
              ) e1' e2' "tmp" builder
              | _ -> raise E.InvalidBinaryOperation) 

    and stmt builder = function
          S.SBlock sl -> 
            List.fold_left stmt builder sl; 
        | S.SExpr (e, _) -> ignore (expr builder e); builder
        | S.SReturn (e, _) -> L.build_ret_void builder; builder
        | S.SAssign (s, e, _) ->
                let expr_t = Semant.sexpr_to_type e in
                (match expr_t with
                | _ -> 
                        (ignore(let e' = expr builder e in 
                        (L.build_store e' (lookup s) builder)); builder))

     (* Lookup gives llvm for variable *)
    and lookup n  = try StringMap.find n !local_vars
        with Not_found ->   StringMap.find n !global_vars  
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

    let build_function_body fdecl =
        let (the_function, _)  = StringMap.find fdecl.S.sfname function_decls in 
        let builder = L.builder_at_end context (L.entry_block the_function) in  
        current_f := fdecl; 

        (*Construct the function's "locals": formal arguments and locally
        declared variables.  Allocate each on the stack, initialize their
         value, if appropriate, and remember their values in the "locals" map *)
        let _local_vars = 
            let add_formal m (n, t) p = 
                L.set_value_name n p; 

                let local = L.build_alloca (ltype_of_typ t) n builder 
                in ignore (L.build_store p local builder); StringMap.add n local m 
            in
            let add_local m (n, t) =
                (match t with 
                | _ ->
                        (let local_var = L.build_alloca (ltype_of_typ t) n builder
                        in StringMap.add n local_var m)
                )
            in
            let formals = List.fold_left2 add_formal StringMap.empty 
                fdecl.S.sformals (Array.to_list (L.params the_function)) 
            in 
            List.fold_left add_local formals fdecl.S.slocals 
        in  
        local_vars := _local_vars;
        
        (* Build the code for each statement in the function *)
        let builder = stmt builder (S.SBlock fdecl.S.sbody) in
     
        (* Add a return if the last block falls off the end *)
        add_terminal builder (match fdecl.S.styp with
            A.Datatype(A.Void) -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

List.iter build_function_body functions;
the_module