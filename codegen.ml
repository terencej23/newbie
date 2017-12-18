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

    let br_block    = ref (L.block_of_value (L.const_int i32_t 0)) in 

    let global_vars = ref (StringMap.empty) in
    let current_f = ref (List.hd functions) in
    let local_vars = ref (StringMap.empty) in 
    (* list lookup *)
    let list_lookup = ref (StringMap.empty) in

    (* pointer wrapper - map of named struct types pointers *)
    let pointer_wrapper =
      List.fold_left
      (fun map name -> StringMap.add name (L.named_struct_type context name) map)
      StringMap.empty ["string"; "int"; "float"; "void"; "bool"]
    in

    (* set struct body fields for each of the types *)
    let () = 
      let set_struct_body name l =
        let t = StringMap.find name pointer_wrapper in
        ignore(
          L.struct_set_body t (Array.of_list(l)) true
        )
      in
      let named_types = ["string"; "int"; "float"; "void"; "bool"] in
      let llvm_types = [
        [L.pointer_type str_t; i32_t; i32_t]    ;
        [L.pointer_type i32_t; i32_t; i32_t]    ; 
        [L.pointer_type float_t; i32_t; i32_t]  ;
        [L.pointer_type void_t; i32_t; i32_t]   ;
        [L.pointer_type i1_t; i32_t; i32_t]     ;
      ] 
      in
      List.iter2 set_struct_body named_types llvm_types
    in

    (* Format strings for printing *) 
    let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder 
    and str_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder 
    and float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder in 

    (* get struct pointer *)
    let lookup_struct typ =
      let s = S.string_of_typ typ in
      StringMap.find s pointer_wrapper
    in

    let ltype_of_typ = function
        A.Datatype(A.Int)     ->  i32_t
      | A.Datatype(A.Bool)    ->  i1_t
      | A.Datatype(A.Void)    ->  void_t
      | A.Datatype(A.String)  ->  str_t
      | A.Datatype(A.Float)   ->  float_t 
      | A.Listtype(t)         ->  L.pointer_type (lookup_struct (A.Datatype(t)))
    in

    (* Declare print *)
    let print_t = L.var_arg_function_type i32_t [| str_t |] in
    let print_func = L.declare_function "printf" print_t the_module in

    (* Define each function (arguments and return type) so we can call it *)
    let function_decls = 
      let function_decl m fdecl = 
        let name = fdecl.S.sfname
        and formal_types = 
        Array.of_list(List.map 
          (fun (_, t) -> ltype_of_typ t) fdecl.S.sformals) 
        in
        let ftype = L.function_type (ltype_of_typ fdecl.S.styp) formal_types in
        StringMap.add name (L.define_function name ftype the_module, fdecl) m
      in
      List.fold_left function_decl StringMap.empty functions
    in


    let rec add_terminal builder f =
        match L.block_terminator (L.insertion_block builder) with
        Some _  -> ()
        | None    -> ignore (f builder)

    and expr builder = function 
        S.SBoolLit(b, _)    -> L.const_int i1_t (if b then 1 else 0)
      | S.SStrLit (s, _)    -> L.build_global_stringptr s "string" builder
      | S.SNoexpr           -> L.const_int i32_t 0
      | S.SFloatLit(f, _)   -> L.const_float float_t f 
      | S.SIntLit (i, _)    -> L.const_int i32_t i
      (* print built-in *)
      | S.SCall("printstr", [e], _)   -> 
          L.build_call print_func [| str_format_str builder; (expr builder e)|]
          "printf" builder
      | S.SCall("printint", [e], _)   -> 
          L.build_call print_func [| int_format_str builder; (expr builder e)|]
          "printf" builder
      | S.SCall("printfloat", [e], _) -> 
          L.build_call print_func [| float_format_str builder; (expr builder e)|]
          "printf" builder
      | S.SCall("printbool", [e], _)  -> 
          L.build_call print_func [| int_format_str builder; (expr builder e)|]
          "printf" builder
      (* function refs *)
      | S.SCall(f, act, _ ) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          let result = 
            match fdecl.S.styp with 
              A.Datatype(A.Void)  -> ""
            | _                   -> f ^ "_result"
          in
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
      (* list expr *)
      | S.SListAccess(s, se, t) ->
          let idx = expr builder se in
          let idx = L.build_add idx (L.const_int i32_t 1) "access1" builder in
          let struct_ptr = expr builder (S.SId(s, t)) in
          let arr = 
            L.build_load 
              (L.build_struct_gep struct_ptr 0 "access2" builder)
              "idl"
              builder
          in
          let res = L.build_gep arr [| idx |] "access3" builder in
          L.build_load res "access4" builder
      | S.SList(se_l, t)        ->
          let it = 
            match t with
              A.Listtype(it)  -> it
            | _               -> (raise (E.InvalidListElementType))
          in
          let struct_ptr = L.build_malloc (lookup_struct t) "list1" builder in
          let size = L.const_int i32_t ((List.length se_l) + 1) in
          let typ = L.pointer_type (ltype_of_typ (A.Datatype(it))) in
          let arr = L.build_array_malloc typ size "list2" builder in
          let arr = L.build_pointercast arr typ "list3" builder in
          let values = List.map (expr builder) se_l in
          let buildf index value = 
            let arr_ptr = 
              L.build_gep arr [| (L.const_int i32_t (index+1)) |] "list4" builder
            in
            ignore(L.build_store value arr_ptr builder)
          in
          List.iteri buildf values ;
          ignore(
            L.build_store 
              arr 
              (L.build_struct_gep struct_ptr 0 "list5" builder) 
              builder
          ) ;
          ignore(
            L.build_store 
              (L.const_int i32_t (List.length se_l)) 
              (L.build_struct_gep struct_ptr 1 "list6" builder) 
              builder
          ) ;
          ignore(
            L.build_store 
              (L.const_int i32_t 0)
              (L.build_struct_gep struct_ptr 2 "list7" builder)
              builder
          ) ;
          struct_ptr

    and stmt builder = 
      let (the_function, _) = StringMap.find !current_f.S.sfname function_decls 
      in function

      | S.SBlock sl           -> List.fold_left stmt builder sl ; 
      | S.SExpr (e, _)        -> ignore (expr builder e) ; builder
      | S.SReturn (e, _) -> ignore (
          match !current_f.S.styp with
            A.Datatype(A.Void) -> L.build_ret_void builder
          | _                  -> L.build_ret (expr builder e) builder
        ); builder
      | S.SAssign (s, e, _)   ->
          let expr_t = Semant.sexpr_to_type e in (
          match expr_t with
            A.Listtype(A.Void)  -> 
              if (StringMap.find s !list_lookup = A.Void) then
                builder
              else
                let typ = StringMap.find s !list_lookup in
                let struct_ptr = 
                  L.build_malloc (lookup_struct (A.Datatype(typ))) "voidassign1" builder 
                in
                let typ = L.pointer_type (ltype_of_typ (A.Datatype(typ))) in
                let arr = L.const_pointer_null typ in
                ignore(
                  L.build_store arr 
                    (L.build_struct_gep struct_ptr 0 "voidassign2" builder) 
                    builder
                ) ;
                let size = L.const_int i32_t 0 in
                ignore(
                  L.build_store size 
                    (L.build_struct_gep struct_ptr 1 "voidassign3" builder) 
                    builder
                ) ;
                ignore(L.build_store struct_ptr (lookup s) builder) ; 
                builder

          | _                   -> ignore (
              let e' = expr builder e in 
              L.build_store e' (lookup s) builder
            ) ; builder
          )
      | S.SIf(condition, if_stmt, else_stmt) ->
          let bool_val = expr builder condition in
          let merge_bb = L.append_block context "merge" the_function in

          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) if_stmt)
          (L.build_br merge_bb);

          let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | S.SWhile (predicate, body) ->
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
      | S.SBreak ->  ignore (L.build_br !br_block builder);  builder

     (* Lookup gives llvm for variable *)
    and lookup n  = 
      try 
        StringMap.find n !local_vars
      with Not_found ->   
        StringMap.find n !global_vars  
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
              match t with 
              | A.Listtype(it)  -> 
                  ignore(list_lookup := StringMap.add n it !list_lookup) ;
                  let local_var = L.build_alloca (ltype_of_typ t) n builder in
                  StringMap.add n local_var m             
              | _               ->
                  let local_var = L.build_alloca (ltype_of_typ t) n builder in 
                  StringMap.add n local_var m
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