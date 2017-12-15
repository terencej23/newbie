(* type inference and semantic checking *)

open Ast
open Sast
module E = Exceptions

module StringMap = Map.Make(String)

type env = {
  env_fmap: fdecl StringMap.t;
  env_fname: string;
  env_return_type: datatype;
  env_globals: datatype StringMap.t;
  env_flocals: datatype StringMap.t;
  env_in_loop: bool;
  env_set_return: bool;
  env_sfmap: sfdecl StringMap.t;
}

let string_of_env env =
  let string_of_fmap fmap =
    let bindings = StringMap.bindings fmap in
    let string_of_bindings = 
      List.map (fun (k,v) -> Printf.sprintf "%s: %s" k (string_of_fdecl v)) bindings
      |> String.concat ",\n    "
    in
    Printf.sprintf "{\n    %s\n  }" string_of_bindings
  in
  let string_of_sfmap sfmap =  
    let bindings = StringMap.bindings sfmap in
    let string_of_bindings = 
      List.map (fun (k,v) -> Printf.sprintf "%s: %s" k (string_of_sfdecl v)) bindings
      |> String.concat ",\n    "
    in
    Printf.sprintf "{\n    %s\n  }" string_of_bindings
  in
  let string_of_vars decls =
    let bindings = StringMap.bindings decls in
    let string_of_bindings =
      List.map (fun (k,v) -> Printf.sprintf "%s: %s" k (string_of_typ v)) bindings
      |> String.concat ",\n    "
    in
    Printf.sprintf "{\n    %s\n  }" string_of_bindings
  in
  Printf.sprintf 
  "{\n  env_fmap: %s,\n  env_fname: %s,\n  env_return_type: %s,\n  env_globals: %s,\n  env_flocals: %s,\n  env_in_loop: %B,\n  env_set_return: %B,\n  env_sfmap: %s\n}\n" 
  (string_of_fmap env.env_fmap) 
  env.env_fname 
  (string_of_typ env.env_return_type) 
  (string_of_vars env.env_globals)
  (string_of_vars env.env_flocals)
  env.env_in_loop
  env.env_set_return
  (string_of_sfmap env.env_sfmap)

let rec expr_to_sexpr expr env =
  match expr with
    IntLit(d)                 -> (SIntLit(d, Datatype(Int)), env)
  | FloatLit(f)               -> (SFloatLit(f, Datatype(Float)), env)
  | StrLit(s)                 -> (SStrLit(s, Datatype(String)), env)
  | BoolLit(b)                -> (SBoolLit(b, Datatype(Bool)), env)
  | Id(s)                     -> (check_scope s env, env)
  | Noexpr                    -> (SNoexpr, env)
  | Unop(op, e)               -> (check_unop op e env)
  | Binop(e1, op, e2)         -> (check_binop e1 op e2 env)
  (* built-in functions *)
  | Call("print", e_l)        -> (check_print e_l env)
  | Call(s, e_l)              -> (check_call s e_l env)

and sexpr_to_type = function
    SIntLit(_, typ)           -> typ
  | SFloatLit(_, typ)         -> typ
  | SStrLit(_, typ)           -> typ
  | SBoolLit(_, typ)          -> typ
  | SId(_, typ)               -> typ
  | SBinop(_, _, _, typ)      -> typ
  | SUnop(_, _, typ)          -> typ
  | SCall(_, _, typ)          -> typ
  | SNoexpr                   -> Datatype(Void)

and expr_list_to_sexpr_list e_l env =
  let env_ref = ref(env) in
  match e_l with
    hd :: tl  ->
      let (se, env) = expr_to_sexpr hd !env_ref in
      env_ref := env ;
      let (l, env) = expr_list_to_sexpr_list tl !env_ref in
      env_ref := env ;
      (se :: l, !env_ref)
  | []        -> ([], !env_ref)

and stmt_to_sstmt stmt env =
  match stmt with
    Block sl            -> check_sblock sl env
  | Expr e              -> check_expr e env
  | Assign(s, e)        -> check_assign s e env
  | Return e            -> check_return e env
  | If(e, s1, s2)       -> check_if e s1 s2 env

and fdecl_to_sfdecl fname arg_type_list env =
  let fdecl = StringMap.find fname env.env_fmap in

  (* make params and their types local vars *)
  let flocals = (
    List.fold_left2 (fun map name typ -> StringMap.add name typ map)
      StringMap.empty fdecl.formals arg_type_list
  )
  in

  (* start env variable *)
  let env = {
    env_globals = env.env_globals;
    env_fmap = env.env_fmap;
    env_fname = fname;
    env_return_type = Datatype(Void); (* placeholder *)
    env_flocals = flocals;
    env_in_loop = false;
    env_set_return = false;
    env_sfmap = env.env_sfmap;
  }
  in

  (* create semantically checked formals for fname *)
  report_duplicate(fdecl.formals) ;

  let sformals = 
    let get_args l name typ = 
      try
        let found_typ = StringMap.find name env.env_flocals in
        (name, found_typ) :: l
      with Not_found ->
        (name, typ) :: l
    in 
    List.rev (List.fold_left2 get_args [] fdecl.formals arg_type_list)
  in

  (* create semantically checked locals for fname *)
  let formals_map = (
    List.fold_left (fun map (name, typ) -> StringMap.add name typ map) 
      StringMap.empty sformals
  )
  in
  let locals_map = 
    let remove_formals map (name, typ) =
      if (StringMap.mem name formals_map) then
        StringMap.remove name map
      else
        map
    in
    List.fold_left remove_formals env.env_flocals (StringMap.bindings env.env_flocals)
  in
  let locals = StringMap.bindings locals_map in
  let slocals = List.fold_left (fun l (name, typ) -> (name, typ) :: l) [] locals 
    |> List.rev 
  in

  (* semantically check body statments *)
  let (sstmts, env) = stmt_to_sstmt (Block fdecl.body) env in

  (* create semantically checked func *)
  let sfdecl = {
    styp = env.env_return_type;
    sfname = fdecl.fname;
    slocals = slocals;
    sformals = sformals;
    sbody = match sstmts with SBlock(sl) -> sl | _ -> [];
  }
  in

  (* return the env with updated semantic func map *) 
  let new_env = {
    env_fmap = env.env_fmap;
    env_fname = env.env_fname;
    env_return_type = env.env_return_type; 
    env_globals = env.env_globals;
    env_flocals = env.env_flocals; 
    env_in_loop = env.env_in_loop;
    env_set_return = env.env_set_return;
    env_sfmap = StringMap.add fname sfdecl env.env_sfmap; 
  }
  in
  new_env

(* report duplicate variables *)
and report_duplicate list =
  let rec helper = function
      n1 :: n2 :: _ when n1 = n2  -> (raise (E.DuplicateVariable n1))
    | _ :: t                      -> helper t
    | []                          -> ()
  in
  helper (List.sort compare list)

and change_sexpr_type sexpr typ env = 
  match sexpr with
    SId(var, _)     -> (
      let flocals = StringMap.add var typ env.env_flocals in
      let new_env = {
        env_globals = env.env_globals;
        env_fmap = env.env_fmap;
        env_fname = env.env_fname;
        env_return_type = env.env_return_type;
        env_flocals = flocals;
        env_in_loop = env.env_in_loop;
        env_set_return = env.env_set_return;
        env_sfmap = env.env_sfmap;
      } in
      (SId(var, typ), new_env)
    )
  | _             -> (sexpr, env)

(* check conditionals *)
and check_if expr s1 s2 env =
  let (sexpr, env) = expr_to_sexpr expr env in
  let typ = sexpr_to_type sexpr in
  let (if_body, env) = stmt_to_sstmt s1 env in
let (else_body, env) = stmt_to_sstmt s2 env in if (typ = Datatype(Bool) ) then (SIf(sexpr, SBlock([if_body]), SBlock([else_body])), env)
  else
    (raise (E.InvalidIfStatementType))

(* check block for validity *)
and check_sblock sl env =
  (* make sure nothing follows a return *)
  let rec check_block_return = function
      Return _ :: _ :: _      -> (raise (E.NothingAfterReturn))
    | Block sl :: ss          -> check_block_return (sl @ ss)
    | _ :: ss                 -> check_block_return ss
    | []                      -> ()
  in check_block_return sl ;

  (* check all statements within block *)
  match sl with
  | []    -> (SBlock([SExpr(SNoexpr, Datatype(Void))]), env)
  | _     -> 
    let env_ref = ref(env) in
    let convert_stmt l stmt =
      let (new_stmt, env) = stmt_to_sstmt stmt !env_ref in
      env_ref := env ; (new_stmt :: l)
    in
    let (block, _) = (List.rev @@ (List.fold_left convert_stmt [] sl), !env_ref) in
    (SBlock(block), !env_ref)

(* check and verify return type *)
and check_return expr env =
  let (sexpr, env) = expr_to_sexpr expr env in
  let typ = sexpr_to_type sexpr in

  (* if return type has been set, check new return type *)
  if (env.env_set_return) then
    let () = ignore(
      if (env.env_return_type <> typ) then 
        Printf.printf "WARNING: function %s has expected return type of %s but is type %s ..." 
        env.env_fname (string_of_typ env.env_return_type) (string_of_typ typ)
    )
    in
    (SReturn(sexpr, typ), env)

  (* no return type set, so set it *)
  else
    let new_env = {
      env_fmap = env.env_fmap;
      env_fname = env.env_fname;
      env_return_type = typ;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_in_loop = env.env_in_loop;
      env_set_return = true;
      env_sfmap = env.env_sfmap;
    }
    in
    (SReturn(sexpr, typ), new_env)

(* check expression *)
and check_expr expr env =
  let (sexpr, env) = expr_to_sexpr expr env in
  let typ = sexpr_to_type sexpr in
  (SExpr(sexpr, typ), env)

(* check access to var *)
and check_scope var env =
  try
    let typ = StringMap.find var env.env_flocals in
    SId(var, typ)
  with Not_found -> (
    try
      let typ = StringMap.find var env.env_globals in
      SId(var, typ)
    with Not_found -> 
      (raise (E.UndefinedId var))
  )

(* check variable assignment *)
and check_assign var expr env =
  let sexpr, env = expr_to_sexpr expr env in
  let new_typ = sexpr_to_type sexpr in

  (* var has been declared, check new type *)
  if (StringMap.mem var env.env_flocals || StringMap.mem var env.env_globals) then
    let old_typ = 
      try
        StringMap.find var env.env_flocals (* try local first *)
      with Not_found -> 
        StringMap.find var env.env_globals (* query global *)
    in
    (
      if (old_typ <> new_typ) then 
        Printf.printf "WARNING: var %s of type %s has been redeclared with type %s ..."
          var (string_of_typ old_typ) (string_of_typ new_typ)
    ) ; (SAssign(var, sexpr, new_typ), env)

  (* var not declared, bind typ in map *)
  else
    let flocals = StringMap.add var new_typ env.env_flocals in
    let new_env = {
        env_fmap = env.env_fmap;
        env_fname = env.env_fname;
        env_return_type = env.env_return_type;
        env_flocals = flocals;
        env_globals = env.env_globals;
        env_in_loop = env.env_in_loop;
        env_set_return = env.env_set_return;
        env_sfmap = env.env_sfmap;
    }
    in
    (SAssign(var, sexpr, new_typ), new_env)

(* check unop operations *)
and check_unop op expr env =
  let check_bool_unop = function
      Not       -> Datatype(Bool) 
    | _         -> (raise (E.InvalidUnaryOperation))
  in
  let check_int_unop = function
      Neg       -> Datatype(Int)
    | _         -> (raise (E.InvalidUnaryOperation))
  in
  let check_float_unop = function
      Neg       -> Datatype(Float)
    | _         -> (raise (E.InvalidUnaryOperation))
  in
  let (sexpr, env) = expr_to_sexpr expr env in
  let typ = sexpr_to_type sexpr in
  match typ with
    Datatype(Int)     -> (SUnop(op, sexpr, check_int_unop op), env)
  | Datatype(Float)   -> (SUnop(op, sexpr, check_float_unop op), env)
  | Datatype(Bool)    -> (SUnop(op, sexpr, check_bool_unop op) , env)
  | _                 -> (raise (E.InvalidUnaryOperation))

(* check binop operations *)
and check_binop e1 op e2 env =
  let (se1, env) = expr_to_sexpr e1 env in
  let (se2, env) = expr_to_sexpr e2 env in
  let typ1 = sexpr_to_type se1 in
  let typ2 = sexpr_to_type se2 in
  match op with
    Eq ->
      if (typ1 = typ2 || typ1 = Datatype(Void) || typ2 = Datatype(Void)) then
        if (typ1 = Datatype(String)) then
          (SCall("strcmp", [se1 ; se2], Datatype(Bool)), env)
        else
          (SBinop(se1, op, se2, Datatype(Bool)), env)
      else 
        (raise (E.InvalidBinaryOperation))
  | And | Or ->
      if (typ1 = Datatype(Bool) && typ2 = Datatype(Bool)) then
        (SBinop(se1, op, se2, Datatype(Bool)), env)
      else 
        (raise (E.InvalidBinaryOperation))
  | Lt | Leq | Gt | Geq ->
      if (typ1 = typ2 && (typ1 = Datatype(Int) || typ1 = Datatype(Float))) then
        (SBinop(se1, op, se2, Datatype(Bool)), env)
      else
        (raise (E.InvalidBinaryOperation))
  | Add | Mult | Sub | Div | Mod ->
      if (typ1 = typ2 && (typ1 = Datatype(Int) || typ1 = Datatype(Float))) then
        (SBinop(se1, op, se2, typ1), env)
      else
        (raise (E.InvalidBinaryOperation))

(* check built-in print type inference *)
and check_print e_l env = 
  if ((List.length e_l) <> 1) then
    (raise (E.WrongNumberOfArguments))
  else
    let (se, env) = expr_to_sexpr (List.hd e_l) env in
    let typ = sexpr_to_type se in
    let new_s = match typ with
        Datatype(Int)           -> "printint"
      | Datatype(String)        -> "printstr"
      | Datatype(Bool)          -> "printbool"
      | Datatype(Float)         -> "printfloat"
      | _                       -> (raise E.CannotPrintType)
    in 
    (SCall(new_s, [se], Datatype(Int)), env)

(* check function call *)
and check_call id e_l env = 
  if (not (StringMap.mem id env.env_fmap)) then
    (raise (E.FunctionNotDefined id))
  else
    let env_ref = ref(env) in

    (* semantically check args *) 
    let _ = 
      let check l expr =
        let (sexpr, env) = expr_to_sexpr expr env in
        env_ref := env; (sexpr :: l)
      in
      List.fold_left check [] e_l
    in

    (* list argument types *)
    let arg_type_list = 
      let get_type l expr =
        let (sexpr, _) = expr_to_sexpr expr env in
        let typ = sexpr_to_type sexpr in
        (typ :: l)
      in
      List.fold_left get_type [] e_l
        |> List.rev
    in

    let fdecl = StringMap.find id env.env_fmap in

    (* check for correct number of args *)
    if (List.length e_l <> List.length fdecl.formals) then
      (raise (E.WrongNumberOfArguments))

    (* recursive *)
    else if (id = env.env_fname) then 
      let (e_l, env) = expr_list_to_sexpr_list e_l env in
      (SCall(id, e_l, env.env_return_type), env)

    (* called by another function and already called/defined (lib) *)
    else if (StringMap.mem id env.env_sfmap) then
      let called_fdecl = StringMap.find id env.env_sfmap in
      let check new_typ (s, old_typ) = ignore(
        if (new_typ <> old_typ) then
          let msg = Printf.sprintf "argument %s should be of type %s not %s" 
            s (string_of_typ old_typ) (string_of_typ new_typ)
          in 
          (raise (E.IncorrectArgumentType msg))
      )
      in
      List.iter2 check arg_type_list called_fdecl.sformals ;
      let (se_l, env) = expr_list_to_sexpr_list e_l env in
      (SCall(id, se_l, called_fdecl.styp), env)

  (* called for the first time - semantically check *)
  else 
    let (se_l, env) = expr_list_to_sexpr_list e_l env in
    let new_env = fdecl_to_sfdecl id arg_type_list env in
    let env = {
      env_fmap = env.env_fmap;
      env_fname = env.env_fname;
      env_return_type = env.env_return_type;
      env_globals = env.env_globals;
      env_flocals = env.env_flocals;
      env_in_loop = env.env_in_loop;
      env_set_return = env.env_set_return; 
      env_sfmap = new_env.env_sfmap;         
    }
    in
    let called_fdecl = StringMap.find id env.env_sfmap in
    (SCall(id, se_l, called_fdecl.styp), env)

(* create function declaration map *)
and build_fdecl_map functions =
 
  (* reserved built-ins *) 
  let builtin_decls = (StringMap.add "print" 
  { fname = "print"; formals = [("x")]; 
  body = []; } (StringMap.add "strcmp"
  { fname = "strcmp"; formals = ["x"; "y"];
  body = []; } (StringMap.add "str"
  { fname = "str"; formals = [("x")];
  body = []; } (StringMap.add "num"
  { fname = "num"; formals = [("x")];
  body = []; } (StringMap.add "bool"
  { fname = "bool"; formals = [("x")];
  body = []; } (StringMap.singleton "prints" 
  { fname = "print" ; formals = [("x")];
  body = []; }) )))))

  in
  (* make sure no functions have reserved/duplicated names *)
  let check map fdecl = 
    if (StringMap.mem fdecl.fname map) then
      (raise (E.DuplicateFunctionName fdecl.fname))
    else if (StringMap.mem fdecl.fname builtin_decls) then
      (raise (E.FunctionNameReserved))
    else 
      StringMap.add fdecl.fname fdecl map 
  in
  List.fold_left check builtin_decls functions

(* convert ast to sast *)
and ast_to_sast (globals, functions) fmap =
  (* temp env *)
  let tmp_env = {
    env_fmap = fmap;
    env_fname = "main";    
    env_return_type = Datatype(Int); 
    env_globals = StringMap.empty;
    env_flocals = StringMap.empty;
    env_in_loop = false;
    env_set_return = true;
    env_sfmap = StringMap.empty;
  }
  in

  (* semantically check globals *)
  let env_ref = ref tmp_env in
  let sglobals = 
    let check l (name, expr) =
      let (sexpr, env) = expr_to_sexpr expr !env_ref in
      let typ = sexpr_to_type sexpr in
      env_ref := env ; (name, sexpr, typ) :: l
    in
    List.fold_left check [] globals
      |> List.rev
  in

  (* create globals map *)
  let globals_map = 
    let add_to_map map (name, _, typ) = StringMap.add name typ map in
    List.fold_left add_to_map StringMap.empty sglobals
  in

  (* create new env for semantic check *)
  let env = {
    env_fmap = fmap;
    env_fname = "main";    
    env_return_type = Datatype(Void); 
    env_globals = globals_map;
    env_flocals = StringMap.empty;
    env_in_loop = false;
    env_set_return = false;
    env_sfmap = StringMap.empty;
  }
  in

  (* check that they are no duplicate functions *)
  report_duplicate (List.map (fun fd -> fd.fname) functions); 

  (* convert all fdecls to sfdecls through main *)  
  let env = fdecl_to_sfdecl "main" [] env in

  (* return all checked functions & globals *)
  let sfdecls = 
    let add_sfdecl l (_, sfdecl) = sfdecl :: l in
    List.fold_left add_sfdecl [] (StringMap.bindings env.env_sfmap)
      |> List.rev
  in
  (sglobals, sfdecls)

and check (globals, functions) =
  let fmap = build_fdecl_map functions in
  (* return SAST *)
  ast_to_sast (globals, functions) fmap
