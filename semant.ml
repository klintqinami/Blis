(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
open Utils

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbol = Ast.typ * string

type translation_environment = {
  scope : symbol StringMap.t;
  names : StringSet.t;
  locals : bind list;
  cur_qualifier : func_qualifier;
  in_loop : bool;
}

(* Semantic checking of a program. Returns the SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program =
  let globals = program.var_decls in
  let functions = program.func_decls in
  let structs = program.struct_decls in
  let pipelines = program.pipeline_decls in

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  let find_symbol_table table name =
    if StringMap.mem name table then
      StringMap.find name table
    else
      raise (Failure ("undeclared identifier " ^ name))
  in

  (* Adds/replaces symbol on the symbol table, returns the new unique name for
   * the symbol 
   *)
  let add_symbol_table env name typ =
    (* if the name already exists, add a number to it to make it unique *)
    let find_unique_name name =
      if not (StringSet.mem name env.names) then
        name
      else
        let rec find_unique_name' env name n = 
          let unique_name = name ^ string_of_int n in
          if not (StringSet.mem unique_name env.names) then
            unique_name
          else
            find_unique_name' env name (n + 1)
        in
        find_unique_name' env name 1
    in
    let unique_name = find_unique_name name in
    ({ env with scope = StringMap.add name (typ, unique_name) env.scope;
       names = StringSet.add unique_name env.names; }, unique_name)
  in

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet = rvaluet then lvaluet else raise err
  in

  (**** Checking Structure and Pipeline Declarations ****)

  report_duplicate
    (fun n -> "duplicate structure " ^ n)
    (List.map (fun s -> s.sname) structs);

  report_duplicate
    (fun n -> "duplicate pipeline " ^ n)
    (List.map (fun p -> p.pname) pipelines);

  let struct_decls = List.fold_left (fun m s ->
    StringMap.add s.sname s m) StringMap.empty structs
  in

  let pipeline_decls = List.fold_left (fun m p ->
    StringMap.add p.pname p m) StringMap.empty pipelines
  in

  let check_buffer_type = function
      Vec(Float, _) -> ()
    | _ as t -> raise (Failure ("bad type " ^ string_of_typ t ^ " for buffer"))
  in

  let check_return_type exceptf = function
      (Struct s) -> if not (StringMap.mem s struct_decls) then
          raise (Failure (exceptf ("struct " ^ s)))
        else
          ()
    | (Pipeline p) -> if not (StringMap.mem p pipeline_decls) then
          raise (Failure (exceptf ("pipeline " ^ p)))
        else
          ()
    | (Buffer t) -> check_buffer_type t
    | _ -> ()
  in

  (* Raise an exception if a given binding is to a void type or a struct that
   * doesn't exist *)
  let rec check_type bad_struct void = function
      (Struct s, n) -> if not (StringMap.mem s struct_decls) then
          raise (Failure (bad_struct ("struct " ^ s) n))
        else
          ()
    | (Pipeline p, n) -> if not (StringMap.mem p pipeline_decls) then
          raise (Failure (bad_struct ("pipeline " ^ p) n))
        else
          ()
    | (Array(t, _), n) -> check_type bad_struct void (t, n)
    | (Buffer(t), _) -> check_buffer_type t
    | (Void, n) -> raise (Failure (void n))
    | _ -> ()
  in

  List.iter (fun s ->
    report_duplicate
      (fun m -> "duplicate member " ^ m ^ " of structure " ^ s.sname)
      (List.map snd s.members)) structs;

  List.iter (fun s ->
    List.iter (fun m -> check_type
      (fun sn n -> sn ^ " does not exist in member " ^ n ^ 
        " of struct " ^ s.sname)
      (fun n -> "illegal void member " ^ n ^ " of struct " ^ s.sname)
      m)
    s.members)
  structs;

  (* sort structures and check for cycles in the definitions *)
  let structs =
    (* function from struct name to a list of struct names used in its members *)
    let succs s = List.fold_left (fun succs m ->
      match base_type (fst m) with
          Struct name -> StringMap.find name struct_decls :: succs
        | _ -> succs)
    [] s.members
    in
    tsort structs succs (fun cycle ->
      raise (Failure ("cycle in struct definitions: " ^
        String.concat " -> " (List.map (fun s -> s.sname) cycle))))
  in
   
  (* Add struct constructors to function declarations *)
  let functions = List.fold_left (fun functions s -> 
    { typ = Struct(s.sname); 
      fname = s.sname; 
      fqual = Both;
      formals = List.map (fun b -> (In, b)) s.members; 
      body = Local((Struct(s.sname), "tmp"), None) ::
        (List.map (fun m -> 
          Expr(Assign(StructDeref(Id("tmp"), snd m), Id(snd m)))) s.members) 
        @ [Return(Id("tmp"))];
    } :: functions      
    )
    functions structs
  in
  

  (**** Checking Global Variables ****)

  List.iter (check_type
    (fun s n -> s ^ " does not exist for global " ^ n)
    (fun n -> "illegal void global " ^ n)) globals;

  let env = {
    in_loop = false;
    scope = StringMap.empty;
    cur_qualifier = Both;
    locals = [];
    names = StringSet.empty; } in

  let env = List.fold_left (fun env (typ, name) ->
      fst (add_symbol_table env name typ))
    env globals in
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)

  let built_in_decls =
    let int1 = Vec(Int, 1) and bool1 = Vec(Bool, 1) and float1 = Vec(Float, 1)
    and byte1 = Vec(Byte, 1) in [
     { typ = Void; fname = "print"; formals = [In, (int1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printb"; formals = [In, (bool1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printf"; formals = [In, (float1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printc"; formals = [In, (byte1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "set_active_window"; formals = [In, (Window, "w")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "draw_arrays"; formals = [In, (Vec(Int, 1), "n")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "swap_buffers"; formals = [In, (Window, "w")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "poll_events"; formals = [];
       fqual = CpuOnly; body = [] };
     { typ = Vec(Bool, 1); fname = "window_should_close";
       formals = [In, (Window, "w")]; fqual = CpuOnly; body = [] };
     { typ = Vec(Float, 4); fname = "read_pixel";
       formals = [In, (Vec(Int, 1), "x"); In, (Vec(Int, 1), "y")];
       fqual = CpuOnly; body = [] };
    ]
  in

  List.iter (fun built_in_decl ->
    let name = built_in_decl.fname in
    if List.mem name (List.map (fun fd -> fd.fname) functions)
    then raise (Failure ("function " ^ name ^ " may not be defined")) else ())
  built_in_decls;
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         StringMap.empty (built_in_decls @ functions)
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let main = function_decl "main" in (* Ensure "main" is defined *)
  if main.fqual <> CpuOnly then
    raise (Failure "main function has bad qualifier")
  else
    ()
  ;

  (* Check pipeline shaders, and add inputs to pipeline declarations.
   * We want to do this after function_decls has been constructed.
   *)

  let pipelines = List.map (fun pd ->
    if pd.fshader = "" then 
      raise (Failure ("pipeline "  ^ pd.pname ^ 
        " doesn't contain a fragment shader"))
    else if pd.vshader = "" then
      raise (Failure ("pipeline "  ^ pd.pname ^ 
        " doesn't contain a vertex shader"))
    else
      let vert_decl = function_decl pd.vshader in
      let frag_decl = function_decl pd.fshader in
      if vert_decl.fqual <> Vertex then
        raise (Failure
         ("vertex entrypoint " ^ pd.vshader ^ " in pipeline " ^ pd.pname ^
          "is not marked @vertex"))
      else if frag_decl.fqual <> Fragment then
        raise (Failure
         ("fragment entrypoint " ^ pd.vshader ^ " in pipeline " ^ pd.pname ^
          "is not marked @fragment"))
      else
        { spname = pd.pname;
          sfshader = pd.fshader;
          svshader = pd.vshader;
          sinputs = List.map (fun (_, (t, n)) -> (Buffer(t), n))
            (List.filter (fun (qual, _) -> qual = In) vert_decl.formals); })
  pipelines
  in

  let pipeline_decls = List.fold_left (fun m p ->
    StringMap.add p.spname p m) StringMap.empty pipelines
  in

  let check_function func =

    List.iter (check_type
      (fun s n -> s ^ " does not exist for formal " ^ n ^ " in " ^
        func.fname)
      (fun n -> "illegal void formal " ^ n ^ " in " ^ func.fname))
    (List.map snd func.formals);

    check_return_type
      (fun s -> s ^ " does not exist in return type of function " ^
      func.fname) func.typ;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map (fun (_, (_, n)) -> n) func.formals);

    let check_call_qualifiers env fname fqual =
      (match env.cur_qualifier, fqual with
          (CpuOnly, CpuOnly)
        | (CpuOnly, Both)
        | (Vertex, GpuOnly)
        | (Vertex, Both)
        | (Fragment, GpuOnly)
        | (Fragment, Both)
        | (GpuOnly, GpuOnly)
        | (GpuOnly, Both)
        | (Both, Both) -> ()
        |  _ -> raise (Failure ("cannot call " ^ string_of_func_qual fqual ^
                  " function " ^ fname ^ " from " ^ 
                  string_of_func_qual env.cur_qualifier ^ " function "
                  ^ func.fname)))
    in

    let rec lvalue need_lvalue env = function
        Id s -> let t, s' = find_symbol_table env.scope s in (t, SId(s'))
      | StructDeref(e, m) as d -> let e' = lvalue need_lvalue env e in
          let typ = fst e' in
          ((match typ with
              Struct s ->
                let stype = StringMap.find s struct_decls in
                (try
                  fst (List.find (fun b -> snd b = m) stype.members)
                with Not_found ->
                  raise (Failure ("struct " ^ s ^ " does not contain member " ^
                    m ^ " in " ^ string_of_expr d)))
            | Pipeline p ->
                let ptype = StringMap.find p pipeline_decls in
                (try
                  fst (List.find (fun b -> snd b = m) ptype.sinputs)
                with Not_found ->
                  raise (Failure ("pipeline " ^ p ^ " does not contain input " ^
                  m ^ " in " ^ string_of_expr d)))
            | Vec(b, w) ->
                (match m with
                    "x" | "y" when w >= 2 -> Vec(b, 1)
                  | "z" when w >= 3 -> Vec(b, 1)
                  | "w" when w = 4 -> Vec(b, 1)
                  | _ -> raise (Failure ("dereference of nonexistant member " ^ m ^
                      " of a vector")))
            | _ -> raise (Failure ("illegal dereference of type " ^
                string_of_typ typ ^ " in " ^ string_of_expr d)))
          , SStructDeref(e', m))
      | ArrayDeref(e, i) as d ->
          let e' = lvalue need_lvalue env e and i' = expr env i in
          if fst i' <> Vec(Int, 1) then
            raise (Failure ("index expression of of type " ^
              string_of_typ (fst i') ^ " instead of int in " ^
              string_of_expr d))
          else (match fst e' with
              Array(t, Some(_)) -> (t, SArrayDeref(e', i'))
            | Array(t, None) -> if env.cur_qualifier = CpuOnly
                then (t, SArrayDeref(e', i'))
                else raise (Failure "variable sized arrays cannot be used in GPU code")
            | _ -> raise (Failure ("array dereference of non-array type in " ^
                      string_of_expr d)))
      | _ as e ->
          if need_lvalue then
            raise (Failure ("expression " ^ string_of_expr e ^ " is not an lvalue"))
          else
            expr env e

    (* Return the type of an expression and new expression or throw an exception *)
    and expr (env : translation_environment) = function
	IntLit(l) -> (Vec(Int, 1), SIntLit(l))
      | FloatLit(l) -> (Vec(Float, 1), SFloatLit(l))
      | BoolLit(l) -> (Vec(Bool, 1), SBoolLit(l))
      | CharLit(c) -> (Vec(Byte, 1), SCharLit(c))
      | StringLit(s) -> (Array(Vec(Byte, 1), Some (String.length s)), SStringLit(s))
      | Id _ | StructDeref(_, _) | ArrayDeref(_, _) as e -> lvalue false env e
      | Binop(e1, op, e2) as e -> let e1 = expr env e1 and e2 = expr env e2 in
        let t1 = fst e1 and t2 = fst e2 in
        let typ, op = (match op with
            Add when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Int, 1), IAdd)
          | Sub when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Int, 1), ISub)
          | Mult when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Int, 1), IMult)
          | Div when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Int, 1), IDiv)
          | Equal when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), IEqual)
          | Neq when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), INeq)
          | Add when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Float, 1), FAdd)
          | Sub when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Float, 1), FSub)
          | Mult when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Float, 1), FMult)
          | Div when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Float, 1), FDiv)
          | Equal when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FEqual)
          | Neq when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FNeq)
          | Less when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FLess)
          | Leq when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FLeq)
          | Greater when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FGreater)
          | Geq when t1 = Vec(Float, 1) && t2 = Vec(Float, 1) -> (Vec(Bool, 1), FGeq)
          | Equal when t1 = Vec(Bool, 1) && t2 = Vec(Bool, 1) -> (Vec(Bool, 1), BEqual)
          | Neq when t1 = Vec(Bool, 1) && t2 = Vec(Bool, 1) -> (Vec(Bool, 1), BNeq)
          | Less when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), ILess)
          | Leq when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), ILeq)
          | Greater when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), IGreater)
          | Geq when t1 = Vec(Int, 1) && t2 = Vec(Int, 1) -> (Vec(Bool, 1), IGeq)
          | And when t1 = Vec(Bool, 1) && t2 = Vec(Bool, 1) -> (Vec(Bool, 1), BAnd)
          | Or when t1 = Vec(Bool, 1) && t2 = Vec(Bool, 1) -> (Vec(Bool, 1), BOr)
          | _ -> raise (Failure ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e))
          )
        in (typ, SBinop(e1, op, e2))
      | Unop(op, e) as ex -> let e = expr env e in
         let t = fst e in
	 (match op with
	   Neg when t = Vec(Int, 1) -> (Vec(Int, 1), SUnop(INeg, e))
	 | Neg when t = Vec(Float, 1) -> (Vec(Float, 1), SUnop(FNeg, e))
	 | Not when t = Vec(Bool, 1) -> (Vec(Bool, 1), SUnop(BNot, e))
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> (Void, SNoexpr)
      | Assign(lval, e) as ex -> let lval = lvalue true env lval in
                                let e = expr env e in
                                let lt = fst lval in
                                let rt = fst e in
        (check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				      " = " ^ string_of_typ rt ^ " in " ^ 
				      string_of_expr ex)), SAssign(lval, e))
      | Call("length", [arr]) as call ->
          let arr = expr env arr in
          (match fst arr with
              Array(_, _) -> (Vec(Int, 1), SCall("length", [arr]))
            | _ as typ ->
                raise (Failure ("expecting an array type instead of " ^
                  string_of_typ typ ^ " in " ^ string_of_expr call)))
      | Call("upload_buffer", [buf; data]) as call ->
          check_call_qualifiers env "upload_buffer" CpuOnly;
          let buf = expr env buf and data = expr env data in
          (match fst buf with
              Buffer(t) ->
                (match fst data with
                    Array(t', _) -> if t' = t then
                      (Void, SCall("upload_buffer", [buf; data]))
                    else
                      raise (Failure ("buffer and array type do not match " ^
                        "in " ^ string_of_expr call))
                  | _ -> raise (Failure ("must upload an array in " ^
                    "upload_buffer in " ^ string_of_expr call)))
            | _ -> raise (Failure ("first parameter to upload_buffer must be " ^
                    "a buffer in " ^ string_of_expr call)))
      | Call("bind_pipeline", [p]) as call ->
          check_call_qualifiers env "bind_pipeline" CpuOnly;
          let p' = expr env p in
          (match fst p' with
              Pipeline(_) ->
                (Void, SCall("bind_pipeline", [p']))
            | _ as t -> raise (Failure ("calling bind_pipeline with " ^
              string_of_typ t ^ " instead of pipeline in " ^ 
              string_of_expr call)))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
          check_call_qualifiers env fname fd.fqual;
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int
              (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
          else
            (fd.typ, SCall(fd.fname,
              List.map2 (fun (fq, (ft, _)) e ->
                let se = if fq = In then
                  expr env e
                else
                  lvalue true env e in
                let et = fst se in
                ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e)));
                se)
              fd.formals actuals))
      | TypeCons(typ, actuals) ->
          let check_cons formals =
            if List.length actuals != List.length formals then
              raise (Failure ("expecting " ^ string_of_int (List.length formals) ^
               " arguments in constructor for " ^ string_of_typ typ))
            else (typ, STypeCons(List.map2 (fun e ft ->
              let se = expr env e in
              let atyp = fst se in
              ignore (check_assign ft atyp
                (Failure ("expecting type " ^ string_of_typ ft ^
                  " in constructor for " ^ string_of_typ typ)));
              se) actuals formals))
          in
          let handle_array_vec base_type size =
            let rec copies n =
              if n = 0 then [] else base_type :: copies (n-1) in
            check_cons (copies size)
          in
          match typ with
              (* struct constructors and functions are in the same namespace,
               * and we'll handle struct constructors as regular functions
               * anyways.
               *)
            | Struct s -> expr env (Call(s, actuals))
            | Vec(b, w) -> handle_array_vec (Vec(b, 1)) w
            | Array(t, Some s) -> handle_array_vec t s
            | Array(_, None) -> check_cons [Vec(Int, 1)]
            | Buffer(t) -> check_buffer_type t; check_cons []
            | Pipeline(_) -> check_cons []
            | Window -> check_cons [Vec(Int, 1); Vec(Int, 1); Vec(Bool, 1)]
            | _ -> raise (Failure ("unhandled type constructor for " ^
                      string_of_typ typ));
                  
    in

    let check_bool_expr env e =
      let se = expr env e in 
        if fst se <> Vec(Bool, 1) then
          raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
        else se in

    let check_in_loop env = if env.in_loop then () else
      raise (Failure ("break/continue must be inside a loop")) in

    (* Verify a statement or throw an exception *)
    let rec check_stmt env in_loop_new = function
        Local((_, s), _) -> raise (Failure ("local variable " ^ s ^
          " not declared inside a block"))
      | _ as s ->
          let env', sl = stmts' { env with in_loop = in_loop_new; } [] [s] in
          { env with locals = env'.locals; names = env'.names; }, List.rev sl
    (* Helper function that returns the list of SAST statements in reverse 
     * order *)
    and stmts' env sstmts sl = List.fold_left
      (fun (env, sstmts) stmt ->
        match sstmts with
            SBreak :: _ -> raise (Failure "nothing may follow a break")
          | SContinue :: _ -> raise (Failure "nothing may follow a continue")
          | SReturn _ :: _ -> raise (Failure "nothing may follow a return")
          | _ -> match stmt with
              Break -> check_in_loop env; env, (SBreak :: sstmts)
            | Continue -> check_in_loop env; env, (SContinue :: sstmts)
            | Return e -> let se = expr env e in
              ignore (check_assign func.typ (fst se) 
                (Failure ("return gives " ^ string_of_typ (fst se) ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e)));
              env, (SReturn(se) :: sstmts)
            | Block sl -> let env', sstmts = stmts' env sstmts sl in
                { env with locals = env'.locals; names = env'.names; }, sstmts
            | If(p, b1, b2) ->
                let p = check_bool_expr env p in
                let env, sthen = check_stmt env env.in_loop b1 in
                let env, selse = check_stmt env env.in_loop b2 in
                env, (SIf(p, sthen, selse) :: sstmts)
            | For(e1, e2, e3, st) ->
                let e1 = expr env e1 in
                let e2 = check_bool_expr env e2 in
                let e3 = expr env e3 in
                let env, body = check_stmt env true st in
                  env, (SFor(e1, e2, e3, body) :: sstmts)
            | While(p, s) ->
                let p = check_bool_expr env p in
                let env, body = check_stmt env true s in
                  env, (SWhile(p, body) :: sstmts)
            | Expr e -> env, (SExpr(expr env e) :: sstmts)
            | Local ((t, s) as b, oe) ->
                (check_type
                  (fun s n -> s ^ " does not exist for local " ^ n ^
                    " in " ^ func.fname)
                  (fun n -> "illegal void local " ^ n ^
                                  " in " ^ func.fname) b);
                let env, name = add_symbol_table env s t in
                let env = { env with locals = (t, name) :: env.locals } in
                match oe with
                    Some e -> let e' = expr env e in
                      ignore (check_assign t (fst e')
                        (Failure ("illegal initialization " ^ string_of_typ t ^
                          " = " ^ string_of_typ (fst e') ^ " in " ^
                          string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^
                          ";")));
                      env, SExpr(t, SAssign((t, SId name), e')) :: sstmts
                  | None -> env, sstmts)
      (env, sstmts) sl
    in

    (* check return type of shaders *)
    (match func.fqual with
        Vertex -> if func.typ <> Vec(Float, 4) then
          raise (Failure ("vertex entrypoint " ^ func.fname ^
            " must return vec4"))
        else
          ()
      | Fragment -> if func.typ <> Void then
          raise (Failure ("fragment entrypoint " ^ func.fname ^
            " must return void"))
        else
          ()
      | _ -> ())
    ;

    let env = { env with cur_qualifier = func.fqual } in

    let env = List.fold_left (fun env (_, (t, s)) ->
      fst (add_symbol_table env s t)) env func.formals
    in

    let env, sbody = stmts' env [] func.body in
    if func.typ <> Void then match sbody with
        SReturn _ :: _ -> ()
      | _ -> raise (Failure ("missing final return from function " ^ func.fname ^
                    " with non-void return type"))
    else ()
    ;

    {
      styp = func.typ;
      sfname = func.fname;
      sfqual = func.fqual;
      sformals = func.formals;
      slocals = env.locals;
      sbody = List.rev sbody;
    }

   
  in

  let functions = List.map check_function functions
  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.sfname fd m)
                         StringMap.empty functions
  in

  (* do a topological sort of the GPU-only function call graph to check for
   * loops, and to ensure that functions are always defined before they're
   * called for the GLSL backend since GLSL cares about the ordering.
   *)
  let func_succs fdecl =
    fold_sfdecl_pre (fun calls expr ->
      match snd expr with
          SCall(name, _) -> StringMap.find name function_decls :: calls
        | _ -> calls)
    [] fdecl
  in
  let gpu_functions = List.filter (fun fdecl ->
    match fdecl.sfqual with
        GpuOnly | Fragment | Vertex | Both -> true
      | CpuOnly -> false) functions
  in
  let cpu_functions = List.filter (fun fdecl ->
    fdecl.sfqual = CpuOnly) functions
  in
  let gpu_functions = tsort gpu_functions func_succs (fun cycle ->
    raise (Failure ("recursive call by not-CPU-only functions: " ^
      String.concat " -> " (List.map (fun f -> f.sfname) cycle))))
  in
  (structs, pipelines, globals, gpu_functions @ cpu_functions)
