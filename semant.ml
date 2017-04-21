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

  (* if the name already exists, add a number to it to make it unique *)
  let find_unique_name env name =
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
  (* return a fresh, never-used name, but don't actually add it to the symbol
   * table. Useful for creating compiler temporaries.
   *)
  let add_private_name env =
    let unique_name = find_unique_name env "_" in
    ({ env with names = StringSet.add unique_name env.names }, unique_name)
  in
  (* Adds/replaces symbol on the symbol table, returns the new unique name for
   * the symbol 
   *)
  let add_symbol_table env name typ =
    let unique_name = find_unique_name env name in
    ({ env with scope = StringMap.add name (typ, unique_name) env.scope;
       names = StringSet.add unique_name env.names; }, unique_name)
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
      Mat(Float, _, 1) -> ()
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
    let int1 = Mat(Int, 1, 1) and bool1 = Mat(Bool, 1, 1) and float1 =
        Mat(Float, 1, 1)
    and byte1 = Mat(Byte, 1, 1) in [
     { typ = Void; fname = "printi"; formals = [In, (int1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printb"; formals = [In, (bool1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printf"; formals = [In, (float1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "printc"; formals = [In, (byte1, "x")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "set_active_window"; formals = [In, (Window, "w")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "draw_arrays"; formals = [In, (int1, "n")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "swap_buffers"; formals = [In, (Window, "w")];
       fqual = CpuOnly; body = [] };
     { typ = Void; fname = "poll_events"; formals = [];
       fqual = CpuOnly; body = [] };
     { typ = bool1; fname = "window_should_close";
       formals = [In, (Window, "w")]; fqual = CpuOnly; body = [] };
     { typ = Mat(Float, 4, 1); fname = "read_pixel";
       formals = [In, (int1, "x"); In, (int1, "y")];
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

    let check_assign lval rval stmts fail =
      let ltyp = fst lval and rtyp = fst rval in
      if ltyp <> rtyp then
        raise fail
      else
        SAssign(lval, rval) :: stmts in

    (* create a new compiler temporary variable of the given type *)
    let add_tmp env typ =
      let env, name = add_private_name env in
      ({ env with locals = (typ, name) :: env.locals }, (typ, SId(name))) in

    let rec lvalue need_lvalue env stmts = function
        Id s -> let t, s' = find_symbol_table env.scope s in env, stmts, (t, SId(s'))
      | StructDeref(e, m) as d ->
          let env, stmts, e' = lvalue need_lvalue env stmts e in
          let typ = fst e' in
          env, stmts, ((match typ with
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
            | Mat(b, w, 1) ->
                (match m with
                    "x" | "y" when w >= 2 -> Mat(b, 1, 1)
                  | "z" when w >= 3 -> Mat(b, 1, 1)
                  | "w" when w = 4 -> Mat(b, 1, 1)
                  | _ -> raise (Failure ("dereference of nonexistant member " ^ m ^
                      " of a vector")))
            | _ -> raise (Failure ("illegal dereference of type " ^
                string_of_typ typ ^ " in " ^ string_of_expr d)))
          , SStructDeref(e', m))
      | ArrayDeref(e, i) as d ->
          let env, stmts, e' = lvalue need_lvalue env stmts e in
          let env, stmts, i' = expr env stmts i in
          if fst i' <> Mat(Int, 1, 1) then
            raise (Failure ("index expression of of type " ^
              string_of_typ (fst i') ^ " instead of int in " ^
              string_of_expr d))
          else env, stmts, (match fst e' with
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
            expr env stmts e

    (* Return the type of an expression and new expression or throw an exception *)
    and expr (env : translation_environment) stmts = function
	IntLit(l) -> env, stmts, (Mat(Int, 1, 1), SIntLit(l))
      | FloatLit(l) -> env, stmts, (Mat(Float, 1, 1), SFloatLit(l))
      | BoolLit(l) -> env, stmts, (Mat(Bool, 1, 1), SBoolLit(l))
      | CharLit(c) -> env, stmts, (Mat(Byte, 1, 1), SCharLit(c))
      | StringLit(s) ->
          env, stmts, (Array(Mat(Byte, 1, 1), Some (String.length s)), SStringLit(s))
      | Id _ | StructDeref(_, _) | ArrayDeref(_, _) as e ->
          lvalue false env stmts e
      | Binop(e1, op, e2) as e ->
        let env, stmts, e1 = expr env stmts e1 in
        let env, stmts, e2 = expr env stmts e2 in
        let t1 = fst e1 and t2 = fst e2 in
        let typ, op = (match op with
            Add when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Int,
            1, 1), IAdd)
          | Sub when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Int, 1, 1), ISub)
          | Mult when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Int,1, 1), IMult)
          | Div when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Int, 1, 1), IDiv)
          | Equal when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), IEqual)
          | Neq when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), INeq)
          | Add when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Float, 1, 1), FAdd)
          | Sub when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Float, 1, 1), FSub)
          | Mult when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Float, 1, 1), FMult)
          | Div when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Float, 1, 1), FDiv)
          | Equal when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FEqual)
          | Neq when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FNeq)
          | Less when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FLess)
          | Leq when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FLeq)
          | Greater when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FGreater)
          | Geq when t1 = Mat(Float, 1, 1) && t2 = Mat(Float, 1, 1) -> (Mat(Bool, 1, 1), FGeq)
          | Equal when t1 = Mat(Bool, 1, 1) && t2 = Mat(Bool, 1, 1) -> (Mat(Bool, 1, 1), BEqual)
          | Neq when t1 = Mat(Bool, 1, 1) && t2 = Mat(Bool, 1, 1) -> (Mat(Bool, 1, 1), BNeq)
          | Less when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), ILess)
          | Leq when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), ILeq)
          | Greater when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), IGreater)
          | Geq when t1 = Mat(Int, 1, 1) && t2 = Mat(Int, 1, 1) -> (Mat(Bool, 1, 1), IGeq)
          | And when t1 = Mat(Bool, 1, 1) && t2 = Mat(Bool, 1, 1) -> (Mat(Bool, 1, 1), BAnd)
          | Or when t1 = Mat(Bool, 1, 1) && t2 = Mat(Bool, 1, 1) -> (Mat(Bool, 1, 1), BOr)
          | _ -> raise (Failure ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e))
          )
        in env, stmts, (typ, SBinop(e1, op, e2))
      | Unop(op, e) as ex -> let env, stmts, e = expr env stmts e in
         let t = fst e in
         let typ, op = (match op with
	   Neg when t = Mat(Int, 1, 1) -> (Mat(Int, 1, 1), INeg)
	 | Neg when t = Mat(Float, 1, 1) -> (Mat(Float, 1, 1), FNeg)
	 | Not when t = Mat(Bool, 1, 1) -> (Mat(Bool, 1, 1), BNot)
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex))) in
         env, stmts, (typ, SUnop(op, e))
      | Noexpr -> env, stmts, (Void, SNoexpr)
      | Assign(lval, e) as ex ->
          let env, stmts, lval = lvalue true env stmts lval in
          let env, stmts, e = expr env stmts e in
          env, check_assign lval e stmts
            (Failure ("illegal assignment " ^ string_of_typ (fst lval) ^
              " = " ^ string_of_typ (fst e) ^ " in " ^ string_of_expr ex)),
          lval
      | Call("length", [arr]) as call ->
          let env, stmts, arr = expr env stmts arr in
          let env, tmp = add_tmp env (Mat(Int, 1, 1)) in
          env, (match fst arr with
              Array(_, _) -> SCall(tmp, "length", [arr])
            | _ as typ ->
                raise (Failure ("expecting an array type instead of " ^
                  string_of_typ typ ^ " in " ^ string_of_expr call))) :: stmts,
          tmp
      | Call("upload_buffer", [buf; data]) as call ->
          check_call_qualifiers env "upload_buffer" CpuOnly;
          let env, stmts, buf = expr env stmts buf in
          let env, stmts, data = expr env stmts data in
          env, (match fst buf with
              Buffer(t) ->
                (match fst data with
                    Array(t', _) -> if t' = t then
                      SCall((Void, SNoexpr), "upload_buffer", [buf; data])
                    else
                      raise (Failure ("buffer and array type do not match " ^
                        "in " ^ string_of_expr call))
                  | _ -> raise (Failure ("must upload an array in " ^
                    "upload_buffer in " ^ string_of_expr call)))
            | _ -> raise (Failure ("first parameter to upload_buffer must be " ^
                    "a buffer in " ^ string_of_expr call))) :: stmts,
          (Void, SNoexpr)
      | Call("bind_pipeline", [p]) as call ->
          check_call_qualifiers env "bind_pipeline" CpuOnly;
          let env, stmts, p' = expr env stmts p in
          env, (match fst p' with
              Pipeline(_) ->
                SCall((Void, SNoexpr), "bind_pipeline", [p'])
            | _ as t -> raise (Failure ("calling bind_pipeline with " ^
              string_of_typ t ^ " instead of pipeline in " ^ 
              string_of_expr call))) :: stmts,
          (Void, SNoexpr)
      | Call(fname, actuals) as call -> let fd = function_decl fname in
          check_call_qualifiers env fname fd.fqual;
          if List.length actuals != List.length fd.formals then
            raise (Failure ("expecting " ^ string_of_int
              (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
          else
            let env, stmts, actuals = List.fold_left2
              (* translate/evaluate function arguments *)
              (fun (env, stmts, actuals) (fq, _) e ->
                let env, stmts, se = if fq = In then
                  expr env stmts e
                else
                  lvalue true env stmts e in
                env, stmts, ((fq, se) :: actuals)) (env, stmts, []) fd.formals actuals in
            let actuals = List.rev actuals in
            (* make a temporary for each formal parameter *)
            let env, params = List.fold_left (fun (env, temps) (_, (ft, _)) ->
              let env, temp = add_tmp env ft in
              (env, temp :: temps)) (env, []) fd.formals in
            let params = List.rev params in
            (* copy in-parameters to temporaries *)
            let stmts = List.fold_left2 (fun stmts temp (fq, actual) ->
              if fq = Out then
                stmts
              else
                let et = fst actual in
                let ft = fst temp in
                check_assign temp actual stmts
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr call)))
            stmts params actuals in
            (* make call *)
            let env, ret_tmp = if fd.typ = Void then
              env, (Void, SNoexpr)
            else
              let env, tmp = add_tmp env fd.typ in
              env, tmp in
            let stmts = SCall(ret_tmp, fd.fname, params) :: stmts in
            (* copy temporaries to out-parameters *)
            let stmts = List.fold_left2 (fun stmts temp (fq, actual) ->
              if fq = In then
                stmts
              else
                let et = fst actual in
                let ft = fst temp in
                check_assign actual temp stmts
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr call)))
             stmts params actuals in
            (* return the temporary we made for the call *)
            env, stmts, ret_tmp
      | TypeCons(typ, actuals) as cons ->
          let check_cons formals =
            if List.length actuals != List.length formals then
              raise (Failure ("expecting " ^ string_of_int (List.length formals) ^
               " arguments in constructor for " ^ string_of_typ typ))
            else 
              let env, stmts, actuals = List.fold_left
                (* translate/evaluate function arguments *)
                (fun (env, stmts, actuals) e ->
                  let env, stmts, se = expr env stmts e in
                  env, stmts, se :: actuals) (env, stmts, []) actuals in
              let actuals = List.rev actuals in
              (* make a temporary for each formal parameter *)
              let env, params = List.fold_left (fun (env, temps) ft ->
                let env, temp = add_tmp env ft in
                (env, temp :: temps)) (env, []) formals in
              let params = List.rev params in
              (* copy in-parameters to temporaries *)
              let stmts = List.fold_left2 (fun stmts temp actual ->
                let et = fst actual in
                let ft = fst temp in
                check_assign temp actual stmts
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr cons)))
              stmts params actuals in
              env, stmts, (typ, STypeCons(params))
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
            | Struct s -> expr env stmts (Call(s, actuals))
            | Mat(b, w, 1) -> handle_array_vec (Mat(b, 1, 1)) w
            | Array(t, Some s) -> handle_array_vec t s
            | Array(_, None) -> check_cons [Mat(Int, 1, 1)]
            | Buffer(t) -> check_buffer_type t; check_cons []
            | Pipeline(_) -> check_cons []
            | Window -> check_cons [Mat(Int, 1, 1); Mat(Int, 1, 1); Mat(Bool, 1, 1)]
            | _ -> raise (Failure ("unhandled type constructor for " ^
                      string_of_typ typ));
                  
    in

    let check_bool_expr env stmts e =
      let env, stmts, se = expr env stmts e in 
        if fst se <> Mat(Bool, 1, 1) then
          raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
        else env, stmts, se in

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
            | Return e ->
                let env, sstmts, se = expr env sstmts e in
                let env, tmp = add_tmp env func.typ in
                let sstmts = check_assign tmp se sstmts
                  (Failure ("return gives " ^ string_of_typ (fst se) ^ " expected " ^
                           string_of_typ func.typ ^ " in " ^ string_of_expr e))
                in
                env, (SReturn(tmp) :: sstmts)
            | Block sl -> let env', sstmts = stmts' env sstmts sl in
                { env with locals = env'.locals; names = env'.names; }, sstmts
            | If(p, b1, b2) ->
                let env, sstmts, p = check_bool_expr env sstmts p in
                let env, sthen = check_stmt env env.in_loop b1 in
                let env, selse = check_stmt env env.in_loop b2 in
                env, (SIf(p, sthen, selse) :: sstmts)
            | For(e1, e2, e3, st) ->
                let env, sstmts, _ = expr env sstmts e1 in
                let env, cond_stmts = check_stmt env true
                  (If (e2, Block([]), Break)) in (* if (!e2) break; *)
                let env, continue_stmts =
                  check_stmt env false (Expr(e3)) in
                let env, body = check_stmt env true st in
                env, (SLoop(cond_stmts @ body, continue_stmts) :: sstmts)
            | While(p, s) ->
                let env, cond_stmts = check_stmt env true
                  (If (p, Block([]), Break)) in (* if (!p) break; *)
                let env, body = check_stmt env true s in
                env, (SLoop(cond_stmts @ body, []) :: sstmts)
            | Expr e -> 
                let env, sstmts, _ = expr env sstmts e in
                env, sstmts
            | Local ((t, s) as b, oe) ->
                (check_type
                  (fun s n -> s ^ " does not exist for local " ^ n ^
                    " in " ^ func.fname)
                  (fun n -> "illegal void local " ^ n ^
                                  " in " ^ func.fname) b);
                let env, name = add_symbol_table env s t in
                let env = { env with locals = (t, name) :: env.locals } in
                match oe with
                    Some e -> let env, sstmts, e' = expr env sstmts e in
                      let sstmts = check_assign (t, SId name) e' sstmts
                        (Failure ("illegal initialization " ^ string_of_typ t ^
                          " = " ^ string_of_typ (fst e') ^ " in " ^
                          string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^
                          ";")) in
                      env, sstmts
                  | None -> env, sstmts)
      (env, sstmts) sl
    in

    (* check return type of shaders *)
    (match func.fqual with
        Vertex -> if func.typ <> Mat(Float, 4, 1) then
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
    fold_sfdecl_pre (fun calls stmt ->
      match stmt with
          SCall(_, name, _) -> StringMap.find name function_decls :: calls
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
