(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbol = Ast.typ * string

type translation_environment = {
  scope : symbol StringMap.t;
  names : StringSet.t;
  locals : bind list;
  in_loop : bool;
}

(* Semantic checking of a program. Returns the SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program =
  let globals = program.var_decls in
  let functions = program.func_decls in
  let structs = program.struct_decls in

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

  (**** Checking Structure Declarations ****)

  report_duplicate
    (fun n -> "duplicate structure " ^ n)
    (List.map (fun s -> s.sname) structs);

  let struct_decls = List.fold_left (fun m s ->
    StringMap.add s.sname s m) StringMap.empty structs
  in

  let check_return_type exceptf = function
      (Struct s) -> if not (StringMap.mem s struct_decls) then
          raise (Failure (exceptf s))
        else
          ()
    | _ -> ()
  in

  (* Raise an exception if a given binding is to a void type or a struct that
   * doesn't exist *)
  let check_type bad_struct void = function
      (Struct s, n) -> if not (StringMap.mem s struct_decls) then
          raise (Failure (bad_struct s n))
        else
          ()
    | (Void, n) -> raise (Failure (void n))
    | _ -> ()
  in

  List.iter (fun s ->
    report_duplicate
      (fun m -> "duplicate member " ^ m ^ " of structure " ^ s.sname)
      (List.map snd s.members)) structs;

  List.iter (fun s ->
    List.iter (fun m -> check_type
      (fun sn n -> "struct " ^ sn ^ " does not exist in member " ^ n ^ 
        " of struct " ^ s.sname)
      (fun n -> "illegal void member " ^ n ^ " of struct " ^ s.sname)
      m)
    s.members)
  structs;

  let _ =
    (* function from struct name to a list of struct names used in its members *)
    let succs s = StringSet.elements (List.fold_left (fun succs m ->
      match fst m with
          Struct succ -> StringSet.add succ succs
        | _ -> succs)
    StringSet.empty (StringMap.find s struct_decls).members)
    in
    let rec tsort path visited = function
        [] -> visited
      | n :: nodes -> 
          if List.mem n path then
            raise (Failure ("cycle in struct definitions: " ^
              String.concat " -> " (List.rev (n :: path))))
          else
            let v' = if List.mem n visited then visited else
              n :: tsort (n :: path) visited (succs n)
            in tsort path v' nodes
    in
    ignore (tsort [] [] (List.map (fun s -> s.sname) structs))
  in
   
  (* Add struct constructors to function declarations *)
  let functions = List.fold_left (fun functions s -> 
    {typ = Struct(s.sname); 
      fname = s.sname; 
      formals = List.map (fun b -> (In, b)) s.members; 
      body = Local((Struct(s.sname), "tmp"), None) ::
        (List.map (fun m -> 
          Expr(Assign(Deref(Id("tmp"), snd m), Id(snd m)))) s.members) 
        @ [Return(Id("tmp"))];
    } :: functions      
    )
    functions structs
  in
  

  (**** Checking Global Variables ****)

  List.iter (check_type
    (fun s n -> "struct " ^ s ^ " does not exist for global " ^ n)
    (fun n -> "illegal void global " ^ n)) globals;

  let env = {
    in_loop = false;
    scope = StringMap.empty;
    locals = [];
    names = StringSet.empty; } in

  let env = List.fold_left (fun env (typ, name) ->
      fst (add_symbol_table env name typ))
    env globals in
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  [
     { typ = Void; fname = "print"; formals = [In, (Vec(Int, 1), "x")];
       body = [] };
     { typ = Void; fname = "printb"; formals = [In, (Vec(Bool, 1), "x")];
       body = [] };
     { typ = Void; fname = "printf"; formals = [In, (Vec(Float, 1), "x")];
       body = [] }]
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         StringMap.empty (built_in_decls @ functions)
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_type
      (fun s n -> "struct " ^ s ^ " does not exist for formal " ^ n ^ " in " ^
        func.fname)
      (fun n -> "illegal void formal " ^ n ^ " in " ^ func.fname))
    (List.map snd func.formals);

    check_return_type
      (fun s -> "struct " ^ s ^ " does not exist in return type of function " ^
      func.fname) func.typ;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map (fun (_, (_, n)) -> n) func.formals);

    let rec lvalue need_lvalue env = function
        Id s -> let t, s' = find_symbol_table env.scope s in (t, SId(s'))
      | Deref(e, m) as d -> let e' = lvalue need_lvalue env e in
          let typ = fst e' in
          ((match typ with
              Struct s ->
                let stype = StringMap.find s struct_decls in
                (try
                  fst (List.find (fun b -> snd b = m) stype.members)
                with Not_found ->
                  raise (Failure ("struct " ^ s ^ " does not contain member " ^
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
          , SDeref(e', m))
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
      | Id _ | Deref(_, _) as e -> lvalue false env e
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
      | TypeConsOrCall(typ, actuals) as call ->
          match typ with
              (* struct constructors and functions are in the same namespace,
               * and we'll handle struct constructors as regular functions
               * anyways.
               *)
              Struct fname -> let fd = function_decl fname in
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
            | Vec(b, w) ->
                if List.length actuals != w then
                  raise (Failure ("expecting " ^ string_of_int w ^
                   " arguments in constructor for " ^ string_of_typ typ))
                else (typ, STypeCons(List.map (fun e ->
                  let se = expr env e in
                  let atyp = fst se in
                  if atyp <> Vec(b, 1) then
                    raise (Failure ("expecting type " ^
                      string_of_typ (Vec (b, 1)) ^
                      " in constructor for " ^ string_of_typ typ))
                  else
                    se) actuals))
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
            | Return e -> let se = expr env e in if fst se = func.typ then
                env, (SReturn(se) :: sstmts)
              else
                raise (Failure ("return gives " ^ string_of_typ (fst se) ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
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
                  (fun s n -> "struct " ^ s ^ " does not exist for local " ^ n ^
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
      sformals = func.formals;
      slocals = env.locals;
      sbody = List.rev sbody;
    }

   
  in
  (structs, globals, List.map check_function functions)
