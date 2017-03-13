(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns the SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  [
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] };
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] };
     { typ = Void; fname = "printf"; formals = [(Float, "x")];
       locals = []; body = [] }]
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         StringMap.empty (built_in_decls @ functions)
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression and new expression or throw an exception *)
    let rec expr = function
	IntLit(l) -> (Int, SIntLit(l))
      | FloatLit(l) -> (Float, SFloatLit(l))
      | BoolLit(l) -> (Bool, SBoolLit(l))
      | Id s -> (type_of_identifier s, SId(s))
      | Binop(e1, op, e2) as e -> let t1, e1 = expr e1 and t2, e2 = expr e2 in
        let typ, op = (match op with
            Add when t1 = Int && t2 = Int -> (Int, IAdd)
          | Sub when t1 = Int && t2 = Int -> (Int, ISub)
          | Mult when t1 = Int && t2 = Int -> (Int, IMult)
          | Div when t1 = Int && t2 = Int -> (Int, IDiv)
          | Equal when t1 = Int && t2 = Int -> (Bool, IEqual)
          | Neq when t1 = Int && t2 = Int -> (Bool, INeq)
          | Add when t1 = Float && t2 = Float -> (Float, FAdd)
          | Sub when t1 = Float && t2 = Float -> (Float, FSub)
          | Mult when t1 = Float && t2 = Float -> (Float, FMult)
          | Div when t1 = Float && t2 = Float -> (Float, FDiv)
          | Equal when t1 = Float && t2 = Float -> (Bool, FEqual)
          | Neq when t1 = Float && t2 = Float -> (Bool, FNeq)
          | Less when t1 = Float && t2 = Float -> (Bool, FLess)
          | Leq when t1 = Float && t2 = Float -> (Bool, FLeq)
          | Greater when t1 = Float && t2 = Float -> (Bool, FGreater)
          | Geq when t1 = Float && t2 = Float -> (Bool, FGeq)
          | Equal when t1 = Bool && t2 = Bool -> (Bool, BEqual)
          | Neq when t1 = Bool && t2 = Bool -> (Bool, BNeq)
          | Less when t1 = Int && t2 = Int -> (Bool, ILess)
          | Leq when t1 = Int && t2 = Int -> (Bool, ILeq)
          | Greater when t1 = Int && t2 = Int -> (Bool, IGreater)
          | Geq when t1 = Int && t2 = Int -> (Bool, IGeq)
          | And when t1 = Bool && t2 = Bool -> (Bool, BAnd)
          | Or when t1 = Bool && t2 = Bool -> (Bool, BOr)
          | _ -> raise (Failure ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e))
          )
        in (typ, SBinop(e1, op, e2))
      | Unop(op, e) as ex -> let t, e = expr e in
	 (match op with
	   Neg when t = Int -> (Int, SUnop(INeg, e))
	 | Neg when t = Float -> (Int, SUnop(FNeg, e))
	 | Not when t = Bool -> (Bool, SUnop(BNot, e))
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> (Void, SNoexpr)
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt, e = expr e in
        (check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				      " = " ^ string_of_typ rt ^ " in " ^ 
				      string_of_expr ex)), SAssign(var, e))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           (fd.typ, SCall(fname,
              List.map2 (fun (ft, _) e -> let et, se = expr e in
                ignore (check_assign ft et
                  (Failure ("illegal actual argument found " ^ string_of_typ et ^
                  " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e)));
                se)
              fd.formals actuals))
    in

    let check_bool_expr e =
      let t, se = expr e in 
        if t != Bool then
          raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
        else se in

    let check_in_loop in_loop = if in_loop then () else
      raise (Failure ("break/continue must be inside a loop")) in

    (* Verify a list of statements or throw an exception *)
    let rec stmts in_loop sl = 
      let rec stmts' sstmts sl = List.fold_left
        (fun sstmts stmt ->
          match sstmts with
              SBreak :: _ -> raise (Failure "nothing may follow a break")
            | SContinue :: _ -> raise (Failure "nothing may follow a continue")
            | SReturn _ :: _ -> raise (Failure "nothing may follow a return")
            | _ -> match stmt with
                Break -> check_in_loop in_loop; SBreak :: sstmts
              | Continue -> check_in_loop in_loop; SContinue :: sstmts
              | Return e -> let t, se = expr e in if t = func.typ then
                  SReturn(se) :: sstmts
                else
                  raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                           string_of_typ func.typ ^ " in " ^ string_of_expr e))
              | Block sl -> stmts' sstmts sl
              | If(p, b1, b2) -> SIf(check_bool_expr p,
                                     stmts in_loop [b1],
                                     stmts in_loop [b2]) :: sstmts
              | For(e1, e2, e3, st) -> SFor(snd (expr e1),
                                            check_bool_expr e2,
                                            snd (expr e3),
                                            stmts true [st]) :: sstmts
              | While(p, s) -> SWhile(check_bool_expr p, stmts true [s]) :: sstmts
              | Expr e -> SExpr(snd (expr e)) :: sstmts)
        sstmts sl
      in
      List.rev (stmts' [] sl)

    in

    {
      styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals = func.locals;
      sbody = stmts false func.body;
    }

   
  in
  (globals, List.map check_function functions)
