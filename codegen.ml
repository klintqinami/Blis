(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
module SA = Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and f32_t  = L.float_type context
  and f64_t  = L.double_type context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> f32_t
    | A.Bool -> i1_t
    | A.Void -> void_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.SA.sfname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.SA.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.SA.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.SA.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.SA.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.SA.slocals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
	SA.SIntLit i -> L.const_int i32_t i
      | SA.SFloatLit f -> L.const_float f32_t f
      | SA.SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SA.SNoexpr -> L.const_int i32_t 0
      | SA.SId s -> L.build_load (lookup s) s builder
      | SA.SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    SA.IAdd     -> L.build_add
	  | SA.ISub     -> L.build_sub
	  | SA.IMult    -> L.build_mul
          | SA.IDiv     -> L.build_sdiv
	  | SA.IEqual   -> L.build_icmp L.Icmp.Eq
	  | SA.INeq     -> L.build_icmp L.Icmp.Ne
	  | SA.ILess    -> L.build_icmp L.Icmp.Slt
	  | SA.ILeq     -> L.build_icmp L.Icmp.Sle
	  | SA.IGreater -> L.build_icmp L.Icmp.Sgt
	  | SA.IGeq     -> L.build_icmp L.Icmp.Sge
          | SA.FAdd     -> L.build_fadd
          | SA.FSub     -> L.build_fsub
          | SA.FMult    -> L.build_fmul
          | SA.FDiv     -> L.build_fdiv
	  | SA.FEqual   -> L.build_fcmp L.Fcmp.Oeq
	  | SA.FNeq     -> L.build_fcmp L.Fcmp.One
	  | SA.FLess    -> L.build_fcmp L.Fcmp.Olt
	  | SA.FLeq     -> L.build_fcmp L.Fcmp.Ole
	  | SA.FGreater -> L.build_fcmp L.Fcmp.Ogt
	  | SA.FGeq     -> L.build_fcmp L.Fcmp.Oge
	  | SA.BAnd     -> L.build_and
	  | SA.BOr      -> L.build_or
	  | SA.BEqual   -> L.build_icmp L.Icmp.Eq
	  | SA.BNeq     -> L.build_icmp L.Icmp.Ne
	  ) e1' e2' "tmp" builder
      | SA.SUnop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	    SA.INeg     -> L.build_neg
	  | SA.FNeg     -> L.build_fneg
          | SA.BNot     -> L.build_not) e' "tmp" builder
      | SA.SAssign (s, e) -> let e' = expr builder e in
	                   ignore (L.build_store e' (lookup s) builder); e'
      | SA.SCall ("print", [e]) | SA.SCall ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | SA.SCall ("printf", [e]) ->
	  L.build_call printf_func
            [| float_format_str ;
               L.build_fpext (expr builder e) f64_t "tmp" builder |]
	    "printf" builder
      | SA.SCall (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.SA.styp with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Build a list of statments, and invoke "f builder" if the list doesn't
     * end with a branch instruction (break, continue, return) *)
    let rec stmts break_bb continue_bb builder sl f =
      let builder = List.fold_left (stmt break_bb continue_bb) builder sl in
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder)
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    and stmt break_bb continue_bb builder = function
      | SA.SExpr e -> ignore (expr builder e); builder
      | SA.SReturn e -> ignore (match fdecl.SA.styp with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | SA.SBreak -> ignore (L.build_br break_bb builder); builder
      | SA.SContinue -> ignore (L.build_br continue_bb builder); builder
      | SA.SIf (predicate, then_stmts, else_stmts) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 stmts break_bb continue_bb (L.builder_at_end context then_bb) then_stmts
	  (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 stmts break_bb continue_bb (L.builder_at_end context else_bb) else_stmts
	  (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SA.SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  let body_bb = L.append_block context "while_body" the_function in
	  let merge_bb = L.append_block context "merge" the_function in

	  ignore (L.build_br pred_bb builder);

	  stmts merge_bb pred_bb (L.builder_at_end context body_bb) body
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);

	  L.builder_at_end context merge_bb

      | SA.SFor (e1, e2, e3, body) -> 
          ignore (expr builder e1);

          let pred_bb = L.append_block context "for" the_function in
          let body_bb = L.append_block context "for_body" the_function in
          let final_bb = L.append_block context "for_end" the_function in
          let merge_bb = L.append_block context "for_merge" the_function in

          ignore (L.build_br pred_bb builder);

          stmts merge_bb final_bb (L.builder_at_end context body_bb) body
            (L.build_br final_bb);

          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder e2 in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);

          let final_builder = L.builder_at_end context final_bb in
          ignore (expr final_builder e3);
          ignore (L.build_br pred_bb final_builder);

          L.builder_at_end context merge_bb


    in

    (* Build the code for each statement in the function *)
    let dummy_bb = L.append_block context "dummy" the_function in
    ignore (L.build_unreachable (L.builder_at_end context dummy_bb));
    stmts dummy_bb dummy_bb builder fdecl.SA.sbody
      (* Add a return if the last block falls off the end *)
      (match fdecl.SA.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in

  List.iter build_function_body functions;
  the_module
