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
module G = Glslcodegen

module StringMap = Map.Make(String)

(* helper function that returns the index of an element in a list
 * why isn't this a stdlib function?
 *)
let index_of e l =
  let rec index_of' i = function
      [] -> raise Not_found
    | hd :: tl -> if hd = e then i else index_of' (i + 1) tl
  in
index_of' 0 l

(* Why is this not a stdlib function? *)
let rec range i j = if i >= j then [] else i :: (range (i+1) j) 

let translate ((structs, pipelines, globals, functions) as program) =
  let shaders = G.translate program in

  (* ignore GPU functions for the rest of the codegen *)
  let functions =
    List.filter (fun f -> f.SA.sfqual = A.CpuOnly || f.SA.sfqual = A.Both)
    functions in

  let context = L.global_context () in
  let the_module = L.create_module context "MicroC"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i1_t   = L.i1_type   context
  and f32_t  = L.float_type context
  and f64_t  = L.double_type context
  and void_t = L.void_type context in
  let string_t = L.pointer_type i8_t in
  let voidp_t = L.pointer_type i8_t (* LLVM uses i8* instead of void* *) in

  let make_vec_t base =
    [| base; L.array_type base 2;
             L.array_type base 3;
             L.array_type base 4 |]
  in

  let make_n_t base n =
      [| L.array_type base n; 
         L.array_type (L.array_type base 2) n;
         L.array_type (L.array_type base 3) n;
         L.array_type (L.array_type base 4) n
      |]
  in

  let make_mat_t base = 
      [| make_vec_t base; make_n_t base 2;
         make_n_t base 3; make_n_t base 4 |]
  in

  let vec_t = make_vec_t f32_t in
  let ivec_t = make_vec_t i32_t in
  let bvec_t = make_vec_t i1_t in
  let byte_vec_t = make_vec_t i8_t in
  let mat_t = make_mat_t f32_t in

  let izero = L.const_int i32_t 0 in

  (* define base pipeline type that every pipeline derives from
   * this is struct pipeline in runtime.c *)
  let pipeline_t = L.struct_type context [|
    (* vertex_array *)
    i32_t;
    (* program *)
    i32_t
  |] in

  (* construct struct types *)
  let struct_decls = List.fold_left (fun m s ->
    StringMap.add s.A.sname s m) StringMap.empty structs
  in

  let pipeline_decls = List.fold_left (fun m p ->
    StringMap.add p.SA.spname p m) StringMap.empty pipelines
  in

  let struct_types = List.fold_left (fun m s ->
    StringMap.add s.A.sname (L.named_struct_type context s.A.sname) m)
    StringMap.empty structs in

  let rec ltype_of_typ = function
    | A.Mat(A.Int, 1, l) -> ivec_t.(l-1)
    | A.Mat(A.Bool, 1, l) -> bvec_t.(l-1)
    | A.Mat(A.Byte, 1, l) -> byte_vec_t.(l-1)
    | A.Mat(A.Float, w, l) -> mat_t.(w-1).(l-1) 
    | A.Mat(_, _, _) -> raise (Failure "unimplemented")
    | A.Struct s -> StringMap.find s struct_types
    | A.Array(t, Some s) -> L.array_type (ltype_of_typ t) s
    | A.Array(t, None)-> L.struct_type context [| i32_t; L.pointer_type (ltype_of_typ t) |]
    | A.Window -> voidp_t
    | A.Pipeline(_) -> pipeline_t
    | A.Buffer(_) -> i32_t
    | A.Void -> void_t in

  List.iter (fun s ->
    let llstruct = StringMap.find s.A.sname struct_types in
    L.struct_set_body llstruct
      (Array.of_list (List.map (fun m -> ltype_of_typ (fst m)) s.A.members)) false)
  structs;

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.undef (ltype_of_typ t)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let shader_globals =
    StringMap.mapi (fun name shader ->
      L.define_global name (L.const_stringz context shader) the_module)
    shaders
  in

  let blis_string_t = ltype_of_typ (A.Array(A.Mat(A.Byte, 1, 1), None)) in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| voidp_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare functions in the built-in library that call into GLFW and OpenGL *)
  let init_t = L.function_type void_t [| |] in
  let init_func = L.declare_function "init" init_t the_module in
  let create_window_t = L.function_type voidp_t [| i32_t; i32_t; i32_t |] in
  let create_window_func =
    L.declare_function "create_window" create_window_t the_module in
  let set_active_window_t = L.function_type void_t [| voidp_t |] in
  let set_active_window_func =
    L.declare_function "set_active_window" set_active_window_t the_module in
  let create_buffer_t = L.function_type i32_t [| |] in
  let create_buffer_func = L.declare_function "create_buffer" create_buffer_t
    the_module in
  let upload_buffer_t =
    L.function_type void_t [| i32_t; voidp_t; i32_t; i32_t |] in
  let upload_buffer_func =
    L.declare_function "upload_buffer" upload_buffer_t the_module in
  let create_pipeline_t =
    L.function_type void_t [| L.pointer_type pipeline_t; string_t; string_t |] in
  let create_pipeline_func =
    L.declare_function "create_pipeline" create_pipeline_t the_module in
  let pipeline_bind_vertex_buffer_t = L.function_type void_t [|
    L.pointer_type pipeline_t; i32_t; i32_t; i32_t |] in
  let pipeline_bind_vertex_buffer_func = 
    L.declare_function "pipeline_bind_vertex_buffer"
      pipeline_bind_vertex_buffer_t the_module in
  let pipeline_get_vertex_buffer_t = L.function_type i32_t [|
    L.pointer_type pipeline_t; i32_t |] in
  let pipeline_get_vertex_buffer_func =
    L.declare_function "pipeline_get_vertex_buffer"
      pipeline_get_vertex_buffer_t the_module in
  let pipeline_get_uniform_location_t =
    L.function_type i32_t [| L.pointer_type pipeline_t; string_t |] in
  let pipeline_get_uniform_location_func =
    L.declare_function "pipeline_get_uniform_location"
      pipeline_get_uniform_location_t the_module in
  let pipeline_set_uniform_float_t =
    L.function_type void_t
      [| L.pointer_type pipeline_t; i32_t; L.pointer_type f32_t; i32_t; i32_t |] in
  let pipeline_set_uniform_float_func =
    L.declare_function "pipeline_set_uniform_float"
      pipeline_set_uniform_float_t the_module in
  let pipeline_set_uniform_int_t =
    L.function_type void_t
      [| L.pointer_type pipeline_t; i32_t; L.pointer_type i32_t; i32_t; i32_t |] in
  let pipeline_set_uniform_int_func =
    L.declare_function "pipeline_set_uniform_int"
      pipeline_set_uniform_int_t the_module in
  let pipeline_get_uniform_float_t =
    L.function_type void_t
      [| L.pointer_type pipeline_t; i32_t; L.pointer_type f32_t |] in
  let pipeline_get_uniform_float_func =
    L.declare_function "pipeline_get_uniform_float"
      pipeline_get_uniform_float_t the_module in
  let pipeline_get_uniform_int_t =
    L.function_type void_t
      [| L.pointer_type pipeline_t; i32_t; L.pointer_type i32_t |] in
  let pipeline_get_uniform_int_func =
    L.declare_function "pipeline_get_uniform_int"
      pipeline_get_uniform_int_t the_module in
  let bind_pipeline_t = L.function_type void_t [| L.pointer_type pipeline_t |] in
  let bind_pipeline_func =
    L.declare_function "bind_pipeline" bind_pipeline_t the_module in
  let draw_arrays_t = L.function_type void_t [| i32_t |] in
  let draw_arrays_func =
    L.declare_function "draw_arrays" draw_arrays_t the_module in
  let swap_buffers_t = L.function_type void_t [| voidp_t |] in
  let swap_buffers_func =
    L.declare_function "glfwSwapBuffers" swap_buffers_t the_module in
  let poll_events_t = L.function_type void_t [| |] in
  let poll_events_func =
    L.declare_function "glfwPollEvents" poll_events_t the_module in
  let should_close_t = L.function_type i32_t [| voidp_t |] in
  let should_close_func =
    L.declare_function "glfwWindowShouldClose" should_close_t the_module in
  let read_pixel_t =
    L.function_type void_t [| i32_t; i32_t; L.pointer_type vec_t.(3) |] in
  let read_pixel_func =
    L.declare_function "read_pixel" read_pixel_t the_module in
  let read_file_t =
    L.function_type void_t [| L.pointer_type blis_string_t; blis_string_t |] in
  let read_file_func =
    L.declare_function "read_file" read_file_t the_module in
  let print_string_t =
    L.function_type void_t [| blis_string_t |] in
  let print_string_func =
    L.declare_function "print_string" print_string_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.SA.sfname
      and formal_types =
	Array.of_list (List.map (fun (q, (t,_)) ->
          let t' = ltype_of_typ t in
          if q = A.In then t' else L.pointer_type t') fdecl.SA.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.SA.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.SA.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in
    
    let add_formal m (q, (t, n)) p = L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      (match q with
          A.In -> ignore (L.build_store p local builder)
        | A.Inout -> ignore (L.build_store
            (L.build_load p "tmp" builder) local builder)
        | A.Out -> ()
        | A.Uniform -> raise (Failure "unreachable"));
      StringMap.add n local m in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.SA.sformals
        (Array.to_list (L.params the_function)) in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in
      List.fold_left add_local formals fdecl.SA.slocals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* evaluates an expression and returns a pointer to its value. If the
     * expression is an lvalue, guarantees that the pointer is to the memory
     * referenced by the lvalue.
     *)
    let rec lvalue builder sexpr = match snd sexpr with
        SA.SId s -> lookup s
      | SA.SStructDeref (e, m) ->
          let e' = lvalue builder e in
          (match fst e with
              A.Struct s ->
                let decl = StringMap.find s struct_decls in
                L.build_struct_gep e'
                  (index_of m (List.map snd decl.A.members))
                  "tmp" builder
            | A.Mat (_, _, _) ->
                L.build_gep e' [|izero; L.const_int i32_t (match m with
                    "x" -> 0
                  | "y" -> 1
                  | "z" -> 2
                  | "w" -> 3
                  | _ -> raise (Failure "shouldn't get here"))|]
                  "tmp" builder
            | _ -> raise (Failure "unexpected type"))
      | SA.SArrayDeref (e, i) ->
          let e' = lvalue builder e in
          let i' = expr builder i in
          (match (fst e) with
            A.Array(_, Some _) -> L.build_gep e' [| izero; i' |]
              "tmp" builder
          | A.Array(_, None) -> L.build_gep
            (L.build_extractvalue (L.build_load e' "" builder) 1 "" builder)
            [| i' |] "tmp" builder
          | _ -> raise (Failure "not supported"))
      | _ -> let e' = expr builder sexpr in
          let temp =
            L.build_alloca (ltype_of_typ (fst sexpr)) "expr_tmp" builder in
          ignore (L.build_store e' temp builder); temp

    and handle_assign builder l r =
      match l with
          (A.Buffer(A.Mat(A.Float, 1, comp)), SA.SStructDeref((A.Pipeline(p), _) as e, m)) ->
            let pdecl = StringMap.find p pipeline_decls in
            let location = index_of m (List.map snd pdecl.SA.sinputs) in
            let lval' = lvalue builder e in
            let e' = expr builder r in
            ignore (L.build_call pipeline_bind_vertex_buffer_func [|
              lval'; e'; L.const_int i32_t comp; L.const_int i32_t location |]
              "" builder)
        | (A.Mat(b, c, n), SA.SStructDeref((A.Pipeline(_), _) as e, m)) ->
            let lval' = lvalue builder e in
            let e' = lvalue builder r in
            let loc = L.build_call pipeline_get_uniform_location_func [|
              lval'; L.build_global_stringptr m "" builder |] "" builder in
            ignore (match b with
                A.Float -> L.build_call pipeline_set_uniform_float_func [|
                  lval'; loc; L.build_gep e' [| izero; izero |] "" builder;
                  L.const_int i32_t n; L.const_int i32_t c |] "" builder
              | A.Int -> L.build_call pipeline_set_uniform_int_func [|
                  lval'; loc; L.build_gep e' [| izero; izero |] "" builder;
                  L.const_int i32_t n; L.const_int i32_t c |] "" builder
              | A.Bool -> raise (Failure "unimplemented boolean uniforms")
              | _ -> raise (Failure "unimplemented"));
        | _ -> let lval' = lvalue builder l in
            let e' = expr builder r in
            ignore (L.build_store e' lval' builder)

    (* Construct code for an expression; return its value *)
    and expr builder sexpr = match snd sexpr with
	SA.SIntLit i -> L.const_int i32_t i
      | SA.SFloatLit f -> L.const_float f32_t f
      | SA.SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SA.SCharLit c -> L.const_int i8_t (Char.code c)
      | SA.SStringLit s -> L.const_string context s
      | SA.SNoexpr -> izero
      | SA.SStructDeref((A.Pipeline(p), _) as e, m) ->
          let pdecl = StringMap.find p pipeline_decls in
          let e' = lvalue builder e in
          (try
            let location = index_of m (List.map snd pdecl.SA.sinputs) in
            L.build_call pipeline_get_vertex_buffer_func [|
              e'; L.const_int i32_t location |] "" builder
          with Not_found ->
            let loc = L.build_call pipeline_get_uniform_location_func [|
              e'; L.build_global_stringptr m "" builder |] "" builder in
            let tmp = L.build_alloca (ltype_of_typ (fst sexpr)) "" builder in
            ignore (match fst sexpr with
                A.Mat(A.Float, _, _) ->
                  L.build_call pipeline_get_uniform_float_func [|
                    e'; loc;
                    L.build_gep tmp [| izero; izero |] "" builder |]
                  "" builder
              | A.Mat(A.Int, _, _) ->
                  L.build_call pipeline_get_uniform_int_func [|
                    e'; loc;
                    L.build_gep tmp [| izero; izero |] "" builder |]
                  "" builder
              | A.Mat(A.Bool, _, _) ->
                  raise (Failure "unimplemented boolean uniforms")
              | _ -> raise (Failure "unimplemented"));
            L.build_load tmp "" builder)
      | SA.SId _ | SA.SStructDeref (_, _) | SA.SArrayDeref (_, _) ->
          L.build_load (lvalue builder sexpr) "load_tmp" builder
      | SA.SBinop (e1, op, e2) ->
          let e1' = expr builder e1
	  and e2' = expr builder e2 in
          let e1cols, e1rows, e2cols, e2rows = match fst e1, fst e2 with
                | A.Mat(_, w, l), A.Mat(_, w', l') -> w, l, w', l'
                | _ -> raise (Failure "shouldn't get here");
          in
          let base_type, llvbase_type, cols, rows = match fst sexpr with
                | A.Mat(b, w, l) -> b, (ltype_of_typ (A.Mat(b, 1, 1))), w, l 
                | _ -> raise (Failure "shouldn't get here");
          in
          let twod_array cols rows llvbase_type = 
            L.undef (L.array_type (L.array_type llvbase_type rows) cols)
          in
          let twod_array_wrap e cols rows llvbase_type str builder = 
            let output = twod_array cols rows llvbase_type in
            let output = if cols = 1 then 
              L.build_insertvalue output e 0 str builder 
            else
              e
            in
            let output = if rows = 1 then 
              List.fold_left (fun acc col -> 
                let value = L.build_extractvalue e col str builder in
                let column = 
                  L.build_insertvalue (L.undef (L.array_type llvbase_type 1)) value 0 str builder
                in
                L.build_insertvalue acc column col str builder) 
                        (twod_array cols rows llvbase_type) (range 0 cols)
            else 
              output
            in
            if cols > 1 && rows > 1 then e
            else output
          in
          let twod_array_unwrap e cols rows llvbase_type str builder = 
            if cols > 1 && rows > 1 then e
            else
              if rows = 1 && cols = 1 then
                L.build_extractvalue (L.build_extractvalue e 0 str builder) 0 str builder
              else if cols = 1 then 
                L.build_extractvalue e 0 str builder 
              else 
                List.fold_left (fun acc col -> 
                  let value = 
                    L.build_extractvalue (L.build_extractvalue e 0 str builder) col str builder
                  in
                L.build_insertvalue acc value col str builder) (L.undef
                (L.array_type llvbase_type cols)) (range 0 cols)
          in
          let per_component_builder_vec op vec1 vec2 str builder = 
            List.fold_left (fun acc row -> 
              let val1 = L.build_extractvalue vec1 row str builder in
              let val2 = L.build_extractvalue vec2 row str builder in
              L.build_insertvalue acc (op val1 val2 str builder) row str builder)
              (L.undef (ltype_of_typ (A.Mat(base_type, 1, rows)))) (range 0 rows)
          in
          let per_component_builder_mat op mat1 mat2 str builder = 
            List.fold_left (fun acc col -> 
              let vec1' = L.build_extractvalue mat1 col str builder in
              let vec2' = L.build_extractvalue mat2 col str builder in
              L.build_insertvalue acc (per_component_builder_vec op vec1'
                vec2' str builder) col str builder) 
              (L.undef (ltype_of_typ (fst sexpr))) (range 0 cols)
          in
          let per_component_builder op e1'' e2'' str builder = 
            if rows = 1 && cols = 1 then op e1'' e2'' str builder
            else if rows = 1 || cols = 1 then per_component_builder_vec op e1''
                e2'' str builder
            else per_component_builder_mat op e1'' e2'' str builder
          in
          let dot_prod vec1 vec2 str builder = 
              let val1 = L.build_extractvalue vec1 0 str builder in
              let val2 = L.build_extractvalue vec2 0 str builder in
              let val3 = L.build_fmul val1 val2 str builder in
            List.fold_left (fun sum index -> 
              let val1' = L.build_extractvalue vec1 index str builder in
              let val2' = L.build_extractvalue vec2 index str builder in
              let val3' = L.build_fmul val1' val2' str builder in
              L.build_fadd val3' sum str builder) val3 (range 1 e1cols)
          in
          let mat_row_extract mat row str builder = 
            List.fold_left (fun acc index -> 
              let colm = L.build_extractvalue mat index str builder in 
              let val1 = L.build_extractvalue colm row str builder in 
              L.build_insertvalue acc val1 index "matrow" builder) 
            (L.undef (ltype_of_typ (A.Mat(base_type, e1cols, 1)))) (range 0 e1cols)
          in
          let mat_mult_col mat colvec str builder = 
             List.fold_left (fun acc row -> 
               let rowvec = mat_row_extract mat row str builder in
               let value = dot_prod rowvec colvec str builder in
               L.build_insertvalue acc value row "matcol" builder) 
             (L.undef (L.array_type llvbase_type rows)) (range 0 rows)
          in
          let mat_mat_mult mat1 mat2 str builder =
             List.fold_left (fun acc col ->
               let x = L.build_extractvalue mat2 col str builder in
               let b = mat_mult_col mat1 x str builder in
              L.build_insertvalue acc b col "matmat" builder) 
             (twod_array cols rows llvbase_type) (range 0 cols)
          in
          let fmat_mult mat1 mat2 str builder =
              let mat1 = 
                twod_array_wrap mat1 e1cols e1rows llvbase_type str builder
              in
              let mat2 = 
                twod_array_wrap mat2 e2cols e2rows llvbase_type str builder
              in
              let output = mat_mat_mult mat1 mat2 str builder in
              twod_array_unwrap output cols rows llvbase_type str builder 
          in
	  (match op with
	    SA.IAdd     -> per_component_builder L.build_add
	  | SA.ISub     -> per_component_builder L.build_sub
	  | SA.IMult    -> per_component_builder L.build_mul
          | SA.IDiv     -> per_component_builder L.build_sdiv
	  | SA.IEqual   -> L.build_icmp L.Icmp.Eq
	  | SA.INeq     -> L.build_icmp L.Icmp.Ne
	  | SA.ILess    -> L.build_icmp L.Icmp.Slt
	  | SA.ILeq     -> L.build_icmp L.Icmp.Sle
	  | SA.IGreater -> L.build_icmp L.Icmp.Sgt
	  | SA.IGeq     -> L.build_icmp L.Icmp.Sge
          | SA.FAdd     -> per_component_builder L.build_fadd
          | SA.FSub     -> per_component_builder L.build_fsub
          | SA.FMult    -> per_component_builder L.build_fmul
          | SA.FDiv     -> per_component_builder L.build_fdiv
          | SA.FMatMult -> fmat_mult 
	  | SA.FEqual   -> L.build_fcmp L.Fcmp.Oeq
	  | SA.FNeq     -> L.build_fcmp L.Fcmp.One
	  | SA.FLess    -> L.build_fcmp L.Fcmp.Olt
	  | SA.FLeq     -> L.build_fcmp L.Fcmp.Ole
	  | SA.FGreater -> L.build_fcmp L.Fcmp.Ogt
	  | SA.FGeq     -> L.build_fcmp L.Fcmp.Oge
          | SA.U8Equal  -> L.build_icmp L.Icmp.Eq
          | SA.U8Neq    -> L.build_icmp L.Icmp.Ne
	  | SA.BAnd     -> L.build_and
	  | SA.BOr      -> L.build_or
	  | SA.BEqual   -> L.build_icmp L.Icmp.Eq
	  | SA.BNeq     -> L.build_icmp L.Icmp.Ne
	  ) e1' e2' "tmp" builder
      | SA.SUnop(op, e) ->
	  let e' = expr builder e in
          let base_type, cols, rows = match fst sexpr with
                | A.Mat(b, w, l) -> b, w, l 
                | _ -> raise (Failure "shouldn't get here");
          in
          let per_component_builder_vec op vec str builder = 
            List.fold_left (fun acc row -> 
              let val1 = L.build_extractvalue vec row str builder in
              L.build_insertvalue acc (op val1 str builder) row str builder)
              (L.undef (ltype_of_typ (A.Mat(base_type, 1, rows)))) (range 0 rows)
          in
          let per_component_builder_mat op mat str builder = 
            List.fold_left (fun acc col -> 
              let vec' = L.build_extractvalue mat col str builder in
              L.build_insertvalue acc (per_component_builder_vec op vec' str builder) col str builder) 
              (L.undef (ltype_of_typ (fst sexpr))) (range 0 cols)
          in
          let per_component_builder op e'' str builder = 
            if rows = 1 && cols = 1 then op e'' str builder
            else if rows = 1 || cols = 1 then per_component_builder_vec op e'' str builder
            else per_component_builder_mat op e'' str builder
          in
	  (match op with
	    SA.INeg       -> per_component_builder L.build_neg e'
	  | SA.FNeg       -> per_component_builder L.build_fneg e'
          | SA.BNot       -> per_component_builder L.build_not e'
          | SA.Int2Float  -> L.build_sitofp e' f32_t
          | SA.Float2Int  -> L.build_fptosi e' i32_t
          | SA.Bool2Int   -> L.build_zext e' i32_t
          | SA.Bool2Float -> L.build_uitofp e' f32_t) "tmp" builder
      | SA.STypeCons act ->
          match fst sexpr with
              A.Mat(_, _, _) | A.Array(_, Some _) ->
                fst (List.fold_left (fun (agg, idx) e ->
                  let e' = expr builder e in
                  (L.build_insertvalue agg e' idx "tmp" builder, idx + 1))
              ((L.undef (ltype_of_typ (fst sexpr))), 0) act)
            | A.Array(t, None) -> let s = expr builder (List.hd act) in
              let a = L.undef (ltype_of_typ (fst sexpr)) in
              let a = L.build_insertvalue a s 0 "" builder in
              L.build_insertvalue a (L.build_array_malloc
                (ltype_of_typ t) s "" builder) 1 "" builder 
            | A.Buffer(_) -> L.build_call create_buffer_func [| |] "" builder
            | A.Pipeline(p) ->
                let pdecl = StringMap.find p pipeline_decls in
                let fshader = StringMap.find pdecl.SA.sfshader shader_globals in
                let vshader = StringMap.find pdecl.SA.svshader shader_globals in
                let tmp = L.build_alloca pipeline_t "pipeline_tmp" builder in
                let v = L.build_gep vshader [| izero; izero |] "" builder in
                let f = L.build_gep fshader [| izero; izero |] "" builder in
                ignore
                  (L.build_call create_pipeline_func [| tmp; v; f |] "" builder);
                L.build_load tmp "" builder
            | A.Window ->
                (match act with
                    [w; h; offscreen] ->
                      let w' = expr builder w in
                      let h' = expr builder h in
                      let offscreen' =
                        L.build_zext (expr builder offscreen) i32_t "" builder
                      in
                      L.build_call create_window_func
                        [| w'; h'; offscreen' |] "" builder
                  | _ -> raise (Failure "shouldn't get here"))
            | _ -> raise (Failure "shouldn't get here")

    in

    let copy_out_params builder =
      List.iter2 (fun p (q, (_, n)) ->
        if q <> A.In then
          let tmp = L.build_load (StringMap.find n formals) "" builder in
          ignore (L.build_store tmp p builder))
      (Array.to_list (L.params the_function)) fdecl.SA.sformals
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
        SA.SAssign (lval, e) -> handle_assign builder lval e; builder
      | SA.SCall (_, "print", [e]) ->
          let e' = expr builder e in
          ignore (L.build_call print_string_func [| e' |] "" builder);
          builder
      | SA.SCall (_, "printi", [e]) | SA.SCall (_, "printb", [e]) ->
	  ignore
            (L.build_call printf_func [| int_format_str ; (expr builder e) |]
            "printf" builder);
          builder
      | SA.SCall (_, "printf", [e]) ->
	  ignore
            (L.build_call printf_func
              [| float_format_str ;
                 L.build_fpext (expr builder e) f64_t "tmp" builder |]
              "printf" builder);
          builder
      | SA.SCall (_, "printc", [e]) ->
          ignore
            (L.build_call printf_func [| char_format_str; (expr builder e) |]
              "printf" builder);
          builder
      | SA.SCall (_, "set_active_window", [w]) ->
          ignore (L.build_call set_active_window_func [| expr builder w |] ""
            builder);
          builder
      | SA.SCall (_, "upload_buffer", [buf; data]) ->
          let buf' = expr builder buf in
          let data', size = (match (fst data) with
            A.Array(A.Mat(A.Float, 1, s), Some n) ->
              (lvalue builder data, L.const_int i32_t (4 * s * n))
          | A.Array(A.Mat(A.Float, 1, n), None) -> let s = expr builder data in
              (L.build_extractvalue s 1 "" builder,
              L.build_mul (L.const_int i32_t (4 * n))
              (L.build_extractvalue s 0 "" builder) "" builder)
          | _ -> raise (Failure "not supported")) in
          let data' = L.build_bitcast data' voidp_t "" builder in
          ignore (L.build_call upload_buffer_func
            [| buf'; data'; size;
               L.const_int i32_t 0x88E4 (* GL_STATIC_DRAW *) |] "" builder);
          builder
      | SA.SCall (_, "bind_pipeline", [p]) ->
          let p' = lvalue builder p in
          ignore (L.build_call bind_pipeline_func [| p' |] "" builder);
          builder
      | SA.SCall (_, "draw_arrays", [i]) ->
          let i' = expr builder i in
          ignore (L.build_call draw_arrays_func [| i' |] "" builder);
          builder
      | SA.SCall (_, "swap_buffers", [w]) ->
          let w' = expr builder w in
          ignore (L.build_call swap_buffers_func [| w' |] "" builder);
          builder
      | SA.SCall (_, "poll_events", []) ->
          ignore (L.build_call poll_events_func [| |] "" builder);
          builder
      | SA.SCall (ret, "window_should_close", [w]) ->
          let w' = expr builder w in
          let ret = lvalue builder ret in
          let llret = L.build_icmp L.Icmp.Ne
            (L.build_call should_close_func [| w' |]  "" builder)
            izero "" builder in
          ignore (L.build_store llret ret builder); builder
      | SA.SCall (ret, "read_pixel", [x; y]) ->
          let x' = expr builder x in
          let y' = expr builder y in
          let ret = lvalue builder ret in
          ignore (L.build_call read_pixel_func [| x'; y'; ret |] "" builder);
          builder
      | SA.SCall (ret, "length", [arr]) ->
          let arr' = expr builder arr in
          let ret = lvalue builder ret in
          let len = (match fst arr with
              A.Array(_, Some len) -> L.const_int i32_t len
            | A.Array(_, None) -> L.build_extractvalue arr' 0 "" builder
            | _ -> raise (Failure "unexpected type")) in
          ignore (L.build_store len ret builder); builder
      | SA.SCall (ret, "read_file", [path]) ->
          let path = expr builder path in
          let ret = lvalue builder ret in
          ignore (L.build_call read_file_func [| ret; path |] "" builder);
          builder
      | SA.SCall (ret, f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let actuals = (List.map2 (fun (q, (_, _)) e ->
           if q = A.In then expr builder e
           else lvalue builder e) fdecl.SA.sformals act) in
	 let result = (match fdecl.SA.styp with A.Void -> ""
                                            | _ -> f ^ "_result") in

         let llret = L.build_call fdef (Array.of_list actuals) result builder in
         (match ret with
            (A.Void, SA.SNoexpr) -> ()
          | _ -> let ret = lvalue builder ret in
              ignore (L.build_store llret ret builder)
          ); builder
      | SA.SReturn e -> copy_out_params builder;
          ignore (match fdecl.SA.styp with
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

      | SA.SLoop (body, continue) -> 
          let body_bb = L.append_block context "loop_body" the_function in
          let continue_bb = L.append_block context "loop_continue" the_function in
          let merge_bb = L.append_block context "loop_merge" the_function in

          ignore (L.build_br body_bb builder);

          let body_builder = L.builder_at_end context body_bb in
          stmts merge_bb continue_bb body_builder body
            (L.build_br continue_bb);

          let continue_builder = L.builder_at_end context continue_bb in
          stmts merge_bb continue_bb continue_builder continue
            (L.build_br body_bb);

          L.builder_at_end context merge_bb


    in

    if fdecl.SA.sfname = "main" then
      ignore (L.build_call init_func [| |] "" builder)
    else
      ()
    ;

    (* Build the code for each statement in the function *)
    let dummy_bb = L.append_block context "dummy" the_function in
    ignore (L.build_unreachable (L.builder_at_end context dummy_bb));
    stmts dummy_bb dummy_bb builder fdecl.SA.sbody
      (* Add a return if the last block falls off the end. Semantic checking
       * ensures that only functions that return void hit this path. *)
      (fun builder -> copy_out_params builder; L.build_ret_void builder)

  in

  List.iter build_function_body functions;
  the_module
