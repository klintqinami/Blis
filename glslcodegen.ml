(* Code generation for shaders. Here we take each entrypoint and turn it into a
 * GLSL shader.
 *)

module A = Ast
module SA = Sast

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type translation_environment = {
  scope : string StringMap.t;
  used_var_names : StringSet.t;
  cur_qualifier : A.func_qualifier;
}

(* return a fresh name given a set of already-used names. The original name is
 * usually a prefix of the new name, although this may not be true if the
 * original name is reserved by OpenGL.
 *
 * TODO handle GLSL keywords
 *)
let get_name orig used =
  (* copied from the GLSL 3.30 spec. The weird line wrapping is identical to the
   * PDF to ease comparisons.
   *)
  let glsl_keywords = [
    "attribute"; "const"; "uniform"; "varying";
    "layout";
    "centroid"; "flat"; "smooth"; "noperspective";
    "break"; "continue"; "do"; "for"; "while"; "switch"; "case"; "default";
    "if"; "else";
    "in"; "out"; "inout";
    "float"; "int"; "void"; "bool"; "true"; "false";
    "invariant";
    "discard"; "return";
    "mat2"; "mat3"; "mat4";
    "mat2x2"; "mat2x3"; "mat2x4";
    "mat3x2"; "mat3x3"; "mat3x4";
    "mat4x2"; "mat4x3"; "mat4x4";
    "vec2"; "vec3"; "vec4"; "ivec2"; "ivec3"; "ivec4"; "bvec2"; "bvec3"; "bvec4";
    "uint"; "uvec2"; "uvec3"; "uvec4";
    "lowp"; "mediump"; "highp"; "precision";
    "sampler1D"; "sampler2D"; "sampler3D"; "samplerCube";
    "sampler1DShadow"; "sampler2DShadow"; "samplerCubeShadow";
    "sampler1DArray"; "sampler2DArray";
    "sampler1DArrayShadow"; "sampler2DArrayShadow";
    "isampler1D"; "isampler2D"; "isampler3D"; "isamplerCube";
    "isampler1DArray"; "isampler2DArray";
    "usampler1D"; "usampler2D"; "usampler3D"; "usamplerCube";
    "usampler1DArray"; "usampler2DArray";
    "sampler2DRect"; "sampler2DRectShadow"; "isampler2DRect"; "usampler2DRect";
    "samplerBuffer"; "isamplerBuffer"; "usamplerBuffer";
    "sampler2DMS"; "isampler2DMS"; "usampler2DMS";
    "sampler2DMSArray"; "isampler2DMSArray"; "usampler2DMSArray";
    "struct"]
  in

  (* names starting with "gl_" are reserved in GLSL *)
  let orig' = if String.length orig > 3 && String.sub orig 0 3 = "gl_" then
    String.sub orig 3 (String.length orig - 3)
  else
    orig
  in
  (* avoid using GLSL keywords *)
  let orig' = if List.mem orig' glsl_keywords then
    orig' ^ "_"
  else
    orig
  in
  (* if we wind up with an empty name, either because the caller passed in one
   * or there was a name called "gl_" in Blis, then we can't return it, so just
   * change it to "_"
   *)
  let orig' = if orig' = "" then "_" else orig' in
  if not (StringSet.mem orig' used) then
    orig'
  else
    (* keep appending digits until we get a new name *)
    let rec get_name' orig n =
      let orig' = orig ^ string_of_int n in
        if not (StringSet.mem orig' used) then
          orig'
        else
          get_name' orig (n + 1)
    in
    get_name' orig' 0

let add_symbol_table env name =
  let new_name = get_name name env.used_var_names in
  ({ env with scope = StringMap.add name new_name env.scope;
     used_var_names = StringSet.add new_name env.used_var_names; },
   new_name)

let translate ((structs, _, _, functions) : SA.sprogram) =
  let env = {
    scope = StringMap.empty;
    used_var_names = StringSet.empty;
    cur_qualifier = A.Both;
  }
  in

  let used, struct_names =
    List.fold_left (fun (used, names) sdecl ->
      let new_name = get_name sdecl.A.sname used in
      StringSet.add new_name used, StringMap.add sdecl.A.sname new_name names)
    (StringSet.singleton "dummy_struct", StringMap.empty) structs 
  in

  let _, func_names =
    List.fold_left (fun (used, names) fdecl ->
      let new_name = get_name fdecl.SA.sfname used in
      StringSet.add new_name used, StringMap.add fdecl.SA.sfname new_name names)
    (used, StringMap.empty) functions
  in

  (* returns the GLSL type for the Blis type stripped of array-ness
   * for example, returns "vec4" for vec4[10][2]
   *)
  let rec string_of_base_typ = function
      A.Vec(A.Int, 1) -> "int"
    | A.Vec(A.Float, 1) -> "float"
    | A.Vec(A.Bool, 1) -> "bool"
    | A.Vec(A.Int, n) -> "ivec" ^ string_of_int n
    | A.Vec(A.Float, n) -> "vec" ^ string_of_int n
    | A.Vec(A.Bool, n) -> "bvec" ^ string_of_int n
    | A.Struct(name) -> StringMap.find name struct_names
    | A.Buffer(_) -> "dummy_struct"
    | A.Window -> "dummy_struct"
    | A.Pipeline(_) -> "dummy_struct"
    | A.Void -> "void"
    | A.Array(typ, _) -> string_of_base_typ typ
  in


  (* returns the arrays required for a given Blis type
   * for example, returns "[10][2]" for vec4[10][2]
   *)
  let rec string_of_array = function
      A.Array(typ, n) -> "[" ^ string_of_int n ^ "]" ^ string_of_array typ
    | _ -> ""
  in

  let string_of_typ typ = string_of_base_typ typ ^ string_of_array typ
  in

  let string_of_bind env (typ, name) =
    let env, new_name = add_symbol_table env name in
    (env, string_of_base_typ typ ^ " " ^ new_name ^ string_of_array typ)
  in

  let make_tmp env typ =
    let env, tmp = add_symbol_table env "" in
    (env, string_of_base_typ typ ^ " " ^ tmp ^ string_of_array typ, tmp)
  in

  (* TODO need to remap struct member names, reorder struct decls *)
  (*let glsl_structs = String.concat "\n\n"
    (List.map (fun sdecl ->
    "struct " ^ StringMap.find sdecl.A.sname struct_names ^ " {\n" ^
    (String.concat "\n" (List.map (fun mem ->
      string_of_bind mem ^ ";") sdecl.A.members)) ^
    "\n};") structs) ^ "\n\n"
  in*)

  let rec translate_stmts env slist =
    let rec expr env stmts (typ, e) = match e with
        SA.SIntLit(i) -> (env, stmts, string_of_int i)
      | SA.SFloatLit(f) -> (env, stmts, string_of_float f)
      | SA.SBoolLit(b) -> (env, stmts, if b then "true" else "false")
      | SA.SId(n) -> (env, stmts, StringMap.find n env.scope)
      | SA.SStructDeref(e, mem) -> let env, stmts, e' = expr env stmts e in
          (match typ with
              A.Vec(_, _) -> (env, stmts, "(" ^ e' ^ ")." ^ mem)
            | _ -> raise (Failure "unimplemented"))
      | SA.SArrayDeref(e, idx) ->
          let env, stmts, e' = expr env stmts e in
          let env, stmts, idx' = expr env stmts idx in
          (env, stmts, "(" ^ e' ^ ")[" ^ idx' ^ "]")
      | SA.SBinop(e1, op, e2) ->
          let env, stmts, e1' = expr env stmts e1 in
          let env, stmts, e2' = expr env stmts e2 in
          (env, stmts, "(" ^ e1' ^ ") " ^ (match op with
              SA.IAdd | SA.FAdd -> "+"
            | SA.ISub | SA.FSub -> "-"
            | SA.IMult | SA.FMult -> "*"
            | SA.IDiv | SA.FDiv -> "/"
            | SA.IEqual | SA.FEqual | SA.BEqual -> "=="
            | SA.INeq | SA.FNeq | SA.BNeq -> "!="
            | SA.ILess | SA.FLess -> "<"
            | SA.IGreater | SA.FGreater -> ">"
            | SA.IGeq | SA.FGeq -> ">="
            | SA.ILeq | SA.FLeq -> "<="
            | SA.BAnd -> "&&"
            | SA.BOr -> "||") ^ " (" ^ e2' ^ ")")
      | SA.SUnop(op, e) -> let env, stmts, e' = expr env stmts e in
          (env, stmts, (match op with
              SA.INeg | SA.FNeg -> "-"
            | SA.BNot -> "!") ^ "(" ^ e' ^ ")")
      | SA.SAssign(l, r) ->
          let env, stmts, l' = expr env stmts l in
          let env, stmts, r' = expr env stmts r in
          (env, stmts ^ "(" ^ l' ^ ") = (" ^ r' ^ ");\n", l')
      | SA.STypeCons(elist) -> (match typ with
          A.Vec(_, _) | A.Array(_, _) ->
            let env, stmts, elist' = expr_list env stmts elist
            in
            (env, stmts, string_of_typ typ ^ "(" ^ elist' ^ ")")
        | _ -> raise (Failure ("unexpected type constructor for " ^
                string_of_typ typ)))
      | SA.SCall(name, elist) ->
          let env, stmts, elist' = expr_list env stmts elist
          in
          (* since calls can have side-effects, make a separate statement *)
          let env, bind, tmp = make_tmp env typ
          in
          (env, stmts ^
           bind ^ " = " ^ StringMap.find name func_names ^ "(" ^ elist' ^ ");\n",
           tmp)
      | SA.SNoexpr -> (env, stmts, "")

    and expr_list env stmts elist =
      (* handle lists for e.g. function arguments *)
      List.fold_left (fun (env, stmts, elist) e ->
        let env, stmts, e' = expr env stmts e in
        (env, stmts, if elist = "" then e' else elist ^ ", " ^ e'))
      (env, stmts, "") elist
    in
        
    let stmt (env, stmts) = function
        SA.SExpr(e) -> let env, stmts, _ = expr env stmts e in
          (env, stmts)
      | SA.SReturn(e) -> let env, stmts, e' = expr env stmts e in
          if env.cur_qualifier = A.Vertex then
            (env, stmts ^ "gl_Position = " ^ e' ^ ";\nreturn;\n")
          else
            (env, stmts ^ "return " ^ e' ^ ";\n")
      | SA.SIf(predicate, then_stmts, else_stmts) ->
          let env, stmts, pred = expr env stmts predicate in
          let then_string = translate_stmts env then_stmts in
          let else_string = translate_stmts env else_stmts in
          (env, stmts ^
           "if (" ^ pred ^ ") {\n" ^
              then_string ^
           "} else {\n" ^
              else_string ^
           "}\n")
      | SA.SFor(e1, e2, e3, body) ->
          let env, stmts, e1' = expr env stmts e1 in
          let env, stmts, e2' = expr env stmts e2 in
          let env, stmts, e3' = expr env stmts e3 in
          let body_string = translate_stmts env body in
          (env, stmts ^
           "for (" ^ e1' ^ "; " ^ e2' ^ "; " ^ e3' ^ ") {\n" ^
              body_string ^
           "}\n")
      | SA.SWhile(cond, body) ->
          let env, stmts, cond' = expr env stmts cond in
          let body_string = translate_stmts env body in
          (env, stmts ^
           "while (" ^ cond' ^ ") {\n" ^ body_string ^ "}\n")
      | SA.SBreak -> (env, "break;\n")
      | SA.SContinue -> (env, "continue;\n")
    in

    snd (List.fold_left stmt (env, "") slist)

  in

  let add_locals env fdecl =
    List.fold_left (fun (env, locals) bind ->
      let env, local = string_of_bind env bind in
      (env, locals ^ local ^ ";\n")) (env, "") fdecl.SA.slocals
  in

  let string_of_func fdecl =
    let env, formals = List.fold_left (fun (env, formals) (qual, bind) ->
      let env, bind' = string_of_bind env bind in
      let formal = (match qual with
          A.In -> ""
        | A.Out -> "out "
        | A.Inout -> "inout ") ^ bind'
      in
      (env, if formals = "" then formal else formals ^ ", " ^ formal))
    (env, "") fdecl.SA.sformals
    in
    let env, locals = add_locals env fdecl
    in
    let env = { env with cur_qualifier = fdecl.SA.sfqual }
    in
    string_of_typ fdecl.SA.styp ^ " " ^ StringMap.find fdecl.SA.sfname func_names ^
    "(" ^ formals ^ ") {\n" ^ locals ^ translate_stmts env fdecl.SA.sbody ^ "}"
  in

  let glsl_funcs = String.concat "\n\n" (List.map string_of_func
    (List.filter (fun fdecl ->
      fdecl.SA.sfqual = A.GpuOnly || fdecl.SA.sfqual = A.Both)
    functions)) ^ "\n\n"
  in

  (* construct a GLSL shader corresponding to a @vertex or @fragment entrypoint
   *)
  let string_of_entrypoint fdecl =
    (* input/output parameters become global in/out variables *)
    let io_decls, _, env = List.fold_left (fun (decls, idx, env) (qual, bind) ->
      let env, bind' = string_of_bind env bind in
      let decl = (if fdecl.SA.sfqual = A.Vertex && qual = A.In then
        (* vertex inputs are linked by location *)
        "layout(location = " ^ string_of_int idx ^ ") "
      else
        "") ^
      (match qual with
          A.In -> "in "
        | A.Out -> "out "
        | A.Inout ->
            raise (Failure ("inout on entrypoints not supported yet"))) ^
      bind' ^ ";\n"
      in
      (decls ^ decl, (if qual = A.In then idx + 1 else idx), env))
    ("", 0, env) fdecl.SA.sformals
    in
    let env, locals = add_locals env fdecl
    in
    let env = { env with cur_qualifier = fdecl.SA.sfqual }
    in
    "#version 330\n" ^
    "struct dummy_struct {int dummy;};\n\n"
    (*^ glsl_structs*) ^ glsl_funcs ^ io_decls ^
    (* the entrypoint itself becomes main() *)
    "\n\nvoid main() {\n" ^ locals ^ translate_stmts env fdecl.SA.sbody ^ "}"
  in

  (* return a map from entrypoint name to shader *)
  List.fold_left (fun funcs fdecl ->
    let shader = string_of_entrypoint fdecl in
    StringMap.add fdecl.SA.sfname shader funcs)
  StringMap.empty (List.filter (fun fdecl ->
    fdecl.SA.sfqual = A.Vertex || fdecl.SA.sfqual = A.Fragment) functions)

