(* Code generation for shaders. Here we take each entrypoint and turn it into a
 * GLSL shader.
 *)

module A = Ast
module SA = Sast

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type symbol_table = {
  scope : string StringMap.t;
  used_names : StringSet.t;
}

let empty_table = {
  scope = StringMap.empty;
  used_names = StringSet.empty;
}

type translation_environment = {
  table : symbol_table;
  cur_qualifier : A.func_qualifier;
  forloop_update_statement : string;
}

(* return a fresh name given a set of already-used names. The original name is
 * usually a prefix of the new name, although this may not be true if the
 * original name is reserved by OpenGL.
 *
 *)
let add_symbol_table table orig =
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
  let orig' = if not (StringSet.mem orig' table.used_names) then
    orig'
  else
    (* keep appending digits until we get a new name *)
    let rec get_name orig n =
      let orig' = orig ^ string_of_int n in
        if not (StringSet.mem orig' table.used_names) then
          orig'
        else
          get_name orig (n + 1)
    in
    get_name orig' 0
  in
  ({ scope = StringMap.add orig orig' table.scope;
    used_names = StringSet.add orig' table.used_names }, orig')

let add_variable_name env name =
  let table', new_name = add_symbol_table env.table name in
  ({ env with table = table' }, new_name)

let translate ((structs, _, _, functions) : SA.sprogram) =
  let env = {
    table = empty_table;
    cur_qualifier = A.Both;
    forloop_update_statement = "";
  }
  in

  (* structs and functions share a namespace in GLSL, so use the same table to
   * translate the names for them.
   *)
  let struct_table =
    List.fold_left (fun table sdecl ->
      fst (add_symbol_table table sdecl.A.sname))
    { used_names = StringSet.singleton "dummy_struct";
      scope = StringMap.empty }
    structs 
  in

  let func_table =
    List.fold_left (fun table fdecl ->
      fst (add_symbol_table table fdecl.SA.sfname))
    struct_table functions
  in

  (* returns the GLSL type for the Blis type stripped of array-ness
   * for example, returns "vec4" for vec4[10][2]
   *)
  let rec string_of_base_typ = function
      A.Mat(A.Int, 1, 1) | A.Mat(A.Byte, 1, 1) -> "int"
    | A.Mat(A.Float, 1, 1) -> "float"
    | A.Mat(A.Bool, 1, 1) -> "bool"
    | A.Mat(A.Int, 1, l) | A.Mat(A.Byte, 1, l) -> "ivec" ^ string_of_int l
    | A.Mat(A.Float, 1, l) -> "vec" ^ string_of_int l
    | A.Mat(A.Bool, 1, l) -> "bvec" ^ string_of_int l
    | A.Mat(_, _, _) -> raise (Failure "unimplemented")
    | A.Struct(name) -> StringMap.find name struct_table.scope
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
      A.Array(typ, n) -> "[" ^
        (match n with Some(w) -> string_of_int w | _ -> "0") ^ "]"
        ^ string_of_array typ
    | _ -> ""
  in

  let string_of_typ typ = string_of_base_typ typ ^ string_of_array typ
  in

  let string_of_bind env (typ, name) =
    let env, new_name = add_variable_name env name in
    (env, string_of_base_typ typ ^ " " ^ new_name ^ string_of_array typ)
  in

  let struct_members = List.fold_left (fun map sdecl ->
    let table = List.fold_left (fun table (_, name) ->
      fst (add_symbol_table table name)) empty_table sdecl.A.members
    in
    StringMap.add sdecl.A.sname table map)
  StringMap.empty structs
  in

  let glsl_structs = String.concat "\n\n"
    (List.map (fun sdecl ->
      let members = StringMap.find sdecl.A.sname struct_members in
      let glsl_name = StringMap.find sdecl.A.sname struct_table.scope in
      "struct " ^ glsl_name ^ " {\n" ^
      (String.concat "\n" (List.map (fun (typ, name) ->
        let glsl_name = StringMap.find name members.scope in
        string_of_base_typ typ ^ " " ^ glsl_name ^ string_of_array typ ^ ";")
      sdecl.A.members)) ^
      "\n};") structs) ^ "\n\n"
  in

  (* from http://stackoverflow.com/questions/10068713/string-to-list-of-char *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  in

  let rec translate_stmts env slist =
    let rec expr env (typ, e) = match e with
        SA.SIntLit(i) -> string_of_int i
      | SA.SFloatLit(f) -> string_of_float f
      | SA.SBoolLit(b) -> if b then "true" else "false"
      | SA.SCharLit(c) -> string_of_int (Char.code c)
      | SA.SStringLit(s) ->
          "int[" ^ string_of_int (String.length s) ^ "](" ^
            String.concat ", "
              (List.map (fun c -> string_of_int (Char.code c)) (explode s)) ^
            ")"
      | SA.SId(n) -> StringMap.find n env.table.scope
      | SA.SStructDeref(e, mem) -> let e' = expr env e in
          (match fst e with
              A.Mat(_, 1, _) -> "(" ^ e' ^ ")." ^ mem
            | A.Struct(name) ->
                let members = StringMap.find name struct_members in
                let glsl_mem = StringMap.find mem members.scope in
                "(" ^ e' ^ ")." ^ glsl_mem
            | _ -> raise (Failure "unimplemented"))
      | SA.SArrayDeref(e, idx) ->
          "(" ^ expr env e ^ ")[" ^ expr env idx ^ "]"
      | SA.SBinop(e1, op, e2) ->
          "(" ^ expr env e1 ^ ") " ^ (match op with
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
            | SA.BOr -> "||") ^
          " (" ^ expr env e2 ^ ")"
      | SA.SUnop(op, e) ->
          (match op with
              SA.INeg | SA.FNeg -> "-"
            | SA.BNot -> "!") ^ "(" ^ expr env e ^ ")"
      | SA.STypeCons(elist) -> (match typ with
          A.Mat(_, 1, _) | A.Array(_, _) ->
            let elist' = expr_list env elist
            in
            string_of_typ typ ^ "(" ^ elist' ^ ")"
        | _ -> raise (Failure ("unexpected type constructor for " ^
                string_of_typ typ)))
      | SA.SNoexpr -> ""

    and expr_list env elist =
      (* handle lists for e.g. function arguments *)
      String.concat ", " (List.map (expr env) elist)
    in
        
    let stmt env = function
        SA.SAssign(l, r) ->
          let l' = expr env l in
          let r' = expr env r in
          "(" ^ l' ^ ") = (" ^ r' ^ ");\n"
      | SA.SCall((A.Void, SA.SNoexpr), name, elist) ->
          let elist' = expr_list env elist
          in
           StringMap.find name func_table.scope ^ "(" ^ elist' ^ ");\n"
      | SA.SCall(ret, name, elist) ->
          let elist' = expr_list env elist
          in
          let ret = expr env ret
          in
           ret ^ " = " ^ StringMap.find name func_table.scope ^ "(" ^ elist' ^
           ");\n"
      | SA.SReturn(e) -> let e' = expr env e in
          if env.cur_qualifier = A.Vertex then
            "gl_Position = " ^ e' ^ ";\nreturn;\n"
          else
            "return " ^ e' ^ ";\n"
      | SA.SIf(predicate, then_stmts, else_stmts) ->
          "if (" ^ expr env predicate ^ ") {\n" ^
            translate_stmts env then_stmts ^
          "} else {\n" ^
            translate_stmts env else_stmts ^
          "}\n"
      | SA.SLoop(body, continue) ->
          let continue' = translate_stmts env continue in
          let env' = { env with forloop_update_statement = continue' } in
          let body' = translate_stmts env' body in
          "while (true) {\n" ^ body' ^ continue' ^ "}\n"
      | SA.SBreak -> "break;\n"
      | SA.SContinue -> env.forloop_update_statement ^ "continue;\n"
    in

    String.concat "" (List.map (stmt env) slist)

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
        | A.Inout -> "inout "
        | A.Uniform -> raise (Failure "unreachable")) ^ bind'
      in
      (env, if formals = "" then formal else formals ^ ", " ^ formal))
    (env, "") fdecl.SA.sformals
    in
    let env, locals = add_locals env fdecl
    in
    let env = { env with cur_qualifier = fdecl.SA.sfqual }
    in
    let name = StringMap.find fdecl.SA.sfname func_table.scope
    in
    string_of_typ fdecl.SA.styp ^ " " ^ name ^
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
        | A.Uniform -> "uniform "
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
    ^ glsl_structs ^ glsl_funcs ^ io_decls ^
    (* the entrypoint itself becomes main() *)
    "\n\nvoid main() {\n" ^ locals ^ translate_stmts env fdecl.SA.sbody ^ "}"
  in

  (* return a map from entrypoint name to shader *)
  List.fold_left (fun funcs fdecl ->
    let shader = string_of_entrypoint fdecl in
    StringMap.add fdecl.SA.sfname shader funcs)
  StringMap.empty (List.filter (fun fdecl ->
    fdecl.SA.sfqual = A.Vertex || fdecl.SA.sfqual = A.Fragment) functions)

