(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

module StringMap = Map.Make(String)

type action = Ast | LLVM_IR | GLSL | Compile

let print_glsl glsl =
  StringMap.iter (fun name glsl ->
    print_string ("\nglsl for " ^ name ^ ":\n\n");
    print_string glsl) glsl


let _ =
  let action = ref Compile in
  let input = ref "" in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the SAST");
    ("-g", Arg.Unit (set_action GLSL), "Print the generated GLSL");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./microc.native [-a|-g|-l] [file]" in
  Arg.parse speclist (fun s -> input := s) usage_msg;
  let channel = if !input = "" then
    stdin
  else
    open_in !input
  in
  let prelude_channel = open_in "./prelude.blis" in
  let lexbuf = Lexing.from_channel channel in
  let prelude_lexbuf = Lexing.from_channel prelude_channel in
  let token _ =
    let token = Scanner.token prelude_lexbuf in
    if token = Parser.EOF then
      Scanner.token lexbuf
    else
      token
  in
  let ast = Parser.program token (Lexing.from_string "") in
  let sast = Semant.check ast in
  match !action with
    Ast -> print_string (Sast.string_of_sprogram sast)
  | GLSL -> print_glsl (Glslcodegen.translate sast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
  | Compile -> let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
