(* Ocamllex scanner for MicroC *)

{ open Parser }

let digits = ['0'-'9']+
let exp = ['e''E'] ['+' '-']? digits

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//" [^'\n']* '\n' { token lexbuf }   (* C++ style comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '.'      { DOT }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "int"    { INT(1) }
| "float"  { FLOAT(1) }
| "vec" (digits as width) {
  let width = int_of_string width in
  if width < 2 || width > 4 then
    raise (Failure("vecN with N not between 2 and 4"))
  else
    FLOAT(width) }
| "ivec" (digits as width) {
  let width = int_of_string width in
  if width < 2 || width > 4 then
    raise (Failure("ivecN with N not between 2 and 4"))
  else
    INT(width) }
| "u8"     { BYTE(1) }
| "bool"   { BOOL(1) }
| "bvec" (digits as width) {
  let width = int_of_string width in
  if width < 2 || width > 4 then
    raise (Failure("bvecN with N not between 2 and 4"))
  else
    BOOL(width) }
| "u8vec" (digits as width) {
  let width = int_of_string width in
  if width < 2 || width > 4 then
    raise (Failure("u8vecN with N not between 2 and 4"))
  else
    BYTE(width) }
| "window" { WINDOW }
| "buffer" { BUFFER }
| "pipeline" { PIPELINE }
| "void"   { VOID }
| "struct" { STRUCT }
| "true"   { TRUE }
| "false"  { FALSE }
| "in"  { IN }
| "out" { OUT }
| "inout" { INOUT }
| digits as lxm { INT_LITERAL(int_of_string lxm) }
| (digits exp | (digits '.' digits? | '.' digits) exp?) as lxm
  { FLOAT_LITERAL(float_of_string lxm) }
| "@gpuonly"  { GPUONLY }
| "@gpu"      { GPU }
| "@vertex"   { VERTEX }
| "@fragment" { FRAGMENT }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| "'" ([^ '\'' '\\'] as c) "'" { CHAR_LITERAL(c) }
| "'\\n'" { CHAR_LITERAL('\n') }
| "'\\t'" { CHAR_LITERAL('\t') }
| "'\\''" { CHAR_LITERAL('\'') }
| "'\\\"'" { CHAR_LITERAL('"') }
| "\\\\" { CHAR_LITERAL('\\') }
| "'\\" (digits as d) "'" {
  let value = int_of_string d in
  if value > 255 then
    raise (Failure "character escape must be 0-255")
  else
    CHAR_LITERAL(Char.chr value)
}
| '"' { STRING_LITERAL(str "" lexbuf) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and str old_str = parse
[^ '\n' '"' '\\']+ as c { str (old_str ^ c) lexbuf }
| "\\n" { str (old_str ^ "\n") lexbuf }
| "\\t" { str (old_str ^ "\t") lexbuf }
| "\\\"" { str (old_str ^ "\"") lexbuf }
| "\\'" { str (old_str ^ "\'") lexbuf }
| "\\" (digits as d) {
  let value = int_of_string d in
  if value > 255 then
    raise (Failure "character escape must be 0-255")
  else
    str (old_str ^ String.make 1 (Char.chr value)) lexbuf
}
| "\\\\" { str (old_str ^ "\\" ) lexbuf }
| "\\\n" { str (old_str ^ "\n") lexbuf }
| '"' { old_str }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char ^
  " in string literal")) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
