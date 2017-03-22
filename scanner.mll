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
| "int"    { INT }
| "float"  { FLOAT(1) }
| "vec" (digits as width) {
  let width = int_of_string width in
  if width < 2 || width > 4 then
    raise (Failure("vecN with N not between 2 and 4"))
  else
    FLOAT(width) }
| "bool"   { BOOL }
| "void"   { VOID }
| "struct" { STRUCT }
| "true"   { TRUE }
| "false"  { FALSE }
| digits as lxm { INT_LITERAL(int_of_string lxm) }
| (digits exp | (digits '.' digits? | '.' digits) exp?) as lxm
  { FLOAT_LITERAL(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
