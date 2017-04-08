/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT DOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN BREAK CONTINUE IF ELSE FOR WHILE
%token INT BOOL VOID STRUCT PIPELINE BUFFER WINDOW
%token GPUONLY GPU VERTEX FRAGMENT
%token IN OUT INOUT
%token <int> FLOAT
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%left DOT LPAREN LBRACKET

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { { struct_decls = []; pipeline_decls = []; var_decls = []; func_decls = []; } }
 | decls vdecl { { $1 with var_decls = $2 :: $1.var_decls; } }
 | decls fdecl { { $1 with func_decls = $2 :: $1.func_decls; } }
 | decls sdecl { { $1 with struct_decls = $2 :: $1.struct_decls; } }
 | decls pdecl { { $1 with pipeline_decls = $2 :: $1.pipeline_decls; } }

fdecl:
  /* this definition has to be repeated to avoid a shift-reduce conflict... grr
   */
   func_qualifier typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
         fqual = $1;
	 formals = $5;
	 body = List.rev $8 } }
 | typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
         fqual = CpuOnly;
	 formals = $4;
	 body = List.rev $7 } }

func_qualifier:
    GPUONLY             { GpuOnly }
  | VERTEX              { Vertex }
  | FRAGMENT            { Fragment }
  | GPU                 { Both }

vdecl:
    bind SEMI { $1 }

vdecl_list:
    /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

sdecl:
    STRUCT ID LBRACE vdecl_list RBRACE SEMI { {
      sname = $2;
      members = List.rev $4;
    } }

pdecl:
    PIPELINE ID LBRACE pdecl_list RBRACE SEMI { {
      $4 with pname = $2;
    } }

pdecl_list:
  /* nothing */ { { pname = ""; fshader = ""; vshader = "" } }
  | pdecl_list VERTEX ID SEMI  { if $1.vshader <> "" then
      raise (Failure ("vertex shader declared twice")) else
    { $1 with vshader = $3 } }
  | pdecl_list FRAGMENT ID SEMI  { if $1.fshader <> "" then
      raise (Failure ("fragment shader declared twice")) else
    { $1 with fshader = $3 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal_qualifier bind                   { [($1,$2)] }
  | formal_list COMMA formal_qualifier bind { ($3,$4) :: $1 }

formal_qualifier:
    /* nothing */ { In }
  | OUT { Out }
  | INOUT {Inout}
  
arrays:
    /* nothing */ { [] }
  | arrays LBRACKET INT_LITERAL RBRACKET { $3 :: $1 }

no_array_typ:
    INT { Vec(Int, 1) }
  | FLOAT { Vec(Float, $1) }
  | BOOL { Vec(Bool, 1) }
  | STRUCT ID { Struct($2) }
  | PIPELINE ID { Pipeline($2) }
  | BUFFER LT typ GT { Buffer($3) }
  | WINDOW { Window }
  | VOID { Void }

typ:
    no_array_typ arrays { List.fold_left (fun t len -> Array(t, len)) $1 $2 }

bind:
  typ ID { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | bind SEMI { Local ($1, None) }
  | bind ASSIGN expr SEMI { Local ($1, Some $3) }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LITERAL      { IntLit($1) }
  | FLOAT_LITERAL    { FloatLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr DOT    ID   { StructDeref($1, $3) }
  | expr LBRACKET expr RBRACKET { ArrayDeref($1, $3) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | typ LPAREN actuals_opt RPAREN { TypeCons($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
