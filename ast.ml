(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod

type uop = Neg | Not | PreInc | PreDec | PostInc | PostDec

type base_type = Float | Int | Byte | Bool

type typ =
    Mat of base_type * int * int
  | Array of typ * int option
  | Struct of string
  | Buffer of typ
  | Pipeline of string
  | Window
  | Void

type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | CharLit of char
  | StringLit of string
  | Id of string
  | StructDeref of expr * string
  | ArrayDeref of expr * expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | TypeCons of typ * expr list
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Local of bind * expr option (* optional initializer *)
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue

type formal_qualifier =
    In
  | Out
  | Inout
  | Uniform

type func_qualifier =
    GpuOnly
  | Vertex (* subset of GPU-only *)
  | Fragment (* subset of GPU-only *)
  | CpuOnly
  | Both

type func_decl = {
    typ : typ;
    fname : string;
    fqual : func_qualifier;
    formals : (formal_qualifier * bind) list;
    body : stmt list;
  }

type struct_decl = {
  sname : string;
  members : bind list;
}

type pipeline_decl = {
  pname : string;
  fshader : string;
  vshader : string;
}

type program = {
  struct_decls : struct_decl list;
  pipeline_decls : pipeline_decl list;
  var_decls : (bind * expr option) list;
  func_decls : func_decl list;
}

let rec base_type = function
    Array(typ, _) -> base_type typ
  | _ as typ -> typ

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Mod -> "%"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec string_of_typ = function
    Mat(Bool, 1, 1) -> "bool"
  | Mat(Int, 1, 1) -> "int"
  | Mat(Float, 1, 1) -> "float"
  | Mat(Byte, 1, 1) -> "u8"
  | Mat(Bool, 1, l) -> "bvec" ^ string_of_int l
  | Mat(Int, 1, l) -> "ivec" ^ string_of_int l
  | Mat(Float, 1, l) -> "vec" ^ string_of_int l
  | Mat(Byte, 1, l) -> "u8vec" ^ string_of_int l
  | Mat(Bool, w, l) -> "bmat" ^ string_of_int w ^ "x" ^ string_of_int l
  | Mat(Int, w, l) -> "imat" ^ string_of_int w ^ "x" ^ string_of_int l
  | Mat(Float, w, l) -> "mat" ^ string_of_int w ^ "x" ^ string_of_int l
  | Mat(Byte, w, l) -> "u8mat" ^ string_of_int w ^ "x" ^ string_of_int l
  | Struct s -> "struct " ^ s
  | Pipeline p -> "pipeline " ^ p
  | Buffer t -> "buffer" ^ "<" ^ string_of_typ t ^ ">"
  | Array(t, s) -> string_of_typ t ^ "[" ^
      (match s with Some(w) -> string_of_int w | _ -> "") ^ "]"
  | Window -> "window"
  | Void -> "void"

let rec string_of_expr expr = 
  let string_of_uop o e = match o with
      Neg -> "-" ^ string_of_expr e
    | Not -> "!" ^ string_of_expr e
    | PreInc -> string_of_expr e ^ "++"
    | PostInc -> "++" ^ string_of_expr e  
    | PreDec -> string_of_expr e ^ "--"
    | PostDec -> "--" ^ string_of_expr e  
  in
  (match expr with
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | StringLit(s) -> "\"" ^ String.escaped s ^ "\""
  | Id(s) -> s
  | StructDeref(e, m) -> string_of_expr e ^ "." ^ m
  | ArrayDeref(e, i) -> string_of_expr e ^ "[" ^ string_of_expr i ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | TypeCons(t, el) ->
      string_of_typ t ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Call(fname, el) ->
      fname ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> "")

let string_of_bind (t, id) =
  string_of_typ t ^ " " ^ id

let string_of_vdecl (bind, init) = match init with
    None -> string_of_bind bind ^ ";\n"
  | Some e -> string_of_bind bind ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_simple_vdecl bind = string_of_bind bind ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Local(decl, e) -> string_of_vdecl (decl, e)
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;"
  | Continue -> "continue;"

let string_of_formal_qual = function
    In -> ""
  | Out -> "out"
  | Inout -> "inout" 
  | Uniform -> "uniform"

let string_of_func_qual = function
    CpuOnly -> "@cpuonly"
  | GpuOnly -> "@gpuonly"
  | Vertex -> "@vertex"
  | Fragment -> "@fragment"
  | Both -> "@gpu"

let string_of_fdecl fdecl =
  string_of_func_qual fdecl.fqual ^ " " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map (fun (q, (t, n)) ->
  string_of_formal_qual q ^ " " ^ string_of_typ t ^ " " ^ n) fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_sdecl sdecl =
  "struct " ^ sdecl.sname ^ " {\n" ^
  String.concat "" (List.map string_of_simple_vdecl sdecl.members) ^ "};\n"

let string_of_pdecl pdecl =
  "pipeline " ^ pdecl.pname ^ " {\n" ^
  "@vertex " ^ pdecl.vshader ^ ";\n" ^
  "@fragment " ^ pdecl.fshader ^ ";\n" ^
  "};\n"

let string_of_program prog =
  String.concat "" (List.map string_of_sdecl prog.struct_decls) ^ "\n" ^
  String.concat "" (List.map string_of_pdecl prog.pipeline_decls) ^ "\n" ^
  String.concat "" (List.map string_of_vdecl prog.var_decls) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl prog.func_decls)
