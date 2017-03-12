(* Lower-level Abstract Syntax Tree and functions for printing it *)

open Ast

type sop = IAdd | ISub | IMult | IDiv
         | IEqual | INeq | ILess | ILeq | IGreater | IGeq
         | FAdd | FSub | FMult | FDiv
         | FEqual | FNeq | FLess | FLeq | FGreater | FGeq
         | BAnd | BOr | BEqual | BNeq

type suop = INeg | FNeg | BNot

type sexpr =
    SIntLit of int
  | SFloatLit of float
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * sop * sexpr
  | SUnop of suop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SFor of sexpr * sexpr * sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SBreak
  | SContinue

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let string_of_sop = function
    IAdd | FAdd -> "+"
  | ISub | FSub -> "-"
  | IMult | FMult -> "*"
  | IDiv | FDiv -> "/"
  | IEqual | BEqual | FEqual -> "=="
  | INeq | BNeq | FNeq -> "!="
  | ILess | FLess -> "<"
  | ILeq | FLeq -> "<="
  | IGreater | FGreater -> ">"
  | IGeq | FGeq -> ">="
  | BAnd -> "&&"
  | BOr -> "||"

let string_of_suop = function
    INeg | FNeg -> "-"
  | BNot -> "!"

let rec string_of_sexpr = function
    SIntLit(l) -> string_of_int l
  | SFloatLit(l) -> string_of_float l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_sop o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_suop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""

let rec string_of_sstmt = function
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, []) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmts s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmts s1 ^ "else\n" ^ string_of_sstmts s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmts s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmts s
  | SBreak -> "break;\n"
  | SContinue -> "continue;\n"
and string_of_sstmts stmts =
  "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
