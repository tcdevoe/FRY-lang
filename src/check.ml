(***************************************
check.ml

Purpose:
	Semantic Analysis of a program
	and generate a SAST
****************************************)

open Ast

type sast_expr = 
	Expr of expr

(* let check_expr = function
	StringLit(l) -> Expr(StringLit(l))
| 	FloatLit(l) ->  Expr(FloatLit(l))
| 	IntLit(l) ->	Expr(IntLit(l))
| 	BoolLit(l) ->  Expr(BoolLit(l))
|   Id(x) -> Expr(Id(x))
 *)
let check_stmt x = x
(*
	ListGen(e1, e2) -> 
*)

let check_fdecl x = x

let check_prgm (prog) = 
	{ stmts = (List.map check_stmt prog.stmts);  funcs = (List.map check_fdecl prog.funcs) } 