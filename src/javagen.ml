open Sast
open Ast
(* Update javagen to take SAST args *)

let rec j_prgm (prog: Sast.s_program) = 
	"import fry.IOUtils;\n" ^
	"import fry.FRYListFactory;\n" ^
	"import java.util.ArrayList;\n" ^
	"public class test{\n" ^
	"public static void main(String[] args){\n" ^
		String.concat "\n" (List.map j_stmt prog.stmts) ^
		"\n}\n}"

and j_data_type = function
	String 	-> "String"
|   Int		-> "int"
|	Float	-> "float"
| 	Bool 	-> "boolean"	
(* Need to add in java libs to handle Layout/Table *)
(*|	Layout  ->	"Layout"
| 	Table	->  "Table" *) 

and j_obj_data_type = function
	String 	-> "String"
|   Int -> "Integer"
|   Float -> "Float"
|   Bool -> "Boolean"
(************************************
	STATEMENT HELPER FUNCTIONS
*************************************)

and j_expr = function
	S_StringLit(s) -> "\"" ^ s ^ "\""
|   S_FloatLit(f) -> f
|   S_IntLit(i) -> string_of_int i
|   S_BoolLit(b) -> b
| 	S_ListGen(e1, e2) -> "FRYListFactory.getGeneratedFryList(" ^ j_expr e1 ^ "," ^ j_expr e2 ^ ")"
(* Should determine the data type in the check pass and add it to the constructor *)
| 	S_ListLit(l, typ) -> "new ArrayList<"  ^ j_obj_data_type typ ^ ">(" ^ String.concat ", " (List.map (fun e -> "new (" ^ j_expr e ^ ")") l) ^ ")"
|   S_Id(x,_) -> x
|   S_Binop(e1, o, e2) -> writeBinOp e1 o e2 
| 	S_Postop(e1, o) -> j_expr e1 ^ 
	(match o with Inc -> "++" | Dec -> "--" )
| 	S_Preop(o,e1) -> "!" ^ j_expr e1
(*| 	 Ref(e1, r, e2, e3) -> "Reference (" ^ expr_s e1 ^ ") " ^
	(match r with ListRef -> "ListRef" | LayRef -> "LayRef") ^ 
	" (" ^ expr_s e2 ^ ") " ^ " (" ^ expr_s e3 ^ ") " *)
| 	S_Assign(v, e) ->  v ^ " = " ^ j_expr e
|   S_Call(f, es) -> (match f with 
	| "Write" -> "IOUtils.Write" ^ "(" ^
		String.concat ", " (List.map elemForIO es) ^ ")"		
	| "Read" -> "IOUtils.Read" ^ "(" ^
		String.concat ", " (List.map elemForIO es) ^ ")"
	| _ -> f ^ "(" ^
		String.concat ", " (List.map (fun e -> j_expr e ) es) ^ ")")
(* | SetBuild(e1, id, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ id ^ " from (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") " *)
| S_Noexpr -> ""

and elemForIO e = match e with S_Id("stdout", String) -> "IOUtils.stdout"
							| 	S_Id("stderr", String) -> "IOUtils.stderr"
							|  	S_Id("stdin", String) ->	"IOUtils.stdin" 
							| 	_ -> j_expr e

and writeForLoop e1 e2 s = 
	"//TODO WRITE FORLOOP"

and writeIf (elif_l:(s_expr * s_stmt) list) (else_stmt: s_stmt) = (match elif_l with
					[] -> ""
				|   [x] -> "if ( " ^ j_expr (fst x) ^ ")\n" ^ j_stmt (snd x) ^ "\n" 
				|   h::t -> "if ( " ^ j_expr (fst h) ^ ")\n" ^ j_stmt (snd h) ^ "\n" ^
					String.concat "\n" (List.map (fun tl -> 
					"else if (" ^ j_expr (fst tl) ^ " )\n" ^ j_stmt (snd tl) ^ "\n" ) t) ) ^
				(match else_stmt with
					S_Block(_,[]) -> ""
				| 	_ -> "else\n" ^ j_stmt else_stmt ^ "\n")	

and writeWhileLoop (e: s_expr) (s: s_stmt) = 
	"while(" ^ j_expr e ^ ") { \n" ^
		j_stmt s ^
	"\n}"

and writeVarDecl = function
	S_BasicDecl(d, e) -> j_data_type d ^ " " ^ j_expr e ^ ";"
|   S_ListDecl(d, e) -> "ArrayList<" ^ j_obj_data_type d ^ "> " ^ j_expr e ^ ";"

and writeBinOp e1 o e2 = j_expr e1 ^ 
		(match o with Add -> "+" | Sub -> "-" | Mult -> "*" |
				  Div -> "/" | Equal -> "==" | Neq -> "!=" | 
				  Less -> "<" | Leq -> "<=" | Greater -> ">" |
				  Geq -> ">=" (* |  In -> "In" | Notin -> "Notin" | From -> "From" *)
				  | And -> "&&" | Or -> "||" ) ^ j_expr e2


and j_stmt = function
	S_Block(syms,ss) -> "{\n" ^ String.concat "\n" ( List.rev (List.map j_stmt ss)) ^ "\n}"	
| 	S_Expr(e,t) -> j_expr e ^ ";"
|   S_Return(e,t) -> "return " ^ j_expr e
| 	S_If(elif_l, s) -> writeIf elif_l s 
| 	S_For(e1, e2, s) -> writeForLoop e1 e2 s
| 	S_While(e, s) -> writeWhileLoop e s
| 	S_VarDecl(v) -> writeVarDecl v
