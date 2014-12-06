open Sast
open Ast

let rec j_prgm (prog: Sast.s_program) = 
	"import fry.*;" ^
	"import java.util.ArrayList;\n" ^
	"import java.util.Arrays;\n" ^ 
	"public class test{\n" ^
	(* Write Layouts as private classes *)
	String.concat "\n" (List.map j_layout prog.syms.layouts) ^ 
	(* Write function declarations *)
	String.concat "\n" (List.map j_fdecl prog.funcs) ^
	"\n\npublic static void main(String[] args){\n" ^
		String.concat "\n" (List.map j_stmt prog.stmts) ^
		"\n}\n}"

and j_data_type = function+
	String 	-> "String"
|   Int		-> "int"
|	Float	-> "float"
| 	Bool 	-> "boolean"	
(* Need to add in java libs to handle Layout/Table *)
 

and j_obj_data_type (typ: dataType)  = match typ with
	String 	-> "String"
|   Int -> "Integer"
|   Float -> "Float"
|   Bool -> "Boolean"
|   List(t) ->  "ArrayList<" ^ j_obj_data_type t ^ ">"
|	Layout(name)  -> name
|   Table(lyt) ->  "FRYTable<" ^ j_obj_data_type lyt ^">"
and getListType = function
	List(t) -> j_obj_data_type t

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
| 	S_ListLit(l, typ) -> "new ArrayList<"  ^ j_obj_data_type typ ^ ">(Arrays.asList(" ^ String.concat ", " (List.rev (List.map (fun e -> "new " ^ j_obj_data_type typ ^ "(" ^ j_expr e ^ ")") l)) ^ "))"
|   S_Id(x,_) -> x
|   S_Binop(e1, o, e2) -> writeBinOp e1 o e2 
| 	S_Postop(e1, o) -> j_expr e1 ^ 
	(match o with Inc -> "++" | Dec -> "--" )
| 	S_Preop(o,e1) -> "!" ^ j_expr e1
|   S_Slice(e1, e2) -> if e1 = S_Noexpr then
					   	"0,"^ j_expr e2
					   else if e2 = S_Noexpr then
					  	j_expr e1
					   else
					   	j_expr e1 ^ "," ^ j_expr e2
| 	S_Ref(e1, r, e2, typ) -> (match r with 
								ListRef -> writeListRef e1 r e2 typ
							| 	LayRef -> writeLayRef e1 r e2 typ)
| 	S_Assign(v, e) ->  v ^ " = " ^ j_expr e
|   S_LayoutLit(d,t) -> "new " ^ j_obj_data_type d ^ " (" ^ String.concat "," (List.rev (List.map j_expr t)) ^ ")"
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

and writeForLoop e1 e2 s = match e1 with 
	S_Id(name, typ) -> "for (" ^ getListType typ ^ " " ^ j_expr e1 ^ ": " ^ j_expr e2 ^ ") {\n" ^ j_stmt s ^"}"

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
	S_BasicDecl(d, e) -> j_obj_data_type d ^ " " ^ j_expr e ^ ";"
|   S_ListDecl(d, e) -> "ArrayList<" ^ j_obj_data_type d ^ "> " ^ j_expr e ^ ";"
|   S_LayoutDecl(d, e) -> j_obj_data_type d ^ " " ^ j_expr e ^ ";"

and writeBinOp e1 o e2 = j_expr e1 ^ 
		(match o with Add -> "+" | Sub -> "-" | Mult -> "*" |
				  Div -> "/" | Equal -> "==" | Neq -> "!=" | 
				  Less -> "<" | Leq -> "<=" | Greater -> ">" |
				  Geq -> ">=" (* |  In -> "In" | Notin -> "Notin" | From -> "From" *)
				  | And -> "&&" | Or -> "||" ) ^ j_expr e2

and writeListRef e1 r e2 typ = match e2 with
					 	S_Slice(es1, es2) ->"new ArrayList<"  ^ j_obj_data_type typ ^ ">(" ^ 
					 						 j_expr e1 ^ ".subList(" ^ 
					 						(match es2 with 
					 						  S_Noexpr -> j_expr e2 ^ ", " ^ j_expr e1 ^ ".size()"
					 						| _ -> j_expr e2 )
					 						^ "))"
					|   _ -> j_expr e1 ^".get(" ^ j_expr e2 ^")"

and writeLayRef e1 r e2 typ = j_expr e1 ^ "." ^ j_expr e2

and j_stmt = function
	S_Block(syms,ss) -> "{\n" ^ String.concat "\n" ( List.rev (List.map j_stmt ss)) ^ "\n}"	
| 	S_Expr(e,t) -> j_expr e ^ ";"
|   S_Return(e,t) -> "return " ^ j_expr e ^";"
| 	S_If(elif_l, s) -> writeIf elif_l s 
| 	S_For(e1, e2, s) -> writeForLoop e1 e2 s
| 	S_While(e, s) -> writeWhileLoop e s
| 	S_VarDecl(v) -> writeVarDecl v

and j_fdecl (f: s_func_decl) = "public static " ^ j_obj_data_type f.ret_type ^ " " ^ f.fname ^
							   "(" ^ String.concat "," (List.map writeFormal f.formals) ^") {\n" ^
							   String.concat "\n" (List.map j_stmt f.body) ^ "}"
	
and writeFormal (v: s_var_decl) = match v with
	S_BasicDecl(d, e) -> j_obj_data_type d ^ " " ^ j_expr e
|   S_ListDecl(d, e) -> "ArrayList<" ^ j_obj_data_type d ^ "> " ^ j_expr e
|   S_LayoutDecl(d, e) -> j_obj_data_type d ^ " " ^ j_expr e 

and j_layout (layout: string * s_var_decl list) = 
	let (name, v_decs) = layout in
	"private static class " ^ name ^ " extends FRYTable{\n" ^ String.concat "\n" (List.rev (List.map writeVarDecl v_decs)) ^
	j_layout_constructor v_decs name ^
	j_toString v_decs ^ 
	"}"

and j_layout_constructor (v_decs: s_var_decl list) (name: string) = 
"\npublic " ^ name ^ "(" ^ String.concat "," (List.rev (List.map writeFormal v_decs)) ^ ")" ^ 
"{\n" ^ String.concat ";\n" (List.map (fun v_dec -> let v_name = (match v_dec with 	
	S_BasicDecl(d, e) 
|   S_ListDecl(d, e) 
|   S_LayoutDecl(d, e) -> j_expr e) in "this." ^ v_name ^ "=" ^ v_name) v_decs) ^
";\n super(); }\n"

and j_toString (v_decs: s_var_decl list) =
"\npublic String toString(){\nreturn " ^ 
String.concat "+\"|\"+" (List.rev (List.map (fun v_dec -> let vname = (match v_dec with 	
	S_BasicDecl(d, e) 
|   S_ListDecl(d, e) 
|   S_LayoutDecl(d, e) -> j_expr e) in vname ^".toString()") v_decs))
^ ";\n}"
