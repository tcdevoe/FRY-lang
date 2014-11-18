open Ast


let rec j_prgm (prog) = 
	"import fry.IOUtils;\n" ^
	"import fry.FRYListFactory;\n" ^
	"import java.util.ArrayList;\n" ^
	"public class test{\n" ^
	"public static void main(String[] args){\n" ^
		String.concat "\n" ( List.rev (List.map j_stmt prog.stmts)) ^
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
	StringLit(s) -> "\"" ^ s ^ "\""
|   FloatLit(f) -> f
|   IntLit(i) -> string_of_int i
|   BoolLit(b) -> b
| 	ListGen(e1, e2) -> "FRYListFactory.getGeneratedFryList(" ^ j_expr e1 ^ "," ^ j_expr e2 ^ ")"
(* Should determine the data type in the check pass and add it to the constructor *)
| 	ListLit(l) -> "new ArrayList<"  ^ "ADD GET DATA TYPE HERE>(" ^ String.concat ", " (List.map (fun e -> "new ADD GET DATA TYPE HERE(" ^ expr_s e ^ ")") l) ^ ")"
|   Id(x) -> x
|   Binop(e1, o, e2) -> writeBinOp e1 o e2 
| 	Postop(e1, o) -> j_expr e1 ^ 
	(match o with Inc -> "++" | Dec -> "--" )
| 	Preop(o,e1) -> "!" ^ j_expr e1
(*| 	 Ref(e1, r, e2, e3) -> "Reference (" ^ expr_s e1 ^ ") " ^
	(match r with ListRef -> "ListRef" | LayRef -> "LayRef") ^ 
	" (" ^ expr_s e2 ^ ") " ^ " (" ^ expr_s e3 ^ ") " *)
| 	Assign(v, e) ->  v ^ " = " ^ j_expr e
|   Call(f, es) -> (match f with 
	| "Write" -> "IOUtils.Write" ^ "(" ^
		String.concat ", " (List.map elemForIO es) ^ ")"		
	| "Read" -> "IOUtils.Read" ^ "(" ^
		String.concat ", " (List.map elemForIO es) ^ ")"
	| _ -> f ^ "(" ^
		String.concat ", " (List.map (fun e -> j_expr e ) es) ^ ")")
(* | SetBuild(e1, id, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ id ^ " from (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") " *)
| Noexpr -> ""

and elemForIO e = match e with Id("stdout") -> "\"stdout\""
							| 	Id("stderr") -> "\"stderr\""
							|  	Id("stdin") ->	"\"stdin\"" 
							| 	_ -> j_expr e

and writeForLoop e s = 
	"//TODO WRITE FORLOOP"

and writeIf (elif_l:(expr*stmt) list) (else_stmt:stmt) = (match elif_l with
					[] -> ""
				|   [x] -> "if ( " ^ j_expr (fst x) ^ ")\n" ^ j_stmt (snd x) ^ "\n" 
				|   h::t -> "if ( " ^ j_expr (fst h) ^ ")\n" ^ j_stmt (snd h) ^ "\n" ^
					String.concat "\n" (List.map (fun tl -> 
					"else if (" ^ j_expr (fst tl) ^ " )\n" ^ j_stmt (snd tl) ^ "\n" ) t) ) ^
				(match else_stmt with
					Block([]) -> ""
				| 	_ -> "else\n" ^ j_stmt else_stmt ^ "\n")	

and writeWhileLoop (e:expr) (s:stmt) = 
	"while(" ^ j_expr e ^ ") { \n" ^
		j_stmt s ^
	"\n}"

and writeVarDecl = function
	BasicDecl(d, e) -> j_data_type d ^ " " ^ j_expr e ^ ";"
|   ListDecl(d, e) -> "ArrayList<" ^ j_obj_data_type d ^ "> " ^ j_expr e ^ ";"

and writeBinOp e1 o e2 = j_expr e1 ^ 
		(match o with Add -> "+" | Sub -> "-" | Mult -> "*" |
				  Div -> "/" | Equal -> "==" | Neq -> "!=" | 
				  Less -> "<" | Leq -> "<=" | Greater -> ">" |
				  Geq -> ">=" (* |  In -> "In" | Notin -> "Notin" | From -> "From" *)
				  | And -> "&&" | Or -> "||" ) ^ j_expr e2


and j_stmt = function
	Block(ss) -> "{\n" ^ String.concat "\n" ( List.rev (List.map j_stmt ss)) ^ "\n}"	
| 	Expr(e) -> j_expr e ^ ";"
|   Return(e) -> "return " ^ j_expr e
| 	If(elif_l, s) -> writeIf elif_l s 
| 	For(e, s) -> writeForLoop e s
| 	While(e, s) -> writeWhileLoop e s
| 	VarDecl(v) -> writeVarDecl v
