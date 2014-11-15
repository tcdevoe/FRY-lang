open Ast


let rec j_prgm (prog) = 
	"public class test{\n" ^
	"public static void main(String[] args){\n" ^
		String.concat ";\n" ( List.rev (List.map j_stmt prog.stmts)) ^
		";\n}\n}"
and j_data_type = function
	String 	-> "String"
|   Int		-> "int"
|	Float	-> "float"
| 	Bool 	-> "boolean"	
(* Need to add in java libs to handle Layout/Table *)
(*|	Layout  ->	"Layout"
| 	Table	->  "Table" *) 

(************************************
	STATEMENT HELPER FUNCTIONS
*************************************)



and j_expr = function
	StringLit(s) -> "\"" ^ s ^ "\""
|   FloatLit(f) -> f
|   IntLit(i) -> string_of_int i
|   BoolLit(b) -> b
(* |   ListLit of expr list
|   ListGen of expr * expr 	(* List generator which keeps the min and max of the generated list *) *)
|   Id(x) -> x
(* |   Binop of expr * op * expr
|   Postop of expr * post
|   Preop of pre * expr
|   Ref of expr * ref * expr * expr (* ID of object * reference type * index1 * index2 *)
|   Assign of string * expr
|   Call of string * expr list
|   SetBuild of expr * string * expr * expr (* [ return-layout | ID <- element-of; expression ] *) *)
|   Noexpr -> ""

and writeForLoop e s = 
	"//TODO WRITE FORLOOP"

and writeIf (elif_l:(expr*stmt) list) (else_stmt:stmt) =
	"//TODO WRITE IF ELIF ELSE"

and writeWhileLoop (e:expr) (s:stmt) = 
	"while(" ^ j_expr e ^ ") { \n" ^
		j_stmt s^
	";\n}"

and writeVarDecl = function
	BasicDecl(d, e) -> j_data_type d ^ " " ^ j_expr e 
|   ListDecl(d, e) -> j_data_type d ^ "[] " ^ j_expr e 

and j_stmt = function
	Block(ss) -> "{" ^ String.concat ";\n" ( List.rev (List.map j_stmt ss)) ^ ";\n}"	
| 	Expr(e) -> j_expr e
|   Return(e) -> "return " ^ j_expr e
| 	If(elif_l, s) -> writeIf elif_l s 
| 	For(e, s) -> writeForLoop e s
| 	While(e, s) -> writeWhileLoop e s
| 	VarDecl(v) -> writeVarDecl v
