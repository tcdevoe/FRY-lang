open Ast

type s_expr = 
    S_StringLit of string
|   S_FloatLit of string
|   S_IntLit of int
|   S_BoolLit of string
|   S_ListLit of expr list * dataType
|   S_ListGen of expr * expr 	(* List generator which keeps the min and max of the generated list *)
|   S_Id of string * dataType
|   S_Binop of expr * op * expr
|   S_Postop of expr * post
|   S_Preop of pre * expr
|   S_Ref of expr * ref * expr * expr (* ID of object * reference type * index1 * index2 *)
|   S_Assign of string * expr
|   S_Call of string * expr list
|   S_SetBuild of expr * string * expr * expr (* [ return-layout | ID <- element-of; expression ] *)
|   S_Noexpr

type s_var_decl = 
	S_BasicDecl of dataType * string * expr
|   S_ListDecl of dataType * string * expr

type s_stmt =
    S_Block of stmt list
|   S_Expr of s_expr * dataType
|   S_Return of expr
|   S_If of (expr * stmt) list * stmt
|   S_For of expr * stmt (* needs to be a X in Y binop expr *)
|   S_While of expr * stmt
| 	S_VarDecl of var_decl 


type s_func_decl = {
    fname : string;
    ret_type : dataType;
    formals : var_decl list;
    body : stmt list;
}

(* stmts includes var decls and statements *)
type s_program = { stmts: stmt list; funcs: func_decl list }
