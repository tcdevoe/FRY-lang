open Ast

type s_expr = 
    S_StringLit of string
|   S_FloatLit of string
|   S_IntLit of int
|   S_BoolLit of string
|   S_ListLit of s_expr list * dataType
|   S_ListGen of s_expr * s_expr 	(* List generator which keeps the min and max of the generated list *)
|   S_Id of string * dataType
|   S_Binop of s_expr * op * s_expr
|   S_Postop of s_expr * post
|   S_Preop of pre * s_expr
|   S_Ref of s_expr * ref * s_expr * dataType * string (* ID of object * reference type * access expression * type *)
|   S_Slice of s_expr * s_expr
|   S_Assign of string * s_expr
|   S_Call of string * s_expr list
|   S_LayoutLit of dataType * s_expr list * string
|   S_TableInit of dataType
|   S_SetBuild of s_expr * string * s_expr * s_expr (* [ return-layout | ID <- element-of; expression ] *)
|   S_Noexpr

type symbol_table = {
	(* Parent symbol table included so variables are included in child scope *)
	parent: symbol_table option;
	mutable variables: (dataType * string * s_expr) list;
	(* Keeps track of tables' associated layouts *)
	mutable tables: (dataType * string) list;
	(* Tracks the layout name and its constituent values *)
	mutable layouts: (string * s_var_decl list) list;
}

and s_var_decl = 
	S_BasicDecl of dataType * s_expr
|   S_ListDecl of dataType * s_expr
|   S_LayoutDecl of dataType * s_expr 

type s_stmt =
    S_Block of symbol_table * s_stmt list
|   S_Expr of s_expr * dataType
|   S_Return of s_expr * dataType
|   S_If of (s_expr * s_stmt) list * s_stmt
|   S_For of s_expr * s_expr * s_stmt 
|   S_While of s_expr * s_stmt
| 	S_VarDecl of s_var_decl 


type s_func_decl = {
    fname : string;
    ret_type : dataType;
    formals : s_var_decl list;
    body : s_stmt list;
}

type translation_environment = {
	mutable funcs: s_func_decl list;
	scope: symbol_table;
	return_type: dataType;
	in_func: bool;
	inSetBuild: string;
}


type s_program = { stmts: s_stmt list; funcs: s_func_decl list; syms: symbol_table }

