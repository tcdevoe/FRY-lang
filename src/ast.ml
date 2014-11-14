(* Ref is List/Layout element reference *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | In | Notin | And | Or | From
type dataType = String | Float | Bool | Int | List | Layout | Table
type post = Inc | Dec 
type pre = Not
type ref = ListRef | LayRef

type expr = 
    StringLit of string
|   FloatLit of string
|   IntLit of int
|   BoolLit of string
|   Id of string
|   Binop of expr * op * expr
|   Postop of expr * post
|   Preop of pre * expr
|   Ref of expr * ref * expr * expr (* ID of object * reference type * index1 * index2 *)
|   Assign of string * expr
|   Call of string * expr list
|   Noexpr

type stmt =
    Block of stmt list
|   Expr of expr
|   Return of expr
|   If of (expr * stmt) list * stmt
|   For of expr * stmt (* needs to be a X in Y binop expr *)
|   While of expr * stmt
|   SetBuild of expr * expr * expr (* [ return-layout | element-of; expression ] *)

type var_decl = {
    data_type : dataType;
    decl : expr;
}

type func_decl = {
    fname : string;
    ret_type : dataType;
    formals : var_decl list;
    locals : var_decl list;
    body : stmt list;
}

type program = { stmts: stmt list; vars: var_decl list; funcs: func_decl list }


(* Low-level AST printing for debugging *)

let rec expr_s = function
  StringLit(l) -> "StringLit " ^ l
| FloatLit(l) -> "FloatLit " ^ l
| IntLit(l) ->	"IntLit " ^ string_of_int l
| BoolLit(l) ->  "BoolLit " ^ l
| Id(s) -> "ID " ^ s
| Binop(e1, o, e2) -> "Binop (" ^ expr_s e1 ^ ") " ^
	(match o with Add -> "Add" | Sub -> "Sub" | Mult -> "Mult" |
				  Div -> "Div" | Equal -> "Equal" | Neq -> "Neq" | 
				  Less -> "Less" | Leq -> "Leq" | Greater -> "Greater" |
				  Geq -> "Geq" | In -> "In" | Notin -> "Notin" | And -> "And" | 
				  Or -> "Or" | From -> "From") ^ " (" ^ expr_s e2 ^ ")"
| Postop(e1, o) -> "Postop (" ^ expr_s e1 ^ ") " ^
	(match o with Inc -> "Inc" | Dec -> "Dec" )
| Preop(o,e1) -> "Not (" ^ expr_s e1 ^ ") "
| Ref(e1, r, e2, e3) -> "Reference (" ^ expr_s e1 ^ ") " ^
	(match r with ListRef -> "ListRef" | LayRef -> "LayRef") ^ 
	" (" ^ expr_s e2 ^ ") " ^ " (" ^ expr_s e3 ^ ") "
| Assign(v, e) -> "Assign " ^ v ^ " (" ^ expr_s e ^ ") "
| Call(f, es) -> "Call " ^ f ^ " [" ^
		String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^ "]"
| Noexpr -> "Noexpr"


(* TODO: FINISH PRINT STMTS *)
let rec stmt_s = function
  Block(ss) -> "Block [" ^ String.concat ",\n"
  								(List.map (fun s -> "(" ^ stmt_s s ^ ")") ss) ^ "]"
| Expr(e) -> "Expr (" ^ expr_s e ^ ") "
| Return(e) -> "Return (" ^ expr_s e ^ ")"
| If(elif_l, s) -> "TODO"
| For(e, s) -> "For (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ") "
| While(e, s) -> "While (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ") "
| SetBuild(e1, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") "

let data_type_s = function
  String -> "String"
| Float -> "Float" 
| Bool -> "Bool" 
| Int -> "Int" 
| List -> "List" 
| Layout -> "Layout" 
| Table -> "Table"

let var_decl_s v = data_type_s v.data_type ^ " " ^ expr_s v.decl

let func_decl_s f = 
" { fname = \"" ^ f.fname ^ "\"\n ret_type = \"" ^ data_type_s f.ret_type ^ "\"\n formals = [" ^ 
String.concat ", " (List.map var_decl_s f.formals) ^ "]\n locals = [" ^ 
String.concat ", " (List.map var_decl_s f.locals) ^ "]\n body = [" ^
String.concat "\n" (List.map stmt_s f.body) ^
"]}\n"

let program_s prog = "([" ^  String.concat ", " (List.map stmt_s prog.stmts) ^ "],\n" ^ 
					  "[" ^ String.concat ", " (List.map var_decl_s prog.vars) ^ "],\n" ^
					  "[" ^ String.concat ", " (List.map func_decl_s prog.funcs) ^"])"

