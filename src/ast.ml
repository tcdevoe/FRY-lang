(* Ref is List/Layout element reference *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | In | Notin | And | Or | From
type dataType  = String | Float | Bool | Int | Layout of string | Table | List of dataType | Void | Res
type post = Inc | Dec 
type pre = Not
type ref = ListRef | LayRef

type expr = 
    StringLit of string
|   FloatLit of string
|   IntLit of int
|   BoolLit of string
|   ListLit of expr list
|   ListGen of expr * expr 	(* List generator which keeps the min and max of the generated list *)
|   Id of string
|   Binop of expr * op * expr
|   Postop of expr * post
|   Preop of pre * expr
|   Ref of expr * ref * expr (* ID of object * reference type * index1 * index2 *)
|   Assign of string * expr
|   Call of string * expr list
| 	Slice of expr * expr
|   LayoutLit of dataType * expr list
|   TableInit of dataType 
|   SetBuild of expr * ((string * expr)  list) * expr (* [ return-layout | ID <- element-of; expression ] *)
|   Noexpr

and var_decl = VarDecl of dataType * expr

type stmt =
    Block of stmt list
|   Expr of expr
|   Return of expr
|   If of (expr * stmt) list * stmt
|   For of expr * expr * stmt (* needs to be a X in Y binop expr *)
|   While of expr * stmt
| 	VarDeclS of var_decl 
|   LayoutCreation of string * var_decl list 


type func_decl = {
    fname : string;
    ret_type : dataType;
    formals : var_decl list;
    body : stmt list;
}

(* stmts includes var decls and statements *)
type program = { stmts: stmt list; funcs: func_decl list }


(* Low-level AST printing for debugging *)

let rec expr_s = function
  StringLit(l) -> "StringLit " ^ l
| FloatLit(l) -> "FloatLit " ^ l
| IntLit(l) ->	"IntLit " ^ string_of_int l
| BoolLit(l) ->  "BoolLit " ^ l
| ListGen(e1, e2) -> "ListGen (" ^ expr_s e1 ^ ", "^ expr_s e2 ^ ")"
| ListLit(l) ->		"ListLit ("  ^ String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") l) ^ ")"
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
| Slice(e1,e2) -> "Slice (" ^ expr_s e1 ^","^expr_s e2 ^")"
| Ref(e1, r, e2) -> "Reference (" ^ expr_s e1 ^ ") " ^
	(match r with ListRef -> "ListRef" | LayRef -> "LayRef") ^ 
	" (" ^ expr_s e2 ^ ") "
| Assign(v, e) -> "Assign " ^ v ^ " (" ^ expr_s e ^ ") "
| Call(f, es) -> "Call " ^ f ^ " [" ^
		String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^ "]"
| TableInit(dataType) -> "TableInit ("^data_type_s dataType^")"
| LayoutLit(typ, e_list) -> "LayoutLit " ^ data_type_s typ ^ " [" ^ String.concat "," (List.map (fun e -> expr_s e) e_list)
| SetBuild(e1, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ String.concat "," (List.map (fun (id,e) -> id ^ " from (" ^ expr_s e ^ ")") e2) ^ "(" ^ expr_s e3 ^ ") "
| Noexpr -> "Noexpr"

and data_type_s = function
  String -> "String"
| Float -> "Float" 
| Bool -> "Bool" 
| Int -> "Int"  
| Layout(name) -> "Layout(" ^ name ^")"
| Table -> "Table"
| List(t) ->  data_type_s t ^ " List"


let var_decl_s = function 
	VarDecl(d,e) -> "VarDecl (" ^ data_type_s d ^ " " ^ expr_s e ^ ")"

let rec stmt_s = function
  Block(ss) -> "Block [" ^ String.concat ",\n"
  								(List.map (fun s -> "(" ^ stmt_s s ^ ")") ss) ^ "]"
| Expr(e) -> "Expr (" ^ expr_s e ^ ") "
| Return(e) -> "Return (" ^ expr_s e ^ ")"
| If(elif_l, s) ->	(match elif_l with
					[] -> ""
				|   [x] -> "If ( " ^ expr_s (fst x) ^ ", " ^ stmt_s (snd x) ^ ")\n" 
				|   h::t -> "If ( " ^ expr_s (fst h) ^ ", " ^ stmt_s (snd h) ^ ")\n" ^
					String.concat "\n" (List.map (fun tl -> 
					"Elif (" ^ expr_s (fst tl) ^ ", " ^ stmt_s (snd tl) ^ ")" ) t) ) ^
				(* Don't really care about empty else for pretty printing, see javagen for implementation *)
				"\n Else (" ^ stmt_s s ^ ")"				
| For(e1, e2, s) -> "For (" ^ expr_s e1 ^ " in " ^ expr_s e2 ^ ") (" ^ stmt_s s ^ ") "
| While(e, s) -> "While (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ") "
| VarDeclS(v) -> var_decl_s v
| LayoutCreation(name, v_list) -> "Layout " ^ name ^ "[" ^ String.concat "," (List.map (fun v -> var_decl_s v) v_list) ^ "]\n"


let func_decl_s f = 
" { fname = \"" ^ f.fname ^ "\"\n ret_type = \"" ^ data_type_s f.ret_type ^ "\"\n formals = [" ^ 
String.concat ", " (List.map var_decl_s f.formals) ^ "]\n body = [" ^
String.concat "\n" (List.map stmt_s f.body) ^
"]}\n"

(* stmt list is built backwards, need to reverse *)
let program_s prog = "([" ^  String.concat ",\n" (List.rev (List.map stmt_s prog.stmts)) ^ "],\n" ^ 
					  "[" ^ String.concat ",\n" (List.map func_decl_s prog.funcs) ^"])"

