(***************************************
check.ml

Purpose:
	Semantic Analysis of a program
	and generate a SAST
****************************************)

open Ast
open Sast
exception Error of string

type symbol_table = {
	(* Parent symbol table included so variables are included in child scope *)
	parent: symbol_table option;
	(* May need to add more to this to keep track of array vars *)
	variables: (dataType * string * expr) list;
}

type function_table = {
	functions: s_func_decl list;
}

type translation_environment = {
	scope: symbol_table;
}

let rec find_variable (scope: symbol_table) name = 
	try
		List.find (fun (_, s, _) -> s = name ) scope.variables
	with Not_found ->
		match scope.parent with
		Some(parent) -> find_variable parent name
		| _ -> raise (Error("Unrecognized identifier " ^ name ^ "."))

let get_dataType (e: Ast.expr) (env: translation_environment) = function
	StringLit(l) -> String
|   FloatLit(l) -> Float 
| 	IntLit(l) -> Int
|   BoolLit(l) -> Bool
|   Id(x) -> let (t,_,_) = find_variable env.scope x in t
(* Add Table, Layout, List *)

let get_math_binop (t1: dataType) (t2: dataType) (env: translation_environment) = match (t1, t2) with
		(Int, Float) ->  Float
	|	(Float, Int) -> Float
	|	(Int, Int) ->  Int
	|	(Float, Float) ->  Float
	|   (_,_) ->  raise (Error("Incompatible types for math operator"))

let get_equal_binop (t1: dataType) (t2: dataType) (env: translation_environment) = match (t1, t2) with
		(Int, Float) ->  Bool
	|	(Float, Int) -> Bool
	|	(Int, Int) ->  Bool
	|	(Float, Float) ->  Bool
	|   (String, String) -> Bool
	|   (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for equality operator"))

let get_ineq_binop (t1: dataType) (t2: dataType) (env: translation_environment) = match (t1, t2) with
		(Int, Float) ->  Bool
	|	(Float, Int) -> Bool
	|	(Int, Int) ->  Bool
	|	(Float, Float) ->  Bool
	|   (_,_) ->  raise (Error("Incompatible types for inequality operator"))

let get_contain_binop (t1: dataType) (t2: dataType) (env: translation_environment) = match (t1, t2) with
		(Int, Int) ->  Bool
	|	(Float, Float) -> Bool
	|   (String, String) -> Bool
	|   (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for containment operator"))

let get_logic_binop (t1: dataType) (t2: dataType) (env: translation_environment) = match (t1, t2) with
       (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for logical operator"))

let rec get_Binop_return (e1: expr) (o: op) (e2: expr) (env: translation_environment) = 
	(* Check children expr *)
	let (e1, t1) = check_expr e1 env
	and (e2, t2) = check_expr e2 env in

	match o with 
	Add -> (match (t1, t2) with
			(String, String) -> S_Binop(e1, o, e2), String
		|	(Int, Float) -> S_Binop(e1, o, e2), Float
		|	(Float, Int) -> S_Binop(e1, o, e2), Float
		|	(Int, Int) -> S_Binop(e1, o, e2), Int
		|	(Float, Float) -> S_Binop(e1, o, e2), Float
		|	_ -> raise (Error("Incompatible types for binary operator.")))
	|  Sub -> S_Binop(e1, o, e2), get_math_binop t1 t2 env
	|  Mult -> S_Binop(e1, o, e2), get_math_binop t1 t2 env 
	|  Div -> S_Binop(e1, o, e2), get_math_binop t1 t2 env
	|  Equal -> S_Binop(e1, o, e2), get_equal_binop t1 t2 env 
	|  Neq -> S_Binop(e1, o, e2), get_equal_binop t1 t2 env 
	|  Less -> S_Binop(e1, o, e2), get_ineq_binop t1 t2  env 
	|  Leq -> S_Binop(e1, o, e2), get_ineq_binop t1 t2   env 
	|  Greater -> S_Binop(e1, o, e2), get_ineq_binop t1 t2  env 
	|  Geq -> S_Binop(e1, o, e2), get_ineq_binop t1 t2  env 
	|  In -> S_Binop(e1, o, e2), get_contain_binop t1 t2  env 
	|  Notin -> S_Binop(e1, o, e2), get_contain_binop t1 t2  env 
	|  And -> S_Binop(e1, o, e2), get_logic_binop t1 t2  env 
	|  Or -> S_Binop(e1, o, e2), get_logic_binop t1 t2  env 
	| From -> S_Binop(e1, o, e2), Table (* Need to add a check here *)

and get_Postop_return (e: expr) (o: post) (env: translation_environment) = 
	let (e, t) = check_expr e env in
		
	if t = Int then
		S_Postop(e,o), Int
	else
		raise (Error("Expression must have integer type for postfix operator."))

and get_Preop_return (e: expr) (o: pre) (env: translation_environment) = 
	let (e, t) = check_expr e env in
	
	if t = Bool then
		S_Preop(o, e), Bool
	else
		raise (Error("Expression must have boolean type for Not operator."))

and check_assign (v: string) (e: expr) (env: translation_environment) =
	let vdecl = find_variable env.scope v
	and e = check_expr e env in

	let (t1,_,_) = vdecl
	and (_,t2) = e in

	if t1 <> t2 then 
		raise (Error("Variable and assignment must have same type"))


and check_expr (e: Ast.expr) (env: translation_environment) = match e with
	StringLit(l) -> S_StringLit(l), get_dataType StringLit(l)
| 	FloatLit(l) ->  S_FloatLit(l), get_dataType FloatLit(l)
| 	IntLit(l) ->	S_IntLit(l), get_dataType IntLit(l)
| 	BoolLit(l) ->  S_BoolLit(l), get_dataType BoolLit(l)
|   Id(x) -> 
	let vdecl = try
		find_variable env.scope
	with Not_found ->
		raise (Error("Undeclared identifier" ^ x))
	in
	let (typ,_,_) = vdecl in
	Sast.Id(vdecl,typ), typ

| 	Binop(e1, o, e2) -> get_Binop_return e1 o e2 env	 
| 	Postop(e1, o) -> get_Postop_return e1 o env
| 	Preop(o,e1) -> get_Preop_return e1 o env
(* | 	Ref(e1, r, e2, e3) ->  NEED TO ADD LIST/LAYOUT References*)
| 	Assign(v, e) -> check_assign v e env
(* | 	Call(f, es) -> "Call " ^ f ^ " [" ^
		String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^ "]"
| 	SetBuild(e1, id, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ id ^ " from (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") " *)
| 	Noexpr -> S_Noexpr, Void

and check_stmt (s: Ast.stmt) (env: translation_environment) = match s with
  Block(ss) -> "Block [" ^ String.concat ",\n"
  								(List.map (fun s -> "(" ^ stmt_s s ^ ")") ss) ^ "]"
| Expr(e) -> S_Expr(check_expr e env)
| Return(e) -> "Return (" ^ expr_s e ^ ")"
| If(elif_l, s) ->	(match elif_l with
					[] -> ""
				|   [x] -> "If ( " ^ expr_s (fst x) ^ ", " ^ stmt_s (snd x) ^ ")\n" 
				|   h::t -> "If ( " ^ expr_s (fst h) ^ ", " ^ stmt_s (snd h) ^ ")\n" ^
					String.concat "\n" (List.map (fun tl -> 
					"Elif (" ^ expr_s (fst tl) ^ ", " ^ stmt_s (snd tl) ^ ")" ) t) ) ^
				(* Don't really care about empty else for pretty printing, see javagen for implementation *)
				"\n Else (" ^ stmt_s s ^ ")"				
| For(e, s) -> "For (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ") "
| While(e, s) -> "While (" ^ expr_s e ^ ") (" ^ stmt_s s ^ ") "
| VarDecl(v) -> var_decl_s v

let check_fdecl x = x

let check_prgm (prog) = 
	{ stmts = (List.map check_stmt prog.stmts);  funcs = (List.map check_fdecl prog.funcs) } 