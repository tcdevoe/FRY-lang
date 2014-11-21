(***************************************
check.ml

Purpose:
	Semantic Analysis of a program
	and generate a SAST
****************************************)

open Ast
open Sast
exception Error of string


let rec find_variable (scope: symbol_table) name = 
	try
		List.find (fun (_, s, _) -> s = name ) scope.variables
	with Not_found ->
		match scope.parent with
		Some(parent) -> find_variable parent name
		| _ -> raise (Error("Unrecognized identifier " ^ name ^ "."))

let get_math_binop (t1: dataType) (t2: dataType) (env: translation_environment) = 
	match (t1, t2) with
		(Int, Float) ->  Float
	|	(Float, Int) -> Float
	|	(Int, Int) ->  Int
	|	(Float, Float) ->  Float
	|   (_,_) ->  raise (Error("Incompatible types for math operator"))

let get_equal_binop (t1: dataType) (t2: dataType) (env: translation_environment) = 
	match (t1, t2) with
		(Int, Float) ->  Bool
	|	(Float, Int) -> Bool
	|	(Int, Int) ->  Bool
	|	(Float, Float) ->  Bool
	|   (String, String) -> Bool
	|   (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for equality operator"))

let get_ineq_binop (t1: dataType) (t2: dataType) (env: translation_environment) = 
	match (t1, t2) with
		(Int, Float) ->  Bool
	|	(Float, Int) -> Bool
	|	(Int, Int) ->  Bool
	|	(Float, Float) ->  Bool
	|   (_,_) ->  raise (Error("Incompatible types for inequality operator"))

let get_contain_binop (t1: dataType) (t2: dataType) (env: translation_environment) = 
	match (t1, t2) with
		(Int, Int) ->  Bool
	|	(Float, Float) -> Bool
	|   (String, String) -> Bool
	|   (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for containment operator"))

let get_logic_binop (t1: dataType) (t2: dataType) (env: translation_environment) = 
	match (t1, t2) with
       (Bool, Bool) -> Bool
	|   (_,_) ->  raise (Error("Incompatible types for logical operator"))

let rec check_expr (e: expr) (env: translation_environment) : (Sast.s_expr * Ast.dataType)  = 
match e with
	StringLit(l) -> S_StringLit(l), String
| 	FloatLit(l) ->  S_FloatLit(l), Float
| 	IntLit(l) ->	S_IntLit(l), Int
| 	BoolLit(l) ->  S_BoolLit(l), Bool
|   Id(x) -> 
	let vdecl = try
		find_variable env.scope x
	with Not_found ->
		raise (Error("Undeclared identifier " ^ x ^ "."))
	in
	let (typ,vname,_) = vdecl in
		S_Id(vname,typ), typ
(*  need to check both expressions are integers for List generator *)
| 	ListGen(e1, e2) -> check_list_gen e1 e2 env, List
(* Need to check that every expression in the list has the same type as the first expr *)
| 	ListLit(l) -> check_list_lit l env, List
| 	Binop(e1, o, e2) -> get_Binop_return e1 o e2 env	 
| 	Postop(e1, o) -> get_Postop_return e1 o env
| 	Preop(o,e1) -> get_Preop_return e1 o env
(* | 	Ref(e1, r, e2, e3) ->  NEED TO ADD LIST/LAYOUT References*)
| 	Assign(v, e) -> check_assign v e env
(* | 	Call(f, es) -> "Call " ^ f ^ " [" ^
		String.concat ", " (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^ "]"
| 	SetBuild(e1, id, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ id ^ " from (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") " *)
| 	Noexpr -> S_Noexpr, Void

and get_Binop_return (e1: expr) (o: op) (e2: expr) (env: translation_environment) : (Sast.s_expr * Ast.dataType) = 
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

and get_Postop_return (e: expr) (o: post) (env: translation_environment) : (Sast.s_expr * Ast.dataType)  = 
	let (e, t) = check_expr e env in
		
	if t = Int then
		S_Postop(e,o), Int
	else
		raise (Error("Expression must have integer type for postfix operator."))

and get_Preop_return (e: expr) (o: pre) (env: translation_environment)  : (Sast.s_expr * Ast.dataType) = 
	let (e, t) = check_expr e env in
	
	if t = Bool then
		S_Preop(o, e), Bool
	else
		raise (Error("Expression must have boolean type for Not operator."))

and check_assign (v: string) (e: expr) (env: translation_environment) : (Sast.s_expr * Ast.dataType) =
	let (t1,_,_) = find_variable env.scope v
	and (e, t2) = check_expr e env in

	if t1 = t2 then 
		S_Assign(v, e), t2
	else
		raise (Error("Variable and assignment must have same type"))		

and check_list_gen (e1: expr) (e2: expr) (env: translation_environment) =
	let (e1, t1) = check_expr e1 env in 
	let (e2, t2) = check_expr e2 env in
	if t1 = Int && t2 = Int then
		S_ListGen(e1, e2)
	else
	   	raise (Error("List generator must have integer valued ranges."))

and check_list_lit (l: expr list) (env: translation_environment) : (s_expr) = 
	let e = List.hd l in
	let (_,t) = check_expr e env in 
	let s_l = List.map (fun ex -> let (e',t') = check_expr ex env in
					if t = t' then
						e'
					else
						raise (Error("Elements of a list literal must all be of the same type"))) l 
	in S_ListLit(s_l, t)


let rec check_stmt (s: Ast.stmt) (env: translation_environment) = match s with
  Block(ss) ->	let scope' = { parent = Some(env.scope); variables = [] } in 
  				let env' = { env with scope = scope'} in
  				let ss = List.map (fun s -> check_stmt s env') ss in
  				scope'.variables <- List.rev scope'.variables;
  				S_Block(scope', ss)
| Expr(e) -> let (e,t) = check_expr e env in S_Expr(e,t)
| Return(e) -> check_return e env
| If(elif_l, s) ->	let elif_l' = check_cond elif_l env in
					let s_else = check_stmt s env in
					S_If(elif_l', s_else)
| For(e1, e2, s) -> check_for e1 e2 s env
| While(e, s) -> check_while e s env
| VarDecl(v) -> check_var_decl v env

(* Need to check every expr in the elif_l is boolean valued *)
(* Need to check stmt is valid *)
and check_cond (elif: (expr * stmt) list) (env: translation_environment) = 
	List.map (fun elif' -> let (e, t) = check_expr (fst elif') env in
	if t = Bool then
		let s = check_stmt (snd elif') env in (e,s)
	else
		raise (Error("Condition must be boolean-valued"))) elif

and check_return (e: expr) (env: translation_environment) = 
	if env.in_func then 
	let (e,t) = check_expr e env in
		if t = env.return_type then
			S_Return(e, t)
		else
			raise (Error("Must return compatible type"))
	else
		raise (Error("Cannot return outside of a function"))

(* e1 must be an identifier; e2 must be a list type *)
and check_for (e1: expr) (e2: expr) (s: stmt) (env: translation_environment) = 
	let (e1, t1) = check_expr e1 env in
	let (e2, t2) = check_expr e2 env in
	match e1 with
	 S_Id(_,_) -> if t2 = List then
					let s = check_stmt s env in
					S_For(e1, e2, s)
				else
					raise (Error("Second arg of for loop must be a List type"))
	| _ -> raise (Error("First arg of for loop must be an identifier"))

and check_while (e: expr) (s: stmt) (env: translation_environment) = 
	(* check expr is boolean valued *)
	let (e,t) = check_expr e env in
	if t = Bool then
		let s' = check_stmt s env in S_While(e,s')
	else
		raise (Error("While expression must be boolean-valued"))

and check_var_decl (v: var_decl) (env: translation_environment) = 
	match v with
	BasicDecl(typ,e) -> (match e with
								Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
										if exist then
											raise (Error("Identifier already declared"))
										else
											env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
											S_VarDecl(S_BasicDecl(typ, S_Id(x, typ)))
							|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
										if exist then
											raise (Error("Identifier already declared"))
										else
										env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
										let (e',_) = check_expr e env in
											env.scope.variables <- (typ, x, e')::env.scope.variables;
											S_VarDecl(S_BasicDecl(typ, S_Assign(x, e')))
							|  _ -> raise (Error ("Not a valid assignment")))
| 	ListDecl(typ, e) -> (match e with
								Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
										if exist then
											raise (Error("Identifier already declared"))
										else
											env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
											S_VarDecl(S_ListDecl(typ, S_Id(x, typ)))
							|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
										if exist then
											raise (Error("Identifier already declared"))
										else
										env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
 										let (e',_) = check_expr e env in
 											env.scope.variables <- (typ, x, e')::env.scope.variables;
											S_VarDecl(S_ListDecl(typ, S_Assign(x, e')))
							|  _ -> raise (Error ("Not a valid assignment")))
let check_formals (decl: var_decl) (env: translation_environment) =
	match decl with
	BasicDecl(typ,e) -> (match e with
								Id(x) -> env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
										 S_BasicDecl(typ, S_Id(x, typ))
							|   _ -> raise (Error("Function formals must be identifiers"))
						)
|   ListDecl(typ, e) -> (match e with
								Id(x) -> env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
										 S_ListDecl(typ, S_Id(x, typ))
							|   _ -> raise (Error("Function formals must be identifiers")))

let check_fdecl (func: Ast.func_decl) (env: translation_environment) : (s_func_decl) = 
	if env.in_func then
		raise (Error ("Cannot nest function declarations"))
	else
		let env' = { funcs = env.funcs; scope = {parent = Some(env.scope); variables = env.scope.variables}; return_type = func.ret_type; in_func = true} in 
		let f = { Sast.fname = func.fname; Sast.ret_type = func.ret_type; Sast.formals = (List.map (fun x -> check_formals x env') func.formals); Sast.body = (List.map (fun x -> check_stmt x env') func.body );} in
		env.funcs <- f::env.funcs; f



let check_prgm (prog: Ast.program ) : (Sast.s_program) = 
	let env = { funcs = [] ; scope = {parent = None; variables = []}; return_type = Void; in_func = false } in
	{ Sast.stmts = (List.map (fun x -> check_stmt x env) prog.stmts );  Sast.funcs = (List.map (fun x -> check_fdecl x env) prog.funcs) } 