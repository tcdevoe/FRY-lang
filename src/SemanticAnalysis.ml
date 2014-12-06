(***************************************
check.ml

Purpose:
	Semantic Analysis of a program
	and generate a SAST
****************************************)

open Ast
open Sast
exception Error of string

let print_sym_tbl (syms: symbol_table) = let str = "SYMBOL TABLE: \n[ Variables :" ^ String.concat "\n" (List.map (fun (typ, name, _) -> "[" ^ Ast.data_type_s typ ^ " " ^ name ^ "]") syms.variables) ^ "\n" ^
																	" Layouts :" ^ String.concat "\n" (List.map (fun (name,_) -> "[ Layout " ^ name ^ "]") syms.layouts) ^ "]" 
											in print_endline str

let rec find_variable (scope: symbol_table) name = 
	try
		List.find (fun (_, s, _) -> s = name ) scope.variables
	with Not_found ->
		match scope.parent with
		Some(parent) -> find_variable parent name
		| _ -> raise (Error("Unrecognized identifier " ^ name))

let rec find_layout (scope: symbol_table) name = 
	try
		List.find (fun (s, _) -> s = name ) scope.layouts
	with Not_found ->
		match scope.parent with
		Some(parent) -> find_layout parent name
		| _ -> raise (Error("Unrecognized layout type " ^ name))

let rec find_func (funcs: s_func_decl list) fname = 
	try 
	(* Return all functions with that name, for overloading *)
		let funcs' = List.filter (fun fn -> fn.fname = fname ) funcs in
			funcs'
	with Not_found -> raise (Error("Unrecognized Function call " ^ fname))

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
		raise (Error("Undeclared identifier " ^ x ))
	in
	let (typ,vname,_) = vdecl in
		S_Id(vname,typ), typ
(*  need to check both expressions are integers for List generator *)
| 	ListGen(e1, e2) -> let (e,t) = check_list_gen e1 e2 env in e, List(t)
(* Need to check that every expression in the list has the same type as the first expr *)
| 	ListLit(l) ->  let (e,t) = check_list_lit l env in e, List(t)
| 	Binop(e1, o, e2) -> get_Binop_return e1 o e2 env	 
| 	Postop(e1, o) -> get_Postop_return e1 o env
| 	Preop(o,e1) -> get_Preop_return e1 o env
|   Slice(e1, e2) -> check_slice e1 e2 env 
| 	Ref(e1, r, e2) -> get_ref_return e1 r e2 env 
| 	Assign(v, e) -> check_assign v e env
| 	Call(f, es) -> check_call f es env
|   LayoutLit(typ, e_list) -> check_layout_lit typ e_list env
(* | 	SetBuild(e1, id, e2, e3) -> "SetBuild (" ^ expr_s e1 ^ ") " ^ id ^ " from (" ^ expr_s e2 ^ ") (" ^ expr_s e3 ^ ") " *)
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
(* 	| From -> S_Binop(e1, o, e2), Table (* Need to add a check here *) *)

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
		S_ListGen(e1, e2), t1
	else
	   	raise (Error("List generator must have integer valued ranges."))

and check_list_lit (l: expr list) (env: translation_environment)  = 
	let e = List.hd l in
	let (_,t) = check_expr e env in 
	let s_l = List.map (fun ex -> let (e',t') = check_expr ex env in
					if t = t' then
						e'
					else
						raise (Error("Elements of a list literal must all be of the same type"))) l 
	in S_ListLit(s_l, t), t

and check_call (f: string) (es: expr list) (env: translation_environment) = 
	let sexpr_l_typ = List.map (fun e -> check_expr e env) es in
	let ret_funcs = find_func env.funcs f in
	let (sexpr_l, fdec) = find_func_signature f sexpr_l_typ ret_funcs in 
		S_Call(f, sexpr_l), fdec.ret_type

and find_func_signature (f: string) (opts: (s_expr * dataType) list) (ret_funcs: s_func_decl list) =
	(* Check function opts *)
	try 
		match ret_funcs with 
						[] -> raise (Error("Argument is not the correct type for function "^f))
					|	hd::tl -> let forms = hd.formals in 
									let sexpr = List.map2 (fun (opt: s_expr * dataType) (form: s_var_decl) -> 
									let opt_typ = snd opt in
									let form_typ = (match form with
														S_BasicDecl(d,_)
													|   S_ListDecl(d,_)
													|   S_LayoutDecl(d,_) -> d) in
									if opt_typ = form_typ then
										fst opt
									else
										S_Noexpr) opts forms in 
									let matched = List.exists (fun e -> e = S_Noexpr) sexpr in
									if matched then
										find_func_signature f opts tl
									else
										sexpr , hd
	with Invalid_argument(x) ->
		raise (Error("Incorrect number of arguments in call to function "^f))

and get_ref_return (e1: expr) (r: ref) (e2: expr) (env: translation_environment) =
	let (e1,typ1) = check_expr e1 env in
	match typ1 with 
	List(t) -> 	get_list_ref_return e1 r e2 env t
|   Layout(name) -> get_layout_ref_return e1 r e2 env name
|   _ -> raise (Error("Must reference a type List or Layout"))

and get_list_ref_return (e1: s_expr) (r: ref) (e2: expr) (env: translation_environment) (t: dataType) =
	let (e2, typ2) = check_expr e2 env in
	if typ2 = Int then
		S_Ref(e1,r,e2,t), t
   else
		raise (Error("List reference index must be integer valued"))	

and get_layout_ref_return (e1: s_expr) (r: ref) (e2: expr) (env: translation_environment) (name: string) =
	let (_, mems) = find_layout env.scope name in 
	(* Need to add layout members to temporary scope *)
	let env' = { env with scope = {env.scope with variables = []}} in 
	List.iter (fun v_dec -> add_layout_mems v_dec env') mems;  
	let (e2, typ2) = check_expr e2 env' in
	match e2 with
	(* Can be either a member name, or a numeric value *)
	S_Id(x, _) -> S_Ref(e1, r, e2, typ2), typ2
|   _ -> if typ2 = Int then
			S_Ref(e1, r, e2, typ2), typ2
		 else
		 	raise (Error("Layout reference must either be an element name or an index"))

and add_layout_mems (v: s_var_decl) (env: translation_environment) =
	match v with
		S_ListDecl(d,e) ->  (match e with
									S_Id(x,_) -> env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
								|  _ -> raise (Error ("Should not be reachable")))
	|   S_LayoutDecl(d,e) -> (match e with
									S_Id(x,_) -> env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
								|  _ -> raise (Error ("Should not be reachable")))
	|   S_BasicDecl(d,e) -> (match e with
									S_Id(x,_) -> env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
								|  _ -> raise (Error ("Should not be reachable")))

and check_slice (e1: expr) (e2: expr) (env: translation_environment) = 
	let (e1, typ1) = check_expr e1 env 
	and (e2, typ2) = check_expr e2 env in
	if typ1 = Int && typ2 = Int then
		S_Slice(e1,e2), Int
	else if e1 = S_Noexpr || e2 = S_Noexpr then
		S_Slice(e1,e2), Int
	else
		raise (Error("Slice indexes must be integers"))

and check_layout_lit (typ: dataType) (e_list: expr list) (env: translation_environment) = 
	(* Need to check whether every expr in e_list has the same type as the corresponding types in typ *)
	match typ with
		Layout(name) -> let (_,lay_var) = find_layout env.scope name in
						let lay_d_list = List.map (fun e' -> match e' with 
															S_BasicDecl(t, _) -> t
															|   S_ListDecl(t, _) -> t 
															|   S_LayoutDecl(t, _) -> t) lay_var in
						let lit_list = List.map (fun e' -> check_expr e' env) e_list in
						let lit_e_list = List.map (fun e' -> let (e,_) = e' in e) lit_list in
						let lit_d_list = List.map (fun e' -> let (_,t) = e' in t) lit_list in
						let correct = compare_lists lay_d_list lit_d_list in
						if correct then
							S_LayoutLit(Layout(name), lit_e_list), Layout(name)
						else
							raise (Error("Layout instance is not the correct format"))
	|   _ -> raise (Error("Can only create a layout instance of a layout type"))

and compare_lists l1 l2 = match l1, l2 with
| [], [] -> true
| [], _
| _, [] -> false
| x::xs, y::ys -> x = y && compare_lists xs ys

let rec check_stmt (s: Ast.stmt) (env: translation_environment) = match s with
  Block(ss) ->	let scope' = { parent = Some(env.scope); variables = []; layouts = env.scope.layouts } in 
  				let env' = { env with scope = scope'} in
  				let ss = List.map (fun s -> check_stmt s env') (List.rev ss) in
  				scope'.variables <- List.rev scope'.variables;
  				S_Block(scope', ss)
| Expr(e) -> let (e,t) = check_expr e env in S_Expr(e,t)
| Return(e) -> check_return e env
| If(elif_l, s) ->	let elif_l' = check_cond elif_l env in
					let s_else = check_stmt s env in
					S_If(elif_l', s_else)
| For(e1, e2, s) -> check_for e1 e2 s env
| While(e, s) -> check_while e s env
| VarDeclS(v) -> check_var_decl v env
| LayoutCreation(name, v_list) ->  check_layout_creation name v_list env

and check_layout_creation (name: string) (v_list: var_decl list) (env: translation_environment) =
	(* Need to check whether this layout is already defined *)
	let exist = List.exists (fun (s, _) -> s = name) env.scope.layouts in
	if exist then
		raise (Error("Layout " ^ name ^ " already defined."))
	else
		(* Then need to check that every expr in the var_decl list is an ID expr *)
		let s_v_list = List.map ( fun v' -> 
			let scope' = {env.scope with variables = []} in
	        let env' = {env with scope = scope' } in
			let v' = check_var_decl v' env' in
			let e = (match v' with 
			S_VarDecl(v) -> (match v with
				S_BasicDecl(_, e) -> e
			|   S_ListDecl(_, e) ->  e
			|   S_LayoutDecl(_, e) -> e)) in
			match e with
				S_Id(x, typ) -> (match typ with
					Layout(name) -> S_LayoutDecl(Layout(name), e)
				|	List(typ) -> S_ListDecl(List(typ), e)
				|   _ -> S_BasicDecl(typ, e))
			|	_ -> raise (Error("Layout creation statement must only contain datatype identifier pairs"))) v_list in
		env.scope.layouts <- (name, s_v_list)::env.scope.layouts;
		env.funcs <- { fname = "Write"; 
					ret_type = Void; 
					formals = [S_BasicDecl(String, S_Id("o_file", String)); S_LayoutDecl(Layout(name), S_Id("output_str", Layout(name)))];
					body = [S_Expr(S_Noexpr, Void)];}::env.funcs;
		(* Layouts created are kept track of in the symbol table *)
		S_Expr(S_Noexpr, Void)

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
			raise (Error("Must return compatible type. Expected type " ^ data_type_s env.return_type ^ ", found type " ^ data_type_s t ))
	else
		raise (Error("Cannot return outside of a function"))

(* e1 must be an identifier; e2 must be a list type *)
and check_for (e1: expr) (e2: expr) (s: stmt) (env: translation_environment) = 
	match e1 with
	Id(x) -> let scope' = { env.scope with variables = (Void, x, S_Noexpr)::env.scope.variables } in
		     let env' = { env with scope = scope'} in
			 let (e1, t1) = check_expr e1 env' in
			 let (e2, t2) = check_expr e2 env' in
			 	let t1 = (match t2 with
						 	List(typ) -> typ
						| 	_	-> t2) in
				 env'.scope.variables <- (t1, x, S_Noexpr)::env.scope.variables;
				 match e2 with
				  S_Id(name, typ) -> let (typ,_,exp) = find_variable env'.scope name in
				 	(match typ with 
				 		List(t) -> let s = check_stmt s env' in S_For(S_Id(x, t2), e2, s)
				 	|   _ -> raise (Error("Must iterate over a list type. Type " ^ data_type_s typ ^ " found")))
				| S_ListLit(_,_) -> let s = check_stmt s env' in S_For(S_Id(x, t2), e2, s)
				| S_ListGen(_,_) -> let s = check_stmt s env' in S_For(S_Id(x, t2), e2, s) 
				| _ -> raise (Error("How did you get here?"))
	|	_ -> raise (Error("First arg of for loop must be an identifier"))


and check_while (e: expr) (s: stmt) (env: translation_environment) = 
	(* check expr is boolean valued *)
	let (e,t) = check_expr e env in
	if t = Bool then
		let s' = check_stmt s env in S_While(e,s')
	else
		raise (Error("While expression must be boolean-valued"))

and check_var_decl (v: var_decl) (env: translation_environment) =
	match v with 
	VarDecl(d,e) ->
		(match d with
		List(typ) ->  (match e with
									Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
												env.scope.variables <- (List(typ), x, S_Noexpr)::env.scope.variables;
												S_VarDecl(S_ListDecl(List(typ), S_Id(x, typ)))
								|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
											env.scope.variables <- (List(typ), x, S_Noexpr)::env.scope.variables;
	 										let (e',_) = check_expr e env in
	 											env.scope.variables <- (List(typ), x, e')::env.scope.variables;
												S_VarDecl(S_ListDecl(typ, S_Assign(x, e')))
								|  _ -> raise (Error ("Not a valid assignment")))
	|   Layout(name) -> (match e with
									Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
												env.scope.variables <- (Layout(name), x, S_Noexpr)::env.scope.variables;
												S_VarDecl(S_LayoutDecl(Layout(name), S_Id(x, Layout(name))))
								|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
											env.scope.variables <- (Layout(name), x, S_Noexpr)::env.scope.variables;
	 										let (e',_) = check_expr e env in
	 											env.scope.variables <- (Layout(name), x, e')::env.scope.variables;
												S_VarDecl(S_LayoutDecl(Layout(name), S_Assign(x, e')))
								|  _ -> raise (Error ("Not a valid assignment")))
	| 	Table(typ) -> (match typ with 
							Layout(_) -> (match e with
									Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
												env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
												S_VarDecl(S_BasicDecl(d, S_Id(x, d)))
								|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
											env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
											let (e',_) = check_expr e env in
												S_VarDecl(S_BasicDecl(d, S_Assign(x, e'))))
						| _ -> raise (Error("Table record layout must have type layout")))
	|   _ -> (match e with
									Id(x) -> let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
												env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
												S_VarDecl(S_BasicDecl(d, S_Id(x, d)))
								|   Assign(x,e) ->  let exist = List.exists (fun (_, s, _) -> s = x) env.scope.variables in
											if exist then
												raise (Error("Identifier already declared"))
											else
											env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
											let (e',_) = check_expr e env in
												S_VarDecl(S_BasicDecl(d, S_Assign(x, e')))
								|  _ -> raise (Error ("Not a valid assignment"))))

let check_formals (decl: var_decl) (env: translation_environment) =
	match decl with 
	VarDecl(d,e) -> 
	( 	match d with
		List(typ) -> (match e with
								Id(x) -> env.scope.variables <- (typ, x, S_Noexpr)::env.scope.variables;
										 S_ListDecl(typ, S_Id(x, typ))
							|   _ -> raise (Error("Function formals must be identifiers")))
	|	Layout(name) -> (match e with
								Id(x) -> env.scope.variables <- (Layout(name), x, S_Noexpr)::env.scope.variables;
										 S_LayoutDecl(Layout(name), S_Id(x, Layout(name)))
							|   _ -> raise (Error("Function formals must be identifiers"))
						)
	|  _ -> (match e with
								Id(x) -> env.scope.variables <- (d, x, S_Noexpr)::env.scope.variables;
										 S_BasicDecl(d, S_Id(x, d))
							|   _ -> raise (Error("Function formals must be identifiers"))))

let check_fdecl (func: Ast.func_decl) (env: translation_environment) : (s_func_decl) = 
	if env.in_func then
		raise (Error ("Cannot nest function declarations"))
	else
		let env' = { funcs = env.funcs; scope = {parent = Some(env.scope); variables = [(String, "stdout", S_Noexpr); (String, "stderr", S_Noexpr)]; layouts=env.scope.layouts}; 
		return_type = func.ret_type; in_func = true} in 
		let formals = (List.rev (List.map (fun x -> check_formals x env') func.formals)) in
		let f = { Sast.fname = func.fname; Sast.ret_type = func.ret_type; Sast.formals = formals; Sast.body = (List.map (fun x -> check_stmt x env') func.body );} in
		env.funcs <- f::env.funcs; f

(* Need to initialize reserved keywords and built-in funcs *)
let init_env : (translation_environment) =
	let func_i = [{ fname = "Write"; 
					ret_type = Void; 
					formals = [S_BasicDecl(String, S_Id("o_file", String)); S_BasicDecl(String, S_Id("output_str", String))];
					body = [S_Expr(S_Noexpr, Void)];};
					{ fname = "Write"; 
					ret_type = Void; 
					formals = [S_BasicDecl(String, S_Id("o_file", String)); S_BasicDecl(Int, S_Id("output_str", Int))];
					body = [S_Expr(S_Noexpr, Void)];};
					{ fname = "Write"; 
					ret_type = Void; 
					formals = [S_BasicDecl(String, S_Id("o_file", String)); S_BasicDecl(Float, S_Id("output_str", Float))];
					body = [S_Expr(S_Noexpr, Void)];};
					{ fname = "Write"; 
					ret_type = Void; 
					formals = [S_BasicDecl(String, S_Id("o_file", String)); S_BasicDecl(Bool, S_Id("output_str", Bool))];
					body = [S_Expr(S_Noexpr, Void)];};
				  { fname = "Read";
				  	ret_type = Void;
				  	formals = [S_BasicDecl(String, S_Id("in_file", String));S_BasicDecl(String, S_Id("delim", String))];
				  	body = [S_Expr(S_Noexpr, Void)]; }; ] in
	let scope_i = { parent = None; 
				   	variables = [(String, "stdout", S_Noexpr); (String, "stderr", S_Noexpr)]; layouts=[]} in 
	{ funcs = func_i ; scope = scope_i; return_type = Void; in_func = false } 			   	

let check_prgm (prog: Ast.program ) : (Sast.s_program) = 
	let env = init_env in
	let layout_stmts = List.filter (fun stmt -> match stmt with LayoutCreation(_,_) -> true | _ -> false) prog.stmts in
	let rest_stmts = List.filter (fun stmt -> match stmt with LayoutCreation(_,_) -> false | _ -> true) prog.stmts in
	let layout_stmts = List.map (fun x -> check_stmt x env) (List.rev layout_stmts) in
	let env = {env with scope = {env.scope with variables = [(String, "stdout", S_Noexpr); (String, "stderr", S_Noexpr)]}} in
	{ Sast.stmts = layout_stmts@(List.map (fun x -> check_stmt x env) (List.rev rest_stmts) );  Sast.funcs = (List.map (fun x -> check_fdecl x env) prog.funcs); Sast.syms = env.scope} 

