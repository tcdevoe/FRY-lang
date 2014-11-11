type action = Raw | Ast | Compile

let _ =
	let action = 
		if Array.length Sys.argv > 1 then
			List.assoc Sys.argv.(1) [ ("-r", Raw);
									  ("-a", Ast);
									  ("-c", Compile)]
		else Compile in
		let lexbuf = Lexing.from_channel stdin in
		let program = Parser.program Scanner.token lexbuf in 
		match action with
		Raw -> print_string (Ast.program_s program)
	 |  Ast -> print_string "Add In"
	 |  Compile -> print_string "Add in"