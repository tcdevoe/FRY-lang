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
	 |  Compile -> (* Need to add in Syntax Checking before Javagen *)
	 			let checked_program = Sem_analysis.check_prgm program in 
	 			let compiled_program = Javagen.j_prgm checked_program
	 				in print_endline compiled_program