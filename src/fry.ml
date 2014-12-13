open Printf
type action = Raw | Ast | Compile

let _ =
	let action = 
		if Array.length Sys.argv > 1 then
			List.assoc Sys.argv.(1) [ ("-r", Raw);
									  ("-c", Compile)]
		else Compile in
		let lexbuf = Lexing.from_channel stdin in
		let program = Parser.program Scanner.token lexbuf in 
		match action with
		Raw -> print_string (Ast.program_s program)
	 |  Compile ->
	 			let checked_program = SemanticAnalysis.check_prgm program in 
	 			let compiled_program = Javagen.j_prgm checked_program
	 				in let file = open_out ("fry.java") in
   						fprintf file "%s"  compiled_program; 
   						print_endline "Program compiled to fry.java"