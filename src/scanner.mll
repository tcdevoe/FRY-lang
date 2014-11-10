{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

rule token = parse
    [' ' '\t' '\r' ] { token lexbuf } (* Whitespace *)
|   [^'\\']'\n'    { NEWLINE }
| 	"/#"    { comment lexbuf }
|   '#'     { sl_comment lexbuf }
| 	'('     { LPAREN }
| 	')'     { RPAREN }
| 	'{'     { LBRACE }
| 	'}'     { RBRACE }
|	']'		{ LBRACK }
|	'['		{ RBRACK }
|	'|'		{ BAR }
| 	';'     { SEMI }
|   ':'     { COLON }
| 	','     { COMMA }
| 	'+'     { PLUS }
| 	'-'     { MINUS }
| 	'*'     { TIMES }        
| 	'/'     { DIVIDE }
| 	'='     { ASSIGN }
|   '.'     { PERIOD }
|   "++"    { INC }
|   "--"    { DEC }
|	"=="    { EQ }
|	"!="    { NEQ }
|	'<'     { LT }
|	"<="    { LEQ }
|	">"     { GT }
|	">="    { GEQ } 
|	"int"   { INT }
|	"str"   { STRING }
|	"float" { FLOAT }
|	"bool"  { BOOL }
| "Layout"	{ LAYOUT }
|	"List"	{ LIST }
|	"Table" { TABLE }
|	"in"	{ IN }
|   "if"    { IF }
|   "else"  { ELSE }
|   "elif"  { ELIF }
|   "and"   { AND  }
|   "or"    { OR }
|	"for"	{ FOR }
|	"while" { WHILE }
|   "continue"  { CONT }
|   "break"     { BREAK }
|   "not"   { NOT }
|   "<-"    { FROM }
|	"ret"	{ RETURN }
|   (digit)+ as lxm { INT_LIT(int_of_string lxm) }
|   ((digit)+'.'|(digit)*'.'(digit)+) as lxm { FLOAT_LIT(lxm) }
|   '"'(("\""|[^'"'])* as lxm)'"' { STRING_LIT(lxm) }
|   "true" | "false" as lxm { BOOL_LIT(lxm) }
|   identifier as lxm { ID(lxm) }
|   _ as char { raise (Failure("Illegal Character found :" ^ Char.escaped char)) }


and comment = parse
    "#/" { token lexbuf }
|    _   { comment lexbuf }

and sl_comment = parse
    "\n" { token lexbuf}
|   _    { sl_comment lexbuf }
