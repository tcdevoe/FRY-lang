{ open Parser }

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| 	"/#"    { comment lexbuf }
| 	'('     { LPAREN }
| 	')'     { RPAREN }
| 	'{'     { LBRACE }
| 	'}'     { RBRACE }
|	']'		{ LBRACK }
|	'['		{ RBRACK }
|	'|'		{ BAR }
| 	';'     { SEMI }
| 	','     { COMMA }
| 	'+'     { PLUS }
| 	'-'     { MINUS }
| 	'*'     { TIMES }        
| 	'/'     { DIVIDE }
| 	'='     { ASSIGN }
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
| 	"Sort"	{ SORT }
