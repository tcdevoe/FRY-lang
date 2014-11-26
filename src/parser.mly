%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token BAR SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ INT STRING FLOAT BOOL
%token LAYOUT LIST TABLE IN NOT FROM
%token IF ELSE ELIF AND OR CONT BREAK
%token INC DEC PERIOD COLON
%token RETURN FOR WHILE TO
%token <int> INT_LIT
%token <string> FLOAT_LIT BOOL_LIT STRING_LIT ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left LBRACK
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left IN NOT_IN
%left PLUS MINUS
%left TIMES DIVIDE 
%right NOT
%left INC DEC

%start program
%type <Ast.program> program

%%

/********************
	EXPRESSIONS
*********************/

literal:
    STRING_LIT      { StringLit($1) } 
|   FLOAT_LIT       { FloatLit($1) }
|   INT_LIT         { IntLit($1) }
|   BOOL_LIT        { BoolLit($1) }

primary_expr:
|   ID              { Id($1) }
|	literal 		{ $1 }
|	LPAREN expr RPAREN { $2 }

set_build_expr:
	primary_expr	{ $1 }
|	LBRACK expr BAR ID FROM set_build_expr SEMI expr RBRACK { SetBuild($2,$4,$6,$8) }

postfix_expr:
	set_build_expr { $1 }
|   postfix_expr INC        { Postop($1, Inc) }
|   postfix_expr DEC        { Postop($1, Dec) }
|   postfix_expr LBRACK expr slice_opt RBRACK { Ref($1, ListRef, $3, $4) }
|   postfix_expr PERIOD LBRACE expr slice_opt RBRACE { Ref($1, LayRef, $4, $5) }


prefix_expr:
    postfix_expr { $1 }
|   NOT prefix_expr      { Preop(Not, $2) }

multi_expr:
	prefix_expr		{ $1 }
|	multi_expr TIMES multi_expr { Binop($1, Mult,$3) }
|   multi_expr DIVIDE multi_expr { Binop($1, Div, $3) } 

add_expr:
	multi_expr { $1 }
|   add_expr PLUS add_expr  { Binop($1, Add, $3) }
|   add_expr MINUS add_expr { Binop($1, Sub, $3) }

cont_expr:
    add_expr IN cont_expr    { Binop($1, In, $3) }
|   add_expr NOT IN cont_expr { Binop($1, Notin, $4) }

relational_expr:
	add_expr		{ $1 }
|   relational_expr LT relational_expr    { Binop($1, Less, $3) }
|   relational_expr LEQ relational_expr   { Binop($1, Leq, $3) }
|   relational_expr GT relational_expr    { Binop($1, Greater, $3) }
|   relational_expr GEQ relational_expr   { Binop($1, Geq, $3) }

equality_expr:
	relational_expr { $1 }
|   equality_expr EQ equality_expr    { Binop($1, Equal, $3) } 
|   equality_expr NEQ equality_expr   { Binop($1, Neq, $3) }

logical_AND_expr:
	equality_expr { $1 }
|	logical_AND_expr AND logical_AND_expr   { Binop($1, And, $3) }

logical_OR_expr:
	logical_AND_expr { $1 }
|   expr OR expr    { Binop($1, Or, $3) }

assign_expr:
	logical_OR_expr { $1 }
|   ID ASSIGN expr { Assign($1, $3) }

func_call:
	assign_expr { $1 }
| 	ID LPAREN actuals_opt RPAREN { Call($1, $3) }

list_initializer:
	func_call  { $1 }
|	LBRACK list_initializer_list RBRACK { ListLit($2) }
|	LBRACK func_call TO func_call RBRACK { ListGen($2, $4) }

list_initializer_list:
	func_call 	{ [$1] }
|	list_initializer_list COMMA func_call { $3::$1 } 

expr:
	list_initializer	{ $1 }



/***************************
		DECLARATIONS
***************************/

type_spec:
	STRING 			{ String }
|	INT 			{ Int }
|	FLOAT 			{ Float }
| 	BOOL 			{ Bool }
|	LAYOUT			{ Layout }
| 	TABLE 			{ Table }

declarator:
	ID 		{ Id($1) }
|	ID ASSIGN expr  { Assign($1, $3) }

vdecl:
    type_spec declarator SEMI { BasicDecl($1, $2) }
|	type_spec LIST declarator SEMI { ListDecl($1, $3) }

program:
    /* nothing */ { { stmts = []; funcs = [] } }
    /* List is built backwards */
|   program fdecl { { stmts = $1.stmts; funcs = $2::$1.funcs } } 
|	program stmt  { { stmts = $2::$1.stmts; funcs = $1.funcs } }

fdecl:
    type_spec ID LPAREN param_list RPAREN LBRACE stmt_list RBRACE
    	{{ 
    		fname = $2;
    		ret_type = $1;
    		formals = $4;
    		body = List.rev $7;
    	}}
| 	type_spec LIST ID LPAREN param_list RPAREN LBRACE stmt_list RBRACE
    	{{ 
    		fname = $3;
    		ret_type = List($1);
    		formals = $5;
    		body = List.rev $8;
    	}}

param_list:
	/* nothing */ { [] }
|	param_list COMMA type_spec ID { BasicDecl($3, Id($4))::$1}
|	param_list COMMA type_spec LIST ID { ListDecl($3, Id($5))::$1 }

/* TODO: Add in jump-statements (break/continue) */
stmt:
	expr SEMI { Expr($1) }
|	RETURN expr SEMI { Return($2) }
|	LBRACE stmt_list RBRACE	{ Block(List.rev $2) }
|	conditional_stmt		{ $1 }
|	FOR LPAREN expr FROM expr RPAREN stmt { For($3, $5, $7) }
|	WHILE expr stmt { While($2, $3) }
|	vdecl 		  { VarDecl($1) }

conditional_stmt:
	IF LPAREN expr RPAREN stmt elif_list %prec NOELSE { If(($3,$5)::$6, Block([])) }
|	IF LPAREN expr RPAREN stmt elif_list ELSE stmt { If(($3,$5)::$6, $8) }

stmt_list:
	/* nothing */ { [] }
|	stmt_list stmt { $2::$1 }

elif_list:
	/* nothing */ { [] }
|	ELIF LPAREN expr RPAREN stmt { [$3, $5] } /* make sure list is being created correctly */
|	elif_list ELIF LPAREN expr RPAREN stmt { ($4, $6)::$1 }

expr_opt:
    /* nothing */ { Noexpr }
|   expr          { $1 }

slice_opt:
    /* nothing */ { Noexpr }
|   COLON expr    { $2 }

actuals_opt:
    /* nothing */ { [] }
|   actuals_list  { List.rev $1 }

actuals_list:
    expr                        { [$1] }
|   actuals_list COMMA expr     { $3 :: $1 }
