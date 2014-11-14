%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token BAR SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ INT STRING FLOAT BOOL
%token LAYOUT LIST TABLE IN NOT FROM
%token IF ELSE ELIF AND OR CONT BREAK
%token INC DEC PERIOD COLON
%token RETURN FOR WHILE
%token <int> INT_LIT
%token <string> FLOAT_LIT BOOL_LIT STRING_LIT ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
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

postfix_expr:
	primary_expr { $1 }
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

expr:
    ID LPAREN actuals_opt RPAREN { Call($1, $3) }
|	assign_expr		{ $1 }

/***************************
		DECLARATIONS
***************************/

type_spec:
	STRING 			{ String }
|	INT 			{ Int }
|	FLOAT 			{ Float }
| 	BOOL 			{ Bool }
|	LIST 			{ List }
|	LAYOUT			{ Layout }
| 	TABLE 			{ Table }

declarator:
	ID 		{ Assign($1, Noexpr) }
|	ID ASSIGN expr  { Assign($1, $3) }

vdecl:
    type_spec declarator SEMI { {data_type=$1; decl=$2} }

program:
    /* nothing */ { { stmts = []; vars = []; funcs = [] } }
    /* List is built backwards */
|   program vdecl { { stmts = $1.stmts; vars = $2::$1.vars; funcs = $1.funcs } }
|   program fdecl { { stmts = $1.stmts; vars = $1.vars; funcs = $2::$1.funcs } } 
|	program stmt  { { stmts = $2::$1.stmts; vars = $1.vars; funcs = $1.funcs } }



vdecl_list:
	/* nothing */ { [] }
|	vdecl_list vdecl { $2::$1 }

fdecl:
    type_spec ID LPAREN param_list RPAREN LBRACE vdecl_list stmt_list RBRACE
    	{{ 
    		fname = $2;
    		ret_type = $1;
    		formals = $4;
    		locals = List.rev $7;
    		body = List.rev $8;
    	}}

param_list:
	/* nothing */ { [] }
|	type_spec ID { [{data_type = $1; decl = Id($2); }] }
|	param_list COMMA type_spec ID { {data_type = $3; decl = Id($4); }::$1}

/* TODO: Add in jump-statements (break/continue) */
stmt:
	expr SEMI { Expr($1) }
|	RETURN expr SEMI { Return($2) }
|	LBRACE stmt_list RBRACE	{ Block(List.rev $2) }
|	IF LPAREN expr RPAREN stmt elif_list NOELSE { If(($3,$5)::$6, Block([])) }
|	IF LPAREN expr RPAREN stmt elif_list ELSE stmt { If(($3,$5)::$6, $8) }
|	FOR expr stmt { For($2, $3) }
|	WHILE expr stmt { While($2, $3) }
|	LBRACK expr BAR expr SEMI expr RBRACK { SetBuild($2,$4,$6) }

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
