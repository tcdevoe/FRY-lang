%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token BAR SEMI COMMA PLUS MINUS TIMES
%token DIVIDE ASSIGN EQ NEQ LT LEQ
%token GT GEQ INT STRING FLOAT BOOL
%token LAYOUT LIST TABLE IN NOT FROM
%token IF ELSE ELIF AND OR CONT BREAK
%token INC DEC PERIOD COLON
%token <int> INT_LIT
%token <string> FLOAT_LIT BOOL_LIT STRING_LIT ID
%token EOF

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

program:
    /* nothing */ { [], [] }
|   expr list

expr:
    STRING_LIT      { StringLit($1) }
|   FLOAT_LIT       { FloatLit($1) }
|   INT_LIT         { IntLit($1) }
|   BOOL_LIT        { BoolLit($1) }
|   ID              { Id($1) }
|   expr PLUS expr  { Binop($1, Add, $3) }
|   expr MINUS expr { Binop($1, Sub, $3) }
|   expr TIMES expr { Binop($1, Mult,$3) }
|   expr DIVIDE expr { Binop($1,Div, $3) } 
|   expr EQ expr    { Binop($1, Equal, $3) } 
|   expr NEQ expr   { Binop($1, Neq, $3) }
|   expr LT expr    { Binop($1, Less, $3) }
|   expr LEQ expr   { Binop($1, Leq, $3) }
|   expr GT expr    { Binop($1, Greater, $3) }
|   expr GEQ expr   { Binop($1, Geq, $3) }
|   expr IN expr    { Binop($1, In, $3) }
|   expr NOT IN expr { Binop($1, Notin, $3) }
|   expr AND expr   { Binop($1, And, $3) }
|   expr OR expr    { Binop($1, Or, $3) }
|   expr FROM expr  { Binop($1, From, $3) }
|   expr INC        { Postop($1, Inc) }
|   expr DEC        { Postop($1, Dec) }
|   NOT expr        { Preop{$2,Not}     }
|   expr LBRACK expr slice_opt RBRACK { Ref($1, ListRef, $3, $4) }
|   expr PERIOD LBRACE expr slice_opt RBRACE { Ref($1, LayRef, $3, $4) }
|   ID ASSIGN expr { Assign($1, $3) }
|   ID LPAREN actuals_opt RPAREN { Call($1, $3) }
|   LPAREN expr RPAREN      { $2 }

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

