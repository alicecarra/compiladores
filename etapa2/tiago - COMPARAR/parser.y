%{
int yylex(void);
void yyerror (char const *s);
%}

%token TK_PR_INT
%token TK_PR_FLOAT
%token TK_PR_BOOL
%token TK_PR_IF
%token TK_PR_ELSE
%token TK_PR_WHILE
%token TK_PR_RETURN
%token TK_OC_LE
%token TK_OC_GE
%token TK_OC_EQ
%token TK_OC_NE
%token TK_OC_AND
%token TK_OC_OR
%token TK_OC_MAP
%token TK_IDENTIFICADOR
%token TK_LIT_INT
%token TK_LIT_FLOAT
%token TK_LIT_FALSE
%token TK_LIT_TRUE
%token TK_ERRO

%%

program: item_list |;
item_list : item_list function | item_list global_variable | function | global_variable;

global_variable : type name_list ';' ; 
name_list : TK_IDENTIFICADOR ',' name_list | TK_IDENTIFICADOR;

function :  function_header function_body;
function_header: TK_IDENTIFICADOR parameters TK_OC_MAP type ;

parameters: '(' parameters_list ')' | '(' ')';
parameters_list: type TK_IDENTIFICADOR ',' parameters_list | type TK_IDENTIFICADOR;

function_body: '{' command_block '}' | '{' '}';
command_block: command_block command | command;
command: variable ';' | assignment ';' | function_call ';' | return ';' | flow_ctrl | command_block ';' ;

variable : type name_with_value_list ; 
name_with_value_list: TK_IDENTIFICADOR  TK_OC_LE literal ',' name_with_value_list | TK_IDENTIFICADOR ',' name_with_value_list | TK_IDENTIFICADOR  TK_OC_LE literal  | TK_IDENTIFICADOR;

assignment: TK_IDENTIFICADOR '=' expression;

function_call : TK_IDENTIFICADOR arguments;
arguments: '(' arguments_list ')' | '(' ')';
arguments_list: expression ',' arguments_list | expression;

return : TK_PR_RETURN expression;

flow_ctrl : if | while;

if: TK_PR_IF '(' expression ')' '{' command_block '}' |  TK_PR_IF '(' expression ')' '{' command_block '}' TK_PR_ELSE '{' command_block '}' ;
while: TK_PR_WHILE  '(' expression ')' '{' command_block '}';

expression : expression TK_OC_OR expression2 | expression2;
expression2: expression2 TK_OC_AND expression3 | expression3;
expression3: expression3 TK_OC_NE expression4 | expression3 TK_OC_EQ expression4 | expression4;
expression4: expression4 '<' expression5 | expression4 '>' expression5 | expression4 TK_OC_LE expression5 | expression4 TK_OC_GE expression5 | expression5;
expression5: expression5 '+' expression6 | expression5 '-' expression6 | expression6;
expression6: expression6 '*' expression7 | expression6 '/' expression7 | expression6 '%' expression7 | expression7;
expression7: '!' expression8 | '-' expression8 | expression8 ;
expression8: operand | '(' expression ')';

operand : TK_IDENTIFICADOR | literal | function;
operators : "-" | "!" | "+" | "*" | "/" | "\%" | TK_OC_AND | TK_OC_OR | TK_OC_EQ | TK_OC_NE | TK_OC_LE | TK_OC_GE ;

literal : TK_LIT_FALSE | TK_LIT_TRUE | TK_LIT_FLOAT | TK_LIT_INT;
type: TK_PR_BOOL | TK_PR_FLOAT | TK_PR_INT;

%%
