%{
    #include <stdio.h>
    #include "tokens.h"
    static int line_num = 1;
    extern int yylineno;

    int get_line_number(void){
        return yylineno;
    }
%}
%option yylineno
%x MULTILINER

ignore [ \t\n\r\s]
digit [0-9]
integer {digit}+
floating ({integer})?("."{integer})
lower_char [a-z]
lower_word {lower_char}+
identifier {lower_word}|("_")
upper_char [A-Z]
upper_word {lower_char}+
all_char  {lower_char|upper_char}
all_word {all_char}+
one_liner_marker "\/\/"
multiliner_start "/*"
multiliner_end "*/"
any .
one_liner {one_liner_marker}{any}*

%%


{multiliner_start} { BEGIN(MULTILINER); }
<MULTILINER>"*/" { BEGIN(INITIAL); }
<MULTILINER>\n   { }
<MULTILINER>.    { }

{ignore} { }
{one_liner} { }
"-" { return '-';}
"!" { return '!';}
"*" { return '*';}
"/" { return '/';}
"\%" { return '%';}
"+" { return '+';}
"_" { return '_';}
"<" { return '<';}
">" { return '>';}
"{" { return '{';}
"}" { return '}';}
"(" { return '(';}
")" { return ')';}
"=" { return '=';}
"," { return ',';}
";" { return ';';}

"int" { return TK_PR_INT;}
"float" { return TK_PR_FLOAT;}
"bool" { return TK_PR_BOOL;}
"if" { return TK_PR_IF;}
"else" { return TK_PR_ELSE;}
"while" { return TK_PR_WHILE;}
"return" { return TK_PR_RETURN;}
"false" {return TK_LIT_FALSE;}
"true" {return TK_LIT_TRUE;}

"<=" { return TK_OC_LE;}
">=" { return TK_OC_GE;}
"==" { return TK_OC_EQ;}
"!=" { return TK_OC_NE;}
"&" { return TK_OC_AND;}
"|" { return TK_OC_OR;}
"->" { return TK_OC_MAP;}

{identifier} { return TK_IDENTIFICADOR;}

{integer} {return TK_LIT_INT;}
{floating} {return TK_LIT_FLOAT;}
{any} {return TK_ERRO;}

%%

