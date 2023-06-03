%{

#include "parser.tab.h"

extern int yylineno;

int get_line_number()
{
	return yylineno;
}

char *match(char *word)
{
	if (strcmp(word, "TK_OC_LE") == 0)
		return "'<='";
	if (strcmp(word, "TK_OC_GE") == 0)
		return "'>='";
	if (strcmp(word, "TK_OC_EQ") == 0)
		return "'=='";
	if (strcmp(word, "TK_OC_NE") == 0)
		return "'!='";
	if (strcmp(word, "TK_OC_AND") == 0)
		return "'&&'";
	if (strcmp(word, "TK_OC_OR") == 0)
		return "'||'";
	return word;
}

void yyerror(const char *msg)
{
	char *word;
	word = strtok(msg, " ");
	while (word != NULL)
	{
		printf("%s ", match(word));
		word = strtok(NULL, " ");
	}
	printf("at line %d\n", get_line_number());
}

%}

%option yylineno
%option noyywrap

lower_char [a-z]
digit [[:digit:]]
space [ \t\n\r\s]
special [\-\!\*\/\%\+\-\<\>\{\}\(\)\=\,\;]

multiliner_start "/*"

%x MULTILINER

%%

{multiliner_start} { BEGIN(MULTILINER); }
<MULTILINER>"*/" { BEGIN(INITIAL); }
<MULTILINER>\n   { }
<MULTILINER>.    { }

(\/\/).* {}

{space} {}

"int" { return TK_PR_INT; }
"float" { return TK_PR_FLOAT; }
"bool" { return TK_PR_BOOL; }
"if" { return TK_PR_IF; }
"else" { return TK_PR_ELSE; }
"while" { return TK_PR_WHILE; }
"return" { return TK_PR_RETURN; }

"<=" { return TK_OC_LE; }
">=" { return TK_OC_GE; }
"==" { return TK_OC_EQ; }
"!=" { return TK_OC_NE; }
"&" { return TK_OC_AND; }
"|" { return TK_OC_OR; }
"->" { return TK_OC_MAP; }

"false" { return TK_LIT_FALSE; }
"true" { return TK_LIT_TRUE; }
{digit}+ { return TK_LIT_INT; }
{digit}*\.{digit}+ { return TK_LIT_FLOAT; }

({lower_char}|_)+ { return TK_IDENTIFICADOR; }

{special} { return yytext[0]; }

. { return TK_ERRO; }

%%
