#include <string.h> 
#include <stdio.h>

extern int yylineno;

int getLineNumber(){
	return yylineno;
}

char *translateToTk(char *word){
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
	if (strcmp(word, "TK_OC_LE,") == 0)
		return "'<=',";
	if (strcmp(word, "TK_OC_GE,") == 0)
		return "'>=',";
	if (strcmp(word, "TK_OC_EQ,") == 0)
		return "'==',";
	if (strcmp(word, "TK_OC_NE,") == 0)
		return "'!=',";
	if (strcmp(word, "TK_OC_AND,") == 0)
		return "'&&',";
	if (strcmp(word, "TK_OC_OR,") == 0)
		return "'||',";
	return word;
}

void yyerror(char *msg){
	char *word;
	word = strtok(msg, " ");
	while (word != NULL)
	{	
		//printf("[%s]",word);
		printf("%s ", translateToTk(word));
		word = strtok(NULL, " ");
	}
	printf("at line %d\n", getLineNumber());
}