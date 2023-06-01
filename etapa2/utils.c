#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern int yylineno;
int get_line_number() {
	return yylineno;
}

void yyerror(const char *msg) {
	printf("error: %c\n", msg);
}