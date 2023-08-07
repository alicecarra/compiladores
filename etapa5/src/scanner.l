%%

(?s)/\*.*?\*/ ;

(?-s)//.* ;
[ \t\n\r\s] ;

int "TK_PR_INT"
float "TK_PR_FLOAT"
bool "TK_PR_BOOL"
if "TK_PR_IF"
else "TK_PR_ELSE"
while "TK_PR_WHILE"
return "TK_PR_RETURN"

\<\= "TK_OC_LE"
>= "TK_OC_GE"
== "TK_OC_EQ"
!= "TK_OC_NE"
&  "TK_OC_AND"
\| "TK_OC_OR"
-> "TK_OC_MAP"

false "TK_LIT_FALSE"
true "TK_LIT_TRUE"
[0-9]+ "TK_LIT_INT"
[0-9]*\.[0-9]+ "TK_LIT_FLOAT"

([a-z]|_)+ "TK_IDENTIFICADOR"

[\,] ","
[\;] ";"
[\=] "="
[\(] "("
[\)] ")"
[\{] "{"
[\}] "}"
[\-] "-"
[\!] "!"
[\*] "*"
[\/] "/"
[\%] "%"
[\+] "+"
[\<] "<"
[\>] ">"

. "TK_ERRO"


