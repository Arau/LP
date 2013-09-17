#header 
<< #include "charptr.h" >>

<<
#include "charptr.c"

int main() {
   ANTLR(expr(), stdin);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token SPACE "[\ \n]" << zzskip(); >>

//expr: NUM (PLUS NUM)* "@" ;
//expr: expr PLUS expr | NUM; //  error: infinite left-recursion to rule expr from rule expr
//expr: NUM PLUS expr | NUM;//  warning: alts 1 and 2 of the rule itself ambiguous upon { NUM }
//expr: expr PLUS NUM  | NUM;// error: infinite left-recursion to rule expr from rule expr
//expr: NUM | NUM PLUS expr;//  warning: alts 1 and 2 of the rule itself ambiguous upon { NUM }

expr: NUM ((PLUS|MINUS) NUM)* "@" ;
