%{
  #include "mini_l.tab.h"
  #include <string>
  using namespace std;
  extern int yyerror(char *s);
  int currLine, currPos = 1;
  char * file;
%}

DIGIT [0-9]
LETTER [a-zA-Z]

%%
"function"        {currPos += yyleng; return FUNCTION;}
"beginparams"     {currPos += yyleng; return BEGIN_PARAMS;}
"endparams"       {currPos += yyleng; return END_PARAMS;}
"beginlocals"     {currPos += yyleng; return BEGIN_LOCALS;}
"endlocals"       {currPos += yyleng; return END_LOCALS;}
"beginbody"       {currPos += yyleng; return BEGIN_BODY;}
"endbody"         {currPos += yyleng; return END_BODY;}
"integer"         {currPos += yyleng; return INTEGER;}
"array"           {currPos += yyleng; return ARRAY;}
"of"              {currPos += yyleng; return OF;}
"if"              {currPos += yyleng; return IF;}
"then"            {currPos += yyleng; return THEN;}
"endif"           {currPos += yyleng; return ENDIF;}
"else"            {currPos += yyleng; return ELSE;}
"while"           {currPos += yyleng; return WHILE;}
"do"              {currPos += yyleng; return DO;}
"for"             {currPos += yyleng; return FOR;}
"beginloop"       {currPos += yyleng; return BEGINLOOP;}
"endloop"         {currPos += yyleng; return ENDLOOP;}
"continue"        {currPos += yyleng; return CONTINUE;}
"read"            {currPos += yyleng; return READ;}
"write"           {currPos += yyleng; return WRITE;}
"and"             {currPos += yyleng; return AND;}
"or"              {currPos += yyleng; return OR;}
"not"             {currPos += yyleng; return NOT;}
"true"            {currPos += yyleng; return TRUE;}
"false"           {currPos += yyleng; return FALSE;}
"return"          {currPos += yyleng; return RETURN;}

"-"               {currPos += yyleng; return SUB;}
"+"               {currPos += yyleng; return ADD;}
"*"               {currPos += yyleng; return MULT;}
"/"               {currPos += yyleng; return DIV;}
"%"               {currPos += yyleng; return MOD;}

"=="              {yylval.str_val = strdup(yytext); currPos += yyleng; return EQ;}
"<>"              {yylval.str_val = strdup(yytext); currPos += yyleng; return NEQ;}
"<"               {yylval.str_val = strdup(yytext); currPos += yyleng; return LT;}
">"               {yylval.str_val = strdup(yytext); currPos += yyleng; return GT;}
"<="              {yylval.str_val = strdup(yytext); currPos += yyleng; return LTE;}
">="              {yylval.str_val = strdup(yytext); currPos += yyleng; return GTE;}

";"               {currPos += yyleng; return SEMICOLON;}
":"               {currPos += yyleng; return COLON;}
","               {currPos += yyleng; return COMMA;}
"("               {currPos += yyleng; return L_PAREN;}
")"               {currPos += yyleng; return R_PAREN;}
"["               {currPos += yyleng; return L_SQUARE_BRACKET;}
"]"               {currPos += yyleng; return R_SQUARE_BRACKET;}
":="              {currPos += yyleng; return ASSIGN;}

{DIGIT}+ {yylval.int_val = atoi(yytext); currPos += yyleng; return NUMBER;}

("_"[a-zA-Z0-9_]*)|({DIGIT}[a-zA-Z0-9_]*) {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(1);}

([a-zA-Z0-9_]*"_") {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(1);}

({LETTER}+[a-zA-Z0-9_]*) {yylval.str_val = strdup(yytext); currPos += yyleng; return IDENT;}

[ \t]+ {/*ignore : whitespace */ currPos += yyleng;}

"##".* {/*ignore : comments */ currLine++;currPos += yyleng;}

"\n" {currLine++; currPos = 1;}

. { printf("Error in parser"); exit(0);}

%%

int main(int argc, char **argv) {
  if (argc > 1) {
    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
      yyin = stdin;
    }
  }
  else {
    yyin = stdin;
  }

  yyparse();
  return 0;
}
