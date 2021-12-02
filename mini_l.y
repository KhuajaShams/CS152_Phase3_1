%code requires{
  using namespace std;
  #include<string>
  
  struct n_Terminal {
    string code;
    string ret_name;
    bool isArray;
    string var;
    string index;
  };
}

%{
  #include "stdio.h"
  #include <string>
  #include <vector>
  #include <map>
  #include <iostream>
  #include <sstream>
  using namespace std;
  int yyeror(char *s);
  int yyerror(string s);
  int yylex(void);
  string makeTemp();
  string makeLabel();
  string createVar(char*);
  string findIndex (const string&);
  void replaceString(string&, const string&, const string&);
  bool varDeclared(const vector<string>&, const string&);
  void addLocalVar(const string&);
  void checkDeclared(const string&);
  void addFunction(const string&);
  void checkFuncDeclared(const string&);
  bool isMain = false;
  extern FILE* yyin;

  vector<string> declaredFunc;
  vector<string> declaredVar;
%}
%union {
  int int_val;
  char* str_val;
  n_Terminal* n_term;
}

%error-verbose
%start Program
%token FUNCTION;
%token BEGIN_PARAMS END_PARAMS;
%token BEGIN_LOCALS END_LOCALS;
%token BEGIN_BODY END_BODY;
%token INTEGER ARRAY OF;
%token IF THEN ENDIF ELSE;
%token WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE;
%token READ WRITE;
%token AND OR NOT TRUE FALSE RETURN;

%token SUB ADD MULT DIV MOD;

%token EQ NEQ LT GT LTE GTE;

%token SEMICOLON COLON COMMA;
%token L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET;
%token ASSIGN;

%token<str_val> IDENT;
%token<int_val> NUMBER;
%type<str_val> Comp

%type<n_term> Program
%type<n_term> DeclarationList
%type<n_term> Declaration
%type<n_term> FunctionList
%type<n_term> Function
%type<n_term> Identifier
%type<n_term> FunctionParams
%type<n_term> FunctionLocals
%type<n_term> FunctionBody
%type<n_term> StatementList
%type<n_term> Statement
%type<n_term> IdentifierList
%type<n_term> Var
%type<n_term> VarList
%type<n_term> Expression
%type<n_term> ExpressionList
%type<n_term> BoolExpr
%type<n_term> RelationAndExpr
%type<n_term> RelationExpr
%type<n_term> Relations
%type<n_term> MultiplicativeExpr
%type<n_term> Term
%type<n_term> TermInner

%%
/* Program */
Program: FunctionList
    {
      if (!isMain) {
        yyerror("\"main\" function not definied in program.");
      }
      string s = $1->code;
      cout << s << endl;
    }
  | %empty 
    {
    }
  ;
FunctionList: Function FunctionList
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << $1->code << endl << $2->code;
      $$->code = ss.str();
    }
  | Function
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
    }
  ;

/* Function */
Function: FUNCTION Identifier SEMICOLON FunctionParams FunctionLocals FunctionBody
    {
      $$ = new n_Terminal();
      stringstream ss;

      if ($2->code == "main") {
        isMain = true;
      }

      addFunction($2->code);
      declaredVar.clear();

      ss << "func " << $2->code << endl;
      
      
      if ($4->code.length() > 0) {ss << $4->code << endl;}
      
      if ($5->code.length() > 0) {ss << $5->code << endl;}
      
      if ($6->code.length() > 0) {ss << $6->code << endl;}
      
      ss << "endfunc";
      $$->code = ss.str();
    }
  ;
FunctionParams: BEGIN_PARAMS DeclarationList END_PARAMS
    {
      $$ = new n_Terminal();
      stringstream ss;
      
      ss << $2->code << endl;
      string ident;
      int paramNum = 0;
      for (int i = 0; i < $2->ret_name.length(); i++) {
        if ($2->ret_name[i] == ',') {
          ss << "= " << ident << ", $" << to_string(paramNum) << endl;
          ident = "";
          paramNum++;
          continue;
        }
        ident.push_back($2->ret_name[i]);
      }

      if (ident.length() > 0) {
        ss << "= " << ident << ", $" << to_string(paramNum);
      }


      $$->code = ss.str();
    }
  | BEGIN_PARAMS END_PARAMS
    {
      $$ = new n_Terminal();
    }
  ;
FunctionLocals: BEGIN_LOCALS DeclarationList END_LOCALS
    {
      $$ = new n_Terminal();
      $$->code = $2->code;
    }
  | BEGIN_LOCALS END_LOCALS {
      $$ = new n_Terminal();
    }
  ;
FunctionBody: BEGIN_BODY StatementList END_BODY
    {

      if ($2->code.find("continue") != string::npos) {
        cout << "Error: continue statement not within a loop." << endl;
        exit(1);
      }

      $$ = new n_Terminal();
      $$->code = $2->code;
    }
  | BEGIN_BODY END_BODY
    {
      $$ = new n_Terminal();
    }
  ;
DeclarationList: DeclarationList Declaration SEMICOLON
    {
      $$ = new n_Terminal();
      stringstream ss, slist;

      ss << $1->code << endl << $2->code;
      
      slist << $1->ret_name << "," << $2->ret_name;

      $$->code = ss.str();
      $$->ret_name = slist.str();
    }
  | Declaration SEMICOLON
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = $1->ret_name; 
    }
  ;

/* Declaration */
Declaration: IdentifierList COLON INTEGER
    {
      $$ = new n_Terminal();
      stringstream ss, var;
      string currVar = "";

      for (int i = 0; i < $1->code.length(); i++) {
        if ($1->code.at(i) == ',') {
          ss << ". " << currVar << endl;
          addLocalVar(currVar);
          currVar = "";
        }
        else {
          currVar.push_back($1->code[i]);
        }
      }

      if (currVar.length() > 0) {
        ss << ". " << currVar;
        addLocalVar(currVar);
      }
      
      $$->code = ss.str();
      $$->ret_name = $1->code; 
    }
  | IdentifierList COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
    {
      string buffer;
      if ($5 <= 0) {
        yyerror("array size < 1");
      }
      $$ = new n_Terminal();
      stringstream ss;
      string currVar = "";

      for (int i = 0; i < $1->code.length(); i++) {
        if ($1->code.at(i) == ',') {
          ss << ".[] " << currVar << ", " << to_string($5) << endl;
          addLocalVar(currVar);
          currVar = "";
        }
        else {
          currVar.push_back($1->code[i]);
        }
      }

      if (currVar.length() > 0 ) {
        ss << ".[] " << currVar << ", " << to_string($5);
        addLocalVar(currVar);
      }
      
      $$->code = ss.str();
      $$->ret_name = $1->code;
    }
  ;
IdentifierList: Identifier
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << "_" << $1->code;
      $$->code = ss.str();
    }
  | Identifier COMMA IdentifierList
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << "_" << $1->code << "," << $3->code;
      $$->code = ss.str();
    }
  ;
Identifier: IDENT
    {
      $$ = new n_Terminal();
      $$->code = $1;
    }
  ;

/* Statement */
Statement: Var ASSIGN Expression
    {
      $$ = new n_Terminal();
      stringstream ss;
      string assign;

      if ($3->ret_name != "") {
        ss << $3->code << endl;
        assign = $3->ret_name;
      }
      else {
        assign = $3->code;
      }

      if ($1->isArray) {
        if ($1->code.length() > 0) {
          ss << $1->code << endl;
        }
        ss << "[]= " << $1->var << ", " << $1->index << ", " << assign;
      }
      else {
        ss << "= " << $1->code << ", " << assign;
      }

      $$->code = ss.str();
      $$->ret_name = $1->code;
    }
  | IF BoolExpr THEN StatementList ENDIF
    {
      $$ = new n_Terminal();
      string ifTrue = makeLabel();
      string ifFalse = makeLabel();
      stringstream ss;
      ss << $2->code << endl; 
      ss << "?:= " << ifTrue << ", " << $2->ret_name << endl; 
      ss << ":= " << ifFalse << endl;
      ss << ": " << ifTrue << endl; 
      ss << $4->code << endl; 
      ss << ": " << ifFalse; 

      $$->code = ss.str();
    }
  | IF BoolExpr THEN StatementList ELSE StatementList ENDIF
    {
      $$ = new n_Terminal();
      string ifTrue = makeLabel();
      string ifFalse = makeLabel();
      stringstream ss;
      ss << $2->code << endl; 
      ss << "?:= " << ifTrue << ", " << $2->ret_name << endl; 
      ss << ":= " << ifFalse << endl; 
      ss << ": " << ifTrue << endl; 
      ss << $4->code << endl; 
      ss << ": " << ifFalse << endl; 
      ss << $6->code; 

      $$->code = ss.str();
    }
  | WHILE BoolExpr BEGINLOOP StatementList ENDLOOP
    {
      $$ = new n_Terminal();
      string conditionalLabel = makeLabel();
      string startLabel = makeLabel();
      string endLabel = makeLabel();
      stringstream ss;

      string replaceContinue = ":= " + conditionalLabel;
      replaceString($4->code, "continue", replaceContinue);

      ss << ": " << conditionalLabel << endl; 
      ss << $2->code << endl; 
      ss << "?:= " << startLabel << ", " << $2->ret_name << endl; 
      ss << ":= " << endLabel << endl; 
      ss << ": " << startLabel << endl;
      ss << $4->code << endl; 
      ss << ":= " << conditionalLabel << endl; 
      ss << ": " << endLabel;

      $$->code = ss.str();
    }
  | DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr
    {
      $$ = new n_Terminal();
      string startLabel = makeLabel();
      string conditionalLabel = makeLabel();
      stringstream ss;

      string replaceContinue = ":= " + conditionalLabel;
      replaceString($3->code, "continue", replaceContinue);

      ss << ": " << startLabel << endl; 
      ss << $3->code << endl;
      ss << ": " << conditionalLabel << endl;
      ss << $6->code << endl;
      ss << "?:= " << startLabel << ", " << $6->ret_name;

      $$->code = ss.str();
    }
  | FOR Var ASSIGN NUMBER SEMICOLON BoolExpr SEMICOLON Var ASSIGN Expression BEGINLOOP StatementList ENDLOOP
    {
      $$ = new n_Terminal();
      string conditionalLabel = makeLabel();
      string startLabel = makeLabel();
      string endLabel = makeLabel();
      string loopVariable = makeTemp();
      stringstream ss;

      string replaceContinue = ":= " + conditionalLabel;
      replaceString($12->code, "continue", replaceContinue);
      
      ss << "= " << loopVariable << ", " << $4 << endl; 
      ss << ": " << conditionalLabel << endl;
      ss << $6->code << endl; 
      ss << "?:= " << startLabel << ", " << $6->ret_name << endl; 
      ss << ":= " << endLabel << endl; 
      ss << ": " << startLabel << endl;
      ss << $12->code << endl;
      ss << $10->code << endl; 
      ss << "= " << loopVariable << ", " << $10->ret_name << endl; 
      ss << ":= " << conditionalLabel << endl; 
      ss << ": " << endLabel;

      $$->code = ss.str();
    }
  | READ VarList
    {
      $$ = new n_Terminal();
      stringstream ss;
      string temp = "";
      for (int i = 0; i < $2->code.length(); i++) {
        if ($2->code[i] == ',') {
          ss << ".< " << temp << endl;
          temp = "";
        }
        else {
          temp.push_back($2->code[i]);
        }
      }

      ss << ".< " << temp;

      $$->code = ss.str();
    }
  | WRITE VarList
    {
      $$ = new n_Terminal();
      stringstream ss;
      string temp = "";
      for (int i = 0; i < $2->code.length(); i++) {
        if ($2->code[i] == ',') {
          ss << ".< " << temp << endl;
          temp = "";
        }
        else {
          temp.push_back($2->code[i]);
        }
      }

      ss << ".> " << temp;

      $$->code = ss.str();
    }
  | CONTINUE
    {
      $$ = new n_Terminal();
      $$->code = "continue";
    }
  | RETURN Expression
    {
      $$ = new n_Terminal();
      stringstream ss;

      string returnOp;

      if ($2->ret_name != "") {
        ss << $2->code << endl;
        returnOp = $2->ret_name;

      }
      else {
        returnOp = makeTemp();
        ss << ". " << returnOp << endl;
        ss << "= " << returnOp << ", " << $2->code << endl;
      }

      ss << "ret " << returnOp;
      
      $$->code = ss.str();
      $$->ret_name = returnOp;
    }
  ;
StatementList: Statement SEMICOLON
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
    }
  | StatementList Statement SEMICOLON
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << $1->code << endl << $2->code;
      $$->code = ss.str();
    }
  ;

/* Bool-Expr */
BoolExpr: BoolExpr OR RelationAndExpr
    {
      $$ = new n_Terminal();
      string returnName = makeTemp(); 
      stringstream ss;

      ss << $1->code << endl << $3->code << endl; 
      ss << ". " << returnName << endl; 
      ss << "|| " << returnName << ", " << $1->ret_name << ", " << $3->ret_name; 
      
      $$->code = ss.str();
      $$->ret_name = returnName;
    }
  | RelationAndExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code; 
      $$->ret_name = $1->ret_name;
    }
  ;
/* Relation_And_Expr */
RelationAndExpr: RelationAndExpr AND RelationExpr
    {
      $$ = new n_Terminal();
      string returnName = makeTemp(); 

      stringstream ss;
      ss << $1->code << endl << $3->code << endl; 
      ss << ". " << returnName << endl; 
      ss << "&& " << returnName << ", " << $1->ret_name << ", " << $3->ret_name;
      
      $$->code = ss.str();
      $$->ret_name = returnName;
    }
  | RelationExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code; 
      $$->ret_name = $1->ret_name;
    }
  ;

/* Relation_Expr */
RelationExpr: Relations
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = $1->ret_name;
    }
  | NOT Relations
    {
      $$ = new n_Terminal();
      string notTemp = makeTemp();

      stringstream ss;
      ss << $2->code << endl;
      ss << "! " << notTemp << ", " << $2->ret_name;
      $$->code = ss.str();
      $$->ret_name = notTemp;
    }
  ;
Relations: Expression Comp Expression
    {
      $$ = new n_Terminal();
      string compResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code; 
      }

      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << compResult << endl;
        ss << $2 << " " << compResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << compResult << endl;
        ss << $2 << " " << compResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = compResult;
    }
  | TRUE
    {
      $$ = new n_Terminal();
      string trueTemp = makeTemp();
      stringstream ss;

      ss << ". " << trueTemp << endl;
      ss << "= " << trueTemp << ", 1";
      $$->code = ss.str();
      $$->ret_name = trueTemp;
    }
  | FALSE
    {
      $$ = new n_Terminal();
      string falseTemp = makeTemp();
      stringstream ss;

      ss << ". " << falseTemp << endl;
      ss << "= " << falseTemp << ", 0";
      $$->code = ss.str();
      $$->ret_name = falseTemp;
    }
  | L_PAREN BoolExpr R_PAREN
    {
      $$ = new n_Terminal();
      $$->code = $2->code;
      $$->ret_name = $2->ret_name;
    }
  ;

/* Comp */
Comp: EQ {/* pass value directly as $$ */}
  | NEQ {/* pass value directly as $$ */}
  | LT {/* pass value directly as $$ */}
  | GT {/* pass value directly as $$ */}
  | LTE {/* pass value directly as $$ */}
  | GTE {/* pass value directly as $$ */}
  ;

/* Expression */
Expression: Expression ADD MultiplicativeExpr
    {
      $$ = new n_Terminal();
      string addResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << addResult << endl;
        ss << "+ " << addResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << addResult << endl;
        ss << "+ " << addResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = addResult;
    }
  | Expression SUB MultiplicativeExpr
    {
      $$ = new n_Terminal();
      string subResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << subResult << endl;
        ss << "- " << subResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << subResult << endl;
        ss << "- " << subResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = subResult;
    }
  | MultiplicativeExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = $1->ret_name;
    }
  ;
ExpressionList: ExpressionList COMMA Expression
    {
      $$ = new n_Terminal();
      stringstream scode, sret;

      scode << $1->code << endl << $3->code; 
      
      sret << $3->ret_name << "," << $3->ret_name; 
      
      $$->code = scode.str();
      $$->ret_name = sret.str();
    }
  | Expression
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = $1->ret_name;
    }
  | %empty
    {
      $$ = new n_Terminal();
    }
  ;

/* Multiplicative_Expr */
MultiplicativeExpr: MultiplicativeExpr MULT Term
    {
      $$ = new n_Terminal();
      string multResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << multResult << endl;
        ss << "* " << multResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << multResult << endl;
        ss << "* " << multResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = multResult;
    }
  | MultiplicativeExpr DIV Term
    {
      $$ = new n_Terminal();
      string divResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code;
      }


      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << divResult << endl;
        ss << "/ " << divResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << divResult << endl;
        ss << "/ " << divResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = divResult;
    }
  | MultiplicativeExpr MOD Term
    {
      $$ = new n_Terminal();
      string modResult = makeTemp();
      stringstream ss;
      string firstOp;

      if ($1->ret_name != "") {
        ss << $1->code << endl;
        firstOp = $1->ret_name;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->ret_name != "") {
        ss << $3->code << endl;
        ss << ". " << modResult << endl;
        ss << "% " << modResult << ", " << firstOp << ", " << $3->ret_name;  
      }
      else {
        ss << ". " << modResult << endl;
        ss << "% " << modResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->ret_name = modResult;
    }
  | Term
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = $1->ret_name;
    }
  ;

/* Term */
Term: TermInner
    {
      $$ = new n_Terminal();

      if ($1->ret_name == "var") {
        string newTemp = makeTemp();
        stringstream ss;
        
        if ($1->isArray) {
          if ($1->code.length() > 0) {
            ss << $1->code << endl;
          }
          ss << "=[] " << newTemp << ", " << $1->var << ", " << $1->index;
          $$->var = $1->var;
          $$->index = $1->index;
        }
        else {
          ss << ". " << newTemp << endl; 
          ss << "= " << newTemp << ", " << $1->code;
        }

        $$->code = ss.str();
        $$->ret_name = newTemp;
      }
      else if ($1->ret_name == "num") {
        $$->code = $1->code;
        $$->ret_name = "";
      }
      else {
        $$->code = $1->code;
        $$->ret_name = $1->ret_name;
      }
    }
  | SUB TermInner
    {
      $$ = new n_Terminal();
      stringstream ss;
      string subTemp = makeTemp();

      if ($2->ret_name == "var") {
        string newTemp = makeTemp();
        
        if ($2->isArray) {
          if ($2->code.length() > 0) {
            ss << $2->code << endl;
          }
          ss << "=[] " << newTemp << ", " << $2->var << ", " << $2->index << endl;

          $$->var = $2->var;
          $$->index = $2->index;
        }
        else {
          ss << ". " << newTemp << endl; 
          ss << "= " << newTemp << ", " << $2->code << endl;
        }

        ss << ". " << subTemp << endl;
        ss << "- " << subTemp << ", 0, " << newTemp;

        $$->code = ss.str();
        $$->ret_name = subTemp;
      }
      else {
        ss << ". " << subTemp << endl;
        ss << "- " << subTemp << ", 0, " << $2->code;

        $$->code = ss.str();
        $$->ret_name = subTemp;
      }
    }
  | Identifier L_PAREN ExpressionList R_PAREN
    {
      $$ = new n_Terminal();
      string newTemp = makeTemp();
      stringstream ss, sret;

      ss << $3->code << endl; 
      string temp;
      for (int i = 0; i < $3->ret_name.length(); i++) {
        if ($3->ret_name[i] == ',') {
          sret << "param " << temp << endl;
          temp = "";
          continue;
        }
        temp.push_back($3->ret_name[i]);
      }

      if (temp.length() > 0) { 
        sret << "param " << temp << endl;
        ss << sret.str(); 
      }

      ss << ". " << newTemp << endl;
      ss << "call " << $1->code << ", " << newTemp;

      $$->code = ss.str();
      $$->ret_name = newTemp;
    }
  ;
TermInner: Var
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->ret_name = "var";
      $$->isArray = $1->isArray;
      $$->var = $1->var;
      $$->index = $1->index;
    }
  | NUMBER
    {
      $$ = new n_Terminal();
      $$->code = to_string($1);
      $$->ret_name = "num";
    }
  | L_PAREN Expression R_PAREN
    {
      $$ = new n_Terminal();
      stringstream ss;

      ss << $2->code; 
      $$->code = ss.str();
      $$->ret_name = $2->ret_name;
    }
  ;

/* Var */
Var: Identifier
    {  
      /* 
        Error 6 of 9: 
        Forgetting to specify an array index when using an array variable 
        (i.e., trying to use an array variable as a regular integer variable).
      */

      $$ = new n_Terminal();
      stringstream ss;
      ss << "_" << $1->code;

      checkDeclared(ss.str());

      $$->code = ss.str();
      $$->var = ss.str();
      $$->isArray = false;
    }
  | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET
    {
      /* 
        Error 7 of 9: 
        Specifying an array index when using a regular integer variable 
        (i.e., trying to use a regular integer variable as an array variable).
      */
      
      $$ = new n_Terminal();
      stringstream ss;
      string index, code = "";

      if ($3->ret_name != "") {
        code = $3->code;
        index = $3->ret_name;
      }
      else {
        index = $3->code; 
      }

      ss << "_" << $1->code;

      checkDeclared(ss.str());

      $$->code = code;
      $$->isArray = true;
      $$->var = ss.str();
      $$->index = index;
    }
  ;
VarList: Var
    {
      $$ = new n_Terminal();
      stringstream ss;
      $$->code = $1->var;
      $$->isArray = $1->isArray;
    }
  | Var COMMA VarList
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << $1->var << "," << $3->code;
      $$->code = ss.str();

      if ($1->isArray != $3->isArray) {
        stringstream er;
        er << "variable \"" << $1->code << "\" is of type ";
        if ($1->isArray) {
          er << "array.";
        }
        else {
          er << "integer.";
        }

        yyerror(er.str());
      }
      $$->isArray = $1->isArray;
    }
  ;

%%
string makeTemp() {
  static int tempNum = 0;
  return "__temp__" + to_string(tempNum++);
}

string makeLabel() {
  static int labelNum = 0;
  return "__label__" + to_string(labelNum++);
}

string createVar(char* ident) {
  string newVar = string(ident);
  return newVar;
}

string findIndex (const string &ref) {
  int leftB = ref.find('[');
  if (leftB != string::npos) {
    int indexLength = ((ref.length() - 1) - leftB) - 1;
    return ref.substr(leftB + 1, indexLength);
  }
  else {
    exit(1);
  }
}

void replaceString(string& str, const string& oldStr, const string& newStr) {
  string::size_type pos = 0u;
  while((pos = str.find(oldStr, pos)) != string::npos){
     str.replace(pos, oldStr.length(), newStr);
     pos += newStr.length();
  }
}

bool varDeclared(const vector<string>& symbolTable, const string& var) {
  for (int i = 0; i < symbolTable.size(); i++) {
    if (symbolTable.at(i) == var) {
      return true;
    }
  }
  return false;
}

void addLocalVar(const string& var) {
  for (int i = 0; i < declaredVar.size(); i++) {
    if (declaredVar.at(i) == var) {
      string errorString = "symbol \"" + var + "\" is multiply-defined.";
      yyerror(errorString);
    }
  }
  declaredVar.push_back(var);
}

void checkDeclared(const string& var) {
  for (int i = 0; i < declaredVar.size(); i++) {
    if (declaredVar.at(i) == var) {
      return;
    }
  }
  string err = "used variable \"" + var + "\" was not previously declared.";
  yyerror(err);
}

void addFunction(const string& func) {
  for (int i = 0; i < declaredFunc.size(); i++) {
    if (declaredFunc.at(i) == func) {
      string errorString = "function \"" + func + "\" is multiply-defined.";
      yyerror(errorString);
    }
  }
  declaredFunc.push_back(func);
}

void checkFuncDeclared(const string& func) {
  for (int i = 0; i < declaredFunc.size(); i++) {
    if (declaredFunc.at(i) == func) {
      return;
    }
  }
  string err = "called function \"" + func + "\" was not previously declared.";
  yyerror(err);
}

int yyerror(string s) {
  extern int currLine, currPos;
  extern char *yytext;

  cout << "Error line: " << currLine << ": " << s << endl;
  exit(1);
}

int yyerror(char* s) {
  return yyerror(string(s));
}
