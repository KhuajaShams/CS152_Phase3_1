%code requires{
  using namespace std;
  #include<string>
  
  struct n_Terminal {
    string code;
    string r_type;
    bool boolArray;
    string var;
    string index;
  };
}

%{
  using namespace std;
  #include "stdio.h"
  #include <string>
  #include <vector>
  #include <iostream>
  #include <sstream>
  int yyerror(string s);
  int yyeror(char *s);
  int yylex(void);
  string createTemp();
  string createLabel();
  void replaceString(string&, const string&, const string&);
  void 
(const string&);
  bool isMain = false;
  extern FILE* yyin;
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
      for (int i = 0; i < $2->r_type.length(); i++) {
        if ($2->r_type[i] == ',') {
          ss << "= " << ident << ", $" << to_string(paramNum) << endl;
          ident = "";
          paramNum++;
          continue;
        }
        ident.push_back($2->r_type[i]);
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
      
      slist << $1->r_type << "," << $2->r_type;

      $$->code = ss.str();
      $$->r_type = slist.str();
    }
  | Declaration SEMICOLON
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = $1->r_type; 
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
          
        (currVar);
          currVar = "";
        }
        else {
          currVar.push_back($1->code[i]);
        }
      }

      if (currVar.length() > 0) {
        ss << ". " << currVar;
        
      (currVar);
      }
      
      $$->code = ss.str();
      $$->r_type = $1->code; 
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
          
        (currVar);
          currVar = "";
        }
        else {
          currVar.push_back($1->code[i]);
        }
      }

      if (currVar.length() > 0 ) {
        ss << ".[] " << currVar << ", " << to_string($5);
        
      (currVar);
      }
      
      $$->code = ss.str();
      $$->r_type = $1->code;
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

      if ($3->r_type != "") {
        ss << $3->code << endl;
        assign = $3->r_type;
      }
      else {
        assign = $3->code;
      }

      if ($1->boolArray) {
        if ($1->code.length() > 0) {
          ss << $1->code << endl;
        }
        ss << "[]= " << $1->var << ", " << $1->index << ", " << assign;
      }
      else {
        ss << "= " << $1->code << ", " << assign;
      }

      $$->code = ss.str();
      $$->r_type = $1->code;
    }
  | IF BoolExpr THEN StatementList ENDIF
    {
      $$ = new n_Terminal();
      string trueLabel = createLabel();
      string falseLabel = createLabel();
      stringstream ss;
      ss << $2->code << endl; 
      ss << "?:= " << trueLabel << ", " << $2->r_type << endl; 
      ss << ":= " << falseLabel << endl;
      ss << ": " << trueLabel << endl; 
      ss << $4->code << endl; 
      ss << ": " << falseLabel; 

      $$->code = ss.str();
    }
  | IF BoolExpr THEN StatementList ELSE StatementList ENDIF
    {
      $$ = new n_Terminal();
      string trueLabel = createLabel();
      string falseLabel = createLabel();
      stringstream ss;
      ss << $2->code << endl; 
      ss << "?:= " << trueLabel << ", " << $2->r_type << endl; 
      ss << ":= " << falseLabel << endl; 
      ss << ": " << trueLabel << endl; 
      ss << $4->code << endl; 
      ss << ": " << falseLabel << endl; 
      ss << $6->code; 

      $$->code = ss.str();
    }
  | WHILE BoolExpr BEGINLOOP StatementList ENDLOOP
    {
      $$ = new n_Terminal();
      string conLabel = createLabel();
      string newLabel = createLabel();
      string endLabel = createLabel();
      stringstream ss;

      string replaceContinue = ":= " + conLabel;
      replaceString($4->code, "continue", replaceContinue);

      ss << ": " << conLabel << endl; 
      ss << $2->code << endl; 
      ss << "?:= " << newLabel << ", " << $2->r_type << endl; 
      ss << ":= " << endLabel << endl; 
      ss << ": " << newLabel << endl;
      ss << $4->code << endl; 
      ss << ":= " << conLabel << endl; 
      ss << ": " << endLabel;

      $$->code = ss.str();
    }
  | DO BEGINLOOP StatementList ENDLOOP WHILE BoolExpr
    {
      $$ = new n_Terminal();
      string newLabel = createLabel();
      string conLabel = createLabel();
      stringstream ss;

      string replaceContinue = ":= " + conLabel;
      replaceString($3->code, "continue", replaceContinue);

      ss << ": " << newLabel << endl; 
      ss << $3->code << endl;
      ss << ": " << conLabel << endl;
      ss << $6->code << endl;
      ss << "?:= " << newLabel << ", " << $6->r_type;

      $$->code = ss.str();
    }
  | FOR Var ASSIGN NUMBER SEMICOLON BoolExpr SEMICOLON Var ASSIGN Expression BEGINLOOP StatementList ENDLOOP
    {
      $$ = new n_Terminal();
      string conLabel = createLabel();
      string newLabel = createLabel();
      string endLabel = createLabel();
      string loopVariable = createTemp();
      stringstream ss;

      string replaceContinue = ":= " + conLabel;
      replaceString($12->code, "continue", replaceContinue);
      
      ss << "= " << loopVariable << ", " << $4 << endl; 
      ss << ": " << conLabel << endl;
      ss << $6->code << endl; 
      ss << "?:= " << newLabel << ", " << $6->r_type << endl; 
      ss << ":= " << endLabel << endl; 
      ss << ": " << newLabel << endl;
      ss << $12->code << endl;
      ss << $10->code << endl; 
      ss << "= " << loopVariable << ", " << $10->r_type << endl; 
      ss << ":= " << conLabel << endl; 
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

      if ($2->r_type != "") {
        ss << $2->code << endl;
        returnOp = $2->r_type;

      }
      else {
        returnOp = createTemp();
        ss << ". " << returnOp << endl;
        ss << "= " << returnOp << ", " << $2->code << endl;
      }

      ss << "ret " << returnOp;
      
      $$->code = ss.str();
      $$->r_type = returnOp;
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
      string returnName = createTemp(); 
      stringstream ss;

      ss << $1->code << endl << $3->code << endl; 
      ss << ". " << returnName << endl; 
      ss << "|| " << returnName << ", " << $1->r_type << ", " << $3->r_type; 
      
      $$->code = ss.str();
      $$->r_type = returnName;
    }
  | RelationAndExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code; 
      $$->r_type = $1->r_type;
    }
  ;
/* Relation_And_Expr */
RelationAndExpr: RelationAndExpr AND RelationExpr
    {
      $$ = new n_Terminal();
      string returnName = createTemp(); 

      stringstream ss;
      ss << $1->code << endl << $3->code << endl; 
      ss << ". " << returnName << endl; 
      ss << "&& " << returnName << ", " << $1->r_type << ", " << $3->r_type;
      
      $$->code = ss.str();
      $$->r_type = returnName;
    }
  | RelationExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code; 
      $$->r_type = $1->r_type;
    }
  ;

/* Relation_Expr */
RelationExpr: Relations
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = $1->r_type;
    }
  | NOT Relations
    {
      $$ = new n_Terminal();
      string notTemp = createTemp();

      stringstream ss;
      ss << $2->code << endl;
      ss << "! " << notTemp << ", " << $2->r_type;
      $$->code = ss.str();
      $$->r_type = notTemp;
    }
  ;
Relations: Expression Comp Expression
    {
      $$ = new n_Terminal();
      string compResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code; 
      }

      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << compResult << endl;
        ss << $2 << " " << compResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << compResult << endl;
        ss << $2 << " " << compResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = compResult;
    }
  | TRUE
    {
      $$ = new n_Terminal();
      string trueTemp = createTemp();
      stringstream ss;

      ss << ". " << trueTemp << endl;
      ss << "= " << trueTemp << ", 1";
      $$->code = ss.str();
      $$->r_type = trueTemp;
    }
  | FALSE
    {
      $$ = new n_Terminal();
      string falseTemp = createTemp();
      stringstream ss;

      ss << ". " << falseTemp << endl;
      ss << "= " << falseTemp << ", 0";
      $$->code = ss.str();
      $$->r_type = falseTemp;
    }
  | L_PAREN BoolExpr R_PAREN
    {
      $$ = new n_Terminal();
      $$->code = $2->code;
      $$->r_type = $2->r_type;
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
      string addResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << addResult << endl;
        ss << "+ " << addResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << addResult << endl;
        ss << "+ " << addResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = addResult;
    }
  | Expression SUB MultiplicativeExpr
    {
      $$ = new n_Terminal();
      string subResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << subResult << endl;
        ss << "- " << subResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << subResult << endl;
        ss << "- " << subResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = subResult;
    }
  | MultiplicativeExpr
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = $1->r_type;
    }
  ;
ExpressionList: ExpressionList COMMA Expression
    {
      $$ = new n_Terminal();
      stringstream scode, sret;

      scode << $1->code << endl << $3->code; 
      
      sret << $3->r_type << "," << $3->r_type; 
      
      $$->code = scode.str();
      $$->r_type = sret.str();
    }
  | Expression
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = $1->r_type;
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
      string multResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << multResult << endl;
        ss << "* " << multResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << multResult << endl;
        ss << "* " << multResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = multResult;
    }
  | MultiplicativeExpr DIV Term
    {
      $$ = new n_Terminal();
      string divResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code;
      }


      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << divResult << endl;
        ss << "/ " << divResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << divResult << endl;
        ss << "/ " << divResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = divResult;
    }
  | MultiplicativeExpr MOD Term
    {
      $$ = new n_Terminal();
      string modResult = createTemp();
      stringstream ss;
      string firstOp;

      if ($1->r_type != "") {
        ss << $1->code << endl;
        firstOp = $1->r_type;

      }
      else {
        firstOp = $1->code; 
      }


      if ($3->r_type != "") {
        ss << $3->code << endl;
        ss << ". " << modResult << endl;
        ss << "% " << modResult << ", " << firstOp << ", " << $3->r_type;  
      }
      else {
        ss << ". " << modResult << endl;
        ss << "% " << modResult << ", " << firstOp << ", " << $3->code;
      }

      $$->code = ss.str();
      $$->r_type = modResult;
    }
  | Term
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = $1->r_type;
    }
  ;

/* Term */
Term: TermInner
    {
      $$ = new n_Terminal();

      if ($1->r_type == "var") {
        string newTemp = createTemp();
        stringstream ss;
        
        if ($1->boolArray) {
          ss << $1->code << endl;
          ss << "=[] " << newTemp << ", " << $1->var << ", " << $1->index;
          $$->var = $1->var;
          $$->index = $1->index;
        }
        else {
          ss << ". " << newTemp << endl; 
          ss << "= " << newTemp << ", " << $1->code;
        }

        $$->code = ss.str();
        $$->r_type = newTemp;
      }
      else if ($1->r_type == "num") {
        $$->code = $1->code;
        $$->r_type = "";
      }
      else {
        $$->code = $1->code;
        $$->r_type = $1->r_type;
      }
    }
  | SUB TermInner
    {
      $$ = new n_Terminal();
      stringstream ss;
      string subTemp = createTemp();

      if ($2->r_type == "var") {
        string newTemp = createTemp();
        
        if ($2->boolArray) {
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
        $$->r_type = subTemp;
      }
      else {
        ss << ". " << subTemp << endl;
        ss << "- " << subTemp << ", 0, " << $2->code;

        $$->code = ss.str();
        $$->r_type = subTemp;
      }
    }
  | Identifier L_PAREN ExpressionList R_PAREN
    {
      $$ = new n_Terminal();
      string newTemp = createTemp();
      stringstream ss, sret;

      ss << $3->code << endl; 
      string temp;
      for (int i = 0; i < $3->r_type.length(); i++) {
        if ($3->r_type[i] == ',') {
          sret << "param " << temp << endl;
          temp = "";
          continue;
        }
        temp.push_back($3->r_type[i]);
      }

      if (temp.length() > 0) { 
        sret << "param " << temp << endl;
        ss << sret.str(); 
      }

      ss << ". " << newTemp << endl;
      ss << "call " << $1->code << ", " << newTemp;

      $$->code = ss.str();
      $$->r_type = newTemp;
    }
  ;
TermInner: Var
    {
      $$ = new n_Terminal();
      $$->code = $1->code;
      $$->r_type = "var";
      $$->boolArray = $1->boolArray;
      $$->var = $1->var;
      $$->index = $1->index;
    }
  | NUMBER
    {
      $$ = new n_Terminal();
      $$->code = to_string($1);
      $$->r_type = "num";
    }
  | L_PAREN Expression R_PAREN
    {
      $$ = new n_Terminal();
      stringstream ss;

      ss << $2->code; 
      $$->code = ss.str();
      $$->r_type = $2->r_type;
    }
  ;

/* Var */
Var: Identifier
    {  

      $$ = new n_Terminal();
      stringstream ss;
      ss << "_" << $1->code;
      $$->code = ss.str();
      $$->var = ss.str();
      $$->boolArray = false;
    }
  | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET
    {
      $$ = new n_Terminal();
      stringstream ss;
      string index, code = "";

      if ($3->r_type != "") {
        code = $3->code;
        index = $3->r_type;
      }
      else {
        index = $3->code; 
      }

      ss << "_" << $1->code;
      $$->code = code;
      $$->boolArray = true;
      $$->var = ss.str();
      $$->index = index;
    }
  ;
VarList: Var
    {
      $$ = new n_Terminal();
      stringstream ss;
      $$->code = $1->var;
      $$->boolArray = $1->boolArray;
    }
  | Var COMMA VarList
    {
      $$ = new n_Terminal();
      stringstream ss;
      ss << $1->var << "," << $3->code;
      $$->code = ss.str();

      if ($1->boolArray != $3->boolArray) {
        stringstream er;
        er << "variable \"" << $1->code << "\" is of type ";
        if ($1->boolArray) {
          er << "array.";
        }
        else {
          er << "integer.";
        }

        yyerror(er.str());
      }
      $$->boolArray = $1->boolArray;
    }
  ;

%%
string createTemp() {
  static int tempNum = 0;
  return "__temp__" + to_string(tempNum++);
}

string createLabel() {
  static int labelNum = 0;
  return "__label__" + to_string(labelNum++);
}



void replaceString(string& str, const string& oldStr, const string& newStr) {
  string::size_type pos = 0u;
  while((pos = str.find(oldStr, pos)) != string::npos){
     str.replace(pos, oldStr.length(), newStr);
     pos += newStr.length();
  }
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
