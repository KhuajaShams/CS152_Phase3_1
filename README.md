# Compilers Phase 3 Submission
Partners: Cameron Morin & Neal Goyal

We have run through all given test cases. Here is a breakdown of our project completion:

## Table of Contents
- [Correct Grammar](#correct-grammar)
- [Correct Semantic Errors](#correct-semantic-errors)
- [Known Bugs](#known-bugs)

## Correct Grammar
Grammar that successfully compiles and runs:
- Program
- DeclarationList
- Declaration
- FunctionList
- Function
- Identifier
- FunctionParams
- FunctionLocals
- FunctionBody
- StatementList
- Statement (mostly, see below)
- IdentifierList
- Var
- VarList
- Expression
- ExpressionList
- BoolExpr
- RelationAndExpr
- RelationExpr
- Relations
- MultiplicativeExpr
- Term
- TermInner

## Correct Semantic Errors
Semantic Errors that correctly output:
- Using a variable without having first declared it.
- Not defining a main function.
- Defining a variable more than once (it should also be an error to declare a variable with the same name as the MINI-L program itself).
- Declaring an array of size <= 0.
- Using continue statement outside a loop

## Known Bugs
Here are features that we have attempted to fix, but resulted with bugs in our submission:

Semantic Errors:
- Calling a function which has not been defined. **Note: Did not work for fibonacci due to recursion, but works elsewhere**
- Trying to name a variable with the same name as a reserved keyword. **Note: Unsure how to tackle this because parser will throw a syntax if a reserved keyword is found elsewhere**
- Forgetting to specify an array index when using an array variable (i.e., trying to use an array variable as a regular integer variable).
- Specifying an array index when using a regular integer variable (i.e., trying to use a regular integer variable as an array variable).

Grammar:
- Statement: Read and write to array have bugs
