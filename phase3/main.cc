#include "heading.h";
#include <stdlib.h>;

int yyparse();

int main(int argc, char **argv) {
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL)) {
    cerr << argv[0] << ": File " << argv[1] << " cannot be opened.\n";
    exit(0);
  }
  
  yyparse();

  return 0;
}