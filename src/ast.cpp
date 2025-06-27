#include "ast.h"
#include <iostream>

// AST implementations (currently all inline in the header)

// Add print method for debugging
void ForEachStmtAST::print() const {
  std::cout << "Parsed a foreach loop: for " << Type << " " << VarName << " in <expression>" << std::endl;
}