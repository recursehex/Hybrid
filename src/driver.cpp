#include "parser.h"
#include "toplevel.h"
#include "ast.h"
#include <cstdio>
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

int main() {
  // Initialize LLVM
  InitializeModule();
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence["="] = 2;  // assignment (lowest)
  BinopPrecedence["<"] = 10;
  BinopPrecedence[">"] = 10;
  BinopPrecedence["<="] = 10;
  BinopPrecedence[">="] = 10;
  BinopPrecedence["=="] = 10;
  BinopPrecedence["!="] = 10;
  BinopPrecedence["+"] = 20;
  BinopPrecedence["-"] = 20;
  BinopPrecedence["*"] = 40; // highest.

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print the entire generated module
  fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
  getModule()->print(llvm::errs(), nullptr);

  return 0;
}