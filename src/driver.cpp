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
  BinopPrecedence["="] = 2;   // assignment (lowest)
  BinopPrecedence["+="] = 2;  // compound assignments
  BinopPrecedence["-="] = 2;
  BinopPrecedence["*="] = 2;
  BinopPrecedence["/="] = 2;
  BinopPrecedence["%="] = 2;
  BinopPrecedence["&="] = 2;  // bitwise compound assignments
  BinopPrecedence["|="] = 2;
  BinopPrecedence["^="] = 2;
  BinopPrecedence["<<="] = 2;
  BinopPrecedence[">>="] = 2;
  BinopPrecedence["||"] = 5;  // logical OR
  BinopPrecedence["&&"] = 6;  // logical AND
  BinopPrecedence["|"] = 7;   // bitwise OR
  BinopPrecedence["^"] = 8;   // bitwise XOR
  BinopPrecedence["&"] = 9;   // bitwise AND
  BinopPrecedence["<"] = 10;  // comparisons
  BinopPrecedence[">"] = 10;
  BinopPrecedence["<="] = 10;
  BinopPrecedence[">="] = 10;
  BinopPrecedence["=="] = 10;
  BinopPrecedence["!="] = 10;
  BinopPrecedence["<<"] = 15; // shift operators
  BinopPrecedence[">>"] = 15;
  BinopPrecedence["+"] = 20;  // arithmetic
  BinopPrecedence["-"] = 20;
  BinopPrecedence["*"] = 40;  // multiplication/division/modulo
  BinopPrecedence["/"] = 40;
  BinopPrecedence["%"] = 40;  // modulo has same precedence as * and /

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