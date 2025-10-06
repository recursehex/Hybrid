#include "parser.h"
#include "toplevel.h"
#include "ast.h"
#include "compiler_session.h"
#include <cstdio>
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

int main() {
  CompilerSession session;
  session.resetAll();
  pushCompilerSession(session);

  // Initialize LLVM
  InitializeModule();
  // Install standard binary operators.
  // 1 is lowest precedence.
  auto &precedence = currentParser().binopPrecedence;
  precedence["="] = 2;   // assignment (lowest)
  precedence["+="] = 2;  // compound assignments
  precedence["-="] = 2;
  precedence["*="] = 2;
  precedence["/="] = 2;
  precedence["%="] = 2;
  precedence["&="] = 2;  // bitwise compound assignments
  precedence["|="] = 2;
  precedence["^="] = 2;
  precedence["<<="] = 2;
  precedence[">>="] = 2;
  precedence["||"] = 5;  // logical OR
  precedence["&&"] = 6;  // logical AND
  precedence["|"] = 7;   // bitwise OR
  precedence["^"] = 8;   // bitwise XOR
  precedence["&"] = 9;   // bitwise AND
  precedence["<"] = 10;  // comparisons
  precedence[">"] = 10;
  precedence["<="] = 10;
  precedence[">="] = 10;
  precedence["=="] = 10;
  precedence["!="] = 10;
  precedence["<<"] = 15; // shift operators
  precedence[">>"] = 15;
  precedence["+"] = 20;  // arithmetic
  precedence["-"] = 20;
  precedence["*"] = 40;  // multiplication/division/modulo
  precedence["/"] = 40;
  precedence["%"] = 40;  // modulo has same precedence as * and /

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print the entire generated module
  fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
  getModule()->print(llvm::errs(), nullptr);

  popCompilerSession();

  return 0;
}
