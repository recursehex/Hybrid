#include "parser.h"
#include "toplevel.h"
#include "ast.h"
#include "compiler_session.h"

#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

namespace {

void initializeOperatorPrecedence() {
  auto &precedence = currentParser().binopPrecedence;
  precedence.clear();
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
}

bool loadSourceFile(const char *path, std::string &outBuffer) {
  std::ifstream input(path, std::ios::in | std::ios::binary);
  if (!input)
    return false;

  std::ostringstream contents;
  contents << input.rdbuf();
  outBuffer = contents.str();
  return true;
}

}

int main(int argc, char **argv) {
  CompilerSession session;
  session.resetAll();
  pushCompilerSession(session);

  // Initialize LLVM
  InitializeModule();
  initializeOperatorPrecedence();

  if (argc > 1) {
    SetInteractiveMode(false);

    for (int i = 1; i < argc; ++i) {
      std::string source;
      if (!loadSourceFile(argv[i], source)) {
        fprintf(stderr, "Error: Failed to open source file '%s'\n", argv[i]);
        popCompilerSession();
        return 1;
      }

      session.beginUnit(i > 1);
      currentLexer().setInputBuffer(source);

      getNextToken();
      MainLoop();
    }

    // Emit the full module once all units have been processed
    fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
    getModule()->print(llvm::errs(), nullptr);
  } else {
    SetInteractiveMode(true);

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    // Run the main "interpreter loop" now.
    MainLoop();

    // Print the entire generated module
    fprintf(stderr, "\n=== Final Generated LLVM IR ===\n");
    getModule()->print(llvm::errs(), nullptr);
  }

  popCompilerSession();

  return 0;
}
