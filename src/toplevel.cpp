#include "toplevel.h"
#include "parser.h"
#include "lexer.h"
#include <cstdio>

void HandleDefinition() {
  if (ParseDefinition()) {
    fprintf(stderr, "Parsed a function definition.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleExtern() {
  if (ParseExtern()) {
    fprintf(stderr, "Parsed an extern\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (ParseTopLevelExpr()) {
    fprintf(stderr, "Parsed a top-level expr\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleVariableDeclaration() {
  if (ParseVariableDeclaration()) {
    fprintf(stderr, "Parsed a variable declaration.\n");
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleForEachStatement() {
  auto ForEach = ParseForEachStatement();
  if (ForEach) {
    ForEach->print();
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleUseStatement() {
  auto Use = ParseUseStatement();
  if (Use) {
    fprintf(stderr, "Parsed a use statement: %s\n", Use->getModule().c_str());
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}


/// top ::= definition | external | expression | variabledecl | foreachstmt | usestmt | ';' | '\n'
void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
    case tok_newline: // ignore top-level newlines.
      getNextToken();
      break;
    case tok_extern:
      HandleExtern();
      break;
    case tok_use:
      HandleUseStatement();
      break;
    case tok_for:
      HandleForEachStatement();
      break;
    case tok_int:
    case tok_float:
    case tok_double:
    case tok_char:
    case tok_void:
    case tok_bool:
    case tok_string:
      // Use the new unified handler
      ParseTypeIdentifier();
      break;
    case tok_identifier:
      // For C-style declarations, functions start with type keywords,
      // so a bare identifier is likely an expression
      HandleTopLevelExpression();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}