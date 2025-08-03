#include "toplevel.h"
#include "parser.h"
#include "lexer.h"
#include "ast.h"
#include <cstdio>

// LLVM includes for module and context management
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    fprintf(stderr, "Parsed function successfully, generating code...\n");
    if (auto FnIR = FnAST->codegen()) {
      fprintf(stderr, "Generated function IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      fprintf(stderr, "Error: Failed to generate IR for function\n");
    }
  } else {
    fprintf(stderr, "Error: Failed to parse function definition\n");
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Generated extern IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  }
 else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto FnIR = FnAST->codegen()) {
      fprintf(stderr, "Generated top-level expression IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  }
 else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleVariableDeclaration() {
  if (auto VarAST = ParseVariableDeclaration()) {
    if (auto VarIR = VarAST->codegen()) {
      fprintf(stderr, "Generated variable declaration IR:\n");
      VarIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  }
 else {
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
    case tok_byte:
    case tok_short:
    case tok_long:
    case tok_sbyte:
    case tok_ushort:
    case tok_uint:
    case tok_ulong:
    case tok_schar:
    case tok_lchar:
      if (!ParseTypeIdentifier()) {
        // If ParseTypeIdentifier failed, it means it wasn't a valid type-prefixed declaration
        // or an error occurred during parsing. Consume the current token to avoid infinite loop.
        getNextToken();
      }
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