// This file implements top-level handlers that drive parsing and LLVM IR emission in interactive or batch mode.

#include "toplevel.h"
#include "parser.h"
#include "lexer.h"
#include "ast.h"
#include "compiler_session.h"
#include <cstdio>
#include <set>
#include <utility>

// LLVM includes for module and context management
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

static bool gInteractiveMode = true;

#define CurTok (currentParser().curTok)
#define StructNames (currentParser().structNames)
#define DelegateNames (currentParser().delegateNames)
#define IdentifierStr (currentLexer().identifierStr)

void SetInteractiveMode(bool enabled) {
  gInteractiveMode = enabled;
}

bool IsInteractiveMode() {
  return gInteractiveMode;
}

void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (!FnAST->getProto()->getGenericParameters().empty()) {
      RegisterGenericFunctionTemplate(std::move(FnAST));
      return;
    }

    if (gInteractiveMode) fprintf(stderr, "Parsed function successfully, generating code...\n");
    if (auto FnIR = FnAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated function IR:\n");
      FnIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate IR for function");
    }
  } else {
    reportCompilerError("Failed to parse function definition");
    // Skip token for error recovery.
    getNextToken();
  }
}


void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto FnIR = ProtoAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated extern IR:\n");
      FnIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  }
 else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  auto Expr = ParseExpression();
  if (!Expr) {
    getNextToken();
    return;
  }

  auto Stmt = std::make_unique<ExpressionStmtAST>(std::move(Expr));
  if (auto Value = Stmt->codegen()) {
    NoteTopLevelStatementEmitted();
    if (gInteractiveMode) {
      fprintf(stderr, "Generated top-level expression IR:\n");
      Value->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  }
}

void HandleVariableDeclaration() {
  if (auto VarAST = ParseVariableDeclaration()) {
    if (auto VarIR = VarAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated variable declaration IR:\n");
      VarIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
      NoteTopLevelStatementEmitted();
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

void HandleAssertStatement() {
  auto Assert = ParseAssertStatement();
  if (Assert) {
    if (auto AssertIR = Assert->codegen()) {
      NoteTopLevelStatementEmitted();
      if (gInteractiveMode) fprintf(stderr, "Generated top-level assert IR:\n");
      AssertIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleIfStatement() {
  if (auto IfAST = ParseIfStatement()) {
    if (gInteractiveMode) fprintf(stderr, "Parsed if statement successfully, generating code...\n");
    if (auto IfIR = IfAST->codegen()) {
      NoteTopLevelStatementEmitted();
      if (gInteractiveMode) fprintf(stderr, "Generated if statement IR:\n");
      IfIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate IR for if statement");
    }
  } else {
    reportCompilerError("Failed to parse if statement");
    getNextToken();
  }
}

void HandleWhileStatement() {
  if (auto WhileAST = ParseWhileStatement()) {
    if (gInteractiveMode) fprintf(stderr, "Parsed while statement successfully, generating code...\n");
    if (auto WhileIR = WhileAST->codegen()) {
      NoteTopLevelStatementEmitted();
      if (gInteractiveMode) fprintf(stderr, "Generated while statement IR:\n");
      WhileIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate IR for while statement");
    }
  } else {
    reportCompilerError("Failed to parse while statement");
    getNextToken();
  }
}

void HandleUseStatement() {
  auto Use = ParseUseStatement();
  if (Use) {
    if (gInteractiveMode) fprintf(stderr, "Parsed a use statement: %s\n", Use->getModule().c_str());
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleStructDefinition() {
  if (auto StructAST = ParseStructDefinition(AggregateKind::Struct)) {
    if (StructAST->isGenericTemplate()) {
      RegisterGenericTemplate(std::move(StructAST));
      return;
    }
    if (auto StructType = StructAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated struct type:\n");
      StructType->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleClassDefinition(bool isAbstract) {
  if (auto ClassAST = ParseStructDefinition(AggregateKind::Class, isAbstract)) {
    if (ClassAST->isGenericTemplate()) {
      RegisterGenericTemplate(std::move(ClassAST));
      return;
    }
    if (auto ClassType = ClassAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated class type:\n");
      ClassType->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

void HandleInterfaceDefinition(bool isAbstract = false) {
  if (auto InterfaceAST = ParseStructDefinition(AggregateKind::Interface, isAbstract)) {
    if (InterfaceAST->isGenericTemplate()) {
      RegisterGenericTemplate(std::move(InterfaceAST));
      return;
    }
    if (auto InterfaceType = InterfaceAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated interface type:\n");
      InterfaceType->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  } else {
    getNextToken();
  }
}

void HandleDelegateDefinition() {
  if (auto DelegateAST = ParseDelegateDefinition()) {
    if (!DelegateAST->codegen())
      reportCompilerError("Failed to register delegate type");
  } else {
    getNextToken();
  }
}

void HandleAbstractComposite() {
  getNextToken(); // eat 'abstract'
  while (CurTok == tok_newline)
    getNextToken();

  if (CurTok == tok_class) {
    HandleClassDefinition(true);
  } else if (CurTok == tok_interface) {
    HandleInterfaceDefinition(true);
  } else {
    reportCompilerError("Expected 'class' or 'interface' after 'abstract'");
  }
}

void HandleUnsafe() {
  getNextToken(); // eat 'unsafe'

  if (CurTok == tok_struct) {
    // Handle unsafe struct definition
    enterUnsafeContext();
    HandleStructDefinition();
    exitUnsafeContext();
  } else if (CurTok == tok_class) {
    enterUnsafeContext();
    HandleClassDefinition();
    exitUnsafeContext();
  } else if (CurTok == tok_interface) {
    enterUnsafeContext();
    HandleInterfaceDefinition();
    exitUnsafeContext();
  } else if (CurTok == tok_extern) {
    // Support 'unsafe extern' prototypes.
    enterUnsafeContext();
    HandleExtern();
    exitUnsafeContext();
  } else {
    // Handle unsafe function definition
    enterUnsafeContext();
    HandleDefinition();
    exitUnsafeContext();
  }
}

void HandleSwitchStatement() {
  if (auto SwitchAST = ParseSwitchStatement()) {
    if (gInteractiveMode) fprintf(stderr, "Parsed switch statement successfully, generating code...\n");
    if (auto SwitchIR = SwitchAST->codegen()) {
      NoteTopLevelStatementEmitted();
      if (gInteractiveMode) fprintf(stderr, "Generated switch statement IR:\n");
      SwitchIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate IR for switch statement");
    }
  } else {
    reportCompilerError("Failed to parse switch statement");
    // Skip token for error recovery.
    getNextToken();
  }
}


/// top ::= definition | external | expression | variabledecl | foreachstmt | usestmt | ';' | '\n'
void MainLoop() {
  while (true) {
    if (gInteractiveMode)
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
    case tok_if:
      HandleIfStatement();
      break;
    case tok_while:
      HandleWhileStatement();
      break;
    case tok_assert:
      HandleAssertStatement();
      break;
    case tok_struct:
      HandleStructDefinition();
      break;
    case tok_class:
      HandleClassDefinition();
      break;
    case tok_interface:
      HandleInterfaceDefinition();
      break;
    case tok_delegate:
      HandleDelegateDefinition();
      break;
    case tok_abstract:
      HandleAbstractComposite();
      break;
    case tok_switch:
      HandleSwitchStatement();
      break;
    case tok_unsafe:
      // Handle unsafe functions or unsafe structs
      HandleUnsafe();
      break;
    case tok_ref:
      // Handle ref variable declarations
      getNextToken(); // eat 'ref'
      if (!ParseTypeIdentifier(true)) {
        getNextToken();
      }
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
      {
        if (StructNames.contains(IdentifierStr) ||
            DelegateNames.contains(IdentifierStr)) {
          if (!ParseTypeIdentifier()) {
            // If parsing as a type-prefixed declaration failed,
            // consume the current token to avoid stalling
            getNextToken();
          }
        } else {
          // For C-style declarations, functions start with type keywords,
          // so a bare identifier is likely an expression
          HandleTopLevelExpression();
        }
      }
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

#undef IdentifierStr
#undef StructNames
#undef DelegateNames
#undef CurTok
