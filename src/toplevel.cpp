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
#define IdentifierStr (currentLexer().identifierStr)

void SetInteractiveMode(bool enabled) {
  gInteractiveMode = enabled;
}

void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (gInteractiveMode) fprintf(stderr, "Parsed function successfully, generating code...\n");
    if (auto FnIR = FnAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated function IR:\n");
      FnIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
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
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto FnIR = FnAST->codegen()) {
      if (gInteractiveMode) fprintf(stderr, "Generated top-level expression IR:\n");
      FnIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
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
      if (gInteractiveMode) fprintf(stderr, "Generated variable declaration IR:\n");
      VarIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
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
      if (gInteractiveMode) fprintf(stderr, "Generated top-level assert IR:\n");
      AssertIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
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
  if (auto StructAST = ParseStructDefinition()) {
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

void HandleUnsafe() {
  getNextToken(); // eat 'unsafe'

  if (CurTok == tok_struct) {
    // Handle unsafe struct definition
    enterUnsafeContext();
    HandleStructDefinition();
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
      if (gInteractiveMode) fprintf(stderr, "Generated switch statement IR:\n");
      SwitchIR->print(llvm::errs());
      if (gInteractiveMode) fprintf(stderr, "\n");
    } else {
      fprintf(stderr, "Error: Failed to generate IR for switch statement\n");
    }
  } else {
    fprintf(stderr, "Error: Failed to parse switch statement\n");
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
    case tok_assert:
      HandleAssertStatement();
      break;
    case tok_struct:
      HandleStructDefinition();
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
        // Check if this identifier is a struct type
        if (StructNames.contains(IdentifierStr)) {
          // It's a struct type, need to look ahead to determine what it is
          std::string structName = IdentifierStr;
          getNextToken(); // consume the struct name
          
          if (CurTok == '(') {
            // Struct constructor call
            // Put the tokens back for expression parsing
            // Since can't easily "unget" tokens, create a CallExprAST directly
            getNextToken(); // eat '('
            std::vector<std::unique_ptr<ExprAST>> Args;
            if (CurTok != ')') {
              while (true) {
                if (auto Arg = ParseExpression())
                  Args.push_back(std::move(Arg));
                else {
                  getNextToken(); // error recovery
                  break;
                }
                
                if (CurTok == ')')
                  break;
                  
                if (CurTok != ',') {
                  fprintf(stderr, "Error: Expected ')' or ',' in argument list\n");
                  getNextToken(); // error recovery
                  break;
                }
                getNextToken();
              }
            }
            
            if (CurTok == ')') {
              getNextToken(); // eat ')'
              
              // Create and codegen the constructor call
              auto Call = std::make_unique<CallExprAST>(structName, std::move(Args));
              if (auto CallIR = Call->codegen()) {
                if (gInteractiveMode) fprintf(stderr, "Generated struct instantiation IR:\n");
                CallIR->print(llvm::errs());
                if (gInteractiveMode) fprintf(stderr, "\n");
              } else {
                fprintf(stderr, "Error: Failed to generate struct instantiation\n");
              }
            }
          } else {
            // Variable declaration
            // The struct name has been consumed, current token should be the variable name
            if (CurTok == tok_identifier) {
              std::string varName = IdentifierStr;
              getNextToken(); // eat variable name
              
              if (CurTok == '=') {
                getNextToken(); // eat '='
                auto Init = ParseExpression();
                if (Init) {
                  TypeInfo declInfo;
                  declInfo.typeName = structName;
                  declInfo.pointerDepth = 0;
                  declInfo.isArray = false;
                  declInfo.refStorage = RefStorageClass::None;
                  declInfo.isMutable = true;
                  declInfo.declaredRef = false;
                  auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(std::move(declInfo), varName, std::move(Init));
                  if (auto VarIR = VarDecl->codegen()) {
                  if (gInteractiveMode) fprintf(stderr, "Generated variable declaration IR:\n");
                  VarIR->print(llvm::errs());
                  if (gInteractiveMode) fprintf(stderr, "\n");
                  }
                }
              } else {
                fprintf(stderr, "Error: Expected '=' after variable name (all variables must be initialized)\n");
                getNextToken(); // error recovery
              }
            } else if (CurTok == '[') {
              // Array type - put back the struct name processing
              // Need to restore the state
              IdentifierStr = structName;
              CurTok = tok_identifier;
              if (!ParseTypeIdentifier()) {
                getNextToken();
              }
            } else {
              fprintf(stderr, "Error: Expected variable name or '(' after struct type '%s'\n", structName.c_str());
              // Token already consumed, just continue
            }
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
#undef CurTok
