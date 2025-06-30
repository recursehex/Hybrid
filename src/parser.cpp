#include "parser.h"
#include "lexer.h"
#include <cstdio>
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
int CurTok;
int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
std::map<std::string, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int GetTokPrecedence() {
  // Handle comparison operator tokens
  if (CurTok == tok_eq) {
    return BinopPrecedence["=="];
  } else if (CurTok == tok_ne) {
    return BinopPrecedence["!="];
  } else if (CurTok == tok_le) {
    return BinopPrecedence["<="];
  } else if (CurTok == tok_ge) {
    return BinopPrecedence[">="];
  } else if (CurTok == tok_lt) {
    return BinopPrecedence["<"];
  } else if (CurTok == tok_gt) {
    return BinopPrecedence[">"];
  } else if (CurTok == tok_and) {
    return BinopPrecedence["&&"];
  } else if (CurTok == tok_or) {
    return BinopPrecedence["||"];
  }
  
  // Handle single character operators
  if (isascii(CurTok)) {
    std::string Op(1, (char)CurTok);
    int TokPrec = BinopPrecedence[Op];
    if (TokPrec > 0)
      return TokPrec;
  }
  
  return -1;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}
std::unique_ptr<StmtAST> LogErrorS(const char *Str) {
  LogError(Str);
  return nullptr;
}

/// numberexpr ::= number
std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// unaryexpr ::= ('-' | '!') primary
std::unique_ptr<ExprAST> ParseUnaryExpr() {
  int UnaryOp = CurTok;
  getNextToken(); // eat the unary operator
  
  auto Operand = ParsePrimary();
  if (!Operand)
    return nullptr;
    
  if (UnaryOp == '-') {
    // Create a binary expression: 0 - operand
    auto Zero = std::make_unique<NumberExprAST>(0.0);
    return std::make_unique<BinaryExprAST>("-", std::move(Zero), std::move(Operand));
  } else if (UnaryOp == tok_not) {
    // Create a binary expression: operand == false (logical NOT)
    auto False = std::make_unique<BoolExprAST>(false);
    return std::make_unique<BinaryExprAST>("==", std::move(Operand), std::move(False));
  }
  
  return LogError("Unknown unary operator");
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= boolexpr
///   ::= stringexpr
///   ::= charexpr
std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case tok_true:
    getNextToken(); // consume 'true'
    return std::make_unique<BoolExprAST>(true);
  case tok_false:
    getNextToken(); // consume 'false'
    return std::make_unique<BoolExprAST>(false);
  case tok_null:
    getNextToken(); // consume 'null'
    return std::make_unique<NullExprAST>();
  case tok_string_literal:
    {
      auto Result = std::make_unique<StringExprAST>(StringVal);
      getNextToken(); // consume the string literal
      return std::move(Result);
    }
  case tok_char_literal:
    {
      auto Result = std::make_unique<CharExprAST>(CharVal);
      getNextToken(); // consume the character literal
      return std::move(Result);
    }
  case '(':
    return ParseParenExpr();
  case '-':
  case tok_not:
    return ParseUnaryExpr();
  }
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    std::string BinOp;
    
    // Convert token to operator string
    if (CurTok == tok_eq) {
      BinOp = "==";
    } else if (CurTok == tok_ne) {
      BinOp = "!=";
    } else if (CurTok == tok_le) {
      BinOp = "<=";
    } else if (CurTok == tok_ge) {
      BinOp = ">=";
    } else if (CurTok == tok_lt) {
      BinOp = "<";
    } else if (CurTok == tok_gt) {
      BinOp = ">";
    } else if (CurTok == tok_and) {
      BinOp = "&&";
    } else if (CurTok == tok_or) {
      BinOp = "||";
    } else if (isascii(CurTok)) {
      BinOp = std::string(1, (char)CurTok);
    } else {
      return LogError("Unknown binary operator");
    }
    
    getNextToken(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id id '(' (id id (',' id id)*)? ')'  // C-style: returntype name(type param, type param, ...)
std::unique_ptr<PrototypeAST> ParsePrototype() {
  // Parse return type
  if (CurTok != tok_identifier && CurTok != tok_int && CurTok != tok_float && 
      CurTok != tok_double && CurTok != tok_char && CurTok != tok_void && CurTok != tok_bool && CurTok != tok_string)
    return LogErrorP("Expected return type in prototype");

  std::string ReturnType = IdentifierStr;
  getNextToken();

  if (CurTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<Parameter> Args;
  getNextToken(); // eat '('
  
  // Handle empty parameter list
  if (CurTok == ')') {
    getNextToken(); // eat ')'
    return std::make_unique<PrototypeAST>(ReturnType, FnName, std::move(Args));
  }
  
  // Parse first parameter
  while (true) {
    if (CurTok != tok_identifier && CurTok != tok_int && CurTok != tok_float && 
        CurTok != tok_double && CurTok != tok_char && CurTok != tok_void && CurTok != tok_bool && CurTok != tok_string)
      return LogErrorP("Expected parameter type");
    
    std::string ParamType = IdentifierStr;
    getNextToken();
    
    if (CurTok != tok_identifier)
      return LogErrorP("Expected parameter name");
    
    std::string ParamName = IdentifierStr;
    getNextToken();
    
    Args.emplace_back(ParamType, ParamName);
    
    if (CurTok == ')') {
      getNextToken(); // eat ')'
      break;
    }
    
    if (CurTok != ',')
      return LogErrorP("Expected ',' or ')' in parameter list");
    
    getNextToken(); // eat ','
  }

  return std::make_unique<PrototypeAST>(ReturnType, FnName, std::move(Args));
}

/// definition ::= prototype block
std::unique_ptr<FunctionAST> ParseDefinition() {
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  auto Body = ParseBlock();
  if (!Body)
    return nullptr;
    
  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}

/// toplevelexpr ::= expression
std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("void", "__anon_expr",
                                                std::vector<Parameter>());
    
    // Wrap the expression in a return statement and block
    auto ReturnStmt = std::make_unique<ReturnStmtAST>(std::move(E));
    std::vector<std::unique_ptr<StmtAST>> Statements;
    Statements.push_back(std::move(ReturnStmt));
    auto Body = std::make_unique<BlockStmtAST>(std::move(Statements));
    
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern.
  return ParsePrototype();
}

/// returnstmt ::= 'return' expression?
std::unique_ptr<ReturnStmtAST> ParseReturnStatement() {
  getNextToken(); // eat 'return'
  
  // Check if this is a void return (no expression)
  if (CurTok == tok_newline || CurTok == ';' || CurTok == '}') {
    // Return with no value
    return std::make_unique<ReturnStmtAST>(nullptr);
  }
  
  auto ReturnValue = ParseExpression();
  if (!ReturnValue)
    return nullptr;
    
  return std::make_unique<ReturnStmtAST>(std::move(ReturnValue));
}

/// variabledecl ::= type identifier '=' expression
std::unique_ptr<VariableDeclarationStmtAST> ParseVariableDeclaration() {
  if (CurTok != tok_int && CurTok != tok_float && CurTok != tok_double && 
      CurTok != tok_char && CurTok != tok_void && CurTok != tok_bool && CurTok != tok_string)
    return nullptr;
    
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  if (CurTok != tok_identifier)
    return nullptr;
    
  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier
  
  if (CurTok != '=') {
    LogError("Expected '=' after variable name (all variables must be initialized)");
    return nullptr;
  }
  
  getNextToken(); // eat '='
  std::unique_ptr<ExprAST> Initializer = ParseExpression();
  if (!Initializer)
    return nullptr;
  
  return std::make_unique<VariableDeclarationStmtAST>(Type, Name, std::move(Initializer));
}

/// foreachstmt ::= 'for' type identifier 'in' expression block
std::unique_ptr<ForEachStmtAST> ParseForEachStatement() {
  getNextToken(); // eat 'for'
  
  // Skip newlines after 'for'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse type
  if (CurTok != tok_int && CurTok != tok_float && CurTok != tok_double &&
      CurTok != tok_char && CurTok != tok_void && CurTok != tok_bool && CurTok != tok_string) {
    LogError("Expected type after 'for'");
    return nullptr;
  }
  
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  // Parse variable name
  if (CurTok != tok_identifier) {
    LogError("Expected identifier after type in foreach loop");
    return nullptr;
  }
  
  std::string VarName = IdentifierStr;
  getNextToken(); // eat identifier
  
  // Parse 'in'
  if (CurTok != tok_in) {
    LogError("Expected 'in' after variable name in foreach loop");
    return nullptr;
  }
  getNextToken(); // eat 'in'
  
  // Parse collection expression
  auto Collection = ParseExpression();
  if (!Collection) {
    LogError("Expected expression after 'in' in foreach loop");
    return nullptr;
  }
  
  // Parse body block
  auto Body = ParseBlock();
  if (!Body) {
    LogError("Expected block after foreach loop header");
    return nullptr;
  }
  
  return std::make_unique<ForEachStmtAST>(Type, VarName, std::move(Collection), std::move(Body));
}

/// usestmt ::= 'use' identifier
std::unique_ptr<UseStmtAST> ParseUseStatement() {
  getNextToken(); // eat 'use'
  
  // Skip newlines after 'use'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse module name
  if (CurTok != tok_identifier) {
    LogError("Expected module name after 'use'");
    return nullptr;
  }
  
  std::string Module = IdentifierStr;
  getNextToken(); // eat module name
  
  return std::make_unique<UseStmtAST>(Module);
}

/// ifstmt ::= 'if' expression block ('else' (ifstmt | block))?
std::unique_ptr<IfStmtAST> ParseIfStatement() {
  getNextToken(); // eat 'if'
  
  // Skip newlines after 'if'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse condition expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected condition after 'if'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse then block
  if (CurTok != '{') {
    LogError("Expected '{' after if condition");
    return nullptr;
  }
  
  auto ThenBranch = ParseBlock();
  if (!ThenBranch)
    return nullptr;
  
  // Skip newlines after then block
  while (CurTok == tok_newline)
    getNextToken();
  
  // Check for else branch
  std::unique_ptr<StmtAST> ElseBranch = nullptr;
  if (CurTok == tok_else) {
    getNextToken(); // eat 'else'
    
    // Skip newlines after 'else'
    while (CurTok == tok_newline)
      getNextToken();
    
    if (CurTok == tok_if) {
      // else if case
      ElseBranch = ParseIfStatement();
    } else if (CurTok == '{') {
      // else block case
      ElseBranch = ParseBlock();
    } else {
      LogError("Expected '{' or 'if' after 'else'");
      return nullptr;
    }
    
    if (!ElseBranch)
      return nullptr;
  }
  
  return std::make_unique<IfStmtAST>(std::move(Condition), std::move(ThenBranch), std::move(ElseBranch));
}

/// whilestmt ::= 'while' expression block
std::unique_ptr<WhileStmtAST> ParseWhileStatement() {
  getNextToken(); // eat 'while'
  
  // Skip newlines after 'while'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse condition expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected condition after 'while'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse body block
  if (CurTok != '{') {
    LogError("Expected '{' after while condition");
    return nullptr;
  }
  
  auto Body = ParseBlock();
  if (!Body)
    return nullptr;
  
  return std::make_unique<WhileStmtAST>(std::move(Condition), std::move(Body));
}

/// statement ::= returnstmt | variabledecl | foreachstmt | usestmt | ifstmt | whilestmt | expressionstmt
std::unique_ptr<StmtAST> ParseStatement() {
  switch (CurTok) {
  case tok_return:
    return ParseReturnStatement();
  case tok_for:
    return ParseForEachStatement();
  case tok_use:
    return ParseUseStatement();
  case tok_if:
    return ParseIfStatement();
  case tok_while:
    return ParseWhileStatement();
  case tok_int:
  case tok_float:
  case tok_double:
  case tok_char:
  case tok_void:
  case tok_bool:
  case tok_string:
    return ParseVariableDeclaration();
  case '}':
    // End of block - not a statement, but not an error either
    // Let the caller (ParseBlock) handle this
    return nullptr;
  default:
    // Try to parse as an expression statement
    auto Expr = ParseExpression();
    if (!Expr)
      return nullptr;
    return std::make_unique<ExpressionStmtAST>(std::move(Expr));
  }
}

/// block ::= '{' statement* '}'
std::unique_ptr<BlockStmtAST> ParseBlock() {
  // Skip newlines before opening brace (for Allman style)
  while (CurTok == tok_newline)
    getNextToken();
    
  if (CurTok != '{')
    return nullptr;
    
  getNextToken(); // eat '{'
  
  std::vector<std::unique_ptr<StmtAST>> Statements;
  
  // Skip newlines after opening brace
  while (CurTok == tok_newline)
    getNextToken();
  
  while (CurTok != '}' && CurTok != tok_eof) {
    // Skip newlines between statements
    if (CurTok == tok_newline) {
      getNextToken();
      continue;
    }
    
    auto Stmt = ParseStatement();
    if (!Stmt) {
      // Check if we hit the closing brace
      if (CurTok == '}')
        break;  // Normal end of block
      return nullptr;  // Actual error
    }
      
    Statements.push_back(std::move(Stmt));
    
    // Skip newlines after statements
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}')
    return nullptr;
    
  getNextToken(); // eat '}'
  
  return std::make_unique<BlockStmtAST>(std::move(Statements));
}

/// ParseTypeIdentifier - Handles top-level declarations that start with a type
/// Could be either a variable declaration or function definition
void ParseTypeIdentifier() {
  // Save the type information
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  if (CurTok != tok_identifier) {
    fprintf(stderr, "Error: Expected identifier after type\n");
    return;
  }
  
  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier
  
  // Check what follows
  if (CurTok == '(') {
    // It's a function definition
    // We need to parse the full prototype and then the body
    getNextToken(); // eat '('
    
    std::vector<Parameter> Args;
    
    // Parse parameters
    if (CurTok != ')') {
      while (true) {
        if (CurTok != tok_identifier && CurTok != tok_int && CurTok != tok_float && 
            CurTok != tok_double && CurTok != tok_char && CurTok != tok_void && CurTok != tok_bool && CurTok != tok_string) {
          fprintf(stderr, "Error: Expected parameter type\n");
          return;
        }
        
        std::string ParamType = IdentifierStr;
        getNextToken();
        
        if (CurTok != tok_identifier) {
          fprintf(stderr, "Error: Expected parameter name\n");
          return;
        }
        
        std::string ParamName = IdentifierStr;
        getNextToken();
        
        Args.emplace_back(ParamType, ParamName);
        
        if (CurTok == ')') {
          getNextToken(); // eat ')'
          break;
        }
        
        if (CurTok != ',') {
          fprintf(stderr, "Error: Expected ',' or ')' in parameter list\n");
          return;
        }
        
        getNextToken(); // eat ','
      }
    } else {
      getNextToken(); // eat ')'
    }
    
    // Create prototype
    auto Proto = std::make_unique<PrototypeAST>(Type, Name, std::move(Args));
    
    // Parse the body
    auto Body = ParseBlock();
    if (!Body) {
      fprintf(stderr, "Error: Expected function body\n");
      return;
    }
    
    auto Fn = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    fprintf(stderr, "Parsed function successfully, generating code...\n");
    if (auto FnIR = Fn->codegen()) {
      fprintf(stderr, "Generated function IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      fprintf(stderr, "Error: Failed to generate IR for function\n");
    }
    
  } else {
    // It's a variable declaration
    if (CurTok != '=') {
      fprintf(stderr, "Error: Expected '=' after variable name (all variables must be initialized)\n");
      return;
    }
    
    getNextToken(); // eat '='
    auto Initializer = ParseExpression();
    if (!Initializer) {
      fprintf(stderr, "Error: Expected expression after '='\n");
      return;
    }
    
    auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(Type, Name, std::move(Initializer));
    fprintf(stderr, "Parsed variable declaration, generating code...\n");
    
    // Wrap the variable declaration in an anonymous function like top-level expressions
    auto Proto = std::make_unique<PrototypeAST>("void", "__anon_var_decl", std::vector<Parameter>());
    std::vector<std::unique_ptr<StmtAST>> Statements;
    Statements.push_back(std::move(VarDecl));
    auto Body = std::make_unique<BlockStmtAST>(std::move(Statements));
    auto Fn = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    
    if (auto FnIR = Fn->codegen()) {
      fprintf(stderr, "Generated variable declaration IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      fprintf(stderr, "Error: Failed to generate IR for variable declaration\n");
    }
  }
}