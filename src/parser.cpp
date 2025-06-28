#include "parser.h"
#include "lexer.h"
#include <cstdio>

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
int CurTok;
int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
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

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
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
    int BinOp = CurTok;
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
      CurTok != tok_double && CurTok != tok_char && CurTok != tok_void)
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
        CurTok != tok_double && CurTok != tok_char && CurTok != tok_void)
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

/// variabledecl ::= type identifier ('=' expression)?
std::unique_ptr<VariableDeclarationStmtAST> ParseVariableDeclaration() {
  if (CurTok != tok_int && CurTok != tok_float && CurTok != tok_double && 
      CurTok != tok_char && CurTok != tok_void)
    return nullptr;
    
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  if (CurTok != tok_identifier)
    return nullptr;
    
  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier
  
  std::unique_ptr<ExprAST> Initializer = nullptr;
  
  if (CurTok == '=') {
    getNextToken(); // eat '='
    Initializer = ParseExpression();
    if (!Initializer)
      return nullptr;
  }
  
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
      CurTok != tok_char && CurTok != tok_void) {
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

/// statement ::= returnstmt | variabledecl | foreachstmt | usestmt | expressionstmt
std::unique_ptr<StmtAST> ParseStatement() {
  switch (CurTok) {
  case tok_return:
    return ParseReturnStatement();
  case tok_for:
    return ParseForEachStatement();
  case tok_use:
    return ParseUseStatement();
  case tok_int:
  case tok_float:
  case tok_double:
  case tok_char:
  case tok_void:
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
            CurTok != tok_double && CurTok != tok_char && CurTok != tok_void) {
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
    fprintf(stderr, "Parsed a function definition.\n");
    
  } else {
    // It's a variable declaration
    std::unique_ptr<ExprAST> Initializer = nullptr;
    
    if (CurTok == '=') {
      getNextToken(); // eat '='
      Initializer = ParseExpression();
      if (!Initializer) {
        fprintf(stderr, "Error: Expected expression after '='\n");
        return;
      }
    }
    
    auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(Type, Name, std::move(Initializer));
    fprintf(stderr, "Parsed a variable declaration.\n");
  }
}