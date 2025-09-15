#include "parser.h"
#include "lexer.h"
#include <cstdio>
#include <set>
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

/// StructNames - Track struct names as valid types
std::set<std::string> StructNames;

/// Helper function to check if current token is a built-in type
bool IsBuiltInType()
{
  return CurTok == tok_int || CurTok == tok_float || CurTok == tok_double ||
         CurTok == tok_char || CurTok == tok_void || CurTok == tok_bool ||
         CurTok == tok_string || CurTok == tok_byte || CurTok == tok_short ||
         CurTok == tok_long || CurTok == tok_sbyte || CurTok == tok_ushort ||
         CurTok == tok_uint || CurTok == tok_ulong || CurTok == tok_schar ||
         CurTok == tok_lchar;
}

/// Helper function to check if current token is a valid type (built-in or struct)
bool IsValidType()
{
  return IsBuiltInType() || 
         (CurTok == tok_identifier && StructNames.find(IdentifierStr) != StructNames.end());
}

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
  } else if (CurTok == tok_plus_eq) {
    return BinopPrecedence["+="];
  } else if (CurTok == tok_minus_eq) {
    return BinopPrecedence["-="];
  } else if (CurTok == tok_mult_eq) {
    return BinopPrecedence["*="];
  } else if (CurTok == tok_div_eq) {
    return BinopPrecedence["/="];
  } else if (CurTok == tok_mod_eq) {
    return BinopPrecedence["%="];
  } else if (CurTok == tok_bitwise_and) {
    return BinopPrecedence["&"];
  } else if (CurTok == tok_bitwise_or) {
    return BinopPrecedence["|"];
  } else if (CurTok == tok_bitwise_xor) {
    return BinopPrecedence["^"];
  } else if (CurTok == tok_left_shift) {
    return BinopPrecedence["<<"];
  } else if (CurTok == tok_right_shift) {
    return BinopPrecedence[">>"];
  } else if (CurTok == tok_and_eq) {
    return BinopPrecedence["&="];
  } else if (CurTok == tok_or_eq) {
    return BinopPrecedence["|="];
  } else if (CurTok == tok_xor_eq) {
    return BinopPrecedence["^="];
  } else if (CurTok == tok_left_shift_eq) {
    return BinopPrecedence["<<="];
  } else if (CurTok == tok_right_shift_eq) {
    return BinopPrecedence[">>="];
  } else if (CurTok == tok_if) {
    return 4; // Ternary operator precedence (higher than assignment, lower than logical OR)
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

// Forward declaration
static std::unique_ptr<ExprAST> ParsePrimaryWithPostfix();

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
///   ::= identifier '[' expression ']'
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok == '[') {
    // Array indexing
    auto ArrayVar = std::make_unique<VariableExprAST>(IdName);
    return ParseArrayIndex(std::move(ArrayVar));
  }

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
/// arrayexpr ::= '[' expression* ']'
std::unique_ptr<ExprAST> ParseArrayExpr() {
  getNextToken(); // eat [
  
  std::vector<std::unique_ptr<ExprAST>> Elements;
  
  if (CurTok != ']') {
    while (true) {
      if (auto Elem = ParseExpression())
        Elements.push_back(std::move(Elem));
      else
        return nullptr;
      
      if (CurTok == ']')
        break;
      
      if (CurTok != ',')
        return LogError("Expected ',' or ']' in array literal");
      getNextToken(); // eat ,
    }
  }
  
  if (CurTok != ']')
    return LogError("Expected ']' to close array literal");
  getNextToken(); // eat ]
  
  // For now, infer the element type from the elements
  // In a real implementation, might want to check all elements have compatible types
  std::string ElementType = "int"; // Default to int, will be improved with type inference
  
  return std::make_unique<ArrayExprAST>(ElementType, std::move(Elements));
}

/// arrayindex ::= expr '[' expression ']'
std::unique_ptr<ExprAST> ParseArrayIndex(std::unique_ptr<ExprAST> Array) {
  getNextToken(); // eat [
  
  auto Index = ParseExpression();
  if (!Index)
    return nullptr;
  
  if (CurTok != ']')
    return LogError("Expected ']' after array index");
  getNextToken(); // eat ]
  
  // Check if there's another index (for multi-dimensional arrays)
  auto Result = std::make_unique<ArrayIndexExprAST>(std::move(Array), std::move(Index));
  
  if (CurTok == '[') {
    return ParseArrayIndex(std::move(Result));
  }
  
  return Result;
}

std::unique_ptr<ExprAST> ParseUnaryExpr() {
  // If the token is not a unary operator, it must be a primary expression.
  if (CurTok != '-' && CurTok != tok_not && CurTok != tok_inc && CurTok != tok_dec) {
    return ParsePrimary();
  }

  int Opc = CurTok;
  std::string OpStr;
  if (Opc == '-') OpStr = "-";
  else if (Opc == tok_not) OpStr = "!";
  else if (Opc == tok_inc) OpStr = "++";
  else if (Opc == tok_dec) OpStr = "--";

  getNextToken(); // eat the operator.

  auto Operand = ParseUnaryExpr();
  if (!Operand)
    return nullptr;

  if (OpStr == "-") {
      // Represent -x as 0 - x
      return std::make_unique<BinaryExprAST>("-", std::make_unique<NumberExprAST>(0), std::move(Operand));
  }
  return std::make_unique<UnaryExprAST>(OpStr, std::move(Operand), true /* isPrefix */);
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= boolexpr
///   ::= stringexpr
///   ::= charexpr
///   ::= arrayexpr
std::unique_ptr<ExprAST> ParsePrimary() {
  // Check for type casting: type: expr
  if (IsBuiltInType()) {
    std::string TypeName = IdentifierStr;
    int SavedTok = CurTok;
    getNextToken(); // consume type
    
    // Check if this is a type cast (type:)
    if (CurTok == tok_colon) {
      getNextToken(); // consume ':'
      auto Operand = ParsePrimary();
      if (!Operand)
        return nullptr;
      return std::make_unique<CastExprAST>(TypeName, std::move(Operand));
    } else {
      // Not a cast, restore state and continue with default handling
      // This shouldn't happen in normal parsing flow, but handle it gracefully
      return LogError("unexpected type token in expression");
    }
  }
  
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_this:
    getNextToken(); // consume 'this'
    return std::make_unique<ThisExprAST>();
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
  case '[':
    return ParseArrayExpr();
  case '-':
  case tok_not:
  case tok_inc:
  case tok_dec:
    return ParseUnaryExpr();
  case tok_switch:
    return ParseSwitchExpression();
  }
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // Consume it, otherwise done
    if (TokPrec < ExprPrec)
      return LHS;

    // Check for ternary operator (special case)
    if (CurTok == tok_if) {
      return ParseTernaryExpression(std::move(LHS));
    }

    // Now know this is a binop
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
    } else if (CurTok == tok_plus_eq) {
      BinOp = "+=";
    } else if (CurTok == tok_minus_eq) {
      BinOp = "-=";
    } else if (CurTok == tok_mult_eq) {
      BinOp = "*=";
    } else if (CurTok == tok_div_eq) {
      BinOp = "/=";
    } else if (CurTok == tok_mod_eq) {
      BinOp = "%=";
    } else if (CurTok == tok_bitwise_and) {
      BinOp = "&";
    } else if (CurTok == tok_bitwise_or) {
      BinOp = "|";
    } else if (CurTok == tok_bitwise_xor) {
      BinOp = "^";
    } else if (CurTok == tok_left_shift) {
      BinOp = "<<";
    } else if (CurTok == tok_right_shift) {
      BinOp = ">>";
    } else if (CurTok == tok_and_eq) {
      BinOp = "&=";
    } else if (CurTok == tok_or_eq) {
      BinOp = "|=";
    } else if (CurTok == tok_xor_eq) {
      BinOp = "^=";
    } else if (CurTok == tok_left_shift_eq) {
      BinOp = "<<=";
    } else if (CurTok == tok_right_shift_eq) {
      BinOp = ">>=";
    } else if (isascii(CurTok)) {
      BinOp = std::string(1, (char)CurTok);
    } else {
      return LogError("Unknown binary operator");
    }
    
    getNextToken(); // eat binop

    // Parse the primary expression (with postfix operators) after the binary operator.
    auto RHS = ParsePrimaryWithPostfix();
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
    // For right-associative operators with equal precedence, also recurse
    else if (TokPrec == NextPrec && TokPrec == 2) { // Assignment precedence is 2
      RHS = ParseBinOpRHS(TokPrec, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Check for chained assignment - not allowed in Hybrid
    // Also check compound assignments on the LHS
    if ((BinOp == "=" || BinOp == "+=" || BinOp == "-=" || BinOp == "*=" ||
         BinOp == "/=" || BinOp == "%=" || BinOp == "&=" || BinOp == "|=" ||
         BinOp == "^=" || BinOp == "<<=" || BinOp == ">>=")) {
      if (BinaryExprAST *RHSBinary = dynamic_cast<BinaryExprAST*>(RHS.get())) {
        if (RHSBinary->getOp() == "=" || RHSBinary->getOp() == "+=" ||
            RHSBinary->getOp() == "-=" || RHSBinary->getOp() == "*=" ||
            RHSBinary->getOp() == "/=" || RHSBinary->getOp() == "%=" ||
            RHSBinary->getOp() == "&=" || RHSBinary->getOp() == "|=" ||
            RHSBinary->getOp() == "^=" || RHSBinary->getOp() == "<<=" ||
            RHSBinary->getOp() == ">>=") {
          return LogError("Chained assignment is not allowed - variables must be assigned one at a time");
        }
      }
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// Parse primary expression and any postfix operators
static std::unique_ptr<ExprAST> ParsePrimaryWithPostfix() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  // Handle postfix operators (array indexing, increment, decrement, member access)
  while (true) {
    if (CurTok == '[') {
      LHS = ParseArrayIndex(std::move(LHS));
      if (!LHS)
        return nullptr;
    } else if (CurTok == tok_inc || CurTok == tok_dec) {
      // Postfix increment/decrement
      std::string OpStr = (CurTok == tok_inc) ? "++" : "--";
      getNextToken(); // eat the operator
      LHS = std::make_unique<UnaryExprAST>(OpStr, std::move(LHS), false);
    } else if (CurTok == tok_dot) {
      // Member access
      getNextToken(); // eat '.'
      if (CurTok != tok_identifier) {
        LogError("Expected member name after '.'");
        return nullptr;
      }
      std::string MemberName = IdentifierStr;
      getNextToken(); // eat member name
      LHS = std::make_unique<MemberAccessExprAST>(std::move(LHS), MemberName);
    } else {
      break;
    }
  }
  
  return LHS;
}

/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimaryWithPostfix();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id id '(' (id id (',' id id)*)? ')'  // C-style: returntype name(type param, type param, ...)
std::unique_ptr<PrototypeAST> ParsePrototype() {
  // Parse return type
  if (!IsValidType())
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
    if (!IsValidType())
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
  if (!IsValidType())
    return nullptr;
    
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  // Check for array type
  if (CurTok == '[') {
    getNextToken(); // eat [
    if (CurTok != ']') {
      LogError("Expected ']' after '[' in array type");
      return nullptr;
    }
    getNextToken(); // eat ]
    Type += "[]"; // Add array indicator to type
  }
  
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
  if (!IsBuiltInType()) {
    LogError("Expected type after 'for'");
    return nullptr;
  }
  
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  // Check for array type
  if (CurTok == '[') {
    getNextToken(); // eat [
    if (CurTok != ']') {
      LogError("Expected ']' after '[' in array type");
      return nullptr;
    }
    getNextToken(); // eat ]
    Type += "[]"; // Add array indicator to type
  }
  
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

/// breakstmt ::= 'break'
std::unique_ptr<BreakStmtAST> ParseBreakStatement() {
  getNextToken(); // eat 'break'
  return std::make_unique<BreakStmtAST>();
}

/// skipstmt ::= 'skip'
std::unique_ptr<SkipStmtAST> ParseSkipStatement() {
  getNextToken(); // eat 'skip'
  return std::make_unique<SkipStmtAST>();
}

/// switchstmt ::= 'switch' expression '{' case* '}'
std::unique_ptr<SwitchStmtAST> ParseSwitchStatement() {
  getNextToken(); // eat 'switch'
  
  // Skip newlines after 'switch'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse switch expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected expression after 'switch'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  if (CurTok != '{') {
    LogError("Expected '{' after switch condition");
    return nullptr;
  }
  
  getNextToken(); // eat '{'
  
  // Parse cases
  std::vector<std::unique_ptr<CaseAST>> Cases;
  
  // Skip newlines after opening brace
  while (CurTok == tok_newline)
    getNextToken();
  
  while (CurTok != '}' && CurTok != tok_eof) {
    // Skip newlines between cases
    if (CurTok == tok_newline) {
      getNextToken();
      continue;
    }
    
    if (CurTok == tok_case || CurTok == tok_default) {
      auto Case = ParseCase(false); // false = statement case
      if (!Case)
        return nullptr;
      Cases.push_back(std::move(Case));
    } else {
      LogError("Expected 'case' or 'default' in switch statement");
      return nullptr;
    }
    
    // Skip newlines after case
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}') {
    LogError("Expected '}' at end of switch statement");
    return nullptr;
  }
  
  getNextToken(); // eat '}'
  
  return std::make_unique<SwitchStmtAST>(std::move(Condition), std::move(Cases));
}

/// switchexpr ::= 'switch' expression '{' caseexpr* '}'
std::unique_ptr<SwitchExprAST> ParseSwitchExpression() {
  getNextToken(); // eat 'switch'
  
  // Skip newlines after 'switch'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse switch expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected expression after 'switch'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  if (CurTok != '{') {
    LogError("Expected '{' after switch condition");
    return nullptr;
  }
  
  getNextToken(); // eat '{'
  
  // Parse cases
  std::vector<std::unique_ptr<CaseAST>> Cases;
  
  // Skip newlines after opening brace
  while (CurTok == tok_newline)
    getNextToken();
  
  while (CurTok != '}' && CurTok != tok_eof) {
    // Skip newlines between cases
    if (CurTok == tok_newline) {
      getNextToken();
      continue;
    }
    
    // For expression switches, we expect value => expr format or default => expr
    if (CurTok == tok_default) {
      auto Case = ParseCase(true); // true = expression case
      if (!Case)
        return nullptr;
      Cases.push_back(std::move(Case));
    } else {
      // Parse value(s) => expression
      auto Case = ParseCase(true); // true = expression case
      if (!Case)
        return nullptr;
      Cases.push_back(std::move(Case));
    }
    
    // Skip newlines after case
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}') {
    LogError("Expected '}' at end of switch expression");
    return nullptr;
  }
  
  getNextToken(); // eat '}'
  
  return std::make_unique<SwitchExprAST>(std::move(Condition), std::move(Cases));
}

/// ternaryexpr ::= thenexpr 'if' condition 'else' elseexpr
std::unique_ptr<TernaryExprAST> ParseTernaryExpression(std::unique_ptr<ExprAST> ThenExpr) {
  getNextToken(); // eat 'if'

  // Skip newlines after 'if'
  while (CurTok == tok_newline)
    getNextToken();

  // Parse condition expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected condition after 'if' in ternary expression");
    return nullptr;
  }

  // Skip newlines before 'else'
  while (CurTok == tok_newline)
    getNextToken();

  // Expect 'else'
  if (CurTok != tok_else) {
    LogError("Expected 'else' after condition in ternary expression");
    return nullptr;
  }

  getNextToken(); // eat 'else'

  // Skip newlines after 'else'
  while (CurTok == tok_newline)
    getNextToken();

  // Parse else expression
  auto ElseExpr = ParseExpression();
  if (!ElseExpr) {
    LogError("Expected expression after 'else' in ternary expression");
    return nullptr;
  }

  return std::make_unique<TernaryExprAST>(std::move(ThenExpr), std::move(Condition), std::move(ElseExpr));
}

/// case ::= 'case' values '{' statements '}' | value* '=>' expression
/// default ::= 'default' '{' statements '}' | 'default' '=>' expression
std::unique_ptr<CaseAST> ParseCase(bool isExpression) {
  bool isDefault = false;
  std::vector<std::unique_ptr<ExprAST>> Values;
  
  if (CurTok == tok_default) {
    isDefault = true;
    getNextToken(); // eat 'default'
  } else if (CurTok == tok_case) {
    getNextToken(); // eat 'case'
    
    // Parse comma-separated values for the case
    do {
      // Skip newlines
      while (CurTok == tok_newline)
        getNextToken();
        
      auto Value = ParseExpression();
      if (!Value) {
        LogError("Expected case value");
        return nullptr;
      }
      Values.push_back(std::move(Value));
      
      // Check for comma (multiple values)
      if (CurTok == ',') {
        getNextToken(); // eat ','
      } else {
        break;
      }
    } while (true);
  } else {
    // In switch expressions, we might have direct values without 'case'
    if (isExpression) {
      do {
        // Skip newlines
        while (CurTok == tok_newline)
          getNextToken();
          
        auto Value = ParseExpression();
        if (!Value) {
          LogError("Expected case value");
          return nullptr;
        }
        Values.push_back(std::move(Value));
        
        // Check for comma (multiple values)
        if (CurTok == ',') {
          getNextToken(); // eat ','
        } else {
          break;
        }
      } while (true);
    } else {
      LogError("Expected 'case' or 'default'");
      return nullptr;
    }
  }
  
  // Skip newlines
  while (CurTok == tok_newline)
    getNextToken();
  
  if (isExpression) {
    // Expression case: expect => expression
    if (CurTok != tok_lambda) {
      LogError("Expected '=>' in switch expression case");
      return nullptr;
    }
    
    getNextToken(); // eat '=>'
    
    // Skip newlines after '=>'
    while (CurTok == tok_newline)
      getNextToken();
    
    auto Expression = ParseExpression();
    if (!Expression) {
      LogError("Expected expression after '=>'");
      return nullptr;
    }
    
    return std::make_unique<CaseAST>(std::move(Values), std::move(Expression), isDefault);
  } else {
    // Statement case: expect { statements }
    if (CurTok != '{') {
      LogError("Expected '{' after case in switch statement");
      return nullptr;
    }
    
    auto Body = ParseBlock();
    if (!Body)
      return nullptr;
    
    return std::make_unique<CaseAST>(std::move(Values), std::move(Body), isDefault);
  }
}

/// Helper to determine which type of for loop and parse accordingly
std::unique_ptr<StmtAST> ParseForStatement() {
  // Save position to peek ahead
  int savedTok = CurTok;
  std::string savedIdentifier = IdentifierStr;
  double savedNumVal = NumVal;
  
  getNextToken(); // eat 'for'
  
  // Skip newlines after 'for'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Check if it's a type or something else
  bool hasType = IsBuiltInType();
  
  if (hasType) {
    std::string type = IdentifierStr;
    getNextToken(); // eat type
    
    // Check for array type
    if (CurTok == '[') {
      getNextToken(); // eat [
      if (CurTok == ']') {
        getNextToken(); // eat ]
        type += "[]";
      }
    }
    
    // Check if it's an identifier
    if (CurTok == tok_identifier) {
      std::string varName = IdentifierStr;
      getNextToken(); // eat identifier
      
      // Now check what comes next: 'in' for foreach, '=' for for-to
      if (CurTok == tok_in) {
        // Foreach loop, put tokens back and call ParseForEachStatement
        // Since already consumed tokens, need to continue parsing here
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
        
        return std::make_unique<ForEachStmtAST>(type, varName, std::move(Collection), std::move(Body));
      } else if (CurTok == '=') {
        // It's a for-to loop, parse it
        getNextToken(); // eat '='
        
        // Parse initialization expression
        auto InitExpr = ParseExpression();
        if (!InitExpr) {
          LogError("Expected initialization expression after '=' in for loop");
          return nullptr;
        }
        
        // Expect 'to' keyword
        if (CurTok != tok_to) {
          LogError("Expected 'to' after initialization in for loop");
          return nullptr;
        }
        getNextToken(); // eat 'to'
        
        // Check if next is the loop variable (for condition syntax)
        std::unique_ptr<ExprAST> LimitExpr = nullptr;
        std::unique_ptr<ExprAST> CondExpr = nullptr;
        
        if (CurTok == tok_identifier && IdentifierStr == varName) {
          // This is a condition expression like "i < size"
          // Parse the full condition expression
          CondExpr = ParseExpression();
          if (!CondExpr) {
            LogError("Expected condition expression after 'to' in for loop");
            return nullptr;
          }
        } else {
          // This is a regular limit expression
          LimitExpr = ParseExpression();
          if (!LimitExpr) {
            LogError("Expected limit expression after 'to' in for loop");
            return nullptr;
          }
        }
        
        // Check for optional 'by' clause
        std::unique_ptr<ExprAST> StepExpr = nullptr;
        char StepOp = '+';  // Default to addition
        if (CurTok == tok_by) {
          getNextToken(); // eat 'by'
          
          // Check for optional operator (* / % -)
          if (CurTok == '*' || CurTok == '/' || CurTok == '%' || CurTok == '-') {
            StepOp = CurTok;
            getNextToken(); // eat operator
          }
          
          StepExpr = ParseExpression();
          if (!StepExpr) {
            LogError("Expected step expression after 'by' in for loop");
            return nullptr;
          }
        }
        
        // Parse body block
        auto Body = ParseBlock();
        if (!Body) {
          LogError("Expected block after for loop header");
          return nullptr;
        }
        
        return std::make_unique<ForLoopStmtAST>(type, varName, std::move(InitExpr), 
                                                 std::move(LimitExpr), std::move(Body),
                                                 std::move(StepExpr), StepOp,
                                                 std::move(CondExpr));
      } else {
        LogError("Expected 'in' or '=' after variable name in for loop");
        return nullptr;
      }
    }
  }
  
  // Check for anonymous loop: for <expr> to <expr>
  // Reset to parse as anonymous loop
  // Need to re-parse from after 'for'
  // Current token should be the first expression
  
  // Parse initial expression (could be a number or identifier)
  auto InitExpr = ParseExpression();
  if (!InitExpr) {
    LogError("Expected expression or type after 'for'");
    return nullptr;
  }
  
  // Expect 'to' keyword
  if (CurTok != tok_to) {
    LogError("Expected 'to' after initial expression in anonymous for loop");
    return nullptr;
  }
  getNextToken(); // eat 'to'
  
  // Parse limit expression
  auto LimitExpr = ParseExpression();
  if (!LimitExpr) {
    LogError("Expected limit expression after 'to' in anonymous for loop");
    return nullptr;
  }
  
  // Check for optional 'by' clause
  std::unique_ptr<ExprAST> StepExpr = nullptr;
  char StepOp = '+';  // Default to addition
  if (CurTok == tok_by) {
    getNextToken(); // eat 'by'
    
    // Check for optional operator (* / % -)
    if (CurTok == '*' || CurTok == '/' || CurTok == '%' || CurTok == '-') {
      StepOp = CurTok;
      getNextToken(); // eat operator
    }
    
    StepExpr = ParseExpression();
    if (!StepExpr) {
      LogError("Expected step expression after 'by' in anonymous for loop");
      return nullptr;
    }
  }
  
  // Parse body block
  auto Body = ParseBlock();
  if (!Body) {
    LogError("Expected block after anonymous for loop header");
    return nullptr;
  }
  
  // Create an anonymous variable name
  static int AnonCounter = 0;
  std::string anonVarName = "__anon_loop_var_" + std::to_string(AnonCounter++);
  
  // Determine the type from the init expression (default to int)
  std::string varType = "int";
  
  return std::make_unique<ForLoopStmtAST>(varType, anonVarName, std::move(InitExpr), 
                                           std::move(LimitExpr), std::move(Body),
                                           std::move(StepExpr), StepOp,
                                           nullptr);
}

/// statement ::= returnstmt | variabledecl | foreachstmt | usestmt | ifstmt | whilestmt | expressionstmt
std::unique_ptr<StmtAST> ParseStatement() {
  switch (CurTok) {
  case tok_return:
    return ParseReturnStatement();
  case tok_for:
    return ParseForStatement();
  case tok_use:
    return ParseUseStatement();
  case tok_if:
    return ParseIfStatement();
  case tok_while:
    return ParseWhileStatement();
  case tok_break:
    return ParseBreakStatement();
  case tok_skip:
    return ParseSkipStatement();
  case tok_switch:
    return ParseSwitchStatement();
  case '}':
    // End of block - not a statement, but not an error either
    // Let the caller (ParseBlock) handle this
    return nullptr;
  default:
    // Check for valid types (built-in and struct) for variable declarations
    if (IsValidType())
      return ParseVariableDeclaration();
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
      // Check if hit closing brace
      if (CurTok == '}')
        break;  // Normal end of block
      return nullptr;  // Actual error
    }
      
    Statements.push_back(std::move(Stmt));
    
    // Skip optional semicolon after statement
    if (CurTok == ';')
      getNextToken();
    
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
bool ParseTypeIdentifier() {
  // Save the type information
  std::string Type = IdentifierStr;
  getNextToken(); // eat type
  
  // Check for array type
  if (CurTok == '[') {
    getNextToken(); // eat [
    if (CurTok != ']') {
      fprintf(stderr, "Error: Expected ']' after '[' in array type\n");
      return false;
    }
    getNextToken(); // eat ]
    Type += "[]"; // Add array indicator to type
  }
  
  if (CurTok != tok_identifier) {
    fprintf(stderr, "Error: Expected identifier after type\n");
    return false;
  }
  
  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier
  
  // Check what follows
  if (CurTok == '(') {
    // It's a function definition
    // Need to parse the full prototype and then the body
    getNextToken(); // eat '('
    
    std::vector<Parameter> Args;
    
    // Parse parameters
    if (CurTok != ')') {
      while (true) {
        if (!IsValidType()) {
          fprintf(stderr, "Error: Expected parameter type\n");
          return false;
        }
        
        std::string ParamType = IdentifierStr;
        getNextToken();
        
        // Check for array type
        if (CurTok == '[') {
          getNextToken(); // eat [
          if (CurTok != ']') {
            fprintf(stderr, "Error: Expected ']' after '[' in array type\n");
            return false;
          }
          getNextToken(); // eat ]
          ParamType += "[]"; // Add array indicator to type
        }
        
        if (CurTok != tok_identifier) {
          fprintf(stderr, "Error: Expected parameter name\n");
          return false;
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
          return false;
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
      return false;
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
    return true;
  } else if (CurTok == '=') {
    // It's a variable declaration
    getNextToken(); // eat '='
    auto Initializer = ParseExpression();
    if (!Initializer) {
      fprintf(stderr, "Error: Expected expression after '='\n");
      return false;
    }
    
    // Create the variable declaration AST but don't generate code here
    // Code generation will be handled by HandleVariableDeclaration in toplevel.cpp
    auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(Type, Name, std::move(Initializer));
    
    // Need to wrap it in a function for top-level variable declarations
    auto Proto = std::make_unique<PrototypeAST>("void", "__anon_var_decl",
                                                std::vector<Parameter>());
    
    // Wrap the variable declaration in a return statement and block
    auto ReturnStmt = std::make_unique<ReturnStmtAST>(
        std::make_unique<VariableExprAST>(Name));
    std::vector<std::unique_ptr<StmtAST>> Statements;
    Statements.push_back(std::move(VarDecl));
    Statements.push_back(std::move(ReturnStmt));
    auto Body = std::make_unique<BlockStmtAST>(std::move(Statements));
    
    auto Func = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    fprintf(stderr, "Parsed variable declaration, generating code...\n");
    
    if (auto FnIR = Func->codegen()) {
      fprintf(stderr, "Generated variable declaration IR:\n");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      fprintf(stderr, "Error: Failed to generate IR for variable declaration\n");
    }
    return true;
  } else {
    fprintf(stderr, "Error: Expected '(' or '=' after identifier in top-level declaration\n");
    return false;
  }
}

/// structdef ::= 'struct' identifier '{' (field | method)* '}'
std::unique_ptr<StructAST> ParseStructDefinition() {
  getNextToken(); // eat 'struct'
  
  // Skip newlines after 'struct'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse struct name
  if (CurTok != tok_identifier) {
    LogError("Expected struct name after 'struct'");
    return nullptr;
  }
  
  std::string StructName = IdentifierStr;
  getNextToken(); // eat struct name
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  if (CurTok != '{') {
    LogError("Expected '{' after struct name");
    return nullptr;
  }
  
  getNextToken(); // eat '{'
  
  // Skip newlines after '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  std::vector<std::unique_ptr<FieldAST>> Fields;
  std::vector<std::unique_ptr<FunctionAST>> Methods;
  
  // Parse struct members
  while (CurTok != '}' && CurTok != tok_eof) {
    // Skip newlines
    while (CurTok == tok_newline)
      getNextToken();
    
    if (CurTok == '}')
      break;
    
    // Check if this is a field or method
    // Need to look ahead to distinguish between field declarations and methods
    // Methods have the struct name as their identifier (constructor)
    if (CurTok == tok_identifier && IdentifierStr == StructName) {
      // This is a constructor
      getNextToken(); // eat struct name
      
      if (CurTok != '(') {
        LogError("Expected '(' after constructor name");
        return nullptr;
      }
      
      // Parse constructor as a function with void return type
      getNextToken(); // eat '('
      
      std::vector<Parameter> Args;
      while (CurTok != ')' && CurTok != tok_eof) {
        // Parse parameter type
        if (!IsValidType()) {
          LogError("Expected parameter type");
          return nullptr;
        }
        
        std::string ParamType = IdentifierStr;
        getNextToken(); // eat type
        
        // Parse parameter name
        if (CurTok != tok_identifier) {
          LogError("Expected parameter name");
          return nullptr;
        }
        
        std::string ParamName = IdentifierStr;
        getNextToken(); // eat name
        
        Args.push_back(Parameter(ParamType, ParamName));
        
        if (CurTok == ')') break;
        
        if (CurTok != ',') {
          LogError("Expected ')' or ',' in parameter list");
          return nullptr;
        }
        getNextToken(); // eat ','
      }
      
      if (CurTok != ')') {
        LogError("Expected ')' after parameters");
        return nullptr;
      }
      getNextToken(); // eat ')'
      
      // Skip newlines before '{'
      while (CurTok == tok_newline)
        getNextToken();
      
      // Parse constructor body
      if (CurTok != '{') {
        LogError("Expected '{' after constructor declaration");
        return nullptr;
      }
      
      auto Body = ParseBlock();
      if (!Body)
        return nullptr;
      
      // Create constructor as a method with void return type
      auto Proto = std::make_unique<PrototypeAST>("void", StructName, std::move(Args));
      auto Constructor = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
      Methods.push_back(std::move(Constructor));
      
    } else if (IsValidType()) {
      // This might be a field or a method
      std::string Type = IdentifierStr;
      getNextToken(); // eat type
      
      // Check for array type
      if (CurTok == '[') {
        getNextToken(); // eat [
        if (CurTok != ']') {
          LogError("Expected ']' after '[' in array type");
          return nullptr;
        }
        getNextToken(); // eat ]
        Type += "[]";
      }
      
      if (CurTok != tok_identifier) {
        LogError("Expected identifier after type");
        return nullptr;
      }
      
      std::string Name = IdentifierStr;
      getNextToken(); // eat identifier
      
      // Skip newlines
      while (CurTok == tok_newline)
        getNextToken();
      
      // If '(', this is a method
      if (CurTok == '(') {
        // Parse method, similar to ParsePrototype but with return type and name
        getNextToken(); // eat '('
        
        std::vector<Parameter> Args;
        while (CurTok != ')' && CurTok != tok_eof) {
          // Parse parameter type
          if (!IsValidType()) {
            LogError("Expected parameter type");
            return nullptr;
          }
          
          std::string ParamType = IdentifierStr;
          getNextToken(); // eat type
          
          // Parse parameter name
          if (CurTok != tok_identifier) {
            LogError("Expected parameter name");
            return nullptr;
          }
          
          std::string ParamName = IdentifierStr;
          getNextToken(); // eat name
          
          Args.push_back(Parameter(ParamType, ParamName));
          
          if (CurTok == ')') break;
          
          if (CurTok != ',') {
            LogError("Expected ')' or ',' in parameter list");
            return nullptr;
          }
          getNextToken(); // eat ','
        }
        
        if (CurTok != ')') {
          LogError("Expected ')' after parameters");
          return nullptr;
        }
        getNextToken(); // eat ')'
        
        // Skip newlines before '{'
        while (CurTok == tok_newline)
          getNextToken();
        
        // Parse method body
        if (CurTok != '{') {
          LogError("Expected '{' after method declaration");
          return nullptr;
        }
        
        auto Body = ParseBlock();
        if (!Body)
          return nullptr;
        
        auto Proto = std::make_unique<PrototypeAST>(Type, Name, std::move(Args));
        auto Method = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
        Methods.push_back(std::move(Method));
        
      } else {
        // This is a field - fields don't have initializers in structs
        auto Field = std::make_unique<FieldAST>(Type, Name);
        Fields.push_back(std::move(Field));
      }
    } else {
      LogError("Expected field or method declaration in struct");
      return nullptr;
    }
    
    // Skip newlines
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}') {
    LogError("Expected '}' at end of struct definition");
    return nullptr;
  }
  
  getNextToken(); // eat '}'
  
  // Add struct name to the set of valid types
  StructNames.insert(StructName);
  
  return std::make_unique<StructAST>(StructName, std::move(Fields), std::move(Methods));
}