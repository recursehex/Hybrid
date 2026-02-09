#include "parser/parser_internal.h"
#include <cctype>
#include <optional>

std::unique_ptr<ExprAST> ParseUnaryExpr() {
  // If the token is not a unary operator, it must be a primary expression.
  if (CurTok != '-' && CurTok != tok_not && CurTok != tok_inc &&
      CurTok != tok_dec && CurTok != tok_hash && CurTok != tok_at) {
    return ParsePrimary();
  }

  int Opc = CurTok;
  bool parsedInUnsafe = false;
  SourceLocation opLoc = currentParser().currentTokenLocation;
  std::string OpStr;
  if (Opc == '-')
    OpStr = "-";
  else if (Opc == tok_not)
    OpStr = "!";
  else if (Opc == tok_inc)
    OpStr = "++";
  else if (Opc == tok_dec)
    OpStr = "--";
  else if (Opc == tok_hash) {
    OpStr = "#"; // Address-of operator
    parsedInUnsafe = isInUnsafeContext();
  } else if (Opc == tok_at) {
    OpStr = "@"; // Dereference operator
    parsedInUnsafe = isInUnsafeContext();
  }

  getNextToken(); // eat the operator.

  // For dereference operator (@), parse with postfix to get lower precedence
  // This makes @holder.pointers[0] parse as @(holder.pointers[0])
  auto Operand = (OpStr == "@") ? ParsePrimaryWithPostfix() : ParseUnaryExpr();
  if (!Operand)
    return nullptr;

  if (OpStr == "-") {
    // Represent -x as 0 - x
    auto zero = withLocation(
        std::make_unique<NumberExprAST>(NumericLiteral::fromSigned(0)), opLoc);
    auto expr = std::make_unique<BinaryExprAST>("-", std::move(zero),
                                                std::move(Operand));
    expr->setSourceLocation(opLoc);
    return expr;
  }
  auto expr = std::make_unique<UnaryExprAST>(OpStr, std::move(Operand),
                                             true /* isPrefix */, parsedInUnsafe);
  expr->setSourceLocation(opLoc);
  return expr;
}

/// binoprhs
///   ::= ('+' primary)*
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                       std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence
  while (true) {
    int TokPrec = GetTokPrecedence();

    if (CurTok == tok_if) {
      ParserContext &parserCtx = currentParser();
      int prevTok = parserCtx.previousToken;
      bool locationsValid = parserCtx.previousTokenLocation.isValid() &&
                            parserCtx.currentTokenLocation.isValid();
      bool sameLine =
          locationsValid &&
          parserCtx.previousTokenLocation.line ==
              parserCtx.currentTokenLocation.line;
      bool atBoundary =
          prevTok == 0 || prevTok == tok_newline || prevTok == ';' ||
          prevTok == '{' || prevTok == '}' || prevTok == tok_eof ||
          prevTok == tok_else;

      if (!sameLine || atBoundary)
        return LHS;
    }

    // If this is a binop that binds at least as tightly as the current binop,
    // Consume it, otherwise done
    if (TokPrec < ExprPrec)
      return LHS;

    // Check for ternary operator (special case)
    if (CurTok == tok_if) {
      return ParseTernaryExpression(std::move(LHS));
    }

    if (CurTok == tok_is) {
      if (!currentParser().allowTypeCheck) {
        LogError("Type checks with 'is' are only supported in conditional expressions");
        RecoverAfterExpressionError();
        return nullptr;
      }

      SourceLocation opLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat 'is'
      SkipNewlines();

      bool isNegated = false;
      if (CurTok == tok_not_kw) {
        isNegated = true;
        getNextToken(); // eat 'not'
        SkipNewlines();
      }

      bool targetIsNull = false;
      TypeInfo targetInfo;
      if (CurTok == tok_null) {
        targetIsNull = true;
        getNextToken(); // eat 'null'
      } else {
        if (!ParseCompleteTypeInfo(targetInfo, false)) {
          RecoverAfterExpressionError();
          return nullptr;
        }
      }

      if (targetIsNull && CurTok == tok_identifier) {
        LogError("Pattern binding is not supported for 'null' checks");
        RecoverAfterExpressionError();
        return nullptr;
      }

      std::optional<std::string> bindingName;
      if (!targetIsNull && CurTok == tok_identifier) {
        bindingName = IdentifierStr;
        getNextToken(); // eat binding name
      }

      auto expr = std::make_unique<TypeCheckExprAST>(
          std::move(LHS), std::move(targetInfo), isNegated, targetIsNull,
          std::move(bindingName));
      expr->setSourceLocation(opLoc);
      LHS = std::move(expr);
      continue;
    }

    // Now know this is a binop
    SourceLocation opLoc = currentParser().currentTokenLocation;
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
    } else if (CurTok == tok_null_coalescing) {
      BinOp = "\?\?";
    } else if (CurTok == tok_null_coalescing_assign) {
      BinOp = "\?\?=";
    } else if (isascii(CurTok)) {
      BinOp = std::string(1, (char)CurTok);
    } else {
      return LogError(
          "Unsupported binary operator",
          "Check the operator spelling or ensure it is supported in this context.");
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
         BinOp == "^=" || BinOp == "<<=" || BinOp == ">>=" ||
         BinOp == "\?\?=")) {
      if (BinaryExprAST *RHSBinary = dynamic_cast<BinaryExprAST *>(RHS.get())) {
        if (RHSBinary->getOp() == "=" || RHSBinary->getOp() == "+=" ||
            RHSBinary->getOp() == "-=" || RHSBinary->getOp() == "*=" ||
            RHSBinary->getOp() == "/=" || RHSBinary->getOp() == "%=" ||
            RHSBinary->getOp() == "&=" || RHSBinary->getOp() == "|=" ||
            RHSBinary->getOp() == "^=" || RHSBinary->getOp() == "<<=" ||
            RHSBinary->getOp() == ">>=" || RHSBinary->getOp() == "\?\?=") {
          return LogError("Chained assignment is not allowed - variables must be assigned one at a time");
        }
      }
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    LHS->setSourceLocation(opLoc);
  }
}

/// expression
///   ::= primary binoprhs
///
std::unique_ptr<ExprAST> ParseConditionExpression() {
  ScopedTypeCheckContext allowTypeChecks(true);
  return ParseExpression();
}

std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimaryWithPostfix();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

void RecoverAfterExpressionError() {
  constexpr int RecoveryTokens[] = {tok_newline, ';', ')', ']', '}', tok_eof};

  auto isRecoveryToken = [&](int tok) {
    for (int candidate : RecoveryTokens) {
      if (tok == candidate)
        return true;
    }
    return false;
  };

  while (!isRecoveryToken(CurTok)) {
    if (CurTok == tok_eof)
      return;
    getNextToken();
  }

  if (CurTok == tok_newline || CurTok == ';')
    getNextToken();
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

    // For expression switches, expect value => expr format or default => expr
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

  return std::make_unique<SwitchExprAST>(std::move(Condition),
                                         std::move(Cases));
}

/// ternaryexpr ::= thenexpr 'if' condition 'else' elseexpr
std::unique_ptr<TernaryExprAST>
ParseTernaryExpression(std::unique_ptr<ExprAST> ThenExpr) {
  getNextToken(); // eat 'if'

  // Skip newlines after 'if'
  while (CurTok == tok_newline)
    getNextToken();

  // Parse condition expression
  auto Condition = ParseConditionExpression();
  if (!Condition) {
    LogError("Expected condition after 'if' in ternary expression");
    return nullptr;
  }

  // Skip newlines before 'else'
  while (CurTok == tok_newline)
    getNextToken();

  // Expect 'else'
  if (CurTok != tok_else) {
    LogError("Expected 'else' after condition",
             "Ternary expressions require both a true and false branch.");
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

  return std::make_unique<TernaryExprAST>(std::move(ThenExpr),
                                          std::move(Condition),
                                          std::move(ElseExpr));
}
