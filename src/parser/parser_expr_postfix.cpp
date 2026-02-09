#include "parser/parser_internal.h"

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
///   ::= identifier '[' expression ']'
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;
  SourceLocation loc = currentParser().currentTokenLocation;

  getNextToken(); // eat identifier.

  if (!ParseOptionalGenericArgumentList(IdName, true))
    return nullptr;
  SkipNewlines();

  if (CurTok == '[') {
    // Array indexing
    auto ArrayVar = withLocation(std::make_unique<VariableExprAST>(IdName),
                                 loc);
    return ParseArrayIndex(std::move(ArrayVar));
  }

  if (CurTok != '(') // Simple variable ref.
    return withLocation(std::make_unique<VariableExprAST>(IdName), loc);

  std::vector<std::unique_ptr<ExprAST>> Args;
  std::vector<std::string> ArgNames;
  std::vector<SourceLocation> ArgNameLocs;
  std::vector<SourceLocation> ArgEqualLocs;
  if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
    return nullptr;

  auto call = std::make_unique<CallExprAST>(IdName, std::move(Args),
                                            std::move(ArgNames),
                                            std::move(ArgNameLocs),
                                            std::move(ArgEqualLocs));
  call->setSourceLocation(loc);
  return call;
}

/// arrayindex ::= expr '[' expression ']'
std::unique_ptr<ExprAST> ParseArrayIndex(std::unique_ptr<ExprAST> Array) {
  SourceLocation loc = currentParser().currentTokenLocation;
  getNextToken(); // eat '['

  std::vector<std::unique_ptr<ExprAST>> Indices;

  while (true) {
    auto Index = ParseExpression();
    if (!Index)
      return nullptr;

    Indices.push_back(std::move(Index));

    if (CurTok == ',') {
      getNextToken(); // eat ','
      continue;
    }
    break;
  }

  if (CurTok != ']')
    return LogError("Expected ']' after array index");
  getNextToken(); // eat ']'

  auto Result =
      std::make_unique<ArrayIndexExprAST>(std::move(Array), std::move(Indices));
  Result->setSourceLocation(loc);

  if (CurTok == '[') {
    return ParseArrayIndex(std::move(Result));
  }

  return Result;
}

std::unique_ptr<ExprAST> ParseNullSafeElementAccess(
    std::unique_ptr<ExprAST> Array) {
  SourceLocation loc = currentParser().currentTokenLocation;
  getNextToken(); // eat '?['

  auto Index = ParseExpression();
  if (!Index)
    return nullptr;

  if (CurTok != ']')
    return LogError("Expected ']' after null-safe array index");
  getNextToken(); // eat ']'

  auto expr =
      std::make_unique<NullSafeElementAccessExprAST>(std::move(Array),
                                                     std::move(Index));
  expr->setSourceLocation(loc);
  return expr;
}

bool ParseArgumentList(std::vector<std::unique_ptr<ExprAST>> &Args,
                       std::vector<std::string> &ArgNames,
                       std::vector<SourceLocation> &ArgNameLocations,
                       std::vector<SourceLocation> &ArgEqualsLocations) {
  getNextToken(); // eat '('
  SkipNewlines();

  bool sawNamedArgument = false;

  if (CurTok == ')') {
    getNextToken(); // eat ')'
    return true;
  }

  while (true) {
    SkipNewlines();

    std::string argName;
    SourceLocation argNameLoc{};
    SourceLocation equalsLoc{};
    bool isNamed = false;

    if (CurTok == tok_identifier) {
      TokenReplayScope replay(true);
      argName = IdentifierStr;
      argNameLoc = currentParser().currentTokenLocation;

      getNextToken(); // tentative consume identifier
      SkipNewlines();
      if (CurTok == '=') {
        isNamed = true;
        equalsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        replay.commit();
      } else {
        argName.clear();
        argNameLoc = {};
        equalsLoc = {};
        replay.rollback();
      }
    }

    if (sawNamedArgument && !isNamed) {
      reportCompilerError("Positional argument cannot follow a named argument");
      return false;
    }

    sawNamedArgument = sawNamedArgument || isNamed;

    SkipNewlines();
    bool argIsRef = false;
    if (CurTok == tok_ref) {
      argIsRef = true;
      getNextToken(); // eat 'ref'
      SkipNewlines();
    }

    auto Arg = ParseExpression();
    if (!Arg)
      return false;

    if (argIsRef)
      Args.push_back(std::make_unique<RefExprAST>(std::move(Arg)));
    else
      Args.push_back(std::move(Arg));

    ArgNames.push_back(std::move(argName));
    ArgNameLocations.push_back(argNameLoc);
    ArgEqualsLocations.push_back(equalsLoc);

    SkipNewlines();

    if (CurTok == ')') {
      getNextToken(); // eat ')'
      break;
    }

    if (CurTok != ',') {
      LogError("Expected ')' or ',' in argument list");
      return false;
    }

    getNextToken(); // eat ','
    SkipNewlines();
  }

  return true;
}

/// Parse primary expression and any postfix operators
std::unique_ptr<ExprAST> ParsePrimaryWithPostfix() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  // Handle postfix operators (array indexing, increment, decrement, member access)
  while (true) {
    if (CurTok == '[') {
      LHS = ParseArrayIndex(std::move(LHS));
      if (!LHS)
        return nullptr;
    } else if (CurTok == tok_null_array_access) {
      LHS = ParseNullSafeElementAccess(std::move(LHS));
      if (!LHS)
        return nullptr;
    } else if (CurTok == tok_inc || CurTok == tok_dec) {
      ParserContext &parserCtx = currentParser();
      bool locationsValid = parserCtx.previousTokenLocation.isValid() &&
                            parserCtx.currentTokenLocation.isValid();
      bool sameLine = locationsValid &&
                      parserCtx.previousTokenLocation.line ==
                          parserCtx.currentTokenLocation.line;
      bool atBoundary = parserCtx.previousToken == tok_newline ||
                        parserCtx.previousToken == ';' ||
                        parserCtx.previousToken == '{' ||
                        parserCtx.previousToken == '}' ||
                        parserCtx.previousToken == tok_eof ||
                        parserCtx.previousToken == 0;
      if (!sameLine || atBoundary)
        break;
      // Postfix increment/decrement
      SourceLocation opLoc = currentParser().currentTokenLocation;
      std::string OpStr = (CurTok == tok_inc) ? "++" : "--";
      getNextToken(); // eat the operator
      LHS = std::make_unique<UnaryExprAST>(OpStr, std::move(LHS), false);
      LHS->setSourceLocation(opLoc);
    } else if (CurTok == tok_dot) {
      // Member access
      SourceLocation dotLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat '.'
      std::string MemberName;
      std::string MemberGenericSuffix;
      bool isDestructor = false;
      if (CurTok == tok_tilde_identifier) {
        isDestructor = true;
        MemberName = IdentifierStr;
        getNextToken();
      } else if (CurTok == tok_identifier || CurTok == tok_get ||
                 CurTok == tok_set) {
        MemberName = IdentifierStr;
        getNextToken();
      } else if (CurTok == tok_weak) {
        MemberName = "weak";
        getNextToken();
      } else if (CurTok == tok_this) {
        MemberName = "this";
        getNextToken();
      } else {
        LogError("Expected member name after '.'");
        return nullptr;
      }
      if (isDestructor) {
        LHS = std::make_unique<MemberAccessExprAST>(std::move(LHS), MemberName,
                                                    std::string{}, true);
        LHS->setSourceLocation(dotLoc);
      } else {
        std::string DecoratedName = MemberName;
        if (!ParseOptionalGenericArgumentList(DecoratedName, true))
          return nullptr;
        if (DecoratedName.size() > MemberName.size()) {
          MemberGenericSuffix = DecoratedName.substr(MemberName.size());
          SkipNewlines();
          if (CurTok != '(') {
            reportCompilerError("Generic argument list must be followed by '(' in member call expressions",
                                "Use parentheses to invoke the method after specifying explicit type arguments.");
            return nullptr;
          }
        }
        LHS = std::make_unique<MemberAccessExprAST>(std::move(LHS), MemberName,
                                                    std::move(MemberGenericSuffix));
        LHS->setSourceLocation(dotLoc);
      }
    } else if (CurTok == tok_null_safe_access) {
      // Null-safe member access
      SourceLocation opLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat '?.'
      if (CurTok == tok_tilde_identifier) {
        reportCompilerError("Null-safe destructor access is not supported");
        return nullptr;
      }
      if (CurTok != tok_identifier && CurTok != tok_get && CurTok != tok_set &&
          CurTok != tok_weak) {
        LogError("Expected member name after '?.'");
        return nullptr;
      }
      std::string MemberName =
          (CurTok == tok_weak) ? std::string("weak") : IdentifierStr;
      getNextToken(); // eat member name
      std::string DecoratedName = MemberName;
      if (!ParseOptionalGenericArgumentList(DecoratedName, true))
        return nullptr;
      if (DecoratedName.size() > MemberName.size()) {
        reportCompilerError("Generic arguments are not supported on null-safe member access",
                            "Invoke the member with '.' instead of '?.' when providing explicit type arguments.");
        return nullptr;
      }
      LHS = std::make_unique<NullSafeAccessExprAST>(std::move(LHS), MemberName);
      LHS->setSourceLocation(opLoc);
    } else if (CurTok == tok_arrow) {
      // Pointer member access (-> is syntactic sugar for (@ptr).member)
      const bool allowSafeArrow = !isInUnsafeContext();
      SourceLocation arrowLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat '->'
      bool isDestructor = false;
      if (CurTok == tok_tilde_identifier)
        isDestructor = true;
      if (CurTok != tok_identifier && CurTok != tok_get && CurTok != tok_set &&
          CurTok != tok_tilde_identifier && CurTok != tok_weak) {
        LogError("Expected member name after '->'");
        return nullptr;
      }
      std::string MemberName =
          (CurTok == tok_weak) ? std::string("weak") : IdentifierStr;
      getNextToken(); // eat member name
      std::string MemberGenericSuffix;
      if (!isDestructor) {
        std::string DecoratedName = MemberName;
        if (!ParseOptionalGenericArgumentList(DecoratedName, true))
          return nullptr;
        if (DecoratedName.size() > MemberName.size()) {
          MemberGenericSuffix = DecoratedName.substr(MemberName.size());
          SkipNewlines();
          if (CurTok != '(') {
            reportCompilerError("Generic argument list must be followed by '(' in member call expressions",
                                "Use parentheses to invoke the method after specifying explicit type arguments.");
            return nullptr;
          }
        }
      }
      if (allowSafeArrow) {
        LHS = std::make_unique<MemberAccessExprAST>(
            std::move(LHS), MemberName, std::move(MemberGenericSuffix),
            isDestructor, true);
        LHS->setSourceLocation(arrowLoc);
      } else {
        // Create (@ptr).member
        auto Deref =
            std::make_unique<UnaryExprAST>("@", std::move(LHS), true, true);
        Deref->setSourceLocation(arrowLoc);
        LHS = std::make_unique<MemberAccessExprAST>(
            std::move(Deref), MemberName, std::move(MemberGenericSuffix),
            isDestructor, false);
        LHS->setSourceLocation(arrowLoc);
      }
    } else if (CurTok == '(') {
      std::vector<std::unique_ptr<ExprAST>> Args;
      std::vector<std::string> ArgNames;
      std::vector<SourceLocation> ArgNameLocs;
      std::vector<SourceLocation> ArgEqualLocs;
      SourceLocation callLoc = currentParser().currentTokenLocation;
      if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
        return nullptr;
      std::unique_ptr<ExprAST> CalleeExpr = std::move(LHS);
      auto Call = std::make_unique<CallExprAST>(std::move(CalleeExpr),
                                                std::move(Args),
                                                std::move(ArgNames),
                                                std::move(ArgNameLocs),
                                                std::move(ArgEqualLocs));
      if (auto *member =
              dynamic_cast<MemberAccessExprAST *>(Call->getCalleeExpr())) {
        if (member->isDestructorAccess())
          Call->markDestructorCall(member->getMemberName());
      }
      Call->setSourceLocation(callLoc);
      LHS = std::move(Call);
    } else {
      break;
    }
  }

  return LHS;
}
