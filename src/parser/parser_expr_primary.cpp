#include "parser/parser_internal.h"
#include <optional>

static bool ParseNewTypeName(std::string &OutType, bool stopBeforeArrayBounds) {
  if (!IsValidType())
    return false;

  std::string Type = IdentifierStr;
  getNextToken(); // eat base type

  if (!ParseOptionalGenericArgumentList(Type))
    return false;

  SkipNewlines();

  bool pointerSeen = false;
  while (true) {
    if (CurTok == tok_nullable) {
      Type += "?";
      getNextToken();
      continue;
    }

    if (CurTok == tok_at && !pointerSeen) {
      if (!isInUnsafeContext()) {
        reportCompilerError(
            "Pointer types can only be used within 'unsafe' contexts",
            "Mark the surrounding function or block as 'unsafe' before using pointer types with 'new'.");
        return false;
      }

      pointerSeen = true;
      getNextToken(); // eat '@'

      if (CurTok == tok_number) {
        if (!LexedNumericLiteral.isInteger() ||
            !LexedNumericLiteral.fitsInUnsignedBits(32) ||
            LexedNumericLiteral.getUnsignedValue() == 0) {
          reportCompilerError("Pointer level must be a positive integer");
          return false;
        }

        uint64_t level = LexedNumericLiteral.getUnsignedValue();
        Type += "@";
        Type += std::to_string(level);
        getNextToken(); // eat number
      } else {
        Type += "@";
      }
      SkipNewlines();
      continue;
    }

    if (CurTok == '[' && stopBeforeArrayBounds)
      break;

    break;
  }

  OutType = std::move(Type);
  return true;
}

static std::unique_ptr<ExprAST> ParseNewExpression() {
  getNextToken(); // consume 'new'
  SkipNewlines();

  bool sawType = false;
  std::string typeName;

  if (CurTok != '[' && IsValidType()) {
    if (!ParseNewTypeName(typeName, /*stopBeforeArrayBounds=*/true))
      return nullptr;
    sawType = !typeName.empty();
    SkipNewlines();
  }

  if (CurTok == '[') {
    getNextToken(); // eat '['
    SkipNewlines();

    if (CurTok == tok_eof || CurTok == tok_newline || CurTok == ']') {
      return LogError("Expected array size expression after '[' in 'new' expression",
                      "Provide a length expression like 'new int[5]'.");
    }

    std::vector<std::unique_ptr<ExprAST>> bounds;
    while (true) {
      auto Length = ParseExpression();
      if (!Length)
        return nullptr;
      bounds.push_back(std::move(Length));

      SkipNewlines();
      if (CurTok != ',')
        break;
      getNextToken(); // eat ','
      SkipNewlines();
      if (CurTok == tok_eof || CurTok == tok_newline || CurTok == ']') {
        return LogError("Expected array size expression after ',' in 'new' expression");
      }
    }

    if (CurTok != ']') {
      return LogError("Expected ']' after array bounds in 'new' expression");
    }
    getNextToken(); // eat ']'

    auto NewExpr = std::make_unique<NewExprAST>(
        typeName, std::vector<std::unique_ptr<ExprAST>>{},
        std::vector<std::string>{}, std::vector<SourceLocation>{},
        std::vector<SourceLocation>{}, std::move(bounds), true, !sawType);
    if (!typeName.empty()) {
      std::string resultType = typeName;
      if (resultType.find('[') == std::string::npos) {
        size_t boundCount = NewExpr->getArraySizes().size();
        if (boundCount > 1) {
          resultType += "[";
          resultType.append(boundCount - 1, ',');
          resultType += "]";
        } else {
          resultType += "[]";
        }
      }
      NewExpr->setTypeName(resultType);
    }
    NewExpr->markTemporary();
    return NewExpr;
  }

  if (CurTok != '(') {
    if (sawType) {
      return LogError("Expected constructor arguments after 'new'",
                      "Add parentheses after 'new " + typeName +
                          "' to invoke a constructor.");
    }
    return LogError("Expected target type or constructor after 'new'",
                    "Provide a type name (e.g. 'new Box()') or array bounds (e.g. 'new[5]').");
  }

  std::vector<std::unique_ptr<ExprAST>> Args;
  std::vector<std::string> ArgNames;
  std::vector<SourceLocation> ArgNameLocs;
  std::vector<SourceLocation> ArgEqualLocs;
  if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
    return nullptr;

  auto NewExpr = std::make_unique<NewExprAST>(
      typeName, std::move(Args), std::move(ArgNames),
      std::move(ArgNameLocs), std::move(ArgEqualLocs),
      std::vector<std::unique_ptr<ExprAST>>{}, false,
      !sawType);
  if (!typeName.empty())
    NewExpr->setTypeName(typeName);
  NewExpr->markTemporary();
  return NewExpr;
}

static std::unique_ptr<ExprAST> ParseInterpolatedStringExpr() {
  std::vector<InterpolatedStringExprAST::Segment> segments;
  segments.push_back(InterpolatedStringExprAST::Segment::makeLiteral(StringVal));
  bool hasExpression = false;

  getNextToken(); // consume start token

  while (true) {
    if (CurTok == tok_interpolated_expr_start) {
      getNextToken(); // consume expression start
      auto Expr = ParseExpression();
      if (!Expr) {
        RecoverAfterExpressionError();
        return nullptr;
      }

      std::optional<std::string> formatSpec;
      if (CurTok == tok_colon) {
        getNextToken(); // consume ':'
        if (CurTok != tok_number || !LexedNumericLiteral.isInteger()) {
          return LogError("Expected integer format specifier in interpolated string expression");
        }
        formatSpec = LexedNumericLiteral.getSpelling();
        getNextToken(); // consume format specifier token
      }

      if (CurTok != tok_interpolated_expr_end) {
        return LogError("Expected ` to close interpolated expression");
      }

      segments.push_back(InterpolatedStringExprAST::Segment::makeExpression(std::move(Expr), std::move(formatSpec)));
      hasExpression = true;

      getNextToken(); // consume expr end

      if (CurTok == tok_interpolated_string_segment) {
        segments.push_back(InterpolatedStringExprAST::Segment::makeLiteral(StringVal));
        getNextToken(); // consume literal segment token
        continue;
      }

      if (CurTok == tok_interpolated_string_end) {
        getNextToken(); // consume end token
        break;
      }

      if (CurTok == tok_interpolated_expr_start) {
        segments.push_back(InterpolatedStringExprAST::Segment::makeLiteral(""));
        continue;
      }

      return LogError("Expected interpolated string segment or end after expression");
    }

    if (CurTok == tok_interpolated_string_end) {
      getNextToken(); // consume end token
      break;
    }

    // No more interpolation-specific tokens; finish.
    break;
  }

  // Merge consecutive literal segments
  std::vector<InterpolatedStringExprAST::Segment> merged;
  merged.reserve(segments.size());

  for (auto &segment : segments) {
    if (segment.isLiteral()) {
      if (!merged.empty() && merged.back().isLiteral()) {
        merged.back().appendLiteral(segment.getLiteral());
      } else {
        merged.push_back(InterpolatedStringExprAST::Segment::makeLiteral(segment.getLiteral()));
      }
    } else {
      merged.push_back(std::move(segment));
    }
  }

  if (!hasExpression) {
    std::string combined;
    combined.reserve(64);
    for (const auto &segment : merged) {
      if (segment.isLiteral())
        combined += segment.getLiteral();
    }
    return std::make_unique<StringExprAST>(combined);
  }

  return std::make_unique<InterpolatedStringExprAST>(std::move(merged));
}

/// InferExprType - Infer the type of an expression at parse time
/// Returns empty string if type cannot be determined
static std::string InferExprType(const ExprAST *expr) {
  if (!expr)
    return "";

  // Check for literal types
  if (const auto *numExpr = dynamic_cast<const NumberExprAST *>(expr)) {
    const NumericLiteral &literal = numExpr->getLiteral();
    if (literal.isDecimal())
      return "decimal";
    if (literal.isInteger()) {
      if (literal.fitsInSignedBits(32))
        return "int";
      if (literal.fitsInSignedBits(64))
        return "long";
      return "ulong";
    }
    return "double";
  }

  if (dynamic_cast<const BoolExprAST *>(expr)) {
    return "bool";
  }

  if (dynamic_cast<const StringExprAST *>(expr)) {
    return "string";
  }

  if (dynamic_cast<const CharExprAST *>(expr)) {
    return "char";
  }

  if (dynamic_cast<const NullExprAST *>(expr)) {
    return ""; // null has no inherent type; defer to contextual typing
  }

  // Check for cast expressions
  if (const CastExprAST *castExpr = dynamic_cast<const CastExprAST *>(expr)) {
    return castExpr->getTargetType();
  }

  // Check for array expressions
  if (const ArrayExprAST *arrayExpr = dynamic_cast<const ArrayExprAST *>(expr)) {
    const std::string &elemType = arrayExpr->getElementType();
    if (elemType.empty())
      return "";
    return elemType + "[]";
  }

  // For other expressions, can't determine type at parse time
  return "";
}

/// AreTypesCompatible - Check if two types are compatible for array elements
static bool AreTypesCompatible(const std::string &type1,
                               const std::string &type2) {
  if (type1.empty() || type2.empty())
    return true; // Can't determine, assume compatible

  if (type1 == type2)
    return true;

  auto isIntegerLike = [](const std::string &typeName) {
    return typeName == "byte" || typeName == "sbyte" ||
           typeName == "short" || typeName == "ushort" ||
           typeName == "int" || typeName == "uint" ||
           typeName == "long" || typeName == "ulong" ||
           typeName == "char" || typeName == "schar" ||
           typeName == "lchar";
  };

  if (type1 == "decimal" || type2 == "decimal") {
    if (type1 == "decimal" && isIntegerLike(type2))
      return true;
    if (type2 == "decimal" && isIntegerLike(type1))
      return true;
    return false;
  }

  // Allow int and float to be compatible (will need casting during codegen)
  if ((type1 == "int" || type1 == "float" || type1 == "double") &&
      (type2 == "int" || type2 == "float" || type2 == "double")) {
    return true;
  }

  return false;
}

/// numberexpr ::= number
std::unique_ptr<ExprAST> ParseNumberExpr() {
  SourceLocation loc = currentParser().currentTokenLocation;
  auto Result = std::make_unique<NumberExprAST>(LexedNumericLiteral);
  Result->setSourceLocation(loc);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
std::unique_ptr<ExprAST> ParseParenExpr() {
  SourceLocation loc = currentParser().currentTokenLocation;
  getNextToken(); // eat '('
  SkipNewlines();

  std::vector<std::unique_ptr<ExprAST>> Elements;
  bool sawComma = false;

  if (CurTok != ')') {
    while (true) {
      auto Element = ParseExpression();
      if (!Element)
        return nullptr;
      Elements.push_back(std::move(Element));

      SkipNewlines();

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in parenthesized expression");

      sawComma = true;
      getNextToken(); // eat ','
      SkipNewlines();
    }
  }

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ')'

  bool isTuple = sawComma || Elements.size() != 1;
  auto expr = std::make_unique<ParenExprAST>(std::move(Elements), isTuple);
  expr->setSourceLocation(loc);
  return expr;
}

/// unaryexpr ::= ('-' | '!') primary
/// arrayexpr ::= '[' expression* ']'
std::unique_ptr<ExprAST> ParseArrayExpr() {
  SourceLocation loc = currentParser().currentTokenLocation;
  getNextToken(); // eat [

  std::vector<std::unique_ptr<ExprAST>> Elements;
  SkipNewlines();

  if (CurTok != ']') {
    while (true) {
      SkipNewlines();

      if (auto Elem = ParseExpression())
        Elements.push_back(std::move(Elem));
      else
        return nullptr;

      SkipNewlines();

      if (CurTok == ']')
        break;

      if (CurTok != ',')
        return LogError("Expected ',' or ']' in array literal");
      getNextToken(); // eat ,
      SkipNewlines();
    }
  }

  if (CurTok != ']')
    return LogError("Expected ']' to close array literal");
  getNextToken(); // eat ]

  // Infer the element type from the elements and check compatibility
  std::string ElementType = "";
  std::string CommonType = "";
  bool hasFloat = false;
  bool hasDecimal = false;

  // Check all elements for type compatibility
  for (const auto &elem : Elements) {
    std::string elemType = InferExprType(elem.get());

    if (!elemType.empty()) {
      if (ElementType.empty()) {
        // First typed element determines the base type
        ElementType = elemType;
        CommonType = elemType;
      } else if (!AreTypesCompatible(ElementType, elemType)) {
        // Types are incompatible
        return LogError("Incompatible types in array literal: cannot mix " +
                        ElementType + " and " + elemType);
      }

      // Track if any floats/doubles for type promotion
      if (elemType == "float" || elemType == "double") {
        hasFloat = true;
      }
      if (elemType == "decimal") {
        hasDecimal = true;
      }
    }
  }

  if (hasDecimal) {
    ElementType = "decimal";
  }

  // If mixed int/float, promote to double
  if (!hasDecimal &&
      hasFloat &&
      (ElementType == "int" || ElementType == "float" ||
       ElementType == "double")) {
    ElementType = "double";
  }

  // Leave ElementType empty when inference failed so codegen can use context or diagnose

  auto expr = std::make_unique<ArrayExprAST>(ElementType, std::move(Elements));
  expr->setSourceLocation(loc);
  return expr;
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
    SourceLocation typeLoc = currentParser().currentTokenLocation;
    int SavedTok = CurTok;
    (void)SavedTok;
    getNextToken(); // consume type

    // Check if this is a type cast (type:)
    if (CurTok == tok_colon) {
      getNextToken(); // consume ':'
      auto Operand = ParsePrimary();
      if (!Operand) {
        RecoverAfterExpressionError();
        return nullptr;
      }
      auto cast = std::make_unique<CastExprAST>(TypeName, std::move(Operand));
      cast->setSourceLocation(typeLoc);
      return cast;
    } else {
      // Not a cast, restore state and continue with default handling
      // This shouldn't happen in normal parsing flow, but handle it gracefully
      return LogError("Unexpected type name in expression",
                      "Did you mean to create a cast using 'Type: expression'?");
    }
  }

  switch (CurTok) {
  case tok_error:
    // Token already produced a diagnostic at the lexer layer; just advance.
    getNextToken();
    return nullptr;
  default:
    // Avoid cascading generic expression errors immediately after a lexer error.
    if (currentParser().previousToken == tok_error) {
      RecoverAfterExpressionError();
      return nullptr;
    }
    return LogError(
        "Unexpected token while parsing an expression",
        "Insert an expression or remove the unexpected token.");
  case tok_unique:
  case tok_shared:
  case tok_weak:
    return ParseIdentifierExpr();
  case tok_value: {
    if (!currentParser().allowValueIdentifier) {
      reportCompilerError(
          "'value' may only be used inside property or indexer setters");
      getNextToken();
      return nullptr;
    }
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'value'
    auto expr = std::make_unique<VariableExprAST>("value");
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_identifier:
  case tok_get:
  case tok_set:
    return ParseIdentifierExpr();
  case tok_this: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'this'
    auto expr = std::make_unique<ThisExprAST>();
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_base: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'base'
    auto expr = std::make_unique<BaseExprAST>();
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_number:
    return ParseNumberExpr();
  case tok_true: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'true'
    auto expr = std::make_unique<BoolExprAST>(true);
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_false: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'false'
    auto expr = std::make_unique<BoolExprAST>(false);
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_null: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'null'
    auto expr = std::make_unique<NullExprAST>();
    expr->setSourceLocation(loc);
    return expr;
  }
  case tok_string_literal: {
    std::string literal = StringVal;
    SourceLocation loc = currentParser().currentTokenLocation;
    auto Result = std::make_unique<StringExprAST>(literal);
    Result->setSourceLocation(loc);
    getNextToken(); // consume the string literal
    if (CurTok == '(') {
      std::vector<std::unique_ptr<ExprAST>> Args;
      std::vector<std::string> ArgNames;
      std::vector<SourceLocation> ArgNameLocs;
      std::vector<SourceLocation> ArgEqualLocs;
      if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
        return nullptr;
      auto call = std::make_unique<CallExprAST>(literal, std::move(Args),
                                                std::move(ArgNames),
                                                std::move(ArgNameLocs),
                                                std::move(ArgEqualLocs));
      call->setSourceLocation(loc);
      return call;
    }
    return std::move(Result);
  }
  case tok_new:
    return ParseNewExpression();
  case tok_interpolated_string_start:
    return ParseInterpolatedStringExpr();
  case tok_char_literal: {
    SourceLocation loc = currentParser().currentTokenLocation;
    auto Result = std::make_unique<CharExprAST>(CharVal);
    Result->setSourceLocation(loc);
    getNextToken(); // consume the character literal
    return std::move(Result);
  }
  case tok_ref: {
    SourceLocation loc = currentParser().currentTokenLocation;
    getNextToken(); // consume 'ref'
    SkipNewlines();
    auto Operand = ParsePrimaryWithPostfix();
    if (!Operand)
      return nullptr;
    auto expr = std::make_unique<RefExprAST>(std::move(Operand));
    expr->setSourceLocation(loc);
    return expr;
  }
  case '(':
    return ParseParenExpr();
  case '[':
    return ParseArrayExpr();
  case '-':
  case tok_not:
  case tok_inc:
  case tok_dec:
  case tok_hash:
  case tok_at:
    return ParseUnaryExpr();
  case tok_switch:
    return ParseSwitchExpression();
  }
}
