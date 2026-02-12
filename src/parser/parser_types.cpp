#include "parser/parser_internal.h"
#include <cctype>
#include <optional>
#include <utility>

static unsigned parsePointerDepth(const std::string &typeName) {
  size_t atPos = typeName.find('@');
  if (atPos == std::string::npos)
    return 0;

  size_t endPos = typeName.find_first_of("[]", atPos);
  std::string suffix = typeName.substr(
      atPos + 1,
      endPos == std::string::npos ? std::string::npos : endPos - atPos - 1);
  if (suffix.empty())
    return 1;

  unsigned depth = 1;
  try {
    depth = static_cast<unsigned>(std::stoul(suffix));
    if (depth == 0)
      depth = 1;
  } catch (...) {
    depth = 1;
  }
  return depth;
}

static std::optional<size_t> findMatchingAngle(const std::string &text,
                                               size_t openPos) {
  if (openPos >= text.size() || text[openPos] != '<')
    return std::nullopt;

  int depth = 1;
  for (size_t pos = openPos + 1; pos < text.size(); ++pos) {
    char ch = text[pos];
    if (ch == '<') {
      ++depth;
    } else if (ch == '>') {
      --depth;
      if (depth == 0)
        return pos;
    }
  }

  return std::nullopt;
}

static bool splitGenericArguments(const std::string &segment,
                                  std::vector<std::string> &out) {
  size_t start = 0;
  int angleDepth = 0;
  int bracketDepth = 0;

  for (size_t i = 0; i <= segment.size(); ++i) {
    const bool atEnd = (i == segment.size());
    const char ch = atEnd ? ',' : segment[i];

    if (ch == '<') {
      ++angleDepth;
    } else if (ch == '>') {
      if (angleDepth > 0)
        --angleDepth;
    } else if (ch == '[') {
      ++bracketDepth;
    } else if (ch == ']') {
      if (bracketDepth > 0)
        --bracketDepth;
    }

    if (atEnd || (ch == ',' && angleDepth == 0 && bracketDepth == 0)) {
      if (i < start)
        return false;
      std::string arg = segment.substr(start, i - start);
      if (arg.empty())
        return false;
      out.push_back(std::move(arg));
      start = i + 1;
    }
  }

  return true;
}

static bool parseGenericArgumentSegment(const std::string &segment,
                                        std::vector<TypeInfo> &out) {
  std::vector<std::string> parts;
  if (!splitGenericArguments(segment, parts))
    return false;

  for (std::string &part : parts)
    out.push_back(buildDeclaredTypeInfo(part, false));

  return true;
}

TypeInfo buildDeclaredTypeInfo(const std::string &typeName, bool declaredRef) {
  TypeInfo info;
  std::string_view working(typeName);

  auto trimLeadingWhitespace = [&]() {
    while (!working.empty() &&
           std::isspace(static_cast<unsigned char>(working.front())))
      working.remove_prefix(1);
  };

  trimLeadingWhitespace();

  OwnershipQualifier parsedOwnership = OwnershipQualifier::Strong;
  bool hasOwnershipQualifier = false;

  auto tryConsumeQualifier = [&](std::string_view keyword,
                                 OwnershipQualifier qualifier) -> bool {
    if (working.size() < keyword.size())
      return false;
    if (working.substr(0, keyword.size()) != keyword)
      return false;
    if (working.size() > keyword.size()) {
      unsigned char next =
          static_cast<unsigned char>(working[keyword.size()]);
      if (!std::isspace(next))
        return false;
    }
    working.remove_prefix(keyword.size());
    trimLeadingWhitespace();
    parsedOwnership = qualifier;
    hasOwnershipQualifier = true;
    return true;
  };

  tryConsumeQualifier("weak", OwnershipQualifier::Weak);

  std::string normalized(working.begin(), working.end());
  std::string sanitized;
  sanitized.reserve(normalized.size());

  bool pendingNullable = false;
  bool arraySeen = false;
  bool explicitNullable = false;
  std::vector<unsigned> arrayRanks;

  for (size_t i = 0; i < normalized.size(); ++i) {
    char c = normalized[i];

    if (c == '?') {
      pendingNullable = true;
      continue;
    }

    if (c == '@') {
      sanitized.push_back(c);
      ++i;
      while (i < normalized.size() &&
             std::isdigit(static_cast<unsigned char>(normalized[i]))) {
        sanitized.push_back(normalized[i]);
        ++i;
      }
      --i; // compensate for extra increment in while loop
      if (pendingNullable) {
        explicitNullable = true;
        pendingNullable = false;
      }
      continue;
    }

    if (c == '[') {
      size_t close = normalized.find(']', i);
      if (close == std::string::npos)
        break;

      unsigned rank = 1;
      for (size_t j = i + 1; j < close; ++j) {
        if (normalized[j] == ',')
          ++rank;
      }

      sanitized.append(normalized, i, close - i + 1);
      arrayRanks.push_back(rank);
      arraySeen = true;
      if (pendingNullable) {
        info.elementNullable = true;
        pendingNullable = false;
      }
      i = close;
      continue;
    }

    sanitized.push_back(c);
  }

  if (pendingNullable)
    explicitNullable = true;

  info.typeName = sanitized;
  info.pointerDepth = parsePointerDepth(info.typeName);
  info.isArray = arraySeen || (info.typeName.find('[') != std::string::npos);
  info.arrayDepth = arrayRanks.size();
  info.arrayRanks = std::move(arrayRanks);
  info.isMultidimensional = false;
  for (unsigned rank : info.arrayRanks) {
    if (rank > 1) {
      info.isMultidimensional = true;
      break;
    }
  }

  info.isNullable = explicitNullable;

  // Pointer types are always nullable even without explicit '?'
  if (info.pointerDepth > 0 && !info.isArray)
    info.isNullable = true;

  std::string basePortion = info.typeName;
  size_t suffixPos = basePortion.find_first_of("@[");
  if (suffixPos != std::string::npos)
    basePortion = basePortion.substr(0, suffixPos);

  size_t anglePos = basePortion.find('<');
  if (anglePos != std::string::npos) {
    if (auto closePos = findMatchingAngle(basePortion, anglePos)) {
      std::string segment =
          basePortion.substr(anglePos + 1, *closePos - anglePos - 1);
      parseGenericArgumentSegment(segment, info.typeArguments);
      info.baseTypeName = basePortion.substr(0, anglePos);
    } else {
      info.baseTypeName = basePortion.substr(0, anglePos);
    }
  } else {
    info.baseTypeName = basePortion;
  }

  info.isGenericParameter = currentParser().isGenericParameter(info.baseTypeName);
  info.refStorage =
      declaredRef ? RefStorageClass::RefValue : RefStorageClass::None;
  info.isMutable = true;
  info.declaredRef = declaredRef;
  finalizeTypeInfoMetadata(info);
  info.hasExplicitOwnership = hasOwnershipQualifier;
  if (hasOwnershipQualifier)
    info.ownership = parsedOwnership;
  return info;
}

void maybeWarnGenericArity(const std::vector<std::string> &params,
                           const std::string &ownerName,
                           std::string_view context) {
  if (params.empty())
    return;
  GenericsDiagnostics &diag = currentCodegen().genericsDiagnostics;
  if (!diag.heuristicsEnabled || params.size() <= diag.arityWarningThreshold)
    return;
  reportCompilerWarning(
      std::string(context) + " '" + ownerName + "' declares " +
          std::to_string(params.size()) + " generic parameters",
      "Consider reducing the number of generic parameters or raising the threshold via --diagnostics generics.");
}

bool IsBuiltInType() {
  return CurTok == tok_int || CurTok == tok_float || CurTok == tok_double ||
         CurTok == tok_decimal || CurTok == tok_char || CurTok == tok_void ||
         CurTok == tok_bool || CurTok == tok_string || CurTok == tok_byte ||
         CurTok == tok_short || CurTok == tok_long || CurTok == tok_sbyte ||
         CurTok == tok_ushort || CurTok == tok_uint || CurTok == tok_ulong ||
         CurTok == tok_schar || CurTok == tok_lchar;
}

bool IsActiveStructName(const std::string &name) {
  const auto &stack = StructDefinitionStack;
  for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
    if (*it == name)
      return true;
  }
  return false;
}

bool IsActiveClassName(const std::string &name) {
  const auto &stack = ClassDefinitionStack;
  for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
    if (*it == name)
      return true;
  }
  return false;
}

bool IsValidType() {
  if (IsBuiltInType())
    return true;

  if (CurTok == tok_unique || CurTok == tok_shared || CurTok == tok_weak)
    return true;

  if (CurTok != tok_identifier)
    return false;

  return StructNames.contains(IdentifierStr) ||
         ClassNames.contains(IdentifierStr) ||
         DelegateNames.contains(IdentifierStr) ||
         IsActiveStructName(IdentifierStr) ||
         IsActiveClassName(IdentifierStr) ||
         currentParser().isGenericParameter(IdentifierStr);
}

bool AppendTypeSuffix(std::string &Type, bool &pointerSeen,
                      bool allowOperatorDisambiguation) {
  while (true) {
    if (CurTok == tok_null_array_access) {
      // Treat '?[' as nullable element type followed by array suffix
      Type += "?";
      std::string segment = "[";
      getNextToken(); // advance to the token after '['

      while (CurTok == ',') {
        segment += ",";
        getNextToken(); // eat ','
      }

      if (CurTok != ']') {
        reportCompilerError("Expected ']' after '[' in array type");
        return false;
      }

      getNextToken(); // eat ']'
      segment += "]";
      Type += segment;
      continue;
    }

    if (CurTok == tok_nullable) {
      Type += "?";
      getNextToken();
      continue;
    }

    if (CurTok == tok_at && !pointerSeen) {
      if (allowOperatorDisambiguation) {
        TokenReplayScope replayScope(true);
        getNextToken(); // inspect token after '@'
        SkipNewlines();
        if (CurTok == '(') {
          replayScope.rollback();
          break;
        }
        replayScope.rollback();
      }

      if (!isInUnsafeContext()) {
        reportCompilerError(
            "Pointer types can only be used within unsafe blocks or unsafe functions");
        return false;
      }

      pointerSeen = true;
      getNextToken(); // eat '@'

      if (CurTok == tok_number) {
        if (!LexedNumericLiteral.isInteger() ||
            !LexedNumericLiteral.fitsInUnsignedBits(32)) {
          reportCompilerError("Pointer level must be a positive integer");
          return false;
        }

        uint64_t level = LexedNumericLiteral.getUnsignedValue();
        if (level == 0) {
          reportCompilerError("Pointer level must be a positive integer");
          return false;
        }

        Type += "@";
        Type += std::to_string(level);
        getNextToken(); // eat number
      } else {
        Type += "@";
      }
      continue;
    }

    if (CurTok == '[') {
      if (allowOperatorDisambiguation) {
        TokenReplayScope replayScope(true);
        getNextToken(); // inspect token after '['
        SkipNewlines();
        if (CurTok == ']') {
          getNextToken(); // inspect token after ']'
          SkipNewlines();
          if (CurTok == '(') {
            replayScope.rollback();
            break;
          }
        }
        replayScope.rollback();
      }

      std::string segment = "[";
      getNextToken(); // eat '['

      while (CurTok != ']' && CurTok != tok_eof) {
        if (CurTok == tok_newline) {
          getNextToken();
          continue;
        }

        if (CurTok == ',') {
          segment += ",";
          getNextToken(); // eat ','
          continue;
        }

        if (CurTok == tok_number) {
          if (!LexedNumericLiteral.isInteger()) {
            reportCompilerError("Array bounds in a type must be an integer literal");
            return false;
          }
          segment += LexedNumericLiteral.getSpelling();
          getNextToken(); // eat number
          continue;
        }

        reportCompilerError("Expected ',' or ']' in array type");
        return false;
      }

      if (CurTok != ']') {
        reportCompilerError("Expected ']' after '[' in array type");
        return false;
      }

      getNextToken(); // eat ']'
      segment += "]";
      Type += segment;
      continue;
    }

    break;
  }

  return true;
}

bool ParseOptionalGenericArgumentList(std::string &Type,
                                      bool allowDisambiguation) {
  if (CurTok != tok_lt)
    return true;

  TokenReplayScope replayScope(allowDisambiguation);
  TemplateAngleScope angleScope;

  std::string buffer;
  buffer.push_back('<');

  getNextToken(); // eat '<'
  SkipNewlines();

  auto fail = [&](const std::string &message,
                  std::string_view hint = {}) -> bool {
    if (allowDisambiguation) {
      replayScope.rollback();
      return true;
    }
    reportCompilerError(message, hint);
    return false;
  };

  bool expectArgument = true;
  while (true) {
    if (CurTok == tok_gt) {
      if (expectArgument)
        return fail("Expected type argument after '<'");
      buffer.push_back('>');
      getNextToken();
      break;
    }

    if (CurTok == tok_eof)
      return fail("Unterminated generic argument list");

    if (!expectArgument) {
      if (CurTok == ',') {
        buffer.push_back(',');
        getNextToken();
        SkipNewlines();
        expectArgument = true;
        continue;
      }
      if (CurTok == tok_gt)
        continue;
      return fail("Expected ',' or '>' in generic argument list");
    }

    if (!IsValidType() && CurTok != '(')
      return fail("Expected type argument in generic list");

    TypeInfo argumentInfo;
    if (!ParseCompleteTypeInfo(argumentInfo, false))
      return fail("Failed to parse type argument");

    buffer.append(typeNameFromInfo(argumentInfo));
    SkipNewlines();
    expectArgument = false;
  }

  if (allowDisambiguation)
    replayScope.commit();
  Type += buffer;
  return true;
}

bool ParseGenericParameterList(std::vector<std::string> &parameters) {
  if (CurTok != tok_lt)
    return false;

  getNextToken(); // eat '<'
  SkipNewlines();

  if (CurTok == tok_gt) {
    reportCompilerError("Generic parameter list cannot be empty");
    return false;
  }

  while (true) {
    if (CurTok != tok_identifier) {
      reportCompilerError("Expected identifier in generic parameter list");
      return false;
    }

    std::string param = IdentifierStr;
    parameters.push_back(param);

    getNextToken();
    SkipNewlines();

    if (CurTok == ',') {
      getNextToken();
      SkipNewlines();
      continue;
    }

    if (CurTok != tok_gt) {
      reportCompilerError("Expected '>' to close generic parameter list");
      return false;
    }

    getNextToken();
    break;
  }

  return true;
}

static bool ParseTypeArgumentListForType(std::string &TypeName,
                                         std::vector<TypeInfo> &TypeArgs,
                                         bool allowDisambiguation) {
  if (CurTok != tok_lt)
    return true;

  std::string originalTypeName = TypeName;
  std::vector<TypeInfo> originalTypeArgs = TypeArgs;
  bool canDisambiguateAsOperator = false;
  if (allowDisambiguation) {
    // Only disambiguate '<' as an operator member name when it is directly
    // followed by a parameter list, e.g. `bool <(...)`.
    TokenReplayScope probeScope(true);
    getNextToken(); // inspect token after '<'
    SkipNewlines();
    canDisambiguateAsOperator = (CurTok == '(');
    probeScope.rollback();
  }

  TokenReplayScope replayScope(canDisambiguateAsOperator);
  TemplateAngleScope angleScope;
  TypeName.push_back('<');

  getNextToken(); // eat '<'
  SkipNewlines();

  auto fail = [&](const std::string &message) -> bool {
    if (canDisambiguateAsOperator) {
      replayScope.rollback();
      TypeName = originalTypeName;
      TypeArgs = originalTypeArgs;
      return true;
    }
    reportCompilerError(message);
    return false;
  };

  bool expectArgument = true;
  while (true) {
    if (CurTok == tok_gt) {
      if (expectArgument) {
        return fail("Expected type argument after '<'");
      }
      TypeName.push_back('>');
      getNextToken();
      break;
    }

    if (CurTok == tok_eof) {
      return fail("Unterminated generic argument list");
    }

    if (!expectArgument) {
      if (CurTok == ',') {
        TypeName.push_back(',');
        getNextToken();
        SkipNewlines();
        expectArgument = true;
        continue;
      }
      if (CurTok == tok_gt)
        continue;
      return fail("Expected ',' or '>' in generic argument list");
    }

    if (!IsValidType() && CurTok != '(') {
      return fail("Expected type argument in generic list");
    }

    TypeInfo argInfo;
    if (!ParseCompleteTypeInfo(argInfo, false, false)) {
      return fail("Failed to parse type argument");
    }

    TypeArgs.push_back(argInfo);
    TypeName.append(typeNameFromInfo(argInfo));
    SkipNewlines();
    expectArgument = false;
  }

  if (canDisambiguateAsOperator)
    replayScope.commit();
  return true;
}

bool ParseCompleteTypeInfo(TypeInfo &outInfo, bool declaredRef,
                           bool allowTypeArgDisambiguation) {
  std::string TypeName;
  std::vector<TypeInfo> parsedTypeArguments;
  std::vector<std::string> tupleElementNames;

  if (CurTok == '(') {
    getNextToken(); // eat '('
    SkipNewlines();

    std::vector<TypeInfo> elementInfos;
    std::vector<std::string> elementTypeNames;
    bool sawComma = false;

    if (CurTok != ')') {
      while (true) {
        TypeInfo elementInfo;
        if (!ParseCompleteTypeInfo(elementInfo, false))
          return false;
        elementTypeNames.push_back(typeNameFromInfo(elementInfo));
        elementInfos.push_back(elementInfo);

        SkipNewlines();
        if (CurTok == tok_identifier) {
          tupleElementNames.push_back(IdentifierStr);
          getNextToken();
          SkipNewlines();
        } else {
          tupleElementNames.emplace_back();
        }

        if (CurTok == ')')
          break;

        if (CurTok != ',') {
          reportCompilerError("Expected ')' or ',' in tuple type");
          return false;
        }

        sawComma = true;
        getNextToken(); // eat ','
        SkipNewlines();
      }
    }

    if (CurTok != ')') {
      reportCompilerError("Expected ')' to close tuple type");
      return false;
    }
    getNextToken(); // eat ')'

    if (!sawComma || elementInfos.size() < 2) {
      reportCompilerError("Tuple types require at least two elements");
      return false;
    }

    TypeName = "tuple<";
    for (size_t i = 0; i < elementTypeNames.size(); ++i) {
      if (i > 0)
        TypeName += ",";
      TypeName += elementTypeNames[i];
    }
    TypeName += ">";

    parsedTypeArguments = std::move(elementInfos);
  } else {
    if (!IsValidType())
      return false;

    TypeName = IdentifierStr;
    getNextToken(); // eat base type

    if (!ParseTypeArgumentListForType(TypeName, parsedTypeArguments,
                                      allowTypeArgDisambiguation))
      return false;
  }

  SkipNewlines();

  bool pointerSeen = false;
  if (!AppendTypeSuffix(TypeName, pointerSeen, allowTypeArgDisambiguation))
    return false;

  outInfo = buildDeclaredTypeInfo(TypeName, declaredRef);
  if (!parsedTypeArguments.empty()) {
    outInfo.typeArguments = std::move(parsedTypeArguments);
    finalizeTypeInfoMetadata(outInfo);
  }
  if (!tupleElementNames.empty())
    outInfo.tupleElementNames = std::move(tupleElementNames);
  return true;
}
