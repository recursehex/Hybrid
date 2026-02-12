#include "parser/parser_internal.h"
#include <cctype>
#include <optional>
#include <set>

enum class AccessSpecifier {
  Default,
  Public,
  Private,
  Protected
};

struct PendingMemberModifiers {
  AccessSpecifier access = AccessSpecifier::Default;
  bool hasExplicitAccess = false;
  bool isStatic = false;
  bool isConst = false;
  bool isAbstract = false;
  bool isVirtual = false;
  bool isOverride = false;
};

static bool ParsePendingMemberModifiers(PendingMemberModifiers &out) {
  PendingMemberModifiers pending;
  bool parsing = true;

  while (parsing) {
    switch (CurTok) {
    case tok_public:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Public;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_private:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Private;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_protected:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Protected;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_static:
      if (pending.isStatic) {
        reportCompilerError("Duplicate 'static' modifier");
        return false;
      }
      pending.isStatic = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_const:
      if (pending.isConst) {
        reportCompilerError("Duplicate 'const' modifier");
        return false;
      }
      pending.isConst = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_abstract:
      if (pending.isAbstract) {
        reportCompilerError("Duplicate 'abstract' modifier");
        return false;
      }
      pending.isAbstract = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_virtual:
      if (pending.isVirtual) {
        reportCompilerError("Duplicate 'virtual' modifier");
        return false;
      }
      pending.isVirtual = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_override:
      if (pending.isOverride) {
        reportCompilerError("Duplicate 'override' modifier");
        return false;
      }
      pending.isOverride = true;
      getNextToken();
      SkipNewlines();
      break;
    default:
      parsing = false;
      break;
    }
  }

  out = pending;
  return true;
}

static MemberModifiers FinalizeMemberModifiers(const PendingMemberModifiers &pending,
                                              AggregateKind kind) {
  MemberModifiers modifiers;
  if (kind == AggregateKind::Struct) {
    modifiers.access = MemberAccess::PublicReadWrite();
  } else {
    modifiers.access = MemberAccess::ReadPublicWritePrivate();
  }

  switch (pending.access) {
  case AccessSpecifier::Public:
    modifiers.access = MemberAccess::PublicReadWrite();
    break;
  case AccessSpecifier::Private:
    modifiers.access = MemberAccess::PrivateOnly();
    break;
  case AccessSpecifier::Protected:
    modifiers.access = MemberAccess::ProtectedReadWrite();
    break;
  case AccessSpecifier::Default:
    break;
  }

  if (pending.isStatic)
    modifiers.storage |= StorageFlag::Static;
  if (pending.isConst)
    modifiers.storage |= StorageFlag::Const;
  modifiers.isAbstract = pending.isAbstract;
  modifiers.isVirtual = pending.isVirtual;
  modifiers.isOverride = pending.isOverride;
  if (modifiers.isAbstract)
    modifiers.isVirtual = true;

  return modifiers;
}

class ScopedValueKeyword {
  ParserContext &Parser;
  bool Previous = false;

public:
  explicit ScopedValueKeyword(bool enable)
      : Parser(currentParser()), Previous(Parser.allowValueIdentifier) {
    Parser.allowValueIdentifier = enable;
  }

  ~ScopedValueKeyword() { Parser.allowValueIdentifier = Previous; }
};

static bool isPascalCase(const std::string &name) {
  if (name.empty())
    return false;

  if (!std::isalpha(static_cast<unsigned char>(name.front())) ||
      !std::isupper(static_cast<unsigned char>(name.front())))
    return false;

  for (char c : name) {
    if (c == '_')
      return false;
  }

  return true;
}

static bool ParseOperatorMemberName(std::string &memberName,
                                    std::string &canonicalName,
                                    OverloadableOperator &opKind) {
  opKind = OverloadableOperator::None;

  switch (CurTok) {
  case '=':
    opKind = OverloadableOperator::Assign;
    getNextToken();
    break;
  case tok_plus_eq:
    opKind = OverloadableOperator::AddAssign;
    getNextToken();
    break;
  case tok_minus_eq:
    opKind = OverloadableOperator::SubAssign;
    getNextToken();
    break;
  case tok_mult_eq:
    opKind = OverloadableOperator::MulAssign;
    getNextToken();
    break;
  case tok_div_eq:
    opKind = OverloadableOperator::DivAssign;
    getNextToken();
    break;
  case tok_mod_eq:
    opKind = OverloadableOperator::ModAssign;
    getNextToken();
    break;
  case '+':
    opKind = OverloadableOperator::Add;
    getNextToken();
    break;
  case '-':
    opKind = OverloadableOperator::Sub;
    getNextToken();
    break;
  case '*':
    opKind = OverloadableOperator::Mul;
    getNextToken();
    break;
  case '/':
    opKind = OverloadableOperator::Div;
    getNextToken();
    break;
  case '%':
    opKind = OverloadableOperator::Mod;
    getNextToken();
    break;
  case tok_eq:
    opKind = OverloadableOperator::Equal;
    getNextToken();
    break;
  case tok_ne:
    opKind = OverloadableOperator::NotEqual;
    getNextToken();
    break;
  case tok_lt:
    opKind = OverloadableOperator::Less;
    getNextToken();
    break;
  case tok_gt:
    opKind = OverloadableOperator::Greater;
    getNextToken();
    break;
  case tok_le:
    opKind = OverloadableOperator::LessEqual;
    getNextToken();
    break;
  case tok_ge:
    opKind = OverloadableOperator::GreaterEqual;
    getNextToken();
    break;
  case tok_at:
    opKind = OverloadableOperator::Dereference;
    getNextToken();
    break;
  case tok_hash:
    opKind = OverloadableOperator::AddressOf;
    getNextToken();
    break;
  case '[':
    getNextToken(); // eat '['
    SkipNewlines();
    if (CurTok != ']') {
      reportCompilerError("Expected ']' after '[' in operator overload name");
      return false;
    }
    opKind = OverloadableOperator::Index;
    getNextToken(); // eat ']'
    break;
  default:
    return false;
  }

  memberName = std::string(overloadableOperatorSymbol(opKind));
  canonicalName = std::string(overloadableOperatorCanonicalName(opKind));
  return true;
}

static bool IsPotentialOperatorToken(int token) {
  switch (token) {
  case tok_eq:
  case tok_ne:
  case tok_le:
  case tok_ge:
  case tok_lt:
  case tok_gt:
  case tok_and:
  case tok_or:
  case tok_not:
  case tok_plus_eq:
  case tok_minus_eq:
  case tok_mult_eq:
  case tok_div_eq:
  case tok_mod_eq:
  case tok_bitwise_and:
  case tok_bitwise_or:
  case tok_bitwise_xor:
  case tok_left_shift:
  case tok_right_shift:
  case tok_and_eq:
  case tok_or_eq:
  case tok_xor_eq:
  case tok_left_shift_eq:
  case tok_right_shift_eq:
  case tok_inc:
  case tok_dec:
  case tok_hash:
  case tok_at:
    return true;
  default:
    break;
  }

  switch (token) {
  case '=':
  case '+':
  case '-':
  case '*':
  case '/':
  case '%':
  case '&':
  case '|':
  case '^':
  case '[':
  case ']':
  case '<':
  case '>':
    return true;
  default:
    return false;
  }
}

/// structdef ::= ('struct' | 'class' | 'interface') identifier ('inherits' type (',' type)*)? '{' (member)* '}'
std::unique_ptr<StructAST> ParseStructDefinition(AggregateKind kind, bool isAbstract) {
  getNextToken(); // eat 'struct', 'class', or 'interface'

  while (CurTok == tok_newline)
    getNextToken();

  if (CurTok != tok_identifier) {
    std::string kindDescription;
    switch (kind) {
    case AggregateKind::Struct:
      kindDescription = "struct";
      break;
    case AggregateKind::Class:
      kindDescription = "class";
      break;
    case AggregateKind::Interface:
      kindDescription = "interface";
      break;
    }
    LogError("Expected " + kindDescription + " name after '" + kindDescription + "'");
    return nullptr;
  }

  std::string compositeName = IdentifierStr;
  getNextToken();

  if (StructNames.contains(compositeName) || ClassNames.contains(compositeName)) {
    reportCompilerError("Type '" + compositeName + "' is already defined");
    return nullptr;
  }

  SkipNewlines();

  std::vector<std::string> genericParameters;
  if (ParseGenericParameterList(genericParameters))
    SkipNewlines();
  if (!genericParameters.empty()) {
    std::string kindLabel = "Struct";
    if (kind == AggregateKind::Class)
      kindLabel = "Class";
    else if (kind == AggregateKind::Interface)
      kindLabel = "Interface";
    maybeWarnGenericArity(genericParameters, compositeName, kindLabel);
  }

  GenericParameterScope compositeGenerics(genericParameters);

  std::optional<std::string> baseClass;
  std::vector<std::string> interfaceTypes;
  std::vector<std::string> baseTypes;
  std::optional<TypeInfo> baseClassInfo;
  std::vector<TypeInfo> interfaceTypeInfos;
  std::vector<TypeInfo> baseTypeInfos;

  if ((kind == AggregateKind::Class || kind == AggregateKind::Interface) &&
      CurTok == tok_inherits) {
    getNextToken(); // eat 'inherits'
    SkipNewlines();

    bool expectBaseClass = (kind == AggregateKind::Class);
    bool sawType = false;
    while (true) {
      if (!IsValidType() && CurTok != '(') {
        LogError("Expected type name after 'inherits'");
        return nullptr;
      }

      TypeInfo clauseInfo;
      if (!ParseCompleteTypeInfo(clauseInfo, false))
        return nullptr;
      sawType = true;
      SkipNewlines();

      std::string typeName = typeNameFromInfo(clauseInfo);
      baseTypeInfos.push_back(clauseInfo);

      if (expectBaseClass) {
        baseClass = typeName;
        baseClassInfo = clauseInfo;
        expectBaseClass = false;
      } else {
        interfaceTypes.push_back(typeName);
        interfaceTypeInfos.push_back(clauseInfo);
      }

      if (CurTok != ',')
        break;

      getNextToken();
      SkipNewlines();
    }

    if (!sawType) {
      LogError("Expected at least one type after 'inherits'");
      return nullptr;
    }
  } else if ((kind == AggregateKind::Class || kind == AggregateKind::Interface) &&
             CurTok == tok_colon) {
    reportCompilerError(
        "Use 'inherits' to declare base types",
        "Replace ':' with 'inherits' before listing base interfaces or classes.");
    return nullptr;
  }

  if (baseClass)
    baseTypes.push_back(*baseClass);
  baseTypes.insert(baseTypes.end(), interfaceTypes.begin(), interfaceTypes.end());

  bool isAbstractComposite = isAbstract || kind == AggregateKind::Interface;

  if (CurTok != '{') {
    LogError("Expected '{' after type name");
    return nullptr;
  }

  auto &definitionStack = (kind == AggregateKind::Struct) ? StructDefinitionStack
                                                          : ClassDefinitionStack;

  struct ScopedCompositeMarker {
    std::vector<std::string> &stack;
    ScopedCompositeMarker(std::vector<std::string> &s, const std::string &name)
        : stack(s) {
      stack.push_back(name);
    }
    ~ScopedCompositeMarker() {
      if (!stack.empty())
        stack.pop_back();
    }
  } scope(definitionStack, compositeName);

  getNextToken(); // eat '{'

  while (CurTok == tok_newline)
    getNextToken();

  std::vector<std::unique_ptr<FieldAST>> Fields;
  std::vector<std::unique_ptr<PropertyAST>> Properties;
  std::vector<MethodDefinition> Methods;
  std::vector<std::unique_ptr<DelegateDeclAST>> Delegates;
  bool seenThisOverride = false;
  bool hasConstructor = false;
  bool hasDestructor = false;
  int destructorIndex = -1;
  bool hasIndexer = false;
  std::set<OverloadableOperator> seenOperatorOverloads;

  auto parseConstructor = [&](MemberModifiers modifiers,
                              std::vector<std::string> ctorGenericParams) -> bool {
    GenericParameterScope ctorScope(ctorGenericParams);
    getNextToken(); // eat '('
    SkipNewlines();

    std::vector<Parameter> Args;
    while (CurTok != ')' && CurTok != tok_eof) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, false)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = false;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return false;
      }
      Args.push_back(std::move(param));

      SkipNewlines();

      if (CurTok == ')')
        break;

      if (CurTok != ',') {
        LogError("Expected ')' or ',' in parameter list");
        return false;
      }
      getNextToken();
      SkipNewlines();
    }

    if (CurTok != ')') {
      LogError("Expected ')' after parameters");
      return false;
    }
    getNextToken();

    if (!ValidateParameterDefaults(Args))
      return false;

    std::vector<ConstructorInitializer> ctorInitializers;

    if (CurTok == tok_colon) {
      LogError("Expected '{' after constructor declaration");
      int braceDepth = 0;
      do {
        getNextToken();
        if (CurTok == '{')
          ++braceDepth;
        else if (CurTok == '}') {
          if (braceDepth == 0)
            break;
          --braceDepth;
        }
      } while (CurTok != tok_eof);
      return false;
    }

    while (CurTok == tok_newline)
      getNextToken();

    if (CurTok != '{') {
      LogError("Expected '{' after constructor declaration");
      return false;
    }

    auto Body = ParseBlock();
    if (!Body)
      return false;

    TypeInfo ctorReturn = buildDeclaredTypeInfo(compositeName, false);
    auto Proto = std::make_unique<PrototypeAST>(
        std::move(ctorReturn), compositeName, std::move(Args),
        false, false, std::move(ctorGenericParams));
    auto Constructor = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    Methods.emplace_back(std::move(Constructor), modifiers, MethodKind::Constructor, compositeName);
    Methods.back().setConstructorInitializers(std::move(ctorInitializers));
    hasConstructor = true;
    return true;
  };

  auto parseCompositeMethod = [&](TypeInfo ReturnTypeInfo,
                                  const std::string &MethodName,
                                  const std::string &CanonicalMethodName,
                                  MemberModifiers modifiers,
                                  MethodKind methodKind,
                                  OverloadableOperator operatorKind,
                                  bool returnsByRef,
                                  std::vector<std::string> methodGenericParams) -> bool {
    GenericParameterScope methodScope(methodGenericParams);
    getNextToken(); // eat '('

    std::vector<Parameter> Args;
    while (CurTok != ')' && CurTok != tok_eof) {
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsConst = false;
      if (CurTok == tok_const) {
        paramIsConst = true;
        getNextToken();
        SkipNewlines();
      }

      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken();
        SkipNewlines();
      }

      if (!paramIsConst && CurTok == tok_const) {
        paramIsConst = true;
        getNextToken();
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      if (paramIsConst)
        ParamTypeInfo.isMutable = false;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return false;
      }
      Args.push_back(std::move(param));

      if (CurTok == ')')
        break;

      if (CurTok != ',') {
        LogError("Expected ')' or ',' in parameter list");
        return false;
      }
      getNextToken();
    }

    if (CurTok != ')') {
      LogError("Expected ')' after parameters");
      return false;
    }
    getNextToken();

    if (!ValidateParameterDefaults(Args))
      return false;

    while (CurTok == tok_newline)
      getNextToken();

    if (kind == AggregateKind::Interface && !modifiers.isAbstract)
      modifiers.isAbstract = true;

    const bool requiresBody = !modifiers.isAbstract && kind != AggregateKind::Interface;
    std::string QualifiedName = compositeName + "." + CanonicalMethodName;
    auto Proto = std::make_unique<PrototypeAST>(std::move(ReturnTypeInfo), QualifiedName,
                                                std::move(Args), false, returnsByRef,
                                                std::move(methodGenericParams));

    if (requiresBody) {
      if (CurTok != '{') {
        LogError("Expected '{' after method declaration");
        return false;
      }

      auto Body = ParseBlock();
      if (!Body)
        return false;

      auto Method = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
      Methods.emplace_back(std::move(Method), modifiers, methodKind, MethodName);
      if (operatorKind != OverloadableOperator::None)
        Methods.back().setOperatorKind(operatorKind);
    } else {
      if (CurTok == '{') {
        reportCompilerError("Abstract methods cannot declare a body");
        return false;
      }
      Methods.emplace_back(std::move(Proto), modifiers, methodKind, MethodName);
      if (operatorKind != OverloadableOperator::None)
        Methods.back().setOperatorKind(operatorKind);
    }

    return true;
  };

  auto memberNameCollides = [&](const std::string &name) -> bool {
    for (const auto &field : Fields) {
      if (field && field->getName() == name)
        return true;
    }
    for (const auto &prop : Properties) {
      if (prop && prop->getName() == name)
        return true;
    }
    for (const auto &method : Methods) {
      if (method.getDisplayName() == name)
        return true;
    }
    return false;
  };

  auto parseIndexerParameters = [&](std::vector<Parameter> &params) -> bool {
    if (CurTok != '[') {
      reportCompilerError("Expected '[' after 'this' in indexer declaration");
      return false;
    }
    getNextToken(); // eat '['
    SkipNewlines();
    while (CurTok != ']' && CurTok != tok_eof) {
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsRef = false;
      bool paramIsConst = false;
      if (CurTok == tok_const) {
        paramIsConst = true;
        getNextToken(); // eat 'const'
        SkipNewlines();
      }

      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      if (ParamName == "value") {
        reportCompilerError("Indexer parameter name 'value' is reserved");
        return false;
      }

      if (paramIsParams) {
        reportCompilerError("Indexers cannot declare 'params' parameters");
        return false;
      }

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      if (paramIsConst)
        ParamTypeInfo.isMutable = false;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        reportCompilerError("Indexer parameters cannot declare default values");
        return false;
      }
      params.push_back(std::move(param));

      SkipNewlines();
      if (CurTok == ']')
        break;
      if (CurTok != ',') {
        LogError("Expected ']' or ',' in indexer parameter list");
        return false;
      }
      getNextToken(); // eat ','
      SkipNewlines();
    }

    if (CurTok != ']') {
      LogError("Expected ']' after indexer parameters");
      return false;
    }
    getNextToken(); // eat ']'

    if (!ValidateParameterDefaults(params))
      return false;

    if (params.empty()) {
      reportCompilerError("Indexers must declare at least one parameter");
      return false;
    }

    return true;
  };

  auto parseAccessorBlock =
      [&](std::unique_ptr<AccessorAST> &getter,
          std::unique_ptr<AccessorAST> &setter) -> bool {
    if (CurTok != '{') {
      reportCompilerError("Expected '{' to begin accessor block");
      return false;
    }
    getNextToken(); // eat '{'
    SkipNewlines();

    while (CurTok != '}' && CurTok != tok_eof) {
      if (CurTok == tok_newline) {
        getNextToken();
        continue;
      }

      bool isGetter = false;
      if (CurTok == tok_get) {
        isGetter = true;
      } else if (CurTok == tok_set) {
        isGetter = false;
      } else {
        reportCompilerError("Expected 'get' or 'set' accessor");
        return false;
      }

      SourceLocation keywordLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat accessor keyword
      SkipNewlines();

      std::unique_ptr<ExprAST> exprBody;
      std::unique_ptr<BlockStmtAST> blockBody;
      bool isImplicit = false;

      if (CurTok == '{') {
        ScopedValueKeyword valueScope(!isGetter);
        blockBody = ParseBlock();
        if (!blockBody)
          return false;
      } else if (CurTok == tok_get || CurTok == tok_set || CurTok == '}' ||
                 CurTok == tok_newline) {
        isImplicit = true;
      } else {
        ScopedValueKeyword valueScope(!isGetter);
        exprBody = ParseExpression();
        if (!exprBody)
          return false;
      }

      if (isGetter) {
        if (getter) {
          reportCompilerError("Duplicate 'get' accessor");
          return false;
        }
        getter = std::make_unique<AccessorAST>(
            AccessorKind::Get, keywordLoc, std::move(exprBody),
            std::move(blockBody), isImplicit);
      } else {
        if (setter) {
          reportCompilerError("Duplicate 'set' accessor");
          return false;
        }
        setter = std::make_unique<AccessorAST>(
            AccessorKind::Set, keywordLoc, std::move(exprBody),
            std::move(blockBody), isImplicit);
      }

      SkipNewlines();
    }

    if (CurTok != '}') {
      LogError("Expected '}' after accessor block");
      return false;
    }
    getNextToken(); // eat '}'
    return true;
  };

  auto applyAccessorAccessDefaults =
      [&](MemberModifiers &modifiers,
          const PendingMemberModifiers &pending,
          const std::string &memberName,
          bool hasGetter,
          bool hasSetter,
          bool isIndexer) -> bool {
    if (!isIndexer && pending.hasExplicitAccess &&
        (pending.access == AccessSpecifier::Private ||
         pending.access == AccessSpecifier::Protected)) {
      const char *accessName =
          pending.access == AccessSpecifier::Private ? "private" : "protected";
      std::string effect;
      if (hasGetter && hasSetter)
        effect = "getting and setting";
      else if (hasGetter)
        effect = "getting";
      else if (hasSetter)
        effect = "setting";
      else
        effect = "accessing";
      reportCompilerError("Property '" + memberName + "' cannot be declared " +
                              accessName + " because it prevents " + effect,
                          std::string("Remove the ") + accessName +
                              " modifier or the accessor block.");
      return false;
    }

    if (!pending.hasExplicitAccess && hasSetter)
      modifiers.access = MemberAccess::PublicReadWrite();

    return true;
  };

  auto validateDelegateModifiers =
      [&](const PendingMemberModifiers &pending) -> bool {
        if (pending.hasExplicitAccess || pending.isStatic || pending.isConst ||
            pending.isAbstract || pending.isVirtual || pending.isOverride) {
          reportCompilerError(
              "Delegate declarations cannot use member modifiers");
          return false;
        }
        return true;
      };

  while (CurTok != '}' && CurTok != tok_eof) {
    SkipNewlines();
    if (CurTok == '}')
      break;

    PendingMemberModifiers pendingMods;
    if (!ParsePendingMemberModifiers(pendingMods))
      return nullptr;

    if (CurTok == tok_delegate) {
      if (!validateDelegateModifiers(pendingMods))
        return nullptr;
      auto delegateDecl = ParseDelegateDefinition();
      if (!delegateDecl)
        return nullptr;
      if (delegateDecl->getName() == compositeName) {
        reportCompilerError("Delegate '" + delegateDecl->getName() +
                            "' conflicts with enclosing type '" +
                            compositeName + "'");
        return nullptr;
      }
      Delegates.push_back(std::move(delegateDecl));
      continue;
    }

    if (CurTok == tok_tilde_identifier) {
      if (kind == AggregateKind::Interface) {
        reportCompilerError("Interfaces cannot declare destructors");
        return nullptr;
      }
      if (hasDestructor) {
        reportCompilerError("Type '" + compositeName + "' already declares a destructor");
        return nullptr;
      }

      std::string dtorTarget = IdentifierStr;
      getNextToken(); // eat '~Name'
      SkipNewlines();

      if (dtorTarget != compositeName) {
        reportCompilerError("Destructor name must match its enclosing type",
                            "Use '~" + compositeName + "()' to declare a destructor for '" +
                                compositeName + "'.");
        return nullptr;
      }

      std::string decoratedName = dtorTarget;
      if (!ParseOptionalGenericArgumentList(decoratedName, true))
        return nullptr;
      if (decoratedName != dtorTarget) {
        reportCompilerError("Destructors cannot declare generic parameters");
        return nullptr;
      }

      if (CurTok != '(') {
        LogError("Expected '(' after destructor name");
        return nullptr;
      }
      getNextToken(); // eat '('
      SkipNewlines();
      if (CurTok != ')') {
        reportCompilerError("Destructors cannot declare parameters");
        return nullptr;
      }
      getNextToken(); // eat ')'
      SkipNewlines();

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("Destructors cannot be static");
        return nullptr;
      }
      if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Destructors cannot be const-qualified");
        return nullptr;
      }
      if (modifiers.isAbstract) {
        reportCompilerError("Destructors cannot be abstract");
        return nullptr;
      }
      if (modifiers.isVirtual || modifiers.isOverride) {
        reportCompilerError("Destructors do not support virtual or override modifiers");
        return nullptr;
      }

      if (CurTok != '{') {
        LogError("Expected '{' after destructor declaration");
        return nullptr;
      }

      auto Body = ParseBlock();
      if (!Body)
        return nullptr;

      TypeInfo dtorReturn = buildDeclaredTypeInfo("void", false);
      std::string functionName = compositeName + ".~" + compositeName;
      auto Proto = std::make_unique<PrototypeAST>(
          std::move(dtorReturn), functionName, std::vector<Parameter>{},
          false, false);
      auto Dtor = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
      Methods.emplace_back(std::move(Dtor), modifiers, MethodKind::Destructor,
                           "~" + compositeName);
      destructorIndex = static_cast<int>(Methods.size()) - 1;
      hasDestructor = true;
      continue;
    }

    TypeInfo memberTypeInfo;
    std::string Type;
    bool hasMemberTypeInfo = false;

    bool memberReturnsByRef = false;
    if (CurTok == tok_ref) {
      memberReturnsByRef = true;
      getNextToken(); // eat 'ref'
      SkipNewlines();
    }

    if (!memberReturnsByRef && CurTok == tok_identifier &&
        IdentifierStr == compositeName) {
      getNextToken();
      SkipNewlines();

      std::vector<std::string> ctorGenericParams;
      if (ParseGenericParameterList(ctorGenericParams))
        SkipNewlines();

      if (!ctorGenericParams.empty() && CurTok != '(') {
        reportCompilerError("Generic parameter list must be followed by '(' in constructor declarations");
        return nullptr;
      }

      if (CurTok == '(') {
        if (kind == AggregateKind::Interface) {
          reportCompilerError("Interfaces cannot declare constructors");
          return nullptr;
        }
        MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
        if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
          reportCompilerError("Constructors cannot be static");
          return nullptr;
        }
        if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
          reportCompilerError("Constructors cannot be const");
          return nullptr;
        }

        if (!parseConstructor(modifiers, std::move(ctorGenericParams)))
          return nullptr;
        continue;
      }

      Type = compositeName;
      bool pointerSeen = false;
      if (!AppendTypeSuffix(Type, pointerSeen))
        return nullptr;
      memberTypeInfo = buildDeclaredTypeInfo(Type, false);
      hasMemberTypeInfo = true;
    } else if (IsValidType() || CurTok == '(') {
      if (!ParseCompleteTypeInfo(memberTypeInfo, memberReturnsByRef, true)) {
        LogError("Failed to parse member type");
        return nullptr;
      }
      Type = typeNameFromInfo(memberTypeInfo);
      hasMemberTypeInfo = true;
    } else {
      LogError("Expected field or method declaration inside type definition");
      return nullptr;
    }

    std::string MemberName;
    std::string CanonicalMemberName;
    OverloadableOperator memberOperator = OverloadableOperator::None;
    SourceLocation memberNameLoc = currentParser().currentTokenLocation;
    if (CurTok == tok_identifier) {
      MemberName = IdentifierStr;
      CanonicalMemberName = MemberName;
      getNextToken();
    } else if (CurTok == tok_this) {
      MemberName = "this";
      CanonicalMemberName = MemberName;
      getNextToken();
    } else if (!ParseOperatorMemberName(MemberName, CanonicalMemberName,
                                        memberOperator)) {
      if (IsPotentialOperatorToken(CurTok)) {
        reportCompilerError("Unsupported operator overload declaration",
                            "Only the language's explicit operator overload allowlist is supported.");
      } else {
        LogError("Expected member name after type");
      }
      return nullptr;
    }

    while (CurTok == tok_newline)
      getNextToken();

    if (memberOperator != OverloadableOperator::None && CurTok != '(') {
      // Recover better when malformed generic types leave trailing '<'/'>' tokens
      // where a member identifier was expected.
      if (memberOperator == OverloadableOperator::Less ||
          memberOperator == OverloadableOperator::Greater ||
          memberOperator == OverloadableOperator::LessEqual ||
          memberOperator == OverloadableOperator::GreaterEqual) {
        LogError("Expected identifier after type");
      } else {
        reportCompilerError(
            "Operator overload declarations must define a parameter list");
      }
      return nullptr;
    }

    std::vector<std::string> methodGenericParams;
    if (ParseGenericParameterList(methodGenericParams))
      SkipNewlines();
    if (!methodGenericParams.empty())
      maybeWarnGenericArity(methodGenericParams, MemberName, "Method");

    if (memberOperator != OverloadableOperator::None &&
        !methodGenericParams.empty()) {
      reportCompilerError("Operator overloads cannot declare generic parameter lists");
      return nullptr;
    }

    if (MemberName == "this" && CurTok == '[') {
      if (memberReturnsByRef) {
        reportCompilerError("Indexer declarations cannot use a leading 'ref' return modifier");
        return nullptr;
      }
      if (!methodGenericParams.empty()) {
        reportCompilerError("Indexers cannot declare generic parameter lists");
        return nullptr;
      }
      if (memberNameCollides(MemberName)) {
        reportCompilerError("Type '" + compositeName +
                            "' already declares member '" + MemberName + "'");
        return nullptr;
      }
      if (hasIndexer) {
        reportCompilerError("Type '" + compositeName +
                            "' already declares an indexer");
        return nullptr;
      }

      std::vector<Parameter> indexerParams;
      if (!parseIndexerParameters(indexerParams))
        return nullptr;

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      modifiers.isProperty = true;

      if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("Indexers must be instance members");
        return nullptr;
      }

      if (kind == AggregateKind::Interface && !modifiers.isAbstract)
        modifiers.isAbstract = true;

      SkipNewlines();
      std::unique_ptr<AccessorAST> getter;
      std::unique_ptr<AccessorAST> setter;
      if (!parseAccessorBlock(getter, setter))
        return nullptr;

      if (!getter && !setter) {
        reportCompilerError("Indexers must declare at least one accessor");
        return nullptr;
      }

      if (setter && !getter) {
        reportCompilerError("Indexer on type '" + compositeName +
                            "' does not define a getter");
        return nullptr;
      }

      if (!applyAccessorAccessDefaults(modifiers, pendingMods, MemberName,
                                       getter != nullptr, setter != nullptr,
                                       true))
        return nullptr;

      auto hasAccessorBody = [](const std::unique_ptr<AccessorAST> &accessor) {
        return accessor &&
               (accessor->hasBlockBody() || accessor->hasExpressionBody());
      };

      if ((kind == AggregateKind::Interface || modifiers.isAbstract) &&
          (hasAccessorBody(getter) || hasAccessorBody(setter))) {
        reportCompilerError("Abstract indexers cannot declare accessor bodies");
        return nullptr;
      }

      if (!modifiers.isAbstract &&
          ((getter && getter->isImplicit()) ||
           (setter && setter->isImplicit()))) {
        reportCompilerError("Indexer accessors must declare a body");
        return nullptr;
      }

      if (setter &&
          (modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Const indexers cannot declare a setter");
        return nullptr;
      }

      TypeInfo indexerInfo =
          hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
      auto Indexer = std::make_unique<PropertyAST>(
          typeNameFromInfo(indexerInfo), std::move(indexerInfo), MemberName,
          modifiers, nullptr, std::move(indexerParams), memberNameLoc,
          std::move(getter), std::move(setter));
      Properties.push_back(std::move(Indexer));
      hasIndexer = true;
      continue;
    }

    if (!methodGenericParams.empty() && CurTok != '(') {
      reportCompilerError("Generic parameter list must be followed by '(' in method declarations");
      return nullptr;
    }

    if (CurTok == '(') {
      MethodKind methodKind = MethodKind::Regular;
      if (MemberName == "this") {
        if (kind != AggregateKind::Class) {
          reportCompilerError("Only classes may define a this() formatter");
          return nullptr;
        }
        if (seenThisOverride) {
          reportCompilerError("Duplicate this() override",
                              "Only one this() method may be defined per class");
          return nullptr;
        }
        seenThisOverride = true;
        methodKind = MethodKind::ThisOverride;
      }

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Methods cannot be declared const yet",
                            "Use readonly state or immutable patterns instead");
        return nullptr;
      }

      if (methodKind == MethodKind::ThisOverride &&
          (modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("this() override cannot be static");
        return nullptr;
      }

      if (kind == AggregateKind::Class && methodKind == MethodKind::Regular &&
          memberOperator == OverloadableOperator::None &&
          !isPascalCase(MemberName)) {
        reportCompilerWarning("Class methods should use PascalCase names",
                              "Rename '" + MemberName + "' to PascalCase for consistency");
      }

      if (memberOperator != OverloadableOperator::None) {
        if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
          reportCompilerError("Operator overloads must be instance methods");
          return nullptr;
        }
        if (seenOperatorOverloads.contains(memberOperator)) {
          reportCompilerError("Duplicate overload for operator '" + MemberName +
                              "' on type '" + compositeName + "'");
          return nullptr;
        }
        seenOperatorOverloads.insert(memberOperator);
      }

      if (!parseCompositeMethod(memberTypeInfo, MemberName, CanonicalMemberName,
                                modifiers, methodKind, memberOperator,
                                memberReturnsByRef,
                                std::move(methodGenericParams)))
        return nullptr;
    } else {
      if (memberOperator != OverloadableOperator::None) {
        reportCompilerError("Operator overload declarations must define a parameter list");
        return nullptr;
      }
      if (memberReturnsByRef) {
        reportCompilerError("Only methods may declare a leading 'ref' return modifier");
        return nullptr;
      }
      if (!methodGenericParams.empty()) {
        reportCompilerError("Only methods may declare generic parameter lists");
        return nullptr;
      }
      if (MemberName == "this") {
        reportCompilerError("'this' can only be used as a formatter method name or indexer");
        return nullptr;
      }
      std::unique_ptr<ExprAST> MemberInitializer;
      if (CurTok == '=') {
        getNextToken(); // eat '='
        SkipNewlines();
        MemberInitializer = ParseExpression();
        if (!MemberInitializer)
          return nullptr;
      }
      SkipNewlines();

      if (CurTok == '{') {
        if (memberNameCollides(MemberName)) {
          reportCompilerError("Type '" + compositeName +
                              "' already declares member '" + MemberName + "'");
          return nullptr;
        }

        std::unique_ptr<AccessorAST> getter;
        std::unique_ptr<AccessorAST> setter;
        if (!parseAccessorBlock(getter, setter))
          return nullptr;

        if (!getter && !setter) {
          reportCompilerError("Properties must declare at least one accessor");
          return nullptr;
        }

        MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
        modifiers.isProperty = true;

        if (!applyAccessorAccessDefaults(modifiers, pendingMods, MemberName,
                                         getter != nullptr, setter != nullptr,
                                         false))
          return nullptr;

        if (kind == AggregateKind::Interface && !modifiers.isAbstract)
          modifiers.isAbstract = true;

        if (kind == AggregateKind::Interface &&
            (modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
          reportCompilerError("Interfaces cannot declare static properties");
          return nullptr;
        }

        auto hasAccessorBody = [](const std::unique_ptr<AccessorAST> &accessor) {
          return accessor &&
                 (accessor->hasBlockBody() || accessor->hasExpressionBody());
        };

        if ((kind == AggregateKind::Interface || modifiers.isAbstract) &&
            (hasAccessorBody(getter) || hasAccessorBody(setter))) {
          reportCompilerError("Abstract properties cannot declare accessor bodies");
          return nullptr;
        }

        if (setter &&
            (modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
          reportCompilerError("Const properties cannot declare a setter");
          return nullptr;
        }

        if (MemberInitializer &&
            (modifiers.storage & StorageFlag::Static) == StorageFlag::None) {
          reportCompilerError("Only static members may specify declaration initializers",
                              "Assign non-static members inside constructors instead");
          return nullptr;
        }

        if (kind == AggregateKind::Interface && MemberInitializer) {
          reportCompilerError("Interfaces cannot declare property initializers");
          return nullptr;
        }

        TypeInfo propInfo =
            hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
        std::string propTypeName = typeNameFromInfo(propInfo);
        if (kind != AggregateKind::Interface) {
          auto Field = std::make_unique<FieldAST>(propTypeName, propInfo, MemberName,
                                                  modifiers,
                                                  std::move(MemberInitializer));
          Fields.push_back(std::move(Field));
        } else {
          MemberInitializer.reset();
        }

        auto Property = std::make_unique<PropertyAST>(
            propTypeName, std::move(propInfo), MemberName, modifiers, nullptr,
            std::vector<Parameter>{}, memberNameLoc,
            std::move(getter), std::move(setter));
        Properties.push_back(std::move(Property));
        continue;
      }

      if (kind == AggregateKind::Interface) {
        reportCompilerError("Interfaces cannot declare fields");
        return nullptr;
      }
      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if (MemberInitializer &&
          (modifiers.storage & StorageFlag::Static) == StorageFlag::None) {
        reportCompilerError("Only static members may specify declaration initializers",
                            "Assign non-static members inside constructors instead");
        return nullptr;
      }
      TypeInfo fieldInfo =
          hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
      std::string fieldTypeName = typeNameFromInfo(fieldInfo);
      auto Field = std::make_unique<FieldAST>(fieldTypeName, std::move(fieldInfo),
                                              MemberName, modifiers,
                                              std::move(MemberInitializer));
      Fields.push_back(std::move(Field));
    }
  }

  if (kind != AggregateKind::Interface && !isAbstractComposite && !hasConstructor) {
    const char *kindDescription =
        (kind == AggregateKind::Struct) ? "Struct" : "Class";
    reportCompilerError(std::string(kindDescription) + " '" + compositeName +
                            "' must declare at least one constructor",
                        "Provide at least one constructor so that all members can be initialized explicitly.");
    return nullptr;
  }

  if (CurTok != '}') {
    LogError("Expected '}' at end of type definition");
    return nullptr;
  }

  getNextToken();

  if (kind == AggregateKind::Struct) {
    StructNames.insert(compositeName);
  } else {
    ClassNames.insert(compositeName);
    StructNames.insert(compositeName);
  }

  auto Result = std::make_unique<StructAST>(
      kind, compositeName, std::move(Delegates), std::move(Fields),
      std::move(Properties),
      std::move(Methods),
      std::move(baseTypes), std::move(genericParameters));
  Result->setBaseClass(std::move(baseClass));
  Result->setInterfaces(std::move(interfaceTypes));
  Result->setBaseTypeInfos(std::move(baseTypeInfos));
  Result->setBaseClassInfo(std::move(baseClassInfo));
  Result->setInterfaceTypeInfos(std::move(interfaceTypeInfos));
  Result->setAbstract(isAbstractComposite);
  ClassInheritanceMetadata inheritance;
  inheritance.baseClassName = Result->getBaseClass();
  inheritance.interfaceNames = Result->getInterfaces();
  if (kind == AggregateKind::Struct) {
    inheritance.defaultMemberAccess = MemberAccess::PublicReadWrite();
  } else {
    inheritance.defaultMemberAccess = MemberAccess::ReadPublicWritePrivate();
  }
  for (const auto &method : Result->getMethods()) {
    if (method.getKind() == MethodKind::Constructor)
      inheritance.constructorAccesses.push_back(method.getModifiers().access);
  }
  Result->setInheritanceMetadata(std::move(inheritance));
  if (destructorIndex >= 0)
    Result->setDestructorIndex(destructorIndex);
  return Result;
}
