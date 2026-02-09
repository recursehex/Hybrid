#include "parser/parser_internal.h"
#include "toplevel.h"
#include <cstdio>
#include <utility>
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"

static void setDiagnosticLocation(SourceLocation loc) {
  if (!loc.isValid())
    return;
  currentParser().currentTokenLocation = loc;
  currentLexer().setTokenStart(loc);
}

bool ValidateParameterDefaults(const std::vector<Parameter> &Args) {
  bool sawDefault = false;
  for (const auto &param : Args) {
    if (param.IsParams)
      return true;
    if (param.HasDefault) {
      sawDefault = true;
      continue;
    }

    if (sawDefault) {
      setDiagnosticLocation(param.NameLocation);
      reportCompilerError(
          "Parameters with default values must be the trailing parameters");
      return false;
    }
  }

  return true;
}

bool ConvertParenInitializerToConstructor(std::unique_ptr<ExprAST> &Initializer,
                                          const TypeInfo &declInfo) {
  if (declInfo.pointerDepth > 0 || declInfo.isArray)
    return false;
  if (declInfo.typeName.empty())
    return false;

  auto *paren = dynamic_cast<ParenExprAST *>(Initializer.get());
  if (!paren)
    return false;

  auto isKnownComposite = [](const std::string &name) {
    return StructNames.contains(name) ||
           ClassNames.contains(name) ||
           IsActiveStructName(name) ||
           IsActiveClassName(name);
  };

  const std::string &fullTypeName = declInfo.typeName;
  std::string constructorName = fullTypeName;

  bool knownComposite = isKnownComposite(fullTypeName);
  if (!knownComposite && !declInfo.baseTypeName.empty() &&
      declInfo.baseTypeName != fullTypeName) {
    knownComposite = isKnownComposite(declInfo.baseTypeName);
    if (knownComposite)
      constructorName = fullTypeName;
  }

  if (!knownComposite)
    return false;

  std::vector<std::unique_ptr<ExprAST>> args;
  if (paren->isTuple()) {
    args = paren->takeElements();
  } else {
    auto single = paren->takeSingleElement();
    if (single)
      args.push_back(std::move(single));
  }

  Initializer = std::make_unique<CallExprAST>(constructorName, std::move(args));
  return true;
}

/// prototype
///   ::= id id '(' (id id (',' id id)*)? ')'  // C-style: returntype name(type param, type param, ...)
std::unique_ptr<PrototypeAST> ParsePrototype(bool isUnsafe) {
  // Check for ref return type
  bool returnsByRef = false;
  if (CurTok == tok_ref) {
    returnsByRef = true;
    getNextToken(); // eat 'ref'
  }

  // Parse return type
  if (!IsValidType() && CurTok != '(')
    return LogErrorP("Expected return type in prototype");

  TypeInfo ReturnTypeInfo;
  if (!ParseCompleteTypeInfo(ReturnTypeInfo, returnsByRef))
    return LogErrorP("Failed to parse return type");

  std::string FnName;
  if (CurTok == tok_identifier) {
    FnName = IdentifierStr;
    getNextToken();
  } else if (CurTok == tok_string_literal) {
    FnName = StringVal;
    getNextToken();
  } else {
    return LogErrorP("Expected function name in prototype");
  }

  std::vector<std::string> GenericParams;
  if (ParseGenericParameterList(GenericParams))
    SkipNewlines();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  GenericParameterScope parameterScope(GenericParams);

  std::vector<Parameter> Args;
  getNextToken(); // eat '('
  SkipNewlines();

  if (CurTok != ')') {
    // Parse parameters
    while (true) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      // Check for ref parameter
      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(')
        return LogErrorP("Expected parameter type");

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef))
        return LogErrorP("Failed to parse parameter type");

      if (CurTok != tok_identifier)
        return LogErrorP("Expected parameter name");

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
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
          return nullptr;
      }

      Args.push_back(std::move(param));

      if (CurTok == ')') {
        getNextToken(); // eat ')'
        break;
      }

      if (CurTok != ',')
        return LogErrorP("Expected ',' or ')' in parameter list");

      getNextToken(); // eat ','
      SkipNewlines();
    }
  } else {
    getNextToken(); // eat ')'
  }

  if (!ValidateParameterDefaults(Args))
    return nullptr;

  return std::make_unique<PrototypeAST>(std::move(ReturnTypeInfo), FnName,
                                        std::move(Args), isUnsafe, returnsByRef,
                                        std::move(GenericParams));
}

/// definition ::= prototype block
/// Note: 'unsafe' is handled by the caller (HandleUnsafe), which sets the context
std::unique_ptr<FunctionAST> ParseDefinition() {
  // Check if we're in an unsafe context (set by HandleUnsafe)
  bool isUnsafe = isInUnsafeContext();

  auto Proto = ParsePrototype(isUnsafe);
  if (!Proto) {
    return nullptr;
  }

  auto Body = ParseBlock();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}

/// external ::= 'extern' ['unsafe'] prototype
std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern.
  SkipNewlines();

  bool explicitUnsafe = false;
  if (CurTok == tok_unsafe) {
    explicitUnsafe = true;
    getNextToken(); // eat 'unsafe'
    SkipNewlines();
    enterUnsafeContext();
  }

  bool prototypeIsUnsafe = explicitUnsafe || isInUnsafeContext();
  auto Proto = ParsePrototype(prototypeIsUnsafe);
  if (Proto)
    Proto->markAsExtern();

  if (explicitUnsafe)
    exitUnsafeContext();
  return Proto;
}

/// delegate ::= 'delegate' ['ref'] type identifier '(' params? ')'
std::unique_ptr<DelegateDeclAST> ParseDelegateDefinition() {
  getNextToken(); // eat 'delegate'
  SkipNewlines();

  bool returnsByRef = false;
  if (CurTok == tok_ref) {
    returnsByRef = true;
    getNextToken(); // eat 'ref'
    SkipNewlines();
  }

  if (!IsValidType() && CurTok != '(') {
    reportCompilerError("Expected return type in delegate declaration");
    return nullptr;
  }

  TypeInfo returnTypeInfo;
  if (!ParseCompleteTypeInfo(returnTypeInfo, returnsByRef)) {
    reportCompilerError("Failed to parse delegate return type");
    return nullptr;
  }

  if (CurTok != tok_identifier) {
    reportCompilerError("Expected delegate name after return type");
    return nullptr;
  }

  std::string delegateName = IdentifierStr;
  SourceLocation nameLoc = currentParser().currentTokenLocation;
  getNextToken();

  if (StructNames.contains(delegateName) || ClassNames.contains(delegateName) ||
      DelegateNames.contains(delegateName)) {
    reportCompilerError("Type '" + delegateName + "' is already defined");
    return nullptr;
  }

  SkipNewlines();
  if (CurTok != '(') {
    reportCompilerError("Expected '(' in delegate declaration");
    return nullptr;
  }

  std::vector<Parameter> params;
  getNextToken(); // eat '('
  SkipNewlines();

  if (CurTok != ')') {
    while (true) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        reportCompilerError("Expected parameter type");
        return nullptr;
      }

      TypeInfo paramTypeInfo;
      if (!ParseCompleteTypeInfo(paramTypeInfo, paramIsRef)) {
        reportCompilerError("Failed to parse parameter type");
        return nullptr;
      }

      if (CurTok != tok_identifier) {
        reportCompilerError("Expected parameter name");
        return nullptr;
      }

      std::string paramName = IdentifierStr;
      SourceLocation paramNameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(paramTypeInfo);
      param.Name = paramName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(paramTypeInfo);
      param.NameLocation = paramNameLoc;
      param.ParamsLocation = paramsLoc;

      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return nullptr;
      }

      params.push_back(std::move(param));

      if (CurTok == ')') {
        getNextToken(); // eat ')'
        break;
      }

      if (CurTok != ',') {
        reportCompilerError("Expected ',' or ')' in delegate parameter list");
        return nullptr;
      }

      getNextToken(); // eat ','
      SkipNewlines();
    }
  } else {
    getNextToken(); // eat ')'
  }

  if (!ValidateParameterDefaults(params))
    return nullptr;

  DelegateNames.insert(delegateName);
  return std::make_unique<DelegateDeclAST>(
      std::move(returnTypeInfo), delegateName, std::move(params),
      returnsByRef, nameLoc);
}

/// ParseTypeIdentifier - Handles top-level declarations that start with a type
/// Could be either a variable declaration or function definition
bool ParseTypeIdentifier(bool isRef) {
  // Parse the complete type (including arrays and pointers)
  TypeInfo typeInfo;
  if (!ParseCompleteTypeInfo(typeInfo, isRef)) {
    return false;
  }

  SkipNewlines();

  std::string Type = typeNameFromInfo(typeInfo);

  // Handle constructor call (struct name followed directly by '(')
  if (CurTok == '(' && (StructNames.contains(Type) || ClassNames.contains(Type))) {
    std::vector<std::unique_ptr<ExprAST>> Args;
    std::vector<std::string> ArgNames;
    std::vector<SourceLocation> ArgNameLocs;
    std::vector<SourceLocation> ArgEqualLocs;
    if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
      return false;

    auto Call = std::make_unique<CallExprAST>(Type, std::move(Args),
                                              std::move(ArgNames),
                                              std::move(ArgNameLocs),
                                              std::move(ArgEqualLocs));
    if (auto CallIR = Call->codegen()) {
      fprintf(stderr, "Generated struct instantiation IR:\n");
      CallIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate struct instantiation");
      return false;
    }
    return true;
  }

  if (CurTok != tok_identifier) {
    reportCompilerError("Expected identifier after type");
    return false;
  }

  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier

  SkipNewlines();

  std::vector<std::string> genericParams;
  if (ParseGenericParameterList(genericParams))
    SkipNewlines();

  // Check what follows
  if (CurTok == '(') {
    // It's a function definition
    // Need to parse the full prototype and then the body
    GenericParameterScope parameterScope(genericParams);
    getNextToken(); // eat '('
    SkipNewlines();

    std::vector<Parameter> Args;

    // Parse parameters
    if (CurTok != ')') {
      while (true) {
        SkipNewlines();
        bool paramIsParams = false;
        SourceLocation paramsLoc{};
        if (CurTok == tok_params) {
          paramIsParams = true;
          paramsLoc = currentParser().currentTokenLocation;
          getNextToken(); // eat 'params'
          SkipNewlines();
        }

        // Check for ref parameter
        bool paramIsRef = false;
        if (CurTok == tok_ref) {
          paramIsRef = true;
          getNextToken(); // eat 'ref'
          SkipNewlines();
        }

        if (!IsValidType() && CurTok != '(') {
          reportCompilerError("Expected parameter type");
          return false;
        }

        TypeInfo ParamTypeInfo;
        if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
          reportCompilerError("Failed to parse parameter type");
          return false;
        }

        if (CurTok != tok_identifier) {
          reportCompilerError("Expected parameter name");
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

        if (CurTok == ')') {
          getNextToken(); // eat ')'
          break;
        }

        if (CurTok != ',') {
          reportCompilerError("Expected ',' or ')' in parameter list");
          return false;
        }

        getNextToken(); // eat ','
        SkipNewlines();
      }
    } else {
      getNextToken(); // eat ')'
    }

    if (!ValidateParameterDefaults(Args))
      return false;

    // Create prototype
    auto Proto = std::make_unique<PrototypeAST>(std::move(typeInfo), Name,
                                                std::move(Args), false, isRef,
                                                std::move(genericParams));

    // Parse the body
    auto Body = ParseBlock();
    if (!Body) {
      if (!currentParser().hadError)
        reportCompilerError("Expected function body");
      return false;
    }

    if (currentParser().hadError)
      return false;

    auto Fn = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    if (Fn->getProto()->getGenericParameters().empty()) {
      fprintf(stderr, "Parsed function successfully, generating code...\n");
      if (auto FnIR = Fn->codegen()) {
        fprintf(stderr, "Generated function IR:\n");
        FnIR->print(llvm::errs());
        fprintf(stderr, "\n");
      } else {
        reportCompilerError("Failed to generate IR for function");
      }
    } else {
      RegisterGenericFunctionTemplate(std::move(Fn));
    }
    return true;
  } else if (CurTok == '=') {
    // It's a variable declaration
    getNextToken(); // eat '='
    SkipNewlines();

    TypeInfo declInfo = typeInfo;

    // Check if the initializer has 'ref' keyword (for linking two variables)
    bool initializerIsRef = false;
    if (CurTok == tok_ref) {
      initializerIsRef = true;
      getNextToken(); // eat 'ref'
      SkipNewlines();
    }

    auto Initializer = ParseExpression();
    if (!Initializer) {
      reportCompilerError("Expected expression after '='");
      return false;
    }

    if (!initializerIsRef)
      ConvertParenInitializerToConstructor(Initializer, declInfo);

    // If initializer has ref, wrap it in RefExprAST
    if (initializerIsRef) {
      Initializer = std::make_unique<RefExprAST>(std::move(Initializer));
    }

    // Create the variable declaration AST but don't generate code here
    // Code generation will be handled by HandleVariableDeclaration in toplevel.cpp
    auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(
        std::move(declInfo), Name, std::move(Initializer), isRef);

    if (IsInteractiveMode())
      fprintf(stderr, "Parsed variable declaration, generating code...\n");

    if (auto VarIR = VarDecl->codegen()) {
      (void)VarIR;
      NoteTopLevelStatementEmitted();
      if (IsInteractiveMode()) {
        fprintf(stderr, "Generated variable declaration IR:\n");
        if (auto *TopFunc = getModule()->getFunction("__hybrid_top_level"))
          TopFunc->print(llvm::errs());
        fprintf(stderr, "\n");
      }
    } else {
      reportCompilerError("Failed to generate IR for variable declaration");
      return false;
    }
    return true;
  } else {
    reportCompilerError("Expected '(' or '=' after identifier in top-level declaration");
    return false;
  }
}
