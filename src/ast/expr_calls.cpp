#include "ast_internal.h"

class ScopedErrorLocation {
public:
  explicit ScopedErrorLocation(SourceLocation loc) : active(loc.isValid()) {
    if (active) {
      prevParserLoc = currentParser().currentTokenLocation;
      prevParserPrevLoc = currentParser().previousTokenLocation;
      prevLexerLoc = currentLexer().tokenStart();
      currentParser().currentTokenLocation = loc;
      currentParser().previousTokenLocation = loc;
      currentLexer().setTokenStart(loc);
    }
  }

  ScopedErrorLocation(const ScopedErrorLocation &) = delete;
  ScopedErrorLocation &operator=(const ScopedErrorLocation &) = delete;

  ~ScopedErrorLocation() {
    if (!active)
      return;
    currentParser().currentTokenLocation = prevParserLoc;
    currentParser().previousTokenLocation = prevParserPrevLoc;
    currentLexer().setTokenStart(prevLexerLoc);
  }

private:
  bool active = false;
  SourceLocation prevParserLoc{};
  SourceLocation prevParserPrevLoc{};
  SourceLocation prevLexerLoc{};
};

struct ProvidedArgument {
  llvm::Value *value = nullptr;
  bool isRef = false;
  const ExprAST *expr = nullptr;
  std::string name;
  SourceLocation nameLoc{};
  SourceLocation equalsLoc{};
};

class InjectedValueExprAST : public ExprAST {
public:
  InjectedValueExprAST(llvm::Value *value, std::string typeName,
                       bool isTemporary)
      : value(value) {
    setTypeName(typeName);
    markTemporary(isTemporary);
  }

  llvm::Value *codegen() override { return value; }

private:
  llvm::Value *value = nullptr;
};

// Generate code for function calls, including struct constructors
llvm::Value *CallExprAST::codegen() {
  std::string decoratedCallee = getCallee();
  std::string baseCallee = decoratedCallee;
  std::vector<TypeInfo> explicitTypeArgs;
  if (!parseExplicitTypeArgumentSuffix(decoratedCallee, baseCallee,
                                       explicitTypeArgs))
    return nullptr;
  bool calleeIsCompositeType = false;
  if (decoratedCallee.find('<') != std::string::npos) {
    TypeInfo boundCallee = applyActiveTypeBindings(makeTypeInfo(decoratedCallee));
    decoratedCallee = typeNameFromInfo(boundCallee);
    if (lookupCompositeInfo(decoratedCallee, /*countHit=*/false))
      calleeIsCompositeType = true;
  }
  const auto *functionTemplates =
      lookupGenericFunctionTemplates(baseCallee);
  if (baseCallee == "describeType" && !hasCalleeExpr()) {
    if (getArgs().size() != 1) {
      reportCompilerError("describeType() expects exactly one argument");
      return nullptr;
    }
    auto *literalArg =
        dynamic_cast<StringExprAST *>(getArgs().front().get());
    if (!literalArg) {
      reportCompilerError(
          "describeType() requires a string literal argument");
      return nullptr;
    }
    auto summary = buildDescribeTypeSummary(literalArg->getValue());
    if (!summary)
      return nullptr;
    setTypeName("string");
    markTemporary();
    return emitStringLiteral(*summary);
  }

  auto trySmartPointerBuilder =
      [&](std::string_view helperName,
          std::string_view targetBase) -> llvm::Value * {
        if (hasCalleeExpr() || baseCallee != helperName)
          return nullptr;
        if (explicitTypeArgs.size() != 1) {
          reportCompilerError(std::string(helperName) +
                              " requires exactly one explicit type argument");
          return nullptr;
        }
        TypeInfo payloadInfo =
            applyActiveTypeBindings(explicitTypeArgs.front());
        finalizeTypeInfoMetadata(payloadInfo);
        std::string payloadName = typeNameFromInfo(payloadInfo);
        std::string targetName =
            std::string(targetBase) + "<" + payloadName + ">";
        TypeInfo targetInfo = makeTypeInfo(targetName);
        finalizeTypeInfoMetadata(targetInfo);
        bool treatAsTuple = Args.size() > 1;
        ParenExprAST paren(std::move(Args), treatAsTuple);
        paren.setTypeName(targetName);
        llvm::Value *constructed =
            emitTargetTypedConstruction(targetInfo, paren);
        resetArgs(paren.takeElements());
        if (!constructed)
          return nullptr;
        setTypeName(typeNameFromInfo(targetInfo));
        markTemporary();
        return constructed;
      };

  if (auto *built = trySmartPointerBuilder("make_unique", "unique"))
    return built;
  if (auto *built = trySmartPointerBuilder("make_shared", "shared"))
    return built;

  if (!hasCalleeExpr() && baseCallee == "weak_from_shared") {
    if (explicitTypeArgs.size() != 1) {
      reportCompilerError("weak_from_shared requires exactly one explicit type argument");
      return nullptr;
    }
    TypeInfo payloadInfo =
        applyActiveTypeBindings(explicitTypeArgs.front());
    finalizeTypeInfoMetadata(payloadInfo);
    std::string targetName =
        "weak<" + typeNameFromInfo(payloadInfo) + ">";
    TypeInfo targetInfo = makeTypeInfo(targetName);
    finalizeTypeInfoMetadata(targetInfo);
    if (getArgs().size() != 1) {
      reportCompilerError(
          "weak_from_shared expects exactly one shared<T> argument");
      return nullptr;
    }
    ParenExprAST paren(std::move(Args), false);
    paren.setTypeName(targetName);
    llvm::Value *constructed =
        emitTargetTypedConstruction(targetInfo, paren);
    resetArgs(paren.takeElements());
    if (!constructed)
      return nullptr;
    setTypeName(typeNameFromInfo(targetInfo));
    markTemporary();
    return constructed;
  }

  if (!hasCalleeExpr()) {
    TypeInfo calleeInfo = makeTypeInfo(decoratedCallee);
    finalizeTypeInfoMetadata(calleeInfo);
    if (calleeInfo.isSmartPointer()) {
      if (!materializeCompositeInstantiation(calleeInfo))
        return nullptr;
      bool treatAsTuple = Args.size() > 1;
      ParenExprAST paren(std::move(Args), treatAsTuple);
      paren.setTypeName(typeNameFromInfo(calleeInfo));
      llvm::Value *constructed =
          emitTargetTypedConstruction(calleeInfo, paren);
      resetArgs(paren.takeElements());
      if (!constructed)
        return nullptr;
      setTypeName(typeNameFromInfo(calleeInfo));
      markTemporary();
      return constructed;
    }
  }

  bool treatAsFunctionGenerics =
      !explicitTypeArgs.empty() &&
      FindGenericTemplate(baseCallee) == nullptr && !calleeIsCompositeType;
  bool preferGeneric = treatAsFunctionGenerics;

  if (!treatAsFunctionGenerics && explicitTypeArgs.empty() &&
      functionTemplates && !functionTemplates->empty()) {
    bool hasNonGenericOverload = false;
    if (auto overloadIt = CG.functionOverloads.find(baseCallee);
        overloadIt != CG.functionOverloads.end()) {
      hasNonGenericOverload = std::ranges::any_of(
          overloadIt->second,
          [](const FunctionOverload &overload) {
            return !overload.isGenericInstantiation;
          });
    }
    if (!hasNonGenericOverload) {
      reportCompilerError("Function '" + baseCallee +
                          "' requires explicit type arguments");
      return nullptr;
    }
  }

  if (hasCalleeExpr()) {
    if (auto *member = dynamic_cast<MemberAccessExprAST *>(getCalleeExpr()))
      return codegenMemberCall(*member);
    if (dynamic_cast<BaseExprAST *>(getCalleeExpr())) {
      const ActiveCompositeContext *ctx = currentCompositeContext();
      if (!ctx || ctx->kind != MethodKind::Constructor || ctx->isStatic) {
        reportCompilerError("'base(...)' is only valid inside instance constructors");
        return nullptr;
      }

      const CompositeTypeInfo *metadata = lookupCompositeInfo(ctx->name);
      if (!metadata || !metadata->baseClass) {
        reportCompilerError("Type '" + ctx->name + "' does not have a base class to construct");
        return nullptr;
      }

      auto structIt = StructTypes.find(ctx->name);
      if (structIt == StructTypes.end()) {
        reportCompilerError("Internal error: missing struct type for '" +
                            ctx->name + "' during base(...) emission");
        return nullptr;
      }

      auto thisIt = NamedValues.find("this");
      if (thisIt == NamedValues.end()) {
        reportCompilerError("Internal error: missing 'this' in constructor for '" +
                            ctx->name + "'");
        return nullptr;
      }

      auto *structPtr = llvm::dyn_cast<llvm::AllocaInst>(thisIt->second);
      if (!structPtr) {
        reportCompilerError("Internal error: unexpected 'this' storage while invoking base constructor for '" +
                            ctx->name + "'");
        return nullptr;
      }

      markBaseConstructorCall();
      setTypeName(*metadata->baseClass);
      return emitBaseConstructorInitialization(ctx->name, structIt->second,
                                               structPtr, *metadata, getArgs());
    }
    llvm::Value *calleeValue = getCalleeExpr()->codegen();
    if (!calleeValue)
      return nullptr;
    const DelegateTypeInfo *delegateInfo =
        resolveDelegateInfoForExpr(getCalleeExpr());
    if (!delegateInfo)
      return LogErrorV("Call target is not a function or delegate");
    return emitDelegateCall(*this, calleeValue, *delegateInfo,
                            getCalleeExpr());
  }

  if (const TypeInfo *symInfo = lookupTypeInfo(baseCallee)) {
    if (const DelegateTypeInfo *delegateInfo =
            lookupDelegateInfo(*symInfo)) {
      VariableExprAST delegateExpr(baseCallee);
      llvm::Value *calleeValue = delegateExpr.codegen();
      if (!calleeValue)
        return nullptr;
      return emitDelegateCall(*this, calleeValue, *delegateInfo,
                              &delegateExpr);
    }
  }

  std::vector<bool> ArgIsRef;
  ArgIsRef.reserve(getArgs().size());
  std::vector<llvm::Value *> ArgValues;
  ArgValues.reserve(getArgs().size());

  for (const auto &ArgExpr : getArgs()) {
    bool handled = false;

    if (baseCallee == "print") {
      auto resolveCompositeForArg = [&]() -> std::optional<std::string> {
        std::string name = baseCompositeName(ArgExpr->getTypeName());
        if (!name.empty())
          return name;

        if (auto *var = dynamic_cast<VariableExprAST *>(ArgExpr.get())) {
          if (const TypeInfo *info = lookupTypeInfo(var->getName())) {
            name = baseCompositeName(typeNameFromInfo(*info));
            if (!name.empty())
              return name;
          }
        }

        return std::nullopt;
      };

      std::optional<std::string> candidateName = resolveCompositeForArg();
      if (candidateName) {
        auto metaIt = CG.compositeMetadata.find(*candidateName);
        if (metaIt != CG.compositeMetadata.end() &&
            metaIt->second.thisOverride) {
          llvm::Value *instanceVal = ArgExpr->codegen_ptr();
          if (!instanceVal) {
            instanceVal = ArgExpr->codegen();
            if (!instanceVal)
              return nullptr;
          }

          llvm::Value *stringValue = emitThisOverrideString(
              metaIt->second, *candidateName, instanceVal);
          if (!stringValue)
            return nullptr;

          ArgIsRef.push_back(false);
          ArgValues.push_back(stringValue);
          handled = true;
        } else if (metaIt != CG.compositeMetadata.end()) {
          reportCompilerError(
              "Cannot print value of type '" + *candidateName +
                  "' because it does not implement string this()",
              "Add 'string this()' to provide a formatter or convert the value "
              "to string manually.");
          return nullptr;
        }
      }
    }

    if (handled)
      continue;

    const ExprAST *coreArg = unwrapRefExpr(ArgExpr.get());
    if (auto *var = dynamic_cast<const VariableExprAST *>(coreArg)) {
      if (isDelegateFunctionReference(*var)) {
        ArgIsRef.push_back(false);
        ArgValues.push_back(nullptr);
        continue;
      }
    } else if (auto *member = dynamic_cast<const MemberAccessExprAST *>(coreArg)) {
      if (isDelegateMethodReference(*member)) {
        ArgIsRef.push_back(false);
        ArgValues.push_back(nullptr);
        continue;
      }
    }

    llvm::Value *Value = ArgExpr->codegen();
    if (!Value)
      return nullptr;

    if (baseCallee == "print") {
      std::string nameAfterCodegen =
          baseCompositeName(ArgExpr->getTypeName());
      if (!nameAfterCodegen.empty()) {
        auto metaIt = CG.compositeMetadata.find(nameAfterCodegen);
        if (metaIt != CG.compositeMetadata.end() &&
            metaIt->second.thisOverride) {
          llvm::Value *stringValue = emitThisOverrideString(
              metaIt->second, nameAfterCodegen, Value);
          if (!stringValue)
            return nullptr;
          ArgIsRef.push_back(false);
          ArgValues.push_back(stringValue);
          continue;
        } else if (metaIt != CG.compositeMetadata.end()) {
          reportCompilerError(
              "Cannot print value of type '" + nameAfterCodegen +
                  "' because it does not implement string this()",
              "Add 'string this()' to provide a formatter or convert the value "
              "to string manually.");
          return nullptr;
        }
      }
    }

    bool isRef = dynamic_cast<RefExprAST *>(ArgExpr.get()) != nullptr;
    ArgIsRef.push_back(isRef);
    ArgValues.push_back(Value);
  }

  if (treatAsFunctionGenerics) {
    if (!functionTemplates) {
      reportCompilerError("Function '" + baseCallee +
                          "' does not declare generic parameters");
      return nullptr;
    }

    const auto arities = collectGenericArities(*functionTemplates);
    if (!std::ranges::any_of(
            arities, [&](std::size_t arity) { return arity == explicitTypeArgs.size(); })) {
      reportCompilerError("Function '" + baseCallee + "' expects " +
                              formatArityList(arities) + " type argument(s)",
                          "Provide exactly " + formatArityList(arities) +
                              " type argument(s) when calling this generic function.");
      return nullptr;
    }

    if (!InstantiateGenericFunction(baseCallee, explicitTypeArgs, nullptr))
      return nullptr;
  }

  if (decoratedCallee.find('<') != std::string::npos && !calleeIsCompositeType)
    lookupCompositeInfo(decoratedCallee);

  if (StructTypes.contains(decoratedCallee)) {
    markTemporary();
    llvm::Value *ConstructedValue =
        emitResolvedCall(baseCallee, std::move(ArgValues), ArgIsRef,
                         preferGeneric);
    if (!ConstructedValue)
      return nullptr;

    setTypeName(decoratedCallee);

    llvm::StructType *StructType = StructTypes[decoratedCallee];
    if (ConstructedValue->getType()->isPointerTy())
      return ConstructedValue;

    if (ConstructedValue->getType() != StructType)
      ConstructedValue = castToType(ConstructedValue, StructType);

    llvm::AllocaInst *StructAlloca = Builder->CreateAlloca(
        StructType, nullptr, decoratedCallee + "_inst");
    Builder->CreateStore(ConstructedValue, StructAlloca);
    return StructAlloca;
  }

  return emitResolvedCall(baseCallee, std::move(ArgValues), ArgIsRef,
                          preferGeneric);
}

llvm::Value *emitResolvedCallInternal(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef,
    const std::vector<std::unique_ptr<ExprAST>> *originalArgs,
    bool preferGeneric, FunctionOverload *forced, ExprAST *typeOwner,
    std::vector<ProvidedArgument> *providedArgs, bool preserveRefReturn) {
  struct CandidateCall {
    FunctionOverload *overload = nullptr;
    llvm::Function *function = nullptr;
    std::vector<int> binding;
    std::vector<int> paramsBinding;
    bool paramsDirect = false;
    unsigned conversions = 0;
  };

  auto findParamsIndex = [](const std::vector<bool> &flags) -> int {
    for (size_t i = 0; i < flags.size(); ++i) {
      if (flags[i])
        return static_cast<int>(i);
    }
    return -1;
  };

  std::vector<FunctionOverload *> overloadList;
  if (forced) {
    overloadList.push_back(forced);
  } else {
    auto overloadIt = CG.functionOverloads.find(calleeBase);
    if (overloadIt == CG.functionOverloads.end()) {
      std::string baseName = baseCompositeName(calleeBase);
      if (!baseName.empty()) {
        auto baseIt = CG.functionOverloads.find(baseName);
        if (baseIt != CG.functionOverloads.end())
          overloadIt = baseIt;
      }
    }
    if (overloadIt == CG.functionOverloads.end())
      return LogErrorV(("Unknown function referenced: " + calleeBase).c_str());

    for (auto &overload : overloadIt->second)
      overloadList.push_back(&overload);
  }

  std::vector<ProvidedArgument> providedStorage;
  std::vector<ProvidedArgument> *provided = providedArgs;
  if (!provided) {
    providedStorage.reserve(ArgValues.size());
    for (size_t i = 0; i < ArgValues.size(); ++i) {
      ProvidedArgument arg;
      arg.value = ArgValues[i];
      arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
      if (originalArgs && i < originalArgs->size())
        arg.expr = (*originalArgs)[i].get();
      providedStorage.push_back(std::move(arg));
    }
    provided = &providedStorage;
  }

  const bool hasNamedArguments = std::ranges::any_of(
      *provided, [](const ProvidedArgument &arg) { return !arg.name.empty(); });

  if (hasNamedArguments) {
    std::set<std::string> seenNames;
    for (const auto &arg : *provided) {
      if (arg.name.empty())
        continue;
      if (!seenNames.insert(arg.name).second) {
        ScopedErrorLocation scoped(arg.nameLoc);
        reportCompilerError("Duplicate argument for parameter '" + arg.name +
                            "'");
        return nullptr;
      }
    }

    std::set<std::string> knownNames;
    for (auto *overload : overloadList) {
      for (const auto &name : overload->parameterNames) {
        if (!name.empty())
          knownNames.insert(name);
      }
    }
    for (const auto &arg : *provided) {
      if (arg.name.empty())
        continue;
      if (!knownNames.contains(arg.name)) {
        ScopedErrorLocation scoped(arg.nameLoc);
        reportCompilerError("Unknown parameter name '" + arg.name +
                            "' for call to '" + calleeBase + "'");
        return nullptr;
      }
    }
  }

  std::vector<CandidateCall> viable;
  viable.reserve(overloadList.size());
  std::optional<std::string> missingRequiredParam;
  bool sawTooManyArgs = false;

  for (auto *overload : overloadList) {
    if (!overload)
      continue;

    CandidateCall cand;
    cand.overload = overload;

    llvm::Function *CandidateFunc = overload->function;
    if (!CandidateFunc)
      CandidateFunc = TheModule->getFunction(overload->mangledName);
    if (!CandidateFunc)
      continue;
    overload->function = CandidateFunc;

    const size_t paramCount = overload->parameterTypes.size();
    std::vector<int> binding(paramCount, -1);
    std::vector<int> paramsBinding;
    const int paramsIndex = findParamsIndex(overload->parameterIsParams);
    size_t nextPositional = 0;
    bool failed = false;
    bool paramsNamed = false;

    for (size_t i = 0; i < provided->size(); ++i) {
      const auto &arg = (*provided)[i];
      if (arg.name.empty()) {
        while (nextPositional < paramCount && binding[nextPositional] != -1)
          ++nextPositional;
        if (paramsIndex >= 0 &&
            nextPositional == static_cast<size_t>(paramsIndex)) {
          if (paramsNamed) {
            failed = true;
            sawTooManyArgs = true;
            break;
          }
          for (size_t j = i; j < provided->size(); ++j) {
            if (!(*provided)[j].name.empty()) {
              failed = true;
              break;
            }
            paramsBinding.push_back(static_cast<int>(j));
          }
          nextPositional = paramCount;
          break;
        }
        if (nextPositional >= paramCount) {
          failed = true;
          sawTooManyArgs = true;
          break;
        }
        binding[nextPositional] = static_cast<int>(i);
        ++nextPositional;
        continue;
      }

      auto nameIt = std::find(overload->parameterNames.begin(),
                              overload->parameterNames.end(), arg.name);
      if (nameIt == overload->parameterNames.end()) {
        failed = true;
        break;
      }
      size_t paramIndex =
          static_cast<size_t>(nameIt - overload->parameterNames.begin());
      if (paramsIndex >= 0 &&
          paramIndex == static_cast<size_t>(paramsIndex)) {
        if (paramsNamed || !paramsBinding.empty()) {
          failed = true;
          break;
        }
        paramsNamed = true;
        paramsBinding.push_back(static_cast<int>(i));
        continue;
      }
      if (binding[paramIndex] != -1) {
        failed = true;
        break;
      }
      binding[paramIndex] = static_cast<int>(i);
    }

    if (failed)
      continue;

    for (size_t idx = 0; idx < paramCount; ++idx) {
      if (paramsIndex >= 0 &&
          idx == static_cast<size_t>(paramsIndex)) {
        continue;
      }
      if (binding[idx] != -1)
        continue;
      const bool hasDefault =
          idx < overload->parameterDefaults.size() &&
          overload->parameterDefaults[idx].isSet();
      if (!hasDefault) {
        failed = true;
        if (!missingRequiredParam && idx < overload->parameterNames.size())
          missingRequiredParam = overload->parameterNames[idx];
        break;
      }
    }

    if (failed)
      continue;

    bool compatible = true;
    unsigned conversions = 0;

    auto checkCompatibility = [&](const ExprAST *argExpr,
                                  llvm::Value *argValue,
                                  const TypeInfo &expectedInfo,
                                  llvm::Type *expectedType,
                                  unsigned &conversionCount) -> bool {
      if (!argValue) {
        const DelegateTypeInfo *delegateInfo = lookupDelegateInfo(expectedInfo);
        if (!delegateInfo)
          return false;
        return canBindDelegateReference(*delegateInfo, argExpr);
      }

      const ExprAST *coreArg = unwrapRefExpr(argExpr);
      if (coreArg && !coreArg->getTypeName().empty()) {
        TypeInfo actualInfo =
            applyActiveTypeBindings(makeTypeInfo(coreArg->getTypeName()));
        const bool expectedIsArray =
            expectedInfo.isArray || expectedInfo.arrayDepth > 0;
        const bool actualIsArray =
            actualInfo.isArray || actualInfo.arrayDepth > 0;
        if ((expectedIsArray || actualIsArray) &&
            !typeInfoEquals(expectedInfo, actualInfo)) {
          return false;
        }
        const bool expectedHasGenerics = !expectedInfo.typeArguments.empty();
        const bool actualHasGenerics = !actualInfo.typeArguments.empty();
        if ((expectedHasGenerics || actualHasGenerics) &&
            !typeInfoEquals(expectedInfo, actualInfo)) {
          return false;
        }
      }

      llvm::Type *ActualType = expectedType;
      if (argValue)
        ActualType = argValue->getType();

      if (ActualType == expectedType)
        return true;

      if (ActualType && expectedType && ActualType->isPointerTy() &&
          expectedType->isPointerTy()) {
        ++conversionCount;
        return true;
      }

      if (expectedType && isDecimalLLVMType(expectedType) && coreArg) {
        if (auto *num = dynamic_cast<const NumberExprAST *>(coreArg)) {
          if (!num->getLiteral().isDecimal()) {
            ++conversionCount;
            return true;
          }
        }
      }

      if (ActualType && expectedType &&
          areTypesCompatible(ActualType, expectedType)) {
        ++conversionCount;
        return true;
      }

      return false;
    };

    for (size_t idx = 0; idx < paramCount; ++idx) {
      if (paramsIndex >= 0 &&
          idx == static_cast<size_t>(paramsIndex))
        continue;

      if (binding[idx] < 0)
        continue;

      bool argIsRef = binding[idx] >= 0
                          ? (*provided)[static_cast<size_t>(binding[idx])].isRef
                          : overload->parameterIsRef[idx];
      if (overload->parameterIsRef[idx] != argIsRef) {
        compatible = false;
        break;
      }

      const ExprAST *argExpr = nullptr;
      llvm::Value *argValue = nullptr;
      if (binding[idx] >= 0) {
        const auto &boundArg = (*provided)[static_cast<size_t>(binding[idx])];
        argExpr = boundArg.expr;
        argValue = boundArg.value;
      }

      llvm::Type *expectedType =
          CandidateFunc->getFunctionType()->getParamType(idx);
      if (!checkCompatibility(argExpr, argValue,
                              overload->parameterTypes[idx], expectedType,
                              conversions)) {
        compatible = false;
        break;
      }
    }

    if (compatible && paramsIndex >= 0) {
      if (paramsIndex >= static_cast<int>(overload->parameterIsRef.size()) ||
          overload->parameterIsRef[static_cast<size_t>(paramsIndex)]) {
        compatible = false;
      } else if (!paramsBinding.empty()) {
        TypeInfo arrayInfo = overload->parameterTypes[static_cast<size_t>(paramsIndex)];
        llvm::Type *expectedArrayType =
            CandidateFunc->getFunctionType()->getParamType(
                static_cast<size_t>(paramsIndex));

        bool directAllowed = paramsBinding.size() == 1;
        if (directAllowed) {
          const auto &arg = (*provided)[static_cast<size_t>(paramsBinding.front())];
          if (arg.isRef)
            directAllowed = false;

          const ExprAST *coreArg = unwrapRefExpr(arg.expr);
          if (directAllowed && coreArg && !coreArg->getTypeName().empty()) {
            TypeInfo actualInfo =
                applyActiveTypeBindings(makeTypeInfo(coreArg->getTypeName()));
            if (!typeInfoEquals(arrayInfo, actualInfo))
              directAllowed = false;
          }

          llvm::Type *actualType =
              arg.value ? arg.value->getType() : nullptr;
          if (!actualType || actualType != expectedArrayType)
            directAllowed = false;
        }

        if (directAllowed) {
          cand.paramsDirect = true;
        } else {
          auto elementInfo = extractElementTypeInfo(arrayInfo);
          if (!elementInfo) {
            compatible = false;
          } else {
            std::string elementTypeName = typeNameFromInfo(*elementInfo);
            llvm::Type *expectedElementType =
                getTypeFromString(elementTypeName);
            if (!expectedElementType) {
              compatible = false;
            } else {
              for (int argIndex : paramsBinding) {
                const auto &arg = (*provided)[static_cast<size_t>(argIndex)];
                if (arg.isRef) {
                  compatible = false;
                  break;
                }
                if (!checkCompatibility(arg.expr, arg.value, *elementInfo,
                                        expectedElementType, conversions)) {
                  compatible = false;
                  break;
                }
              }
            }
          }
        }
      }
    }

    if (!compatible)
      continue;

    cand.binding = std::move(binding);
    cand.paramsBinding = std::move(paramsBinding);
    cand.conversions = conversions;
    cand.function = CandidateFunc;
    viable.push_back(std::move(cand));
  }

  if (viable.empty()) {
    if (missingRequiredParam) {
      reportCompilerError(
          "Missing argument for parameter '" + *missingRequiredParam +
          "' in call to '" + calleeBase + "'");
      return nullptr;
    }
    if (sawTooManyArgs) {
      reportCompilerError("Too many arguments provided to call '" +
                          calleeBase + "'");
      return nullptr;
    }
    return LogErrorV(
        ("No matching overload found for call to '" + calleeBase + "'")
            .c_str());
  }

  if (preferGeneric) {
    std::vector<CandidateCall> genericViable;
    genericViable.reserve(viable.size());
    for (auto &candidate : viable) {
      if (candidate.overload && candidate.overload->isGenericInstantiation)
        genericViable.push_back(std::move(candidate));
    }
    if (!genericViable.empty())
      viable = std::move(genericViable);
  }

  auto bestIt = std::min_element(
      viable.begin(), viable.end(),
      [](const CandidateCall &lhs, const CandidateCall &rhs) {
        return lhs.conversions < rhs.conversions;
      });

  unsigned bestConversions = bestIt->conversions;
  unsigned bestCount = static_cast<unsigned>(std::count_if(
      viable.begin(), viable.end(),
      [bestConversions](const CandidateCall &candidate) {
        return candidate.conversions == bestConversions;
      }));

  if (bestCount > 1)
    return LogErrorV(("Ambiguous call to '" + calleeBase + "'").c_str());

  CandidateCall *selected = &*bestIt;
  FunctionOverload *chosen = selected->overload;

  llvm::Function *CalleeF = selected->function;
  if (!CalleeF) {
    CalleeF = TheModule->getFunction(chosen->mangledName);
    if (!CalleeF)
      return LogErrorV(("Internal error: overload for '" + calleeBase +
                        "' lacks function definition").c_str());
    chosen->function = CalleeF;
  }

  std::vector<llvm::Value *> CallArgs;
  CallArgs.reserve(selected->binding.size());
  std::vector<bool> CallArgIsRef;
  CallArgIsRef.reserve(selected->binding.size());
  std::vector<std::unique_ptr<ExprAST>> ownedDefaultExprs;
  const int paramsIndex = findParamsIndex(chosen->parameterIsParams);

  auto ensureDelegateBindingValue = [&](const DelegateTypeInfo &delegateInfo,
                                        int bindingIndex,
                                        std::string_view usageLabel) -> bool {
    if (bindingIndex < 0 ||
        static_cast<size_t>(bindingIndex) >= provided->size())
      return true;
    ProvidedArgument &arg = (*provided)[static_cast<size_t>(bindingIndex)];
    if (arg.value)
      return true;
    if (!arg.expr)
      return false;
    bool handled = false;
    llvm::Value *value = emitDelegateValueForTarget(
        delegateInfo, const_cast<ExprAST *>(arg.expr), usageLabel, handled);
    if (!handled || !value)
      return false;
    arg.value = value;
    arg.isRef = false;
    return true;
  };

  if (paramsIndex >= 0 && !selected->paramsDirect &&
      paramsIndex < static_cast<int>(chosen->parameterTypes.size())) {
    TypeInfo paramsInfo = chosen->parameterTypes[static_cast<size_t>(paramsIndex)];
    finalizeTypeInfoMetadata(paramsInfo);
    if (auto elementInfoOpt = extractElementTypeInfo(paramsInfo)) {
      if (const DelegateTypeInfo *delegateInfo =
              lookupDelegateInfo(*elementInfoOpt)) {
        for (int bindingIndex : selected->paramsBinding) {
          if (!ensureDelegateBindingValue(*delegateInfo, bindingIndex,
                                          "params argument"))
            return nullptr;
        }
      }
    }
  }

  for (size_t idx = 0; idx < selected->binding.size(); ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex))
      continue;
    int bindingIndex = selected->binding[idx];
    if (bindingIndex < 0)
      continue;
    if (const DelegateTypeInfo *delegateInfo =
            lookupDelegateInfo(chosen->parameterTypes[idx])) {
      if (!ensureDelegateBindingValue(*delegateInfo, bindingIndex,
                                      "argument"))
        return nullptr;
    }
  }

  for (size_t idx = 0; idx < selected->binding.size(); ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex)) {
      if (selected->paramsDirect) {
        const auto &arg =
            (*provided)[static_cast<size_t>(selected->paramsBinding.front())];
        CallArgs.push_back(arg.value);
        CallArgIsRef.push_back(arg.isRef);
      } else {
        llvm::Value *packed =
            emitPackedParamsArray(selected->paramsBinding, *provided,
                                  chosen->parameterTypes[idx], calleeBase);
        if (!packed)
          return nullptr;
        CallArgs.push_back(packed);
        CallArgIsRef.push_back(false);
      }
      continue;
    }
    int bindingIndex = selected->binding[idx];
    if (bindingIndex >= 0) {
      const auto &arg = (*provided)[static_cast<size_t>(bindingIndex)];
      CallArgs.push_back(arg.value);
      CallArgIsRef.push_back(arg.isRef);
    } else {
      ScopedErrorLocation scoped(
          idx < chosen->parameterDefaultLocations.size()
              ? chosen->parameterDefaultLocations[idx]
              : SourceLocation{});
      std::unique_ptr<ExprAST> defaultExpr =
          instantiateDefaultExpr(idx < chosen->parameterDefaults.size()
                                     ? chosen->parameterDefaults[idx]
                                     : DefaultArgInfo{});
      if (!defaultExpr) {
        reportCompilerError("Default value unavailable for parameter '" +
                            (idx < chosen->parameterNames.size()
                                 ? chosen->parameterNames[idx]
                                 : std::to_string(idx)) +
                            "'");
        return nullptr;
      }
      std::string targetTypeName =
          typeNameFromInfo(chosen->parameterTypes[idx]);
      defaultExpr->setTypeName(targetTypeName);
      defaultExpr->markTemporary();
      llvm::Type *targetType =
          targetTypeName.empty() ? nullptr : getTypeFromString(targetTypeName);
      llvm::Value *value = nullptr;
      if (targetType && isDecimalLLVMType(targetType)) {
        if (auto *num = dynamic_cast<NumberExprAST *>(defaultExpr.get()))
          value = num->codegen_with_target(targetType);
      }
      if (!value)
        value = defaultExpr->codegen();
      if (!value)
        return nullptr;
      CallArgs.push_back(value);
      CallArgIsRef.push_back(false);
      ownedDefaultExprs.push_back(std::move(defaultExpr));
    }
  }

  std::vector<llvm::Value *> FinalArgs;
  FinalArgs.reserve(CallArgs.size());

  for (size_t idx = 0; idx < CallArgs.size(); ++idx) {
    llvm::Value *ArgVal = CallArgs[idx];
    llvm::Type *ExpectedType =
        CalleeF->getFunctionType()->getParamType(idx);
    const ExprAST *argExpr = nullptr;
    if (paramsIndex >= 0 && static_cast<int>(idx) == paramsIndex &&
        selected->paramsDirect && !selected->paramsBinding.empty()) {
      int bindingIndex = selected->paramsBinding.front();
      if (bindingIndex >= 0 &&
          static_cast<size_t>(bindingIndex) < provided->size())
        argExpr = (*provided)[static_cast<size_t>(bindingIndex)].expr;
    } else if (idx < selected->binding.size()) {
      int bindingIndex = selected->binding[idx];
      if (bindingIndex >= 0 &&
          static_cast<size_t>(bindingIndex) < provided->size())
        argExpr = (*provided)[static_cast<size_t>(bindingIndex)].expr;
    }

    if (chosen->parameterIsRef[idx]) {
      if (ExpectedType && ExpectedType->isPointerTy() &&
          !ArgVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = Builder->CreateAlloca(
            ArgVal->getType(), nullptr,
            buildArcOpLabel(calleeBase, "ref.arg"));
        Builder->CreateStore(ArgVal, tmp);
        ArgVal = tmp;
      }
      if (ExpectedType && ExpectedType->isPointerTy() &&
          ArgVal->getType() != ExpectedType) {
        ArgVal = Builder->CreateBitCast(
            ArgVal, ExpectedType,
            buildArcOpLabel(calleeBase, "ref.cast"));
      }
    } else {
      const std::string targetTypeName =
          typeNameFromInfo(chosen->parameterTypes[idx]);
      if (ExpectedType && argExpr && isDecimalLLVMType(ExpectedType)) {
        ExprAST *mutableExpr = const_cast<ExprAST *>(argExpr);
        if (auto *num = dynamic_cast<NumberExprAST *>(mutableExpr)) {
          if (llvm::Value *targeted = num->codegen_with_target(ExpectedType))
            ArgVal = targeted;
        }
      }
      ArgVal = castToType(ArgVal, ExpectedType, targetTypeName);
    }
    FinalArgs.push_back(ArgVal);
  }

  llvm::Value *CallValue = nullptr;
  if (CalleeF->getReturnType()->isVoidTy()) {
    CallValue = Builder->CreateCall(CalleeF, FinalArgs);
    if (typeOwner)
      typeOwner->setTypeName("void");
  } else {
    CallValue = Builder->CreateCall(CalleeF, FinalArgs, "calltmp");
    if (typeOwner)
      typeOwner->setTypeInfo(chosen->returnType);

    if (chosen->returnsByRef && !preserveRefReturn) {
      llvm::Type *valueType = getTypeFromString(typeNameFromInfo(chosen->returnType));
      if (!valueType)
        return LogErrorV("Unable to determine ref return type for call");
      CallValue = Builder->CreateLoad(valueType, CallValue, "refcalltmp");
    }
  }

  return CallValue;
}

llvm::Value *CallExprAST::emitResolvedCall(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef, bool preferGeneric,
    FunctionOverload *forced) {
  std::vector<ProvidedArgument> provided;
  provided.reserve(ArgValues.size());
  for (size_t i = 0; i < ArgValues.size(); ++i) {
    ProvidedArgument arg;
    arg.value = ArgValues[i];
    arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
    if (i < Args.size())
      arg.expr = Args[i].get();
    if (i < ArgNames.size())
      arg.name = ArgNames[i];
    if (i < ArgNameLocations.size())
      arg.nameLoc = ArgNameLocations[i];
    if (i < ArgEqualsLocations.size())
      arg.equalsLoc = ArgEqualsLocations[i];
    provided.push_back(std::move(arg));
  }

  return emitResolvedCallInternal(calleeBase, std::move(ArgValues), ArgIsRef,
                                  &Args, preferGeneric, forced, this,
                                  &provided);
}

llvm::Value *CallExprAST::codegenMemberCall(MemberAccessExprAST &member) {
  llvm::Value *instancePtr = nullptr;
  llvm::Value *instanceValue = nullptr;
  bool objectEvaluated = false;
  bool isTypeReference = false;

  std::string ownerName =
      sanitizeCompositeLookupName(member.getObject()->getTypeName());

  if (auto *varObj = dynamic_cast<VariableExprAST *>(member.getObject())) {
    if (lookupCompositeInfo(varObj->getName())) {
      ownerName = varObj->getName();
      isTypeReference = true;
    }
  }

  if (ownerName.empty()) {
    if (auto infoOpt = resolveExprTypeInfo(member.getObject())) {
      ownerName = sanitizeCompositeLookupName(typeNameFromInfo(*infoOpt));
    }
  }

  if (ownerName.empty() && !isTypeReference) {
    instanceValue = member.getObject()->codegen();
    if (!instanceValue)
      return nullptr;
    objectEvaluated = true;
    if (instanceValue->getType()->isPointerTy())
      instancePtr = instanceValue;
    ownerName = resolveCompositeName(member.getObject());
  }

  if (ownerName.empty())
    return LogErrorV("Unable to determine composite type for member call");


  const CompositeTypeInfo *info = lookupCompositeInfo(ownerName);
  if (!info)
    return LogErrorV(("Type '" + ownerName +
                      "' has no metadata for member calls")
                         .c_str());

  const bool isDtorCall =
      member.isDestructorAccess() || isDestructorCall();

  auto ensureInstancePointer = [&]() -> llvm::Value * {
    if (instancePtr)
      return instancePtr;
    if (!instanceValue) {
      instanceValue = member.getObject()->codegen();
      if (!instanceValue)
        return nullptr;
      objectEvaluated = true;
    }
    if (instanceValue->getType()->isPointerTy()) {
      instancePtr = instanceValue;
    } else {
      llvm::AllocaInst *Tmp =
          Builder->CreateAlloca(instanceValue->getType(), nullptr,
                                "method.recv");
      Builder->CreateStore(instanceValue, Tmp);
      instancePtr = Tmp;
    }
    return instancePtr;
  };

  if (isDtorCall && info->smartPointerKind != SmartPointerKind::None) {
    reportCompilerError(
        "Manual destructor calls are not allowed on smart pointers",
        "Control blocks manage the payload; calling '~" +
            member.getMemberName() +
            "()' manually can double-drop or corrupt the managed value.");
    return nullptr;
  }

  if (isDtorCall) {
    TypeInfo ownerInfoFull = makeTypeInfo(member.getObject()->getTypeName());
    ownerInfoFull = applyActiveTypeBindings(ownerInfoFull);
    finalizeTypeInfoMetadata(ownerInfoFull);
    if (ownerInfoFull.ownership != OwnershipQualifier::Strong) {
      reportCompilerError(
          "Manual destructor calls require strong-owned targets",
          "Convert weak or unowned references to strong ones before invoking a destructor.");
      return nullptr;
    }
    if (!info->hasDestructor || info->destructorFunctionName.empty()) {
      reportCompilerError("Type '" + ownerName +
                          "' does not declare a destructor");
      return nullptr;
    }
    if (!getArgs().empty()) {
      reportCompilerError("Destructor calls cannot take arguments");
      return nullptr;
    }
    std::string targetName =
        sanitizeCompositeLookupName(member.getMemberName());
    if (!targetName.empty() && targetName != ownerName) {
      reportCompilerError("Destructor '~" + targetName +
                          "()' does not match receiver of type '" + ownerName +
                          "'");
      return nullptr;
    }
    if (!ensureMemberAccessAllowed(info->destructorModifiers,
                                   AccessIntent::Call, ownerName,
                                   "~" + ownerName))
      return nullptr;

    llvm::Value *thisPtr = ensureInstancePointer();
    if (!thisPtr)
      return nullptr;

    llvm::Function *dtorFn =
        TheModule->getFunction(info->destructorFunctionName);
    if (!dtorFn) {
      reportCompilerError(
          "Internal error: destructor '" + info->destructorFunctionName +
          "' missing during call emission");
      return nullptr;
    }

    llvm::Value *dispatchTarget = nullptr;
    if (!info->baseClass &&
        info->kind == AggregateKind::Class &&
        info->destructorVtableSlot !=
            std::numeric_limits<unsigned>::max()) {
      auto structIt = StructTypes.find(ownerName);
      if (structIt != StructTypes.end() && info->hasARCHeader) {
        llvm::StructType *structTy = structIt->second;
        llvm::StructType *headerTy = getArcHeaderType();
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            structTy, thisPtr, info->headerFieldIndex, "dtor.header");
        llvm::Value *descAddr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, "dtor.desc.addr");
        llvm::Value *descriptor = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), descAddr, "dtor.desc");
        llvm::Value *vtableAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), descriptor, 2, "dtor.vtable.addr");
        llvm::Value *vtablePtr =
            Builder->CreateLoad(pointerType(), vtableAddr, "dtor.vtable");
        llvm::Value *slotIndex = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext),
            static_cast<uint64_t>(info->destructorVtableSlot));
        llvm::Value *slotPtr = Builder->CreateInBoundsGEP(
            pointerType(), vtablePtr, slotIndex, "dtor.vslot");
        llvm::Value *slotFnPtr =
            Builder->CreateLoad(pointerType(), slotPtr, "dtor.fptr");
        llvm::Type *fnPtrTy = pointerType(dtorFn->getFunctionType());
        dispatchTarget =
            Builder->CreateBitCast(slotFnPtr, fnPtrTy, "dtor.target");
      }
    }

    llvm::Value *callArg = thisPtr;
    if (!dtorFn->arg_empty()) {
      llvm::Type *expected = dtorFn->getFunctionType()->getParamType(0);
      if (callArg->getType() != expected) {
        callArg = Builder->CreateBitCast(
            thisPtr, expected, "dtor.manual.this");
      }
    }

    TypeInfo ownerInfo = ownerInfoFull;
    llvm::Value *retained =
        emitArcRetain(callArg, ownerInfo,
                      ownerName + ".dtor.manual.retain");
    llvm::Value *callValue = nullptr;
    if (dispatchTarget) {
      if (dtorFn->arg_empty()) {
        callValue = Builder->CreateCall(dtorFn->getFunctionType(),
                                        dispatchTarget, {});
      } else {
        callValue = Builder->CreateCall(dtorFn->getFunctionType(),
                                        dispatchTarget, {retained});
      }
    } else {
      callValue = dtorFn->arg_empty()
                      ? Builder->CreateCall(dtorFn)
                      : Builder->CreateCall(dtorFn, {retained});
    }
    const CompositeTypeInfo *baseCursor = info;
    llvm::Value *basePtr = thisPtr;
    while (baseCursor && baseCursor->baseClass) {
      auto baseStructIt = StructTypes.find(*baseCursor->baseClass);
      if (baseStructIt != StructTypes.end() &&
          basePtr->getType() != pointerType(baseStructIt->second)) {
        basePtr = Builder->CreateBitCast(
            basePtr, pointerType(baseStructIt->second),
            ownerName + ".dtor.base.ptr");
      }
      const CompositeTypeInfo *baseInfo =
          lookupCompositeInfo(*baseCursor->baseClass);
      if (baseInfo && baseInfo->hasDestructor &&
          !baseInfo->destructorFunctionName.empty()) {
        llvm::Function *baseDtor =
            TheModule->getFunction(baseInfo->destructorFunctionName);
        if (!baseDtor) {
          reportCompilerError("Internal error: base destructor '" +
                              baseInfo->destructorFunctionName +
                              "' missing during manual call");
          break;
        }
        llvm::Value *baseThis = basePtr;
        if (!baseDtor->arg_empty()) {
          llvm::Type *expected =
              baseDtor->getFunctionType()->getParamType(0);
          if (baseThis->getType() != expected)
            baseThis = Builder->CreateBitCast(
                basePtr, expected, ownerName + ".dtor.base.this");
          Builder->CreateCall(baseDtor, {baseThis});
        } else {
          Builder->CreateCall(baseDtor, {});
        }
      }
      baseCursor = baseInfo;
    }
    if (info->kind == AggregateKind::Class &&
        info->destructorVtableSlot !=
            std::numeric_limits<unsigned>::max()) {
      auto structIt = StructTypes.find(ownerName);
      if (structIt != StructTypes.end()) {
        llvm::StructType *structTy = structIt->second;
        llvm::StructType *headerTy = getArcHeaderType();
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            structTy, thisPtr, info->headerFieldIndex,
            ownerName + ".dtor.base.header");
        llvm::Value *descAddr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, ownerName + ".dtor.base.desc.addr");
        llvm::Value *dynamicDesc = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), descAddr,
            ownerName + ".dtor.base.desc");
        llvm::Value *baseDescAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), dynamicDesc, 1,
            ownerName + ".dtor.parent.addr");
        llvm::Value *baseDescriptor = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), baseDescAddr,
            ownerName + ".dtor.parent");
        llvm::Value *hasBase = Builder->CreateICmpNE(
            baseDescriptor,
            llvm::ConstantPointerNull::get(
                pointerType(getTypeDescriptorType())),
            ownerName + ".dtor.has.parent");
        llvm::Function *parentFn = Builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *baseCallBB = llvm::BasicBlock::Create(
            *TheContext, ownerName + ".dtor.base.call", parentFn);
        llvm::BasicBlock *baseSkipBB = llvm::BasicBlock::Create(
            *TheContext, ownerName + ".dtor.base.skip", parentFn);
        Builder->CreateCondBr(hasBase, baseCallBB, baseSkipBB);
        Builder->SetInsertPoint(baseCallBB);
        llvm::Value *baseVtableAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), baseDescriptor, 2,
            ownerName + ".dtor.base.vtable.addr");
        llvm::Value *baseVtable =
            Builder->CreateLoad(pointerType(), baseVtableAddr,
                                ownerName + ".dtor.base.vtable");
        llvm::Value *slotIndex = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext),
            static_cast<uint64_t>(info->destructorVtableSlot));
        llvm::Value *baseSlotPtr = Builder->CreateInBoundsGEP(
            pointerType(), baseVtable, slotIndex,
            ownerName + ".dtor.base.vslot");
        llvm::Value *baseSlotFn =
            Builder->CreateLoad(pointerType(), baseSlotPtr,
                                ownerName + ".dtor.base.fn");
        llvm::Value *baseTypedFn = Builder->CreateBitCast(
            baseSlotFn, pointerType(dtorFn->getFunctionType()),
            ownerName + ".dtor.base.target");
        llvm::Value *baseThis = thisPtr;
        if (!dtorFn->arg_empty()) {
          llvm::Type *expected =
              dtorFn->getFunctionType()->getParamType(0);
          if (baseThis->getType() != expected)
            baseThis = Builder->CreateBitCast(
                thisPtr, expected, ownerName + ".dtor.base.this");
          Builder->CreateCall(dtorFn->getFunctionType(), baseTypedFn,
                              {baseThis});
        } else {
          Builder->CreateCall(dtorFn->getFunctionType(), baseTypedFn, {});
        }
        Builder->CreateBr(baseSkipBB);
        Builder->SetInsertPoint(baseSkipBB);
      }
    }
    emitArcRelease(retained, ownerInfo,
                   ownerName + ".dtor.manual.release");
    if (auto *allocaPtr = llvm::dyn_cast<llvm::AllocaInst>(thisPtr))
      markArcSlotDestroyed(allocaPtr);
    setTypeName("void");
    return callValue;
  }

  auto methodIt = info->methodInfo.find(member.getMemberName());
  if (methodIt == info->methodInfo.end()) {
    llvm::Value *delegateValue = nullptr;
    const DelegateTypeInfo *delegateInfo = nullptr;

    if (objectEvaluated) {
      std::string objectTypeName = member.getObject()->getTypeName();
      if (objectTypeName.empty()) {
        if (auto infoOpt = resolveExprTypeInfo(member.getObject()))
          objectTypeName = typeNameFromInfo(*infoOpt);
      }
      if (objectTypeName.empty())
        objectTypeName = ownerName;

      bool objectTemporary = member.getObject()->isTemporary();
      auto injectedObject = std::make_unique<InjectedValueExprAST>(
          instanceValue, objectTypeName, objectTemporary);
      MemberAccessExprAST injectedMember(
          std::move(injectedObject), member.getMemberName(),
          member.getGenericArguments(), member.isDestructorAccess(),
          member.isSafeSmartArrow());
      delegateValue = injectedMember.codegen();
      if (!delegateValue)
        return nullptr;
      delegateInfo = resolveDelegateInfoForExpr(&injectedMember);
      if (!delegateInfo) {
        return LogErrorV(("Member '" + member.getMemberName() + "' of type '" +
                          ownerName + "' is not a method")
                             .c_str());
      }
      return emitDelegateCall(*this, delegateValue, *delegateInfo,
                              &injectedMember);
    }

    delegateValue = member.codegen();
    if (!delegateValue)
      return nullptr;
    delegateInfo = resolveDelegateInfoForExpr(&member);
    if (!delegateInfo) {
      return LogErrorV(("Member '" + member.getMemberName() + "' of type '" +
                        ownerName + "' is not a method")
                           .c_str());
    }
    return emitDelegateCall(*this, delegateValue, *delegateInfo, &member);
  }

  if (!ensureMemberAccessAllowed(methodIt->second.modifiers,
                                 AccessIntent::Call, ownerName,
                                 member.getMemberName()))
    return nullptr;

  bool isStaticMethod =
      static_cast<uint8_t>(methodIt->second.modifiers.storage &
                           StorageFlag::Static) != 0;

  std::vector<bool> ArgIsRef;
  std::vector<llvm::Value *> ArgValues;

  if (!isStaticMethod) {
    if (!instancePtr) {
      instancePtr = ensureInstancePointer();
      if (!instancePtr)
        return nullptr;
    }

    ArgIsRef.push_back(true);
    ArgValues.push_back(instancePtr);
  }

  for (const auto &ArgExpr : getArgs()) {
    const ExprAST *coreArg = unwrapRefExpr(ArgExpr.get());
    if (auto *var = dynamic_cast<const VariableExprAST *>(coreArg)) {
      if (isDelegateFunctionReference(*var)) {
        ArgIsRef.push_back(false);
        ArgValues.push_back(nullptr);
        continue;
      }
    } else if (auto *memberRef =
                   dynamic_cast<const MemberAccessExprAST *>(coreArg)) {
      if (isDelegateMethodReference(*memberRef)) {
        ArgIsRef.push_back(false);
        ArgValues.push_back(nullptr);
        continue;
      }
    }

    bool isRef = dynamic_cast<RefExprAST *>(ArgExpr.get()) != nullptr;
    ArgIsRef.push_back(isRef);
    llvm::Value *Value = ArgExpr->codegen();
    if (!Value)
      return nullptr;
    ArgValues.push_back(Value);
  }

  const CompositeMemberInfo &memberInfo = methodIt->second;
  std::vector<TypeInfo> methodTypeArgs;
  if (member.hasExplicitGenerics()) {
    std::string decoratedMemberName =
        member.getMemberName() + member.getGenericArguments();
    std::string parsedBase;
    if (!parseExplicitTypeArgumentSuffix(decoratedMemberName, parsedBase,
                                         methodTypeArgs))
      return nullptr;
    if (parsedBase != member.getMemberName()) {
      reportCompilerError("Internal error: mismatched member name while parsing generic arguments");
      return nullptr;
    }
  }

  if (!methodTypeArgs.empty() && !memberInfo.isGenericTemplate) {
    reportCompilerError("Method '" + memberInfo.signature +
                        "' does not accept explicit type arguments");
    return nullptr;
  }

  if (memberInfo.isGenericTemplate) {
    if (methodTypeArgs.empty()) {
      reportCompilerError("Method '" + memberInfo.signature +
                          "' requires explicit type arguments");
      return nullptr;
    }

    const auto *templates =
        lookupGenericFunctionTemplates(memberInfo.signature);
    if (!templates || templates->empty()) {
      reportCompilerError("Internal error: generic method template '" +
                          memberInfo.signature + "' is not registered");
      return nullptr;
    }

    const auto arities = collectGenericArities(*templates);
    if (!std::ranges::any_of(
            arities, [&](std::size_t arity) { return arity == methodTypeArgs.size(); })) {
      reportCompilerError("Method '" + memberInfo.signature + "' expects " +
                              formatArityList(arities) + " type argument(s)",
                          "Provide " + formatArityList(arities) +
                              " type argument(s) for this generic method.");
      return nullptr;
    }

    const std::map<std::string, TypeInfo> *additionalBindings =
        &info->typeArgumentBindings;
    llvm::Function *instantiated =
        InstantiateGenericFunction(memberInfo.signature, methodTypeArgs,
                                   additionalBindings);
    if (!instantiated)
      return nullptr;
    if (auto infoRefIt = CG.compositeMetadata.find(ownerName);
        infoRefIt != CG.compositeMetadata.end()) {
      auto &records =
          infoRefIt->second.genericMethodInstantiations[memberInfo.signature];
      std::string mangled(instantiated->getName());
      if (std::find(records.begin(), records.end(), mangled) == records.end())
        records.push_back(std::move(mangled));
    }
  }

  if (!isStaticMethod && info->kind == AggregateKind::Interface) {
    if (!instancePtr)
      return LogErrorV("Interface member call requires an instance");

    auto slotIt =
        info->interfaceMethodSlotMap.find(memberInfo.dispatchKey);
    if (slotIt == info->interfaceMethodSlotMap.end())
      return LogErrorV(("Internal error: interface slot unresolved for '" +
                        memberInfo.dispatchKey + "'")
                           .c_str());

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateBitCast(
        instancePtr, pointerType(headerTy), "hybrid.header.iface");
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    llvm::GlobalVariable *ifaceDescriptorGV =
        TheModule->getGlobalVariable(info->descriptorGlobalName, true);
    if (!ifaceDescriptorGV)
      return LogErrorV(("Internal error: interface descriptor '" +
                        info->descriptorGlobalName +
                        "' missing during dispatch")
                           .c_str());
    llvm::Value *ifaceDescriptorConst =
        llvm::ConstantExpr::getBitCast(ifaceDescriptorGV, typeDescPtrTy);

    llvm::Function *lookupFn = getInterfaceLookupFunction();
    llvm::Value *methodTablePtr =
        Builder->CreateCall(lookupFn,
                            {descriptorValue, ifaceDescriptorConst},
                            "hybrid.iface.table");

    llvm::Value *isNull = Builder->CreateICmpEQ(
        methodTablePtr, llvm::ConstantPointerNull::get(voidPtrPtrTy),
        "hybrid.iface.table.null");

    llvm::Function *parentFunc = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *failBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.fail", parentFunc);
    llvm::BasicBlock *contBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.cont", parentFunc);
    Builder->CreateCondBr(isNull, failBB, contBB);

    Builder->SetInsertPoint(failBB);
    llvm::Function *abortFn = TheModule->getFunction("abort");
    if (!abortFn) {
      llvm::FunctionType *abortTy =
          llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      abortFn = llvm::Function::Create(abortTy,
                                       llvm::Function::ExternalLinkage, "abort",
                                       TheModule.get());
    }
    Builder->CreateCall(abortFn);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(contBB);

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), slotIt->second);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, methodTablePtr, slotIndex, "hybrid.iface.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.iface.fn");

    return emitDynamicFunctionCall(*this, memberInfo, fnPtrRaw, ArgValues,
                                   ArgIsRef);
  }

  bool isBaseQualifier =
      dynamic_cast<BaseExprAST *>(member.getObject()) != nullptr;
  bool canVirtual = !isStaticMethod &&
                    info->kind == AggregateKind::Class &&
                    memberInfo.vtableSlot !=
                        std::numeric_limits<unsigned>::max() &&
                    !isBaseQualifier;

  if (canVirtual) {
    auto structIt = StructTypes.find(ownerName);
    if (structIt == StructTypes.end())
      return LogErrorV(("Internal error: struct type for '" + ownerName +
                        "' unavailable during dispatch")
                           .c_str());

    llvm::StructType *ownerStructTy = structIt->second;
    llvm::Value *typedInstancePtr = instancePtr;
    if (typedInstancePtr->getType() != pointerType(ownerStructTy)) {
      typedInstancePtr = Builder->CreateBitCast(
          typedInstancePtr, pointerType(ownerStructTy),
          "hybrid.method.recv");
    }

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateStructGEP(
        ownerStructTy, typedInstancePtr, 0, "hybrid.header.ptr");
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    llvm::Value *vtableAddr = Builder->CreateStructGEP(
        getTypeDescriptorType(), descriptorValue, 2, "hybrid.vtable.ptr");
    llvm::Value *vtablePtr =
        Builder->CreateLoad(voidPtrPtrTy, vtableAddr, "hybrid.vtable");

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), memberInfo.vtableSlot);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, vtablePtr, slotIndex, "hybrid.vtable.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.vtable.fn");

    return emitDynamicFunctionCall(*this, memberInfo, fnPtrRaw, ArgValues,
                                   ArgIsRef);
  }

  FunctionOverload *forcedOverload = nullptr;
  const bool hasParamsParam = std::ranges::any_of(
      memberInfo.parameterIsParams,
      [](bool isParams) { return isParams; });
  const bool hasDeferredArg =
      std::ranges::any_of(ArgValues,
                          [](llvm::Value *argValue) { return argValue == nullptr; });
  if (memberInfo.directFunction && !hasDeferredArg &&
      ArgValues.size() == memberInfo.parameterTypes.size() &&
      !hasParamsParam) {
    llvm::Function *DirectFunc = memberInfo.directFunction;
    for (size_t idx = 0; idx < ArgValues.size(); ++idx) {
      llvm::Value *argVal = ArgValues[idx];
      llvm::Type *ExpectedType =
          DirectFunc->getFunctionType()->getParamType(idx);
      if (idx < memberInfo.parameterIsRef.size() &&
          memberInfo.parameterIsRef[idx]) {
        if (ExpectedType && ExpectedType->isPointerTy() &&
            !argVal->getType()->isPointerTy()) {
          llvm::AllocaInst *tmp = Builder->CreateAlloca(
              argVal->getType(), nullptr,
              buildArcOpLabel(memberInfo.signature, "ref.arg"));
          Builder->CreateStore(argVal, tmp);
          argVal = tmp;
        }
        if (ExpectedType && ExpectedType->isPointerTy() &&
            argVal->getType() != ExpectedType) {
          argVal = Builder->CreateBitCast(
              argVal, ExpectedType,
              buildArcOpLabel(memberInfo.signature, "ref.cast"));
        }
      } else {
        const std::string targetTypeName =
            idx < memberInfo.parameterTypes.size()
                ? typeNameFromInfo(memberInfo.parameterTypes[idx])
                : "";
        argVal = castToType(argVal, ExpectedType, targetTypeName);
      }
      ArgValues[idx] = argVal;
    }
    if (DirectFunc->getReturnType()->isVoidTy()) {
      llvm::Value *CallValue = Builder->CreateCall(DirectFunc, ArgValues);
      setTypeName("void");
      return CallValue;
    }
    llvm::Value *CallValue =
        Builder->CreateCall(DirectFunc, ArgValues, "calltmp");
    setTypeName(typeNameFromInfo(memberInfo.returnType));
    return CallValue;
  }
  if (auto overloadIt = CG.functionOverloads.find(memberInfo.signature);
      overloadIt != CG.functionOverloads.end()) {
    for (auto &candidate : overloadIt->second) {
      if (candidate.parameterTypes.size() !=
          memberInfo.parameterTypes.size())
        continue;
      if (candidate.returnsByRef != memberInfo.returnsByRef)
        continue;
      if (!typeInfoEquals(candidate.returnType, memberInfo.returnType))
        continue;
      bool match = true;
      for (size_t idx = 0; idx < candidate.parameterTypes.size(); ++idx) {
        if (candidate.parameterIsRef[idx] != memberInfo.parameterIsRef[idx] ||
            !typeInfoEquals(candidate.parameterTypes[idx],
                            memberInfo.parameterTypes[idx])) {
          match = false;
          break;
        }
      }
      if (match) {
        forcedOverload = &candidate;
        break;
      }
    }
  }

  std::vector<ProvidedArgument> provided;
  provided.reserve(ArgValues.size());
  const size_t receiverOffset =
      ArgValues.size() > Args.size() ? ArgValues.size() - Args.size() : 0;
  for (size_t i = 0; i < ArgValues.size(); ++i) {
    ProvidedArgument arg;
    arg.value = ArgValues[i];
    arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
    if (i >= receiverOffset) {
      size_t userIndex = i - receiverOffset;
      if (userIndex < Args.size())
        arg.expr = Args[userIndex].get();
      if (userIndex < ArgNames.size())
        arg.name = ArgNames[userIndex];
      if (userIndex < ArgNameLocations.size())
        arg.nameLoc = ArgNameLocations[userIndex];
      if (userIndex < ArgEqualsLocations.size())
        arg.equalsLoc = ArgEqualsLocations[userIndex];
    }
    provided.push_back(std::move(arg));
  }

  return emitResolvedCallInternal(
      memberInfo.signature, std::move(ArgValues), ArgIsRef, &Args,
      memberInfo.isGenericTemplate && member.hasExplicitGenerics(),
      forcedOverload, this, &provided);
}

static llvm::Value *emitDynamicCallRaw(const CompositeMemberInfo &memberInfo,
                                       llvm::Value *fnPtrRaw,
                                       std::vector<llvm::Value *> argValues,
                                       const std::vector<bool> &argIsRef,
                                       ExprAST *typeOwner,
                                       bool preserveRefReturn) {
  const size_t paramCount = memberInfo.parameterTypes.size();
  std::vector<llvm::Type *> paramLLVMTypes;
  paramLLVMTypes.reserve(paramCount);
  for (size_t idx = 0; idx < paramCount; ++idx) {
    std::string paramTypeName = typeNameFromInfo(memberInfo.parameterTypes[idx]);
    llvm::Type *paramType = getTypeFromString(paramTypeName);
    if (!paramType)
      return LogErrorV(("Internal error: unable to resolve parameter type '" +
                        paramTypeName + "' for dynamic call")
                           .c_str());
    if (memberInfo.parameterIsRef[idx])
      paramType = llvm::PointerType::get(*TheContext, 0);
    paramLLVMTypes.push_back(paramType);
  }

  std::string returnTypeName = typeNameFromInfo(memberInfo.returnType);
  llvm::Type *retType = getTypeFromString(returnTypeName);
  if (!retType)
    return LogErrorV(("Internal error: unable to resolve return type '" +
                      returnTypeName + "' for dynamic call")
                         .c_str());
  if (memberInfo.returnsByRef)
    retType = llvm::PointerType::get(*TheContext, 0);

  llvm::FunctionType *fnType =
      llvm::FunctionType::get(retType, paramLLVMTypes, false);
  llvm::Value *fnPtr = Builder->CreateBitCast(
      fnPtrRaw, pointerType(fnType), "member.dyncall.cast");

  for (size_t idx = 0; idx < argValues.size(); ++idx) {
    llvm::Value *argVal = argValues[idx];
    llvm::Type *expectedType = fnType->getParamType(idx);
    if (idx < memberInfo.parameterIsRef.size() &&
        memberInfo.parameterIsRef[idx]) {
      if (expectedType && expectedType->isPointerTy() &&
          !argVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = Builder->CreateAlloca(
            argVal->getType(), nullptr,
            buildArcOpLabel(memberInfo.signature, "ref.arg"));
        Builder->CreateStore(argVal, tmp);
        argVal = tmp;
      }
      if (expectedType && expectedType->isPointerTy() &&
          argVal->getType() != expectedType) {
        argVal = Builder->CreateBitCast(
            argVal, expectedType,
            buildArcOpLabel(memberInfo.signature, "ref.cast"));
      }
    } else {
      std::string targetTypeName =
          idx < memberInfo.parameterTypes.size()
              ? typeNameFromInfo(memberInfo.parameterTypes[idx])
              : "";
      argVal = castToType(argVal, expectedType, targetTypeName);
    }
    argValues[idx] = argVal;
  }

  llvm::Value *callValue = nullptr;
  if (fnType->getReturnType()->isVoidTy()) {
    callValue = Builder->CreateCall(fnType, fnPtr, argValues);
    if (typeOwner)
      typeOwner->setTypeName("void");
  } else {
    callValue = Builder->CreateCall(fnType, fnPtr, argValues, "member.dyncall");
    if (typeOwner)
      typeOwner->setTypeInfo(memberInfo.returnType);
    if (memberInfo.returnsByRef && !preserveRefReturn) {
      llvm::Type *valueType = getTypeFromString(typeNameFromInfo(memberInfo.returnType));
      if (!valueType)
        return LogErrorV("Unable to determine ref return type for dynamic call");
      callValue = Builder->CreateLoad(valueType, callValue, "member.refload");
    }
  }

  return callValue;
}

llvm::Value *emitMemberCallByInfo(const CompositeTypeInfo &info,
                                  const CompositeMemberInfo &memberInfo,
                                  const std::string &ownerName,
                                  ExprAST *objectExpr,
                                  llvm::Value *instanceValue,
                                  std::vector<llvm::Value *> argValues,
                                  std::vector<bool> argIsRef,
                                  ExprAST *typeOwner,
                                  bool preserveRefReturn) {
  bool isStaticMethod =
      static_cast<uint8_t>(memberInfo.modifiers.storage & StorageFlag::Static) != 0;

  llvm::Value *instancePtr = nullptr;
  llvm::Value *instanceValueLocal = instanceValue;

  auto ensureInstancePointer = [&]() -> llvm::Value * {
    if (instancePtr)
      return instancePtr;
    if (instanceValueLocal) {
      if (instanceValueLocal->getType()->isPointerTy()) {
        instancePtr = instanceValueLocal;
      } else {
        llvm::AllocaInst *Tmp =
            Builder->CreateAlloca(instanceValueLocal->getType(), nullptr,
                                  "method.recv");
        Builder->CreateStore(instanceValueLocal, Tmp);
        instancePtr = Tmp;
      }
      return instancePtr;
    }
    if (!objectExpr)
      return nullptr;
    instancePtr = objectExpr->codegen_ptr();
    if (!instancePtr) {
      instanceValueLocal = objectExpr->codegen();
      if (!instanceValueLocal)
        return nullptr;
      if (instanceValueLocal->getType()->isPointerTy()) {
        instancePtr = instanceValueLocal;
      } else {
        llvm::AllocaInst *Tmp =
            Builder->CreateAlloca(instanceValueLocal->getType(), nullptr,
                                  "method.recv");
        Builder->CreateStore(instanceValueLocal, Tmp);
        instancePtr = Tmp;
      }
    }
    return instancePtr;
  };

  if (!isStaticMethod) {
    instancePtr = ensureInstancePointer();
    if (!instancePtr)
      return nullptr;
    argIsRef.insert(argIsRef.begin(), true);
    argValues.insert(argValues.begin(), instancePtr);
  }

  if (!isStaticMethod && info.kind == AggregateKind::Interface) {
    auto slotIt =
        info.interfaceMethodSlotMap.find(memberInfo.dispatchKey);
    if (slotIt == info.interfaceMethodSlotMap.end())
      return LogErrorV(("Internal error: interface slot unresolved for '" +
                        memberInfo.dispatchKey + "'")
                           .c_str());

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateBitCast(
        instancePtr, pointerType(headerTy), "hybrid.header.iface");
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    llvm::GlobalVariable *ifaceDescriptorGV =
        TheModule->getGlobalVariable(info.descriptorGlobalName, true);
    if (!ifaceDescriptorGV)
      return LogErrorV(("Internal error: interface descriptor '" +
                        info.descriptorGlobalName +
                        "' missing during dispatch")
                           .c_str());
    llvm::Value *ifaceDescriptorConst =
        llvm::ConstantExpr::getBitCast(ifaceDescriptorGV, typeDescPtrTy);

    llvm::Function *lookupFn = getInterfaceLookupFunction();
    llvm::Value *methodTablePtr =
        Builder->CreateCall(lookupFn,
                            {descriptorValue, ifaceDescriptorConst},
                            "hybrid.iface.table");

    llvm::Value *isNull = Builder->CreateICmpEQ(
        methodTablePtr, llvm::ConstantPointerNull::get(voidPtrPtrTy),
        "hybrid.iface.table.null");

    llvm::Function *parentFunc = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *failBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.fail", parentFunc);
    llvm::BasicBlock *contBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.cont", parentFunc);
    Builder->CreateCondBr(isNull, failBB, contBB);

    Builder->SetInsertPoint(failBB);
    llvm::Function *abortFn = TheModule->getFunction("abort");
    if (!abortFn) {
      llvm::FunctionType *abortTy =
          llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      abortFn = llvm::Function::Create(abortTy,
                                       llvm::Function::ExternalLinkage, "abort",
                                       TheModule.get());
    }
    Builder->CreateCall(abortFn);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(contBB);

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), slotIt->second);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, methodTablePtr, slotIndex, "hybrid.iface.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.iface.fn");

    return emitDynamicCallRaw(memberInfo, fnPtrRaw, std::move(argValues),
                              argIsRef, typeOwner, preserveRefReturn);
  }

  bool isBaseQualifier =
      objectExpr && dynamic_cast<BaseExprAST *>(objectExpr) != nullptr;
  bool canVirtual = !isStaticMethod &&
                    info.kind == AggregateKind::Class &&
                    memberInfo.vtableSlot !=
                        std::numeric_limits<unsigned>::max() &&
                    !isBaseQualifier;

  if (canVirtual) {
    auto structIt = StructTypes.find(ownerName);
    if (structIt == StructTypes.end())
      return LogErrorV(("Internal error: struct type for '" + ownerName +
                        "' unavailable during dispatch")
                           .c_str());

    llvm::StructType *ownerStructTy = structIt->second;
    llvm::Value *typedInstancePtr = instancePtr;
    if (typedInstancePtr->getType() != pointerType(ownerStructTy)) {
      typedInstancePtr = Builder->CreateBitCast(
          typedInstancePtr, pointerType(ownerStructTy),
          "hybrid.method.recv");
    }

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateStructGEP(
        ownerStructTy, typedInstancePtr, 0, "hybrid.header.ptr");
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    llvm::Value *vtableAddr = Builder->CreateStructGEP(
        getTypeDescriptorType(), descriptorValue, 2, "hybrid.vtable.ptr");
    llvm::Value *vtablePtr =
        Builder->CreateLoad(voidPtrPtrTy, vtableAddr, "hybrid.vtable");

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), memberInfo.vtableSlot);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, vtablePtr, slotIndex, "hybrid.vtable.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.vtable.fn");

    return emitDynamicCallRaw(memberInfo, fnPtrRaw, std::move(argValues),
                              argIsRef, typeOwner, preserveRefReturn);
  }

  return emitResolvedCallInternal(memberInfo.signature, std::move(argValues),
                                  argIsRef, nullptr, false, nullptr,
                                  typeOwner, nullptr, preserveRefReturn);
}
