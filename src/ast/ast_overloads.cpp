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

static void dumpGenericBindingStackForOverloads(const GenericsDiagnostics &diag) {
  if (diag.bindingStack.empty())
    return;
  fprintf(stderr, "[generics-stack] Active bindings:\n");
  for (std::size_t idx = diag.bindingStack.size(); idx-- > 0;) {
    fprintf(stderr, "  #%zu %s\n", diag.bindingStack.size() - idx,
            diag.bindingStack[idx].c_str());
  }
}

class GenericTypeBindingScope {
public:
  explicit GenericTypeBindingScope(const std::map<std::string, TypeInfo> &bindings,
                                   std::string frameLabel = {})
      : ctx(currentCodegen()), active(false), label(std::move(frameLabel)) {
    GenericsDiagnostics &diag = ctx.genericsDiagnostics;
    if (diag.maxBindingDepth > 0 &&
        diag.currentBindingDepth >= diag.maxBindingDepth) {
      diag.depthLimitHit = true;
      reportCompilerError("Maximum generic binding depth (" +
                          std::to_string(diag.maxBindingDepth) + ") exceeded");
      if (diag.stackDumpEnabled)
        dumpGenericBindingStackForOverloads(diag);
      return;
    }
    ctx.genericTypeBindingsStack.push_back(bindings);
    active = true;
    ++diag.currentBindingDepth;
    if (diag.currentBindingDepth > diag.peakBindingDepth)
      diag.peakBindingDepth = diag.currentBindingDepth;
    if (!label.empty())
      diag.bindingStack.push_back(label);
  }

  GenericTypeBindingScope(const GenericTypeBindingScope &) = delete;
  GenericTypeBindingScope &operator=(const GenericTypeBindingScope &) = delete;

  ~GenericTypeBindingScope() {
    if (!active)
      return;
    ctx.genericTypeBindingsStack.pop_back();
    GenericsDiagnostics &diag = ctx.genericsDiagnostics;
    if (!label.empty() && !diag.bindingStack.empty())
      diag.bindingStack.pop_back();
    if (diag.currentBindingDepth > 0)
      --diag.currentBindingDepth;
  }

  bool isActive() const { return active; }

private:
  CodegenContext &ctx;
  bool active = false;
  std::string label;
};

void markArcSlotDestroyed(llvm::Value *storage) {
  if (!storage)
    return;
  for (auto &frame : CG.arcScopeStack) {
    for (auto &slot : frame) {
      if (slot.storage == storage)
        slot.isTemporary = true;
    }
  }
}

void registerArcLocal(const std::string &name, llvm::Value *storage,
                      const TypeInfo &info, bool isTemporary) {
  if (!storage || info.isAlias() || !typeNeedsLifetimeTracking(info))
    return;
  if (CG.arcScopeStack.empty())
    pushArcScope();
  ARCLifetimeSlot slot;
  slot.storage = storage;
  slot.type = info;
  slot.isTemporary = isTemporary;
  slot.lifetimeInfo = name.empty() ? nullptr : lookupLifetimePlanEntry(name);
  CG.arcScopeStack.back().push_back(slot);
}

llvm::StructType *
getOrCreateSharedControlBlockType(const std::string &constructedName,
                                  llvm::Type *payloadTy) {
  std::string controlName =
      "__HybridSharedControl$" + sanitizeForMangle(constructedName);
  auto it = StructTypes.find(controlName);
  if (it != StructTypes.end() && it->second)
    return it->second;
  llvm::StructType *controlTy =
      llvm::StructType::create(*TheContext, controlName);
  StructTypes[controlName] = controlTy;
  auto *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  std::vector<llvm::Type *> body = {int32Ty, int32Ty, payloadTy};
  controlTy->setBody(body);
  return controlTy;
}

static bool genericBindingEquals(const GenericBindingKey &lhs,
                                 const GenericBindingKey &rhs) {
  return lhs.typeName == rhs.typeName &&
         lhs.typeArguments == rhs.typeArguments;
}

bool typeInfoEquals(const TypeInfo &lhs, const TypeInfo &rhs) {
  if (lhs.typeName != rhs.typeName)
    return false;
  if (lhs.baseTypeName != rhs.baseTypeName)
    return false;
  if (lhs.pointerDepth != rhs.pointerDepth)
    return false;
  if (lhs.isArray != rhs.isArray)
    return false;
  if (lhs.arrayDepth != rhs.arrayDepth)
    return false;
  if (lhs.arrayRanks != rhs.arrayRanks)
    return false;
  if (lhs.isMultidimensional != rhs.isMultidimensional)
    return false;
  if (lhs.isNullable != rhs.isNullable)
    return false;
  if (lhs.elementNullable != rhs.elementNullable)
    return false;
  if (lhs.refStorage != rhs.refStorage)
    return false;
  if (lhs.isMutable != rhs.isMutable)
    return false;
  if (lhs.declaredRef != rhs.declaredRef)
    return false;
  if (lhs.isGenericParameter != rhs.isGenericParameter)
    return false;
  if (lhs.ownership != rhs.ownership)
    return false;
  if (lhs.smartPointerKind != rhs.smartPointerKind)
    return false;
  if (!genericBindingEquals(lhs.genericKey, rhs.genericKey))
    return false;
  if (lhs.typeArguments.size() != rhs.typeArguments.size())
    return false;
  for (size_t i = 0; i < lhs.typeArguments.size(); ++i) {
    if (!typeInfoEquals(lhs.typeArguments[i], rhs.typeArguments[i]))
      return false;
  }
  return true;
}

static bool isReferenceLikeParameter(const TypeInfo &info) {
  return info.pointerDepth > 0 || info.isArray || info.isNullable ||
         info.isReference() || info.participatesInARC() || info.isSmartPointer();
}

static bool numericLiteralEquals(const NumericLiteral &lhs,
                                 const NumericLiteral &rhs) {
  if (lhs.kind != rhs.kind)
    return false;
  if (lhs.isInteger() && rhs.isInteger())
    return lhs.getIntegerValue() == rhs.getIntegerValue();
  if (lhs.isDecimal() && rhs.isDecimal())
    return lhs.getSpelling() == rhs.getSpelling();
  if (lhs.isFloating() && rhs.isFloating())
    return lhs.getFloatValue().compare(rhs.getFloatValue()) ==
           llvm::APFloat::cmpEqual;
  return false;
}

static bool defaultArgEquals(const DefaultArgInfo &lhs,
                             const DefaultArgInfo &rhs) {
  if (lhs.kind != rhs.kind)
    return false;
  switch (lhs.kind) {
  case DefaultArgInfo::Kind::None:
    return true;
  case DefaultArgInfo::Kind::Number:
    return numericLiteralEquals(lhs.numberValue, rhs.numberValue);
  case DefaultArgInfo::Kind::Bool:
    return lhs.boolValue == rhs.boolValue;
  case DefaultArgInfo::Kind::String:
    return lhs.stringValue == rhs.stringValue;
  case DefaultArgInfo::Kind::Char:
    return lhs.charValue == rhs.charValue;
  case DefaultArgInfo::Kind::Null:
    return true;
  case DefaultArgInfo::Kind::GlobalAddress:
    return lhs.globalName == rhs.globalName;
  }
  return false;
}

static DefaultArgInfo makeNumericDefault(const ConstantValue &value) {
  DefaultArgInfo info;
  info.kind = DefaultArgInfo::Kind::Number;
  switch (value.type) {
  case ConstantValue::INTEGER:
    info.numberValue = NumericLiteral::fromSigned(value.intVal);
    break;
  case ConstantValue::UNSIGNED_INTEGER:
    info.numberValue = NumericLiteral::fromUnsigned(value.uintVal);
    break;
  case ConstantValue::FLOAT: {
    llvm::APFloat ap(value.floatVal);
    info.numberValue =
        NumericLiteral::makeFloating(ap, std::to_string(value.floatVal),
                                     true, false);
    break;
  }
  case ConstantValue::BOOLEAN:
    break;
  }
  return info;
}

static bool resolveDefaultArgument(Parameter &param,
                                   const std::string &functionName) {
  if (!param.HasDefault || param.ResolvedDefault.isSet())
    return true;

  SourceLocation errorLoc = param.DefaultEqualsLocation.isValid()
                                ? param.DefaultEqualsLocation
                                : param.NameLocation;
  ScopedErrorLocation scoped(errorLoc);

  if (!param.DefaultValue) {
    reportCompilerError("Default value for parameter '" + param.Name +
                        "' of '" + functionName + "' is missing");
    return false;
  }

  DefaultArgInfo info;
  if (auto *num = dynamic_cast<NumberExprAST *>(param.DefaultValue.get())) {
    info.kind = DefaultArgInfo::Kind::Number;
    info.numberValue = num->getLiteral();
  } else if (auto *boolean =
                 dynamic_cast<BoolExprAST *>(param.DefaultValue.get())) {
    info.kind = DefaultArgInfo::Kind::Bool;
    info.boolValue = boolean->getValue();
  } else if (auto *str =
                 dynamic_cast<StringExprAST *>(param.DefaultValue.get())) {
    info.kind = DefaultArgInfo::Kind::String;
    info.stringValue = str->getValue();
  } else if (auto *ch =
                 dynamic_cast<CharExprAST *>(param.DefaultValue.get())) {
    info.kind = DefaultArgInfo::Kind::Char;
    info.charValue = ch->getValue();
  } else if (dynamic_cast<NullExprAST *>(param.DefaultValue.get())) {
    info.kind = DefaultArgInfo::Kind::Null;
  } else if (auto *unary = dynamic_cast<UnaryExprAST *>(param.DefaultValue.get());
             unary && unary->getOp() == "#") {
    auto *var = dynamic_cast<VariableExprAST *>(unary->getOperand());
    if (!var) {
      reportCompilerError("Default value for parameter '" + param.Name +
                          "' must take the address of a global value");
      return false;
    }
    info.kind = DefaultArgInfo::Kind::GlobalAddress;
    info.globalName = var->getName();
  } else {
    ConstantValue constVal(0LL);
    SourceLocation failLoc{};
    if (!EvaluateConstantExpression(param.DefaultValue.get(), constVal,
                                    &failLoc)) {
      ScopedErrorLocation failing(
          failLoc.isValid() ? failLoc : errorLoc);
      reportCompilerError("Default value for parameter '" + param.Name +
                          "' must be a compile-time constant expression");
      return false;
    }

    if (constVal.type == ConstantValue::BOOLEAN) {
      info.kind = DefaultArgInfo::Kind::Bool;
      info.boolValue = constVal.boolVal;
    } else {
      info = makeNumericDefault(constVal);
    }
  }

  TypeInfo declared = param.DeclaredType;
  finalizeTypeInfoMetadata(declared);

  if (info.kind == DefaultArgInfo::Kind::GlobalAddress) {
    const bool expectsPointer =
        declared.pointerDepth > 0 || declared.isArray || declared.isReference();
    if (!expectsPointer) {
      reportCompilerError("Default value for parameter '" + param.Name +
                          "' must match its declared type");
      return false;
    }
  }

  if (info.kind == DefaultArgInfo::Kind::Null) {
    bool allowsNull = isReferenceLikeParameter(declared);
    if (param.IsRef && declared.pointerDepth == 0 && !declared.isArray &&
        !declared.isNullable && !declared.participatesInARC() &&
        !declared.isSmartPointer()) {
      allowsNull = false;
    }
    if (!allowsNull) {
      if (param.IsRef) {
        reportCompilerError("Null default value is not allowed for ref parameter '" +
                            param.Name + "'");
      } else {
        reportCompilerError("Null default value is only allowed for reference or nullable parameter '" +
                            param.Name + "'");
      }
      return false;
    }
  }

  if (info.kind == DefaultArgInfo::Kind::String) {
    std::string baseType = stripNullableAnnotations(typeNameFromInfo(declared));
    if (baseType != "string") {
      reportCompilerError(
          "Default value for parameter '" + param.Name +
          "' must match its declared type");
      return false;
    }
  }

  param.ResolvedDefault = info;
  return true;
}

bool validateParamsParameterList(const std::vector<Parameter> &params,
                                 const std::string &functionName) {
  int paramsIndex = -1;
  for (size_t i = 0; i < params.size(); ++i) {
    const auto &param = params[i];
    if (!param.IsParams)
      continue;

    if (paramsIndex != -1) {
      ScopedErrorLocation scoped(
          param.ParamsLocation.isValid() ? param.ParamsLocation
                                         : param.NameLocation);
      reportCompilerError("Only one params parameter is allowed");
      return false;
    }

    paramsIndex = static_cast<int>(i);

    auto scoped = ScopedErrorLocation(
        param.ParamsLocation.isValid() ? param.ParamsLocation
                                       : param.NameLocation);

    if (param.IsRef) {
      reportCompilerError("params parameter '" + param.Name +
                          "' cannot be declared as ref");
      return false;
    }

    if (param.HasDefault) {
      reportCompilerError("params parameter '" + param.Name +
                          "' cannot declare a default value");
      return false;
    }

    TypeInfo declared = param.DeclaredType;
    finalizeTypeInfoMetadata(declared);
    if (!declared.isArray || declared.arrayDepth != 1 ||
        declared.isMultidimensional) {
      reportCompilerError("params parameter '" + param.Name +
                          "' must be a single-dimensional array type");
      return false;
    }
  }

  if (paramsIndex != -1 &&
      paramsIndex != static_cast<int>(params.size()) - 1) {
    const auto &param = params[static_cast<size_t>(paramsIndex)];
    ScopedErrorLocation scoped(
        param.ParamsLocation.isValid() ? param.ParamsLocation
                                       : param.NameLocation);
    reportCompilerError("params parameter must be the final parameter in the list");
    return false;
  }

  (void)functionName;
  return true;
}

bool resolveParameterDefaults(std::vector<Parameter> &params,
                                     const std::string &functionName) {
  if (!validateParamsParameterList(params, functionName))
    return false;
  for (auto &param : params) {
    if (!resolveDefaultArgument(param, functionName))
      return false;
  }
  return true;
}

std::unique_ptr<ExprAST> instantiateDefaultExpr(
    const DefaultArgInfo &info) {
  switch (info.kind) {
  case DefaultArgInfo::Kind::Number:
    return std::make_unique<NumberExprAST>(info.numberValue);
  case DefaultArgInfo::Kind::Bool:
    return std::make_unique<BoolExprAST>(info.boolValue);
  case DefaultArgInfo::Kind::String:
    return std::make_unique<StringExprAST>(info.stringValue);
  case DefaultArgInfo::Kind::Char:
    return std::make_unique<CharExprAST>(info.charValue);
  case DefaultArgInfo::Kind::Null:
    return std::make_unique<NullExprAST>();
  case DefaultArgInfo::Kind::GlobalAddress: {
    auto var = std::make_unique<VariableExprAST>(info.globalName);
    auto addr =
        std::make_unique<UnaryExprAST>("#", std::move(var), true, true);
    return addr;
  }
  case DefaultArgInfo::Kind::None:
    break;
  }
  return nullptr;
}

std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params);
std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params);
std::vector<bool> gatherParamParamsFlags(const std::vector<Parameter> &params);
std::string makeMethodSignatureKey(const std::string &methodName,
                                   const std::vector<TypeInfo> &paramTypes,
                                   const std::vector<bool> &paramIsRef,
                                   bool skipFirstParam) {
  std::string key = methodName;
  key.push_back('(');
  size_t start = skipFirstParam && !paramTypes.empty() ? 1 : 0;
  for (size_t i = start; i < paramTypes.size(); ++i) {
    if (i != 0)
      key.push_back(',');
    if (i < paramIsRef.size() && paramIsRef[i])
      key.append("ref ");
    key.append(typeNameFromInfo(paramTypes[i]));
  }
  key.push_back(')');
  return key;
}

std::string makeMethodSignatureKey(const std::string &methodName,
                                   const PrototypeAST &proto,
                                   bool skipFirstParam) {
  std::vector<TypeInfo> paramTypes = gatherParamTypes(proto.getArgs());
  std::vector<bool> paramIsRef = gatherParamRefFlags(proto.getArgs());
  return makeMethodSignatureKey(methodName, paramTypes, paramIsRef,
                                skipFirstParam);
}

template <typename Func>
static void visitBaseChain(const std::string &typeName, Func &&fn) {
  std::set<std::string> visited;
  std::string currentName = typeName;

  while (true) {
    const CompositeTypeInfo *info = lookupCompositeInfo(currentName);
    if (!info || !info->baseClass)
      break;

    const std::string &baseName = *info->baseClass;
    if (!visited.insert(baseName).second)
      break;

    const CompositeTypeInfo *baseInfo = lookupCompositeInfo(baseName);
    if (!baseInfo)
      break;

    if (!fn(baseName, *baseInfo))
      break;

    currentName = baseName;
  }
}

void collectInterfaceAncestors(const std::string &interfaceName,
                               std::set<std::string> &out) {
  if (!out.insert(interfaceName).second)
    return;
  if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(interfaceName)) {
    for (const std::string &parent : ifaceInfo->interfaces)
      collectInterfaceAncestors(parent, out);
  }
}

void gatherInterfaceRequirements(
    const CompositeTypeInfo &metadata,
    std::map<std::string, MethodRequirement> &requirements) {
  std::set<std::string> interfaces;
  for (const std::string &iface : metadata.interfaces)
    collectInterfaceAncestors(iface, interfaces);

  for (const std::string &ifaceName : interfaces) {
    const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName);
    if (!ifaceInfo)
      continue;
    for (const auto &entry : ifaceInfo->methodInfo) {
      const CompositeMemberInfo &member = entry.second;
      std::string key = makeMethodSignatureKey(entry.first, member.parameterTypes,
                                               member.parameterIsRef, true);
      if (!requirements.contains(key))
        requirements.emplace(key, MethodRequirement{ifaceName, &member});
    }
  }
}

void collectAbstractBaseMethods(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements) {
  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    for (const auto &entry : baseInfo.methodInfo) {
      const CompositeMemberInfo &member = entry.second;
      if (!member.modifiers.isAbstract)
        continue;
      std::string key = makeMethodSignatureKey(entry.first, member.parameterTypes,
                                               member.parameterIsRef, true);
      if (!requirements.contains(key))
        requirements.emplace(key, MethodRequirement{baseName, &member});
    }
    return true;
  });
}

static void removeSatisfiedRequirementsFromType(
    const CompositeTypeInfo &metadata,
    std::map<std::string, MethodRequirement> &requirements,
    bool requireConcrete) {
  for (const auto &entry : metadata.methodInfo) {
    const CompositeMemberInfo &member = entry.second;
    if (requireConcrete && member.modifiers.isAbstract)
      continue;
    std::string key = makeMethodSignatureKey(entry.first, member.parameterTypes,
                                             member.parameterIsRef, true);
    requirements.erase(key);
  }
}

void removeInterfaceRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements) {
  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    (void)baseName;
    removeSatisfiedRequirementsFromType(baseInfo, requirements, false);
    return !requirements.empty();
  });
}

void removeAbstractRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements) {
  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    (void)baseName;
    removeSatisfiedRequirementsFromType(baseInfo, requirements, true);
    return !requirements.empty();
  });
}

std::optional<BaseMethodMatch>
findBaseMethodMatch(const std::string &typeName,
                    const std::string &methodName,
                    const PrototypeAST &proto) {
  const std::string key = makeMethodSignatureKey(methodName, proto, true);
  std::optional<BaseMethodMatch> result;

  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    auto it = baseInfo.methodInfo.find(methodName);
    if (it == baseInfo.methodInfo.end())
      return true;

    const CompositeMemberInfo &member = it->second;
    std::string baseKey = makeMethodSignatureKey(methodName, member.parameterTypes,
                                                 member.parameterIsRef, true);
    if (baseKey == key) {
      result = BaseMethodMatch{baseName, &member};
      return false;
    }
    return true;
  });

  return result;
}

std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params) {
  std::vector<TypeInfo> types;
  types.reserve(params.size());
  for (const auto &param : params)
    types.push_back(applyActiveTypeBindings(param.DeclaredType));
  return types;
}

std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params) {
  std::vector<bool> flags;
  flags.reserve(params.size());
  for (const auto &param : params)
    flags.push_back(param.IsRef);
  return flags;
}

std::vector<bool> gatherParamParamsFlags(const std::vector<Parameter> &params) {
  std::vector<bool> flags;
  flags.reserve(params.size());
  for (const auto &param : params)
    flags.push_back(param.IsParams);
  return flags;
}

static std::string formatSpecializationSignature(
    const std::string &name, const TypeInfo &returnType, bool returnsByRef,
    const std::vector<TypeInfo> &paramTypes,
    const std::vector<bool> &paramIsRef) {
  std::string signature = name;
  signature += "|ret=" + stripNullableAnnotations(typeNameFromInfo(returnType));
  signature += returnsByRef ? "|retref=1" : "|retref=0";
  for (std::size_t i = 0; i < paramTypes.size(); ++i) {
    signature += "|arg" + std::to_string(i) + "=" +
                 stripNullableAnnotations(typeNameFromInfo(paramTypes[i]));
    signature += paramIsRef[i] ? "&ref=1" : "&ref=0";
  }
  return signature;
}

FunctionOverload *findRegisteredOverload(
    const std::string &name, const TypeInfo &returnType, bool returnsByRef,
    const std::vector<TypeInfo> &paramTypes,
    const std::vector<bool> &paramIsRef) {
  auto it = CG.functionOverloads.find(name);
  if (it == CG.functionOverloads.end())
    return nullptr;

  for (auto &overload : it->second) {
    if (!typeInfoEquals(overload.returnType, returnType))
      continue;
    if (overload.returnsByRef != returnsByRef)
      continue;
    if (overload.parameterTypes.size() != paramTypes.size())
      continue;

    bool matches = true;
    for (size_t i = 0; i < paramTypes.size(); ++i) {
      if (overload.parameterIsRef[i] != paramIsRef[i] ||
          !typeInfoEquals(overload.parameterTypes[i], paramTypes[i])) {
        matches = false;
        break;
      }
    }

    if (matches)
      return &overload;
  }

  return nullptr;
}

static SourceLocation selectDefaultDiagnosticLocation(
    size_t idx, const std::vector<SourceLocation> &defaultLocs,
    const std::vector<SourceLocation> &nameLocs) {
  if (idx < defaultLocs.size() && defaultLocs[idx].isValid())
    return defaultLocs[idx];
  if (idx < nameLocs.size())
    return nameLocs[idx];
  return {};
}

static bool defaultsAreCompatible(
    const FunctionOverload &existing,
    const std::vector<DefaultArgInfo> &incomingDefaults,
    const std::vector<SourceLocation> &incomingDefaultLocs,
    const std::vector<SourceLocation> &incomingNameLocs,
    const std::vector<std::string> &paramNames,
    const std::string &functionName) {
  const size_t paramCount =
      std::max(existing.parameterDefaults.size(), incomingDefaults.size());
  for (size_t idx = 0; idx < paramCount; ++idx) {
    const bool existingHas =
        idx < existing.parameterDefaults.size() &&
        existing.parameterDefaults[idx].isSet();
    const bool incomingHas =
        idx < incomingDefaults.size() && incomingDefaults[idx].isSet();
    if (existingHas != incomingHas ||
        (existingHas &&
         !defaultArgEquals(existing.parameterDefaults[idx],
                           incomingDefaults[idx]))) {
      ScopedErrorLocation scoped(selectDefaultDiagnosticLocation(
          idx, incomingDefaultLocs, incomingNameLocs));
      const std::string paramLabel =
          idx < paramNames.size() && !paramNames[idx].empty()
              ? paramNames[idx]
              : std::to_string(idx);
      reportCompilerError("Default value for parameter '" + paramLabel +
                          "' of '" + functionName +
                          "' must match previous declaration");
      return false;
    }
  }

  return true;
}

FunctionOverload *registerFunctionOverload(const PrototypeAST &proto,
                                                  const std::string &mangledName) {
  std::vector<TypeInfo> paramTypes = gatherParamTypes(proto.getArgs());
  std::vector<bool> paramIsRef = gatherParamRefFlags(proto.getArgs());
  std::vector<bool> paramIsParams = gatherParamParamsFlags(proto.getArgs());
  std::vector<std::string> paramNames;
  std::vector<DefaultArgInfo> paramDefaults;
  std::vector<SourceLocation> paramDefaultLocs;
  std::vector<SourceLocation> paramNameLocs;
  paramNames.reserve(proto.getArgs().size());
  paramDefaults.reserve(proto.getArgs().size());
  paramDefaultLocs.reserve(proto.getArgs().size());
  paramNameLocs.reserve(proto.getArgs().size());
  for (const auto &param : proto.getArgs()) {
    paramNames.push_back(param.Name);
    paramDefaults.push_back(param.ResolvedDefault);
    paramDefaultLocs.push_back(param.DefaultEqualsLocation);
    paramNameLocs.push_back(param.NameLocation);
  }
  TypeInfo boundReturn = applyActiveTypeBindings(proto.getReturnTypeInfo());

  FunctionOverload *existing = findRegisteredOverload(
      proto.getName(), boundReturn, proto.returnsByRef(),
      paramTypes, paramIsRef);

  if (existing) {
    if (!defaultsAreCompatible(*existing, paramDefaults, paramDefaultLocs,
                               paramNameLocs, paramNames, proto.getName()))
      return nullptr;
    if (existing->mangledName.empty())
      existing->mangledName = mangledName;
    existing->isUnsafe = proto.isUnsafe();
    existing->isExtern = proto.isExtern();
    existing->parameterNames = paramNames;
    existing->parameterDefaults = paramDefaults;
    existing->parameterDefaultLocations = paramDefaultLocs;
    existing->parameterIsParams = paramIsParams;
    if (!proto.getGenericParameters().empty())
      existing->isGenericInstantiation = true;
    return existing;
  }

  FunctionOverload entry;
  entry.mangledName = mangledName;
  entry.returnType = boundReturn;
  entry.returnsByRef = proto.returnsByRef();
  entry.parameterTypes = std::move(paramTypes);
  entry.parameterIsRef = std::move(paramIsRef);
  entry.parameterIsParams = std::move(paramIsParams);
  entry.parameterNames = std::move(paramNames);
  entry.parameterDefaults = std::move(paramDefaults);
  entry.parameterDefaultLocations = std::move(paramDefaultLocs);
  entry.isUnsafe = proto.isUnsafe();
  entry.isExtern = proto.isExtern();
  entry.isGenericInstantiation = !proto.getGenericParameters().empty();

  auto &overloads = CG.functionOverloads[proto.getName()];
  overloads.push_back(std::move(entry));
  return &overloads.back();
}

bool verifyOverrideDefaultCompatibility(
    const CompositeMemberInfo &baseMember, const PrototypeAST &overrideProto,
    const std::string &baseOwner, const std::string &methodName,
    const std::string &derivedOwner) {
  const auto &args = overrideProto.getArgs();
  std::vector<SourceLocation> nameLocs;
  std::vector<SourceLocation> defaultLocs;
  nameLocs.reserve(args.size());
  defaultLocs.reserve(args.size());
  for (const auto &param : args) {
    nameLocs.push_back(param.NameLocation);
    defaultLocs.push_back(param.DefaultEqualsLocation);
  }

  for (size_t idx = 0; idx < args.size(); ++idx) {
    const bool baseHas =
        idx < baseMember.parameterDefaults.size() &&
        baseMember.parameterDefaults[idx].isSet();
    const bool overrideHas = args[idx].ResolvedDefault.isSet();
    if (baseHas != overrideHas ||
        (baseHas &&
         !defaultArgEquals(baseMember.parameterDefaults[idx],
                           args[idx].ResolvedDefault))) {
      ScopedErrorLocation scoped(selectDefaultDiagnosticLocation(
          idx, defaultLocs, nameLocs));
      std::string paramLabel;
      if (idx < baseMember.parameterNames.size() &&
          !baseMember.parameterNames[idx].empty()) {
        paramLabel = baseMember.parameterNames[idx];
      } else if (idx < args.size() && !args[idx].Name.empty()) {
        paramLabel = args[idx].Name;
      } else {
        paramLabel = std::to_string(idx);
      }
      reportCompilerError("Override of '" + baseOwner + "." + methodName +
                          "' in '" + derivedOwner +
                          "' must use the same default for parameter '" +
                          paramLabel + "'");
      return false;
    }
  }

  return true;
}

FunctionOverload *lookupFunctionOverload(const PrototypeAST &proto) {
  std::vector<TypeInfo> paramTypes = gatherParamTypes(proto.getArgs());
  std::vector<bool> paramIsRef = gatherParamRefFlags(proto.getArgs());
  TypeInfo boundReturn = applyActiveTypeBindings(proto.getReturnTypeInfo());
  return findRegisteredOverload(proto.getName(), boundReturn,
                                proto.returnsByRef(), paramTypes, paramIsRef);
}

class FunctionInstantiationScope {
public:
  FunctionInstantiationScope()
      : ctx(currentCodegen()),
        savedInsertBlock(Builder->GetInsertBlock()),
        hasInsertPoint(savedInsertBlock != nullptr),
        savedNamedValues(ctx.namedValues),
        savedLocalTypes(ctx.localTypes),
        savedArraySizes(ctx.arraySizes),
        savedNonNullFacts(ctx.nonNullFactsStack),
        savedLoopExitBlocks(ctx.loopExitBlocks),
        savedLoopContinueBlocks(ctx.loopContinueBlocks) {
    if (hasInsertPoint)
      savedInsertPoint = Builder->GetInsertPoint();
  }

  FunctionInstantiationScope(const FunctionInstantiationScope &) = delete;
  FunctionInstantiationScope &
  operator=(const FunctionInstantiationScope &) = delete;

  ~FunctionInstantiationScope() {
    ctx.namedValues = std::move(savedNamedValues);
    ctx.localTypes = std::move(savedLocalTypes);
    ctx.arraySizes = std::move(savedArraySizes);
    ctx.nonNullFactsStack = std::move(savedNonNullFacts);
    ctx.loopExitBlocks = std::move(savedLoopExitBlocks);
    ctx.loopContinueBlocks = std::move(savedLoopContinueBlocks);
    if (hasInsertPoint)
      Builder->SetInsertPoint(savedInsertBlock, savedInsertPoint);
    else
      Builder->ClearInsertionPoint();
  }

private:
  CodegenContext &ctx;
  llvm::BasicBlock *savedInsertBlock = nullptr;
  llvm::BasicBlock::iterator savedInsertPoint;
  bool hasInsertPoint = false;
  std::map<std::string, llvm::Value *> savedNamedValues;
  std::map<std::string, TypeInfo> savedLocalTypes;
  std::map<std::string, std::vector<int64_t>> savedArraySizes;
  std::vector<std::set<std::string>> savedNonNullFacts;
  std::vector<llvm::BasicBlock *> savedLoopExitBlocks;
  std::vector<llvm::BasicBlock *> savedLoopContinueBlocks;
};

llvm::Function *InstantiateGenericFunction(
    const std::string &name, const std::vector<TypeInfo> &typeArguments,
    const std::map<std::string, TypeInfo> *additionalBindings) {
  const std::vector<GenericFunctionTemplate> *templates =
      lookupGenericFunctionTemplates(name);
  if (!templates)
    return nullptr;

  llvm::Function *primaryResult = nullptr;

  // Snapshot template pointers before iterating. Codegen can register
  // additional templates under the same name, which may reallocate the
  // registry vector and invalidate iterators/references.
  std::vector<FunctionAST *> templateSnapshot;
  templateSnapshot.reserve(templates->size());
  for (const auto &entry : *templates) {
    if (entry.function)
      templateSnapshot.push_back(entry.function.get());
  }

  for (FunctionAST *fn : templateSnapshot) {
    if (!fn)
      continue;

    PrototypeAST *proto = fn->getProto();
    if (!proto)
      continue;

    const auto &genericParams = proto->getGenericParameters();
    if (genericParams.size() != typeArguments.size())
      continue;

    std::map<std::string, TypeInfo> substitutions;
    if (additionalBindings)
      substitutions = *additionalBindings;

    bool duplicateBinding = false;
    for (size_t idx = 0; idx < genericParams.size(); ++idx) {
      const std::string &paramName = genericParams[idx];
      const auto [itBinding, inserted] =
          substitutions.emplace(paramName, typeArguments[idx]);
      if (!inserted) {
        if (!typeInfoEquals(itBinding->second, typeArguments[idx])) {
          reportCompilerError(
              "Conflicting bindings for generic parameter '" + paramName +
                  "' while instantiating '" + name + "'",
              "Ensure method-level generic parameters use unique names.");
          duplicateBinding = true;
          break;
        }
      }
    }

    if (duplicateBinding)
      return nullptr;

    std::string frameLabel =
        buildGenericFrameLabel(proto->getName(), typeArguments);
    GenericTypeBindingScope bindingScope(substitutions, frameLabel);
    if (!bindingScope.isActive())
      return nullptr;

    TypeInfo boundReturn = applyActiveTypeBindings(proto->getReturnTypeInfo());
    std::vector<TypeInfo> boundParams = gatherParamTypes(proto->getArgs());
    std::vector<bool> boundParamIsRef = gatherParamRefFlags(proto->getArgs());
    std::string instantiationKey =
        formatSpecializationSignature(proto->getName(), boundReturn,
                                      proto->returnsByRef(), boundParams,
                                      boundParamIsRef);

    if (const auto cacheIt =
            CG.genericFunctionInstantiationCache.find(instantiationKey);
        cacheIt != CG.genericFunctionInstantiationCache.end()) {
      if (llvm::Function *cached =
              TheModule->getFunction(cacheIt->second)) {
        noteFunctionCacheHit();
        currentCodegen().instantiatedGenericFunctions.insert(
            std::string(cached->getName()));
        if (!primaryResult)
          primaryResult = cached;
        continue;
      }
    }

    if (FunctionOverload *existing =
            findRegisteredOverload(proto->getName(), boundReturn,
                                   proto->returnsByRef(), boundParams,
                                   boundParamIsRef)) {
      noteFunctionCacheHit();
      if (!existing->function)
        existing->function =
            TheModule->getFunction(existing->mangledName);
      if (existing->function)
        currentCodegen().instantiatedGenericFunctions.insert(
            std::string(existing->function->getName()));
      if (!primaryResult)
        primaryResult = existing->function;
      continue;
    }

    noteFunctionCacheMiss();
    if (!recordGenericInstantiation(true))
      return nullptr;
    FunctionInstantiationScope instantiationScope;
    llvm::Function *instantiated = fn->codegen();
    if (!instantiated)
      return nullptr;
    CG.genericFunctionInstantiationCache[instantiationKey] =
        std::string(instantiated->getName());
    currentCodegen().instantiatedGenericFunctions.insert(
        std::string(instantiated->getName()));
    if (!primaryResult)
      primaryResult = instantiated;
  }

  return primaryResult;
}
