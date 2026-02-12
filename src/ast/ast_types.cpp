#include "ast_internal.h"
#include <iostream>

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

static void populateTypeInfoGenerics(TypeInfo &info);
static bool emitSmartPointerHelpers(const std::string &constructedName,
                                    llvm::StructType *structTy,
                                    CompositeTypeInfo &metadata,
                                    SmartPointerKind kind);

class GenericTypeBindingScope {
public:
  explicit GenericTypeBindingScope(
      const std::map<std::string, TypeInfo> &bindings,
      std::string frameLabel = {})
      : active(pushGenericTypeBindingScope(bindings, std::move(frameLabel))) {}

  GenericTypeBindingScope(const GenericTypeBindingScope &) = delete;
  GenericTypeBindingScope &operator=(const GenericTypeBindingScope &) = delete;

  ~GenericTypeBindingScope() {
    if (active)
      popGenericTypeBindingScope();
  }

  bool isActive() const { return active; }

private:
  bool active = false;
};

class GenericInstantiationScope {
public:
  explicit GenericInstantiationScope(std::string name) : active(true) {
    pushInstantiationContext(std::move(name));
  }

  GenericInstantiationScope(const GenericInstantiationScope &) = delete;
  GenericInstantiationScope &
  operator=(const GenericInstantiationScope &) = delete;

  ~GenericInstantiationScope() {
    if (active)
      popInstantiationContext();
  }

private:
  bool active = false;
};

class FunctionInstantiationScope {
public:
  FunctionInstantiationScope()
      : ctx(currentCodegen()), savedInsertBlock(Builder->GetInsertBlock()),
        hasInsertPoint(savedInsertBlock != nullptr),
        savedNamedValues(ctx.namedValues), savedLocalTypes(ctx.localTypes),
        savedArraySizes(ctx.arraySizes), savedNonNullFacts(ctx.nonNullFactsStack),
        savedLoopExitBlocks(ctx.loopExitBlocks),
        savedLoopContinueBlocks(ctx.loopContinueBlocks) {}

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
    if (!hasInsertPoint) {
      Builder->ClearInsertionPoint();
      return;
    }

    if (!savedInsertBlock || !blockStillExists(savedInsertBlock)) {
      Builder->ClearInsertionPoint();
      return;
    }

    if (llvm::Instruction *terminator = savedInsertBlock->getTerminator())
      Builder->SetInsertPoint(terminator);
    else
      Builder->SetInsertPoint(savedInsertBlock);
  }

private:
  static bool blockStillExists(llvm::BasicBlock *candidate) {
    if (!candidate || !TheModule)
      return false;
    for (llvm::Function &fn : *TheModule) {
      for (llvm::BasicBlock &bb : fn) {
        if (&bb == candidate)
          return true;
      }
    }
    return false;
  }

  CodegenContext &ctx;
  llvm::BasicBlock *savedInsertBlock = nullptr;
  bool hasInsertPoint = false;
  std::map<std::string, llvm::Value *> savedNamedValues;
  std::map<std::string, TypeInfo> savedLocalTypes;
  std::map<std::string, std::vector<int64_t>> savedArraySizes;
  std::vector<std::set<std::string>> savedNonNullFacts;
  std::vector<llvm::BasicBlock *> savedLoopExitBlocks;
  std::vector<llvm::BasicBlock *> savedLoopContinueBlocks;
};

ParsedTypeDescriptor parseTypeString(const std::string &typeName) {
  ParsedTypeDescriptor desc;
  std::string_view working(typeName);

  auto trimLeadingWhitespace = [&]() {
    while (!working.empty() &&
           std::isspace(static_cast<unsigned char>(working.front())))
      working.remove_prefix(1);
  };

  trimLeadingWhitespace();

  auto stripOwnershipQualifier = [&]() {
    auto tryConsume = [&](std::string_view keyword) -> bool {
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
      return true;
    };
  };

  stripOwnershipQualifier();

  std::string normalized(working.begin(), working.end());
  std::string sanitized;
  sanitized.reserve(normalized.size());

  bool pendingNullable = false;
  bool arraySeen = false;

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
      if (i > 0)
        --i;
      if (pendingNullable) {
        desc.isNullable = true;
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
      desc.arrayRanks.push_back(rank);
      arraySeen = true;
      if (pendingNullable) {
        desc.elementNullable = true;
        pendingNullable = false;
      }
      i = close;
      continue;
    }

    sanitized.push_back(c);
  }

  if (pendingNullable)
    desc.isNullable = true;

  desc.sanitized = sanitized;
  desc.isArray = arraySeen || isArrayTypeName(desc.sanitized);
  desc.pointerDepth = computePointerDepth(desc.sanitized);
  desc.arrayDepth = desc.arrayRanks.size();
  desc.isMultidimensional = std::ranges::any_of(desc.arrayRanks, [](unsigned rank) { return rank > 1; });
  if (desc.pointerDepth > 0 && !desc.isArray)
    desc.isNullable = true;

  return desc;
}

std::optional<TypeInfo> resolveExprTypeInfo(const ExprAST *expr);
bool ensureMemberAccessAllowed(const MemberModifiers &modifiers,
                               AccessIntent intent,
                               const std::string &ownerName,
                               const std::string &memberName);

std::string describeAggregateKind(AggregateKind kind) {
  switch (kind) {
  case AggregateKind::Struct:
    return "struct";
  case AggregateKind::Class:
    return "class";
  case AggregateKind::Interface:
    return "interface";
  }
  return "type";
}

static bool isDerivedFrom(const std::string &derivedName,
                          const std::string &baseName) {
  if (derivedName == baseName)
    return true;

  std::set<std::string> visited;
  std::string current = derivedName;

  while (visited.insert(current).second) {
    const CompositeTypeInfo *info = lookupCompositeInfo(current);
    if (!info || !info->baseClass)
      break;

    const std::string &parent = *info->baseClass;
    if (parent == baseName)
      return true;
    current = parent;
  }

  return false;
}

bool validateCompositeHierarchy(const std::string &name,
                                CompositeTypeInfo &metadata) {
  auto fail = [&](const std::string &message) {
    reportCompilerError(message);
    return false;
  };

  if (metadata.baseClass) {
    if (*metadata.baseClass == name)
      return fail("Type '" + name + "' cannot inherit from itself");

    const CompositeTypeInfo *baseInfo =
        lookupCompositeInfo(*metadata.baseClass);
    if (!baseInfo)
      return fail("Type '" + name + "' inherits from undefined base '" +
                  *metadata.baseClass + "'");

    if (metadata.kind == AggregateKind::Class) {
      if (baseInfo->kind == AggregateKind::Interface) {
        if (std::find(metadata.interfaces.begin(), metadata.interfaces.end(),
                      *metadata.baseClass) == metadata.interfaces.end()) {
          metadata.interfaces.push_back(*metadata.baseClass);
        }
        metadata.baseClass.reset();
        baseInfo = nullptr;
      } else if (baseInfo->kind != AggregateKind::Class) {
        return fail("Class '" + name + "' can only inherit from another class");
      }
    } else if (metadata.kind == AggregateKind::Interface) {
      if (baseInfo->kind != AggregateKind::Interface)
        return fail("Interface '" + name + "' can only inherit from other interfaces");
    }

    if (metadata.baseClass) {
      std::set<std::string> visited;
      std::string current = *metadata.baseClass;
      while (visited.insert(current).second) {
        if (current == name)
          return fail("Inheritance cycle detected for type '" + name + "'");
        const CompositeTypeInfo *info = lookupCompositeInfo(current);
        if (!info || !info->baseClass)
          break;
        current = *info->baseClass;
      }
    }
  }

  std::set<std::string> seenInterfaces;
  for (const std::string &iface : metadata.interfaces) {
    if (!seenInterfaces.insert(iface).second)
      return fail("Type '" + name + "' implements interface '" + iface +
                  "' multiple times");

    const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(iface);
    if (!ifaceInfo)
      return fail("Type '" + name + "' implements undefined interface '" +
                  iface + "'");
    if (ifaceInfo->kind != AggregateKind::Interface)
      return fail("Type '" + name + "' cannot implement non-interface type '" +
                  iface + "'");
  }

  return true;
}

static ActiveCompositeContext *currentCompositeContextMutable() {
  if (CG.compositeContextStack.empty())
    return nullptr;
  return &CG.compositeContextStack.back();
}

const ActiveCompositeContext *currentCompositeContext() {
  return currentCompositeContextMutable();
}

std::optional<OverloadableOperator>
overloadableOperatorFromSymbol(std::string_view symbol) {
  if (symbol == "=")
    return OverloadableOperator::Assign;
  if (symbol == "+=")
    return OverloadableOperator::AddAssign;
  if (symbol == "-=")
    return OverloadableOperator::SubAssign;
  if (symbol == "*=")
    return OverloadableOperator::MulAssign;
  if (symbol == "/=")
    return OverloadableOperator::DivAssign;
  if (symbol == "%=")
    return OverloadableOperator::ModAssign;
  if (symbol == "+")
    return OverloadableOperator::Add;
  if (symbol == "-")
    return OverloadableOperator::Sub;
  if (symbol == "*")
    return OverloadableOperator::Mul;
  if (symbol == "/")
    return OverloadableOperator::Div;
  if (symbol == "%")
    return OverloadableOperator::Mod;
  if (symbol == "==")
    return OverloadableOperator::Equal;
  if (symbol == "!=")
    return OverloadableOperator::NotEqual;
  if (symbol == "<")
    return OverloadableOperator::Less;
  if (symbol == ">")
    return OverloadableOperator::Greater;
  if (symbol == "<=")
    return OverloadableOperator::LessEqual;
  if (symbol == ">=")
    return OverloadableOperator::GreaterEqual;
  if (symbol == "@")
    return OverloadableOperator::Dereference;
  if (symbol == "#")
    return OverloadableOperator::AddressOf;
  if (symbol == "[]")
    return OverloadableOperator::Index;
  return std::nullopt;
}

std::string_view overloadableOperatorSymbol(OverloadableOperator op) {
  switch (op) {
  case OverloadableOperator::Assign:
    return "=";
  case OverloadableOperator::AddAssign:
    return "+=";
  case OverloadableOperator::SubAssign:
    return "-=";
  case OverloadableOperator::MulAssign:
    return "*=";
  case OverloadableOperator::DivAssign:
    return "/=";
  case OverloadableOperator::ModAssign:
    return "%=";
  case OverloadableOperator::Add:
    return "+";
  case OverloadableOperator::Sub:
    return "-";
  case OverloadableOperator::Mul:
    return "*";
  case OverloadableOperator::Div:
    return "/";
  case OverloadableOperator::Mod:
    return "%";
  case OverloadableOperator::Equal:
    return "==";
  case OverloadableOperator::NotEqual:
    return "!=";
  case OverloadableOperator::Less:
    return "<";
  case OverloadableOperator::Greater:
    return ">";
  case OverloadableOperator::LessEqual:
    return "<=";
  case OverloadableOperator::GreaterEqual:
    return ">=";
  case OverloadableOperator::Dereference:
    return "@";
  case OverloadableOperator::AddressOf:
    return "#";
  case OverloadableOperator::Index:
    return "[]";
  case OverloadableOperator::None:
    break;
  }
  return {};
}

std::string_view overloadableOperatorCanonicalName(OverloadableOperator op) {
  switch (op) {
  case OverloadableOperator::Assign:
    return "__op_assign";
  case OverloadableOperator::AddAssign:
    return "__op_add_assign";
  case OverloadableOperator::SubAssign:
    return "__op_sub_assign";
  case OverloadableOperator::MulAssign:
    return "__op_mul_assign";
  case OverloadableOperator::DivAssign:
    return "__op_div_assign";
  case OverloadableOperator::ModAssign:
    return "__op_mod_assign";
  case OverloadableOperator::Add:
    return "__op_add";
  case OverloadableOperator::Sub:
    return "__op_sub";
  case OverloadableOperator::Mul:
    return "__op_mul";
  case OverloadableOperator::Div:
    return "__op_div";
  case OverloadableOperator::Mod:
    return "__op_mod";
  case OverloadableOperator::Equal:
    return "__op_eq";
  case OverloadableOperator::NotEqual:
    return "__op_ne";
  case OverloadableOperator::Less:
    return "__op_lt";
  case OverloadableOperator::Greater:
    return "__op_gt";
  case OverloadableOperator::LessEqual:
    return "__op_le";
  case OverloadableOperator::GreaterEqual:
    return "__op_ge";
  case OverloadableOperator::Dereference:
    return "__op_deref";
  case OverloadableOperator::AddressOf:
    return "__op_addr";
  case OverloadableOperator::Index:
    return "__op_index";
  case OverloadableOperator::None:
    break;
  }
  return {};
}

bool overloadableOperatorRequiresUnsafe(OverloadableOperator op) {
  return op == OverloadableOperator::Dereference ||
         op == OverloadableOperator::AddressOf;
}

std::string baseCompositeName(const std::string &typeName) {
  if (typeName.empty())
    return {};
  ParsedTypeDescriptor desc = parseTypeString(typeName);
  std::string base = desc.sanitized;
  size_t atPos = base.find('@');
  if (atPos != std::string::npos)
    base.erase(atPos);
  size_t bracketPos = base.find('[');
  if (bracketPos != std::string::npos)
    base.erase(bracketPos);
  return base;
}

std::string resolveCompositeName(const ExprAST *expr) {
  if (!expr)
    return {};
  return baseCompositeName(expr->getTypeName());
}

std::string sanitizeCompositeLookupName(const std::string &typeName) {
  if (typeName.empty())
    return {};
  std::string cleaned = stripNullableAnnotations(typeName);
  size_t atPos = cleaned.find('@');
  if (atPos != std::string::npos)
    cleaned.erase(atPos);
  size_t bracketPos = cleaned.find('[');
  if (bracketPos != std::string::npos)
    cleaned.erase(bracketPos);
  return cleaned;
}

static std::string makeMemberKey(const std::string &owner, const std::string &member) {
  return owner + "." + member;
}

static bool delegateSignatureMatches(const DelegateTypeInfo &delegateInfo,
                                     const TypeInfo &returnType,
                                     bool returnsByRef,
                                     const std::vector<TypeInfo> &paramTypes,
                                     const std::vector<bool> &paramIsRef,
                                     const std::vector<bool> &paramIsParams,
                                     size_t paramOffset) {
  if (returnsByRef != delegateInfo.returnsByRef)
    return false;
  if (!typeInfoEquals(returnType, delegateInfo.returnType))
    return false;
  if (paramTypes.size() < paramOffset)
    return false;
  if (paramTypes.size() - paramOffset != delegateInfo.parameterTypes.size())
    return false;

  for (size_t idx = 0; idx < delegateInfo.parameterTypes.size(); ++idx) {
    size_t paramIndex = paramOffset + idx;
    if (paramIndex >= paramTypes.size())
      return false;
    bool expectedRef = paramIndex < paramIsRef.size() ? paramIsRef[paramIndex] : false;
    bool expectedParams =
        paramIndex < paramIsParams.size() ? paramIsParams[paramIndex] : false;
    bool delegateRef =
        idx < delegateInfo.parameterIsRef.size() ? delegateInfo.parameterIsRef[idx] : false;
    bool delegateParams =
        idx < delegateInfo.parameterIsParams.size() ? delegateInfo.parameterIsParams[idx] : false;
    if (expectedRef != delegateRef || expectedParams != delegateParams)
      return false;
    if (!typeInfoEquals(paramTypes[paramIndex], delegateInfo.parameterTypes[idx]))
      return false;
  }

  return true;
}

static FunctionOverload *selectOverloadForDelegate(
    const std::string &calleeBase, const DelegateTypeInfo &delegateInfo,
    bool reportErrors) {
  auto it = CG.functionOverloads.find(calleeBase);
  if (it == CG.functionOverloads.end()) {
    if (reportErrors) {
      reportCompilerError("Unknown function '" + calleeBase +
                          "' for delegate '" + delegateInfo.name + "'");
    }
    return nullptr;
  }

  FunctionOverload *match = nullptr;
  for (auto &overload : it->second) {
    if (!delegateSignatureMatches(delegateInfo, overload.returnType,
                                  overload.returnsByRef,
                                  overload.parameterTypes,
                                  overload.parameterIsRef,
                                  overload.parameterIsParams, 0)) {
      continue;
    }
    if (match) {
      if (reportErrors) {
        reportCompilerError("Ambiguous reference to function '" + calleeBase +
                            "' for delegate '" + delegateInfo.name + "'");
      }
      return nullptr;
    }
    match = &overload;
  }

  if (!match && reportErrors) {
    reportCompilerError("No overload of function '" + calleeBase +
                        "' matches delegate '" + delegateInfo.name + "'");
  }
  return match;
}

static llvm::Value *buildDelegateValue(const DelegateTypeInfo &delegateInfo,
                                       llvm::Value *fnPtr,
                                       llvm::Value *receiver) {
  DelegateTypeInfo *mutableInfo = lookupDelegateInfoMutable(delegateInfo.name);
  DelegateTypeInfo localInfo = delegateInfo;
  DelegateTypeInfo &info = mutableInfo ? *mutableInfo : localInfo;

  llvm::StructType *structTy = ensureDelegateStructType(info);
  if (!structTy)
    return nullptr;
  llvm::FunctionType *fnType = ensureDelegateFunctionType(info);
  if (!fnType)
    return nullptr;

  llvm::Type *expectedFnPtrTy = pointerType(fnType);
  if (!fnPtr || !fnPtr->getType()->isPointerTy())
    return LogErrorV("Delegate target must be a function pointer");
  if (fnPtr->getType() != expectedFnPtrTy)
    fnPtr = Builder->CreateBitCast(fnPtr, expectedFnPtrTy, "delegate.fn.cast");

  llvm::Type *expectedRecvTy = pointerType();
  if (!receiver) {
    receiver = llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(expectedRecvTy));
  } else if (receiver->getType() != expectedRecvTy) {
    if (!receiver->getType()->isPointerTy())
      return LogErrorV("Delegate receiver must be a pointer");
    receiver = Builder->CreateBitCast(receiver, expectedRecvTy,
                                      "delegate.recv.cast");
  }

  llvm::Value *delegateValue = llvm::UndefValue::get(structTy);
  delegateValue = Builder->CreateInsertValue(delegateValue, fnPtr, 0,
                                             "delegate.fn");
  delegateValue = Builder->CreateInsertValue(delegateValue, receiver, 1,
                                             "delegate.recv");
  return delegateValue;
}

static std::string buildDelegateWrapperKey(const llvm::Function *target,
                                           const DelegateTypeInfo &delegateInfo) {
  std::string key;
  if (target)
    key = target->getName().str();
  key += "|";
  key += formatDelegateSignature(delegateInfo, false);
  return key;
}

static llvm::Function *ensureDelegateWrapper(llvm::Function *target,
                                             const DelegateTypeInfo &delegateInfo) {
  if (!target)
    return nullptr;

  static std::map<std::string, llvm::Function *> wrapperCache;
  std::string cacheKey = buildDelegateWrapperKey(target, delegateInfo);
  auto cached = wrapperCache.find(cacheKey);
  if (cached != wrapperCache.end())
    return cached->second;

  DelegateTypeInfo *mutableInfo = lookupDelegateInfoMutable(delegateInfo.name);
  DelegateTypeInfo localInfo = delegateInfo;
  DelegateTypeInfo &info = mutableInfo ? *mutableInfo : localInfo;
  llvm::FunctionType *delegateFnType = ensureDelegateFunctionType(info);
  if (!delegateFnType)
    return nullptr;

  std::string wrapperName = "__hybrid_delegate_wrap$" +
                            sanitizeForMangle(target->getName().str()) +
                            "$" +
                            sanitizeForMangle(formatDelegateSignature(info, false));

  llvm::Function *wrapper =
      llvm::Function::Create(delegateFnType, llvm::Function::InternalLinkage,
                             wrapperName, TheModule.get());

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(*TheContext, "entry", wrapper);
  llvm::IRBuilder<> tmpBuilder(*TheContext);
  tmpBuilder.SetInsertPoint(entry);

  std::vector<llvm::Value *> callArgs;
  callArgs.reserve(wrapper->arg_size() > 0 ? wrapper->arg_size() - 1 : 0);
  auto it = wrapper->arg_begin();
  if (it != wrapper->arg_end())
    ++it; // skip receiver
  for (; it != wrapper->arg_end(); ++it)
    callArgs.push_back(&*it);

  llvm::Value *callValue = nullptr;
  if (target->getReturnType()->isVoidTy()) {
    tmpBuilder.CreateCall(target, callArgs);
    tmpBuilder.CreateRetVoid();
  } else {
    callValue = tmpBuilder.CreateCall(target, callArgs);
    tmpBuilder.CreateRet(callValue);
  }

  wrapperCache[cacheKey] = wrapper;
  return wrapper;
}

bool isDelegateFunctionReference(const VariableExprAST &var) {
  if (lookupTypeInfo(var.getName()))
    return false;
  return CG.functionOverloads.contains(var.getName());
}

static const CompositeTypeInfo *resolveMemberOwnerInfo(const MemberAccessExprAST &member,
                                                       std::string &ownerName,
                                                       bool &isTypeReference) {
  isTypeReference = false;
  if (auto *varObj = dynamic_cast<VariableExprAST *>(member.getObject())) {
    ownerName = varObj->getName();
    if (const CompositeTypeInfo *info = lookupCompositeInfo(ownerName)) {
      isTypeReference = true;
      return info;
    }
  }

  if (auto infoOpt = resolveExprTypeInfo(member.getObject())) {
    ownerName = sanitizeCompositeLookupName(typeNameFromInfo(*infoOpt));
    if (!ownerName.empty())
      return lookupCompositeInfo(ownerName);
  }

  return nullptr;
}

bool isDelegateMethodReference(const MemberAccessExprAST &member) {
  std::string ownerName;
  bool isTypeReference = false;
  const CompositeTypeInfo *info =
      resolveMemberOwnerInfo(member, ownerName, isTypeReference);
  if (!info)
    return false;
  return info->methodInfo.contains(member.getMemberName());
}

bool canBindDelegateReference(const DelegateTypeInfo &delegateInfo,
                              const ExprAST *expr) {
  if (!expr)
    return false;

  const ExprAST *coreExpr = unwrapRefExpr(expr);
  if (auto *var = dynamic_cast<const VariableExprAST *>(coreExpr)) {
    if (lookupTypeInfo(var->getName()))
      return false;
    if (var->getName().find('<') != std::string::npos)
      return false;
    FunctionOverload *match =
        selectOverloadForDelegate(var->getName(), delegateInfo, false);
    return match != nullptr;
  }

  auto *member = dynamic_cast<const MemberAccessExprAST *>(coreExpr);
  if (!member)
    return false;

  if (member->hasExplicitGenerics())
    return false;

  std::string ownerName;
  bool isTypeReference = false;
  const CompositeTypeInfo *ownerInfo =
      resolveMemberOwnerInfo(*member, ownerName, isTypeReference);
  if (!ownerInfo)
    return false;

  auto methodIt = ownerInfo->methodInfo.find(member->getMemberName());
  if (methodIt == ownerInfo->methodInfo.end())
    return false;

  const CompositeMemberInfo &memberInfo = methodIt->second;
  bool isStaticMethod =
      static_cast<uint8_t>(memberInfo.modifiers.storage & StorageFlag::Static) != 0;
  size_t paramOffset = isStaticMethod ? 0 : 1;
  if (!delegateSignatureMatches(delegateInfo, memberInfo.returnType,
                                memberInfo.returnsByRef,
                                memberInfo.parameterTypes,
                                memberInfo.parameterIsRef,
                                memberInfo.parameterIsParams, paramOffset)) {
    return false;
  }
  if (!isStaticMethod && isTypeReference)
    return false;
  if (ownerInfo->kind == AggregateKind::Interface && !isStaticMethod)
    return false;
  return true;
}

llvm::Value *emitDelegateValueForTarget(
    const DelegateTypeInfo &delegateInfo, ExprAST *expr,
    std::string_view contextDescription, bool &handled) {
  handled = false;
  if (!expr)
    return nullptr;
  (void)contextDescription;

  ExprAST *coreExpr = const_cast<ExprAST *>(unwrapRefExpr(expr));
  if (auto *var = dynamic_cast<VariableExprAST *>(coreExpr)) {
    if (const TypeInfo *sym = lookupTypeInfo(var->getName())) {
      if (lookupDelegateInfo(*sym)) {
        handled = true;
        return expr->codegen();
      }
      return nullptr;
    }

    handled = true;
    if (var->getName().find('<') != std::string::npos) {
      reportCompilerError("Generic function references are not supported for delegates");
      return nullptr;
    }

    FunctionOverload *match =
        selectOverloadForDelegate(var->getName(), delegateInfo, true);
    if (!match)
      return nullptr;

    llvm::Function *targetFn = match->function;
    if (!targetFn)
      targetFn = TheModule->getFunction(match->mangledName);
    if (!targetFn) {
      reportCompilerError("Internal error: missing function for delegate target '" +
                          var->getName() + "'");
      return nullptr;
    }

    llvm::Function *wrapper = ensureDelegateWrapper(targetFn, delegateInfo);
    if (!wrapper)
      return nullptr;
    llvm::Value *receiver = llvm::ConstantPointerNull::get(pointerType());
    return buildDelegateValue(delegateInfo, wrapper, receiver);
  }

  auto *member = dynamic_cast<MemberAccessExprAST *>(coreExpr);
  if (!member)
    return nullptr;

  if (!isDelegateMethodReference(*member))
    return nullptr;

  handled = true;
  if (member->hasExplicitGenerics()) {
    reportCompilerError("Explicit method type arguments are not supported for delegates");
    return nullptr;
  }

  std::string ownerName;
  bool isTypeReference = false;
  const CompositeTypeInfo *ownerInfo =
      resolveMemberOwnerInfo(*member, ownerName, isTypeReference);
  if (!ownerInfo) {
    reportCompilerError("Unable to resolve type for delegate target '" +
                        member->getMemberName() + "'");
    return nullptr;
  }

  auto methodIt = ownerInfo->methodInfo.find(member->getMemberName());
  if (methodIt == ownerInfo->methodInfo.end()) {
    reportCompilerError("Member '" + member->getMemberName() +
                        "' of type '" + ownerName +
                        "' is not a method");
    return nullptr;
  }

  const CompositeMemberInfo &memberInfo = methodIt->second;
  if (!ensureMemberAccessAllowed(memberInfo.modifiers, AccessIntent::Call,
                                 ownerName, member->getMemberName()))
    return nullptr;

  bool isStaticMethod =
      static_cast<uint8_t>(memberInfo.modifiers.storage & StorageFlag::Static) != 0;
  size_t paramOffset = isStaticMethod ? 0 : 1;

  if (!delegateSignatureMatches(delegateInfo, memberInfo.returnType,
                                memberInfo.returnsByRef,
                                memberInfo.parameterTypes,
                                memberInfo.parameterIsRef,
                                memberInfo.parameterIsParams, paramOffset)) {
    reportCompilerError("Method '" + memberInfo.signature +
                        "' does not match delegate '" + delegateInfo.name + "'");
    return nullptr;
  }

  if (!isStaticMethod && isTypeReference) {
    reportCompilerError("Instance method '" + memberInfo.signature +
                        "' requires an instance receiver");
    return nullptr;
  }

  llvm::Function *targetFn = memberInfo.directFunction;
  if (!targetFn && !memberInfo.mangledName.empty())
    targetFn = TheModule->getFunction(memberInfo.mangledName);
  if (!targetFn) {
    reportCompilerError("Method '" + memberInfo.signature +
                        "' is unavailable for delegate binding");
    return nullptr;
  }

  if (ownerInfo->kind == AggregateKind::Interface && !isStaticMethod) {
    reportCompilerError("Cannot bind interface method '" + memberInfo.signature +
                        "' to a delegate");
    return nullptr;
  }

  if (isStaticMethod) {
    llvm::Function *wrapper = ensureDelegateWrapper(targetFn, delegateInfo);
    if (!wrapper)
      return nullptr;
    llvm::Value *receiver = llvm::ConstantPointerNull::get(pointerType());
    return buildDelegateValue(delegateInfo, wrapper, receiver);
  }

  if (expressionIsNullable(member->getObject())) {
    reportCompilerError("Cannot bind nullable receiver to delegate target '" +
                        memberInfo.signature + "'",
                        "Ensure the receiver is non-null before creating the delegate.");
    return nullptr;
  }

  llvm::Value *instancePtr = member->getObject()->codegen_ptr();
  llvm::Value *instanceValue = nullptr;
  if (!instancePtr) {
    instanceValue = member->getObject()->codegen();
    if (!instanceValue)
      return nullptr;
    if (instanceValue->getType()->isPointerTy()) {
      instancePtr = instanceValue;
    } else {
      llvm::AllocaInst *Tmp =
          Builder->CreateAlloca(instanceValue->getType(), nullptr,
                                "delegate.recv");
      Builder->CreateStore(instanceValue, Tmp);
      instancePtr = Tmp;
    }
  }

  return buildDelegateValue(delegateInfo, targetFn, instancePtr);
}

const DelegateTypeInfo *resolveDelegateInfoForExpr(const ExprAST *expr) {
  if (!expr)
    return nullptr;
  if (auto infoOpt = resolveExprTypeInfo(expr))
    return lookupDelegateInfo(*infoOpt);
  std::string typeName = expr->getTypeName();
  if (!typeName.empty())
    return lookupDelegateInfo(typeName);
  return nullptr;
}

llvm::Value *emitDelegateCall(CallExprAST &call, llvm::Value *calleeValue,
                              const DelegateTypeInfo &delegateInfo,
                              ExprAST *calleeExpr) {
  if (!calleeValue)
    return nullptr;

  if (calleeExpr && expressionIsNullable(calleeExpr)) {
    reportCompilerError("Cannot call nullable delegate '" + delegateInfo.name +
                        "' without a null check",
                        "Guard the call with 'if <delegate> != null'.");
    return nullptr;
  }

  DelegateTypeInfo *mutableInfo = lookupDelegateInfoMutable(delegateInfo.name);
  DelegateTypeInfo localInfo = delegateInfo;
  DelegateTypeInfo &info = mutableInfo ? *mutableInfo : localInfo;
  llvm::StructType *structTy = ensureDelegateStructType(info);
  llvm::FunctionType *fnType = ensureDelegateFunctionType(info);
  if (!structTy || !fnType)
    return nullptr;

  llvm::Value *delegateValue = calleeValue;
  if (delegateValue->getType()->isPointerTy()) {
    delegateValue =
        Builder->CreateLoad(structTy, delegateValue, "delegate.load");
  } else if (delegateValue->getType() != structTy) {
    delegateValue = castToType(delegateValue, structTy, info.name);
  }
  if (!delegateValue)
    return nullptr;

  llvm::Value *fnPtr =
      Builder->CreateExtractValue(delegateValue, 0, "delegate.fn");
  llvm::Value *receiver =
      Builder->CreateExtractValue(delegateValue, 1, "delegate.recv");
  llvm::Value *fnPtrCast =
      Builder->CreateBitCast(fnPtr, pointerType(fnType), "delegate.fn.cast");

  std::vector<ProvidedArgument> provided;
  provided.reserve(call.getArgs().size());
  for (size_t i = 0; i < call.getArgs().size(); ++i) {
    ProvidedArgument arg;
    arg.expr = call.getArgs()[i].get();
    arg.isRef = dynamic_cast<const RefExprAST *>(arg.expr) != nullptr;
    if (i < call.getArgNames().size())
      arg.name = call.getArgNames()[i];
    if (i < call.getArgNameLocations().size())
      arg.nameLoc = call.getArgNameLocations()[i];
    if (i < call.getArgEqualsLocations().size())
      arg.equalsLoc = call.getArgEqualsLocations()[i];

    const ExprAST *coreArg = unwrapRefExpr(arg.expr);
    bool deferred = false;
    if (auto *var = dynamic_cast<const VariableExprAST *>(coreArg)) {
      if (isDelegateFunctionReference(*var))
        deferred = true;
    } else if (auto *member = dynamic_cast<const MemberAccessExprAST *>(coreArg)) {
      if (isDelegateMethodReference(*member))
        deferred = true;
    }

    if (deferred) {
      arg.value = nullptr;
      provided.push_back(std::move(arg));
      continue;
    }

    llvm::Value *value = call.getArgs()[i]->codegen();
    if (!value)
      return nullptr;
    arg.value = value;
    provided.push_back(std::move(arg));
  }

  auto findParamsIndex = [](const std::vector<bool> &flags) -> int {
    for (size_t i = 0; i < flags.size(); ++i) {
      if (flags[i])
        return static_cast<int>(i);
    }
    return -1;
  };

  const bool hasNamedArguments = std::ranges::any_of(
      provided, [](const ProvidedArgument &arg) { return !arg.name.empty(); });

  if (hasNamedArguments) {
    std::set<std::string> seenNames;
    for (const auto &arg : provided) {
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
    for (const auto &name : info.parameterNames) {
      if (!name.empty())
        knownNames.insert(name);
    }
    for (const auto &arg : provided) {
      if (arg.name.empty())
        continue;
      if (!knownNames.contains(arg.name)) {
        ScopedErrorLocation scoped(arg.nameLoc);
        reportCompilerError("Unknown parameter name '" + arg.name +
                            "' for delegate '" + info.name + "'");
        return nullptr;
      }
    }
  }

  const size_t paramCount = info.parameterTypes.size();
  std::vector<int> binding(paramCount, -1);
  std::vector<int> paramsBinding;
  const int paramsIndex = findParamsIndex(info.parameterIsParams);
  size_t nextPositional = 0;
  bool failed = false;
  bool paramsNamed = false;

  for (size_t i = 0; i < provided.size(); ++i) {
    const auto &arg = provided[i];
    if (arg.name.empty()) {
      while (nextPositional < paramCount && binding[nextPositional] != -1)
        ++nextPositional;
      if (paramsIndex >= 0 &&
          nextPositional == static_cast<size_t>(paramsIndex)) {
        if (paramsNamed) {
          failed = true;
          break;
        }
        for (size_t j = i; j < provided.size(); ++j) {
          if (!provided[j].name.empty()) {
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
        break;
      }
      binding[nextPositional] = static_cast<int>(i);
      ++nextPositional;
      continue;
    }

    auto nameIt = std::find(info.parameterNames.begin(),
                            info.parameterNames.end(), arg.name);
    if (nameIt == info.parameterNames.end()) {
      failed = true;
      break;
    }
    size_t paramIndex =
        static_cast<size_t>(nameIt - info.parameterNames.begin());
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

  if (failed) {
    reportCompilerError("Invalid argument binding for delegate '" +
                        info.name + "'");
    return nullptr;
  }

  for (size_t idx = 0; idx < paramCount; ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex)) {
      continue;
    }
    if (binding[idx] != -1)
      continue;
    const bool hasDefault =
        idx < info.parameterDefaults.size() &&
        info.parameterDefaults[idx].isSet();
    if (!hasDefault) {
      reportCompilerError(
          "Missing argument for parameter '" +
              (idx < info.parameterNames.size()
                   ? info.parameterNames[idx]
                   : std::to_string(idx)) +
              "' in call to delegate '" + info.name + "'");
      return nullptr;
    }
  }

  auto checkCompatibility = [&](const ExprAST *argExpr,
                                llvm::Value *argValue,
                                const TypeInfo &expectedInfo,
                                llvm::Type *expectedType) -> bool {
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

    llvm::Type *actualType = argValue->getType();
    if (actualType == expectedType)
      return true;
    if (actualType && expectedType && actualType->isPointerTy() &&
        expectedType->isPointerTy())
      return true;
    if (actualType && expectedType &&
        areTypesCompatible(actualType, expectedType))
      return true;
    return false;
  };

  for (size_t idx = 0; idx < paramCount; ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex))
      continue;
    bool argIsRef = binding[idx] >= 0
                        ? provided[static_cast<size_t>(binding[idx])].isRef
                        : info.parameterIsRef[idx];
    if (info.parameterIsRef[idx] != argIsRef) {
      reportCompilerError("Argument for parameter '" +
                          (idx < info.parameterNames.size()
                               ? info.parameterNames[idx]
                               : std::to_string(idx)) +
                          "' must match ref usage");
      return nullptr;
    }
    if (binding[idx] >= 0) {
      const auto &arg = provided[static_cast<size_t>(binding[idx])];
      llvm::Type *expectedType =
          fnType->getParamType(idx + 1);
      if (!checkCompatibility(arg.expr, arg.value,
                              info.parameterTypes[idx], expectedType)) {
        reportCompilerError("Argument for parameter '" +
                            (idx < info.parameterNames.size()
                                 ? info.parameterNames[idx]
                                 : std::to_string(idx)) +
                            "' is incompatible with delegate '" +
                            info.name + "'");
        return nullptr;
      }
    }
  }

  bool paramsDirect = false;
  if (paramsIndex >= 0 && !paramsBinding.empty()) {
    TypeInfo arrayInfo = info.parameterTypes[static_cast<size_t>(paramsIndex)];
    llvm::Type *expectedArrayType = fnType->getParamType(paramsIndex + 1);
    bool directAllowed = paramsBinding.size() == 1;
    if (directAllowed) {
      const auto &arg = provided[static_cast<size_t>(paramsBinding.front())];
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
      paramsDirect = true;
    } else {
      auto elementInfo = extractElementTypeInfo(arrayInfo);
      if (!elementInfo) {
        reportCompilerError("Unable to determine params element type for delegate '" +
                            info.name + "'");
        return nullptr;
      }
      std::string elementTypeName = typeNameFromInfo(*elementInfo);
      llvm::Type *expectedElementType = getTypeFromString(elementTypeName);
      if (!expectedElementType) {
        reportCompilerError("Unknown params element type '" + elementTypeName +
                            "' in delegate '" + info.name + "'");
        return nullptr;
      }
      for (int argIndex : paramsBinding) {
        const auto &arg = provided[static_cast<size_t>(argIndex)];
        if (arg.isRef) {
          reportCompilerError("params arguments cannot be passed by ref");
          return nullptr;
        }
        if (!checkCompatibility(arg.expr, arg.value, *elementInfo,
                                expectedElementType)) {
          reportCompilerError("params argument is incompatible with delegate '" +
                              info.name + "'");
          return nullptr;
        }
      }
    }
  }

  auto ensureDelegateBindingValue = [&](const DelegateTypeInfo &delegateInfo,
                                        int bindingIndex,
                                        std::string_view usageLabel) -> bool {
    if (bindingIndex < 0 ||
        static_cast<size_t>(bindingIndex) >= provided.size())
      return true;
    ProvidedArgument &arg = provided[static_cast<size_t>(bindingIndex)];
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

  if (paramsIndex >= 0 && !paramsDirect &&
      paramsIndex < static_cast<int>(info.parameterTypes.size())) {
    TypeInfo paramsInfo = info.parameterTypes[static_cast<size_t>(paramsIndex)];
    finalizeTypeInfoMetadata(paramsInfo);
    if (auto elementInfoOpt = extractElementTypeInfo(paramsInfo)) {
      if (const DelegateTypeInfo *delegateInfo =
              lookupDelegateInfo(*elementInfoOpt)) {
        for (int bindingIndex : paramsBinding) {
          if (!ensureDelegateBindingValue(*delegateInfo, bindingIndex,
                                          "params argument"))
            return nullptr;
        }
      }
    }
  }

  for (size_t idx = 0; idx < binding.size(); ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex))
      continue;
    int bindingIndex = binding[idx];
    if (bindingIndex < 0)
      continue;
    if (const DelegateTypeInfo *delegateInfo =
            lookupDelegateInfo(info.parameterTypes[idx])) {
      if (!ensureDelegateBindingValue(*delegateInfo, bindingIndex, "argument"))
        return nullptr;
    }
  }

  std::vector<llvm::Value *> CallArgs;
  CallArgs.reserve(binding.size());
  std::vector<std::unique_ptr<ExprAST>> ownedDefaultExprs;

  for (size_t idx = 0; idx < binding.size(); ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex)) {
      if (paramsDirect) {
        const auto &arg =
            provided[static_cast<size_t>(paramsBinding.front())];
        CallArgs.push_back(arg.value);
      } else {
        llvm::Value *packed =
            emitPackedParamsArray(paramsBinding, provided,
                                  info.parameterTypes[idx], info.name);
        if (!packed)
          return nullptr;
        CallArgs.push_back(packed);
      }
      continue;
    }
    int bindingIndex = binding[idx];
    if (bindingIndex >= 0) {
      const auto &arg = provided[static_cast<size_t>(bindingIndex)];
      CallArgs.push_back(arg.value);
    } else {
      ScopedErrorLocation scoped(
          idx < info.parameterDefaultLocations.size()
              ? info.parameterDefaultLocations[idx]
              : SourceLocation{});
      std::unique_ptr<ExprAST> defaultExpr =
          instantiateDefaultExpr(idx < info.parameterDefaults.size()
                                     ? info.parameterDefaults[idx]
                                     : DefaultArgInfo{});
      if (!defaultExpr) {
        reportCompilerError("Default value unavailable for parameter '" +
                            (idx < info.parameterNames.size()
                                 ? info.parameterNames[idx]
                                 : std::to_string(idx)) +
                            "'");
        return nullptr;
      }
      std::string targetTypeName =
          typeNameFromInfo(info.parameterTypes[idx]);
      defaultExpr->setTypeName(targetTypeName);
      defaultExpr->markTemporary();
      llvm::Value *value = defaultExpr->codegen();
      if (!value)
        return nullptr;
      CallArgs.push_back(value);
      ownedDefaultExprs.push_back(std::move(defaultExpr));
    }
  }

  std::vector<llvm::Value *> FinalArgs;
  FinalArgs.reserve(CallArgs.size() + 1);
  llvm::Value *receiverArg = receiver;
  llvm::Type *expectedReceiverType = fnType->getParamType(0);
  if (receiverArg->getType() != expectedReceiverType)
    receiverArg = Builder->CreateBitCast(receiverArg, expectedReceiverType,
                                         "delegate.recv.cast");
  FinalArgs.push_back(receiverArg);

  for (size_t idx = 0; idx < CallArgs.size(); ++idx) {
    llvm::Value *ArgVal = CallArgs[idx];
    llvm::Type *ExpectedType =
        fnType->getParamType(idx + 1);
    if (info.parameterIsRef[idx]) {
      if (ExpectedType && ExpectedType->isPointerTy() &&
          !ArgVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = Builder->CreateAlloca(
            ArgVal->getType(), nullptr, "delegate.ref.arg");
        Builder->CreateStore(ArgVal, tmp);
        ArgVal = tmp;
      }
      if (ExpectedType && ExpectedType->isPointerTy() &&
          ArgVal->getType() != ExpectedType) {
        ArgVal = Builder->CreateBitCast(
            ArgVal, ExpectedType, "delegate.ref.cast");
      }
    } else {
      const std::string targetTypeName =
          typeNameFromInfo(info.parameterTypes[idx]);
      ArgVal = castToType(ArgVal, ExpectedType, targetTypeName);
    }
    FinalArgs.push_back(ArgVal);
  }

  llvm::Value *CallValue = nullptr;
  if (fnType->getReturnType()->isVoidTy()) {
    CallValue = Builder->CreateCall(fnType, fnPtrCast, FinalArgs);
    call.setTypeName("void");
  } else {
    CallValue =
        Builder->CreateCall(fnType, fnPtrCast, FinalArgs, "delegate.call");
    call.setTypeInfo(info.returnType);
    if (info.returnsByRef) {
      llvm::Type *valueType =
          getTypeFromString(typeNameFromInfo(info.returnType));
      if (!valueType)
        return LogErrorV("Unable to determine delegate ref return type");
      CallValue = Builder->CreateLoad(valueType, CallValue,
                                      "delegate.refload");
    }
  }

  return CallValue;
}

void markStaticFieldInitialized(const std::string &owner, const std::string &member) {
  currentCodegen().initializedStaticFields.insert(makeMemberKey(owner, member));
}

void noteMemberAssignment(const std::string &ownerName,
                          const std::string &memberName,
                          bool isStatic) {
  if (isStatic) {
    markStaticFieldInitialized(ownerName, memberName);
    return;
  }

  if (auto *ctx = currentCompositeContextMutable();
      ctx && ctx->name == ownerName && ctx->kind == MethodKind::Constructor) {
    ctx->initializedInstanceFields.insert(memberName);
  }
}

bool hasParameterlessConstructor(const std::string &typeName) {
  auto tryLookup = [&](const std::string &lookupName) -> bool {
    auto it = CG.functionOverloads.find(lookupName);
    if (it == CG.functionOverloads.end())
      return false;
    return std::ranges::any_of(it->second, [](const FunctionOverload &overload) {
      return overload.parameterTypes.empty();
    });
  };

  if (tryLookup(typeName))
    return true;

  std::string base = baseCompositeName(typeName);
  if (!base.empty() && base != typeName)
    return tryLookup(base);

  return false;
}

llvm::Value *emitBaseConstructorInitialization(
    const std::string &typeKey, llvm::StructType *structTy,
    llvm::AllocaInst *structPtr, const CompositeTypeInfo &metadata,
    const std::vector<std::unique_ptr<ExprAST>> &args,
    std::optional<std::string> explicitTarget) {
  if (!structTy || !structPtr)
    return nullptr;

  if (!metadata.baseClass) {
    reportCompilerError("Constructor for '" + typeKey +
                        "' does not declare a base class to initialize");
    return nullptr;
  }

  if (explicitTarget && *explicitTarget != *metadata.baseClass) {
    reportCompilerError("Constructor initializer targets '" + *explicitTarget +
                        "' but '" + typeKey + "' inherits '" +
                        *metadata.baseClass + "'");
    return nullptr;
  }

  ActiveCompositeContext *ctx = currentCompositeContextMutable();
  const bool trackInvocation =
      ctx && ctx->name == typeKey && ctx->kind == MethodKind::Constructor &&
      !ctx->isStatic;
  if (trackInvocation) {
    ctx->baseConstructorRequired = true;
    ctx->baseClassName = metadata.baseClass;
    if (ctx->baseConstructorInvoked) {
      reportCompilerError("Constructor for " +
                          describeAggregateKind(metadata.kind) + " '" +
                          typeKey + "' cannot invoke the base constructor more than once",
                          "Remove the duplicate base(...) call.");
      return nullptr;
    }
  }

  if (structPtr->getAllocatedType() != structTy) {
    reportCompilerError("Internal error: constructor 'this' pointer type mismatch for '" +
                        typeKey + "'");
    return nullptr;
  }

  auto baseStructIt = StructTypes.find(*metadata.baseClass);
  if (baseStructIt == StructTypes.end()) {
    reportCompilerError("Base class '" + *metadata.baseClass +
                        "' is not available for constructor chaining in '" +
                        typeKey + "'");
    return nullptr;
  }

  std::vector<bool> argIsRef;
  argIsRef.reserve(args.size());
  std::vector<llvm::Value *> argValues;
  argValues.reserve(args.size());
  for (const auto &argExpr : args) {
    bool isRef = dynamic_cast<RefExprAST *>(argExpr.get()) != nullptr;
    argIsRef.push_back(isRef);
    llvm::Value *value = argExpr->codegen();
    if (!value)
      return nullptr;
    argValues.push_back(value);
  }

  llvm::Value *baseValue = emitResolvedCallInternal(
      *metadata.baseClass, std::move(argValues), argIsRef, &args,
      /*preferGeneric=*/false, nullptr, nullptr);
  if (!baseValue)
    return nullptr;

  llvm::StructType *baseStructTy = baseStructIt->second;
  if (baseValue->getType()->isPointerTy()) {
    baseValue =
        Builder->CreateLoad(baseStructTy, baseValue, "ctor.base.value");
  } else if (baseValue->getType() != baseStructTy) {
    baseValue =
        Builder->CreateBitCast(baseValue, baseStructTy, "ctor.base.cast");
  }

  llvm::Value *basePtr = Builder->CreateBitCast(
      structPtr, pointerType(baseStructTy), "ctor.base.ptr");
  Builder->CreateStore(baseValue, basePtr);
  if (const CompositeTypeInfo *baseInfo =
          lookupCompositeInfo(*metadata.baseClass)) {
    for (const auto &fieldEntry : baseInfo->fieldTypes)
      noteMemberAssignment(typeKey, fieldEntry.first, false);
  }

  if (trackInvocation)
    ctx->baseConstructorInvoked = true;

  return baseValue;
}

bool emitConstructorInitializers(const std::string &typeKey,
                                 llvm::StructType *structTy,
                                 llvm::AllocaInst *structPtr,
                                 const CompositeTypeInfo &metadata,
                                 const std::vector<ConstructorInitializer> &initializers) {
  if (initializers.empty())
    return true;
  if (!structTy || !structPtr)
    return false;

  std::set<std::string> seenFields;
  for (auto &init : initializers) {
    if (init.kind == ConstructorInitializer::Kind::Base) {
      if (!emitBaseConstructorInitialization(typeKey, structTy, structPtr,
                                             metadata, init.arguments,
                                             init.target)) {
        return false;
      }
      continue;
    }

    auto staticIt = metadata.staticFieldModifiers.find(init.target);
    if (staticIt != metadata.staticFieldModifiers.end()) {
      reportCompilerError("Constructor initializer cannot target static field '" +
                              init.target + "' in '" + typeKey + "'");
      return false;
    }

    if (!seenFields.insert(init.target).second) {
      reportCompilerError("Constructor initializer duplicates field '" +
                          init.target + "' in '" + typeKey + "'");
      return false;
    }

    auto fieldTypeIt = metadata.fieldTypes.find(init.target);
    if (fieldTypeIt == metadata.fieldTypes.end()) {
      reportCompilerError("Unknown field '" + init.target +
                          "' in constructor initializer for '" + typeKey + "'");
      return false;
    }

    auto indicesIt = StructFieldIndices.find(typeKey);
    if (indicesIt == StructFieldIndices.end()) {
      reportCompilerError("Internal error: missing field layout for '" +
                          typeKey + "'");
      return false;
    }
    std::optional<unsigned> fieldIndex;
    for (const auto &entry : indicesIt->second) {
      if (entry.first == init.target) {
        fieldIndex = entry.second;
        break;
      }
    }
    if (!fieldIndex) {
      reportCompilerError("Field '" + init.target +
                          "' is not an instance member of '" + typeKey + "'");
      return false;
    }

    llvm::Type *fieldLLVMType = structTy->getElementType(*fieldIndex);
    llvm::Value *fieldPtr = Builder->CreateStructGEP(
        structTy, structPtr, *fieldIndex, init.target + ".ctor.ptr");
    if (init.arguments.empty() || !init.arguments.front()) {
      reportCompilerError("Field initializer for '" + init.target +
                          "' requires a value");
      return false;
    }
    llvm::Value *fieldValue = init.arguments.front()->codegen();
    if (!fieldValue)
      return false;
    const bool fieldIsTemporary = init.arguments.front()->isTemporary();

    const std::string &fieldTypeName = fieldTypeIt->second;
    if (fieldLLVMType && fieldValue->getType() != fieldLLVMType)
      fieldValue = castToType(fieldValue, fieldLLVMType, fieldTypeName);

    TypeInfo fieldInfo = makeTypeInfo(fieldTypeName);
    finalizeTypeInfoMetadata(fieldInfo);
    emitManagedStore(fieldPtr, fieldValue, fieldInfo,
                     typeKey + "." + init.target + ".ctorinit",
                     fieldIsTemporary);
    noteMemberAssignment(typeKey, init.target, false);
  }

  return true;
}

bool emitCompositeDealloc(const std::string &typeKey,
                          llvm::StructType *structTy,
                          CompositeTypeInfo &metadata) {
  if (!structTy)
    return true;
  if (!metadata.hasARCHeader)
    return true;

  auto *voidTy = llvm::Type::getVoidTy(*TheContext);
  auto *opaquePtrTy = pointerType();
  llvm::FunctionType *fnTy =
      llvm::FunctionType::get(voidTy, {opaquePtrTy}, false);
  std::string symbol = makeRuntimeSymbolName("__hybrid_dealloc$", typeKey);
  llvm::Function *fn = TheModule->getFunction(symbol);
  if (fn) {
    if (!fn->isDeclaration()) {
      metadata.deallocFunctionName = symbol;
      return true;
    }
    if (fn->getFunctionType() != fnTy) {
      reportCompilerError("Internal error: dealloc prototype mismatch for '" +
                          typeKey + "'");
      return false;
    }
  } else {
    fn = llvm::Function::Create(fnTy, llvm::Function::ExternalLinkage, symbol,
                                TheModule.get());
  }
  metadata.deallocFunctionName = symbol;

  llvm::BasicBlock *entry = llvm::BasicBlock::Create(*TheContext, "entry", fn);
  auto *savedBlock = Builder->GetInsertBlock();
  llvm::BasicBlock::iterator savedPoint;
  bool hadInsertPoint = savedBlock != nullptr;
  if (hadInsertPoint)
    savedPoint = Builder->GetInsertPoint();

  llvm::BasicBlock *bodyBB =
      llvm::BasicBlock::Create(*TheContext, "dealloc.body", fn);
  llvm::BasicBlock *retBB =
      llvm::BasicBlock::Create(*TheContext, "dealloc.ret", fn);

  Builder->SetInsertPoint(entry);
  llvm::Argument &rawObject = *fn->arg_begin();
  rawObject.setName("object");
  llvm::Value *isNull = Builder->CreateICmpEQ(
      &rawObject, llvm::ConstantPointerNull::get(opaquePtrTy),
      "dealloc.isnull");
  Builder->CreateCondBr(isNull, retBB, bodyBB);

  auto restoreInsertPoint = [&]() {
    if (hadInsertPoint)
      Builder->SetInsertPoint(savedBlock, savedPoint);
    else
      Builder->ClearInsertionPoint();
  };

  Builder->SetInsertPoint(bodyBB);
  llvm::Value *typedPtr = Builder->CreateBitCast(
      &rawObject, pointerType(structTy), "dealloc.typed");

  if (metadata.hasDestructor) {
    auto callDestructor = [&](llvm::Function *fn, llvm::Value *target) {
      if (!fn->arg_empty()) {
        llvm::Type *expected = fn->getFunctionType()->getParamType(0);
        if (target->getType() != expected)
          target = Builder->CreateBitCast(
              target, expected, "dealloc.dtor.this");
        Builder->CreateCall(fn, {target});
      } else {
        Builder->CreateCall(fn, {});
      }
    };

    llvm::Value *descriptor = nullptr;
    llvm::Value *descriptorMatches = nullptr;
    if (metadata.kind == AggregateKind::Class) {
      llvm::StructType *headerTy = getArcHeaderType();
      llvm::Value *headerPtr = Builder->CreateStructGEP(
          structTy, typedPtr, metadata.headerFieldIndex, "dealloc.header");
      llvm::Value *descAddr = Builder->CreateStructGEP(
          headerTy, headerPtr, 2, "dealloc.desc.addr");
      descriptor = Builder->CreateLoad(
          pointerType(getTypeDescriptorType()), descAddr, "dealloc.desc");
      if (metadata.descriptorGlobalName.empty()) {
        reportCompilerError("Internal error: missing descriptor symbol for '" +
                            typeKey + "' during destructor dispatch");
        restoreInsertPoint();
        return false;
      }
      llvm::GlobalVariable *selfDescriptorGV =
          TheModule->getGlobalVariable(metadata.descriptorGlobalName, true);
      if (!selfDescriptorGV) {
        reportCompilerError("Internal error: descriptor '" +
                            metadata.descriptorGlobalName +
                            "' missing while emitting dealloc for '" + typeKey +
                            "'");
        restoreInsertPoint();
        return false;
      }
      llvm::Value *selfDescriptor = llvm::ConstantExpr::getBitCast(
          selfDescriptorGV, pointerType(getTypeDescriptorType()));
      descriptorMatches = Builder->CreateICmpEQ(
          descriptor, selfDescriptor, "dealloc.is_exact");
    }

    auto emitDestructorCall = [&]() {
      if (metadata.kind == AggregateKind::Class &&
          metadata.destructorVtableSlot !=
              std::numeric_limits<unsigned>::max()) {
        if (!descriptor) {
          reportCompilerError("Internal error: descriptor unavailable during destructor dispatch for '" +
                              typeKey + "'");
          return false;
        }
        llvm::Value *vtableAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), descriptor, 2, "dealloc.vtable.addr");
        llvm::Value *vtablePtr =
            Builder->CreateLoad(pointerType(), vtableAddr, "dealloc.vtable");
        llvm::Value *slotIndex = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext),
            static_cast<uint64_t>(metadata.destructorVtableSlot));
        llvm::Value *slotPtr = Builder->CreateInBoundsGEP(
            pointerType(), vtablePtr, slotIndex, "dealloc.vslot");
        llvm::Value *slotFnPtr =
            Builder->CreateLoad(pointerType(), slotPtr, "dealloc.fptr");
        llvm::Function *dtorFn =
            TheModule->getFunction(metadata.destructorFunctionName);
        if (!dtorFn) {
          reportCompilerError("Internal error: destructor '" +
                              metadata.destructorFunctionName +
                              "' not found while emitting dealloc for '" +
                              typeKey + "'");
          return false;
        }
        llvm::Value *typedFnPtr = Builder->CreateBitCast(
            slotFnPtr, pointerType(dtorFn->getFunctionType()),
            "dealloc.dtor.func");
        if (dtorFn->arg_empty()) {
          Builder->CreateCall(dtorFn->getFunctionType(), typedFnPtr, {});
        } else {
          llvm::Value *dtorThis = typedPtr;
          llvm::Type *expected = dtorFn->getFunctionType()->getParamType(0);
          if (dtorThis->getType() != expected)
            dtorThis = Builder->CreateBitCast(
                typedPtr, expected, "dealloc.dtor.this");
          Builder->CreateCall(dtorFn->getFunctionType(), typedFnPtr,
                              {dtorThis});
        }
        return true;
      }

      if (metadata.destructorFunctionName.empty()) {
        reportCompilerError("Internal error: destructor symbol missing for '" +
                            typeKey + "' during dealloc emission");
        return false;
      }
      llvm::Function *dtorFn =
          TheModule->getFunction(metadata.destructorFunctionName);
      if (!dtorFn) {
        reportCompilerError("Internal error: destructor '" +
                            metadata.destructorFunctionName +
                            "' not found while emitting dealloc for '" +
                            typeKey + "'");
        return false;
      }
      callDestructor(dtorFn, typedPtr);
      return true;
    };

    if (descriptorMatches && metadata.kind == AggregateKind::Class) {
      llvm::Function *parentFn = Builder->GetInsertBlock()->getParent();
      llvm::BasicBlock *exactBB =
          llvm::BasicBlock::Create(*TheContext, "dealloc.dtor.exact", parentFn);
      llvm::BasicBlock *chainBB =
          llvm::BasicBlock::Create(*TheContext, "dealloc.dtor.chain", parentFn);
      llvm::BasicBlock *afterBB =
          llvm::BasicBlock::Create(*TheContext, "dealloc.dtor.after", parentFn);
      Builder->CreateCondBr(descriptorMatches, exactBB, chainBB);

      Builder->SetInsertPoint(exactBB);
      if (!emitDestructorCall()) {
        restoreInsertPoint();
        return false;
      }
      Builder->CreateBr(afterBB);

      Builder->SetInsertPoint(chainBB);
      llvm::Function *dtorFn =
          TheModule->getFunction(metadata.destructorFunctionName);
      if (!dtorFn) {
        reportCompilerError("Internal error: destructor '" +
                            metadata.destructorFunctionName +
                            "' not found while emitting dealloc for '" +
                            typeKey + "'");
        restoreInsertPoint();
        return false;
      }
      callDestructor(dtorFn, typedPtr);
      Builder->CreateBr(afterBB);

      Builder->SetInsertPoint(afterBB);
    } else {
      if (!emitDestructorCall()) {
        restoreInsertPoint();
        return false;
      }
    }
  }

  auto indicesIt = StructFieldIndices.find(typeKey);
  if (indicesIt == StructFieldIndices.end()) {
    reportCompilerError("Internal error: missing layout info while emitting dealloc for '" +
                        typeKey + "'");
    restoreInsertPoint();
    return false;
  }

  auto releaseDeclaredField = [&](const InstanceFieldInfo &fieldInfo) {
    if (fieldInfo.name.empty())
      return;
    if (fieldInfo.index >= structTy->getNumElements())
      return;
    TypeInfo info = fieldInfo.type;
    finalizeTypeInfoMetadata(info);
    if (!info.requiresARC() && !info.isSmartPointer())
      return;
    llvm::Value *fieldPtr = Builder->CreateStructGEP(
        structTy, typedPtr, fieldInfo.index, fieldInfo.name + ".dtor.ptr");
    llvm::Type *elementTy = structTy->getElementType(fieldInfo.index);
    llvm::Value *fieldVal = Builder->CreateLoad(
        elementTy, fieldPtr, fieldInfo.name + ".dtor.load");
    emitArcRelease(fieldVal, info, fieldInfo.name + ".dtor.release");
  };

  std::set<std::string> inheritedFields;
  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseMeta =
            lookupCompositeInfo(*metadata.baseClass)) {
      for (const auto &entry : baseMeta->fieldTypes)
        inheritedFields.insert(entry.first);
    }
  }

  auto shouldSkipInherited = [&](const std::string &name) {
    return metadata.baseClass.has_value() && inheritedFields.count(name) > 0;
  };

  if (!metadata.instanceFields.empty()) {
    for (const auto &fieldInfo : metadata.instanceFields)
      if (!shouldSkipInherited(fieldInfo.name))
        releaseDeclaredField(fieldInfo);
  } else {
    for (const auto &fieldEntry : metadata.fieldTypes) {
      const std::string &fieldName = fieldEntry.first;
      if (shouldSkipInherited(fieldName))
        continue;
      const std::string &fieldTypeName = fieldEntry.second;
      auto fieldIndexIt = std::find_if(
          indicesIt->second.begin(), indicesIt->second.end(),
          [&](const auto &pair) { return pair.first == fieldName; });
      if (fieldIndexIt == indicesIt->second.end())
        continue;
      InstanceFieldInfo info;
      info.name = fieldName;
      info.index = fieldIndexIt->second;
      info.type = makeTypeInfo(fieldTypeName);
      finalizeTypeInfoMetadata(info.type);
      releaseDeclaredField(info);
    }
  }

  if (metadata.baseClass) {
    const CompositeTypeInfo *baseInfo =
        lookupCompositeInfo(*metadata.baseClass);
    if (!baseInfo || baseInfo->deallocFunctionName.empty()) {
      reportCompilerError("Internal error: missing dealloc helper for base class '" +
                          *metadata.baseClass + "' while emitting '" + typeKey + "'");
      restoreInsertPoint();
      return false;
    }
    llvm::Function *baseFn =
        TheModule->getFunction(baseInfo->deallocFunctionName);
    if (!baseFn) {
      reportCompilerError("Internal error: function '" +
                          baseInfo->deallocFunctionName +
                          "' missing while emitting dealloc for '" + typeKey + "'");
      restoreInsertPoint();
      return false;
    }
    Builder->CreateCall(baseFn, {&rawObject});
  } else {
    Builder->CreateCall(getHybridDeallocFunction(), {&rawObject});
  }

  Builder->CreateBr(retBB);
  Builder->SetInsertPoint(retBB);
  Builder->CreateRetVoid();
  restoreInsertPoint();
  return true;
}

static bool memberHasDeclarationInitializer(const CompositeTypeInfo &info,
                                            bool isStatic,
                                            const std::string &memberName) {
  if (isStatic) {
    return info.staticDeclarationInitializers.count(memberName) != 0;
  }
  return info.fieldDeclarationInitializers.count(memberName) != 0;
}

bool ensureMemberInitializedForMutation(MemberAccessExprAST &member) {
  std::string ownerName = resolveCompositeName(member.getObject());
  if (ownerName.empty()) {
    if (auto *var = dynamic_cast<VariableExprAST*>(member.getObject())) {
      if (lookupCompositeInfo(var->getName()))
        ownerName = var->getName();
    }
  }
  if (ownerName.empty())
    return true;

  const CompositeTypeInfo *info = lookupCompositeInfo(ownerName);
  if (!info)
    return true;

  const std::string &memberName = member.getMemberName();
  bool isStatic = false;
  if (info->staticFieldModifiers.count(memberName) != 0) {
    isStatic = true;
  } else if (info->fieldModifiers.count(memberName) == 0) {
    return true;
  }

  if (memberHasDeclarationInitializer(*info, isStatic, memberName))
    return true;

  const std::string ownerKind = describeAggregateKind(info->kind);

  if (isStatic) {
    auto key = makeMemberKey(ownerName, memberName);
    if (currentCodegen().initializedStaticFields.count(key) != 0)
      return true;

    reportCompilerError(
        "Cannot increment or otherwise modify uninitialized member '" +
            memberName + "' of " + ownerKind + " '" + ownerName + "'",
        "Initialize it at its declaration or assign it before modifying it.");
    return false;
  }

  const ActiveCompositeContext *ctx = currentCompositeContext();
  if (!ctx || ctx->name != ownerName)
    return true;
  if (ctx->kind != MethodKind::Constructor)
    return true;
  if (ctx->initializedInstanceFields.count(memberName) != 0)
    return true;

  reportCompilerError(
      "Cannot increment or otherwise modify uninitialized member '" +
          memberName + "' of " + ownerKind + " '" + ownerName + "'",
      "Initialize it at its declaration or assign it in every constructor before modifying it.");
  return false;
}

bool shouldBypassPropertyAccess(const std::string &ownerName,
                                const std::string &memberName) {
  const ActiveCompositeContext *ctx = currentCompositeContext();
  if (!ctx || !ctx->isPropertyAccessor || !ctx->activePropertyName)
    return false;
  if (*ctx->activePropertyName != memberName)
    return false;
  if (!ownerName.empty() && !ctx->name.empty() && ctx->name != ownerName)
    return false;
  return true;
}


std::optional<std::string>
resolveStaticFieldOwnerInCurrentContext(const std::string &memberName) {
  const ActiveCompositeContext *ctx = currentCompositeContext();
  if (!ctx)
    return std::nullopt;

  std::set<std::string> visited;
  std::string lookupName = ctx->name;

  while (!lookupName.empty() && visited.insert(lookupName).second) {
    const CompositeTypeInfo *info = lookupCompositeInfo(lookupName);
    if (!info)
      break;

    if (info->staticFieldModifiers.count(memberName) != 0)
      return lookupName;

    if (info->baseClass)
      lookupName = *info->baseClass;
    else
      break;
  }

  return std::nullopt;
}

std::optional<std::string>
resolveInstanceMemberOwnerInCurrentContext(const std::string &memberName) {
  const ActiveCompositeContext *ctx = currentCompositeContext();
  auto resolveInType = [&](const std::string &typeName)
      -> std::optional<std::string> {
    if (typeName.empty())
      return std::nullopt;

    std::set<std::string> visited;
    std::string lookupName = typeName;

    while (!lookupName.empty() && visited.insert(lookupName).second) {
      const CompositeTypeInfo *info = lookupCompositeInfo(lookupName);
      if (!info)
        break;

      if (info->fieldModifiers.count(memberName) != 0)
        return lookupName;

      auto propIt = info->properties.find(memberName);
      if (propIt != info->properties.end() && !propIt->second.isStatic)
        return lookupName;

      if (info->baseClass)
        lookupName = *info->baseClass;
      else
        break;
    }

    return std::nullopt;
  };

  if (ctx && !ctx->isStatic) {
    if (auto resolved = resolveInType(ctx->name))
      return resolved;
  }

  if (const TypeInfo *thisInfo = lookupLocalTypeInfo("this")) {
    std::string typeName = sanitizeCompositeLookupName(typeNameFromInfo(*thisInfo));
    if (auto resolved = resolveInType(typeName))
      return resolved;
  }

  return std::nullopt;
}

llvm::Constant *constantValueToLLVM(const ConstantValue &value,
                                    llvm::Type *targetType,
                                    const std::string &typeName) {
  if (targetType->isIntegerTy()) {
    unsigned bits = targetType->getIntegerBitWidth();
    if (bits == 1) {
      if (value.type != ConstantValue::BOOLEAN) {
        reportCompilerError("Static field initializer for type '" + typeName +
                            "' must be a boolean literal");
        return nullptr;
      }
      return llvm::ConstantInt::get(targetType, value.boolVal ? 1 : 0);
    }

    if (value.type == ConstantValue::FLOAT) {
      reportCompilerError("Static field initializer for type '" + typeName +
                          "' cannot use a floating-point literal");
      return nullptr;
    }

    if (value.type == ConstantValue::BOOLEAN) {
      reportCompilerError("Static field initializer for type '" + typeName +
                          "' cannot use a boolean literal");
      return nullptr;
    }

    if (value.type == ConstantValue::INTEGER) {
      llvm::APInt ap(bits, static_cast<uint64_t>(value.intVal), true);
      if (bits < 64 && ap.getSExtValue() != value.intVal) {
        reportCompilerError("Static field initializer value does not fit in type '" + typeName + "'");
        return nullptr;
      }
      return llvm::ConstantInt::get(targetType, ap);
    }

    if (value.type == ConstantValue::UNSIGNED_INTEGER) {
      llvm::APInt ap(bits, value.uintVal, false);
      if (bits < 64 && ap.getZExtValue() != value.uintVal) {
        reportCompilerError("Static field initializer value does not fit in type '" + typeName + "'");
        return nullptr;
      }
      return llvm::ConstantInt::get(targetType, ap);
    }

    reportCompilerError("Unsupported static field initializer for type '" + typeName + "'");
    return nullptr;
  }

  if (targetType->isFloatingPointTy()) {
    double numeric = 0.0;
    switch (value.type) {
    case ConstantValue::INTEGER:
      numeric = static_cast<double>(value.intVal);
      break;
    case ConstantValue::UNSIGNED_INTEGER:
      numeric = static_cast<double>(value.uintVal);
      break;
    case ConstantValue::FLOAT:
      numeric = value.floatVal;
      break;
    case ConstantValue::BOOLEAN:
      numeric = value.boolVal ? 1.0 : 0.0;
      break;
    }
    return llvm::ConstantFP::get(targetType, numeric);
  }

  reportCompilerError("Static field initializer is not supported for type '" + typeName + "'");
  return nullptr;
}

static bool hasAccessFlag(MemberAccess access, AccessFlag flag) {
  return static_cast<uint8_t>(access.flags & flag) != 0;
}

bool ClassDescriptor::isClass() const {
  return kind == AggregateKind::Class;
}

bool ClassDescriptor::derivesFrom(const std::string &candidate) const {
  if (candidate == name)
    return true;
  return std::find(inheritanceChain.begin(), inheritanceChain.end(), candidate) !=
         inheritanceChain.end();
}

bool ClassDescriptor::implementsInterface(const std::string &iface) const {
  return std::find(interfaceNames.begin(), interfaceNames.end(), iface) !=
         interfaceNames.end();
}

bool ClassDescriptor::hasPublicConstructor() const {
  return std::any_of(constructors.begin(), constructors.end(),
                     [](const Constructor &ctor) {
                       return hasAccessFlag(ctor.access, AccessFlag::ReadPublic);
                     });
}

bool ClassDescriptor::hasProtectedConstructor() const {
  return std::any_of(constructors.begin(), constructors.end(),
                     [](const Constructor &ctor) {
                       return hasAccessFlag(ctor.access, AccessFlag::ReadProtected);
                     });
}

bool ensureMemberAccessAllowed(const MemberModifiers &modifiers,
                               AccessIntent intent,
                               const std::string &ownerName,
                               const std::string &memberName) {
  const ActiveCompositeContext *ctx = currentCompositeContext();
  const bool sameType = ctx && ctx->name == ownerName;
  const bool isDerived = ctx && !sameType && isDerivedFrom(ctx->name, ownerName);
  const bool sameOrDerived = sameType || isDerived;
  const auto *info = lookupCompositeInfo(ownerName);
  const std::string ownerKind =
      info ? describeAggregateKind(info->kind) : "type";

  const MemberAccess access = modifiers.access;
  if (intent == AccessIntent::Write &&
      static_cast<uint8_t>(modifiers.storage & StorageFlag::Const) != 0) {
    bool allowedInCtor =
        sameType && ctx && ctx->kind == MethodKind::Constructor;
    if (!allowedInCtor) {
      reportCompilerError("Cannot write to const member '" + memberName +
                          "' of " + ownerKind + " '" + ownerName + "'");
      return false;
    }
  }

  auto allowRead = [&] {
    if (hasAccessFlag(access, AccessFlag::ReadPublic))
      return true;
    if (sameType && hasAccessFlag(access, AccessFlag::ReadPrivate))
      return true;
    if (sameOrDerived && hasAccessFlag(access, AccessFlag::ReadProtected))
      return true;
    return false;
  };
  auto allowWrite = [&] {
    if (hasAccessFlag(access, AccessFlag::WritePublic))
      return true;
    if (sameType && hasAccessFlag(access, AccessFlag::WritePrivate))
      return true;
    if (sameOrDerived && hasAccessFlag(access, AccessFlag::WriteProtected))
      return true;
    return false;
  };

  bool permitted = false;
  switch (intent) {
  case AccessIntent::Read:
  case AccessIntent::Call:
    permitted = allowRead();
    break;
  case AccessIntent::Write:
    permitted = allowWrite();
    break;
  }

  if (permitted)
    return true;

  std::string action;
  switch (intent) {
  case AccessIntent::Read:
    action = "read";
    break;
  case AccessIntent::Write:
    action = "write to";
    break;
  case AccessIntent::Call:
    action = "call";
    break;
  }

  std::string message;
  if (sameType) {
    message = "Cannot " + action + " member '" + memberName + "' of " +
              ownerKind + " '" + ownerName + "'";
  } else if (isDerived) {
    if (hasAccessFlag(access, AccessFlag::ReadPrivate) ||
        hasAccessFlag(access, AccessFlag::WritePrivate)) {
      message = "Cannot " + action + " private member '" + memberName +
                "' of " + ownerKind + " '" + ownerName +
                "' from a subclass";
    } else {
      message = "Cannot " + action + " member '" + memberName + "' of " +
                ownerKind + " '" + ownerName + "'";
    }
  } else {
    if (hasAccessFlag(access, AccessFlag::ReadPrivate) ||
        hasAccessFlag(access, AccessFlag::WritePrivate)) {
      message = "Cannot " + action + " private member '" + memberName +
                "' of " + ownerKind + " '" + ownerName + "'";
    } else if (hasAccessFlag(access, AccessFlag::ReadProtected) ||
               hasAccessFlag(access, AccessFlag::WriteProtected)) {
      message = "Cannot " + action + " protected member '" + memberName +
                "' of " + ownerKind + " '" + ownerName +
                "' without inheriting from it";
    } else if (intent == AccessIntent::Write &&
               hasAccessFlag(access, AccessFlag::WritePrivate)) {
      message = "Member '" + memberName + "' of " + ownerKind + " '" +
                ownerName + "' is read-only outside its definition";
    } else {
      message = "Access to member '" + memberName + "' of " + ownerKind +
                " '" + ownerName + "' is not permitted";
    }
  }

  reportCompilerError(message);
  return false;
}

bool ensureMemberReadable(const MemberModifiers &modifiers,
                          const std::string &ownerName,
                          const std::string &memberName) {
  return ensureMemberAccessAllowed(modifiers, AccessIntent::Read, ownerName,
                                   memberName);
}

class ScopedCompositeContext {
  CodegenContext &Ctx;
  bool Active = false;

public:
  ScopedCompositeContext(const std::string &name, MethodKind kind,
                         bool isStatic,
                         std::optional<std::string> activeProperty = std::nullopt)
      : Ctx(currentCodegen()) {
    ActiveCompositeContext context;
    context.name = name;
    context.kind = kind;
    context.isStatic = isStatic;
    context.isPropertyAccessor = activeProperty.has_value();
    context.activePropertyName = std::move(activeProperty);
    Ctx.compositeContextStack.push_back(std::move(context));
    Active = true;
  }

  ~ScopedCompositeContext() {
    if (!Active)
      return;

    ActiveCompositeContext context = std::move(Ctx.compositeContextStack.back());
    Ctx.compositeContextStack.pop_back();

    if (context.kind != MethodKind::Constructor || context.isStatic)
      return;

    const CompositeTypeInfo *info = lookupCompositeInfo(context.name);
    if (!info)
      return;

    const std::string ownerKind = describeAggregateKind(info->kind);

    if (context.baseConstructorRequired && !context.baseConstructorInvoked) {
      std::string baseName = context.baseClassName.value_or("base type");
      reportCompilerError(
          "Constructor for " + ownerKind + " '" + context.name +
              "' must invoke base constructor of '" + baseName + "'",
          "Add a 'base(...)' call inside the constructor body.");
    }

    std::vector<std::string> missingMembers;
    missingMembers.reserve(info->fieldTypes.size());
    for (const auto &fieldEntry : info->fieldTypes) {
      const std::string &fieldName = fieldEntry.first;
      if (info->fieldDeclarationInitializers.count(fieldName) != 0)
        continue;
      if (context.initializedInstanceFields.count(fieldName) != 0)
        continue;
      missingMembers.push_back(fieldName);
    }

    if (missingMembers.empty())
      return;

    std::string missingList;
    missingList.reserve(missingMembers.size() * 12);
    for (size_t i = 0; i < missingMembers.size(); ++i) {
      if (i != 0)
        missingList += "', '";
      missingList += missingMembers[i];
    }

    std::string message;
    if (missingMembers.size() == 1) {
      message = "Constructor for " + ownerKind + " '" + context.name +
                "' must initialize member '" + missingList + "'";
    } else {
      message = "Constructor for " + ownerKind + " '" + context.name +
                "' must initialize members '" + missingList + "'";
    }

    reportCompilerError(
        std::move(message),
        "Assign every instance field in each constructor before it returns.");
  }
};

static bool exprIsNullLiteral(const ExprAST *expr) {
  return dynamic_cast<const NullExprAST*>(expr) != nullptr;
}

const ExprAST *unwrapRefExpr(const ExprAST *expr) {
  if (const auto *Ref = dynamic_cast<const RefExprAST*>(expr))
    return Ref->getOperand();
  return expr;
}

bool typeAllowsNull(const TypeInfo &info) {
  return info.isNullable || (!info.isArray && info.pointerDepth > 0);
}

static bool typeAllowsNull(const ParsedTypeDescriptor &desc) {
  return desc.isNullable || (!desc.isArray && desc.pointerDepth > 0);
}

bool isKnownNonNull(const std::string &name);
std::optional<TypeInfo> resolveExprTypeInfo(const ExprAST *expr);

bool expressionIsNullable(const ExprAST *expr) {
  if (!expr)
    return false;
  if (exprIsNullLiteral(expr))
    return true;
  if (const auto *Binary = dynamic_cast<const BinaryExprAST *>(expr)) {
    const std::string &Op = Binary->getOp();
    if (Op == "\?\?" || Op == "\?\?=") {
      const ExprAST *rhsExpr = unwrapRefExpr(Binary->getRHS());
      if (!expressionIsNullable(rhsExpr))
        return false;
    }
  }
  if (const auto *var = dynamic_cast<const VariableExprAST *>(expr)) {
    if (isKnownNonNull(var->getName()))
      return false;
  }

  if (auto infoOpt = resolveExprTypeInfo(expr)) {
    return typeAllowsNull(*infoOpt);
  }

  std::string typeName = expr->getTypeName();
  if (typeName.empty())
    return false;
  ParsedTypeDescriptor desc = parseTypeString(typeName);
  return typeAllowsNull(desc);
}

std::optional<TypeInfo> resolveExprTypeInfo(const ExprAST *expr) {
  if (!expr)
    return std::nullopt;

  if (const TypeInfo *info = expr->getTypeInfo()) {
    TypeInfo bound = applyActiveTypeBindings(*info);
    finalizeTypeInfoMetadata(bound);
    return bound;
  }

  if (const auto *var = dynamic_cast<const VariableExprAST *>(expr)) {
    if (const TypeInfo *sym = lookupTypeInfo(var->getName())) {
      TypeInfo bound = applyActiveTypeBindings(*sym);
      finalizeTypeInfoMetadata(bound);
      return bound;
    }
  }

  std::string typeName = expr->getTypeName();
  if (typeName.empty())
    return std::nullopt;

  TypeInfo info = applyActiveTypeBindings(makeTypeInfo(typeName));
  finalizeTypeInfoMetadata(info);
  return info;
}

const CompositeTypeInfo *resolveCompositeTypeInfo(const TypeInfo &info) {
  TypeInfo bound = applyActiveTypeBindings(info);
  finalizeTypeInfoMetadata(bound);
  std::string typeName = stripNullableAnnotations(typeNameFromInfo(bound));
  if (typeName.empty())
    typeName = stripNullableAnnotations(bound.typeName);
  if (typeName.empty())
    return nullptr;

  if (const CompositeTypeInfo *meta =
          lookupCompositeInfo(typeName, /*countHit=*/false))
    return meta;
  return materializeCompositeInstantiation(bound);
}

const CompositeMemberInfo *lookupOperatorMember(const CompositeTypeInfo &info,
                                                OverloadableOperator op) {
  auto nameIt = info.operatorMethodNames.find(op);
  if (nameIt == info.operatorMethodNames.end())
    return nullptr;
  auto methodIt = info.methodInfo.find(nameIt->second);
  if (methodIt == info.methodInfo.end())
    return nullptr;
  return &methodIt->second;
}

llvm::Value *emitNullCheckValue(llvm::Value *value,
                                const TypeInfo &valueInfo,
                                std::string_view label) {
  if (!value)
    return nullptr;

  if (value->getType()->isPointerTy()) {
    auto *ptrTy = llvm::cast<llvm::PointerType>(value->getType());
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(ptrTy);
    return Builder->CreateICmpEQ(value, nullPtr,
                                 std::string(label) + ".isnull");
  }

  if (valueInfo.isArray && value->getType()->isStructTy()) {
    llvm::Value *arrayPtr =
        Builder->CreateExtractValue(value, 0, std::string(label) + ".ptr");
    if (!arrayPtr->getType()->isPointerTy())
      return llvm::ConstantInt::getFalse(*TheContext);
    auto *ptrTy = llvm::cast<llvm::PointerType>(arrayPtr->getType());
    llvm::Value *nullPtr = llvm::ConstantPointerNull::get(ptrTy);
    return Builder->CreateICmpEQ(arrayPtr, nullPtr,
                                 std::string(label) + ".isnull");
  }

  return llvm::ConstantInt::getFalse(*TheContext);
}

llvm::Value *emitTypeDescriptorMatch(llvm::Value *objectPtr,
                                     llvm::Value *targetDescriptor,
                                     TypeCheckMatchKind kind,
                                     std::string_view label) {
  if (!objectPtr || !objectPtr->getType()->isPointerTy())
    return llvm::ConstantInt::getFalse(*TheContext);

  llvm::Function *parent = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *entryBB = Builder->GetInsertBlock();
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *boolTy = llvm::Type::getInt1Ty(*TheContext);

  auto *objPtrTy = llvm::cast<llvm::PointerType>(objectPtr->getType());
  llvm::Value *nullObj = llvm::ConstantPointerNull::get(objPtrTy);
  llvm::BasicBlock *loadBB =
      llvm::BasicBlock::Create(*TheContext, std::string(label) + ".load",
                               parent);
  llvm::BasicBlock *doneBB =
      llvm::BasicBlock::Create(*TheContext, std::string(label) + ".done");

  llvm::Value *objIsNull = Builder->CreateICmpEQ(
      objectPtr, nullObj, std::string(label) + ".null");
  Builder->CreateCondBr(objIsNull, doneBB, loadBB);

  Builder->SetInsertPoint(loadBB);
  llvm::StructType *headerTy = getArcHeaderType();
  llvm::Value *headerPtr = Builder->CreateBitCast(
      objectPtr, pointerType(headerTy), std::string(label) + ".header");
  llvm::Value *descAddr = Builder->CreateStructGEP(
      headerTy, headerPtr, 2, std::string(label) + ".desc.addr");
  llvm::Value *descriptor = Builder->CreateLoad(
      typeDescPtrTy, descAddr, std::string(label) + ".desc");

  llvm::Value *matchValue = nullptr;
  llvm::BasicBlock *matchBB = nullptr;
  llvm::BasicBlock *checkBB = nullptr;

  if (kind == TypeCheckMatchKind::Interface) {
    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    llvm::Value *table = Builder->CreateCall(
        getInterfaceLookupFunction(), {descriptor, targetDescriptor},
        std::string(label) + ".iface.table");
    matchValue = Builder->CreateICmpNE(
        table, llvm::ConstantPointerNull::get(voidPtrPtrTy),
        std::string(label) + ".iface.match");
    Builder->CreateBr(doneBB);
  } else if (kind == TypeCheckMatchKind::Exact) {
    matchValue = Builder->CreateICmpEQ(
        descriptor, targetDescriptor, std::string(label) + ".match");
    Builder->CreateBr(doneBB);
  } else {
    checkBB = llvm::BasicBlock::Create(*TheContext,
                                       std::string(label) + ".check",
                                       parent);
    matchBB = llvm::BasicBlock::Create(*TheContext,
                                       std::string(label) + ".match",
                                       parent);
    llvm::BasicBlock *advanceBB = llvm::BasicBlock::Create(
        *TheContext, std::string(label) + ".advance", parent);

    Builder->CreateBr(checkBB);
    Builder->SetInsertPoint(checkBB);
    llvm::PHINode *descPhi =
        Builder->CreatePHI(typeDescPtrTy, 2, std::string(label) + ".cursor");
    descPhi->addIncoming(descriptor, loadBB);
    llvm::Value *descIsNull = Builder->CreateICmpEQ(
        descPhi, llvm::ConstantPointerNull::get(typeDescPtrTy),
        std::string(label) + ".desc.null");
    Builder->CreateCondBr(descIsNull, doneBB, matchBB);

    Builder->SetInsertPoint(matchBB);
    llvm::Value *isMatch = Builder->CreateICmpEQ(
        descPhi, targetDescriptor, std::string(label) + ".desc.match");
    Builder->CreateCondBr(isMatch, doneBB, advanceBB);

    Builder->SetInsertPoint(advanceBB);
    llvm::Value *baseAddr = Builder->CreateStructGEP(
        getTypeDescriptorType(), descPhi, 1,
        std::string(label) + ".base.addr");
    llvm::Value *baseDesc = Builder->CreateLoad(
        typeDescPtrTy, baseAddr, std::string(label) + ".base");
    Builder->CreateBr(checkBB);
    descPhi->addIncoming(baseDesc, advanceBB);
  }

  doneBB->insertInto(parent);
  Builder->SetInsertPoint(doneBB);

  if (kind == TypeCheckMatchKind::Chain) {
    llvm::PHINode *phi = Builder->CreatePHI(
        boolTy, 3, std::string(label) + ".result");
    phi->addIncoming(llvm::ConstantInt::getFalse(boolTy), entryBB);
    phi->addIncoming(llvm::ConstantInt::getFalse(boolTy), checkBB);
    phi->addIncoming(llvm::ConstantInt::getTrue(boolTy), matchBB);
    return phi;
  }

  llvm::PHINode *phi =
      Builder->CreatePHI(boolTy, 2, std::string(label) + ".result");
  phi->addIncoming(llvm::ConstantInt::getFalse(boolTy), entryBB);
  phi->addIncoming(matchValue, loadBB);
  return phi;
}

class InjectedValueExprAST : public ExprAST {
public:
  InjectedValueExprAST(llvm::Value *value, std::string typeName,
                       bool isTemporary)
      : Value(value) {
    setTypeName(typeName);
    markTemporary(isTemporary);
  }

  llvm::Value *codegen() override { return Value; }

private:
  llvm::Value *Value = nullptr;
};

bool emitTypeCheckBinding(const TypeInfo &targetInfo, const std::string &name,
                          llvm::Value *value) {
  if (!value)
    return false;

  TypeInfo boundTarget = applyActiveTypeBindings(targetInfo);
  finalizeTypeInfoMetadata(boundTarget);
  std::string typeName = typeNameFromInfo(boundTarget);
  if (typeName.empty())
    typeName = boundTarget.typeName;
  if (typeName.empty()) {
    reportCompilerError("Unable to resolve type for pattern binding '" + name + "'");
    return false;
  }

  llvm::Type *targetLLVMType = getTypeFromString(typeName);
  if (!targetLLVMType) {
    reportCompilerError("Unknown binding type '" + typeName + "' for '" + name + "'");
    return false;
  }

  llvm::Value *bindingValue = value;
  if (bindingValue->getType() != targetLLVMType)
    bindingValue = castToType(bindingValue, targetLLVMType, typeName);

  auto injected =
      std::make_unique<InjectedValueExprAST>(bindingValue, typeName, false);
  VariableDeclarationStmtAST bindingDecl(std::move(boundTarget), name,
                                         std::move(injected), false);
  return bindingDecl.codegen() != nullptr;
}

llvm::Value *emitPackedParamsArray(
    const std::vector<int> &paramIndices,
    const std::vector<ProvidedArgument> &provided,
    const TypeInfo &arrayInfo,
    std::string_view label) {
  TypeInfo arrayInfoCopy = arrayInfo;
  finalizeTypeInfoMetadata(arrayInfoCopy);

  auto elementInfoOpt = extractElementTypeInfo(arrayInfoCopy);
  if (!elementInfoOpt)
    return LogErrorV("Unknown element type in array");

  TypeInfo elementInfo = *elementInfoOpt;
  std::string elementTypeName = typeNameFromInfo(elementInfo);
  llvm::Type *elementLLVMType = getTypeFromString(elementTypeName);
  if (!elementLLVMType) {
    reportCompilerError("Unknown array element type '" + elementTypeName + "'");
    return nullptr;
  }

  const bool elementAllowsNull =
      typeAllowsNull(elementInfo) || elementInfo.elementNullable ||
      arrayInfoCopy.elementNullable;
  if (!elementAllowsNull) {
    for (int idx : paramIndices) {
      if (idx < 0 || static_cast<size_t>(idx) >= provided.size())
        continue;
      const ExprAST *expr = provided[static_cast<size_t>(idx)].expr;
      if (expressionIsNullable(unwrapRefExpr(expr))) {
        reportCompilerError(
            "Array elements of type '" + elementTypeName + "' cannot be null",
            "Remove null entries or mark the element type nullable with '?'.");
        return nullptr;
      }
    }
  }

  std::vector<std::unique_ptr<ExprAST>> elements;
  elements.reserve(paramIndices.size());
  for (int idx : paramIndices) {
    if (idx < 0 || static_cast<size_t>(idx) >= provided.size())
      return LogErrorV("Unknown element type in array");
    const auto &arg = provided[static_cast<size_t>(idx)];
    if (!arg.value)
      return LogErrorV("Unknown element type in array");
    std::string typeName = arg.expr ? arg.expr->getTypeName() : "";
    bool isTemporary = arg.expr ? arg.expr->isTemporary() : false;
    elements.push_back(std::make_unique<InjectedValueExprAST>(
        arg.value, std::move(typeName), isTemporary));
  }

  ArrayExprAST arrayExpr(elementTypeName, std::move(elements));
  (void)label;
  return arrayExpr.codegen_with_element_target(elementLLVMType,
                                               elementTypeName,
                                               &arrayInfoCopy);
}

llvm::Value *materializeAliasPointer(llvm::Value *storage,
                                            const TypeInfo &info,
                                            const std::string &name) {
  if (!storage)
    return nullptr;

  llvm::Type *expectedElement = getTypeFromString(info.typeName);
  llvm::PointerType *expectedPtrTy =
      expectedElement ? pointerType(expectedElement) : nullptr;

  auto loadPointer = [&](llvm::Value *addr,
                         llvm::Type *pointee) -> llvm::Value * {
    llvm::Value *loaded =
        Builder->CreateLoad(pointee, addr, name + "_ptr");
    if (expectedPtrTy && loaded->getType() != expectedPtrTy) {
      loaded = Builder->CreateBitCast(loaded, expectedPtrTy,
                                      name + "_ptrcast");
    }
    return loaded;
  };

  if (auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(storage)) {
    llvm::Type *pointee = alloca->getAllocatedType();
    if (pointee && pointee->isPointerTy())
      return loadPointer(storage, pointee);
  } else if (auto *global = llvm::dyn_cast<llvm::GlobalVariable>(storage)) {
    llvm::Type *pointee = global->getValueType();
    if (pointee && pointee->isPointerTy())
      return loadPointer(storage, pointee);
  }

  if (!storage->getType()->isPointerTy())
    return storage;

  if (expectedPtrTy && storage->getType() != expectedPtrTy)
    return Builder->CreateBitCast(storage, expectedPtrTy,
                                  name + "_ptrcast");
  return storage;
}

bool validateInvariantAssignment(const TypeInfo &targetInfo,
                                 const ExprAST *sourceExpr,
                                 const std::string &contextDescription) {
  if (!sourceExpr)
    return true;

  const ExprAST *coreExpr = unwrapRefExpr(sourceExpr);
  if (!coreExpr || exprIsNullLiteral(coreExpr))
    return true;

  const std::string &sourceTypeName = coreExpr->getTypeName();

  TypeInfo boundTarget = applyActiveTypeBindings(targetInfo);
  TypeInfo boundSource;

  if (const auto *var = dynamic_cast<const VariableExprAST *>(coreExpr)) {
    if (const TypeInfo *symbolInfo = lookupTypeInfo(var->getName()))
      boundSource = applyActiveTypeBindings(*symbolInfo);
  }

  if (boundSource.typeName.empty()) {
    if (sourceTypeName.empty())
      return true;
    boundSource = applyActiveTypeBindings(makeTypeInfo(sourceTypeName));
  }

  const bool targetHasGenerics = !boundTarget.typeArguments.empty();
  const bool sourceHasGenerics = !boundSource.typeArguments.empty();
  if (!targetHasGenerics && !sourceHasGenerics)
    return true;

  if (!boundTarget.baseTypeName.empty() && !boundSource.baseTypeName.empty() &&
      boundTarget.baseTypeName != boundSource.baseTypeName)
    return true;

  if (!typeInfoEquals(boundTarget, boundSource)) {
    reportCompilerError("Cannot use value of type '" +
                        typeNameFromInfo(boundSource) + "' for " +
                        contextDescription + " expecting '" +
                        typeNameFromInfo(boundTarget) + "'");
    return false;
  }

  return true;
}

bool validateTupleAssignmentCompatibility(
    const TypeInfo &targetInfo, const ExprAST *sourceExpr,
    const std::string &contextDescription) {
  const ExprAST *coreExpr = unwrapRefExpr(sourceExpr);
  if (!coreExpr || exprIsNullLiteral(coreExpr))
    return true;

  TypeInfo boundTarget = applyActiveTypeBindings(targetInfo);
  finalizeTypeInfoMetadata(boundTarget);

  auto sourceInfoOpt = resolveExprTypeInfo(coreExpr);
  if (!sourceInfoOpt)
    return true;

  TypeInfo boundSource = *sourceInfoOpt;

  const bool targetIsTuple = boundTarget.isTupleType();
  const bool sourceIsTuple = boundSource.isTupleType();

  if (targetIsTuple != sourceIsTuple) {
    if (targetIsTuple) {
      reportCompilerError("Cannot use non-tuple value for " +
                          contextDescription + " expecting '" +
                          typeNameFromInfo(boundTarget) + "'");
    } else {
      reportCompilerError("Cannot use tuple value of type '" +
                          typeNameFromInfo(boundSource) + "' for " +
                          contextDescription + " expecting '" +
                          typeNameFromInfo(boundTarget) + "'");
    }
    return false;
  }

  if (!targetIsTuple)
    return true;

  if (boundTarget.typeArguments.size() != boundSource.typeArguments.size()) {
    reportCompilerError(
        "Tuple arity mismatch for " + contextDescription + ": expected " +
        std::to_string(boundTarget.typeArguments.size()) +
        " element(s), got " +
        std::to_string(boundSource.typeArguments.size()));
    return false;
  }

  for (size_t i = 0; i < boundTarget.typeArguments.size(); ++i) {
    if (!typeInfoEquals(boundTarget.typeArguments[i],
                        boundSource.typeArguments[i])) {
      reportCompilerError(
          "Tuple element " + std::to_string(i) +
          " type mismatch for " + contextDescription + ": expected '" +
          typeNameFromInfo(boundTarget.typeArguments[i]) + "', got '" +
          typeNameFromInfo(boundSource.typeArguments[i]) + "'");
      return false;
    }
  }

  return true;
}

void propagateTypeToNewExpr(ExprAST *expr, const TypeInfo &targetInfo) {
  if (!expr)
    return;
  if (auto *ternary = dynamic_cast<TernaryExprAST *>(expr)) {
    propagateTypeToNewExpr(ternary->getThenExpr(), targetInfo);
    propagateTypeToNewExpr(ternary->getElseExpr(), targetInfo);
    return;
  }
  auto *newExpr = dynamic_cast<NewExprAST *>(expr);
  if (!newExpr || !newExpr->typeWasElided())
    return;

  std::string inferred = typeNameFromInfo(targetInfo);
  if (inferred.empty())
    inferred = targetInfo.typeName;
  if (inferred.empty())
    return;

  newExpr->setInferredType(inferred);
  newExpr->setTypeName(inferred);
}

std::unique_ptr<ParenExprAST>
convertHashShorthandToParen(UnaryExprAST &hashExpr) {
  if (hashExpr.getOp() != "#")
    return nullptr;

  std::unique_ptr<ExprAST> operand = hashExpr.takeOperand();
  if (!operand)
    return nullptr;

  bool isTuple = false;
  std::vector<std::unique_ptr<ExprAST>> elements;
  if (auto *paren = dynamic_cast<ParenExprAST *>(operand.get())) {
    isTuple = paren->isTuple();
    elements = paren->takeElements();
  } else {
    elements.push_back(std::move(operand));
  }

  return std::make_unique<ParenExprAST>(std::move(elements), isTuple);
}

bool validateNullableAssignment(const TypeInfo &targetInfo,
                                const ExprAST *expr,
                                const std::string &targetDescription) {
  if (!typeAllowsNull(targetInfo) && expressionIsNullable(expr)) {
    LogErrorV(("Cannot assign nullable value to non-nullable " + targetDescription).c_str());
    return false;
  }
  return true;
}

TypeInfo makeTypeInfo(std::string typeName,
                             RefStorageClass storage,
                             bool isMutable,
                             bool declaredRef) {
  ParsedTypeDescriptor desc = parseTypeString(typeName);
  TypeInfo info;
  info.typeName = std::move(desc.sanitized);
  info.pointerDepth = desc.pointerDepth;
  info.isArray = desc.isArray;
  info.arrayDepth = desc.arrayDepth;
  info.arrayRanks = desc.arrayRanks;
  info.isMultidimensional = desc.isMultidimensional;
  info.isNullable = desc.isNullable;
  info.elementNullable = desc.elementNullable;
  info.refStorage = storage;
  info.isMutable = isMutable;
  info.declaredRef = declaredRef;
  populateTypeInfoGenerics(info);
  finalizeTypeInfoMetadata(info);
  return info;
}

std::vector<TypeInfo> buildGenericArgumentTypeInfos(const std::string &segment) {
  std::vector<TypeInfo> result;
  std::vector<std::string> parts;
  if (!splitGenericArgumentList(segment, parts))
    return result;

  result.reserve(parts.size());
  for (const std::string &part : parts)
    result.push_back(makeTypeInfo(part));
  return result;
}

static void populateTypeInfoGenerics(TypeInfo &info) {
  info.typeArguments.clear();

  std::string basePortion = info.typeName;
  size_t suffixPos = basePortion.find_first_of("@[");
  if (suffixPos != std::string::npos)
    basePortion = basePortion.substr(0, suffixPos);

  size_t anglePos = basePortion.find('<');
  if (anglePos != std::string::npos) {
    info.baseTypeName = basePortion.substr(0, anglePos);
    if (auto closePos = findMatchingAngleInTypeName(basePortion, anglePos)) {
      std::string segment =
          basePortion.substr(anglePos + 1, *closePos - anglePos - 1);
      info.typeArguments = buildGenericArgumentTypeInfos(segment);
    } else {
      info.typeArguments.clear();
    }
  } else {
    info.baseTypeName = basePortion;
  }

  if (info.baseTypeName.empty())
    info.baseTypeName = basePortion;

  info.isGenericParameter = isActiveGenericParameter(info.baseTypeName);
}

static std::pair<std::string, std::string>
splitTypeStemAndSuffix(const std::string &typeName) {
  size_t suffixPos = typeName.find_first_of("@[");
  if (suffixPos == std::string::npos)
    return {typeName, std::string()};
  return {typeName.substr(0, suffixPos), typeName.substr(suffixPos)};
}

std::vector<std::string>
buildInheritanceChain(const std::optional<std::string> &baseClassName) {
  std::vector<std::string> chain;
  if (!baseClassName || baseClassName->empty())
    return chain;

  std::string current = *baseClassName;
  while (!current.empty()) {
    chain.push_back(current);
    const CompositeTypeInfo *info = lookupCompositeInfo(current);
    if (!info || !info->baseClass || info->baseClass->empty())
      break;
    if (*info->baseClass == current)
      break;
    current = *info->baseClass;
  }
  return chain;
}

static TypeInfo applyTypeWrappers(const TypeInfo &replacement,
                                  const TypeInfo &pattern) {
  std::string replacementStem =
      stripNullableAnnotations(typeNameFromInfo(replacement));
  auto [ignoredStem, patternSuffix] = splitTypeStemAndSuffix(pattern.typeName);
  (void)ignoredStem;
  std::string sanitized = replacementStem + patternSuffix;
  TypeInfo result = makeTypeInfo(sanitized);
  result.typeArguments = replacement.typeArguments;
  result.baseTypeName = replacement.baseTypeName;
  result.isNullable = replacement.isNullable || pattern.isNullable;
  result.elementNullable = replacement.elementNullable || pattern.elementNullable;
  result.refStorage = pattern.refStorage;
  result.isMutable = pattern.isMutable;
  result.declaredRef = pattern.declaredRef;
  result.ownership = replacement.ownership;
  result.smartPointerKind = replacement.smartPointerKind;
  result.arcManaged = replacement.arcManaged;
  result.classDescriptor = replacement.classDescriptor;
  result.genericKey = replacement.genericKey;
  result.tupleElementNames = replacement.tupleElementNames;
  return result;
}

static TypeInfo
substituteTypeInfo(const TypeInfo &info,
                   const std::map<std::string, TypeInfo> &substitutions);

static TypeInfo
substituteTypeInfo(const TypeInfo &info,
                   const std::map<std::string, TypeInfo> &substitutions) {
  if (info.isGenericParameter) {
    auto It = substitutions.find(info.baseTypeName);
    if (It == substitutions.end())
      return info;
    return applyTypeWrappers(It->second, info);
  }

  if (!info.hasTypeArguments())
    return info;

  std::vector<TypeInfo> substitutedArgs;
  substitutedArgs.reserve(info.typeArguments.size());
  std::vector<std::string> sanitizedArgs;
  sanitizedArgs.reserve(info.typeArguments.size());

  for (const auto &arg : info.typeArguments) {
    TypeInfo substituted = substituteTypeInfo(arg, substitutions);
    sanitizedArgs.push_back(
        stripNullableAnnotations(typeNameFromInfo(substituted)));
    substitutedArgs.push_back(std::move(substituted));
  }

  auto [stem, suffix] = splitTypeStemAndSuffix(info.typeName);
  (void)stem;
  std::string basePortion = info.baseTypeName;
  if (!sanitizedArgs.empty()) {
    basePortion += "<";
    for (size_t i = 0; i < sanitizedArgs.size(); ++i) {
      if (i != 0)
        basePortion += ",";
      basePortion += sanitizedArgs[i];
    }
    basePortion += ">";
  }

  std::string sanitized = basePortion + suffix;
  TypeInfo result = makeTypeInfo(sanitized);
  result.typeArguments = substitutedArgs;
  result.baseTypeName = info.baseTypeName;
  result.isNullable = info.isNullable;
  result.elementNullable = info.elementNullable;
  result.refStorage = info.refStorage;
  result.isMutable = info.isMutable;
  result.declaredRef = info.declaredRef;
  result.ownership = info.ownership;
  result.smartPointerKind = info.smartPointerKind;
  result.arcManaged = info.arcManaged;
  result.classDescriptor = info.classDescriptor;
  result.tupleElementNames = info.tupleElementNames;
  rebuildGenericBindingKey(result);
  return result;
}

static std::string buildNormalizedCompositeKey(const StructAST &templ,
                                               const TypeInfo &requestedType) {
  std::string key = requestedType.baseTypeName;
  const auto &usage = templ.layoutParameterUsage();
  if (requestedType.typeArguments.empty())
    return key;
  key += "<";
  for (std::size_t i = 0; i < requestedType.typeArguments.size(); ++i) {
    if (i)
      key += ",";
    bool depends = i < usage.size() ? usage[i] : true;
    if (!depends) {
      key += "_";
    } else {
      key += stripNullableAnnotations(
          typeNameFromInfo(requestedType.typeArguments[i]));
    }
  }
  key += ">";
  return key;
}

const InstanceFieldInfo *
findInstanceField(const CompositeTypeInfo &metadata,
                  std::string_view fieldName) {
  for (const auto &field : metadata.instanceFields) {
    if (field.name == fieldName)
      return &field;
  }
  return nullptr;
}

static bool registerSmartPointerOverload(const std::string &baseName,
                                         const std::string &constructedName,
                                         llvm::Function *fn,
                                         const std::vector<TypeInfo> &params) {
  if (!fn)
    return false;
  FunctionOverload entry;
  TypeInfo returnInfo = makeTypeInfo(constructedName);
  finalizeTypeInfoMetadata(returnInfo);
  entry.mangledName = fn->getName().str();
  entry.returnType = std::move(returnInfo);
  entry.returnsByRef = false;
  entry.parameterTypes = params;
  entry.parameterIsRef.assign(params.size(), false);
  entry.parameterIsParams.assign(params.size(), false);
  entry.function = fn;
  entry.isGenericInstantiation = true;
  auto &overloads = currentCodegen().functionOverloads;
  overloads[baseName].push_back(entry);
  if (constructedName != baseName)
    overloads[constructedName].push_back(entry);
  return true;
}

static llvm::Function *
createSmartPointerCtorFunction(const std::string &constructedName,
                               const std::string &suffix,
                               llvm::StructType *structTy,
                               const std::vector<llvm::Type *> &paramTypes,
                               const std::vector<std::string> &argNames) {
  std::string fnName = "__hybrid_smart_" + sanitizeForMangle(constructedName) +
                       "$" + suffix;
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(structTy, paramTypes, false);
  llvm::Function *fn = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, fnName, TheModule.get());
  unsigned idx = 0;
  for (auto &arg : fn->args()) {
    if (idx < argNames.size() && !argNames[idx].empty())
      arg.setName(argNames[idx]);
    else
      arg.setName("arg" + std::to_string(idx));
    ++idx;
  }
  return fn;
}

static llvm::Function *
createSmartPointerHelperFunction(const std::string &constructedName,
                                 const std::string &suffix,
                                 llvm::Type *returnType,
                                 const std::vector<llvm::Type *> &paramTypes,
                                 const std::vector<std::string> &argNames) {
  std::string fnName = "__hybrid_smart_" + sanitizeForMangle(constructedName) +
                       "$" + suffix;
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(returnType, paramTypes, false);
  llvm::Function *fn = llvm::Function::Create(
      fnType, llvm::Function::ExternalLinkage, fnName, TheModule.get());
  unsigned idx = 0;
  for (auto &arg : fn->args()) {
    if (idx < argNames.size() && !argNames[idx].empty())
      arg.setName(argNames[idx]);
    else
      arg.setName("arg" + std::to_string(idx));
    ++idx;
  }
  return fn;
}

static bool emitSmartPointerConstructors(const TypeInfo &requestedType,
                                         llvm::StructType *structTy,
                                         CompositeTypeInfo &metadata,
                                         SmartPointerKind kind,
                                         const TypeInfo &payloadInfo,
                                         llvm::Type *payloadTy) {
  if (!structTy || kind == SmartPointerKind::None)
    return true;

  const std::string constructedName =
      stripNullableAnnotations(typeNameFromInfo(requestedType));
  const std::string baseName = requestedType.baseTypeName;

  auto *savedBlock = Builder->GetInsertBlock();
  llvm::BasicBlock::iterator savedPoint;
  const bool hadInsertPoint = savedBlock != nullptr;
  if (hadInsertPoint)
    savedPoint = Builder->GetInsertPoint();

  const InstanceFieldInfo *valueField =
      findInstanceField(metadata, "value");
  const InstanceFieldInfo *flagField =
      findInstanceField(metadata, "hasValue");
  const InstanceFieldInfo *payloadField =
      findInstanceField(metadata, "payload");
  const InstanceFieldInfo *controlField =
      findInstanceField(metadata,
                        kind == SmartPointerKind::Weak ? "weakControl"
                                                        : "control");

  auto getFieldLLVMType = [&](const InstanceFieldInfo *field) -> llvm::Type * {
    if (!field)
      return nullptr;
    return getTypeFromString(typeNameFromInfo(field->type));
  };

  auto buildCtor =
      [&](const std::string &suffix,
          const std::vector<TypeInfo> &paramInfos,
          const std::vector<llvm::Type *> &paramLLVMTypes,
          const std::vector<std::string> &argNames,
          llvm::function_ref<bool(llvm::Function *, llvm::AllocaInst *)> body)
      -> bool {
    llvm::Function *fn = createSmartPointerCtorFunction(
        constructedName, suffix, structTy, paramLLVMTypes, argNames);
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);

    llvm::AllocaInst *storage =
        Builder->CreateAlloca(structTy, nullptr, "smart.alloca");
    if (!body(fn, storage)) {
      fn->eraseFromParent();
      if (hadInsertPoint)
        Builder->SetInsertPoint(savedBlock, savedPoint);
      else
        Builder->ClearInsertionPoint();
      return false;
    }

    llvm::Value *result =
        Builder->CreateLoad(structTy, storage, "smart.result");
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);

    if (!registerSmartPointerOverload(baseName, constructedName, fn,
                                      paramInfos))
      return false;
    return true;
  };

  auto storeBoolField = [&](llvm::AllocaInst *storage, bool value) {
    llvm::Type *flagTy = getFieldLLVMType(flagField);
    if (!flagField || !flagTy)
      return false;
    llvm::Value *flagPtr = Builder->CreateStructGEP(
        structTy, storage, flagField->index, "smart.flag.ptr");
    llvm::Value *constant =
        llvm::ConstantInt::get(flagTy, value ? 1 : 0);
    Builder->CreateStore(constant, flagPtr);
    return true;
  };

  const bool payloadUsesARC = payloadInfo.requiresARC();
  bool success = true;
  switch (kind) {
  case SmartPointerKind::Unique: {
    if (!valueField || !flagField) {
      success = false;
      break;
    }
    llvm::Type *storedType = getFieldLLVMType(valueField);
    if (!storedType) {
      success = false;
      break;
    }

    if (!buildCtor(
            "unique.default", {}, {}, {},
            [&](llvm::Function *, llvm::AllocaInst *storage) {
              llvm::Value *valuePtr = Builder->CreateStructGEP(
                  structTy, storage, valueField->index, "smart.value.ptr");
              Builder->CreateStore(
                  llvm::Constant::getNullValue(storedType), valuePtr);
              return storeBoolField(storage, false);
            }))
      success = false;

    std::vector<TypeInfo> params{payloadInfo};
    std::vector<llvm::Type *> llvmParams{payloadTy};
	    if (!buildCtor(
	            "unique.payload", params, llvmParams, {"payload"},
	            [&](llvm::Function *fn, llvm::AllocaInst *storage) {
	              llvm::Argument &payloadArg = *fn->arg_begin();
	              llvm::Value *valuePtr = Builder->CreateStructGEP(
	                  structTy, storage, valueField->index, "smart.value.ptr");
	              if (payloadInfo.requiresARC()) {
	                auto *opaquePtrTy =
	                    pointerType(llvm::Type::getInt8Ty(*TheContext));
	                llvm::Value *opaquePayload = Builder->CreateBitCast(
	                    &payloadArg, opaquePtrTy,
	                    "smart.unique.payload.cast");
	                llvm::Value *retainedOpaque = Builder->CreateCall(
	                    getHybridRetainFunction(), {opaquePayload},
	                    "smart.unique.payload.retain");
	                llvm::Value *retained =
	                    Builder->CreateBitCast(retainedOpaque, storedType,
	                                           "smart.unique.payload.value");
	                Builder->CreateStore(retained, valuePtr);
	              } else {
	                Builder->CreateStore(&payloadArg, valuePtr);
	              }
	              return storeBoolField(storage, true);
	            }))
      success = false;
    break;
  }
  case SmartPointerKind::Shared: {
    if (!payloadField || !controlField) {
      success = false;
      break;
    }
    llvm::Type *payloadFieldTy = getFieldLLVMType(payloadField);
    llvm::Type *controlTy = getFieldLLVMType(controlField);
    if (!payloadFieldTy || !controlTy) {
      success = false;
      break;
    }

    if (!buildCtor(
            "shared.default", {}, {}, {},
            [&](llvm::Function *, llvm::AllocaInst *storage) {
              llvm::Value *payloadPtr = Builder->CreateStructGEP(
                  structTy, storage, payloadField->index,
                  "smart.payload.ptr");
              llvm::Value *controlPtr = Builder->CreateStructGEP(
                  structTy, storage, controlField->index,
                  "smart.control.ptr");
              Builder->CreateStore(
                  llvm::Constant::getNullValue(payloadFieldTy),
                  payloadPtr);
              Builder->CreateStore(
                  llvm::Constant::getNullValue(controlTy), controlPtr);
              return true;
            }))
      break;

    std::vector<TypeInfo> params{payloadInfo};
    std::vector<llvm::Type *> llvmParams{payloadTy};
    if (!buildCtor(
            "shared.payload", params, llvmParams, {"payload"},
            [&](llvm::Function *fn, llvm::AllocaInst *storage) {
              llvm::Argument &payloadArg = *fn->arg_begin();
              llvm::Value *payloadPtr = Builder->CreateStructGEP(
                  structTy, storage, payloadField->index,
                  "smart.payload.ptr");
              llvm::Value *storedPayload = &payloadArg;
              if (storedPayload->getType() != payloadFieldTy)
                storedPayload = Builder->CreateBitCast(
                    storedPayload, payloadFieldTy,
                    "smart.shared.payload.cast");
              Builder->CreateStore(storedPayload, payloadPtr);

              llvm::Value *controlPtr = Builder->CreateStructGEP(
                  structTy, storage, controlField->index,
                  "smart.control.ptr");
              if (!payloadUsesARC) {
                Builder->CreateStore(
                    llvm::Constant::getNullValue(controlTy), controlPtr);
                return true;
              }

              llvm::Value *opaquePayload = Builder->CreateBitCast(
                  &payloadArg, pointerType(),
                  "smart.shared.payload.cast");
              llvm::Value *controlOpaque = Builder->CreateCall(
                  getSharedControlCreateFunction(), {opaquePayload},
                  "smart.shared.control.raw");
              if (!controlOpaque)
                return false;
              llvm::Value *typedControl = controlOpaque;
              if (typedControl->getType() != controlTy)
                typedControl =
                    Builder->CreateBitCast(controlOpaque, controlTy,
                                           "smart.shared.control");
              Builder->CreateStore(typedControl, controlPtr);
              return true;
            }))
      success = false;
    if (success && !metadata.smartPointerCopyHelper.empty()) {
      llvm::Function *helperFn =
          TheModule->getFunction(metadata.smartPointerCopyHelper);
      if (!helperFn) {
        reportCompilerError("Internal error: missing smart pointer copy helper '" +
                            metadata.smartPointerCopyHelper + "'");
        success = false;
        break;
      }

      std::vector<TypeInfo> params{requestedType};
      llvm::Type *selfLLVM =
          getTypeFromString(stripNullableAnnotations(constructedName));
      std::vector<llvm::Type *> llvmParams{selfLLVM};

      if (!buildCtor(
              "shared.copy", params, llvmParams, {"source"},
              [&](llvm::Function *fn, llvm::AllocaInst *storage) {
                llvm::Argument &sourceArg = *fn->arg_begin();
                llvm::Value *sourceVal = &sourceArg;

                llvm::Type *expected = helperFn->getFunctionType()->getParamType(0);
                if (sourceVal->getType() != expected) {
                  if (!expected->isPointerTy())
                    return false;

                  llvm::Value *coerced = sourceVal;
                  llvm::Type *elemTy = nullptr;
                  if (expected->getNumContainedTypes() > 0)
                    elemTy = expected->getContainedType(0);

                  if (elemTy && !sourceVal->getType()->isPointerTy() &&
                      sourceVal->getType() == elemTy) {
                    llvm::AllocaInst *tmp = Builder->CreateAlloca(
                        elemTy, nullptr, "smart.shared.copy.src");
                    Builder->CreateStore(sourceVal, tmp);
                    coerced = tmp;
                  } else {
                    coerced = castToType(sourceVal, expected, constructedName);
                    if (!coerced)
                      return false;
                  }

                  sourceVal = coerced;
                }

                llvm::Value *result =
                    Builder->CreateCall(helperFn, {sourceVal},
                                        "smart.shared.copy.call");
                Builder->CreateStore(result, storage);
                return true;
              }))
        success = false;
    }
    break;
  }
  case SmartPointerKind::Weak: {
    if (!payloadField || !controlField) {
      success = false;
      break;
    }
    llvm::Type *payloadFieldTy = getFieldLLVMType(payloadField);
    llvm::Type *controlTy = getFieldLLVMType(controlField);
    if (!payloadFieldTy || !controlTy) {
      success = false;
      break;
    }

    if (!buildCtor(
            "weak.default", {}, {}, {},
            [&](llvm::Function *, llvm::AllocaInst *storage) {
              llvm::Value *payloadPtr = Builder->CreateStructGEP(
                  structTy, storage, payloadField->index,
                  "smart.payload.ptr");
              llvm::Value *controlPtr = Builder->CreateStructGEP(
                  structTy, storage, controlField->index,
                  "smart.control.ptr");
              Builder->CreateStore(
                  llvm::Constant::getNullValue(payloadFieldTy),
                  payloadPtr);
              Builder->CreateStore(
                  llvm::Constant::getNullValue(controlTy), controlPtr);
              return true;
            }))
      success = false;

    std::string payloadName = typeNameFromInfo(payloadInfo);
    std::string sharedName = "shared<" + payloadName + ">";
    TypeInfo sharedType = makeTypeInfo(sharedName);
    finalizeTypeInfoMetadata(sharedType);
    materializeCompositeInstantiation(sharedType);
    std::string sharedKey =
        stripNullableAnnotations(typeNameFromInfo(sharedType));
    llvm::StructType *sharedStructTy = StructTypes[sharedKey];
    const CompositeTypeInfo *sharedInfo =
        lookupCompositeInfo(sharedKey, /*countHit=*/false);
    if (!sharedStructTy || !sharedInfo) {
      success = false;
      break;
    }
    const InstanceFieldInfo *sharedPayloadField =
        findInstanceField(*sharedInfo, "payload");
    const InstanceFieldInfo *sharedControlField =
        findInstanceField(*sharedInfo, "control");
    if (!sharedPayloadField || !sharedControlField) {
      success = false;
      break;
    }
    llvm::Type *sharedPayloadTy =
        getTypeFromString(typeNameFromInfo(sharedPayloadField->type));
    llvm::Type *sharedControlTy =
        getTypeFromString(typeNameFromInfo(sharedControlField->type));
    if (!sharedPayloadTy || !sharedControlTy) {
      success = false;
      break;
    }

    std::vector<TypeInfo> params{sharedType};
    std::vector<llvm::Type *> llvmParams{
        getTypeFromString(typeNameFromInfo(sharedType))};
    if (llvmParams.empty() || !llvmParams.front()) {
      success = false;
      break;
    }

    if (!buildCtor(
            "weak.shared", params, llvmParams, {"owner"},
            [&](llvm::Function *fn, llvm::AllocaInst *storage) {
              llvm::Argument &ownerArg = *fn->arg_begin();
              llvm::Value *typedOwner = &ownerArg;
              llvm::Type *ownerPtrTy = pointerType(sharedStructTy);
              if (!typedOwner->getType()->isPointerTy()) {
                llvm::AllocaInst *tmp = Builder->CreateAlloca(
                    sharedStructTy, nullptr, "smart.shared.arg");
                Builder->CreateStore(typedOwner, tmp);
                typedOwner = tmp;
              } else if (typedOwner->getType() != ownerPtrTy) {
                typedOwner =
                    Builder->CreateBitCast(typedOwner, ownerPtrTy,
                                           "smart.shared.ptr");
              }

              llvm::Value *ownerPayloadPtr = Builder->CreateStructGEP(
                  sharedStructTy, typedOwner, sharedPayloadField->index,
                  "smart.shared.payload.ptr");
              llvm::Value *ownerPayload =
                  Builder->CreateLoad(sharedPayloadTy, ownerPayloadPtr,
                                      "smart.shared.payload");

              llvm::Value *payloadPtr = Builder->CreateStructGEP(
                  structTy, storage, payloadField->index,
                  "smart.payload.ptr");
              if (ownerPayload->getType() != payloadFieldTy)
                ownerPayload =
                    Builder->CreateBitCast(ownerPayload, payloadFieldTy);
              Builder->CreateStore(ownerPayload, payloadPtr);

              llvm::Value *ownerControlPtr = Builder->CreateStructGEP(
                  sharedStructTy, typedOwner, sharedControlField->index,
                  "smart.shared.control.ptr");
              llvm::Value *ownerControl =
                  Builder->CreateLoad(sharedControlTy, ownerControlPtr,
                                      "smart.shared.control");

              llvm::Value *controlPtr = Builder->CreateStructGEP(
                  structTy, storage, controlField->index,
                  "smart.control.ptr");
              if (!payloadUsesARC) {
                Builder->CreateStore(
                    llvm::Constant::getNullValue(controlTy), controlPtr);
                return true;
              }

              llvm::Value *hasControl = Builder->CreateICmpNE(
                  ownerControl,
                  llvm::Constant::getNullValue(sharedControlTy),
                  "smart.shared.hascontrol");
              llvm::Function *parent = Builder->GetInsertBlock()->getParent();
              llvm::BasicBlock *retainBB = llvm::BasicBlock::Create(
                  *TheContext, "smart.weak.retain", parent);
              llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(
                  *TheContext, "smart.weak.merge", parent);
              Builder->CreateCondBr(hasControl, retainBB, mergeBB);

              Builder->SetInsertPoint(retainBB);
              llvm::Value *opaqueControl = Builder->CreateBitCast(
                  ownerControl, pointerType(),
                  "smart.shared.control.opaque");
              Builder->CreateCall(getSharedControlRetainWeakFunction(),
                                  {opaqueControl});
              Builder->CreateBr(mergeBB);

              Builder->SetInsertPoint(mergeBB);
              Builder->CreateStore(ownerControl, controlPtr);
              return true;
            }))
      success = false;
    break;
  }
  case SmartPointerKind::None:
    break;
  }

  if (hadInsertPoint)
    Builder->SetInsertPoint(savedBlock, savedPoint);
  else
    Builder->ClearInsertionPoint();
  return success;
}

static bool materializeSmartPointerInstantiation(
    const std::string &constructedName, const TypeInfo &requestedType,
    SmartPointerKind kind) {
  if (CG.compositeMetadata.contains(constructedName))
    return true;

  if (requestedType.typeArguments.size() != 1) {
    reportCompilerError("Smart pointer '" + requestedType.baseTypeName +
                        "' expects exactly one type argument");
    return false;
  }

  TypeInfo payloadInfo = requestedType.typeArguments.front();
  payloadInfo = applyActiveTypeBindings(payloadInfo);
  finalizeTypeInfoMetadata(payloadInfo);
  llvm::Type *payloadTy = getTypeFromString(typeNameFromInfo(payloadInfo));
  if (!payloadTy) {
    reportCompilerError("Unable to materialize smart pointer '" +
                        constructedName + "' because payload type '" +
                        typeNameFromInfo(payloadInfo) + "' is unknown");
    return false;
  }

  llvm::StructType *structTy = nullptr;
  auto structIt = StructTypes.find(constructedName);
  if (structIt != StructTypes.end() && structIt->second)
    structTy = structIt->second;
  if (!structTy) {
    structTy = llvm::StructType::create(*TheContext, constructedName);
    StructTypes[constructedName] = structTy;
  }

  std::vector<llvm::Type *> fieldTypes;
  std::vector<std::pair<std::string, unsigned>> fieldIndices;
  std::map<std::string, std::string> fieldTypeMap;
  std::map<std::string, MemberModifiers> fieldModifiers;
  std::vector<InstanceFieldInfo> instanceFields;

  auto appendField = [&](const std::string &fieldName, llvm::Type *llvmType,
                         TypeInfo fieldInfo) {
    unsigned index = fieldTypes.size();
    fieldTypes.push_back(llvmType);
    fieldIndices.emplace_back(fieldName, index);
    fieldTypeMap[fieldName] = typeNameFromInfo(fieldInfo);
    fieldModifiers[fieldName] = MemberModifiers{};
    InstanceFieldInfo info;
    info.name = fieldName;
    info.index = index;
    info.type = std::move(fieldInfo);
    instanceFields.push_back(info);
  };

  switch (kind) {
  case SmartPointerKind::Unique: {
    appendField("value", payloadTy, payloadInfo);
    TypeInfo flagInfo = makeTypeInfo("bool");
    finalizeTypeInfoMetadata(flagInfo);
    llvm::Type *boolTy = getTypeFromString("bool");
    appendField("hasValue", boolTy, flagInfo);
    break;
  }
  case SmartPointerKind::Shared: {
    TypeInfo storedPayload = payloadInfo;
    storedPayload.ownership = OwnershipQualifier::Unowned;
    appendField("payload", payloadTy, storedPayload);
    llvm::StructType *controlTy =
        getOrCreateSharedControlBlockType(constructedName, payloadTy);
    std::string controlBase =
        "__HybridSharedControl$" + sanitizeForMangle(constructedName);
    TypeInfo controlInfo = makeTypeInfo(controlBase + "@");
    finalizeTypeInfoMetadata(controlInfo);
    appendField("control", pointerType(controlTy), controlInfo);
    break;
  }
  case SmartPointerKind::Weak: {
    TypeInfo storedPayload = payloadInfo;
    storedPayload.ownership = OwnershipQualifier::Unowned;
    appendField("payload", payloadTy, storedPayload);
    llvm::StructType *controlTy =
        getOrCreateSharedControlBlockType(constructedName, payloadTy);
    std::string controlBase =
        "__HybridSharedControl$" + sanitizeForMangle(constructedName);
    TypeInfo controlInfo = makeTypeInfo(controlBase + "@");
    finalizeTypeInfoMetadata(controlInfo);
    appendField("weakControl", pointerType(controlTy), controlInfo);
    break;
  }
  case SmartPointerKind::None:
    return false;
  }

  structTy->setBody(fieldTypes);
  StructFieldIndices[constructedName] = fieldIndices;
  StructFieldTypes[constructedName] = fieldTypeMap;

  // Force the struct type to be emitted early in LLVM textual IR by creating
  // a dummy global variable. This ensures the type definition appears before
  // any functions that use it, which is required by LLVM's textual IR parser.
  std::string dummyGlobalName =
      "__hybrid_force_type_emission_" + sanitizeForMangle(constructedName);
  if (!TheModule->getGlobalVariable(dummyGlobalName, true)) {
    new llvm::GlobalVariable(
        *TheModule, structTy, true, llvm::GlobalValue::InternalLinkage,
        llvm::Constant::getNullValue(structTy), dummyGlobalName);
  }

  CompositeTypeInfo info;
  info.kind = AggregateKind::Struct;
  info.genericParameters = {"T"};
  info.hasARCHeader = false;
  info.headerFieldIndex = std::numeric_limits<unsigned>::max();
  info.fieldTypes = fieldTypeMap;
  info.fieldModifiers = fieldModifiers;
  info.instanceFields = instanceFields;
  info.typeArgumentBindings["T"] = payloadInfo;
  info.hasClassDescriptor = true;
  info.descriptor.name = constructedName;
  info.descriptor.kind = AggregateKind::Struct;
  info.descriptor.baseClassName = std::nullopt;
  info.descriptor.interfaceNames.clear();
  info.descriptor.inheritanceChain.clear();
  info.descriptor.isAbstract = false;
  info.descriptor.isInterface = false;

  if (info.descriptorGlobalName.empty()) {
    llvm::StructType *typeDescTy = getTypeDescriptorType();
    std::string descriptorName =
        makeRuntimeSymbolName("__hybrid_type_descriptor$", constructedName);
    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(descriptorName, true);
    if (!descriptorGV) {
      descriptorGV = new llvm::GlobalVariable(
          *TheModule, typeDescTy, false, llvm::GlobalValue::InternalLinkage,
          llvm::Constant::getNullValue(typeDescTy), descriptorName);
    }
    info.descriptorGlobalName = descriptorName;
  }

  CG.compositeMetadata[constructedName] = std::move(info);
  CompositeTypeInfo &metadata = CG.compositeMetadata[constructedName];
  metadata.smartPointerKind = kind;
  metadata.smartPointerCopyHelper.clear();
  metadata.smartPointerMoveHelper.clear();
  metadata.smartPointerDestroyHelper.clear();

  if (!emitClassRuntimeStructures(constructedName, structTy, metadata))
    return false;
  if (!emitSmartPointerConstructors(requestedType, structTy, metadata, kind,
                                    payloadInfo, payloadTy))
    return false;
  if (!emitSmartPointerHelpers(constructedName, structTy, metadata, kind))
    return false;

  return true;
}

bool materializeTupleInstantiation(const TypeInfo &requestedType) {
  TypeInfo tupleInfo = applyActiveTypeBindings(requestedType);
  finalizeTypeInfoMetadata(tupleInfo);

  if (!tupleInfo.isTupleType())
    return false;

  std::string constructedName =
      stripNullableAnnotations(typeNameFromInfo(tupleInfo));
  if (constructedName.empty())
    return false;

  if (CG.compositeMetadata.contains(constructedName))
    return true;

  llvm::StructType *structTy = nullptr;
  auto structIt = StructTypes.find(constructedName);
  if (structIt != StructTypes.end() && structIt->second)
    structTy = structIt->second;
  if (!structTy) {
    structTy = llvm::StructType::create(*TheContext, constructedName);
    StructTypes[constructedName] = structTy;
  }

  std::vector<llvm::Type *> fieldTypes;
  std::vector<std::pair<std::string, unsigned>> fieldIndices;
  std::map<std::string, std::string> fieldTypeMap;
  std::map<std::string, MemberModifiers> fieldModifiers;
  std::vector<InstanceFieldInfo> instanceFields;

  llvm::StructType *headerTy = getArcHeaderType();
  fieldTypes.push_back(headerTy);

  auto appendField = [&](std::string fieldName, const TypeInfo &fieldInfo) -> bool {
    std::string fieldTypeName = typeNameFromInfo(fieldInfo);
    llvm::Type *fieldLLVMType = getTypeFromString(fieldTypeName);
    if (!fieldLLVMType) {
      reportCompilerError("Unable to materialize tuple element of type '" +
                          fieldTypeName + "'");
      return false;
    }

    unsigned index = static_cast<unsigned>(fieldTypes.size());
    fieldTypes.push_back(fieldLLVMType);
    fieldIndices.emplace_back(fieldName, index);
    fieldTypeMap[fieldName] = fieldTypeName;

    MemberModifiers modifiers;
    modifiers.access = MemberAccess::PublicReadWrite();
    fieldModifiers[fieldName] = modifiers;

    InstanceFieldInfo info;
    info.name = std::move(fieldName);
    info.index = index;
    info.type = fieldInfo;
    instanceFields.push_back(std::move(info));
    return true;
  };

  for (size_t i = 0; i < tupleInfo.typeArguments.size(); ++i) {
    std::string fieldName = "$" + std::to_string(i);
    if (!appendField(std::move(fieldName), tupleInfo.typeArguments[i]))
      return false;
  }

  structTy->setBody(fieldTypes);
  StructFieldIndices[constructedName] = fieldIndices;
  StructFieldTypes[constructedName] = fieldTypeMap;

  std::string dummyGlobalName =
      "__hybrid_force_type_emission_" + sanitizeForMangle(constructedName);
  if (!TheModule->getGlobalVariable(dummyGlobalName, true)) {
    new llvm::GlobalVariable(
        *TheModule, structTy, true, llvm::GlobalValue::InternalLinkage,
        llvm::Constant::getNullValue(structTy), dummyGlobalName);
  }

  CompositeTypeInfo info;
  info.kind = AggregateKind::Struct;
  info.hasARCHeader = true;
  info.headerFieldIndex = 0;
  info.fieldTypes = fieldTypeMap;
  info.fieldModifiers = fieldModifiers;
  info.instanceFields = instanceFields;
  info.hasClassDescriptor = true;
  info.descriptor.name = constructedName;
  info.descriptor.kind = AggregateKind::Struct;
  info.descriptor.baseClassName = std::nullopt;
  info.descriptor.interfaceNames.clear();
  info.descriptor.inheritanceChain.clear();
  info.descriptor.isAbstract = false;
  info.descriptor.isInterface = false;

  if (info.descriptorGlobalName.empty()) {
    llvm::StructType *typeDescTy = getTypeDescriptorType();
    std::string descriptorName =
        makeRuntimeSymbolName("__hybrid_type_descriptor$", constructedName);
    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(descriptorName, true);
    if (!descriptorGV) {
      descriptorGV = new llvm::GlobalVariable(
          *TheModule, typeDescTy, false, llvm::GlobalValue::InternalLinkage,
          llvm::Constant::getNullValue(typeDescTy), descriptorName);
    }
    info.descriptorGlobalName = descriptorName;
  }

  CG.compositeMetadata[constructedName] = std::move(info);
  CompositeTypeInfo &metadata = CG.compositeMetadata[constructedName];

  if (!emitCompositeDealloc(constructedName, structTy, metadata))
    return false;
  if (!emitClassRuntimeStructures(constructedName, structTy, metadata))
    return false;

  return true;
}

static bool emitSmartPointerHelpers(const std::string &constructedName,
                                    llvm::StructType *structTy,
                                    CompositeTypeInfo &metadata,
                                    SmartPointerKind kind) {
  if (!structTy)
    return true;

  auto *savedBlock = Builder->GetInsertBlock();
  llvm::BasicBlock::iterator savedPoint;
  const bool hadInsertPoint = savedBlock != nullptr;
  if (hadInsertPoint)
    savedPoint = Builder->GetInsertPoint();
  const bool arcEnabled = isArcLoweringEnabled();
  auto restoreInsertPoint = [&]() {
    if (hadInsertPoint)
      Builder->SetInsertPoint(savedBlock, savedPoint);
    else
      Builder->ClearInsertionPoint();
  };

  auto getField = [&](std::string_view name) -> const InstanceFieldInfo * {
    return findInstanceField(metadata, name);
  };

  llvm::PointerType *structPtrTy = pointerType(structTy);
  TypeInfo selfType = makeTypeInfo(constructedName);
  finalizeTypeInfoMetadata(selfType);
  selfType.refStorage = RefStorageClass::RefAlias;
  selfType.declaredRef = true;

  auto registerMethod = [&](const std::string &name, llvm::Function *fn,
                            const TypeInfo &returnType,
                            bool returnsByRef = false) -> bool {
    if (!fn)
      return false;
    CompositeMemberInfo member;
    member.modifiers = MemberModifiers{};
    member.modifiers.access = MemberAccess::PublicReadWrite();
    member.signature = constructedName + "." + name;
    member.dispatchKey = member.signature;
    member.returnType = returnType;
    member.parameterTypes = {selfType};
    member.parameterIsRef = {true};
    member.parameterIsParams = {false};
    member.returnsByRef = returnsByRef;
    member.directFunction = fn;
    metadata.methodInfo[name] = std::move(member);
    auto &instantiations = metadata.genericMethodInstantiations[name];
    std::string mangled(fn->getName());
    if (std::find(instantiations.begin(), instantiations.end(), mangled) ==
        instantiations.end())
      instantiations.push_back(std::move(mangled));
    return true;
  };

  auto emitUniqueMoveHelper =
      [&](const InstanceFieldInfo *valueInfo,
          const InstanceFieldInfo *flagInfo) -> llvm::Function * {
    if (!valueInfo || !flagInfo)
      return nullptr;
    llvm::Type *valueTy =
        getTypeFromString(typeNameFromInfo(valueInfo->type));
    llvm::Type *flagTy =
        getTypeFromString(typeNameFromInfo(flagInfo->type));
    if (!valueTy || !flagTy)
      return nullptr;
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, "unique.move", structTy, {structPtrTy}, {"source"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &sourceArg = *fn->arg_begin();
    llvm::AllocaInst *resultAlloca =
        Builder->CreateAlloca(structTy, nullptr,
                              "smart.unique.move.alloca");
    llvm::Value *valuePtr =
        Builder->CreateStructGEP(structTy, &sourceArg, valueInfo->index,
                                 "smart.unique.move.value.ptr");
    llvm::Value *flagPtr =
        Builder->CreateStructGEP(structTy, &sourceArg, flagInfo->index,
                                 "smart.unique.move.flag.ptr");
    llvm::Value *valueVal =
        Builder->CreateLoad(valueTy, valuePtr, "smart.unique.move.value");
    llvm::Value *flagVal =
        Builder->CreateLoad(flagTy, flagPtr, "smart.unique.move.flag");

    llvm::Value *destValuePtr =
        Builder->CreateStructGEP(structTy, resultAlloca, valueInfo->index,
                                 "smart.unique.move.dest.value.ptr");
    llvm::Value *destFlagPtr =
        Builder->CreateStructGEP(structTy, resultAlloca, flagInfo->index,
                                 "smart.unique.move.dest.flag.ptr");
    Builder->CreateStore(valueVal, destValuePtr);
    Builder->CreateStore(flagVal, destFlagPtr);

    Builder->CreateStore(llvm::Constant::getNullValue(valueTy), valuePtr);
    Builder->CreateStore(llvm::ConstantInt::get(flagTy, 0), flagPtr);

    llvm::Value *result =
        Builder->CreateLoad(structTy, resultAlloca,
                            "smart.unique.move.result");
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);
    return fn;
  };

  auto emitUniqueDestroyHelper =
      [&](const InstanceFieldInfo *valueInfo,
          const InstanceFieldInfo *flagInfo) -> llvm::Function * {
    if (!valueInfo || !flagInfo)
      return nullptr;
    llvm::Type *valueTy =
        getTypeFromString(typeNameFromInfo(valueInfo->type));
    llvm::Type *flagTy =
        getTypeFromString(typeNameFromInfo(flagInfo->type));
    if (!valueTy || !flagTy)
      return nullptr;
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, "unique.destroy",
        llvm::Type::getVoidTy(*TheContext), {structPtrTy}, {"value"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &selfArg = *fn->arg_begin();
    llvm::Value *valuePtr =
        Builder->CreateStructGEP(structTy, &selfArg, valueInfo->index,
                                 "smart.unique.destroy.value.ptr");
    llvm::Value *flagPtr =
        Builder->CreateStructGEP(structTy, &selfArg, flagInfo->index,
                                 "smart.unique.destroy.flag.ptr");
    llvm::Value *flagVal =
        Builder->CreateLoad(flagTy, flagPtr, "smart.unique.destroy.flag");
    llvm::Value *hasValue = Builder->CreateICmpNE(
        flagVal, llvm::ConstantInt::get(flagTy, 0),
        "smart.unique.destroy.has");

    llvm::Function *parent = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *releaseBB =
        llvm::BasicBlock::Create(*TheContext, "smart.unique.destroy.release",
                                 parent);
    llvm::BasicBlock *continueBB =
        llvm::BasicBlock::Create(*TheContext, "smart.unique.destroy.cont",
                                 parent);
    Builder->CreateCondBr(hasValue, releaseBB, continueBB);

    Builder->SetInsertPoint(releaseBB);
    if (valueTy->isPointerTy() || valueInfo->type.isSmartPointer()) {
      llvm::Value *payload = Builder->CreateLoad(
          valueTy, valuePtr, "smart.unique.destroy.payload");
      emitArcRelease(payload, valueInfo->type, "smart.unique.destroy");
    }
    Builder->CreateBr(continueBB);

    Builder->SetInsertPoint(continueBB);
    Builder->CreateStore(llvm::Constant::getNullValue(valueTy), valuePtr);
    Builder->CreateStore(llvm::ConstantInt::get(flagTy, 0), flagPtr);
    Builder->CreateRetVoid();
    llvm::verifyFunction(*fn);
    return fn;
  };

  auto emitPointerCopyMoveHelper =
      [&](const std::string &suffix, bool retainControl, bool zeroSource,
          SmartPointerKind helperKind) -> llvm::Function * {
    const InstanceFieldInfo *payloadInfo = getField("payload");
    const InstanceFieldInfo *controlInfo =
        getField(helperKind == SmartPointerKind::Weak ? "weakControl"
                                                      : "control");
    if (!payloadInfo || !controlInfo)
      return nullptr;
    llvm::Type *payloadTyField =
        getTypeFromString(typeNameFromInfo(payloadInfo->type));
    llvm::Type *controlTyField =
        getTypeFromString(typeNameFromInfo(controlInfo->type));
    if (!payloadTyField || !controlTyField)
      return nullptr;
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, suffix, structTy, {structPtrTy}, {"source"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &sourceArg = *fn->arg_begin();
    llvm::AllocaInst *resultAlloca =
        Builder->CreateAlloca(structTy, nullptr, "smart.helper.alloca");

    llvm::Value *payloadPtr =
        Builder->CreateStructGEP(structTy, &sourceArg, payloadInfo->index,
                                 "smart.helper.payload.ptr");
    llvm::Value *controlPtr =
        Builder->CreateStructGEP(structTy, &sourceArg, controlInfo->index,
                                 "smart.helper.control.ptr");
    llvm::Value *payloadVal =
        Builder->CreateLoad(payloadTyField, payloadPtr,
                            "smart.helper.payload");
    llvm::Value *controlVal =
        Builder->CreateLoad(controlTyField, controlPtr,
                            "smart.helper.control");

    llvm::Value *destPayloadPtr =
        Builder->CreateStructGEP(structTy, resultAlloca, payloadInfo->index,
                                 "smart.helper.dest.payload.ptr");
    llvm::Value *destControlPtr =
        Builder->CreateStructGEP(structTy, resultAlloca, controlInfo->index,
                                 "smart.helper.dest.control.ptr");
    Builder->CreateStore(payloadVal, destPayloadPtr);
    Builder->CreateStore(controlVal, destControlPtr);

    if (retainControl) {
      llvm::Value *hasControl = Builder->CreateICmpNE(
          controlVal, llvm::Constant::getNullValue(controlTyField),
          "smart.helper.hasctrl");
      llvm::Function *parent = Builder->GetInsertBlock()->getParent();
      llvm::BasicBlock *retainBB = llvm::BasicBlock::Create(
          *TheContext, "smart.helper.retain", parent);
      llvm::BasicBlock *retainCont = llvm::BasicBlock::Create(
          *TheContext, "smart.helper.retain.cont", parent);
      Builder->CreateCondBr(hasControl, retainBB, retainCont);

      Builder->SetInsertPoint(retainBB);
      llvm::Value *opaqueControl =
          Builder->CreateBitCast(controlVal, pointerType(),
                                 "smart.helper.control.cast");
      if (helperKind == SmartPointerKind::Weak)
        Builder->CreateCall(getSharedControlRetainWeakFunction(),
                            {opaqueControl});
      else
        Builder->CreateCall(getSharedControlRetainStrongFunction(),
                            {opaqueControl});
      Builder->CreateBr(retainCont);

      Builder->SetInsertPoint(retainCont);
    }

    if (zeroSource) {
      Builder->CreateStore(llvm::Constant::getNullValue(payloadTyField),
                           payloadPtr);
      Builder->CreateStore(llvm::Constant::getNullValue(controlTyField),
                           controlPtr);
    }

    llvm::Value *result =
        Builder->CreateLoad(structTy, resultAlloca, "smart.helper.result");
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);
    return fn;
  };

  auto emitPointerDestroyHelper =
      [&](const std::string &suffix, SmartPointerKind helperKind)
      -> llvm::Function * {
    const InstanceFieldInfo *payloadInfo = getField("payload");
    const InstanceFieldInfo *controlInfo =
        getField(helperKind == SmartPointerKind::Weak ? "weakControl"
                                                      : "control");
    if (!payloadInfo || !controlInfo)
      return nullptr;
    llvm::Type *payloadTyField =
        getTypeFromString(typeNameFromInfo(payloadInfo->type));
    llvm::Type *controlTyField =
        getTypeFromString(typeNameFromInfo(controlInfo->type));
    if (!payloadTyField || !controlTyField)
      return nullptr;
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, suffix, llvm::Type::getVoidTy(*TheContext),
        {structPtrTy}, {"value"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &selfArg = *fn->arg_begin();
    llvm::Value *payloadPtr =
        Builder->CreateStructGEP(structTy, &selfArg, payloadInfo->index,
                                 "smart.dtor.payload.ptr");
    llvm::Value *controlPtr =
        Builder->CreateStructGEP(structTy, &selfArg, controlInfo->index,
                                 "smart.dtor.control.ptr");
    llvm::Value *controlVal =
        Builder->CreateLoad(controlTyField, controlPtr,
                            "smart.dtor.control");
    llvm::Value *hasControl = Builder->CreateICmpNE(
        controlVal, llvm::Constant::getNullValue(controlTyField),
        "smart.dtor.hasctrl");

    llvm::Function *parent = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *releaseBB =
        llvm::BasicBlock::Create(*TheContext, "smart.dtor.release", parent);
    llvm::BasicBlock *continueBB =
        llvm::BasicBlock::Create(*TheContext, "smart.dtor.cont", parent);
    Builder->CreateCondBr(hasControl, releaseBB, continueBB);

    Builder->SetInsertPoint(releaseBB);
    if (arcEnabled) {
      llvm::Value *opaqueControl =
          Builder->CreateBitCast(controlVal, pointerType(),
                                 "smart.dtor.control.cast");
      if (helperKind == SmartPointerKind::Weak)
        Builder->CreateCall(getSharedControlReleaseWeakFunction(),
                            {opaqueControl});
      else
        Builder->CreateCall(getSharedControlReleaseStrongFunction(),
                            {opaqueControl});
    }
    Builder->CreateBr(continueBB);

    Builder->SetInsertPoint(continueBB);
    Builder->CreateStore(llvm::Constant::getNullValue(payloadTyField),
                         payloadPtr);
    Builder->CreateStore(llvm::Constant::getNullValue(controlTyField),
                         controlPtr);
    Builder->CreateRetVoid();
    llvm::verifyFunction(*fn);
    return fn;
  };

  auto emitSharedUseCountHelper = [&]() -> llvm::Function * {
    const InstanceFieldInfo *controlInfo = getField("control");
    if (!controlInfo)
      return nullptr;
    llvm::Type *controlTyField =
        getTypeFromString(typeNameFromInfo(controlInfo->type));
    if (!controlTyField)
      return nullptr;
    llvm::Type *intTy = llvm::Type::getInt32Ty(*TheContext);
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, "shared.arcUseCount", intTy, {structPtrTy},
        {"self"});
    if (!fn)
      return nullptr;
    if (!arcEnabled) {
      llvm::BasicBlock *entry =
          llvm::BasicBlock::Create(*TheContext, "entry", fn);
      Builder->SetInsertPoint(entry);
      Builder->CreateRet(llvm::ConstantInt::get(intTy, 0));
      llvm::verifyFunction(*fn);
      return fn;
    }
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &selfArg = *fn->arg_begin();
    llvm::Value *controlPtr =
        Builder->CreateStructGEP(structTy, &selfArg, controlInfo->index,
                                 "smart.use_count.control.ptr");
    llvm::Value *controlVal =
        Builder->CreateLoad(controlTyField, controlPtr,
                            "smart.use_count.control");
    llvm::Value *hasControl = Builder->CreateICmpNE(
        controlVal, llvm::Constant::getNullValue(controlTyField),
        "smart.use_count.hasctrl");
    llvm::Function *parent = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *haveCtrlBB = llvm::BasicBlock::Create(
        *TheContext, "smart.use_count.have", parent);
    llvm::BasicBlock *noCtrlBB = llvm::BasicBlock::Create(
        *TheContext, "smart.use_count.none", parent);
    llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(
        *TheContext, "smart.use_count.done", parent);
    Builder->CreateCondBr(hasControl, haveCtrlBB, noCtrlBB);

    Builder->SetInsertPoint(noCtrlBB);
    Builder->CreateBr(doneBB);

    Builder->SetInsertPoint(haveCtrlBB);
    llvm::Value *opaqueControl = Builder->CreateBitCast(
        controlVal, pointerType(), "smart.use_count.control.cast");
    llvm::Value *count = Builder->CreateCall(
        getSharedControlUseCountFunction(), {opaqueControl},
        "smart.use_count.call");
    Builder->CreateBr(doneBB);

    Builder->SetInsertPoint(doneBB);
    llvm::PHINode *result = Builder->CreatePHI(
        intTy, 2, "smart.use_count.result");
    result->addIncoming(llvm::ConstantInt::get(intTy, 0), noCtrlBB);
    result->addIncoming(count, haveCtrlBB);
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);
    return fn;
  };

  auto emitGetHelper = [&](const InstanceFieldInfo *payloadInfo,
                           const std::string &suffix,
                           TypeInfo &returnInfo) -> llvm::Function * {
    if (!payloadInfo)
      return nullptr;
    llvm::Type *payloadTy =
        getTypeFromString(typeNameFromInfo(payloadInfo->type));
    if (!payloadTy)
      return nullptr;
    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, suffix, payloadTy, {structPtrTy}, {"self"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &selfArg = *fn->arg_begin();
    llvm::Value *payloadPtr =
        Builder->CreateStructGEP(structTy, &selfArg, payloadInfo->index,
                                 "smart.get.payload.ptr");
    llvm::Value *payloadVal =
        Builder->CreateLoad(payloadTy, payloadPtr, "smart.get.payload");
    Builder->CreateRet(payloadVal);
    llvm::verifyFunction(*fn);
    returnInfo = payloadInfo->type;
    return fn;
  };

  auto emitWeakLockHelper = [&](TypeInfo &outReturnType) -> llvm::Function * {
    const InstanceFieldInfo *payloadInfo = getField("payload");
    const InstanceFieldInfo *controlInfo = getField("weakControl");
    if (!payloadInfo || !controlInfo)
      return nullptr;
    llvm::Type *payloadTyField =
        getTypeFromString(typeNameFromInfo(payloadInfo->type));
    llvm::Type *controlTyField =
        getTypeFromString(typeNameFromInfo(controlInfo->type));
    if (!payloadTyField || !controlTyField)
      return nullptr;

    std::string payloadTypeName = typeNameFromInfo(payloadInfo->type);
    TypeInfo sharedReturnInfo =
        makeTypeInfo("shared<" + payloadTypeName + ">");
    finalizeTypeInfoMetadata(sharedReturnInfo);
    materializeCompositeInstantiation(sharedReturnInfo);
    std::string sharedKey =
        stripNullableAnnotations(typeNameFromInfo(sharedReturnInfo));
    llvm::StructType *sharedStructTy = StructTypes[sharedKey];
    const CompositeTypeInfo *sharedInfo =
        lookupCompositeInfo(sharedKey, /*countHit=*/false);
    if (!sharedStructTy || !sharedInfo)
      return nullptr;
    const InstanceFieldInfo *sharedPayloadField =
        findInstanceField(*sharedInfo, "payload");
    const InstanceFieldInfo *sharedControlField =
        findInstanceField(*sharedInfo, "control");
    if (!sharedPayloadField || !sharedControlField)
      return nullptr;
    llvm::Type *sharedPayloadTy =
        getTypeFromString(typeNameFromInfo(sharedPayloadField->type));
    llvm::Type *sharedControlTy =
        getTypeFromString(typeNameFromInfo(sharedControlField->type));
    if (!sharedPayloadTy || !sharedControlTy)
      return nullptr;

    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, "weak.lock", sharedStructTy, {structPtrTy},
        {"self"});
    if (!fn)
      return nullptr;
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    if (!arcEnabled) {
      llvm::AllocaInst *resultAlloca = Builder->CreateAlloca(
          sharedStructTy, nullptr, "smart.lock.stub");
      llvm::Value *payloadPtr = Builder->CreateStructGEP(
          sharedStructTy, resultAlloca, sharedPayloadField->index,
          "smart.lock.stub.payload.ptr");
      llvm::Value *controlPtr = Builder->CreateStructGEP(
          sharedStructTy, resultAlloca, sharedControlField->index,
          "smart.lock.stub.control.ptr");
      Builder->CreateStore(llvm::Constant::getNullValue(sharedPayloadTy),
                           payloadPtr);
      Builder->CreateStore(llvm::Constant::getNullValue(sharedControlTy),
                           controlPtr);
      llvm::Value *result = Builder->CreateLoad(
          sharedStructTy, resultAlloca, "smart.lock.stub.result");
      Builder->CreateRet(result);
      llvm::verifyFunction(*fn);
      outReturnType = sharedReturnInfo;
      return fn;
    }
    llvm::Argument &selfArg = *fn->arg_begin();

    llvm::AllocaInst *resultAlloca =
        Builder->CreateAlloca(sharedStructTy, nullptr,
                              "smart.lock.alloca");
    Builder->CreateStore(llvm::Constant::getNullValue(sharedStructTy),
                         resultAlloca);

    llvm::Value *controlPtr =
        Builder->CreateStructGEP(structTy, &selfArg, controlInfo->index,
                                 "smart.lock.control.ptr");
    llvm::Value *controlVal =
        Builder->CreateLoad(controlTyField, controlPtr,
                            "smart.lock.control");
    llvm::Value *hasControl = Builder->CreateICmpNE(
        controlVal, llvm::Constant::getNullValue(controlTyField),
        "smart.lock.hasctrl");
    llvm::Function *parent = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *tryLockBB = llvm::BasicBlock::Create(
        *TheContext, "smart.lock.try", parent);
    llvm::BasicBlock *doneBB = llvm::BasicBlock::Create(
        *TheContext, "smart.lock.done", parent);
    llvm::BasicBlock *successBB = llvm::BasicBlock::Create(
        *TheContext, "smart.lock.success", parent);
    Builder->CreateCondBr(hasControl, tryLockBB, doneBB);

    Builder->SetInsertPoint(tryLockBB);
    llvm::Value *opaqueControl = Builder->CreateBitCast(
        controlVal, pointerType(), "smart.lock.control.cast");
    llvm::Value *payloadOpaque = Builder->CreateCall(
        getSharedControlLockFunction(), {opaqueControl},
        "smart.lock.call");
    llvm::Value *shareable = Builder->CreateICmpNE(
        payloadOpaque, llvm::ConstantPointerNull::get(pointerType()),
        "smart.lock.live");
    Builder->CreateCondBr(shareable, successBB, doneBB);

    Builder->SetInsertPoint(successBB);
    llvm::Value *typedPayload = nullptr;
    if (sharedPayloadTy->isPointerTy()) {
      typedPayload = Builder->CreateBitCast(
          payloadOpaque, sharedPayloadTy, "smart.lock.payload.cast");
    } else {
      llvm::PointerType *payloadPtrTy = pointerType(sharedPayloadTy);
      llvm::Value *payloadPtr = Builder->CreateBitCast(
          payloadOpaque, payloadPtrTy, "smart.lock.payload.ptr");
      typedPayload = Builder->CreateLoad(
          sharedPayloadTy, payloadPtr, "smart.lock.payload");
    }
    llvm::Value *payloadPtr =
        Builder->CreateStructGEP(sharedStructTy, resultAlloca,
                                 sharedPayloadField->index,
                                 "smart.lock.result.payload.ptr");
    Builder->CreateStore(typedPayload, payloadPtr);

    llvm::Value *typedControl = controlVal;
    if (typedControl->getType() != sharedControlTy)
      typedControl = Builder->CreateBitCast(
          controlVal, sharedControlTy, "smart.lock.control.result.cast");
    llvm::Value *controlPtrOut =
        Builder->CreateStructGEP(sharedStructTy, resultAlloca,
                                 sharedControlField->index,
                                 "smart.lock.result.control.ptr");
    Builder->CreateStore(typedControl, controlPtrOut);
    Builder->CreateBr(doneBB);

    Builder->SetInsertPoint(doneBB);
    llvm::Value *result = Builder->CreateLoad(
        sharedStructTy, resultAlloca, "smart.lock.result");
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);
    outReturnType = std::move(sharedReturnInfo);
    return fn;
  };

  auto emitSharedToWeakHelper =
      [&](TypeInfo &returnInfo) -> llvm::Function * {
    const InstanceFieldInfo *payloadInfo = getField("payload");
    const InstanceFieldInfo *controlInfo = getField("control");
    if (!payloadInfo || !controlInfo)
      return nullptr;

    TypeInfo payloadBinding = payloadInfo->type;
    auto bindingIt = metadata.typeArgumentBindings.find("T");
    if (bindingIt != metadata.typeArgumentBindings.end())
      payloadBinding = bindingIt->second;
    finalizeTypeInfoMetadata(payloadBinding);
    std::string weakName =
        "weak<" + typeNameFromInfo(payloadBinding) + ">";
    TypeInfo weakReturnInfo = makeTypeInfo(weakName);
    finalizeTypeInfoMetadata(weakReturnInfo);
    materializeCompositeInstantiation(weakReturnInfo);
    std::string weakKey =
        stripNullableAnnotations(typeNameFromInfo(weakReturnInfo));
    llvm::StructType *weakStructTy = StructTypes[weakKey];
    const CompositeTypeInfo *weakMeta =
        lookupCompositeInfo(weakKey, /*countHit=*/false);
    if (!weakStructTy || !weakMeta)
      return nullptr;
    const InstanceFieldInfo *weakPayloadField =
        findInstanceField(*weakMeta, "payload");
    const InstanceFieldInfo *weakControlField =
        findInstanceField(*weakMeta, "weakControl");
    if (!weakPayloadField || !weakControlField)
      return nullptr;

    llvm::Type *sharedPayloadTy =
        getTypeFromString(typeNameFromInfo(payloadInfo->type));
    llvm::Type *sharedControlTy =
        getTypeFromString(typeNameFromInfo(controlInfo->type));
    llvm::Type *weakPayloadTy =
        getTypeFromString(typeNameFromInfo(weakPayloadField->type));
    llvm::Type *weakControlTy =
        getTypeFromString(typeNameFromInfo(weakControlField->type));
    if (!sharedPayloadTy || !sharedControlTy || !weakPayloadTy ||
        !weakControlTy)
      return nullptr;

    llvm::Function *fn = createSmartPointerHelperFunction(
        constructedName, "shared.weak", weakStructTy, {structPtrTy},
        {"self"});
    if (!fn)
      return nullptr;

    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(*TheContext, "entry", fn);
    Builder->SetInsertPoint(entry);
    llvm::Argument &selfArg = *fn->arg_begin();
    llvm::Value *payloadPtr = Builder->CreateStructGEP(
        structTy, &selfArg, payloadInfo->index,
        "smart.weak.payload.ptr");
    llvm::Value *controlPtr = Builder->CreateStructGEP(
        structTy, &selfArg, controlInfo->index,
        "smart.weak.control.ptr");
    llvm::Value *payloadVal =
        Builder->CreateLoad(sharedPayloadTy, payloadPtr,
                            "smart.weak.payload");
    llvm::Value *controlVal =
        Builder->CreateLoad(sharedControlTy, controlPtr,
                            "smart.weak.control");

    llvm::AllocaInst *resultAlloca = Builder->CreateAlloca(
        weakStructTy, nullptr, "smart.weak.alloca");
    llvm::Value *destPayloadPtr = Builder->CreateStructGEP(
        weakStructTy, resultAlloca, weakPayloadField->index,
        "smart.weak.result.payload.ptr");
    llvm::Value *storedPayload = payloadVal;
    if (storedPayload->getType() != weakPayloadTy)
      storedPayload = Builder->CreateBitCast(
          storedPayload, weakPayloadTy, "smart.weak.payload.cast");
    Builder->CreateStore(storedPayload, destPayloadPtr);

    llvm::Value *destControlPtr = Builder->CreateStructGEP(
        weakStructTy, resultAlloca, weakControlField->index,
        "smart.weak.result.control.ptr");
    llvm::Value *storedControl = controlVal;
    if (storedControl->getType() != weakControlTy)
      storedControl = Builder->CreateBitCast(
          storedControl, weakControlTy, "smart.weak.control.cast");
    Builder->CreateStore(storedControl, destControlPtr);

    if (arcEnabled) {
      llvm::Value *hasControl = Builder->CreateICmpNE(
          storedControl, llvm::Constant::getNullValue(weakControlTy),
          "smart.weak.hasctrl");
      llvm::Function *parent = Builder->GetInsertBlock()->getParent();
      llvm::BasicBlock *retainBB = llvm::BasicBlock::Create(
          *TheContext, "smart.weak.retain", parent);
      llvm::BasicBlock *retainCont = llvm::BasicBlock::Create(
          *TheContext, "smart.weak.retain.cont", parent);
      Builder->CreateCondBr(hasControl, retainBB, retainCont);

      Builder->SetInsertPoint(retainBB);
      llvm::Value *opaqueControl =
          Builder->CreateBitCast(storedControl, pointerType(),
                                 "smart.weak.control.cast");
      Builder->CreateCall(getSharedControlRetainWeakFunction(),
                          {opaqueControl});
      Builder->CreateBr(retainCont);
      Builder->SetInsertPoint(retainCont);
    }

    llvm::Value *result = Builder->CreateLoad(
        weakStructTy, resultAlloca, "smart.weak.result");
    Builder->CreateRet(result);
    llvm::verifyFunction(*fn);
    returnInfo = std::move(weakReturnInfo);
    return fn;
  };

  bool ok = true;
  switch (kind) {
  case SmartPointerKind::Unique: {
    const InstanceFieldInfo *valueInfo = getField("value");
    const InstanceFieldInfo *flagInfo = getField("hasValue");
    llvm::Function *moveFn =
        emitUniqueMoveHelper(valueInfo, flagInfo);
    llvm::Function *destroyFn =
        emitUniqueDestroyHelper(valueInfo, flagInfo);
    TypeInfo getReturn;
    llvm::Function *getFn =
        emitGetHelper(valueInfo, "unique.get", getReturn);
    if (!moveFn || !destroyFn || !getFn)
      ok = false;
    else {
      metadata.smartPointerMoveHelper = moveFn->getName().str();
      metadata.smartPointerCopyHelper.clear();
      metadata.smartPointerDestroyHelper = destroyFn->getName().str();
      registerMethod("get", getFn, getReturn);
    }
    break;
  }
  case SmartPointerKind::Shared: {
    const InstanceFieldInfo *payloadField = getField("payload");
    const InstanceFieldInfo *controlField = getField("control");
    if (!payloadField || !controlField) {
      ok = false;
      break;
    }
    llvm::Function *copyFn =
        emitPointerCopyMoveHelper("shared.copy", arcEnabled, false,
                                  SmartPointerKind::Shared);
    llvm::Function *moveFn =
        emitPointerCopyMoveHelper("shared.move", false, true,
                                  SmartPointerKind::Shared);
    llvm::Function *destroyFn =
        emitPointerDestroyHelper("shared.destroy", SmartPointerKind::Shared);
    llvm::Function *useCountFn = emitSharedUseCountHelper();
    TypeInfo weakReturnInfo;
    llvm::Function *weakHelper = emitSharedToWeakHelper(weakReturnInfo);
    TypeInfo getReturn;
    llvm::Function *getFn =
        emitGetHelper(payloadField, "shared.get", getReturn);
    if (!copyFn || !moveFn || !destroyFn || !useCountFn || !weakHelper ||
        !getFn)
      ok = false;
    else {
      metadata.smartPointerCopyHelper = copyFn->getName().str();
      metadata.smartPointerMoveHelper = moveFn->getName().str();
      metadata.smartPointerDestroyHelper = destroyFn->getName().str();
      TypeInfo countReturn = makeTypeInfo("int");
      finalizeTypeInfoMetadata(countReturn);
      registerMethod("arcUseCount", useCountFn, countReturn);
      registerMethod("get", getFn, getReturn);
      registerMethod("weak", weakHelper, weakReturnInfo);
    }
    break;
  }
  case SmartPointerKind::Weak: {
    const InstanceFieldInfo *payloadField = getField("payload");
    const InstanceFieldInfo *controlField = getField("weakControl");
    if (!payloadField || !controlField) {
      ok = false;
      break;
    }
    llvm::Function *copyFn =
        emitPointerCopyMoveHelper("weak.copy", arcEnabled, false,
                                  SmartPointerKind::Weak);
    llvm::Function *moveFn =
        emitPointerCopyMoveHelper("weak.move", false, true,
                                  SmartPointerKind::Weak);
    llvm::Function *destroyFn =
        emitPointerDestroyHelper("weak.destroy", SmartPointerKind::Weak);
    TypeInfo lockReturn;
    llvm::Function *lockFn = emitWeakLockHelper(lockReturn);
    TypeInfo getReturn;
    llvm::Function *getFn =
        emitGetHelper(payloadField, "weak.get", getReturn);
    if (!copyFn || !moveFn || !destroyFn || !lockFn || !getFn)
      ok = false;
    else {
      metadata.smartPointerCopyHelper = copyFn->getName().str();
      metadata.smartPointerMoveHelper = moveFn->getName().str();
      metadata.smartPointerDestroyHelper = destroyFn->getName().str();
      registerMethod("get", getFn, getReturn);
      registerMethod("lock", lockFn, lockReturn);
    }
    break;
  }
  case SmartPointerKind::None:
    break;
  }

  restoreInsertPoint();
  return ok;
}

const CompositeTypeInfo *
materializeCompositeInstantiation(const TypeInfo &requestedType) {
  std::string constructedName =
      stripNullableAnnotations(typeNameFromInfo(requestedType));
  SmartPointerKind smartKind =
      detectSmartPointerKind(requestedType.baseTypeName);
  if (smartKind != SmartPointerKind::None) {
    if (!materializeSmartPointerInstantiation(constructedName, requestedType,
                                              smartKind))
      return nullptr;
    return lookupCompositeInfo(constructedName, /*countHit=*/false);
  }
  if (const auto aliasIt =
          CG.compositeMetadataAliases.find(constructedName);
      aliasIt != CG.compositeMetadataAliases.end()) {
    noteTypeCacheHit();
    return lookupCompositeInfo(aliasIt->second);
  }
  auto existing = CG.compositeMetadata.find(constructedName);
  if (existing != CG.compositeMetadata.end()) {
    noteTypeCacheHit();
    return &existing->second;
  }

  StructAST *templateAst = FindGenericTemplate(requestedType.baseTypeName);
  if (!templateAst)
    return nullptr;

  if (templateAst->getGenericParameters().size() !=
      requestedType.typeArguments.size())
    return nullptr;

  std::string normalizedKey =
      buildNormalizedCompositeKey(*templateAst, requestedType);
  if (auto reuseIt = CG.compositeLayoutCache.find(normalizedKey);
      reuseIt != CG.compositeLayoutCache.end()) {
    const std::string &canonicalName = reuseIt->second;
    CG.compositeMetadataAliases[constructedName] = canonicalName;
    if (auto structIt = StructTypes.find(canonicalName);
        structIt != StructTypes.end())
      StructTypes[constructedName] = structIt->second;
    noteTypeCacheHit();
    return lookupCompositeInfo(canonicalName);
  }

  noteTypeCacheMiss();
  if (!recordGenericInstantiation(false))
    return nullptr;

  std::map<std::string, TypeInfo> substitutions;

  for (size_t i = 0; i < templateAst->getGenericParameters().size(); ++i)
    substitutions.emplace(templateAst->getGenericParameters()[i],
                          requestedType.typeArguments[i]);

  GenericTypeBindingScope bindingScope(substitutions, constructedName);
  if (!bindingScope.isActive())
    return nullptr;
  GenericInstantiationScope instantiationScope(constructedName);

  if (StructTypes.contains(constructedName))
    return lookupCompositeInfo(constructedName);

  FunctionInstantiationScope reentrantScope;
  if (!templateAst->codegen())
    return nullptr;

  CG.compositeLayoutCache[normalizedKey] = constructedName;
  return lookupCompositeInfo(constructedName, /*countHit=*/false);
}

const CompositeTypeInfo *
lookupCompositeInfo(const std::string &name, bool countHit) {
  if (const auto aliasIt = CG.compositeMetadataAliases.find(name);
      aliasIt != CG.compositeMetadataAliases.end())
    return lookupCompositeInfo(aliasIt->second, countHit);
  auto it = CG.compositeMetadata.find(name);
  if (it != CG.compositeMetadata.end()) {
    if (countHit)
      noteTypeCacheHit();
    return &it->second;
  }

  if (name.find('<') == std::string::npos)
    return nullptr;

  TypeInfo requested = makeTypeInfo(name);
  requested = applyActiveTypeBindings(requested);
  if (!requested.hasTypeArguments())
    return nullptr;

  if (requested.baseTypeName == "tuple") {
    if (!materializeTupleInstantiation(requested))
      return nullptr;
    std::string key = stripNullableAnnotations(typeNameFromInfo(requested));
    auto it = CG.compositeMetadata.find(key);
    return it != CG.compositeMetadata.end() ? &it->second : nullptr;
  }

  return materializeCompositeInstantiation(requested);
}

const std::map<std::string, TypeInfo> *currentTypeBindings() {
  auto &stack = currentCodegen().genericTypeBindingsStack;
  if (stack.empty())
    return nullptr;
  return &stack.back();
}

TypeInfo applyActiveTypeBindings(const TypeInfo &info) {
  if (const auto *bindings = currentTypeBindings())
    return substituteTypeInfo(info, *bindings);
  return info;
}

std::vector<TypeInfo>
applyActiveTypeBindingsToInfos(const std::vector<TypeInfo> &infos) {
  std::vector<TypeInfo> result;
  result.reserve(infos.size());
  for (const auto &info : infos)
    result.push_back(applyActiveTypeBindings(info));
  return result;
}

std::optional<TypeInfo>
applyActiveTypeBindingsToOptionalInfo(const std::optional<TypeInfo> &info) {
  if (!info)
    return std::nullopt;
  return applyActiveTypeBindings(*info);
}

const TypeInfo *lookupLocalTypeInfo(const std::string &name) {
  auto It = LocalTypes.find(name);
  if (It != LocalTypes.end())
    return &It->second;
  return nullptr;
}

const TypeInfo *lookupGlobalTypeInfo(const std::string &name) {
  auto It = GlobalTypes.find(name);
  if (It != GlobalTypes.end())
    return &It->second;
  return nullptr;
}

const TypeInfo *lookupTypeInfo(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info;
  return lookupGlobalTypeInfo(name);
}

void rememberGlobalType(const std::string &name, TypeInfo info) {
  GlobalTypes[name] = std::move(info);
}

void rememberLocalType(const std::string &name, TypeInfo info) {
  LocalTypes[name] = std::move(info);
}

TypeInfo runtimeTypeFrom(const TypeInfo &declared, RefStorageClass storage,
                         bool declaredRefOverride) {
  TypeInfo info = declared;
  info.refStorage = storage;
  info.declaredRef = declaredRefOverride;
  return info;
}

bool validateTypeForGenerics(const TypeInfo &info,
                             const std::string &contextDescription,
                             const GenericDefinitionInfo *currentDefinition) {
  bool valid = true;
  if (!maybeReportNestedDepthIssues(info, contextDescription))
    valid = false;

  if (info.isGenericParameter) {
    if (!isActiveGenericParameter(info.baseTypeName)) {
      reportCompilerError("Unknown generic parameter '" + info.baseTypeName +
                          "' in " + contextDescription);
      valid = false;
    }
    if (info.hasTypeArguments()) {
      reportCompilerError("Generic parameter '" + info.baseTypeName +
                          "' cannot have type arguments in " + contextDescription);
      valid = false;
    }
  }

  size_t expectedArguments = 0;
  bool hasDefinition = false;

  const GenericDefinitionInfo *definition = nullptr;
  if (currentDefinition && currentDefinition->typeName == info.baseTypeName) {
    definition = currentDefinition;
  } else {
    for (auto it = ActiveGenericDefinitions.rbegin();
         it != ActiveGenericDefinitions.rend(); ++it) {
      if (*it && (*it)->typeName == info.baseTypeName) {
        definition = *it;
        break;
      }
    }
  }

  if (definition) {
    if (definition->parameters)
      expectedArguments = definition->parameters->size();
    hasDefinition = true;
  } else if (const CompositeTypeInfo *metadata =
                 lookupCompositeInfo(info.baseTypeName)) {
    expectedArguments = metadata->genericParameters.size();
    hasDefinition = true;
  } else if (StructAST *templateAst = FindGenericTemplate(info.baseTypeName)) {
    expectedArguments = templateAst->getGenericParameters().size();
    hasDefinition = expectedArguments > 0;
  }

  if (info.isSmartPointer()) {
    hasDefinition = true;
    expectedArguments = 1;
  }

  if (info.hasTypeArguments()) {
    if (!hasDefinition) {
      reportCompilerError("Type '" + info.baseTypeName +
                          "' does not accept generic arguments in " + contextDescription);
      valid = false;
    } else if (expectedArguments == 0) {
      reportCompilerError("Type '" + info.baseTypeName +
                          "' does not declare generic parameters but was used with type arguments in " + contextDescription);
      valid = false;
    } else if (info.typeArguments.size() != expectedArguments) {
      reportCompilerError("Type '" + info.baseTypeName + "' expects " +
                          std::to_string(expectedArguments) +
                          (expectedArguments == 1 ? " type argument" : " type arguments") +
                          " but received " +
                          std::to_string(info.typeArguments.size()) + " in " +
                          contextDescription);
      valid = false;
    }
  }

  for (const auto &arg : info.typeArguments) {
    if (!validateTypeForGenerics(arg, "type argument of '" + info.baseTypeName + "'",
                                 definition))
      valid = false;
  }

  if (info.isSmartPointer()) {
    if (info.hasTypeArguments() && info.typeArguments.size() != 1) {
      reportCompilerError("Smart pointer '" + info.baseTypeName + "' expects exactly one type argument");
      valid = false;
    }
  }

  if (!info.isGenericParameter && !info.hasTypeArguments()) {
    bool known = hasDefinition;
    if (!known) {
      if (lookupTypeInfo(info.baseTypeName))
        known = true;
      else if (getTypeFromString(info.baseTypeName))
        known = true;
      else if (info.isSmartPointer())
        known = true;
    }
    if (!known) {
      reportCompilerError("Unknown type '" + info.baseTypeName +
                          "' in " + contextDescription);
      valid = false;
    }
  }

  return valid;
}

bool isDeclaredRefGlobal(const std::string &name) {
  if (const auto *info = lookupGlobalTypeInfo(name))
    return info->declaredRef;
  return false;
}

bool isDeclaredRefLocal(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info->declaredRef;
  return false;
}

void ensureBaseNonNullScope() {
  if (NonNullFacts.empty())
    NonNullFacts.emplace_back();
}

std::set<std::string> currentNonNullFactsCopy() {
  if (NonNullFacts.empty())
    return {};
  return NonNullFacts.back();
}

void replaceCurrentNonNullFacts(const std::set<std::string> &facts) {
  ensureBaseNonNullScope();
  NonNullFacts.back() = facts;
}

void pushNonNullFactsFrom(const std::set<std::string> &seed) {
  NonNullFacts.push_back(seed);
}

void popNonNullFactsScope() {
  if (!NonNullFacts.empty())
    NonNullFacts.pop_back();
}

bool isKnownNonNull(const std::string &name) {
  for (auto It = NonNullFacts.rbegin(); It != NonNullFacts.rend(); ++It) {
    if (It->find(name) != It->end())
      return true;
  }
  return false;
}

void markKnownNonNull(const std::string &name) {
  ensureBaseNonNullScope();
  NonNullFacts.back().insert(name);
}

static void markKnownNullable(const std::string &name) {
  if (!NonNullFacts.empty())
    NonNullFacts.back().erase(name);
}

static NullComparisonRelation invertRelation(NullComparisonRelation relation) {
  return relation == NullComparisonRelation::EqualsNull
             ? NullComparisonRelation::NotEqualsNull
             : NullComparisonRelation::EqualsNull;
}

std::optional<NullComparison> extractNullComparison(const ExprAST *expr,
                                                    bool inverted) {
  if (!expr)
    return std::nullopt;

  if (const auto *typeCheck = dynamic_cast<const TypeCheckExprAST *>(expr)) {
    if (typeCheck->isNullCheck()) {
      const ExprAST *operand = unwrapRefExpr(typeCheck->getOperand());
      if (const auto *var = dynamic_cast<const VariableExprAST *>(operand)) {
        NullComparisonRelation relation =
            typeCheck->isNegated() ? NullComparisonRelation::NotEqualsNull
                                   : NullComparisonRelation::EqualsNull;
        if (inverted)
          relation = invertRelation(relation);
        return NullComparison{var->getName(), relation};
      }
    }
  }

  if (const auto *Unary = dynamic_cast<const UnaryExprAST *>(expr)) {
    if (Unary->getOp() == "!") {
      return extractNullComparison(Unary->getOperand(), !inverted);
    }
  }

  if (const auto *Binary = dynamic_cast<const BinaryExprAST *>(expr)) {
    const std::string &Op = Binary->getOp();
    if (Op == "==" || Op == "!=") {
      const ExprAST *lhs = unwrapRefExpr(Binary->getLHS());
      const ExprAST *rhs = unwrapRefExpr(Binary->getRHS());
      bool lhsNull = exprIsNullLiteral(lhs);
      bool rhsNull = exprIsNullLiteral(rhs);
      if (lhsNull == rhsNull)
        return std::nullopt;
      const ExprAST *candidate = lhsNull ? rhs : lhs;
      if (const auto *Var = dynamic_cast<const VariableExprAST *>(candidate)) {
        NullComparisonRelation relation =
            (Op == "!=") ? NullComparisonRelation::NotEqualsNull
                         : NullComparisonRelation::EqualsNull;
        if (inverted)
          relation = invertRelation(relation);
        return NullComparison{Var->getName(), relation};
      }
    }
  }

  return std::nullopt;
}

static bool variableSupportsNullComparison(const NullComparison &comparison) {
  if (const TypeInfo *info = lookupTypeInfo(comparison.variableName))
    return typeAllowsNull(*info);
  return false;
}

void applyNullComparisonToCurrentScope(const NullComparison &comparison,
                                       BranchKind branch) {
  if (!variableSupportsNullComparison(comparison))
    return;

  bool branchImpliesNonNull = false;
  bool branchImpliesNull = false;
  if (branch == BranchKind::Then) {
    branchImpliesNonNull =
        comparison.relation == NullComparisonRelation::NotEqualsNull;
    branchImpliesNull =
        comparison.relation == NullComparisonRelation::EqualsNull;
  } else {
    branchImpliesNonNull =
        comparison.relation == NullComparisonRelation::EqualsNull;
    branchImpliesNull =
        comparison.relation == NullComparisonRelation::NotEqualsNull;
  }

  if (branchImpliesNonNull)
    markKnownNonNull(comparison.variableName);
  if (branchImpliesNull)
    markKnownNullable(comparison.variableName);
}

std::set<std::string> intersectNonNullFacts(const std::set<std::string> &a,
                                            const std::set<std::string> &b) {
  if (a.empty() || b.empty())
    return {};
  const std::set<std::string> *smaller = &a;
  const std::set<std::string> *larger = &b;
  if (a.size() > b.size())
    std::swap(smaller, larger);
  std::set<std::string> result;
  for (const auto &item : *smaller) {
    if (larger->find(item) != larger->end())
      result.insert(item);
  }
  return result;
}

void updateKnownNonNullOnAssignment(const std::string &name,
                                    bool rhsIsNullable) {
  if (NonNullFacts.empty())
    return;
  if (const TypeInfo *info = lookupTypeInfo(name); info && typeAllowsNull(*info)) {
    markKnownNullable(name);
    if (!rhsIsNullable)
      markKnownNonNull(name);
  }
}

static void emitArcDebugInitializer() {
  if (!TheModule || !TheContext)
    return;
  if (!isArcLoweringEnabled())
    return;
  const ArcDebugOptions &debug = CG.arcDebug;
  if (!debug.runtimeTracing && !debug.leakDetection &&
      !debug.runtimeVerify && !debug.poolDebug)
    return;

  auto *fnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
  llvm::Function *initFn = llvm::Function::Create(
      fnType, llvm::GlobalValue::InternalLinkage,
      "__hybrid_arc_debug_init", TheModule.get());
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(*TheContext, "entry", initFn);
  llvm::IRBuilder<> initBuilder(entry);
  auto flag = [&](bool enabled) {
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext),
                                  enabled ? 1 : 0);
  };

  initBuilder.CreateCall(
      getHybridArcDebugConfigFunction(),
      {flag(debug.leakDetection), flag(debug.runtimeTracing),
       flag(debug.runtimeVerify), flag(debug.poolDebug)});
  initBuilder.CreateRetVoid();
  llvm::appendToGlobalCtors(*TheModule, initFn, 0);
}

// Initialize LLVM
void InitializeModule() {
  CG.reset();
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("Hybrid JIT", *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  
  // Add a simple print function that takes an int
  std::vector<llvm::Type*> PrintArgs = {llvm::Type::getInt32Ty(*TheContext)};
  llvm::FunctionType *PrintType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), PrintArgs, false);
  llvm::Function *PrintFunc = llvm::Function::Create(
      PrintType, llvm::Function::ExternalLinkage, "print", TheModule.get());

  FunctionOverload printOverload;
  printOverload.mangledName = "print";
  printOverload.returnType = makeTypeInfo("void");
  printOverload.returnsByRef = false;
  printOverload.parameterTypes.push_back(makeTypeInfo("int"));
  printOverload.parameterIsRef.push_back(false);
  printOverload.parameterIsParams.push_back(false);
  printOverload.isExtern = true;
  printOverload.function = PrintFunc;
  CG.functionOverloads["print"].push_back(std::move(printOverload));

  std::vector<llvm::Type *> PrintStringArgs = {getTypeFromString("string")};
  llvm::FunctionType *PrintStringType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                               PrintStringArgs, false);
  llvm::Function *PrintStringFunc = llvm::Function::Create(
      PrintStringType, llvm::Function::ExternalLinkage, "print_string",
      TheModule.get());

  FunctionOverload printStringOverload;
  printStringOverload.mangledName = "print_string";
  printStringOverload.returnType = makeTypeInfo("void");
  printStringOverload.returnsByRef = false;
  printStringOverload.parameterTypes.push_back(makeTypeInfo("string"));
  printStringOverload.parameterIsRef.push_back(false);
  printStringOverload.parameterIsParams.push_back(false);
  printStringOverload.isExtern = true;
  printStringOverload.function = PrintStringFunc;
  CG.functionOverloads["print"].push_back(std::move(printStringOverload));

  emitArcDebugInitializer();
}

// Get the LLVM module for printing
llvm::Module *getModule() {
  return TheModule.get();
}


// Error handling utilities
llvm::Value *LogErrorV(const char *Str, std::string_view hint) {
  reportCompilerError(Str, hint);
  return nullptr;
}

llvm::Value *LogErrorV(const std::string &Str, std::string_view hint) {
  return LogErrorV(Str.c_str(), hint);
}

llvm::Function *LogErrorF(const char *Str, std::string_view hint) {
  reportCompilerError(Str, hint);
  return nullptr;
}

// Helper to check if a type name represents a signed type
bool isSignedType(std::string_view TypeStr) {
  // Signed types: int, sbyte, short, long, char (in C, char is signed by default)
  // Note: schar is "short char" (8-bit), not signed char
  return TypeStr == "int" || TypeStr == "sbyte" || TypeStr == "short" ||
         TypeStr == "long" || TypeStr == "char";
}

// Helper to check if a type name represents an unsigned type
bool isUnsignedType(std::string_view TypeStr) {
  // Unsigned types: byte, ushort, uint, ulong
  // Note: schar and lchar are character types, treating as unsigned
  return TypeStr == "byte" || TypeStr == "ushort" || TypeStr == "uint" ||
         TypeStr == "ulong" || TypeStr == "schar" || TypeStr == "lchar";
}

bool isDecimalTypeName(std::string_view typeName) {
  if (typeName.empty())
    return false;
  return sanitizeBaseTypeName(typeName) == "decimal";
}

[[maybe_unused]] static bool isIntegerLikeTypeName(std::string_view typeName) {
  std::string clean = sanitizeBaseTypeName(typeName);
  if (clean.empty())
    return false;
  return clean == "byte" || clean == "sbyte" || clean == "short" ||
         clean == "ushort" || clean == "int" || clean == "uint" ||
         clean == "long" || clean == "ulong" || clean == "char" ||
         clean == "schar" || clean == "lchar";
}

std::string sanitizeBaseTypeName(std::string_view typeName) {
  if (typeName.empty())
    return {};

  ParsedTypeDescriptor desc = parseTypeString(std::string(typeName));
  return desc.sanitized;
}

std::optional<bool> unsignedHintFromTypeName(const std::string &typeName) {
  if (typeName.empty())
    return std::nullopt;

  if (typeName == "char" || typeName == "lchar")
    return true;

  if (isUnsignedType(typeName))
    return true;

  if (isSignedType(typeName))
    return false;

  return std::nullopt;
}

std::optional<MemberFieldAssignmentInfo>
collectMemberFieldAssignmentInfo(MemberAccessExprAST &member) {
  MemberFieldAssignmentInfo info;

  info.fieldPtr = member.codegen_ptr();
  if (!info.fieldPtr)
    return std::nullopt;

  if (auto tupleInfoOpt = resolveTupleTypeInfo(member.getObject());
      tupleInfoOpt && tupleInfoOpt->isTupleType()) {
    auto indexOpt = findTupleElementIndex(*tupleInfoOpt, member.getMemberName());
    if (!indexOpt) {
      reportCompilerError("Unknown tuple field '" + member.getMemberName() + "'");
      return std::nullopt;
    }

    TypeInfo elementInfo =
        applyActiveTypeBindings(tupleInfoOpt->typeArguments[*indexOpt]);
    finalizeTypeInfoMetadata(elementInfo);
    info.declaredFieldType = elementInfo;
    info.rawFieldTypeName = typeNameFromInfo(elementInfo);
    info.sanitizedFieldTypeName = sanitizeBaseTypeName(info.rawFieldTypeName);
    info.allowsNull = typeAllowsNull(elementInfo);
    info.isStatic = false;
    info.fieldType = getTypeFromString(info.rawFieldTypeName);
    return info;
  }

  std::string objectTypeName;
  if (auto *obj = member.getObject())
    objectTypeName = obj->getTypeName();
  ParsedTypeDescriptor objectDesc = parseTypeString(objectTypeName);
  info.structName = objectDesc.sanitized;
  if (info.structName.empty())
    info.structName = resolveCompositeName(member.getObject());

  std::string fieldTypeName;
  std::string cleanFieldTypeName;
  bool fieldIsStatic = false;
  std::string staticGlobalName;
  bool fieldAllowsNull = false;

  if (!info.structName.empty()) {
    if (const CompositeTypeInfo *comp = lookupCompositeInfo(info.structName)) {
      if (auto typeIt = comp->staticFieldTypes.find(member.getMemberName());
          typeIt != comp->staticFieldTypes.end()) {
        fieldTypeName = typeIt->second;
        ParsedTypeDescriptor fieldDesc = parseTypeString(fieldTypeName);
        fieldAllowsNull = typeAllowsNull(fieldDesc);
        cleanFieldTypeName = fieldDesc.sanitized;
        fieldIsStatic = true;
        if (auto globalIt = comp->staticFieldGlobals.find(member.getMemberName());
            globalIt != comp->staticFieldGlobals.end())
          staticGlobalName = globalIt->second;
      }
    }
  }

  if (fieldTypeName.empty() && !info.structName.empty()) {
    if (auto fieldTypesIt = StructFieldTypes.find(info.structName);
        fieldTypesIt != StructFieldTypes.end()) {
      if (auto typeIt = fieldTypesIt->second.find(member.getMemberName());
          typeIt != fieldTypesIt->second.end()) {
        fieldTypeName = typeIt->second;
        ParsedTypeDescriptor fieldDesc = parseTypeString(fieldTypeName);
        fieldAllowsNull = typeAllowsNull(fieldDesc);
        cleanFieldTypeName = fieldDesc.sanitized;
      }
    }
  }

  if (staticGlobalName.empty() && !info.structName.empty())
    staticGlobalName = info.structName + "." + member.getMemberName();

  llvm::Type *fieldType = nullptr;
  if (!fieldIsStatic && !info.structName.empty()) {
    if (auto structIt = StructTypes.find(info.structName);
        structIt != StructTypes.end()) {
      if (auto fieldIndicesIt = StructFieldIndices.find(info.structName);
          fieldIndicesIt != StructFieldIndices.end()) {
        for (const auto &entry : fieldIndicesIt->second) {
          if (entry.first == member.getMemberName()) {
            fieldType = structIt->second->getElementType(entry.second);
            break;
          }
        }
      }
    }
  }

  if (!fieldType && fieldIsStatic) {
    if (auto globalIt = CG.globalValues.find(staticGlobalName);
        globalIt != CG.globalValues.end()) {
      fieldType = globalIt->second->getValueType();
    }
  }

  if (!fieldType) {
    if (auto *GV = llvm::dyn_cast<llvm::GlobalVariable>(info.fieldPtr))
      fieldType = GV->getValueType();
  }

  if (!fieldType) {
    LogErrorV(("Internal error: missing type info for field '" + member.getMemberName() + "'").c_str());
    return std::nullopt;
  }

  if (cleanFieldTypeName.empty() && !fieldTypeName.empty()) {
    ParsedTypeDescriptor fieldDesc = parseTypeString(fieldTypeName);
    cleanFieldTypeName = fieldDesc.sanitized;
  }

  if (cleanFieldTypeName.empty())
    cleanFieldTypeName = sanitizeBaseTypeName(member.getTypeName());

  if (fieldTypeName.empty())
    fieldTypeName = member.getTypeName();

  info.declaredFieldType =
      applyActiveTypeBindings(makeTypeInfo(fieldTypeName));
  info.fieldType = fieldType;
  info.rawFieldTypeName = fieldTypeName;
  info.sanitizedFieldTypeName = cleanFieldTypeName;
  info.allowsNull = fieldAllowsNull;
  info.isStatic = fieldIsStatic;

  return info;
}

[[maybe_unused]] static bool isAssignmentExpression(const ExprAST *expr) {
  auto *binary = dynamic_cast<const BinaryExprAST *>(expr);
  if (!binary)
    return false;
  const std::string &op = binary->getOp();
  return op == "=" || op == "+=" || op == "-=" || op == "*=" || op == "/=" ||
         op == "%=" || op == "&=" || op == "|=" || op == "^=" ||
         op == "<<=" || op == ">>=" || op == "\?\?=";
}

[[maybe_unused]] static bool isLValueExpression(const ExprAST *expr) {
  if (!expr)
    return false;
  if (auto *paren = dynamic_cast<const ParenExprAST *>(expr)) {
    if (paren->isTuple() || paren->size() != 1)
      return false;
    return isLValueExpression(paren->getElement(0));
  }
  if (dynamic_cast<const VariableExprAST *>(expr))
    return true;
  if (dynamic_cast<const MemberAccessExprAST *>(expr))
    return true;
  if (dynamic_cast<const ArrayIndexExprAST *>(expr))
    return true;
  if (auto *unary = dynamic_cast<const UnaryExprAST *>(expr))
    return unary->getOp() == "@";
  return false;
}

bool isPointerTypeDescriptor(const ParsedTypeDescriptor &desc) {
  return desc.pointerDepth > 0 && !desc.isArray;
}

std::optional<std::string>
getPointerElementTypeName(const std::string &pointerTypeName) {
  size_t atPos = pointerTypeName.find('@');
  if (atPos == std::string::npos)
    return std::nullopt;

  unsigned depth = computePointerDepth(pointerTypeName);
  std::string base = pointerTypeName.substr(0, atPos);
  if (depth <= 1)
    return base;

  if (depth == 2)
    return base + "@";

  return base + "@" + std::to_string(depth - 1);
}

llvm::IntegerType *getPointerIndexType() {
  unsigned pointerBits = 64;
  if (TheModule) {
    const llvm::DataLayout &DL = TheModule->getDataLayout();
    if (!DL.getStringRepresentation().empty()) {
      unsigned size = DL.getPointerSizeInBits();
      if (size > 0)
        pointerBits = size;
    }
  }
  return llvm::IntegerType::get(*TheContext, pointerBits);
}

uint64_t getTypeSizeInBytes(llvm::Type *type) {
  if (!type)
    return 0;

  if (TheModule) {
    const llvm::DataLayout &DL = TheModule->getDataLayout();
    if (!DL.getStringRepresentation().empty())
      return DL.getTypeAllocSize(type);
  }

  if (type->isIntegerTy())
    return type->getIntegerBitWidth() / 8;

  if (type->isFloatingPointTy())
    return type->getPrimitiveSizeInBits() / 8;

  if (type->isPointerTy()) {
    llvm::IntegerType *indexTy = getPointerIndexType();
    return indexTy->getBitWidth() / 8;
  }

  if (auto *arrayTy = llvm::dyn_cast<llvm::ArrayType>(type)) {
    uint64_t elemSize = getTypeSizeInBytes(arrayTy->getElementType());
    if (elemSize == 0)
      return 0;
    return elemSize * arrayTy->getNumElements();
  }

  if (auto *structTy = llvm::dyn_cast<llvm::StructType>(type)) {
    if (structTy->isOpaque())
      return 0;
    uint64_t size = 0;
    uint64_t maxAlign = 1;
    auto alignTo = [](uint64_t value, uint64_t align) {
      if (align == 0)
        return value;
      uint64_t rem = value % align;
      if (rem == 0)
        return value;
      return value + (align - rem);
    };
    for (llvm::Type *elemTy : structTy->elements()) {
      uint64_t elemSize = getTypeSizeInBytes(elemTy);
      if (elemSize == 0)
        return 0;
      uint64_t elemAlign = elemSize;
      size = alignTo(size, elemAlign);
      size += elemSize;
      if (elemAlign > maxAlign)
        maxAlign = elemAlign;
    }
    size = alignTo(size, maxAlign);
    return size;
  }

  return 0;
}

bool emitArrayResizeAssignment(llvm::Value *storagePtr,
                               const TypeInfo &arrayInfo, NewExprAST &newExpr,
                               std::string_view label,
                               std::vector<int64_t> *constantDimsOut) {
  if (!storagePtr || !arrayInfo.isArray)
    return false;

  ParsedTypeDescriptor desc = parseTypeString(arrayInfo.typeName);
  if (!desc.isArray)
    return false;

  const auto &bounds = newExpr.getArraySizes();
  if (bounds.empty())
    return false;

  unsigned rank =
      desc.arrayRanks.empty() ? 1 : std::max(1u, desc.arrayRanks.back());
  if (bounds.size() != rank) {
    reportCompilerError(
        "Array bounds do not match declared rank for '" +
            typeNameFromInfo(arrayInfo) + "'",
        "Expected " + std::to_string(rank) + " bound(s) in 'new'.");
    return false;
  }

  auto boundsInfoOpt =
      emitArrayBoundsInfo(bounds, buildArcOpLabel(label, "array.resize.bounds"));
  if (!boundsInfoOpt)
    return false;
  const ArrayBoundsInfo &boundsInfo = *boundsInfoOpt;
  if (constantDimsOut)
    *constantDimsOut = boundsInfo.constantDims;

  std::string elementTypeName = removeLastArrayGroup(desc.sanitized);
  if (elementTypeName.empty())
    elementTypeName = arrayInfo.baseTypeName;
  llvm::Type *elemType = getTypeFromString(elementTypeName);
  if (!elemType) {
    reportCompilerError("Unknown array element type '" + elementTypeName + "'");
    return false;
  }
  TypeInfo elementInfo = makeTypeInfo(elementTypeName);
  finalizeTypeInfoMetadata(elementInfo);

  uint64_t elemSize = getTypeSizeInBytes(elemType);
  if (elemSize == 0) {
    reportCompilerError(
        "Unable to determine element size for array allocation of '" +
        elementTypeName + "'");
    return false;
  }

  llvm::StructType *arrayStructTy = getArrayStructType(elemType, rank);
  llvm::Value *oldArrayValue = Builder->CreateLoad(
      arrayStructTy, storagePtr, buildArcOpLabel(label, "array.resize.old"));
  llvm::Value *oldHeader = computeArrayHeaderPointer(
      oldArrayValue, buildArcOpLabel(label, "array.resize.header"));

  llvm::Value *elemSizeVal =
      llvm::ConstantInt::get(getSizeType(), elemSize);
  llvm::Value *releaseFn = selectArrayElementReleaseFunction(
      elementInfo, buildArcOpLabel(label, "array.resize.releasefn"));
  llvm::Value *retainFn = selectArrayElementRetainFunction(
      elementInfo, buildArcOpLabel(label, "array.resize.retainfn"));
  llvm::Value *rawPtr = Builder->CreateCall(
      getHybridArrayResizeFunction(),
      {oldHeader, elemSizeVal, boundsInfo.totalSize, releaseFn, retainFn},
      buildArcOpLabel(label, "array.resize.raw"));

  llvm::Value *rawBytePtr = Builder->CreateBitCast(
      rawPtr, pointerType(llvm::Type::getInt8Ty(*TheContext)),
      buildArcOpLabel(label, "array.resize.byteptr"));
  llvm::Value *payloadBytePtr = Builder->CreateInBoundsGEP(
      llvm::Type::getInt8Ty(*TheContext), rawBytePtr,
      llvm::ConstantInt::get(getSizeType(), getArrayPayloadOffsetBytes()),
      buildArcOpLabel(label, "array.resize.payload.byte"));
  llvm::Value *dataPtr = Builder->CreateBitCast(
      payloadBytePtr, pointerType(elemType),
      buildArcOpLabel(label, "array.resize.payload"));

  llvm::Value *arrayValue = llvm::UndefValue::get(arrayStructTy);
  llvm::Value *opaqueDataPtr = Builder->CreateBitCast(
      dataPtr, pointerType(), buildArcOpLabel(label, "array.resize.ptr"));
  arrayValue = Builder->CreateInsertValue(arrayValue, opaqueDataPtr, {0});
  arrayValue =
      Builder->CreateInsertValue(arrayValue, boundsInfo.totalSize32, {1});
  for (unsigned i = 0; i < rank; ++i) {
    llvm::Value *dimVal = boundsInfo.dims32[i];
    arrayValue = Builder->CreateInsertValue(arrayValue, dimVal, {2u, i});
  }

  emitManagedStore(storagePtr, arrayValue, arrayInfo, label, true);
  return true;
}

static ExprAST *unwrapSingleParenExpr(ExprAST *expr) {
  while (auto *paren = dynamic_cast<ParenExprAST *>(expr)) {
    if (paren->isTuple() || paren->size() != 1)
      break;
    expr = paren->getElement(0);
  }
  return expr;
}

NewExprAST *extractNewArrayExpr(ExprAST *expr) {
  expr = unwrapSingleParenExpr(expr);
  auto *newExpr = dynamic_cast<NewExprAST *>(expr);
  if (newExpr && newExpr->isArray())
    return newExpr;
  return nullptr;
}

const ArrayExprAST *extractArrayLiteralExpr(const ExprAST *expr) {
  auto *mutableExpr = const_cast<ExprAST *>(expr);
  mutableExpr = unwrapSingleParenExpr(mutableExpr);
  return dynamic_cast<const ArrayExprAST *>(mutableExpr);
}

static llvm::Value *convertOffsetToPointerIndex(llvm::Value *offsetValue,
                                                const std::string &offsetTypeName) {
  if (!offsetValue->getType()->isIntegerTy())
    return LogErrorV("Pointer arithmetic requires an integer offset");

  llvm::IntegerType *indexType = getPointerIndexType();
  if (offsetValue->getType() == indexType)
    return offsetValue;

  unsigned offsetBits = offsetValue->getType()->getIntegerBitWidth();
  unsigned indexBits = indexType->getIntegerBitWidth();

  std::string cleanName = sanitizeBaseTypeName(offsetTypeName);
  bool isUnsigned = unsignedHintFromTypeName(cleanName).value_or(false);

  if (offsetBits > indexBits)
    return Builder->CreateTrunc(offsetValue, indexType, "ptroff.trunc");

  if (isUnsigned)
    return Builder->CreateZExt(offsetValue, indexType, "ptroff.zext");

  return Builder->CreateSExt(offsetValue, indexType, "ptroff.sext");
}

llvm::Value *emitPointerOffset(llvm::Value *ptrValue,
                               llvm::Value *offsetValue,
                               const std::string &pointerTypeName,
                               const std::string &offsetTypeName,
                               bool negateOffset,
                               const char *name) {
  ParsedTypeDescriptor ptrDesc = parseTypeString(pointerTypeName);
  if (!isPointerTypeDescriptor(ptrDesc))
    return LogErrorV("Pointer arithmetic requires pointer operands");

  llvm::Value *indexValue = convertOffsetToPointerIndex(offsetValue, offsetTypeName);
  if (!indexValue)
    return nullptr;

  if (negateOffset)
    indexValue = Builder->CreateNeg(indexValue, "ptroff.neg");

  auto elementNameOpt = getPointerElementTypeName(ptrDesc.sanitized);
  if (!elementNameOpt)
    return LogErrorV("Cannot determine element type for pointer arithmetic");

  llvm::Type *elementType = getTypeFromString(*elementNameOpt);
  if (!elementType)
    return LogErrorV("Unsupported element type for pointer arithmetic");

  return Builder->CreateInBoundsGEP(elementType, ptrValue, indexValue, name);
}

static bool isIntegerLiteralExpr(const ExprAST *expr) {
  if (!expr)
    return false;

  if (dynamic_cast<const NumberExprAST *>(expr))
    return true;

  if (dynamic_cast<const CharExprAST *>(expr))
    return true;

  ConstantValue constantResult(0LL);
  if (EvaluateConstantExpression(expr, constantResult)) {
    return constantResult.type == ConstantValue::INTEGER ||
           constantResult.type == ConstantValue::UNSIGNED_INTEGER;
  }

  return false;
}

std::string describeTypeForDiagnostic(llvm::Type *type) {
  if (!type)
    return "value";

  if (const DelegateTypeInfo *delegateInfo = lookupDelegateInfo(type)) {
    return "delegate '" + delegateInfo->name + "'";
  }

  if (type->isIntegerTy()) {
    const unsigned bits = type->getIntegerBitWidth();
    if (bits == 1)
      return "bool";
    return std::to_string(bits) + "-bit integer";
  }

  if (type->isFloatTy())
    return "float";
  if (type->isDoubleTy())
    return "double";
  if (isDecimalLLVMType(type))
    return "decimal";
  if (type->isPointerTy())
    return "pointer";
  return "value";
}

std::string sanitizeBaseTypeName(std::string_view typeName);

static IntegerRangeInfo makeSignedRange(std::string_view typeName,
                                        long long minValue,
                                        long long maxValue) {
  IntegerRangeInfo info;
  info.typeName = std::string(typeName);
  info.isSigned = true;
  info.minSigned = minValue;
  info.maxSigned = maxValue;
  info.minText = std::to_string(minValue);
  info.maxText = std::to_string(maxValue);
  return info;
}

static IntegerRangeInfo makeUnsignedRange(std::string_view typeName,
                                          unsigned long long maxValue) {
  IntegerRangeInfo info;
  info.typeName = std::string(typeName);
  info.isSigned = false;
  info.maxUnsigned = maxValue;
  info.minText = "0";
  info.maxText = std::to_string(maxValue);
  return info;
}

std::optional<IntegerRangeInfo> getIntegerRangeInfo(std::string_view typeName) {
  if (typeName == "byte")
    return makeUnsignedRange("byte", 255ULL);
  if (typeName == "sbyte")
    return makeSignedRange("sbyte", -128LL, 127LL);
  if (typeName == "short")
    return makeSignedRange("short", -32768LL, 32767LL);
  if (typeName == "ushort")
    return makeUnsignedRange("ushort", 65535ULL);
  if (typeName == "int")
    return makeSignedRange("int", -2147483648LL, 2147483647LL);
  if (typeName == "uint")
    return makeUnsignedRange("uint", 4294967295ULL);
  if (typeName == "long")
    return makeSignedRange("long",
                           std::numeric_limits<long long>::min(),
                           std::numeric_limits<long long>::max());
  if (typeName == "ulong")
    return makeUnsignedRange("ulong",
                             std::numeric_limits<unsigned long long>::max());
  return std::nullopt;
}

static std::string formatIntegerLiteralValue(const llvm::APInt &value,
                                             bool isSigned) {
  if (isSigned)
    return std::to_string(value.getSExtValue());
  return std::to_string(value.getZExtValue());
}

std::string buildIntegerRangeError(const llvm::APInt &value,
                                   std::string_view targetTypeName) {
  std::string clean = sanitizeBaseTypeName(targetTypeName);
  if (clean.empty())
    clean = std::string(targetTypeName);

  auto infoOpt = getIntegerRangeInfo(clean);
  if (!infoOpt) {
    std::string valueText = formatIntegerLiteralValue(value, true);
    std::string message = "Integer literal " + valueText + " is out of range";
    if (!clean.empty())
      message += " for target type " + clean;
    return message;
  }

  const IntegerRangeInfo &info = *infoOpt;
  std::string valueText = formatIntegerLiteralValue(value, info.isSigned);
  return valueText + " exceeds " + info.typeName + " range [" +
         info.minText + "-" + info.maxText + "]";
}

bool integerLiteralFitsRange(const llvm::APInt &value,
                             const IntegerRangeInfo &info) {
  if (info.isSigned) {
    long long v = value.getSExtValue();
    return v >= info.minSigned && v <= info.maxSigned;
  }
  unsigned long long v = value.getZExtValue();
  return v <= info.maxUnsigned;
}

static bool requiresExplicitCastForIntegerConversion(const ExprAST *sourceExpr,
                                                     llvm::Type *sourceType,
                                                     std::string_view sourceTypeName,
                                                     llvm::Type *targetType,
                                                     std::string_view targetTypeName) {
  if (!sourceType || !targetType)
    return false;

  if (!sourceType->isIntegerTy() || !targetType->isIntegerTy())
    return false;

  if (sourceType->isIntegerTy(1) || targetType->isIntegerTy(1))
    return false;

  const unsigned sourceBits = sourceType->getIntegerBitWidth();
  const unsigned targetBits = targetType->getIntegerBitWidth();
  const bool sourceIsLiteral = isIntegerLiteralExpr(sourceExpr);

  if (sourceBits > targetBits)
    return !sourceIsLiteral;

  std::string cleanSource = sanitizeBaseTypeName(sourceTypeName);
  std::string cleanTarget = sanitizeBaseTypeName(targetTypeName);

  auto sourceUnsigned = unsignedHintFromTypeName(cleanSource);
  auto targetUnsigned = unsignedHintFromTypeName(cleanTarget);

  if (sourceUnsigned && targetUnsigned &&
      sourceUnsigned.value() != targetUnsigned.value()) {
    return !sourceIsLiteral;
  }

  return false;
}

bool diagnoseDisallowedImplicitIntegerConversion(
    const ExprAST *sourceExpr, llvm::Value *sourceValue,
    llvm::Type *targetType, std::string_view targetTypeName,
    std::string_view contextDescription) {
  if (!sourceExpr || !sourceValue || !targetType)
    return false;

  if (!requiresExplicitCastForIntegerConversion(sourceExpr,
                                                sourceValue->getType(),
                                                sourceExpr->getTypeName(),
                                                targetType,
                                                targetTypeName))
    return false;

  std::string cleanSource = sanitizeBaseTypeName(sourceExpr->getTypeName());
  if (cleanSource.empty())
    cleanSource = describeTypeForDiagnostic(sourceValue->getType());

  std::string cleanTarget = sanitizeBaseTypeName(std::string(targetTypeName));
  if (cleanTarget.empty())
    cleanTarget = describeTypeForDiagnostic(targetType);

  std::string message = "Cannot implicitly convert '" + cleanSource +
                        "' to '" + cleanTarget + "'";
  if (!contextDescription.empty()) {
    message += " in ";
    message.append(contextDescription);
  }
  message += "; explicit cast required";

  LogErrorV(message.c_str());
  return true;
}

llvm::FunctionType *ensureDelegateFunctionType(DelegateTypeInfo &info) {
  if (info.functionType)
    return info.functionType;

  std::vector<llvm::Type *> paramTypes;
  paramTypes.reserve(info.parameterTypes.size() + 1);
  paramTypes.push_back(pointerType());

  for (size_t idx = 0; idx < info.parameterTypes.size(); ++idx) {
    std::string paramTypeName = typeNameFromInfo(info.parameterTypes[idx]);
    llvm::Type *paramType = getTypeFromString(paramTypeName);
    if (!paramType) {
      reportCompilerError("Unknown parameter type '" + paramTypeName +
                          "' in delegate '" + info.name + "'");
      return nullptr;
    }
    if (idx < info.parameterIsRef.size() && info.parameterIsRef[idx])
      paramType = pointerType();
    paramTypes.push_back(paramType);
  }

  std::string returnTypeName = typeNameFromInfo(info.returnType);
  llvm::Type *retType = getTypeFromString(returnTypeName);
  if (!retType) {
    reportCompilerError("Unknown return type '" + returnTypeName +
                        "' in delegate '" + info.name + "'");
    return nullptr;
  }
  if (info.returnsByRef)
    retType = pointerType();

  info.functionType = llvm::FunctionType::get(retType, paramTypes, false);
  return info.functionType;
}

llvm::StructType *ensureDelegateStructType(DelegateTypeInfo &info) {
  if (info.structType)
    return info.structType;
  llvm::FunctionType *fnType = ensureDelegateFunctionType(info);
  if (!fnType)
    return nullptr;
  std::string structName = "delegate." + info.name;
  info.structType = llvm::StructType::create(
      *TheContext, {pointerType(fnType), pointerType()}, structName);
  return info.structType;
}

// Type conversion helper
llvm::Type *getTypeFromString(const std::string &TypeStr) {
  std::string CleanType = stripNullableAnnotations(TypeStr);
  if (const auto *bindings = currentTypeBindings()) {
    TypeInfo info = makeTypeInfo(CleanType);
    TypeInfo substituted = substituteTypeInfo(info, *bindings);
    std::string boundName = stripNullableAnnotations(typeNameFromInfo(substituted));
    if (boundName != CleanType)
      return getTypeFromString(boundName);
    CleanType = boundName;
  }

  if (CleanType == "int")
    return llvm::Type::getInt32Ty(*TheContext);
  else if (CleanType == "float")
    return llvm::Type::getFloatTy(*TheContext);
  else if (CleanType == "double")
    return llvm::Type::getDoubleTy(*TheContext);
  else if (CleanType == "decimal")
    return getDecimalStorageType();
  else if (CleanType == "char")
    return llvm::Type::getInt16Ty(*TheContext);
  else if (CleanType == "bool")
    return llvm::Type::getInt8Ty(*TheContext);
  else if (CleanType == "void")
    return llvm::Type::getVoidTy(*TheContext);
  else if (CleanType == "string")
    return pointerType(getStringStorageType());
  // New sized integer types
  else if (CleanType == "byte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit unsigned
  else if (CleanType == "sbyte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit signed
  else if (CleanType == "short")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit signed
  else if (CleanType == "ushort")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit unsigned
  else if (CleanType == "uint")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit unsigned
  else if (CleanType == "long")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit signed
  else if (CleanType == "ulong")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit unsigned
  // Sized character types
  else if (CleanType == "schar")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit character
  else if (CleanType == "lchar")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit character (Unicode)
  else if (isArrayTypeName(CleanType)) {
    unsigned rank = std::max(1u, getLastArrayGroupRank(CleanType));
    std::string ElementType = removeLastArrayGroup(CleanType);
    llvm::Type *ElemType = getTypeFromString(ElementType);
    if (ElemType)
      return getArrayStructType(ElemType, rank);
    return nullptr;
  }
  // Check for pointer types (e.g. "int@", "float@2")
  else if (CleanType.find('@') != std::string::npos) {
    size_t atPos = CleanType.find('@');
    std::string BaseType = CleanType.substr(0, atPos);

    // Parse pointer level (default is 1)
    int level = 1;
    if (atPos + 1 < CleanType.size()) {
      std::string levelStr = CleanType.substr(atPos + 1);
      if (!levelStr.empty()) {
        level = std::stoi(levelStr);
      }
    }

    // Get the base type
    llvm::Type *BaseLLVMType = getTypeFromString(BaseType);
    if (!BaseLLVMType)
      return nullptr;

    // Create nested pointer types based on level
    llvm::Type *Result = BaseLLVMType;
    for (int i = 0; i < level; i++) {
      Result = llvm::PointerType::get(*TheContext, 0);
    }

    return Result;
  }

  if (auto *delegateInfo = lookupDelegateInfoMutable(CleanType)) {
    llvm::StructType *structTy = ensureDelegateStructType(*delegateInfo);
    return structTy;
  }

  // Check if it's a struct type
  if (CleanType.find('<') != std::string::npos)
    lookupCompositeInfo(CleanType);

  auto structIt = StructTypes.find(CleanType);
  if (structIt != StructTypes.end()) {
    if (const CompositeTypeInfo *meta =
            lookupCompositeInfo(CleanType, /*countHit=*/false)) {
      if (meta->smartPointerKind != SmartPointerKind::None)
        return structIt->second;
    }
    return llvm::PointerType::get(*TheContext, 0); // Struct instances are opaque pointers
  }

  return nullptr;
}

struct PackedDecimalBits {
  uint64_t lo = 0;
  uint64_t hi = 0;
};

static PackedDecimalBits packDecimalBits(long double value) {
  PackedDecimalBits bits{};
  std::memcpy(&bits, &value, std::min(sizeof(bits), sizeof(value)));
  return bits;
}

llvm::Constant *buildDecimalConstantFromLongDouble(long double value) {
  PackedDecimalBits bits = packDecimalBits(value);
  llvm::Constant *lo =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), bits.lo);
  llvm::Constant *hi =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), bits.hi);
  return llvm::ConstantStruct::get(getDecimalStorageType(), {lo, hi});
}

llvm::Constant *buildDecimalConstantFromSpelling(
    const std::string &spelling) {
  errno = 0;
  char *end = nullptr;
  long double value = std::strtold(spelling.c_str(), &end);
  if (end != spelling.c_str() + spelling.size() || errno == ERANGE ||
      !std::isfinite(value)) {
    return buildDecimalConstantFromLongDouble(0.0L);
  }
  return buildDecimalConstantFromLongDouble(value);
}

// Helper to get array struct type for a given element type
llvm::StructType *getArrayStructType(llvm::Type *ElementType, unsigned rank) {
  llvm::Type *PtrType = llvm::PointerType::get(*TheContext, 0);
  llvm::Type *SizeType = llvm::Type::getInt32Ty(*TheContext);
  llvm::ArrayType *DimsType = llvm::ArrayType::get(SizeType, std::max(1u, rank));
  return llvm::StructType::get(*TheContext, {PtrType, SizeType, DimsType});
}

// AST implementations (currently all inline in the header)

// Add print method for debugging
void ForEachStmtAST::print() const {
  std::cout << "Parsed a foreach loop: for "
            << (isRef() ? "ref " : "") << getTypeName() << " " << VarName
            << " in <expression>" << std::endl;
}

void ForLoopStmtAST::print() const {
  std::cout << "Parsed a for loop: for " << Type << " " << VarName << " = <init> to <limit>" << std::endl;
}
