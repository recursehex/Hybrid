#include "ast_internal.h"

//===----------------------------------------------------------------------===//
// Function and Prototype Code Generation
void PrototypeAST::prependImplicitParameter(Parameter Param) {
  Args.insert(Args.begin(), std::move(Param));
  MangledName.clear();
}

//===----------------------------------------------------------------------===//

const std::string &PrototypeAST::getMangledName() const {
  auto buildSignature = [this]() {
    std::string signature;
    signature.reserve(32 + Args.size() * 16);

    TypeInfo boundReturn = applyActiveTypeBindings(ReturnTypeInfo);
    signature.append("R");
    signature.push_back(returnsByRef() ? 'R' : 'V');
    signature.push_back('_');
    signature.append(sanitizeForMangle(typeNameFromInfo(boundReturn)));
    signature.append("_P");
    signature.append(std::to_string(Args.size()));

    for (const auto &Param : Args) {
      signature.push_back('_');
      signature.push_back(Param.IsRef ? 'R' : 'V');
      signature.push_back('_');
      TypeInfo boundParam = applyActiveTypeBindings(Param.DeclaredType);
      signature.append(sanitizeForMangle(typeNameFromInfo(boundParam)));
    }

    return signature;
  };

  if (currentTypeBindings()) {
    thread_local std::string BoundMangledName;
    BoundMangledName = Name + "$" + buildSignature();
    return BoundMangledName;
  }

  if (!MangledName.empty())
    return MangledName;

  if (IsExtern || Name.rfind("__", 0) == 0) {
    MangledName = Name;
    return MangledName;
  }

  if (Name == "main") {
    const bool hasParameters = !Args.empty();
    const bool returnsVoid = ReturnTypeInfo.typeName == "void" && !returnsByRef();
    if (!hasParameters && !returnsVoid) {
      MangledName = Name;
      return MangledName;
    }
  }

  MangledName = Name + "$" + buildSignature();
  return MangledName;
}

llvm::Type *DelegateDeclAST::codegen() {
  if (!resolveParameterDefaults(Params, Name))
    return nullptr;

  std::string cleanName = stripNullableAnnotations(Name);
  if (lookupDelegateInfo(cleanName)) {
    reportCompilerError("Delegate '" + cleanName + "' is already defined");
    return nullptr;
  }

  if (lookupCompositeInfo(cleanName, /*countHit=*/false)) {
    reportCompilerError("Delegate '" + cleanName + "' conflicts with existing type");
    return nullptr;
  }

  DelegateTypeInfo info;
  info.name = cleanName;
  info.returnType = applyActiveTypeBindings(ReturnTypeInfo);
  info.returnsByRef = ReturnsByRef;
  info.parameterTypes = gatherParamTypes(Params);
  info.parameterIsRef = gatherParamRefFlags(Params);
  info.parameterIsParams = gatherParamParamsFlags(Params);
  info.parameterNames.reserve(Params.size());
  info.parameterDefaults.reserve(Params.size());
  info.parameterDefaultLocations.reserve(Params.size());

  if (!validateTypeForGenerics(info.returnType,
                               "return type of delegate '" + cleanName + "'"))
    return nullptr;

  for (const auto &param : Params) {
    TypeInfo boundParam = applyActiveTypeBindings(param.DeclaredType);
    if (!validateTypeForGenerics(boundParam,
                                 "parameter '" + param.Name +
                                     "' of delegate '" + cleanName + "'"))
      return nullptr;
    info.parameterNames.push_back(param.Name);
    info.parameterDefaults.push_back(param.ResolvedDefault);
    info.parameterDefaultLocations.push_back(param.DefaultEqualsLocation);
  }

  if (!ensureDelegateStructType(info))
    return nullptr;

  CG.delegateTypes[cleanName] = std::move(info);
  return CG.delegateTypes[cleanName].structType;
}

// Generate code for function prototypes
llvm::Function *PrototypeAST::codegen() {
  if (!ensureNoDuplicateGenericParameters(getGenericParameters(),
                                          "function '" + Name + "'"))
    return nullptr;
  SemanticGenericParameterScope prototypeGenerics(getGenericParameters());
  if (!resolveParameterDefaults(getMutableArgs(), Name))
    return nullptr;
  std::vector<llvm::Type*> ParamTypes;
  ParamTypes.reserve(Args.size());
  for (const auto &Param : Args) {
    TypeInfo boundParam = applyActiveTypeBindings(Param.DeclaredType);
    if (!validateTypeForGenerics(boundParam,
                                 "parameter '" + Param.Name + "' of function '" + Name + "'"))
      return nullptr;
    llvm::Type *ParamType = getTypeFromString(boundParam.typeName);
    if (!ParamType)
      return LogErrorF("Unknown parameter type");
    if (Param.IsRef) {
      ParamTypes.push_back(llvm::PointerType::get(*TheContext, 0));
    } else {
      ParamTypes.push_back(ParamType);
    }
  }

  TypeInfo boundReturn = applyActiveTypeBindings(ReturnTypeInfo);
  if (!validateTypeForGenerics(boundReturn,
                               "return type of function '" + Name + "'"))
    return nullptr;

  llvm::Type *RetType = getTypeFromString(boundReturn.typeName);
  if (!RetType)
    return LogErrorF("Unknown return type");

  if (returnsByRef())
    RetType = llvm::PointerType::get(*TheContext, 0);

  llvm::FunctionType *FT = llvm::FunctionType::get(RetType, ParamTypes, false);

  const std::string &Mangled = getMangledName();

  llvm::Function *F = TheModule->getFunction(Mangled);
  if (!F) {
    F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Mangled, TheModule.get());
  } else if (F->getFunctionType() != FT) {
    return LogErrorF("Function redefinition with incompatible signature");
  }

  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++].Name);

  FunctionOverload *entry = registerFunctionOverload(*this, Mangled);
  if (!entry) {
    F->eraseFromParent();
    return nullptr;
  }
  entry->function = F;

  return F;
}

static void ensureVoidMainWrapper(llvm::Function *UserMain) {
  if (!UserMain)
    return;

  auto *Int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Function *EntryFunc = ScriptMainFunction;

  if (!EntryFunc) {
    auto *MainType = llvm::FunctionType::get(Int32Ty, false);
    EntryFunc = llvm::Function::Create(MainType, llvm::Function::ExternalLinkage, "main", TheModule.get());
    ScriptMainFunction = EntryFunc;
  } else if (EntryFunc->getReturnType() != Int32Ty) {
    EntryFunc->eraseFromParent();
    auto *MainType = llvm::FunctionType::get(Int32Ty, false);
    EntryFunc = llvm::Function::Create(MainType, llvm::Function::ExternalLinkage, "main", TheModule.get());
    ScriptMainFunction = EntryFunc;
  }

  EntryFunc->deleteBody();
  llvm::BasicBlock *EntryBB = llvm::BasicBlock::Create(*TheContext, "entry", EntryFunc);
  llvm::IRBuilder<> TmpBuilder(*TheContext);
  TmpBuilder.SetInsertPoint(EntryBB);
  TmpBuilder.CreateCall(UserMain);
  TmpBuilder.CreateRet(llvm::ConstantInt::get(Int32Ty, 0));
  llvm::verifyFunction(*EntryFunc);
  ScriptMainIsSynthetic = false;
}

// Generate code for function definitions
llvm::Function *FunctionAST::codegen() {
  const bool isSourceMain = getProto()->getName() == "main";
  const bool isVoidMain = isSourceMain &&
                          getProto()->getReturnType() == "void" &&
                          !getProto()->returnsByRef();

  const std::string &MangledName = getProto()->getMangledName();
  llvm::Function *TheFunction = TheModule->getFunction(MangledName);

  if (!TheFunction)
    TheFunction = getProto()->codegen();

  if (!TheFunction)
    return nullptr;

  currentAnalysis().analyzeFunction(*this);
  ActiveLifetimePlanScope lifetimeScope(currentAnalysis().planFor(*this));

  SemanticGenericParameterScope functionGenerics(getProto()->getGenericParameters());
  FunctionOverload *overloadEntry = lookupFunctionOverload(*getProto());
  if (overloadEntry)
    overloadEntry->function = TheFunction;

  if (isSourceMain && ScriptMainIsSynthetic &&
      ScriptMainFunction && ScriptMainFunction == TheFunction) {
    TheFunction->deleteBody();
    ScriptMainIsSynthetic = false;
  }

  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  bool functionCodegenSucceeded = false;
  {
    ArcScopeGuard functionScope("function");

    NamedValues.clear();
    LocalTypes.clear();
    NonNullFacts.clear();
    ensureBaseNonNullScope();

  for (auto it = ArraySizes.begin(); it != ArraySizes.end();) {
    if (NamedValues.count(it->first) || LocalTypes.count(it->first)) {
      it = ArraySizes.erase(it);
    } else {
      ++it;
    }
  }

  const auto &Params = getProto()->getArgs();
  size_t i = 0;

  for (auto &Arg : TheFunction->args()) {
    bool isRefParam = (i < Params.size() && Params[i].IsRef);

    llvm::AllocaInst *Alloca =
        isRefParam ? nullptr
                   : Builder->CreateAlloca(Arg.getType(), nullptr,
                                           Arg.getName());
    if (i < Params.size()) {
      TypeInfo paramInfo = applyActiveTypeBindings(Params[i].DeclaredType);
      if (isRefParam) {
        paramInfo.refStorage = RefStorageClass::RefAlias;
        paramInfo.declaredRef = true;
        NamedValues[std::string(Arg.getName())] = &Arg;
        rememberLocalType(std::string(Arg.getName()), paramInfo);
      } else {
        if (typeNeedsLifetimeTracking(paramInfo)) {
          llvm::Value *zeroInit =
              llvm::Constant::getNullValue(Arg.getType());
          Builder->CreateStore(zeroInit, Alloca);
          emitManagedStore(Alloca, &Arg, paramInfo, Arg.getName().str());
        } else {
          Builder->CreateStore(&Arg, Alloca);
        }
        NamedValues[std::string(Arg.getName())] = Alloca;
        rememberLocalType(std::string(Arg.getName()), paramInfo);
        registerArcLocal(std::string(Arg.getName()), Alloca, paramInfo,
                         false);
      }
    } else {
      if (!Alloca)
        Alloca = Builder->CreateAlloca(Arg.getType(), nullptr, Arg.getName());
      Builder->CreateStore(&Arg, Alloca);
      NamedValues[std::string(Arg.getName())] = Alloca;
    }
    ++i;
  }

  if (const ActiveCompositeContext *ctx = currentCompositeContext()) {
    if (!ctx->isStatic) {
      auto ThisIt = NamedValues.find("__hybrid_this");
      if (ThisIt != NamedValues.end()) {
        llvm::Value *storage = ThisIt->second;
        llvm::Value *ThisPtr = storage;
        if (const TypeInfo *thisInfo =
                lookupLocalTypeInfo("__hybrid_this")) {
          ThisPtr = materializeAliasPointer(storage, *thisInfo, "this");
        } else if (auto *Alloca =
                       llvm::dyn_cast<llvm::AllocaInst>(storage)) {
          ThisPtr = Builder->CreateLoad(Alloca->getAllocatedType(), Alloca,
                                        "this");
        }
        if (ThisPtr) {
          NamedValues["this"] = ThisPtr;
          TypeInfo thisInfo = makeTypeInfo(ctx->name);
          thisInfo.refStorage = RefStorageClass::RefAlias;
          thisInfo.declaredRef = true;
          rememberLocalType("this", std::move(thisInfo));
          markKnownNonNull("this");
        }
      }
    }
  }

  static bool TopLevelExecEmitted = false;
  if (!TopLevelExecEmitted && isSourceMain && TopLevelExecFunction) {
    Builder->CreateCall(TopLevelExecFunction);
    TopLevelExecEmitted = true;
  }

    if (getBody()->codegen()) {
      if (!Builder->GetInsertBlock()->getTerminator()) {
        if (TheFunction->getReturnType()->isVoidTy()) {
          Builder->CreateRetVoid();
        } else {
          Builder->CreateRet(llvm::Constant::getNullValue(TheFunction->getReturnType()));
        }
      }

      llvm::verifyFunction(*TheFunction);
      functionCodegenSucceeded = true;
    }

    if (!functionCodegenSucceeded) {
      if (Builder)
        Builder->ClearInsertionPoint();
      CG.arcScopeStack.clear();
      TheFunction->eraseFromParent();
      if (overloadEntry)
        overloadEntry->function = nullptr;
    }
  }

  Builder->ClearInsertionPoint();
  NamedValues.clear();
  LocalTypes.clear();
  NonNullFacts.clear();

  if (functionCodegenSucceeded) {
    if (isVoidMain)
      ensureVoidMainWrapper(TheFunction);
    return TheFunction;
  }

  return nullptr;
}
