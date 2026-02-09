#include "ast_internal.h"

static void emitAbortIf(llvm::Value *condition, std::string_view label) {
  if (!condition)
    return;
  llvm::Function *func = Builder->GetInsertBlock()
                             ? Builder->GetInsertBlock()->getParent()
                             : nullptr;
  if (!func)
    return;

  std::string base(label);
  llvm::BasicBlock *abortBB =
      llvm::BasicBlock::Create(*TheContext, base + ".abort", func);
  llvm::BasicBlock *okBB =
      llvm::BasicBlock::Create(*TheContext, base + ".ok", func);

  Builder->CreateCondBr(condition, abortBB, okBB);
  Builder->SetInsertPoint(abortBB);
  llvm::Function *AbortFunc = TheModule->getFunction("abort");
  if (!AbortFunc) {
    auto *abortTy =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
    AbortFunc = llvm::Function::Create(abortTy, llvm::Function::ExternalLinkage,
                                       "abort", TheModule.get());
  }
  Builder->CreateCall(AbortFunc);
  Builder->CreateUnreachable();
  Builder->SetInsertPoint(okBB);
}

static std::string sanitizeCompositeLookupNameForArc(
    const std::string &typeName) {
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

static bool typeHasDestructorForArc(const TypeInfo &info) {
  std::string base =
      sanitizeCompositeLookupNameForArc(typeNameFromInfo(info));
  if (base.empty())
    return false;
  if (const CompositeTypeInfo *meta = lookupCompositeInfo(base))
    return meta->hasDestructor;
  return false;
}

std::optional<ArrayBoundsInfo>
emitArrayBoundsInfo(const std::vector<std::unique_ptr<ExprAST>> &bounds,
                    std::string_view label) {
  if (bounds.empty())
    return std::nullopt;

  ArrayBoundsInfo info;
  info.dims32.reserve(bounds.size());
  info.constantDims.reserve(bounds.size());
  bool allConstant = true;

  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Value *zero32 = llvm::ConstantInt::get(int32Ty, 0);
  llvm::Value *negativeFlag = nullptr;

  for (const auto &expr : bounds) {
    if (!expr)
      return std::nullopt;

    llvm::Value *value = expr->codegen();
    if (!value)
      return std::nullopt;

    if (!value->getType()->isIntegerTy()) {
      value = castToType(value, int32Ty, "int");
      if (!value)
        return std::nullopt;
    }

    if (!value->getType()->isIntegerTy(32))
      value = Builder->CreateTruncOrBitCast(value, int32Ty, "array.dim32");

    if (auto *constVal = llvm::dyn_cast<llvm::ConstantInt>(value)) {
      if (constVal->isNegative()) {
        reportCompilerError("Array size cannot be negative");
        return std::nullopt;
      }
      info.constantDims.push_back(constVal->getSExtValue());
    } else {
      allConstant = false;
    }

    llvm::Value *isNegative =
        Builder->CreateICmpSLT(value, zero32, "array.dim.neg");
    negativeFlag =
        negativeFlag ? Builder->CreateOr(negativeFlag, isNegative,
                                         "array.dim.neg.any")
                     : isNegative;
    info.dims32.push_back(value);
  }

  if (negativeFlag) {
    emitAbortIf(negativeFlag, buildArcOpLabel(label, "array.dim.neg"));
  }

  llvm::Type *sizeTy = getSizeType();
  llvm::Value *total = llvm::ConstantInt::get(sizeTy, 1);
  llvm::Value *overflowFlag = nullptr;
#if LLVM_VERSION_MAJOR >= 21
  auto mulWithOverflow = llvm::Intrinsic::getOrInsertDeclaration(
      TheModule.get(), llvm::Intrinsic::umul_with_overflow, {sizeTy});
#else
  auto mulWithOverflow = llvm::Intrinsic::getDeclaration(
      TheModule.get(), llvm::Intrinsic::umul_with_overflow, {sizeTy});
#endif

  for (llvm::Value *dim32 : info.dims32) {
    llvm::Value *dimSize =
        Builder->CreateZExt(dim32, sizeTy, "array.dim.size");
    llvm::Value *result =
        Builder->CreateCall(mulWithOverflow, {total, dimSize},
                            "array.size.mul");
    llvm::Value *nextTotal =
        Builder->CreateExtractValue(result, 0, "array.size.next");
    llvm::Value *overflow =
        Builder->CreateExtractValue(result, 1, "array.size.overflow");
    overflowFlag =
        overflowFlag ? Builder->CreateOr(overflowFlag, overflow,
                                         "array.size.overflow.any")
                     : overflow;
    total = nextTotal;
  }

  if (overflowFlag) {
    emitAbortIf(overflowFlag, buildArcOpLabel(label, "array.size.overflow"));
  }

  llvm::Value *maxInt32 =
      llvm::ConstantInt::get(sizeTy, static_cast<uint64_t>(INT32_MAX));
  llvm::Value *tooLarge =
      Builder->CreateICmpUGT(total, maxInt32, "array.size.too.large");
  emitAbortIf(tooLarge, buildArcOpLabel(label, "array.size.limit"));

  info.totalSize = total;
  info.totalSize32 =
      Builder->CreateTruncOrBitCast(total, int32Ty, "array.size32");

  if (!allConstant)
    info.constantDims.clear();

  return info;
}

llvm::Value *emitArcRetain(llvm::Value *value, const TypeInfo &info,
                           std::string_view label) {
  if (!value)
    return nullptr;
  if (!info.requiresARC() || info.isAlias())
    return value;
  if (info.isArray)
    return emitArrayRetainValue(value, label);
  if (info.isSmartPointer())
    return value;
  if (!value->getType()->isPointerTy())
    return value;

  const ArcDebugOptions &arcDebug = CG.arcDebug;
  if (arcDebug.runtimeTracing) {
    llvm::Value *labelConst = Builder->CreateGlobalString(
        std::string(label), buildArcOpLabel(label, "trace.label"));
    Builder->CreateCall(getHybridArcTraceLabelFunction(), {labelConst});
  }

  llvm::Value *castValue =
      Builder->CreateBitCast(value, pointerType(),
                             buildArcOpLabel(label, "retain.cast"));
  if (arcDebug.runtimeVerify) {
    Builder->CreateCall(getHybridArcVerifyRuntimeFunction(), {castValue});
  }
  llvm::Value *retained =
      Builder->CreateCall(getHybridRetainFunction(), {castValue},
                          buildArcOpLabel(label, "retain.call"));
  return Builder->CreateBitCast(retained, value->getType(),
                                buildArcOpLabel(label, "retain.result"));
}

void emitArcRelease(llvm::Value *value, const TypeInfo &info,
                    std::string_view label) {
  if (!value)
    return;
  if (!info.requiresARC() || info.isAlias())
    return;

  if (info.isArray) {
    emitArrayReleaseValue(value, label);
    return;
  }

  if (info.isSmartPointer()) {
    const bool isConcrete = typeInfoIsConcrete(info);
    const CompositeTypeInfo *metadata =
        isConcrete ? resolveSmartPointerMetadata(info) : nullptr;
    if (!metadata) {
      if (isConcrete) {
        std::string typeLabel =
            stripNullableAnnotations(typeNameFromInfo(info));
        reportCompilerError(
            "Internal error: unable to resolve smart pointer metadata for '" +
            typeLabel + "'");
      }
      return;
    }
    if (metadata->smartPointerDestroyHelper.empty())
      return;
    llvm::Function *destroyFn =
        TheModule->getFunction(metadata->smartPointerDestroyHelper);
    if (!destroyFn) {
      reportCompilerError("Internal error: missing smart pointer destroy "
                          "helper '" +
                          metadata->smartPointerDestroyHelper + "'");
      return;
    }
    llvm::Value *arg = value;
    if (!arg->getType()->isPointerTy()) {
      llvm::AllocaInst *tmp =
          Builder->CreateAlloca(arg->getType(), nullptr,
                                buildArcOpLabel(label, "smart.tmp"));
      Builder->CreateStore(arg, tmp);
      arg = tmp;
    }
    llvm::Type *expectedTy =
        destroyFn->getFunctionType()->getParamType(0);
    if (expectedTy && arg->getType() != expectedTy) {
      arg = Builder->CreateBitCast(
          arg, expectedTy, buildArcOpLabel(label, "smart.cast"));
    }
    Builder->CreateCall(destroyFn, {arg});
    return;
  }

  if (!value->getType()->isPointerTy())
    return;

  const ArcDebugOptions &arcDebug = CG.arcDebug;
  if (arcDebug.runtimeTracing) {
    llvm::Value *labelConst = Builder->CreateGlobalString(
        std::string(label), buildArcOpLabel(label, "trace.label"));
    Builder->CreateCall(getHybridArcTraceLabelFunction(), {labelConst});
  }

  llvm::Value *castValue =
      Builder->CreateBitCast(value, pointerType(),
                             buildArcOpLabel(label, "release.cast"));
  if (arcDebug.runtimeVerify) {
    Builder->CreateCall(getHybridArcVerifyRuntimeFunction(), {castValue});
  }
  Builder->CreateCall(getHybridReleaseFunction(), {castValue});
}

llvm::Value *emitManagedStore(llvm::Value *storagePtr,
                                     llvm::Value *incoming,
                                     const TypeInfo &info,
                                     std::string_view label,
                                     bool incomingIsTemporary) {
  auto promoteStackArcValueIfNeeded = [&](llvm::Value *value) -> llvm::Value * {
    if (!value || !info.requiresARC() || info.isSmartPointer() ||
        info.isAlias())
      return value;

    llvm::Value *stripped = value->stripPointerCasts();
    auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(stripped);
    if (!alloca)
      return value;

    auto *structTy =
        llvm::dyn_cast<llvm::StructType>(alloca->getAllocatedType());
    if (!structTy)
      return value;

    std::string lookupName =
        sanitizeCompositeLookupNameForArc(typeNameFromInfo(info));
    const CompositeTypeInfo *meta =
        lookupCompositeInfo(lookupName, /*countHit=*/false);
    if (!meta || !meta->hasARCHeader)
      return value;

    if (meta->descriptorGlobalName.empty()) {
      reportCompilerError("Internal error: missing descriptor for '" +
                          lookupName + "' while storing value");
      return nullptr;
    }

    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(meta->descriptorGlobalName, true);
    if (!descriptorGV) {
      reportCompilerError("Internal error: descriptor '" +
                          meta->descriptorGlobalName +
                          "' missing while storing '" + lookupName + "'");
      return nullptr;
    }

    const llvm::DataLayout &DL = TheModule->getDataLayout();
    uint64_t typeSize = DL.getTypeAllocSize(structTy);
    llvm::Value *sizeVal =
        llvm::ConstantInt::get(getSizeType(), typeSize);

    llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
        descriptorGV, pointerType(getTypeDescriptorType()));

    llvm::Value *rawPtr = Builder->CreateCall(
        getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
        buildArcOpLabel(label, "stack.promote"));
    llvm::Value *typedPtr =
        Builder->CreateBitCast(rawPtr, pointerType(structTy),
                               buildArcOpLabel(label, "stack.promote.typed"));

    llvm::Value *loadedVal = Builder->CreateLoad(
        structTy, alloca, buildArcOpLabel(label, "stack.init"));
    Builder->CreateStore(loadedVal, typedPtr);
    return typedPtr;
  };

  if (!storagePtr || !incoming) {
    if (storagePtr)
      Builder->CreateStore(incoming, storagePtr);
    return incoming;
  }

  incoming = promoteStackArcValueIfNeeded(incoming);
  if (!incoming)
    return nullptr;
  if (!info.requiresARC() || info.isSmartPointer() || info.isAlias()) {
    Builder->CreateStore(incoming, storagePtr);
    return incoming;
  }

  llvm::Type *storedTy = incoming->getType();
  llvm::Value *current = Builder->CreateLoad(
      storedTy, storagePtr, buildArcOpLabel(label, "managed.load.old"));
  llvm::Value *retained = incoming;
  if (!incomingIsTemporary) {
    retained = emitArcRetain(
        incoming, info, buildArcOpLabel(label, "managed.retain"));
  }
  emitArcRelease(current, info, buildArcOpLabel(label, "managed.release"));
  Builder->CreateStore(retained, storagePtr);
  return retained;
}

void pushArcScope() { CG.arcScopeStack.emplace_back(); }

static void drainArcSlots(const std::vector<ARCLifetimeSlot> &slots,
                          std::string_view label) {
  for (auto it = slots.rbegin(); it != slots.rend(); ++it) {
    const ARCLifetimeSlot &slot = *it;
    if (slot.isTemporary)
      continue;
    if (!slot.storage)
      continue;
    const bool hasDestructor = typeHasDestructorForArc(slot.type);
    bool useArcRelease = slot.type.requiresARC();
    if (useArcRelease) {
      std::string typeKey =
          sanitizeCompositeLookupNameForArc(typeNameFromInfo(slot.type));
      if (!typeKey.empty()) {
        if (const CompositeTypeInfo *meta =
                lookupCompositeInfo(typeKey, /*countHit=*/false)) {
          if (meta->kind == AggregateKind::Struct &&
              !slot.type.isSmartPointer())
            useArcRelease = false;
        }
      }
    }
    if (!useArcRelease && !slot.type.isSmartPointer() &&
        !hasDestructor)
      continue;
    if (slot.lifetimeInfo) {
      if (slot.lifetimeInfo->escapes)
        continue;
      if (slot.lifetimeInfo->manuallyReleased)
        continue;
    }

    auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(slot.storage);
    if (!alloca)
      continue;
    llvm::Type *storedTy = alloca->getAllocatedType();
    if (!storedTy)
      continue;
    const bool storedIsPointer = storedTy->isPointerTy();
    if (useArcRelease) {
      llvm::Value *current = Builder->CreateLoad(
          storedTy, slot.storage, buildArcOpLabel(label, "scope.load"));
      emitArcRelease(current, slot.type,
                     buildArcOpLabel(label, "scope.release"));
      Builder->CreateStore(llvm::Constant::getNullValue(storedTy),
                           slot.storage);
    } else if (hasDestructor) {
      std::string typeKey =
          sanitizeCompositeLookupNameForArc(typeNameFromInfo(slot.type));
      const CompositeTypeInfo *meta = lookupCompositeInfo(typeKey);
      if (meta && meta->hasDestructor &&
          !meta->destructorFunctionName.empty()) {
        llvm::Function *dtor =
            TheModule->getFunction(meta->destructorFunctionName);
        if (dtor) {
          llvm::Value *dtorArg = slot.storage;
          if (storedIsPointer) {
            dtorArg = Builder->CreateLoad(
                storedTy, slot.storage,
                buildArcOpLabel(label, "scope.dtor.load"));
          }
          if (!dtor->arg_empty()) {
            llvm::Type *expected = dtor->getFunctionType()->getParamType(0);
            if (dtorArg->getType() != expected)
              dtorArg = Builder->CreateBitCast(
                  dtorArg, expected,
                  buildArcOpLabel(label, "scope.dtor.cast"));
            Builder->CreateCall(dtor, {dtorArg});
          } else {
            Builder->CreateCall(dtor, {});
          }
        }
      }
      Builder->CreateStore(llvm::Constant::getNullValue(storedTy),
                           slot.storage);
    }
  }
}

void popArcScope(bool emitReleases, std::string_view label) {
  if (CG.arcScopeStack.empty())
    return;
  std::vector<ARCLifetimeSlot> slots = std::move(CG.arcScopeStack.back());
  CG.arcScopeStack.pop_back();
  if (!emitReleases)
    return;

  drainArcSlots(slots, label);
}

void emitArcScopeDrainAll(std::string_view label) {
  if (CG.arcScopeStack.empty())
    return;
  llvm::IRBuilder<> *builderPtr = Builder.get();
  if (!builderPtr || !builderPtr->GetInsertBlock())
    return;

  for (auto it = CG.arcScopeStack.rbegin();
       it != CG.arcScopeStack.rend(); ++it) {
    drainArcSlots(*it, label);
  }
}
