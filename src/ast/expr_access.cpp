#include "ast_internal.h"

// Generate code for array indexing
llvm::Value *ArrayIndexExprAST::codegen() {
  llvm::Value *arrayValue = getArray()->codegen();
  if (!arrayValue)
    return nullptr;

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (arrayDesc.isNullable) {
    reportCompilerError("Cannot index nullable type '" + arrayTypeName +
                        "' without null-safe operator");
    return nullptr;
  }

  if (auto tupleInfoOpt = resolveTupleTypeInfo(getArray());
      tupleInfoOpt && tupleInfoOpt->isTupleType()) {
    if (getIndexCount() != 1) {
      reportCompilerError("Tuple indexing requires a single index");
      return nullptr;
    }

    ExprAST *indexExpr = getIndex(0);
    auto *literalIndex = dynamic_cast<NumberExprAST *>(indexExpr);
    if (!literalIndex || !literalIndex->isIntegerLiteral()) {
      reportCompilerError("Tuple index must be an integer literal");
      return nullptr;
    }

    int64_t indexValue = literalIndex->getLiteral().getSignedValue();
    if (indexValue < 0) {
      reportCompilerError("Tuple index cannot be negative");
      return nullptr;
    }

    if (static_cast<size_t>(indexValue) >= tupleInfoOpt->typeArguments.size()) {
      reportCompilerError("Tuple index out of range");
      return nullptr;
    }

    auto accessOpt = computeTupleElementAccess(*tupleInfoOpt, arrayValue,
                                               static_cast<size_t>(indexValue),
                                               "tuple.index");
    if (!accessOpt)
      return nullptr;

    auto &access = *accessOpt;
    setTypeInfo(access.elementTypeInfo);
    return Builder->CreateLoad(access.elementLLVMType, access.elementPtr,
                               "tuple.elem");
  }

  std::string ownerName = arrayDesc.sanitized;
  if (!ownerName.empty()) {
    if (const CompositeTypeInfo *info = lookupCompositeInfo(ownerName)) {
      if (info->indexer) {
        const PropertyInfo &indexer = *info->indexer;
        if (!indexer.hasGetter) {
          reportCompilerError("Indexer on type '" + ownerName +
                              "' does not define a getter");
          return nullptr;
        }
        if (!ensureMemberReadable(indexer.modifiers, ownerName, "this"))
          return nullptr;
        if (indexer.isStatic) {
          reportCompilerError("Indexers must be instance members");
          return nullptr;
        }

        std::vector<llvm::Value *> argValues;
        std::vector<bool> argIsRef;
        argValues.reserve(getIndices().size());
        argIsRef.reserve(getIndices().size());
        for (const auto &idxExpr : getIndices()) {
          bool isRef = dynamic_cast<RefExprAST *>(idxExpr.get()) != nullptr;
          argIsRef.push_back(isRef);
          llvm::Value *val = idxExpr->codegen();
          if (!val)
            return nullptr;
          argValues.push_back(val);
        }

        auto getterIt = info->methodInfo.find(indexer.getterName);
        if (getterIt == info->methodInfo.end()) {
          reportCompilerError("Internal error: missing indexer getter on '" +
                              ownerName + "'");
          return nullptr;
        }

        return emitMemberCallByInfo(*info, getterIt->second, ownerName,
                                    getArray(), arrayValue,
                                    std::move(argValues), std::move(argIsRef),
                                    this);
      }
    }
  }

  auto accessOpt = computeArrayElementAccess(this, arrayValue);
  if (!accessOpt)
    return nullptr;

  auto &access = *accessOpt;
  std::string displayTypeName = access.elementTypeName;
  if (access.elementNullable)
    displayTypeName = ensureOuterNullable(displayTypeName);
  setTypeName(displayTypeName);

  return Builder->CreateLoad(access.elementLLVMType, access.elementPtr, "array.elem");
}

llvm::Value *NullSafeElementAccessExprAST::codegen() {
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;

  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;

  if (!IndexVal->getType()->isIntegerTy(32)) {
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (!arrayDesc.isArray) {
    return LogErrorV("Null-safe array indexing requires an array value");
  }
  if (!arrayDesc.isNullable) {
    return LogErrorV("Null-safe array indexing requires nullable array type");
  }

  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  if (ArrayVal->getType()->isStructTy()) {
    ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "nsarray.ptr");
    ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "nsarray.size");
  } else if (ArrayVal->getType()->isPointerTy()) {
    ArrayPtr = ArrayVal;
  } else {
    return LogErrorV("Null-safe array indexing requires array storage");
  }

  if (!ArrayPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe array indexing requires pointer-compatible storage");

  bool elementAllowsNull = arrayDesc.elementNullable;
  std::string elemTypeStr;
  if (!arrayTypeName.empty()) {
    std::string sanitized = stripNullableAnnotations(arrayTypeName);
    if (isArrayTypeName(sanitized))
      elemTypeStr = removeLastArrayGroup(sanitized);
  }

  if (elemTypeStr.empty()) {
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      if (const TypeInfo *info = lookupTypeInfo(VarExpr->getName())) {
        if (info->isArray && isArrayTypeName(info->typeName)) {
          elemTypeStr = removeLastArrayGroup(
              stripNullableAnnotations(typeNameFromInfo(*info)));
          elementAllowsNull = elementAllowsNull || info->elementNullable;
        }
      }
    }
  }

  if (elemTypeStr.empty())
    elemTypeStr = "int";

  llvm::Type *ElemType = getTypeFromString(elemTypeStr);
  if (!ElemType)
    return LogErrorV("Unknown element type in array");

  if (!ElemType->isPointerTy())
    return LogErrorV("Null-safe array indexing is only supported for reference-type elements");

  llvm::PointerType *ArrayPtrType = llvm::cast<llvm::PointerType>(ArrayPtr->getType());
  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(ArrayPtrType);
  llvm::Value *IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nsarray.isnull");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nsarray.merge", Func);

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);

  if (ArraySize) {
    if (auto *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
      if (ConstIndex->getSExtValue() < 0)
        return LogErrorV("Array index cannot be negative");
      if (auto *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
        if (ConstIndex->getSExtValue() >= ConstSize->getSExtValue())
          return LogErrorV("Array index out of bounds");
      }
    }
  }

  llvm::BasicBlock *ValueBB = NotNullBB;
  if (ArraySize) {
    llvm::BasicBlock *BoundsErrorBB = llvm::BasicBlock::Create(*TheContext, "nsarray.bounds_error", Func);
    ValueBB = llvm::BasicBlock::Create(*TheContext, "nsarray.value", Func);

    llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "nsarray.isneg");
    llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "nsarray.isoob");
    llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "nsarray.badindex");
    Builder->CreateCondBr(IsBadIndex, BoundsErrorBB, ValueBB);

    Builder->SetInsertPoint(BoundsErrorBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
                                         "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(ValueBB);
  }

  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "nsarray.elemptr");
  llvm::Value *LoadedValue = Builder->CreateLoad(ElemType, ElemPtr, "nsarray.elem");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullValue = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ElemType));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(ElemType, 2, "nsarray.maybe");
  Phi->addIncoming(LoadedValue, NotNullEnd);
  Phi->addIncoming(NullValue, NullEnd);

  std::string resultTypeName = elemTypeStr;
  if (elementAllowsNull)
    resultTypeName = ensureOuterNullable(resultTypeName);
  resultTypeName = ensureOuterNullable(resultTypeName);
  setTypeName(resultTypeName);

  return Phi;
}

// Variable pointer code generation for increment/decrement
llvm::Value *VariableExprAST::codegen_ptr() {
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    if (const TypeInfo *info = lookupLocalTypeInfo(getName())) {
      setTypeInfo(*info);
      if (info->isAlias())
        return materializeAliasPointer(V, *info, getName());
      bool needsLoad = false;
      if (info->isSmartPointer()) {
        needsLoad = true;
      } else if (const CompositeTypeInfo *comp =
                     lookupCompositeInfo(info->typeName)) {
        needsLoad = comp->kind == AggregateKind::Class ||
                    comp->kind == AggregateKind::Interface;
      }
      if (needsLoad) {
        llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
        return Builder->CreateLoad(Alloca->getAllocatedType(), V, (getName() + "_ptr").c_str());
      }
      // For ref value owners we can just return the alloca itself
    }
    return V;
  }

  llvm::Value *G = GlobalValues[getName()];
  if (G) {
    if (const TypeInfo *info = lookupGlobalTypeInfo(getName())) {
      setTypeInfo(*info);
      if (info->isAlias())
        return materializeAliasPointer(G, *info, getName());
      bool needsLoad = false;
      if (info->isSmartPointer()) {
        needsLoad = true;
      } else if (const CompositeTypeInfo *comp =
                     lookupCompositeInfo(info->typeName)) {
        needsLoad = comp->kind == AggregateKind::Class ||
                    comp->kind == AggregateKind::Interface;
      }
      if (needsLoad) {
        llvm::GlobalVariable *GV = static_cast<llvm::GlobalVariable*>(G);
        return Builder->CreateLoad(GV->getValueType(), G, (getName() + "_ptr").c_str());
      }
      // For ref value owners we can just return the global itself
    }
    return G;
  }

  if (const ActiveCompositeContext *ctx = currentCompositeContext()) {
    if (ctx->isPropertyAccessor && ctx->activePropertyName &&
        *ctx->activePropertyName == getName()) {
      const CompositeTypeInfo *info = lookupCompositeInfo(ctx->name);
      if (info && info->properties.contains(getName())) {
        bool isStatic = info->properties.at(getName()).isStatic;
        std::unique_ptr<ExprAST> objectExpr;
        if (isStatic)
          objectExpr = std::make_unique<VariableExprAST>(ctx->name);
        else
          objectExpr = std::make_unique<ThisExprAST>();
        MemberAccessExprAST synthetic(std::move(objectExpr), getName());
        llvm::Value *Ptr = synthetic.codegen_ptr();
        if (!Ptr)
          return nullptr;
        setTypeName(synthetic.getTypeName());
        return Ptr;
      }
    }
  }

  if (resolveInstanceMemberOwnerInCurrentContext(getName())) {
    auto object = std::make_unique<ThisExprAST>();
    MemberAccessExprAST synthetic(std::move(object), getName());
    llvm::Value *Ptr = synthetic.codegen_ptr();
    if (!Ptr)
      return nullptr;
    setTypeName(synthetic.getTypeName());
    return Ptr;
  }

  if (auto owner = resolveStaticFieldOwnerInCurrentContext(getName())) {
    auto object = std::make_unique<VariableExprAST>(*owner);
    MemberAccessExprAST synthetic(std::move(object), getName());
    llvm::Value *Ptr = synthetic.codegen_ptr();
    if (!Ptr)
      return nullptr;
    setTypeName(synthetic.getTypeName());
    return Ptr;
  }

  return LogErrorV("Unknown variable name for increment/decrement");
}

// Array index pointer code generation for increment/decrement
llvm::Value *ArrayIndexExprAST::codegen_ptr() {
  llvm::Value *arrayValue = getArray()->codegen();
  if (!arrayValue)
    return nullptr;

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (auto tupleInfoOpt = resolveTupleTypeInfo(getArray());
      tupleInfoOpt && tupleInfoOpt->isTupleType()) {
    if (getIndexCount() != 1) {
      reportCompilerError("Tuple indexing requires a single index");
      return nullptr;
    }

    ExprAST *indexExpr = getIndex(0);
    auto *literalIndex = dynamic_cast<NumberExprAST *>(indexExpr);
    if (!literalIndex || !literalIndex->isIntegerLiteral()) {
      reportCompilerError("Tuple index must be an integer literal");
      return nullptr;
    }

    int64_t indexValue = literalIndex->getLiteral().getSignedValue();
    if (indexValue < 0) {
      reportCompilerError("Tuple index cannot be negative");
      return nullptr;
    }

    if (static_cast<size_t>(indexValue) >= tupleInfoOpt->typeArguments.size()) {
      reportCompilerError("Tuple index out of range");
      return nullptr;
    }

    auto accessOpt = computeTupleElementAccess(*tupleInfoOpt, arrayValue,
                                               static_cast<size_t>(indexValue),
                                               "tuple.index");
    if (!accessOpt)
      return nullptr;

    auto &access = *accessOpt;
    setTypeInfo(access.elementTypeInfo);
    return access.elementPtr;
  }

  std::string ownerName = arrayDesc.sanitized;
  if (!ownerName.empty()) {
    if (const CompositeTypeInfo *info = lookupCompositeInfo(ownerName)) {
      if (info->indexer) {
        reportCompilerError("Cannot take address of indexer result");
        return nullptr;
      }
    }
  }

  auto accessOpt = computeArrayElementAccess(this, arrayValue);
  if (!accessOpt)
    return nullptr;

  auto &access = *accessOpt;
  std::string displayTypeName = access.elementTypeName;
  if (access.elementNullable)
    displayTypeName = ensureOuterNullable(displayTypeName);
  setTypeName(displayTypeName);

  return access.elementPtr;
}

llvm::Value *NullSafeElementAccessExprAST::codegen_ptr() {
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;

  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;

  if (!IndexVal->getType()->isIntegerTy(32)) {
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (!arrayDesc.isArray) {
    return LogErrorV("Null-safe array indexing requires an array value");
  }
  if (!arrayDesc.isNullable) {
    return LogErrorV("Null-safe array indexing requires nullable array type");
  }

  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  if (ArrayVal->getType()->isStructTy()) {
    ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "nsarray.ptr");
    ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "nsarray.size");
  } else if (ArrayVal->getType()->isPointerTy()) {
    ArrayPtr = ArrayVal;
  } else {
    return LogErrorV("Null-safe array indexing requires array storage");
  }

  if (!ArrayPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe array indexing requires pointer-compatible storage");

  std::string elemTypeStr;
  if (!arrayTypeName.empty()) {
    std::string sanitized = stripNullableAnnotations(arrayTypeName);
    if (isArrayTypeName(sanitized))
      elemTypeStr = removeLastArrayGroup(sanitized);
  }

  if (elemTypeStr.empty()) {
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      if (const TypeInfo *info = lookupTypeInfo(VarExpr->getName())) {
        if (info->isArray && isArrayTypeName(info->typeName))
          elemTypeStr = removeLastArrayGroup(
              stripNullableAnnotations(typeNameFromInfo(*info)));
      }
    }
  }

  if (elemTypeStr.empty())
    elemTypeStr = "int";

  llvm::Type *ElemType = getTypeFromString(elemTypeStr);
  if (!ElemType)
    return LogErrorV("Unknown element type in array");

  llvm::PointerType *ArrayPtrType = llvm::cast<llvm::PointerType>(ArrayPtr->getType());
  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(ArrayPtrType);
  llvm::Value *IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nsarray.isnull");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nsarray.merge", Func);

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);

  if (ArraySize) {
    if (auto *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
      if (ConstIndex->getSExtValue() < 0)
        return LogErrorV("Array index cannot be negative");
      if (auto *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
        if (ConstIndex->getSExtValue() >= ConstSize->getSExtValue())
          return LogErrorV("Array index out of bounds");
      }
    }
  }

  llvm::BasicBlock *ValueBB = NotNullBB;
  if (ArraySize) {
    llvm::BasicBlock *BoundsErrorBB = llvm::BasicBlock::Create(*TheContext, "nsarray.bounds_error", Func);
    ValueBB = llvm::BasicBlock::Create(*TheContext, "nsarray.value", Func);

    llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "nsarray.isneg");
    llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "nsarray.isoob");
    llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "nsarray.badindex");
    Builder->CreateCondBr(IsBadIndex, BoundsErrorBB, ValueBB);

    Builder->SetInsertPoint(BoundsErrorBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
                                         "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(ValueBB);
  }

  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "nsarray.elemptr");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullElementPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ElemPtr->getType()));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(ElemPtr->getType(), 2, "nsarray.ptrmaybe");
  Phi->addIncoming(ElemPtr, NotNullEnd);
  Phi->addIncoming(NullElementPtr, NullEnd);

  return Phi;
}

// Generate code for variable expressions
llvm::Value *VariableExprAST::codegen() {
  // First look this variable up in the local function scope
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    llvm::AllocaInst *Alloca = llvm::dyn_cast<llvm::AllocaInst>(V);
    // Set type name from local types
    if (const TypeInfo *info = lookupLocalTypeInfo(getName())) {
      TypeInfo effective = *info;
      if (typeAllowsNull(*info) && isKnownNonNull(getName()))
        effective.isNullable = false;
      setTypeInfo(effective);

      if (info->isAlias()) {
        llvm::Value *Ptr = materializeAliasPointer(V, *info, getName());
        if (!Ptr)
          return nullptr;
        llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
        if (!ActualLLVMType)
          return LogErrorV("Invalid type for ref variable");
        return Builder->CreateLoad(ActualLLVMType, Ptr, (getName() + "_deref").c_str());
      }

      if (!Alloca)
        return LogErrorV("Internal error: expected stack storage for local variable");
      return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
    }

    if (!Alloca)
      return LogErrorV("Internal error: expected stack storage for local variable");
    return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
  }

  // Not found locally, check global scope
  llvm::GlobalVariable *GV = GlobalValues[getName()];
  if (GV) {
    // Global variable - load from global
    // Set type name from global types
    if (const TypeInfo *info = lookupGlobalTypeInfo(getName())) {
      TypeInfo effective = *info;
      if (typeAllowsNull(*info) && isKnownNonNull(getName()))
        effective.isNullable = false;
      setTypeInfo(effective);

      if (info->isAlias()) {
        llvm::Value *Ptr = materializeAliasPointer(GV, *info, getName());
        if (!Ptr)
          return nullptr;
        llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
        if (!ActualLLVMType)
          return LogErrorV("Invalid type for ref variable");
        return Builder->CreateLoad(ActualLLVMType, Ptr, (getName() + "_deref").c_str());
      }

      return Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
    }

    return Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
  }

  if (const ActiveCompositeContext *ctx = currentCompositeContext()) {
    if (ctx->isPropertyAccessor && ctx->activePropertyName &&
        *ctx->activePropertyName == getName()) {
      const CompositeTypeInfo *info = lookupCompositeInfo(ctx->name);
      if (info && info->properties.contains(getName())) {
        bool isStatic = info->properties.at(getName()).isStatic;
        std::unique_ptr<ExprAST> objectExpr;
        if (isStatic)
          objectExpr = std::make_unique<VariableExprAST>(ctx->name);
        else
          objectExpr = std::make_unique<ThisExprAST>();
        MemberAccessExprAST synthetic(std::move(objectExpr), getName());
        llvm::Value *Value = synthetic.codegen();
        if (!Value)
          return nullptr;
        setTypeName(synthetic.getTypeName());
        return Value;
      }
    }
  }

  if (resolveInstanceMemberOwnerInCurrentContext(getName())) {
    auto object = std::make_unique<ThisExprAST>();
    MemberAccessExprAST synthetic(std::move(object), getName());
    llvm::Value *Value = synthetic.codegen();
    if (!Value)
      return nullptr;
    setTypeName(synthetic.getTypeName());
    return Value;
  }

  if (auto owner = resolveStaticFieldOwnerInCurrentContext(getName())) {
    auto object = std::make_unique<VariableExprAST>(*owner);
    MemberAccessExprAST synthetic(std::move(object), getName());
    llvm::Value *Value = synthetic.codegen();
    if (!Value)
      return nullptr;
    setTypeName(synthetic.getTypeName());
    return Value;
  }

  return LogErrorV(("Unknown variable name: " + getName()).c_str());
}
