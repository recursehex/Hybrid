#include "ast_internal.h"

// Helper function to cast a value to a target type
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType) {
  llvm::Type* sourceType = value->getType();
  if (llvm::isa<llvm::ConstantPointerNull>(value)) {
    if (targetType->isPointerTy())
      return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(targetType));
    if (!targetType->isVoidTy())
      return llvm::Constant::getNullValue(targetType);
  }
  
  if (sourceType == targetType)
    return value;
  
  // Special case: allow casting constant integers to smaller integer types
  // This is needed for literals like: byte b = 100
  if (llvm::ConstantInt* CI = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    if (sourceType->isIntegerTy() && targetType->isIntegerTy() && 
        !targetType->isIntegerTy(1)) { // Don't allow to bool
      // Check if the constant fits in the target type
      llvm::APInt Val = CI->getValue();
      unsigned targetBits = targetType->getIntegerBitWidth();
      
      // Check ranges based on target type
      bool inRange = false;
      if (targetBits == 8) {
        // byte (unsigned) or sbyte/char (signed)
        int64_t v = Val.getSExtValue();
        // Assuming unsigned types use the full range
        inRange = (v >= 0 && v <= 255); // For byte
        // For sbyte/char, would need to track signedness
      } else if (targetBits == 16) {
        // short (signed) or ushort (unsigned)
        int64_t v = Val.getSExtValue();
        inRange = (v >= -32768 && v <= 65535); // Conservative: covers both signed and unsigned
      } else if (targetBits == 32) {
        // int (signed) or uint (unsigned)
        inRange = true; // Source is already 32-bit
      } else if (targetBits == 64) {
        // long (signed) or ulong (unsigned)
        inRange = true; // Can always extend to 64-bit
      }
      
      if (!inRange) {
        LogErrorV(buildIntegerRangeError(Val, ""));
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
  }

  const bool sourceIsDecimal = isDecimalLLVMType(sourceType);
  const bool targetIsDecimal = isDecimalLLVMType(targetType);
  if (sourceIsDecimal || targetIsDecimal) {
    if (sourceIsDecimal && targetIsDecimal)
      return value;

    if (targetIsDecimal && sourceType->isIntegerTy() &&
        !sourceType->isIntegerTy(1)) {
      if (llvm::Value *converted = emitIntegerToDecimalValue(value, ""))
        return converted;
    }

    std::string sourceLabel = describeTypeForDiagnostic(sourceType);
    std::string targetLabel = describeTypeForDiagnostic(targetType);
    reportCompilerError("Cannot implicitly convert '" + sourceLabel +
                        "' to '" + targetLabel + "'",
                        "Use an explicit cast with ':'");
    return llvm::UndefValue::get(targetType);
  }
  
  // Check if types are compatible for implicit casting
  if (!areTypesCompatible(sourceType, targetType)) {
    // For explicit casts in the future, might allow this
    // For now, implicit casts must follow compatibility rules
    return value; // Return original value, let the caller handle the error
  }
  
  // Integer to float
  if (sourceType->isIntegerTy() && targetType->isFloatingPointTy()) {
    return Builder->CreateSIToFP(value, targetType, "casttmp");
  }
  
  // Float to integer
  if (sourceType->isFloatingPointTy() && targetType->isIntegerTy()) {
    return Builder->CreateFPToSI(value, targetType, "casttmp");
  }
  
  // Float to float (different precision)
  if (sourceType->isFloatingPointTy() && targetType->isFloatingPointTy()) {
    if (sourceType->getPrimitiveSizeInBits() < targetType->getPrimitiveSizeInBits()) {
      return Builder->CreateFPExt(value, targetType, "casttmp");
    } else {
      return Builder->CreateFPTrunc(value, targetType, "casttmp");
    }
  }
  
  // Can't cast, return the original value
  return value;
}

// Overloaded castToType that knows the target type name for proper range checking
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType, const std::string& targetTypeName) {
  llvm::Type* sourceType = value->getType();
  if (llvm::isa<llvm::ConstantPointerNull>(value)) {
    if (targetType->isPointerTy())
      return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(targetType));
    if (!targetType->isVoidTy())
      return llvm::Constant::getNullValue(targetType);
  }
  
  if (sourceType == targetType)
    return value;
  
  // Special case: allow casting constant integers to smaller integer types
  // This is needed for literals like: byte b = 100
  if (llvm::ConstantInt* CI = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    if (sourceType->isIntegerTy() && targetType->isIntegerTy() && 
        !targetType->isIntegerTy(1)) { // Don't allow to bool
      // Check if the constant fits in the target type
      llvm::APInt Val = CI->getValue();
      unsigned targetBits = targetType->getIntegerBitWidth();
      int64_t v = Val.getSExtValue();
      
      // Check ranges based on target type name (which tells us signedness)
      bool inRange = false;
      if (targetBits == 8) {
        if (isSignedType(targetTypeName)) {
          // sbyte, char: -128 to 127
          inRange = (v >= -128 && v <= 127);
        } else {
          // byte, schar: 0 to 255
          inRange = (v >= 0 && v <= 255);
        }
      } else if (targetBits == 16) {
        if (isSignedType(targetTypeName)) {
          // short: -32768 to 32767
          inRange = (v >= -32768 && v <= 32767);
        } else {
          // ushort: 0 to 65535
          inRange = (v >= 0 && v <= 65535);
        }
      } else if (targetBits == 32) {
        if (isSignedType(targetTypeName)) {
          // int: -2147483648 to 2147483647
          inRange = (v >= INT32_MIN && v <= INT32_MAX);
        } else {
          // uint: 0 to 4294967295
          inRange = (v >= 0 && v <= UINT32_MAX);
        }
      } else if (targetBits == 64) {
        // For 64-bit, always can fit a 32-bit source
        inRange = true;
      }
      
      if (!inRange) {
        LogErrorV(buildIntegerRangeError(Val, targetTypeName));
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
  }

  const bool sourceIsDecimal = isDecimalLLVMType(sourceType);
  const bool targetIsDecimal = isDecimalLLVMType(targetType);
  if (sourceIsDecimal || targetIsDecimal) {
    if (sourceIsDecimal && targetIsDecimal)
      return value;

    if (targetIsDecimal && sourceType->isIntegerTy() &&
        !sourceType->isIntegerTy(1)) {
      if (llvm::Value *converted = emitIntegerToDecimalValue(value, ""))
        return converted;
    }

    std::string cleanTarget = sanitizeBaseTypeName(targetTypeName);
    if (cleanTarget.empty())
      cleanTarget = describeTypeForDiagnostic(targetType);
    std::string sourceLabel = describeTypeForDiagnostic(sourceType);
    reportCompilerError("Cannot implicitly convert '" + sourceLabel +
                        "' to '" + cleanTarget + "'",
                        "Use an explicit cast with ':'");
    return llvm::UndefValue::get(targetType);
  }

  // For non-constant values, add runtime range checking for integer narrowing
  if (sourceType->isIntegerTy() && targetType->isIntegerTy() &&
      !targetType->isIntegerTy(1) && // Don't allow to bool
      sourceType->getIntegerBitWidth() > targetType->getIntegerBitWidth()) {

    unsigned targetBits = targetType->getIntegerBitWidth();

    // Determine range based on target type name
    int64_t minVal = 0, maxVal = 0;
    if (targetBits == 8) {
      if (isSignedType(targetTypeName)) {
        minVal = -128; maxVal = 127;  // sbyte, char
      } else {
        minVal = 0; maxVal = 255;  // byte
      }
    } else if (targetBits == 16) {
      if (isSignedType(targetTypeName)) {
        minVal = -32768; maxVal = 32767;  // short
      } else {
        minVal = 0; maxVal = 65535;  // ushort
      }
    } else if (targetBits == 32) {
      if (isSignedType(targetTypeName)) {
        minVal = INT32_MIN; maxVal = INT32_MAX;  // int
      } else {
        minVal = 0; maxVal = UINT32_MAX;  // uint
      }
    }

    // Generate runtime range check
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *InRangeBB = llvm::BasicBlock::Create(*TheContext, "range_ok", TheFunction);
    llvm::BasicBlock *OutOfRangeBB = llvm::BasicBlock::Create(*TheContext, "range_fail", TheFunction);

    // Create comparison: value >= minVal && value <= maxVal
    llvm::Value *MinCheck = Builder->CreateICmpSGE(value,
        llvm::ConstantInt::get(sourceType, minVal), "mincheck");
    llvm::Value *MaxCheck = Builder->CreateICmpSLE(value,
        llvm::ConstantInt::get(sourceType, maxVal), "maxcheck");
    llvm::Value *InRange = Builder->CreateAnd(MinCheck, MaxCheck, "inrange");

    // Branch based on range check
    Builder->CreateCondBr(InRange, InRangeBB, OutOfRangeBB);

    // Out of range block - call abort()
    Builder->SetInsertPoint(OutOfRangeBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage, "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    // In range block - perform the truncation
    Builder->SetInsertPoint(InRangeBB);
    return Builder->CreateTrunc(value, targetType, "rangecast");
  }

  // For non-constant values without range checking, use the regular castToType
  return castToType(value, targetType);
}

// Helper function to promote types for binary operations
std::pair<llvm::Value*, llvm::Value*> promoteTypes(llvm::Value* L, llvm::Value* R,
                                                   std::string_view lhsTypeName,
                                                   std::string_view rhsTypeName) {
  if (!L || !R) {
    std::string lhsName = lhsTypeName.empty() ? "<unknown>" : std::string(lhsTypeName);
    std::string rhsName = rhsTypeName.empty() ? "<unknown>" : std::string(rhsTypeName);
    std::string funcName;
    if (auto *bb = Builder->GetInsertBlock()) {
      if (auto *fn = bb->getParent())
        funcName = fn->getName().str();
    }
    std::string opName = ActiveBinaryOp.empty() ? "<op>" : ActiveBinaryOp;
    LogErrorV(("Internal error: null operand while promoting binary expression types in '" +
               funcName + "' for op '" + opName + "' (lhs=" + lhsName + ", rhs=" + rhsName + ")")
                  .c_str());
    return {L, R};
  }

  llvm::Type* LType = L->getType();
  llvm::Type* RType = R->getType();
  
  // If both are the same type, no promotion needed
  if (LType == RType)
    return {L, R};

  // Check if types are compatible
  if (!areTypesCompatible(LType, RType)) {
    // Generate a more helpful error message
    std::string LTypeStr = "unknown", RTypeStr = "unknown";
    
    // Determine type names
    if (LType->isIntegerTy(1)) LTypeStr = "bool";
    else if (LType->isIntegerTy(8)) LTypeStr = "byte/sbyte/char";
    else if (LType->isIntegerTy(16)) LTypeStr = "short/ushort";
    else if (LType->isIntegerTy(32)) LTypeStr = "int/uint";
    else if (LType->isIntegerTy(64)) LTypeStr = "long/ulong";
    else if (LType->isFloatTy()) LTypeStr = "float";
    else if (LType->isDoubleTy()) LTypeStr = "double";
    else if (LType->isFP128Ty()) LTypeStr = "fp128";
    
    if (RType->isIntegerTy(1)) RTypeStr = "bool";
    else if (RType->isIntegerTy(8)) RTypeStr = "byte/sbyte/char";
    else if (RType->isIntegerTy(16)) RTypeStr = "short/ushort";
    else if (RType->isIntegerTy(32)) RTypeStr = "int/uint";
    else if (RType->isIntegerTy(64)) RTypeStr = "long/ulong";
    else if (RType->isFloatTy()) RTypeStr = "float";
    else if (RType->isDoubleTy()) RTypeStr = "double";
    else if (RType->isFP128Ty()) RTypeStr = "fp128";
    
    std::string funcName;
    if (auto *bb = Builder->GetInsertBlock()) {
      if (auto *fn = bb->getParent())
        funcName = fn->getName().str();
    }
    std::string opName = ActiveBinaryOp.empty() ? "<op>" : ActiveBinaryOp;
    LogErrorV(("Type mismatch in '" + funcName + "' for op '" + opName +
               "': cannot implicitly convert between '" + LTypeStr + "' and '" +
               RTypeStr + "'")
                  .c_str());
    return {nullptr, nullptr};
  }

  std::string lhsClean = sanitizeBaseTypeName(lhsTypeName);
  std::string rhsClean = sanitizeBaseTypeName(rhsTypeName);

  const bool lhsDecimal = isDecimalLLVMType(LType);
  const bool rhsDecimal = isDecimalLLVMType(RType);
  if (lhsDecimal || rhsDecimal) {
    if (lhsDecimal && rhsDecimal)
      return {L, R};

    if (lhsDecimal && RType->isIntegerTy() && !RType->isIntegerTy(1)) {
      R = emitIntegerToDecimalValue(R, rhsClean);
      return {L, R};
    }
    if (rhsDecimal && LType->isIntegerTy() && !LType->isIntegerTy(1)) {
      L = emitIntegerToDecimalValue(L, lhsClean);
      return {L, R};
    }

    LogErrorV("Cannot mix decimal with float/double without an explicit cast");
    return {nullptr, nullptr};
  }

  // Handle integer promotions explicitly
  if (LType->isIntegerTy() && RType->isIntegerTy()) {
    if (LType->isIntegerTy(1) || RType->isIntegerTy(1))
      return {L, R};

    unsigned lBits = LType->getIntegerBitWidth();
    unsigned rBits = RType->getIntegerBitWidth();

    auto chooseUnsignedHint = [&](const std::string &primary,
                                  const std::string &secondary) -> std::optional<bool> {
      if (auto hint = unsignedHintFromTypeName(primary); hint.has_value())
        return hint;
      return unsignedHintFromTypeName(secondary);
    };

    auto extendValue = [&](llvm::Value *Value, llvm::Type *TargetType,
                           std::optional<bool> unsignedHint) -> llvm::Value * {
      if (unsignedHint.value_or(false))
        return Builder->CreateZExt(Value, TargetType, "promext");
      return Builder->CreateSExt(Value, TargetType, "promext");
    };

    if (lBits < rBits) {
      auto hint = chooseUnsignedHint(lhsClean, rhsClean);
      L = extendValue(L, RType, hint);
    } else if (rBits < lBits) {
      auto hint = chooseUnsignedHint(rhsClean, lhsClean);
      R = extendValue(R, LType, hint);
    }

    return {L, R};
  }

  // If one is float and other is int, promote int to float
  if (LType->isFloatingPointTy() && RType->isIntegerTy()) {
    std::optional<bool> hint = unsignedHintFromTypeName(rhsClean);
    if (hint.value_or(false))
      R = Builder->CreateUIToFP(R, LType, "promtmp");
    else
      R = Builder->CreateSIToFP(R, LType, "promtmp");
    return {L, R};
  }

  if (LType->isIntegerTy() && RType->isFloatingPointTy()) {
    std::optional<bool> hint = unsignedHintFromTypeName(lhsClean);
    if (hint.value_or(false))
      L = Builder->CreateUIToFP(L, RType, "promtmp");
    else
      L = Builder->CreateSIToFP(L, RType, "promtmp");
    return {L, R};
  }

  // If both are floats but different precision, promote to higher precision
  if (LType->isFloatingPointTy() && RType->isFloatingPointTy()) {
    if (LType->isFloatTy() && RType->isDoubleTy()) {
      L = Builder->CreateFPExt(L, RType, "promtmp");
    } else if (LType->isDoubleTy() && RType->isFloatTy()) {
      R = Builder->CreateFPExt(R, LType, "promtmp");
    }
  }

  return {L, R};
}

llvm::Value *BinaryExprAST::codegenNullCoalescing(llvm::Value *lhsValue) {
  const ExprAST *lhsExpr = unwrapRefExpr(getLHS());
  if (!expressionIsNullable(lhsExpr))
    return LogErrorV("Null-coalescing operator '\?\?' requires nullable left-hand side");

  std::string leftTypeName = getLHS()->getTypeName();
  ParsedTypeDescriptor leftDesc = parseTypeString(leftTypeName);

  bool lhsIsArray = leftDesc.isArray;
  bool lhsIsPointer = lhsValue->getType()->isPointerTy();

  if (!lhsIsPointer && !lhsIsArray)
    return LogErrorV("Null-coalescing operator '\?\?' requires reference or array type on the left-hand side");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.merge", Func);

  llvm::Value *IsNull = nullptr;
  if (lhsIsArray) {
    if (!lhsValue->getType()->isStructTy())
      return LogErrorV("Null-coalescing operator '\?\?' expected array struct value");
    llvm::Value *ArrayPtr = Builder->CreateExtractValue(lhsValue, 0, "nullcoal.arrptr");
    if (!ArrayPtr->getType()->isPointerTy())
      return LogErrorV("Nullable array pointer field must be a pointer");
    llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ArrayPtr->getType()));
    IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nullcoal.check");
  } else {
    llvm::PointerType *PtrTy = llvm::cast<llvm::PointerType>(lhsValue->getType());
    llvm::Value *NullPtr = llvm::ConstantPointerNull::get(PtrTy);
    IsNull = Builder->CreateICmpEQ(lhsValue, NullPtr, "nullcoal.check");
  }

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *rhsValueRaw = getRHS()->codegen();
  if (!rhsValueRaw)
    return nullptr;
  std::string rhsTypeName = getRHS()->getTypeName();
  llvm::Value *ResultWhenNull = rhsValueRaw;

  if (lhsIsArray) {
    if (rhsValueRaw->getType() != lhsValue->getType())
      return LogErrorV("Null-coalescing operator '\?\?' requires fallback to match array type");
  } else {
    if (!rhsValueRaw->getType()->isPointerTy())
      return LogErrorV("Null-coalescing operator '\?\?' fallback must be a reference type");
    if (rhsValueRaw->getType() != lhsValue->getType())
      ResultWhenNull = Builder->CreateBitCast(rhsValueRaw, lhsValue->getType(), "nullcoal.cast");
  }

  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(lhsValue->getType(), 2, "nullcoal.result");
  Phi->addIncoming(lhsValue, NotNullEnd);
  Phi->addIncoming(ResultWhenNull, NullEnd);

  bool rhsNullable = expressionIsNullable(unwrapRefExpr(getRHS()));
  std::string baseTypeName = leftDesc.sanitized;
  if (baseTypeName.empty()) {
    ParsedTypeDescriptor rhsDesc = parseTypeString(rhsTypeName);
    baseTypeName = rhsDesc.sanitized;
  }
  if (baseTypeName.empty())
    baseTypeName = rhsTypeName;

  if (!baseTypeName.empty()) {
    if (rhsNullable)
      setTypeName(ensureOuterNullable(baseTypeName));
    else
      setTypeName(baseTypeName);
  }

  return Phi;
}

llvm::Value *BinaryExprAST::codegenNullCoalescingAssign(llvm::Value *lhsValue) {
  const ExprAST *lhsExpr = unwrapRefExpr(getLHS());
  if (!expressionIsNullable(lhsExpr))
    return LogErrorV("Null-coalescing assignment '\?\?=' requires nullable left-hand side");

  std::string leftTypeName = getLHS()->getTypeName();
  ParsedTypeDescriptor leftDesc = parseTypeString(leftTypeName);

  bool lhsIsArray = leftDesc.isArray;
  bool lhsIsPointerValue = lhsValue->getType()->isPointerTy();

  if (!lhsIsPointerValue && !lhsIsArray)
    return LogErrorV("Null-coalescing assignment '\?\?=' requires reference or array type on the left-hand side");

  llvm::Value *lhsPtr = getLHS()->codegen_ptr();
  if (!lhsPtr)
    return nullptr;
  if (!lhsPtr->getType()->isPointerTy())
    return LogErrorV("destination of '\?\?=' must be assignable");

  llvm::PointerType *lhsPtrType = llvm::cast<llvm::PointerType>(lhsPtr->getType());
  llvm::Value *NullStoragePtr = llvm::ConstantPointerNull::get(lhsPtrType);
  llvm::Value *PtrValid = Builder->CreateICmpNE(lhsPtr, NullStoragePtr, "nullassign.ptrvalid");

  llvm::Value *ValueIsNull = nullptr;
  if (lhsIsArray) {
    if (!lhsValue->getType()->isStructTy())
      return LogErrorV("Null-coalescing assignment '\?\?=' expected array struct value");
    llvm::Value *ArrayPtr = Builder->CreateExtractValue(lhsValue, 0, "nullassign.arrptr");
    if (!ArrayPtr->getType()->isPointerTy())
      return LogErrorV("Nullable array pointer field must be a pointer");
    llvm::Value *NullArrayPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ArrayPtr->getType()));
    ValueIsNull = Builder->CreateICmpEQ(ArrayPtr, NullArrayPtr, "nullassign.value");
  } else {
    llvm::PointerType *PtrTy = llvm::cast<llvm::PointerType>(lhsValue->getType());
    llvm::Value *NullPtrValue = llvm::ConstantPointerNull::get(PtrTy);
    ValueIsNull = Builder->CreateICmpEQ(lhsValue, NullPtrValue, "nullassign.value");
  }

  llvm::Value *ShouldAssign = Builder->CreateAnd(ValueIsNull, PtrValid, "nullassign.should");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *AssignBB = llvm::BasicBlock::Create(*TheContext, "nullassign.assign", Func);
  llvm::BasicBlock *DoneBB = llvm::BasicBlock::Create(*TheContext, "nullassign.done", Func);
  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();

  Builder->CreateCondBr(ShouldAssign, AssignBB, DoneBB);

  Builder->SetInsertPoint(AssignBB);
  llvm::Value *rhsValueRaw = getRHS()->codegen();
  if (!rhsValueRaw)
    return nullptr;
  std::string rhsTypeName = getRHS()->getTypeName();
  llvm::Value *AssignedValue = rhsValueRaw;

  if (lhsIsArray) {
    if (rhsValueRaw->getType() != lhsValue->getType())
      return LogErrorV("Null-coalescing assignment '\?\?=' requires fallback to match array type");
  } else {
    if (!rhsValueRaw->getType()->isPointerTy())
      return LogErrorV("Null-coalescing assignment '\?\?=' fallback must be a reference type");
    if (rhsValueRaw->getType() != lhsValue->getType())
      AssignedValue = Builder->CreateBitCast(rhsValueRaw, lhsValue->getType(), "nullassign.cast");
  }

  Builder->CreateStore(AssignedValue, lhsPtr);
  Builder->CreateBr(DoneBB);
  llvm::BasicBlock *AssignEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(DoneBB);
  llvm::PHINode *Phi = Builder->CreatePHI(lhsValue->getType(), 2, "nullassign.result");
  Phi->addIncoming(lhsValue, CurrentBB);
  Phi->addIncoming(AssignedValue, AssignEnd);

  bool rhsNullable = expressionIsNullable(unwrapRefExpr(getRHS()));
  std::string baseTypeName = leftDesc.sanitized;
  if (baseTypeName.empty()) {
    ParsedTypeDescriptor rhsDesc = parseTypeString(rhsTypeName);
    baseTypeName = rhsDesc.sanitized;
  }
  if (baseTypeName.empty())
    baseTypeName = rhsTypeName;

  if (!baseTypeName.empty()) {
    if (rhsNullable)
      setTypeName(ensureOuterNullable(baseTypeName));
    else
      setTypeName(baseTypeName);
  }

  return Phi;
}

// Generate code for binary expressions like +, -, *, /, <, >
llvm::Value *BinaryExprAST::codegen() {
  ActiveBinaryOpScope activeOpScope(Op);
  if (Op == "\?\?" || Op == "\?\?=") {
    llvm::Value *L = getLHS()->codegen();
    if (!L)
      return nullptr;
    if (Op == "\?\?")
      return codegenNullCoalescing(L);
    return codegenNullCoalescingAssign(L);
  }

  llvm::Value *R = nullptr;
  llvm::Value *precomputedLhsValue = nullptr;
  bool hasPrecomputedLhsValue = false;
  const bool isCompoundArithmetic =
      (Op == "+=" || Op == "-=" || Op == "*=" || Op == "/=" || Op == "%=");
  const bool isCompoundBitwise =
      (Op == "&=" || Op == "|=" || Op == "^=" || Op == "<<=" || Op == ">>=");
  const bool isAssignmentOp =
      (Op == "=" || isCompoundArithmetic || isCompoundBitwise);

  auto tryEmitOverloadedBinaryOperator =
      [&]() -> std::optional<llvm::Value *> {
    auto opKindOpt = overloadableOperatorFromSymbol(Op);
    if (!opKindOpt)
      return std::nullopt;

    const OverloadableOperator opKind = *opKindOpt;
    if (opKind == OverloadableOperator::Dereference ||
        opKind == OverloadableOperator::AddressOf ||
        opKind == OverloadableOperator::Index) {
      return std::nullopt;
    }

    if (isAssignmentOp) {
      if (auto *member = dynamic_cast<MemberAccessExprAST *>(getLHS())) {
        std::string ownerName = resolveCompositeName(member->getObject());
        if (const CompositeTypeInfo *memberOwner =
                lookupCompositeInfo(ownerName)) {
          if (!shouldBypassPropertyAccess(ownerName, member->getMemberName()) &&
              memberOwner->properties.contains(member->getMemberName())) {
            return std::nullopt;
          }
        }
      }

      if (auto *index = dynamic_cast<ArrayIndexExprAST *>(getLHS())) {
        if (auto indexOwnerInfo = resolveExprTypeInfo(index->getArray())) {
          if (const CompositeTypeInfo *indexOwner =
                  resolveCompositeTypeInfo(*indexOwnerInfo)) {
            if (indexOwner->indexer &&
                !lookupOperatorMember(*indexOwner,
                                      OverloadableOperator::Index)) {
              return std::nullopt;
            }
          }
        }
      }
    }

    auto lhsInfoOpt = resolveExprTypeInfo(getLHS());
    if (!lhsInfoOpt) {
      precomputedLhsValue = getLHS()->codegen();
      if (!precomputedLhsValue)
        return std::optional<llvm::Value *>{nullptr};
      hasPrecomputedLhsValue = true;
      lhsInfoOpt = resolveExprTypeInfo(getLHS());
      if (!lhsInfoOpt)
        return std::nullopt;
    }

    const CompositeTypeInfo *ownerInfo = resolveCompositeTypeInfo(*lhsInfoOpt);
    if (!ownerInfo)
      return std::nullopt;

    const CompositeMemberInfo *operatorInfo =
        lookupOperatorMember(*ownerInfo, opKind);
    if (!operatorInfo)
      return std::nullopt;

    std::string ownerName = stripNullableAnnotations(typeNameFromInfo(*lhsInfoOpt));
    if (ownerName.empty())
      ownerName = stripNullableAnnotations(lhsInfoOpt->typeName);
    if (ownerName.empty())
      ownerName = "<composite>";

    if (!ensureMemberAccessAllowed(operatorInfo->modifiers, AccessIntent::Call,
                                   ownerName,
                                   std::string(overloadableOperatorSymbol(opKind)))) {
      return std::optional<llvm::Value *>{nullptr};
    }

    std::vector<llvm::Value *> argValues;
    std::vector<bool> argIsRef;
    argValues.reserve(1);
    argIsRef.reserve(1);

    bool rhsIsRef = false;
    if (operatorInfo->parameterIsRef.size() > 1)
      rhsIsRef = operatorInfo->parameterIsRef[1];
    else
      rhsIsRef = dynamic_cast<RefExprAST *>(getRHS()) != nullptr;
    llvm::Value *rhsValue = nullptr;
    if (rhsIsRef) {
      if (dynamic_cast<RefExprAST *>(getRHS()) != nullptr) {
        rhsValue = getRHS()->codegen_ptr();
        if (!rhsValue)
          return std::optional<llvm::Value *>{nullptr};
      } else {
        llvm::Value *materialized = getRHS()->codegen();
        if (!materialized)
          return std::optional<llvm::Value *>{nullptr};
        llvm::AllocaInst *tmp =
            Builder->CreateAlloca(materialized->getType(), nullptr,
                                  "op.ref.arg");
        Builder->CreateStore(materialized, tmp);
        rhsValue = tmp;
      }
    } else {
      rhsValue = getRHS()->codegen();
      if (!rhsValue)
        return std::optional<llvm::Value *>{nullptr};
    }
    argValues.push_back(rhsValue);
    argIsRef.push_back(rhsIsRef);

    llvm::Value *lhsInstanceValue = nullptr;
    if (hasPrecomputedLhsValue) {
      lhsInstanceValue = precomputedLhsValue;
    } else {
      lhsInstanceValue = getLHS()->codegen();
      if (!lhsInstanceValue)
        return std::optional<llvm::Value *>{nullptr};
    }

    llvm::Value *callValue = emitMemberCallByInfo(
        *ownerInfo, *operatorInfo, ownerName, getLHS(), lhsInstanceValue,
        std::move(argValues), std::move(argIsRef), this);
    return std::optional<llvm::Value *>{callValue};
  };

  if (auto overloaded = tryEmitOverloadedBinaryOperator();
      overloaded.has_value()) {
    return *overloaded;
  }

  if (isAssignmentOp) {
    const ExprAST *rhsCheckExpr = unwrapRefExpr(getRHS());
    bool rhsIsNullableLocal = expressionIsNullable(rhsCheckExpr);
    bool rhsIsTemporaryLocal = getRHS() && getRHS()->isTemporary();

    auto emitAssignmentRHS = [&](const TypeInfo *targetInfo) -> llvm::Value * {
      llvm::Value *rhsVal = R;
      if (targetInfo && targetInfo->isSmartPointer()) {
        resolveSmartPointerMetadata(*targetInfo);
        if (auto *hashInit = dynamic_cast<UnaryExprAST *>(getRHS())) {
          if (hashInit->getOp() == "#") {
            if (auto parenInit = convertHashShorthandToParen(*hashInit)) {
              RHS = std::move(parenInit);
              rhsVal = nullptr;
              R = nullptr;
            }
          }
        }
      }
      if (targetInfo)
        propagateTypeToNewExpr(getRHS(), *targetInfo);

      if (auto *paren = dynamic_cast<ParenExprAST *>(getRHS())) {
        if (targetInfo) {
          if (auto constructed =
                  emitTargetTypedConstruction(*targetInfo, *paren)) {
            rhsVal = constructed;
            if (targetInfo->requiresARC())
              rhsIsTemporaryLocal = true;
          }
        }
      }

      if (targetInfo) {
        if (const DelegateTypeInfo *delegateInfo =
                lookupDelegateInfo(*targetInfo)) {
          bool handled = false;
          llvm::Value *delegateValue = emitDelegateValueForTarget(
              *delegateInfo, getRHS(), "assignment", handled);
          if (handled) {
            rhsVal = delegateValue;
            R = rhsVal;
            return rhsVal;
          }
        }
      }

      if (!rhsVal)
        if (targetInfo) {
          std::string targetTypeName = typeNameFromInfo(*targetInfo);
          llvm::Type *targetType =
              targetTypeName.empty() ? nullptr : getTypeFromString(targetTypeName);
          if (targetType && isDecimalLLVMType(targetType)) {
            if (auto *num = dynamic_cast<NumberExprAST *>(getRHS()))
              rhsVal = num->codegen_with_target(targetType);
          }
        }

      if (!rhsVal)
        rhsVal = getRHS()->codegen();

      R = rhsVal;
      return rhsVal;
    };

    auto computeCompoundAssignment =
        [&](llvm::Value *currentVal, const std::string &lhsPromoteType,
            llvm::Value *rhsValue,
            const std::string &rhsPromoteType) -> llvm::Value * {
      if (isCompoundArithmetic) {
        if (currentVal->getType()->isPointerTy()) {
          if (Op != "+=" && Op != "-=")
            return LogErrorV(
                "Pointer compound assignment only supports '+=' and '-='");
          llvm::Value *newPtr = emitPointerOffset(
              currentVal, rhsValue, lhsPromoteType, rhsPromoteType, Op == "-=",
              "ptrarith");
          if (!newPtr)
            return nullptr;
          return newPtr;
        }

        auto [PromotedCurrent, PromotedR] = promoteTypes(
            currentVal, rhsValue, lhsPromoteType, rhsPromoteType);
        if (!PromotedCurrent || !PromotedR)
          return nullptr;
        if (isDecimalLLVMType(PromotedCurrent->getType())) {
          llvm::Value *Result =
              emitDecimalArithmeticBinary(Op[0], PromotedCurrent, PromotedR);
          if (!Result)
            return nullptr;
          return Result;
        }
        bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
        auto lhsUnsigned = unsignedHintFromTypeName(
            sanitizeBaseTypeName(lhsPromoteType));
        auto rhsUnsigned = unsignedHintFromTypeName(
            sanitizeBaseTypeName(rhsPromoteType));
        bool compoundUnsigned =
            lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
        if (!compoundUnsigned) {
          compoundUnsigned = unsignedHintFromTypeName(
                                 sanitizeBaseTypeName(lhsPromoteType))
                                 .value_or(false);
        }

        llvm::Value *Result = nullptr;
        char baseOp = Op[0];
        switch (baseOp) {
        case '+':
          Result = isFloat
                       ? Builder->CreateFAdd(PromotedCurrent, PromotedR,
                                             "addtmp")
                       : Builder->CreateAdd(PromotedCurrent, PromotedR,
                                            "addtmp");
          break;
        case '-':
          Result = isFloat
                       ? Builder->CreateFSub(PromotedCurrent, PromotedR,
                                             "subtmp")
                       : Builder->CreateSub(PromotedCurrent, PromotedR,
                                            "subtmp");
          break;
        case '*':
          Result = isFloat
                       ? Builder->CreateFMul(PromotedCurrent, PromotedR,
                                             "multmp")
                       : Builder->CreateMul(PromotedCurrent, PromotedR,
                                            "multmp");
          break;
        case '/':
          if (isFloat)
            Result =
                Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else if (compoundUnsigned)
            Result =
                Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result =
                Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result =
                Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else if (compoundUnsigned)
            Result =
                Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result =
                Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
        }

        return Result;
      }

      if (isCompoundBitwise) {
        if (currentVal->getType()->isFloatingPointTy() ||
            rhsValue->getType()->isFloatingPointTy() ||
            isDecimalLLVMType(currentVal->getType()) ||
            isDecimalLLVMType(rhsValue->getType()))
          return LogErrorV(
              "Bitwise compound assignment requires integer operands");

        auto [PromotedCurrent, PromotedR] = promoteTypes(
            currentVal, rhsValue, lhsPromoteType, rhsPromoteType);
        if (!PromotedCurrent || !PromotedR)
          return nullptr;
        auto lhsUnsigned = unsignedHintFromTypeName(
            sanitizeBaseTypeName(lhsPromoteType));
        auto rhsUnsigned = unsignedHintFromTypeName(
            sanitizeBaseTypeName(rhsPromoteType));
        bool compoundUnsigned =
            lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
        if (!compoundUnsigned) {
          compoundUnsigned = unsignedHintFromTypeName(
                                 sanitizeBaseTypeName(lhsPromoteType))
                                 .value_or(false);
        }

        llvm::Value *Result = nullptr;
        if (Op == "&=") {
          Result = Builder->CreateAnd(PromotedCurrent, PromotedR, "andtmp");
        } else if (Op == "|=") {
          Result = Builder->CreateOr(PromotedCurrent, PromotedR, "ortmp");
        } else if (Op == "^=") {
          Result = Builder->CreateXor(PromotedCurrent, PromotedR, "xortmp");
        } else if (Op == "<<=") {
          Result = Builder->CreateShl(PromotedCurrent, PromotedR, "shltmp");
        } else if (Op == ">>=") {
          if (compoundUnsigned)
            Result = Builder->CreateLShr(PromotedCurrent, PromotedR, "lshrtmp");
          else
            Result = Builder->CreateAShr(PromotedCurrent, PromotedR, "ashrtmp");
        } else {
          return LogErrorV("Unknown bitwise compound assignment operator");
        }

        return Result;
      }

      return nullptr;
    };

    auto emitPropertyAssignment =
        [&](MemberAccessExprAST &member) -> std::optional<llvm::Value *> {
      const std::string &propName = member.getMemberName();
      const CompositeTypeInfo *info = nullptr;
      const PropertyInfo *prop = nullptr;
      std::string ownerName;
      llvm::Value *instanceValue = nullptr;
      bool isTypeReference = false;

      if (auto *varObj =
              dynamic_cast<VariableExprAST *>(member.getObject())) {
        ownerName = varObj->getName();
        info = lookupCompositeInfo(ownerName);
        if (info) {
          isTypeReference = true;
          if (!shouldBypassPropertyAccess(ownerName, propName)) {
            auto propIt = info->properties.find(propName);
            if (propIt != info->properties.end())
              prop = &propIt->second;
          }
          if (prop && !prop->isStatic) {
            reportCompilerError("Instance property '" + propName +
                                "' cannot be accessed on type '" +
                                ownerName + "'");
            return std::optional<llvm::Value *>{nullptr};
          }
        }
      }

      if (isTypeReference) {
        if (!prop)
          return std::nullopt;
      } else {
        llvm::Value *objectValue = member.getObject()->codegen();
        if (!objectValue)
          return std::optional<llvm::Value *>{nullptr};

        std::string objectTypeName = member.getObject()->getTypeName();
        ParsedTypeDescriptor objectDesc = parseTypeString(objectTypeName);
        if (objectDesc.isNullable) {
          LogErrorV(("Cannot access nullable type '" + objectTypeName +
                     "' without null-safe operator")
                        .c_str());
          return std::optional<llvm::Value *>{nullptr};
        }

        if (member.isSafeSmartArrow()) {
          TypeInfo arrowInfo = makeTypeInfo(objectTypeName);
          finalizeTypeInfoMetadata(arrowInfo);
          if (!arrowInfo.isSmartPointer()) {
            LogErrorV("'->' requires a smart pointer outside unsafe contexts");
            return std::optional<llvm::Value *>{nullptr};
          }
          const CompositeTypeInfo *smartMeta =
              resolveSmartPointerMetadata(arrowInfo);
          if (!smartMeta) {
            LogErrorV("Internal error: missing smart pointer metadata");
            return std::optional<llvm::Value *>{nullptr};
          }
          const InstanceFieldInfo *payloadField = findInstanceField(
              *smartMeta,
              smartMeta->smartPointerKind == SmartPointerKind::Unique
                  ? "value"
                  : "payload");
          if (!payloadField) {
            LogErrorV("Internal error: missing smart pointer payload");
            return std::optional<llvm::Value *>{nullptr};
          }
          llvm::StructType *smartTy = StructTypes[stripNullableAnnotations(
              typeNameFromInfo(arrowInfo))];
          if (!smartTy) {
            LogErrorV("Internal error: missing smart pointer struct type");
            return std::optional<llvm::Value *>{nullptr};
          }
          llvm::Value *payloadVal = nullptr;
          if (objectValue->getType()->isPointerTy()) {
            llvm::Value *payloadPtr = Builder->CreateStructGEP(
                smartTy, objectValue, payloadField->index,
                "arrow.smart.payload.ptr");
            payloadVal = Builder->CreateLoad(
                getTypeFromString(typeNameFromInfo(payloadField->type)),
                payloadPtr, "arrow.smart.payload");
          } else {
            payloadVal = Builder->CreateExtractValue(
                objectValue, payloadField->index, "arrow.smart.payload");
          }
          if (!payloadField->type.typeName.empty())
            objectTypeName = payloadField->type.typeName;
          objectValue = payloadVal;
          objectDesc = parseTypeString(objectTypeName);
        }

        ownerName = objectDesc.sanitized;
        info = lookupCompositeInfo(ownerName);
        if (info && !shouldBypassPropertyAccess(ownerName, propName)) {
          auto propIt = info->properties.find(propName);
          if (propIt != info->properties.end())
            prop = &propIt->second;
        }

        if (!prop)
          return std::nullopt;

        instanceValue = objectValue;
      }

      if (!info || !prop)
        return std::nullopt;

      if (Op == "=") {
        if (!prop->hasSetter) {
          reportCompilerError("Property '" + propName +
                              "' does not define a setter");
          return std::optional<llvm::Value *>{nullptr};
        }
        if (!ensureMemberAccessAllowed(prop->modifiers, AccessIntent::Write,
                                       ownerName, propName))
          return std::optional<llvm::Value *>{nullptr};

        if (rhsIsNullableLocal && !typeAllowsNull(prop->type)) {
          reportCompilerError(
              "Cannot assign nullable value to non-nullable property '" +
              propName + "'");
          return std::optional<llvm::Value *>{nullptr};
        }
        if (!validateInvariantAssignment(
                prop->type, getRHS(),
                "assignment to property '" + propName + "'"))
          return std::optional<llvm::Value *>{nullptr};

        llvm::Value *rhsValue = emitAssignmentRHS(&prop->type);
        if (!rhsValue)
          return std::optional<llvm::Value *>{nullptr};

        const std::string propTypeName = typeNameFromInfo(prop->type);
        llvm::Type *propLLVMType = getTypeFromString(propTypeName);
        if (diagnoseDisallowedImplicitIntegerConversion(
                getRHS(), rhsValue, propLLVMType, propTypeName,
                "assignment to property '" + propName + "'"))
          return std::optional<llvm::Value *>{nullptr};

        auto setterIt = info->methodInfo.find(prop->setterName);
        if (setterIt == info->methodInfo.end()) {
          reportCompilerError(
              "Internal error: missing setter for property '" + propName +
              "'");
          return std::optional<llvm::Value *>{nullptr};
        }

        std::vector<llvm::Value *> setterArgs;
        std::vector<bool> setterIsRef;
        setterArgs.push_back(rhsValue);
        setterIsRef.push_back(false);
        if (!emitMemberCallByInfo(*info, setterIt->second, ownerName,
                                  member.getObject(), instanceValue,
                                  std::move(setterArgs),
                                  std::move(setterIsRef), nullptr)) {
          return std::optional<llvm::Value *>{nullptr};
        }

        noteMemberAssignment(ownerName, propName, prop->isStatic);
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      if (!prop->hasGetter) {
        reportCompilerError("Property '" + propName +
                            "' does not define a getter");
        return std::optional<llvm::Value *>{nullptr};
      }
      if (!prop->hasSetter) {
        reportCompilerError("Property '" + propName +
                            "' does not define a setter");
        return std::optional<llvm::Value *>{nullptr};
      }
      if (!ensureMemberAccessAllowed(prop->modifiers, AccessIntent::Read,
                                     ownerName, propName))
        return std::optional<llvm::Value *>{nullptr};
      if (!ensureMemberAccessAllowed(prop->modifiers, AccessIntent::Write,
                                     ownerName, propName))
        return std::optional<llvm::Value *>{nullptr};

      auto getterIt = info->methodInfo.find(prop->getterName);
      if (getterIt == info->methodInfo.end()) {
        reportCompilerError(
            "Internal error: missing getter for property '" + propName + "'");
        return std::optional<llvm::Value *>{nullptr};
      }
      auto setterIt = info->methodInfo.find(prop->setterName);
      if (setterIt == info->methodInfo.end()) {
        reportCompilerError(
            "Internal error: missing setter for property '" + propName + "'");
        return std::optional<llvm::Value *>{nullptr};
      }

      llvm::Value *currentVal =
          emitMemberCallByInfo(*info, getterIt->second, ownerName,
                               member.getObject(), instanceValue, {}, {},
                               nullptr);
      if (!currentVal)
        return std::optional<llvm::Value *>{nullptr};

      if (!R)
        R = getRHS()->codegen();
      if (!R)
        return std::optional<llvm::Value *>{nullptr};

      const std::string propTypeName = typeNameFromInfo(prop->type);
      std::string rhsPromoteType = getRHS()->getTypeName();
      llvm::Value *resultValue = computeCompoundAssignment(
          currentVal, propTypeName, R, rhsPromoteType);
      if (!resultValue)
        return std::optional<llvm::Value *>{nullptr};

      llvm::Value *resultToStore = resultValue;
      llvm::Type *propLLVMType = getTypeFromString(propTypeName);
      if (propLLVMType && resultToStore->getType() != propLLVMType) {
        resultToStore =
            castToType(resultToStore, propLLVMType, propTypeName);
        if (!resultToStore)
          return std::optional<llvm::Value *>{nullptr};
      }

      std::vector<llvm::Value *> setterArgs;
      std::vector<bool> setterIsRef;
      setterArgs.push_back(resultToStore);
      setterIsRef.push_back(false);
      if (!emitMemberCallByInfo(*info, setterIt->second, ownerName,
                                member.getObject(), instanceValue,
                                std::move(setterArgs),
                                std::move(setterIsRef), nullptr)) {
        return std::optional<llvm::Value *>{nullptr};
      }

      noteMemberAssignment(ownerName, propName, prop->isStatic);
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
    };

    auto emitIndexerAssignment =
        [&](ArrayIndexExprAST &indexExpr) -> std::optional<llvm::Value *> {
      llvm::Value *arrayValue = indexExpr.getArray()->codegen();
      if (!arrayValue)
        return std::optional<llvm::Value *>{nullptr};

      std::string arrayTypeName = indexExpr.getArray()->getTypeName();
      ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
      if (arrayDesc.isNullable) {
        reportCompilerError("Cannot index nullable type '" + arrayTypeName +
                            "' without null-safe operator");
        return std::optional<llvm::Value *>{nullptr};
      }

      std::string ownerName = arrayDesc.sanitized;
      if (ownerName.empty()) {
        if (auto arrayInfoOpt = resolveExprTypeInfo(indexExpr.getArray()))
          ownerName =
              sanitizeCompositeLookupName(typeNameFromInfo(*arrayInfoOpt));
      }
      if (ownerName.empty())
        return std::nullopt;

      const CompositeTypeInfo *info = lookupCompositeInfo(ownerName);
      if (!info || !info->indexer)
        return std::nullopt;

      const PropertyInfo &indexer = *info->indexer;
      if (indexer.isStatic) {
        reportCompilerError("Indexers must be instance members");
        return std::optional<llvm::Value *>{nullptr};
      }
      if ((isCompoundArithmetic || isCompoundBitwise) && !indexer.hasGetter) {
        reportCompilerError("Indexer on type '" + ownerName +
                            "' does not define a getter");
        return std::optional<llvm::Value *>{nullptr};
      }
      if (!indexer.hasSetter) {
        reportCompilerError("Indexer on type '" + ownerName +
                            "' does not define a setter");
        return std::optional<llvm::Value *>{nullptr};
      }

      if ((isCompoundArithmetic || isCompoundBitwise) &&
          !ensureMemberAccessAllowed(indexer.modifiers, AccessIntent::Read,
                                     ownerName, "this"))
        return std::optional<llvm::Value *>{nullptr};
      if (!ensureMemberAccessAllowed(indexer.modifiers, AccessIntent::Write,
                                     ownerName, "this"))
        return std::optional<llvm::Value *>{nullptr};

      std::vector<llvm::Value *> indexArgs;
      std::vector<bool> indexArgIsRef;
      const auto &indices = indexExpr.getIndices();
      indexArgs.reserve(indices.size());
      indexArgIsRef.reserve(indices.size());
      for (const auto &idxExpr : indices) {
        bool isRef = dynamic_cast<RefExprAST *>(idxExpr.get()) != nullptr;
        indexArgIsRef.push_back(isRef);
        llvm::Value *val = idxExpr->codegen();
        if (!val)
          return std::optional<llvm::Value *>{nullptr};
        indexArgs.push_back(val);
      }

      auto setterIt = info->methodInfo.find(indexer.setterName);
      if (setterIt == info->methodInfo.end()) {
        reportCompilerError("Internal error: missing indexer setter on '" +
                            ownerName + "'");
        return std::optional<llvm::Value *>{nullptr};
      }

      if (Op == "=") {
        if (rhsIsNullableLocal && !typeAllowsNull(indexer.type)) {
          reportCompilerError(
              "Cannot assign nullable value to non-nullable indexer on type '" +
              ownerName + "'");
          return std::optional<llvm::Value *>{nullptr};
        }
        if (!validateInvariantAssignment(
                indexer.type, getRHS(),
                "assignment to indexer on type '" + ownerName + "'"))
          return std::optional<llvm::Value *>{nullptr};

        llvm::Value *rhsValue = emitAssignmentRHS(&indexer.type);
        if (!rhsValue)
          return std::optional<llvm::Value *>{nullptr};

        const std::string indexerTypeName = typeNameFromInfo(indexer.type);
        llvm::Type *indexerLLVMType = getTypeFromString(indexerTypeName);
        if (diagnoseDisallowedImplicitIntegerConversion(
                getRHS(), rhsValue, indexerLLVMType, indexerTypeName,
                "assignment to indexer on type '" + ownerName + "'"))
          return std::optional<llvm::Value *>{nullptr};

        std::vector<llvm::Value *> setterArgs = indexArgs;
        std::vector<bool> setterIsRef = indexArgIsRef;
        setterArgs.push_back(rhsValue);
        setterIsRef.push_back(false);
        if (!emitMemberCallByInfo(*info, setterIt->second, ownerName,
                                  indexExpr.getArray(), arrayValue,
                                  std::move(setterArgs),
                                  std::move(setterIsRef), nullptr)) {
          return std::optional<llvm::Value *>{nullptr};
        }

        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      auto getterIt = info->methodInfo.find(indexer.getterName);
      if (getterIt == info->methodInfo.end()) {
        reportCompilerError("Internal error: missing indexer getter on '" +
                            ownerName + "'");
        return std::optional<llvm::Value *>{nullptr};
      }

      std::vector<llvm::Value *> getterArgs = indexArgs;
      std::vector<bool> getterIsRef = indexArgIsRef;
      llvm::Value *currentVal = emitMemberCallByInfo(
          *info, getterIt->second, ownerName, indexExpr.getArray(), arrayValue,
          std::move(getterArgs), std::move(getterIsRef), nullptr);
      if (!currentVal)
        return std::optional<llvm::Value *>{nullptr};

      if (!R)
        R = getRHS()->codegen();
      if (!R)
        return std::optional<llvm::Value *>{nullptr};

      const std::string indexerTypeName = typeNameFromInfo(indexer.type);
      std::string rhsPromoteType = getRHS()->getTypeName();
      llvm::Value *resultValue = computeCompoundAssignment(
          currentVal, indexerTypeName, R, rhsPromoteType);
      if (!resultValue)
        return std::optional<llvm::Value *>{nullptr};

      llvm::Value *resultToStore = resultValue;
      llvm::Type *indexerLLVMType = getTypeFromString(indexerTypeName);
      if (indexerLLVMType && resultToStore->getType() != indexerLLVMType) {
        resultToStore =
            castToType(resultToStore, indexerLLVMType, indexerTypeName);
        if (!resultToStore)
          return std::optional<llvm::Value *>{nullptr};
      }

      std::vector<llvm::Value *> setterArgs = indexArgs;
      std::vector<bool> setterIsRef = indexArgIsRef;
      setterArgs.push_back(resultToStore);
      setterIsRef.push_back(false);
      if (!emitMemberCallByInfo(*info, setterIt->second, ownerName,
                                indexExpr.getArray(), arrayValue,
                                std::move(setterArgs),
                                std::move(setterIsRef), nullptr)) {
        return std::optional<llvm::Value *>{nullptr};
      }

      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
    };

    if (auto *member = dynamic_cast<MemberAccessExprAST *>(getLHS())) {
      auto result = emitPropertyAssignment(*member);
      if (result.has_value())
        return *result;
    }
    if (auto *indexExpr = dynamic_cast<ArrayIndexExprAST *>(getLHS())) {
      auto result = emitIndexerAssignment(*indexExpr);
      if (result.has_value())
        return *result;
    }
  }

  llvm::Value *L = nullptr;
  if (!isAssignmentOp) {
    if (hasPrecomputedLhsValue)
      L = precomputedLhsValue;
    else
      L = getLHS()->codegen();
    if (!L)
      return nullptr;
  }

  // For comparisons, arithmetic, and equality operators, try to regenerate literals so they
  // match the other operand's type (numbers and chars).
  bool isComparisonOrArithmetic =
      (Op == "==" || Op == "!=" || Op == "<" || Op == ">" || Op == "<=" ||
       Op == ">=" || Op == "+" || Op == "-" || Op == "*" || Op == "/" ||
       Op == "%");
  if (isComparisonOrArithmetic) {
    if (NumberExprAST *NumRHS = dynamic_cast<NumberExprAST *>(getRHS())) {
      // Right side is a number literal - generate it with left's type as target
      R = NumRHS->codegen_with_target(L->getType());
    } else if (NumberExprAST *NumLHS = dynamic_cast<NumberExprAST *>(getLHS())) {
      // Left side is a number literal - regenerate it and the right side
      // This handles cases like: 255 == byte_var or 10 + byte_var
      llvm::Value *RTemp = getRHS()->codegen();
      if (RTemp) {
        L = NumLHS->codegen_with_target(RTemp->getType());
        R = RTemp;
      }
    }

    if (!R) {
      if (CharExprAST *CharRHS = dynamic_cast<CharExprAST *>(getRHS())) {
        R = CharRHS->codegen_with_target(L->getType(), getLHS()->getTypeName());
      } else if (CharExprAST *CharLHS = dynamic_cast<CharExprAST *>(getLHS())) {
        llvm::Value *RTemp = getRHS()->codegen();
        if (RTemp) {
          L = CharLHS->codegen_with_target(RTemp->getType(),
                                           getRHS()->getTypeName());
          R = RTemp;
        }
      }
    }
  }

  // If R wasn't generated yet (not a comparison or not a number literal), generate normally.
  // Defer generation for simple assignments so the RHS can be type-directed later.
  if (!R && Op != "=")
    R = getRHS()->codegen();

  if (Op != "=" && !R)
    return nullptr;

  // Get type names from operands after potential regeneration
  std::string rawLeftTypeName = getLHS()->getTypeName();
  std::string rawRightTypeName = getRHS()->getTypeName();
  std::string lhsSanitized = stripNullableAnnotations(rawLeftTypeName);
  std::string rhsSanitized = stripNullableAnnotations(rawRightTypeName);

  auto emitCharLikeToString = [&](llvm::Value *val,
                                  const std::string &typeName) -> llvm::Value * {
    std::string clean = stripNullableAnnotations(typeName);
    if (clean == "lchar")
      return LogErrorV("Cannot append 32-bit character 'lchar' to string");
    if (clean != "char" && clean != "schar")
      return nullptr;
    llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
    llvm::Value *asInt32 = val;
    if (!val->getType()->isIntegerTy(32)) {
      if (val->getType()->isIntegerTy() &&
          val->getType()->getIntegerBitWidth() < 32)
        asInt32 = Builder->CreateZExt(val, int32Ty, "str.char.zext");
      else if (val->getType()->isIntegerTy() &&
               val->getType()->getIntegerBitWidth() > 32)
        asInt32 = Builder->CreateTrunc(val, int32Ty, "str.char.trunc");
      else
        return LogErrorV("Cannot convert value to character for string append");
    }
    return Builder->CreateCall(getCharToStringFunction(), {asInt32},
                               "str.char");
  };

  auto emitStringConcat = [&](llvm::Value *leftStr, llvm::Value *rightStr) {
    llvm::Type *stringPtrTy = getTypeFromString("string");
    llvm::ArrayType *arrayTy = llvm::ArrayType::get(stringPtrTy, 2);
    llvm::AllocaInst *arrayAlloca =
        Builder->CreateAlloca(arrayTy, nullptr, "str.add.tmp");

    llvm::Value *zeroIdx =
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
    llvm::Value *lhsPtr =
        Builder->CreateInBoundsGEP(arrayTy, arrayAlloca, {zeroIdx, zeroIdx},
                                   "str.add.lhs");
    Builder->CreateStore(leftStr, lhsPtr);

    llvm::Value *oneIdx =
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 1);
    llvm::Value *rhsPtr =
        Builder->CreateInBoundsGEP(arrayTy, arrayAlloca, {zeroIdx, oneIdx},
                                   "str.add.rhs");
    Builder->CreateStore(rightStr, rhsPtr);

    llvm::Value *basePtr =
        Builder->CreateInBoundsGEP(arrayTy, arrayAlloca, {zeroIdx, zeroIdx},
                                   "str.add.base");
    llvm::Value *countVal = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), 2);
    llvm::FunctionCallee concatFn = getConcatStringsFunction();
    llvm::Value *result =
        Builder->CreateCall(concatFn, {basePtr, countVal}, "str.addtmp");
    setTypeName("string");
    markTemporary();
    return result;
  };

  if (Op == "+" && lhsSanitized == "string" && rhsSanitized == "string") {
    return emitStringConcat(ensureStringPointer(L), ensureStringPointer(R));
  }

  const bool lhsIsString = lhsSanitized == "string";
  const bool rhsIsString = rhsSanitized == "string";
  if (Op == "+" && (lhsIsString || rhsIsString)) {
    llvm::Value *lhsStr = nullptr;
    llvm::Value *rhsStr = nullptr;

    if (lhsIsString) {
      lhsStr = ensureStringPointer(L);
      rhsStr = rhsIsString ? ensureStringPointer(R)
                           : emitCharLikeToString(R, rawRightTypeName);
    } else {
      rhsStr = ensureStringPointer(R);
      lhsStr = emitCharLikeToString(L, rawLeftTypeName);
    }

    if (!lhsStr || !rhsStr)
      return nullptr;
    return emitStringConcat(lhsStr, rhsStr);
  }

  ParsedTypeDescriptor leftDesc = parseTypeString(rawLeftTypeName);
  ParsedTypeDescriptor rightDesc = parseTypeString(rawRightTypeName);

  if ((Op == "==" || Op == "!=") &&
      lhsSanitized == "string" && rhsSanitized == "string") {
    llvm::FunctionCallee equalsFn = getStringEqualsFunction();
    llvm::Value *cmpInt =
        Builder->CreateCall(equalsFn, {L, R}, "str.eqcall");
    llvm::Value *cmp = Builder->CreateICmpNE(
        cmpInt, llvm::ConstantInt::get(cmpInt->getType(), 0), "str.eqbool");
    if (Op == "!=")
      cmp = Builder->CreateNot(cmp, "str.neqcall");
    setTypeName("bool");
    return Builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  }

  if ((Op == "+" || Op == "-") &&
      (isPointerTypeDescriptor(leftDesc) || isPointerTypeDescriptor(rightDesc))) {
    const bool lhsPointer = isPointerTypeDescriptor(leftDesc);
    const bool rhsPointer = isPointerTypeDescriptor(rightDesc);

    if (lhsPointer && rhsPointer) {
      if (Op == "+")
        return LogErrorV("Cannot add two pointer values");

      if (leftDesc.sanitized != rightDesc.sanitized)
        return LogErrorV("Pointer subtraction requires both operands to have the same pointer type");

      auto elementNameOpt = getPointerElementTypeName(leftDesc.sanitized);
      if (!elementNameOpt)
        return LogErrorV("Cannot determine element type for pointer subtraction");

      llvm::Type *elementType = getTypeFromString(*elementNameOpt);
      if (!elementType)
        return LogErrorV("Unsupported element type for pointer subtraction");

      llvm::IntegerType *indexType = getPointerIndexType();
      llvm::Value *lhsInt = Builder->CreatePtrToInt(L, indexType, "ptrlhs.int");
      llvm::Value *rhsInt = Builder->CreatePtrToInt(R, indexType, "ptrrhs.int");
      llvm::Value *byteDiff = Builder->CreateSub(lhsInt, rhsInt, "ptrdiff.bytes");

      uint64_t elementSize = getTypeSizeInBytes(elementType);
      if (elementSize == 0)
        return LogErrorV("Cannot determine element size for pointer subtraction");

      llvm::Value *resultValue = nullptr;
      if (elementSize == 1) {
        resultValue = byteDiff;
      } else {
        llvm::Value *elemSizeConst = llvm::ConstantInt::get(indexType, elementSize);
        resultValue = Builder->CreateSDiv(byteDiff, elemSizeConst, "ptrdiff.elems");
      }

      const unsigned indexBits = indexType->getIntegerBitWidth();
      setTypeName(indexBits <= 32 ? "int" : "long");
      return resultValue;
    }

    if (Op == "-" && !lhsPointer)
      return LogErrorV("Pointer subtraction requires the left-hand side to be a pointer");

    const bool pointerOnLeft = lhsPointer;
    llvm::Value *ptrValue = pointerOnLeft ? L : R;
    llvm::Value *offsetValue = pointerOnLeft ? R : L;
    const ParsedTypeDescriptor &pointerDesc = pointerOnLeft ? leftDesc : rightDesc;
    const ParsedTypeDescriptor &offsetDesc = pointerOnLeft ? rightDesc : leftDesc;
    const std::string &pointerTypeName = pointerOnLeft ? rawLeftTypeName : rawRightTypeName;
    const std::string &offsetTypeName = pointerOnLeft ? rawRightTypeName : rawLeftTypeName;

    if (isPointerTypeDescriptor(offsetDesc))
      return LogErrorV("Pointer arithmetic requires an integer offset");
    if (offsetValue->getType()->isFloatingPointTy())
      return LogErrorV("Pointer arithmetic requires an integer offset");

    llvm::Value *resultPtr = emitPointerOffset(
        ptrValue, offsetValue, pointerTypeName, offsetTypeName,
        /*negateOffset=*/(Op == "-" && pointerOnLeft), "ptrarith");
    if (!resultPtr)
      return nullptr;

    setTypeName(pointerDesc.sanitized);
    return resultPtr;
  }

  if (Op == "+=" || Op == "-=" || Op == "*=" || Op == "/=" || Op == "%=") {
    // Compound assignment operators: a += b is equivalent to a = a + b
    auto emitMemberCompoundAssignment =
        [&](MemberAccessExprAST &member) -> llvm::Value * {
          if (!ensureMemberInitializedForMutation(member))
            return nullptr;

          auto fieldInfoOpt = collectMemberFieldAssignmentInfo(member);
          if (!fieldInfoOpt)
            return nullptr;
          auto &fieldInfo = *fieldInfoOpt;

          llvm::Value *CurrentVal =
              Builder->CreateLoad(fieldInfo.fieldType, fieldInfo.fieldPtr, "memberload");

          if (CurrentVal->getType()->isPointerTy()) {
            if (Op != "+=" && Op != "-=")
              return LogErrorV("Pointer compound assignment only supports '+=' and '-='");

            std::string pointerTypeName =
                !fieldInfo.rawFieldTypeName.empty() ? fieldInfo.rawFieldTypeName
                                                    : member.getTypeName();
            llvm::Value *NewPtr =
                emitPointerOffset(CurrentVal, R, pointerTypeName,
                                  getRHS()->getTypeName(), Op == "-=",
                                  "ptrarith");
            if (!NewPtr)
              return nullptr;

            Builder->CreateStore(NewPtr, fieldInfo.fieldPtr);
            noteMemberAssignment(fieldInfo.structName, member.getMemberName(),
                                 fieldInfo.isStatic);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }

          std::string lhsPromoteType =
              fieldInfo.sanitizedFieldTypeName.empty()
                  ? sanitizeBaseTypeName(member.getTypeName())
                  : fieldInfo.sanitizedFieldTypeName;
          std::string rhsPromoteType = getRHS()->getTypeName();

          auto [PromotedCurrent, PromotedR] =
              promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
          if (!PromotedCurrent || !PromotedR)
            return nullptr;
          if (isDecimalLLVMType(PromotedCurrent->getType())) {
            llvm::Value *Result =
                emitDecimalArithmeticBinary(Op[0], PromotedCurrent, PromotedR);
            if (!Result)
              return nullptr;

            llvm::Value *ResultToStore = Result;
            if (ResultToStore->getType() != fieldInfo.fieldType) {
              std::string castTargetName =
                  !lhsPromoteType.empty() ? lhsPromoteType : member.getTypeName();
              ResultToStore =
                  castToType(ResultToStore, fieldInfo.fieldType, castTargetName);
              if (!ResultToStore)
                return nullptr;
            }

            Builder->CreateStore(ResultToStore, fieldInfo.fieldPtr);
            noteMemberAssignment(fieldInfo.structName, member.getMemberName(),
                                 fieldInfo.isStatic);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }
          bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
          auto lhsUnsigned =
              unsignedHintFromTypeName(sanitizeBaseTypeName(lhsPromoteType));
          auto rhsUnsigned =
              unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
          bool compoundUnsigned =
              lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
          if (!compoundUnsigned) {
            compoundUnsigned = unsignedHintFromTypeName(
                                   sanitizeBaseTypeName(member.getTypeName()))
                                   .value_or(false);
          }

          llvm::Value *Result;
          char baseOp = Op[0];

          switch (baseOp) {
          case '+':
            Result = isFloat ? Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp")
                             : Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
            break;
          case '-':
            Result = isFloat ? Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp")
                             : Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
            break;
          case '*':
            Result = isFloat ? Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp")
                             : Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
            break;
          case '/':
            if (isFloat)
              Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
            else if (compoundUnsigned)
              Result = Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
            else
              Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
            break;
          case '%':
            if (isFloat)
              Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
            else if (compoundUnsigned)
              Result = Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
            else
              Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
            break;
          default:
            return LogErrorV("Unknown compound assignment operator");
          }

          llvm::Value *ResultToStore = Result;
          if (ResultToStore->getType() != fieldInfo.fieldType) {
            std::string castTargetName =
                !lhsPromoteType.empty() ? lhsPromoteType : member.getTypeName();
            ResultToStore =
                castToType(ResultToStore, fieldInfo.fieldType, castTargetName);
            if (!ResultToStore)
              return nullptr;
          }

          Builder->CreateStore(ResultToStore, fieldInfo.fieldPtr);
          noteMemberAssignment(fieldInfo.structName, member.getMemberName(),
                               fieldInfo.isStatic);
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        };

    if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
      // Simple variable compound assignment
      const std::string &varName = LHSE->getName();
      llvm::Value *Variable = NamedValues[varName];
      bool isLocal = true;
      if (!Variable) {
        Variable = GlobalValues[varName];
        isLocal = false;
        if (!Variable) {
          if (resolveInstanceMemberOwnerInCurrentContext(varName)) {
            auto object = std::make_unique<ThisExprAST>();
            MemberAccessExprAST synthetic(std::move(object), varName);
            return emitMemberCompoundAssignment(synthetic);
          }
          if (auto owner = resolveStaticFieldOwnerInCurrentContext(varName)) {
            auto object = std::make_unique<VariableExprAST>(*owner);
            MemberAccessExprAST synthetic(std::move(object), varName);
            return emitMemberCompoundAssignment(synthetic);
          }
          return LogErrorV("Unknown variable name for compound assignment");
        }
      }

      const TypeInfo *info = lookupTypeInfo(varName);
      if (info) {
        if (!validateInvariantAssignment(*info, getRHS(),
                                         "assignment to variable '" + varName + "'"))
          return nullptr;
      }
      std::string lhsPromoteType;
      if (info)
        lhsPromoteType = typeNameFromInfo(*info);
      else
        lhsPromoteType = LHSE->getTypeName();

      bool isAlias = info && info->isAlias();
      llvm::Value *StoragePtr = nullptr;   // Points to the actual value for alias variables
      llvm::Type *ValueType = nullptr;
      llvm::Value *CurrentVal = nullptr;
      llvm::AllocaInst *Alloca =
          isLocal ? llvm::dyn_cast<llvm::AllocaInst>(Variable) : nullptr;
      llvm::GlobalVariable *GV =
          isLocal ? nullptr : llvm::dyn_cast<llvm::GlobalVariable>(Variable);

      if (isAlias) {
        ValueType = info ? getTypeFromString(info->typeName) : nullptr;
        if (!ValueType)
          return LogErrorV("Invalid type for ref variable in compound assignment");
        llvm::Type *slotType =
            Alloca ? Alloca->getAllocatedType()
                   : GV   ? GV->getValueType()
                          : nullptr;
        const bool hasPointerSlot = slotType && slotType->isPointerTy();
        if (hasPointerSlot) {
          StoragePtr = Builder->CreateLoad(slotType, Variable,
                                           (varName + "_ptr").c_str());
        } else if (info) {
          StoragePtr = materializeAliasPointer(Variable, *info, varName);
        }
        if (!StoragePtr || !StoragePtr->getType()->isPointerTy())
          return LogErrorV("Invalid ref storage for compound assignment");
        CurrentVal = Builder->CreateLoad(ValueType, StoragePtr, varName.c_str());
      } else {
        if (isLocal) {
          if (!Alloca)
            return LogErrorV("Internal error: expected stack storage for compound assignment");
          ValueType = Alloca->getAllocatedType();
          CurrentVal = Builder->CreateLoad(ValueType, Variable, varName.c_str());
        } else {
          if (!GV)
            return LogErrorV("Internal error: expected global storage for compound assignment");
          ValueType = GV->getValueType();
          CurrentVal = Builder->CreateLoad(ValueType, Variable, varName.c_str());
        }
      }

      if (!CurrentVal || !ValueType)
        return LogErrorV("Failed to load value for compound assignment");

      if (CurrentVal->getType()->isPointerTy()) {
        if (Op != "+=" && Op != "-=")
          return LogErrorV("Pointer compound assignment only supports '+=' and '-='");

        std::string pointerTypeName = lhsPromoteType.empty() ? getLHS()->getTypeName() : lhsPromoteType;
        llvm::Value *NewPtr = emitPointerOffset(CurrentVal, R, pointerTypeName,
                                                getRHS()->getTypeName(), Op == "-=", "ptrarith");
        if (!NewPtr)
          return nullptr;

        if (isAlias) {
          Builder->CreateStore(NewPtr, StoragePtr);
        } else {
          Builder->CreateStore(NewPtr, Variable);
        }

        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)

      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      if (!R)
        return LogErrorV("Internal error: RHS missing before compound assignment");
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      if (isDecimalLLVMType(PromotedCurrent->getType())) {
        Result = emitDecimalArithmeticBinary(baseOp, PromotedCurrent, PromotedR);
        if (!Result)
          return nullptr;
      } else {
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      auto lhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(lhsPromoteType));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(getLHS()->getTypeName())).value_or(false);
      }
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      }
      
      // Cast result back to the original storage type if needed
      llvm::Value *ResultToStore = Result;
      if (ResultToStore->getType() != ValueType) {
        std::string castTargetName = !lhsPromoteType.empty() ? lhsPromoteType : LHSE->getTypeName();
        ResultToStore = castToType(ResultToStore, ValueType, castTargetName);
        if (!ResultToStore)
          return nullptr;
      }

      // Store the result back to the correct destination
      if (isAlias) {
        Builder->CreateStore(ResultToStore, StoragePtr);
      } else {
        Builder->CreateStore(ResultToStore, Variable);
      }
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
    } else if (ArrayIndexExprAST *LHSE = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
      // Array element compound assignment: arr[i] += val
      auto accessOpt = computeArrayElementAccess(LHSE);
      if (!accessOpt)
        return nullptr;
      auto &access = *accessOpt;

      llvm::Value *ElementPtr = access.elementPtr;
      llvm::Type *ElemType = access.elementLLVMType;
      if (!ElementPtr || !ElemType || !ElementPtr->getType()->isPointerTy())
        return LogErrorV("Invalid array element pointer");

      std::string elementTypeNameForPromotion = sanitizeBaseTypeName(access.elementTypeName);
      if (elementTypeNameForPromotion.empty())
        elementTypeNameForPromotion = sanitizeBaseTypeName(LHSE->getTypeName());
      
      // Load current value
      llvm::Value *CurrentVal = Builder->CreateLoad(ElemType, ElementPtr, "arrayload");
      
      if (CurrentVal->getType()->isPointerTy()) {
        if (Op != "+=" && Op != "-=")
          return LogErrorV("Pointer compound assignment only supports '+=' and '-='");

        std::string pointerTypeName = access.elementTypeName.empty() ? LHSE->getTypeName() : access.elementTypeName;
        llvm::Value *NewPtr = emitPointerOffset(CurrentVal, R, pointerTypeName,
                                                getRHS()->getTypeName(), Op == "-=", "ptrarith");
        if (!NewPtr)
          return nullptr;

        Builder->CreateStore(NewPtr, ElementPtr);
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)
      
      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      if (isDecimalLLVMType(PromotedCurrent->getType())) {
        Result = emitDecimalArithmeticBinary(baseOp, PromotedCurrent, PromotedR);
        if (!Result)
          return nullptr;
      } else {
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      auto elementUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(elementTypeNameForPromotion));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = elementUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(LHSE->getArray()->getTypeName())).value_or(false);
      }
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      }
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
    } else if (MemberAccessExprAST *LHSMA = dynamic_cast<MemberAccessExprAST*>(getLHS())) {
      return emitMemberCompoundAssignment(*LHSMA);
    } else {
      return LogErrorV("destination of compound assignment must be a variable, array element, or public struct/class member");
    }
  }

  std::string leftTypeName = sanitizeBaseTypeName(rawLeftTypeName);
  std::string rightTypeName = sanitizeBaseTypeName(rawRightTypeName);

  auto leftUnsignedHint = unsignedHintFromTypeName(leftTypeName);
  auto rightUnsignedHint = unsignedHintFromTypeName(rightTypeName);
  bool preferUnsigned = leftUnsignedHint.value_or(false) || rightUnsignedHint.value_or(false);

  llvm::Type *resultType = nullptr;
  bool isFloat = false;
  bool isDecimal = false;

  if (!isAssignmentOp) {
    // Promote types to compatible types
    auto promoted = promoteTypes(L, R, leftTypeName, rightTypeName);
    L = promoted.first;
    R = promoted.second;
    if (!L || !R)
      return nullptr;

    // Check if working with floating point, decimal, or integer types
    resultType = L->getType();
    isFloat = resultType->isFloatingPointTy();
    isDecimal = isDecimalLLVMType(resultType);

    // Set result type name based on the promoted type
    if (isDecimal) {
      setTypeName("decimal");
    } else if (isFloat) {
      setTypeName(resultType->isFloatTy() ? "float" : "double");
    } else {
      const unsigned bitWidth = resultType->getIntegerBitWidth();
      auto pickMatchingName = [&](const std::string &candidate) -> std::string {
        if (candidate.empty())
          return {};
        std::string clean = sanitizeBaseTypeName(candidate);
        switch (bitWidth) {
        case 8:
          if (clean == "byte" || clean == "sbyte" || clean == "schar")
            return clean;
          break;
        case 16:
          if (clean == "short" || clean == "ushort" || clean == "char")
            return clean;
          break;
        case 32:
          if (clean == "int" || clean == "uint" || clean == "lchar")
            return clean;
          break;
        case 64:
          if (clean == "long" || clean == "ulong")
            return clean;
          break;
        default:
          break;
        }
        return {};
      };

      std::string chosen = pickMatchingName(rawLeftTypeName);
      if (chosen.empty())
        chosen = pickMatchingName(rawRightTypeName);
      if (chosen.empty()) {
        switch (bitWidth) {
        case 8:
          chosen = "sbyte";
          break;
        case 16:
          chosen = "char";
          break;
        case 32:
          chosen = "int";
          break;
        case 64:
          chosen = "long";
          break;
        default:
          chosen = "int";
          break;
        }
      }

      setTypeName(chosen);

      if (!preferUnsigned) {
        std::string chosenBase = sanitizeBaseTypeName(chosen);
        preferUnsigned = unsignedHintFromTypeName(chosenBase).value_or(false);
      }
    }
  }

  // Handle single-character operators
  if (Op.length() == 1) {
    switch (Op[0]) {
    case '+':
      if (isDecimal)
        return emitDecimalArithmeticBinary('+', L, R);
      if (isFloat)
        return Builder->CreateFAdd(L, R, "addtmp");
      else
        return Builder->CreateAdd(L, R, "addtmp");
    case '-':
      if (isDecimal)
        return emitDecimalArithmeticBinary('-', L, R);
      if (isFloat)
        return Builder->CreateFSub(L, R, "subtmp");
      else
        return Builder->CreateSub(L, R, "subtmp");
    case '*':
      if (isDecimal)
        return emitDecimalArithmeticBinary('*', L, R);
      if (isFloat)
        return Builder->CreateFMul(L, R, "multmp");
      else
        return Builder->CreateMul(L, R, "multmp");
    case '/':
      if (isDecimal)
        return emitDecimalArithmeticBinary('/', L, R);
      if (isFloat)
        return Builder->CreateFDiv(L, R, "divtmp");
      else if (preferUnsigned)
        return Builder->CreateUDiv(L, R, "divtmp");
      else
        return Builder->CreateSDiv(L, R, "divtmp");
    case '%':
      if (isDecimal)
        return emitDecimalArithmeticBinary('%', L, R);
      if (isFloat)
        return Builder->CreateFRem(L, R, "modtmp");
      else if (preferUnsigned)
        return Builder->CreateURem(L, R, "modtmp");
      else
        return Builder->CreateSRem(L, R, "modtmp");
    case '<':
      setTypeName("bool");
      if (isDecimal) {
        llvm::Value *cmp = emitDecimalComparisonI1("<", L, R);
        if (!cmp)
          return nullptr;
        return Builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*TheContext),
                                   "booltmp");
      }
      if (isFloat) {
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
      } else {
        if (preferUnsigned)
          L = Builder->CreateICmpULT(L, R, "cmptmp");
        else
          L = Builder->CreateICmpSLT(L, R, "cmptmp");
      }
      // Zero-extend i1 to i8 for bool type
      return Builder->CreateZExt(L, llvm::Type::getInt8Ty(*TheContext), "booltmp");
    case '>':
      setTypeName("bool");
      if (isDecimal) {
        llvm::Value *cmp = emitDecimalComparisonI1(">", L, R);
        if (!cmp)
          return nullptr;
        return Builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*TheContext),
                                   "booltmp");
      }
      if (isFloat) {
        L = Builder->CreateFCmpUGT(L, R, "cmptmp");
      } else {
        if (preferUnsigned)
          L = Builder->CreateICmpUGT(L, R, "cmptmp");
        else
          L = Builder->CreateICmpSGT(L, R, "cmptmp");
      }
      // Zero-extend i1 to i8 for bool type
      return Builder->CreateZExt(L, llvm::Type::getInt8Ty(*TheContext), "booltmp");
    case '=':
      // Assignment operator - LHS must be a variable or array element
      {
        const ExprAST *rhsCheckExpr = unwrapRefExpr(getRHS());
        bool rhsIsNullable = expressionIsNullable(rhsCheckExpr);
        bool rhsIsTemporary = getRHS() && getRHS()->isTemporary();
        auto *rhsVarExpr = dynamic_cast<VariableExprAST *>(getRHS());
        auto emitAssignmentRHS = [&](const TypeInfo *targetInfo) -> llvm::Value * {
          llvm::Value *rhsVal = R;
          if (targetInfo && targetInfo->isSmartPointer()) {
            resolveSmartPointerMetadata(*targetInfo);
            if (auto *hashInit =
                    dynamic_cast<UnaryExprAST *>(getRHS())) {
              if (hashInit->getOp() == "#") {
                if (auto parenInit = convertHashShorthandToParen(*hashInit)) {
                  RHS = std::move(parenInit);
                  rhsVal = nullptr;
                  R = nullptr;
                }
              }
            }
          }
          if (targetInfo)
            propagateTypeToNewExpr(getRHS(), *targetInfo);

          if (auto *paren = dynamic_cast<ParenExprAST *>(getRHS())) {
            if (targetInfo) {
              if (auto constructed =
                      emitTargetTypedConstruction(*targetInfo, *paren)) {
                rhsVal = constructed;
                if (targetInfo->requiresARC())
                  rhsIsTemporary = true;
              }
            }
          }

          if (targetInfo) {
            if (const DelegateTypeInfo *delegateInfo =
                    lookupDelegateInfo(*targetInfo)) {
              bool handled = false;
              llvm::Value *delegateValue = emitDelegateValueForTarget(
                  *delegateInfo, getRHS(), "assignment", handled);
              if (handled) {
                rhsVal = delegateValue;
                R = rhsVal;
                return rhsVal;
              }
            }
          }

          if (!rhsVal)
            if (targetInfo) {
              std::string targetTypeName = typeNameFromInfo(*targetInfo);
              llvm::Type *targetType =
                  targetTypeName.empty() ? nullptr : getTypeFromString(targetTypeName);
              if (targetType && isDecimalLLVMType(targetType)) {
                if (auto *num = dynamic_cast<NumberExprAST *>(getRHS()))
                  rhsVal = num->codegen_with_target(targetType);
              }
            }

          if (!rhsVal)
            rhsVal = getRHS()->codegen();

          R = rhsVal;
          return rhsVal;
        };

        auto emitMemberAssignment =
            [&](MemberAccessExprAST &member) -> llvm::Value * {
              auto fieldInfoOpt = collectMemberFieldAssignmentInfo(member);
              if (!fieldInfoOpt)
                return nullptr;
              auto &fieldInfo = *fieldInfoOpt;

              if (!validateInvariantAssignment(fieldInfo.declaredFieldType, getRHS(),
                                               "assignment to field '" +
                                                   member.getMemberName() +
                                                   "'"))
                return nullptr;

              if (rhsIsNullable && !fieldInfo.allowsNull) {
                return LogErrorV(
                    ("Cannot assign nullable value to non-nullable field '" +
                     member.getMemberName() + "'")
                        .c_str());
              }

              if (fieldInfo.declaredFieldType.isArray) {
                if (auto *newExpr = extractNewArrayExpr(getRHS())) {
                  if (!emitArrayResizeAssignment(fieldInfo.fieldPtr,
                                                 fieldInfo.declaredFieldType,
                                                 *newExpr,
                                                 member.getMemberName(),
                                                 nullptr))
                    return nullptr;
                  noteMemberAssignment(fieldInfo.structName,
                                       member.getMemberName(),
                                       fieldInfo.isStatic);
                  setTypeName("void");
                  return llvm::UndefValue::get(
                      llvm::Type::getVoidTy(*TheContext));
                }
              }

              std::string diagFieldTypeName =
                  fieldInfo.sanitizedFieldTypeName;

              llvm::Value *rhsValue =
                  emitAssignmentRHS(&fieldInfo.declaredFieldType);
              if (!rhsValue)
                return nullptr;

              std::string contextDescription =
                  "assignment to field '" + member.getMemberName() + "'";
              if (diagnoseDisallowedImplicitIntegerConversion(
                      getRHS(), rhsValue, fieldInfo.fieldType,
                      diagFieldTypeName, contextDescription))
                return nullptr;

              if (!diagFieldTypeName.empty())
                rhsValue = castToType(rhsValue, fieldInfo.fieldType,
                                      diagFieldTypeName);
              else
                rhsValue = castToType(rhsValue, fieldInfo.fieldType);

              if (fieldInfo.declaredFieldType.isSmartPointer()) {
                const std::string constructedName =
                    stripNullableAnnotations(
                        typeNameFromInfo(fieldInfo.declaredFieldType));
                auto structIt = StructTypes.find(constructedName);
                llvm::StructType *structTy =
                    structIt != StructTypes.end() ? structIt->second : nullptr;
                const CompositeTypeInfo *metadata =
                    resolveSmartPointerMetadata(fieldInfo.declaredFieldType);
                if (!structTy || !metadata) {
                  reportCompilerError(
                      "Initializer for '" + member.getMemberName() +
                      "' has incompatible smart pointer representation");
                  return nullptr;
                }

                if (rhsVarExpr) {
                  if (emitSmartPointerInitFromVariable(
                          fieldInfo.declaredFieldType, fieldInfo.fieldPtr,
                          *rhsVarExpr, member.getMemberName())) {
                    noteMemberAssignment(fieldInfo.structName,
                                         member.getMemberName(),
                                         fieldInfo.isStatic);
                    setTypeName("void");
                    return llvm::UndefValue::get(
                        llvm::Type::getVoidTy(*TheContext));
                  }
                }

                if (!metadata->smartPointerDestroyHelper.empty()) {
                  llvm::Function *destroyFn = TheModule->getFunction(
                      metadata->smartPointerDestroyHelper);
                  if (!destroyFn) {
                    reportCompilerError(
                        "Internal error: missing smart pointer destroy helper '" +
                        metadata->smartPointerDestroyHelper + "'");
                    return nullptr;
                  }
                  llvm::Value *destroyArg = fieldInfo.fieldPtr;
                  llvm::Type *expectedTy =
                      destroyFn->getFunctionType()->getParamType(0);
                  if (expectedTy && destroyArg->getType() != expectedTy) {
                    destroyArg = Builder->CreateBitCast(
                        destroyArg, expectedTy,
                        buildArcOpLabel(member.getMemberName(),
                                        "smart.destroy.cast"));
                  }
                  Builder->CreateCall(destroyFn, {destroyArg});
                }

                llvm::Value *stored = rhsValue;
                if (stored->getType()->isPointerTy()) {
                  stored = Builder->CreateLoad(
                      structTy, stored,
                      buildArcOpLabel(member.getMemberName(),
                                      "smart.assign.load"));
                }
                if (stored->getType() != structTy) {
                  reportCompilerError(
                      "Initializer for '" + member.getMemberName() +
                      "' has incompatible smart pointer representation");
                  return nullptr;
                }

                Builder->CreateStore(stored, fieldInfo.fieldPtr);
              } else if (fieldInfo.declaredFieldType.requiresARC()) {
                emitManagedStore(fieldInfo.fieldPtr, rhsValue,
                                 fieldInfo.declaredFieldType,
                                 member.getMemberName(), rhsIsTemporary);
              } else {
                Builder->CreateStore(rhsValue, fieldInfo.fieldPtr);
              }
              noteMemberAssignment(fieldInfo.structName, member.getMemberName(),
                                   fieldInfo.isStatic);
              setTypeName("void");
              return llvm::UndefValue::get(
                  llvm::Type::getVoidTy(*TheContext));
            };

      if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
        // Simple variable assignment - check local first, then global
        llvm::Value *Variable = NamedValues[LHSE->getName()];
        if (Variable) {
          // Local variable
          llvm::AllocaInst *Alloca = llvm::dyn_cast<llvm::AllocaInst>(Variable);

          if (const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName()); info && info->isAlias()) {
            llvm::Type *slotType = Alloca ? Alloca->getAllocatedType() : nullptr;
            const bool hasPointerSlot = slotType && slotType->isPointerTy();
            if (dynamic_cast<RefExprAST *>(getRHS())) {
              if (!hasPointerSlot)
                return LogErrorV(("Cannot rebind ref parameter '" + LHSE->getName() + "'").c_str());
              llvm::Value *newPtr = getRHS()->codegen();
              if (!newPtr)
                return nullptr;
              if (slotType && newPtr->getType() != slotType)
                newPtr = Builder->CreateBitCast(
                    newPtr, slotType,
                    (LHSE->getName() + ".rebind.cast").c_str());
              Builder->CreateStore(newPtr, Variable);
              updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
              setTypeName("void");
              return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
            }

            llvm::Value *Ptr = nullptr;
            if (hasPointerSlot)
              Ptr = Builder->CreateLoad(slotType, Variable, (LHSE->getName() + "_ptr").c_str());
            else
              Ptr = materializeAliasPointer(Variable, *info, LHSE->getName());
            if (!Ptr)
              return nullptr;
            llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
            if (!ActualLLVMType)
              return LogErrorV("Invalid type for ref variable");
            if (info->isArray) {
              if (auto *newExpr = extractNewArrayExpr(getRHS())) {
                std::vector<int64_t> constantDims;
                if (!emitArrayResizeAssignment(Ptr, *info, *newExpr,
                                               LHSE->getName(),
                                               &constantDims))
                  return nullptr;
                if (!constantDims.empty())
                  ArraySizes[LHSE->getName()] = constantDims;
                else
                  ArraySizes.erase(LHSE->getName());
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }

              if (!validateArrayLiteralAssignmentSize(
                      LHSE->getName(), *info,
                      extractArrayLiteralExpr(getRHS()))) {
                return nullptr;
              }
            }
            llvm::Value *rhsValue = emitAssignmentRHS(info);
            if (!rhsValue)
              return nullptr;
            if (info &&
                !validateTupleAssignmentCompatibility(
                    *info, getRHS(),
                    "assignment to variable '" + LHSE->getName() + "'"))
              return nullptr;
            if (diagnoseDisallowedImplicitIntegerConversion(getRHS(), rhsValue, ActualLLVMType, info->typeName, "assignment to '" + LHSE->getName() + "'"))
              return nullptr;
            rhsValue = castToType(rhsValue, ActualLLVMType);
            Builder->CreateStore(rhsValue, Ptr);
            updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }

          // Regular variable assignment
          if (!Alloca)
            return LogErrorV("Internal error: expected stack storage for local variable");
          llvm::Type *VarType = Alloca->getAllocatedType();
          if (const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName())) {
            if (rhsIsNullable && !typeAllowsNull(*info)) {
              return LogErrorV(("Cannot assign nullable value to non-nullable variable '" + LHSE->getName() + "'").c_str());
            }
            if (info->isArray) {
              if (auto *newExpr = extractNewArrayExpr(getRHS())) {
                std::vector<int64_t> constantDims;
                if (!emitArrayResizeAssignment(Variable, *info, *newExpr,
                                               LHSE->getName(),
                                               &constantDims))
                  return nullptr;
                if (!constantDims.empty())
                  ArraySizes[LHSE->getName()] = constantDims;
                else
                  ArraySizes.erase(LHSE->getName());
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }

              if (!validateArrayLiteralAssignmentSize(
                      LHSE->getName(), *info,
                      extractArrayLiteralExpr(getRHS()))) {
                return nullptr;
              }
            }
          }

          llvm::Value *rhsValue =
              emitAssignmentRHS(lookupLocalTypeInfo(LHSE->getName()));
          if (!rhsValue)
            return nullptr;

          const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName());
          if (info &&
              !validateTupleAssignmentCompatibility(
                  *info, getRHS(),
                  "assignment to variable '" + LHSE->getName() + "'"))
            return nullptr;
          if (info && info->isSmartPointer()) {
            const std::string constructedName =
                stripNullableAnnotations(typeNameFromInfo(*info));
            auto structIt = StructTypes.find(constructedName);
            llvm::StructType *structTy =
                structIt != StructTypes.end() ? structIt->second : nullptr;
            const CompositeTypeInfo *metadata =
                resolveSmartPointerMetadata(*info);
            if (!structTy || !metadata) {
              reportCompilerError(
                  "Initializer for '" + LHSE->getName() +
                  "' has incompatible smart pointer representation");
              return nullptr;
            }

            if (rhsVarExpr && rhsVarExpr->getName() != LHSE->getName()) {
              if (emitSmartPointerInitFromVariable(*info, Variable, *rhsVarExpr,
                                                   LHSE->getName())) {
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }
            }

            if (!metadata->smartPointerDestroyHelper.empty()) {
              llvm::Function *destroyFn = TheModule->getFunction(
                  metadata->smartPointerDestroyHelper);
              if (!destroyFn) {
                reportCompilerError(
                    "Internal error: missing smart pointer destroy helper '" +
                    metadata->smartPointerDestroyHelper + "'");
                return nullptr;
              }
              llvm::Value *destroyArg = Variable;
              llvm::Type *expectedTy =
                  destroyFn->getFunctionType()->getParamType(0);
              if (expectedTy && destroyArg->getType() != expectedTy) {
                destroyArg = Builder->CreateBitCast(
                    destroyArg, expectedTy,
                    buildArcOpLabel(LHSE->getName(), "smart.destroy.cast"));
              }
              Builder->CreateCall(destroyFn, {destroyArg});
            }

            llvm::Value *stored = rhsValue;
            if (stored->getType()->isPointerTy()) {
              stored = Builder->CreateLoad(
                  structTy, stored,
                  buildArcOpLabel(LHSE->getName(), "smart.assign.load"));
            }
            if (stored->getType() != structTy) {
              reportCompilerError(
                  "Initializer for '" + LHSE->getName() +
                  "' has incompatible smart pointer representation");
              return nullptr;
            }

            Builder->CreateStore(stored, Variable);
            updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
            setTypeName("void");
            return llvm::UndefValue::get(
                llvm::Type::getVoidTy(*TheContext));
          }

          // Get type name for proper range checking
          std::string targetTypeName = info && !info->typeName.empty() ? info->typeName : LHSE->getTypeName();
          if (diagnoseDisallowedImplicitIntegerConversion(getRHS(), rhsValue, VarType, targetTypeName, "assignment to '" + LHSE->getName() + "'"))
            return nullptr;
          if (info && !info->typeName.empty()) {
            rhsValue = castToType(rhsValue, VarType, info->typeName);
          } else {
            rhsValue = castToType(rhsValue, VarType);
          }
          if (info && info->requiresARC() && !info->isSmartPointer())
            emitManagedStore(Variable, rhsValue, *info, LHSE->getName(),
                             rhsIsTemporary);
          else
            Builder->CreateStore(rhsValue, Variable);
          updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        }

        // Check global scope
        llvm::GlobalVariable *GV = GlobalValues[LHSE->getName()];
        if (GV) {
          if (const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName()); info && info->isAlias()) {
            llvm::Type *slotType = GV->getValueType();
            const bool hasPointerSlot = slotType && slotType->isPointerTy();
            if (dynamic_cast<RefExprAST *>(getRHS())) {
              if (!hasPointerSlot)
                return LogErrorV(("Cannot rebind ref parameter '" + LHSE->getName() + "'").c_str());
              llvm::Value *newPtr = getRHS()->codegen();
              if (!newPtr)
                return nullptr;
              llvm::Type *expectedPtrTy = slotType;
              if (newPtr->getType() != expectedPtrTy)
                newPtr = Builder->CreateBitCast(
                    newPtr, expectedPtrTy,
                    (LHSE->getName() + ".rebind.cast").c_str());
              Builder->CreateStore(newPtr, GV);
              updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
              setTypeName("void");
              return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
            }

            llvm::Value *Ptr = hasPointerSlot
                                   ? Builder->CreateLoad(slotType, GV,
                                                         (LHSE->getName() + "_ptr").c_str())
                                   : materializeAliasPointer(GV, *info, LHSE->getName());
            if (!Ptr)
              return nullptr;
            llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
            if (!ActualLLVMType)
              return LogErrorV("Invalid type for ref variable");
            if (info->isArray) {
              if (auto *newExpr = extractNewArrayExpr(getRHS())) {
                std::vector<int64_t> constantDims;
                if (!emitArrayResizeAssignment(Ptr, *info, *newExpr,
                                               LHSE->getName(),
                                               &constantDims))
                  return nullptr;
                if (!constantDims.empty())
                  ArraySizes[LHSE->getName()] = constantDims;
                else
                  ArraySizes.erase(LHSE->getName());
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }

              if (!validateArrayLiteralAssignmentSize(
                      LHSE->getName(), *info,
                      extractArrayLiteralExpr(getRHS()))) {
                return nullptr;
              }
            }
            llvm::Value *rhsValue = emitAssignmentRHS(info);
            if (!rhsValue)
              return nullptr;
            if (info &&
                !validateTupleAssignmentCompatibility(
                    *info, getRHS(),
                    "assignment to variable '" + LHSE->getName() + "'"))
              return nullptr;
            if (diagnoseDisallowedImplicitIntegerConversion(getRHS(), rhsValue, ActualLLVMType, info->typeName, "assignment to '" + LHSE->getName() + "'"))
              return nullptr;
            rhsValue = castToType(rhsValue, ActualLLVMType);
            Builder->CreateStore(rhsValue, Ptr);
            updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }

          // Regular global variable
          llvm::Type *VarType = GV->getValueType();
          if (const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName())) {
            if (!info->isAlias() && rhsIsNullable && !typeAllowsNull(*info)) {
              return LogErrorV(("Cannot assign nullable value to non-nullable variable '" + LHSE->getName() + "'").c_str());
            }
            if (info->isArray) {
              if (auto *newExpr = extractNewArrayExpr(getRHS())) {
                std::vector<int64_t> constantDims;
                if (!emitArrayResizeAssignment(GV, *info, *newExpr,
                                               LHSE->getName(),
                                               &constantDims))
                  return nullptr;
                if (!constantDims.empty())
                  ArraySizes[LHSE->getName()] = constantDims;
                else
                  ArraySizes.erase(LHSE->getName());
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }

              if (!validateArrayLiteralAssignmentSize(
                      LHSE->getName(), *info,
                      extractArrayLiteralExpr(getRHS()))) {
                return nullptr;
              }
            }
          }

          llvm::Value *rhsValue =
              emitAssignmentRHS(lookupGlobalTypeInfo(LHSE->getName()));
          if (!rhsValue)
            return nullptr;

          const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName());
          if (info &&
              !validateTupleAssignmentCompatibility(
                  *info, getRHS(),
                  "assignment to variable '" + LHSE->getName() + "'"))
            return nullptr;
          if (info && info->isSmartPointer()) {
            const std::string constructedName =
                stripNullableAnnotations(typeNameFromInfo(*info));
            auto structIt = StructTypes.find(constructedName);
            llvm::StructType *structTy =
                structIt != StructTypes.end() ? structIt->second : nullptr;
            const CompositeTypeInfo *metadata =
                resolveSmartPointerMetadata(*info);
            if (!structTy || !metadata) {
              reportCompilerError(
                  "Initializer for '" + LHSE->getName() +
                  "' has incompatible smart pointer representation");
              return nullptr;
            }

            if (rhsVarExpr && rhsVarExpr->getName() != LHSE->getName()) {
              if (emitSmartPointerInitFromVariable(*info, GV, *rhsVarExpr,
                                                   LHSE->getName())) {
                updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
                setTypeName("void");
                return llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*TheContext));
              }
            }

            if (!metadata->smartPointerDestroyHelper.empty()) {
              llvm::Function *destroyFn = TheModule->getFunction(
                  metadata->smartPointerDestroyHelper);
              if (!destroyFn) {
                reportCompilerError(
                    "Internal error: missing smart pointer destroy helper '" +
                    metadata->smartPointerDestroyHelper + "'");
                return nullptr;
              }
              llvm::Value *destroyArg = GV;
              llvm::Type *expectedTy =
                  destroyFn->getFunctionType()->getParamType(0);
              if (expectedTy && destroyArg->getType() != expectedTy) {
                destroyArg = Builder->CreateBitCast(
                    destroyArg, expectedTy,
                    buildArcOpLabel(LHSE->getName(), "smart.destroy.cast"));
              }
              Builder->CreateCall(destroyFn, {destroyArg});
            }

            llvm::Value *stored = rhsValue;
            if (stored->getType()->isPointerTy()) {
              stored = Builder->CreateLoad(
                  structTy, stored,
                  buildArcOpLabel(LHSE->getName(), "smart.assign.load"));
            }
            if (stored->getType() != structTy) {
              reportCompilerError(
                  "Initializer for '" + LHSE->getName() +
                  "' has incompatible smart pointer representation");
              return nullptr;
            }

            Builder->CreateStore(stored, GV);
            updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
            setTypeName("void");
            return llvm::UndefValue::get(
                llvm::Type::getVoidTy(*TheContext));
          }

          // Get type name for proper range checking
          std::string targetTypeName = info && !info->typeName.empty() ? info->typeName : LHSE->getTypeName();
          if (diagnoseDisallowedImplicitIntegerConversion(getRHS(), rhsValue, VarType, targetTypeName, "assignment to '" + LHSE->getName() + "'"))
            return nullptr;
          if (info && !info->typeName.empty()) {
            rhsValue = castToType(rhsValue, VarType, info->typeName);
          } else {
            rhsValue = castToType(rhsValue, VarType);
          }
          if (info && info->requiresARC() && !info->isSmartPointer())
            emitManagedStore(GV, rhsValue, *info, LHSE->getName(), rhsIsTemporary);
          else
            Builder->CreateStore(rhsValue, GV);
          updateKnownNonNullOnAssignment(LHSE->getName(), rhsIsNullable);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        }

        const std::string &memberName = LHSE->getName();
        if (resolveInstanceMemberOwnerInCurrentContext(memberName)) {
          auto object = std::make_unique<ThisExprAST>();
          MemberAccessExprAST synthetic(std::move(object), memberName);
          return emitMemberAssignment(synthetic);
        }
        if (auto owner = resolveStaticFieldOwnerInCurrentContext(memberName)) {
          auto object = std::make_unique<VariableExprAST>(*owner);
          MemberAccessExprAST synthetic(std::move(object), memberName);
          return emitMemberAssignment(synthetic);
        }
        return LogErrorV(("Unknown variable name: " + LHSE->getName()).c_str());
      } else if (ArrayIndexExprAST *LHSAI = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
        // Array or tuple element assignment
        if (auto arrayInfoOpt = resolveExprTypeInfo(LHSAI->getArray())) {
          if (const CompositeTypeInfo *ownerInfo =
                  resolveCompositeTypeInfo(*arrayInfoOpt)) {
            if (lookupOperatorMember(*ownerInfo, OverloadableOperator::Index)) {
              llvm::Value *elemPtr = LHSAI->codegen_ptr();
              if (!elemPtr)
                return nullptr;

              auto elemInfoOpt = resolveExprTypeInfo(LHSAI);
              if (!elemInfoOpt) {
                reportCompilerError(
                    "Unable to determine indexed assignment target type");
                return nullptr;
              }
              TypeInfo elementInfo = *elemInfoOpt;
              finalizeTypeInfoMetadata(elementInfo);

              if (rhsIsNullable && !typeAllowsNull(elementInfo))
                return LogErrorV(
                    "Cannot assign nullable value to non-nullable indexed value");

              llvm::Value *rhsValue = emitAssignmentRHS(&elementInfo);
              if (!rhsValue)
                return nullptr;

              const std::string elementTypeName = typeNameFromInfo(elementInfo);
              llvm::Type *elemType = getTypeFromString(elementTypeName);
              if (!elemType)
                return LogErrorV("Invalid indexed assignment target type");
              if (diagnoseDisallowedImplicitIntegerConversion(
                      getRHS(), rhsValue, elemType, elementTypeName,
                      "assignment to indexed value"))
                return nullptr;
              rhsValue = castToType(rhsValue, elemType, elementTypeName);

              bool rhsIsTemporary = getRHS() && getRHS()->isTemporary();
              if (elementInfo.requiresARC() && !elementInfo.isSmartPointer()) {
                if (rhsIsTemporary) {
                  llvm::Value *currentVal = Builder->CreateLoad(
                      elemType, elemPtr,
                      buildArcOpLabel("index.op", "temp.load.old"));
                  emitArcRelease(currentVal, elementInfo,
                                 buildArcOpLabel("index.op",
                                                 "temp.release.old"));
                  Builder->CreateStore(rhsValue, elemPtr);
                } else {
                  emitManagedStore(elemPtr, rhsValue, elementInfo, "index.op");
                }
              } else {
                Builder->CreateStore(rhsValue, elemPtr);
              }

              setTypeName("void");
              return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
            }
          }
        }

        std::optional<ArrayElementAccessInfo> accessOpt;
        if (auto tupleInfoOpt = resolveTupleTypeInfo(LHSAI->getArray());
            tupleInfoOpt && tupleInfoOpt->isTupleType()) {
          if (LHSAI->getIndexCount() != 1) {
            return LogErrorV("Tuple indexing requires a single index");
          }

          ExprAST *indexExpr = LHSAI->getIndex(0);
          auto *literalIndex = dynamic_cast<NumberExprAST *>(indexExpr);
          if (!literalIndex || !literalIndex->isIntegerLiteral())
            return LogErrorV("Tuple index must be an integer literal");

          int64_t indexValue = literalIndex->getLiteral().getSignedValue();
          if (indexValue < 0)
            return LogErrorV("Tuple index cannot be negative");
          if (static_cast<size_t>(indexValue) >=
              tupleInfoOpt->typeArguments.size())
            return LogErrorV("Tuple index out of range");

          accessOpt = computeTupleElementAccess(
              *tupleInfoOpt, LHSAI->getArray()->codegen(),
              static_cast<size_t>(indexValue), "tuple.index.assign");
        } else {
          accessOpt = computeArrayElementAccess(LHSAI);
        }

        if (!accessOpt)
          return nullptr;
        auto &access = *accessOpt;

        llvm::Value *ElemPtr = access.elementPtr;
        llvm::Type *ElemType = access.elementLLVMType;

        if (!ElemPtr || !ElemType || !ElemPtr->getType()->isPointerTy())
          return LogErrorV("Invalid array element pointer");

        if (access.elementTypeInfo.isArray) {
          if (auto *newExpr = extractNewArrayExpr(getRHS())) {
            if (!emitArrayResizeAssignment(ElemPtr, access.elementTypeInfo,
                                           *newExpr, "array.element",
                                           nullptr))
              return nullptr;
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }
        }

        auto *arrayElementRHSVar =
            dynamic_cast<VariableExprAST *>(getRHS());

        bool rhsIsTemporary = getRHS() && getRHS()->isTemporary();

        if (rhsIsNullable && !access.elementNullable) {
          return LogErrorV("Cannot assign nullable value to non-nullable array element");
        }

        llvm::Value *rhsValue = emitAssignmentRHS(&access.elementTypeInfo);
        if (!rhsValue)
          return nullptr;
        if (!validateTupleAssignmentCompatibility(access.elementTypeInfo, getRHS(),
                                                   "assignment to array element"))
          return nullptr;
        rhsIsTemporary =
            rhsIsTemporary || (getRHS() && getRHS()->isTemporary());

        if (access.elementTypeInfo.isSmartPointer()) {
          const std::string constructedName =
              stripNullableAnnotations(
                  typeNameFromInfo(access.elementTypeInfo));
          auto structIt = StructTypes.find(constructedName);
          llvm::StructType *structTy =
              structIt != StructTypes.end() ? structIt->second : nullptr;
          const CompositeTypeInfo *metadata =
              resolveSmartPointerMetadata(access.elementTypeInfo);
          if (!structTy || !metadata) {
            reportCompilerError(
                "Initializer for array element has incompatible smart pointer representation");
            return nullptr;
          }

          if (arrayElementRHSVar) {
            if (emitSmartPointerInitFromVariable(access.elementTypeInfo, ElemPtr,
                                                 *arrayElementRHSVar, "array")) {
              setTypeName("void");
              return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
            }
          }

          if (!metadata->smartPointerDestroyHelper.empty()) {
            llvm::Function *destroyFn = TheModule->getFunction(
                metadata->smartPointerDestroyHelper);
            if (!destroyFn) {
              reportCompilerError(
                  "Internal error: missing smart pointer destroy helper '" +
                  metadata->smartPointerDestroyHelper + "'");
              return nullptr;
            }
            llvm::Value *destroyArg = ElemPtr;
            llvm::Type *expectedTy =
                destroyFn->getFunctionType()->getParamType(0);
            if (expectedTy && destroyArg->getType() != expectedTy) {
              destroyArg = Builder->CreateBitCast(
                  destroyArg, expectedTy,
                  buildArcOpLabel("array", "smart.destroy.cast"));
            }
            Builder->CreateCall(destroyFn, {destroyArg});
          }

          llvm::Value *stored = rhsValue;
          if (stored->getType()->isPointerTy()) {
            stored = Builder->CreateLoad(
                structTy, stored,
                buildArcOpLabel("array", "smart.assign.load"));
          }
          if (stored->getType() != structTy) {
            reportCompilerError(
                "Initializer for array element has incompatible smart pointer representation");
            return nullptr;
          }

          Builder->CreateStore(stored, ElemPtr);
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        } else if (access.elementTypeInfo.requiresARC()) {
          if (rhsIsTemporary) {
            llvm::Value *currentVal = Builder->CreateLoad(
                ElemType, ElemPtr, buildArcOpLabel("array", "temp.load.old"));
            emitArcRelease(currentVal, access.elementTypeInfo,
                           buildArcOpLabel("array", "temp.release.old"));
            Builder->CreateStore(rhsValue, ElemPtr);
          } else {
            emitManagedStore(ElemPtr, rhsValue, access.elementTypeInfo, "array");
          }
        } else {
          Builder->CreateStore(rhsValue, ElemPtr);
        }
        // Return a void value to indicate this is a statement, not an expression
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      } else if (MemberAccessExprAST *LHSMA = dynamic_cast<MemberAccessExprAST*>(getLHS())) {
        return emitMemberAssignment(*LHSMA);
      } else if (UnaryExprAST *LHSU = dynamic_cast<UnaryExprAST*>(getLHS())) {
        // Check if it's a dereference operator (pointer assignment)
        if (LHSU->getOp() == "@") {
          // Get the pointer from the dereference operation
          llvm::Value *Ptr = LHSU->codegen_ptr();
          if (!Ptr)
            return nullptr;

          // Ensure RHS is generated and cast to the pointee type if needed
          llvm::Value *rhsVal = R;
          if (!rhsVal) {
            rhsVal = emitAssignmentRHS(nullptr);
            if (!rhsVal)
              return nullptr;
          }

          // Store the value to the pointer location
          Builder->CreateStore(rhsVal, Ptr);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        } else {
          return LogErrorV("destination of '=' must be a variable, array element, struct member, or dereferenced pointer");
        }
      } else {
        return LogErrorV("destination of '=' must be a variable, array element, struct member, or dereferenced pointer");
      }
    }
  }
      }
  
  // Handle multi-character operators
  if (Op == "==") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isDecimal)
      Cmp = emitDecimalComparisonI1("==", L, R);
    else if (isFloat)
      Cmp = Builder->CreateFCmpOEQ(L, R, "eqtmp");
    else
      Cmp = Builder->CreateICmpEQ(L, R, "eqtmp");
    if (!Cmp)
      return nullptr;
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "!=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isDecimal)
      Cmp = emitDecimalComparisonI1("!=", L, R);
    else if (isFloat)
      Cmp = Builder->CreateFCmpONE(L, R, "netmp");
    else
      Cmp = Builder->CreateICmpNE(L, R, "netmp");
    if (!Cmp)
      return nullptr;
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "<=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isDecimal)
      Cmp = emitDecimalComparisonI1("<=", L, R);
    else if (isFloat)
      Cmp = Builder->CreateFCmpOLE(L, R, "letmp");
    else if (preferUnsigned)
      Cmp = Builder->CreateICmpULE(L, R, "letmp");
    else
      Cmp = Builder->CreateICmpSLE(L, R, "letmp");
    if (!Cmp)
      return nullptr;
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == ">=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isDecimal)
      Cmp = emitDecimalComparisonI1(">=", L, R);
    else if (isFloat)
      Cmp = Builder->CreateFCmpOGE(L, R, "getmp");
    else if (preferUnsigned)
      Cmp = Builder->CreateICmpUGE(L, R, "getmp");
    else
      Cmp = Builder->CreateICmpSGE(L, R, "getmp");
    if (!Cmp)
      return nullptr;
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "+=" || Op == "-=" || Op == "*=" || Op == "/=" || Op == "%=") {
    // Compound assignment operators: a += b is equivalent to a = a + b
    if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
      // Simple variable compound assignment
      const std::string &varName = LHSE->getName();
      llvm::Value *Variable = NamedValues[varName];
      bool isLocal = true;
      if (!Variable) {
        Variable = GlobalValues[varName];
        isLocal = false;
        if (!Variable)
          return LogErrorV("Unknown variable name for compound assignment");
      }

      const TypeInfo *info = lookupTypeInfo(varName);
      std::string lhsPromoteType;
      if (info)
        lhsPromoteType = typeNameFromInfo(*info);
      else
        lhsPromoteType = LHSE->getTypeName();

      bool isAlias = info && info->isAlias();
      llvm::Value *StoragePtr = nullptr;   // Points to the actual value for alias variables
      llvm::Type *ValueType = nullptr;
      llvm::Value *CurrentVal = nullptr;
      llvm::AllocaInst *Alloca =
          isLocal ? llvm::dyn_cast<llvm::AllocaInst>(Variable) : nullptr;
      llvm::GlobalVariable *GV =
          isLocal ? nullptr : llvm::dyn_cast<llvm::GlobalVariable>(Variable);

      if (isAlias) {
        ValueType = info ? getTypeFromString(info->typeName) : nullptr;
        if (!ValueType)
          return LogErrorV("Invalid type for ref variable in compound assignment");
        llvm::Type *slotType =
            Alloca ? Alloca->getAllocatedType()
                   : GV   ? GV->getValueType()
                          : nullptr;
        const bool hasPointerSlot = slotType && slotType->isPointerTy();
        if (hasPointerSlot) {
          StoragePtr = Builder->CreateLoad(slotType, Variable,
                                           (varName + "_ptr").c_str());
        } else if (info) {
          StoragePtr = materializeAliasPointer(Variable, *info, varName);
        }
        if (!StoragePtr || !StoragePtr->getType()->isPointerTy())
          return LogErrorV("Invalid ref storage for compound assignment");
        CurrentVal = Builder->CreateLoad(ValueType, StoragePtr, varName.c_str());
      } else {
        if (isLocal) {
          if (!Alloca)
            return LogErrorV("Internal error: expected stack storage for compound assignment");
          ValueType = Alloca->getAllocatedType();
          CurrentVal = Builder->CreateLoad(ValueType, Variable, varName.c_str());
        } else {
          if (!GV)
            return LogErrorV("Internal error: expected global storage for compound assignment");
          ValueType = GV->getValueType();
          CurrentVal = Builder->CreateLoad(ValueType, Variable, varName.c_str());
        }
      }

      if (!CurrentVal || !ValueType)
        return LogErrorV("Failed to load value for compound assignment");

      if (CurrentVal->getType()->isPointerTy()) {
        if (Op != "+=" && Op != "-=")
          return LogErrorV("Pointer compound assignment only supports '+=' and '-='");

        std::string pointerTypeName = lhsPromoteType.empty() ? getLHS()->getTypeName() : lhsPromoteType;
        llvm::Value *NewPtr = emitPointerOffset(CurrentVal, R, pointerTypeName,
                                                getRHS()->getTypeName(), Op == "-=", "ptrarith");
        if (!NewPtr)
          return nullptr;

        if (isAlias) {
          Builder->CreateStore(NewPtr, StoragePtr);
        } else {
          Builder->CreateStore(NewPtr, Variable);
        }

        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)

      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      if (isDecimalLLVMType(PromotedCurrent->getType())) {
        Result = emitDecimalArithmeticBinary(baseOp, PromotedCurrent, PromotedR);
        if (!Result)
          return nullptr;
      } else {
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      auto lhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(lhsPromoteType));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(getLHS()->getTypeName())).value_or(false);
      }
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      }
      
      // Cast result back to the original storage type if needed
      llvm::Value *ResultToStore = Result;
      if (ResultToStore->getType() != ValueType) {
        std::string castTargetName = !lhsPromoteType.empty() ? lhsPromoteType : LHSE->getTypeName();
        ResultToStore = castToType(ResultToStore, ValueType, castTargetName);
        if (!ResultToStore)
          return nullptr;
      }

      // Store the result back to the correct destination
      if (isAlias) {
        Builder->CreateStore(ResultToStore, StoragePtr);
      } else {
        Builder->CreateStore(ResultToStore, Variable);
      }
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
    } else if (ArrayIndexExprAST *LHSE = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
      // Array element compound assignment: arr[i] += val
      auto accessOpt = computeArrayElementAccess(LHSE);
      if (!accessOpt)
        return nullptr;
      auto &access = *accessOpt;

      llvm::Value *ElementPtr = access.elementPtr;
      llvm::Type *ElemType = access.elementLLVMType;
      if (!ElementPtr || !ElemType || !ElementPtr->getType()->isPointerTy())
        return LogErrorV("Invalid array element pointer");

      std::string elementTypeNameForPromotion = sanitizeBaseTypeName(access.elementTypeName);
      if (elementTypeNameForPromotion.empty())
        elementTypeNameForPromotion = sanitizeBaseTypeName(LHSE->getTypeName());
      
      // Load current value
      llvm::Value *CurrentVal = Builder->CreateLoad(ElemType, ElementPtr, "arrayload");
      
      if (CurrentVal->getType()->isPointerTy()) {
        if (Op != "+=" && Op != "-=")
          return LogErrorV("Pointer compound assignment only supports '+=' and '-='");

        std::string pointerTypeName = access.elementTypeName.empty() ? LHSE->getTypeName() : access.elementTypeName;
        llvm::Value *NewPtr = emitPointerOffset(CurrentVal, R, pointerTypeName,
                                                getRHS()->getTypeName(), Op == "-=", "ptrarith");
        if (!NewPtr)
          return nullptr;

        Builder->CreateStore(NewPtr, ElementPtr);
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)
      
      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      if (isDecimalLLVMType(PromotedCurrent->getType())) {
        Result = emitDecimalArithmeticBinary(baseOp, PromotedCurrent, PromotedR);
        if (!Result)
          return nullptr;
      } else {
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      auto elementUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(elementTypeNameForPromotion));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = elementUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(LHSE->getArray()->getTypeName())).value_or(false);
      }
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateUDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else if (compoundUnsigned)
            Result = Builder->CreateURem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      }
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
    } else {
      return LogErrorV("destination of compound assignment must be a variable, array element, or public struct/class member");
    }
  } else if (Op == "&&") {
    // Check that both operands are boolean types
    if (leftTypeName != "bool" || rightTypeName != "bool") {
      return LogErrorV("Boolean AND operator '&&' can only be used with bool types");
    }
    // Convert i8 bool operands to i1 for logical operations
    if (L->getType()->isIntegerTy(8)) {
      L = Builder->CreateTrunc(L, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    if (R->getType()->isIntegerTy(8)) {
      R = Builder->CreateTrunc(R, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    setTypeName("bool");
    llvm::Value *Result = Builder->CreateAnd(L, R, "andtmp");
    return Builder->CreateZExt(Result, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "||") {
    // Check that both operands are boolean types
    if (leftTypeName != "bool" || rightTypeName != "bool") {
      return LogErrorV("Boolean OR operator '||' can only be used with bool types");
    }
    // Convert i8 bool operands to i1 for logical operations
    if (L->getType()->isIntegerTy(8)) {
      L = Builder->CreateTrunc(L, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    if (R->getType()->isIntegerTy(8)) {
      R = Builder->CreateTrunc(R, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    setTypeName("bool");
    llvm::Value *Result = Builder->CreateOr(L, R, "ortmp");
    return Builder->CreateZExt(Result, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "&") {
    // Bitwise AND - only works on integers
    if (isFloat || isDecimal)
      return LogErrorV("Bitwise AND requires integer operands");
    return Builder->CreateAnd(L, R, "andtmp");
  } else if (Op == "|") {
    // Bitwise OR - only works on integers
    if (isFloat || isDecimal)
      return LogErrorV("Bitwise OR requires integer operands");
    return Builder->CreateOr(L, R, "ortmp");
  } else if (Op == "^") {
    // Bitwise XOR - only works on integers
    if (isFloat || isDecimal)
      return LogErrorV("Bitwise XOR requires integer operands");
    return Builder->CreateXor(L, R, "xortmp");
  } else if (Op == "<<") {
    // Left shift - only works on integers
    if (isFloat || isDecimal)
      return LogErrorV("Left shift requires integer operands");
    return Builder->CreateShl(L, R, "shltmp");
  } else if (Op == ">>") {
    // Right shift (arithmetic) - only works on integers
    if (isFloat || isDecimal)
      return LogErrorV("Right shift requires integer operands");
    if (preferUnsigned)
      return Builder->CreateLShr(L, R, "lshrtmp");
    return Builder->CreateAShr(L, R, "ashrtmp");
  } else if (Op == "&=" || Op == "|=" || Op == "^=" || Op == "<<=" || Op == ">>=") {
    // Bitwise compound assignment operators
    if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
      // Simple variable compound assignment
      llvm::Value *Variable = NamedValues[LHSE->getName()];
      bool isLocal = true;
      if (!Variable) {
        Variable = GlobalValues[LHSE->getName()];
        isLocal = false;
        if (!Variable)
          return LogErrorV("Unknown variable name for compound assignment");
      }
      const TypeInfo *info = lookupTypeInfo(LHSE->getName());
      const bool isAlias = info && info->isAlias();

      // Load current value
      llvm::AllocaInst *Alloca =
          isLocal ? llvm::dyn_cast<llvm::AllocaInst>(Variable) : nullptr;
      llvm::GlobalVariable *GV =
          isLocal ? nullptr : llvm::dyn_cast<llvm::GlobalVariable>(Variable);
      llvm::Value *CurrentVal = nullptr;
      llvm::Value *StoragePtr = nullptr;
      llvm::Type *ValueType = nullptr;
      if (isAlias) {
        ValueType = info ? getTypeFromString(info->typeName) : nullptr;
        if (!ValueType)
          return LogErrorV("Invalid type for ref variable in compound assignment");
        llvm::Type *slotType =
            Alloca ? Alloca->getAllocatedType()
                   : GV   ? GV->getValueType()
                          : nullptr;
        const bool hasPointerSlot = slotType && slotType->isPointerTy();
        if (hasPointerSlot) {
          StoragePtr = Builder->CreateLoad(slotType, Variable,
                                           (LHSE->getName() + "_ptr").c_str());
        } else if (info) {
          StoragePtr =
              materializeAliasPointer(Variable, *info, LHSE->getName());
        }
        if (!StoragePtr || !StoragePtr->getType()->isPointerTy())
          return LogErrorV("Invalid ref storage for compound assignment");
        CurrentVal =
            Builder->CreateLoad(ValueType, StoragePtr, LHSE->getName());
      } else {
        if (isLocal) {
          if (!Alloca)
            return LogErrorV("Internal error: expected stack storage for compound assignment");
          ValueType = Alloca->getAllocatedType();
          CurrentVal =
              Builder->CreateLoad(ValueType, Variable, LHSE->getName());
        } else {
          if (!GV)
            return LogErrorV("Internal error: expected global storage for compound assignment");
          ValueType = GV->getValueType();
          CurrentVal =
              Builder->CreateLoad(ValueType, Variable, LHSE->getName());
        }
      }

      if (!CurrentVal || !ValueType)
        return LogErrorV("Failed to load value for compound assignment");

      if (!R) {
        R = getRHS()->codegen();
        if (!R)
          return nullptr;
      }
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() ||
          R->getType()->isFloatingPointTy() ||
          isDecimalLLVMType(CurrentVal->getType()) ||
          isDecimalLLVMType(R->getType()))
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      std::string lhsPromoteType;
      if (info)
        lhsPromoteType = typeNameFromInfo(*info);
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      auto lhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(lhsPromoteType));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(getLHS()->getTypeName())).value_or(false);
      }
      
      // Perform the operation based on the compound operator
      llvm::Value *Result;
      if (Op == "&=") {
        Result = Builder->CreateAnd(PromotedCurrent, PromotedR, "andtmp");
      } else if (Op == "|=") {
        Result = Builder->CreateOr(PromotedCurrent, PromotedR, "ortmp");
      } else if (Op == "^=") {
        Result = Builder->CreateXor(PromotedCurrent, PromotedR, "xortmp");
      } else if (Op == "<<=") {
        Result = Builder->CreateShl(PromotedCurrent, PromotedR, "shltmp");
      } else if (Op == ">>=") {
        if (compoundUnsigned)
          Result = Builder->CreateLShr(PromotedCurrent, PromotedR, "lshrtmp");
        else
          Result = Builder->CreateAShr(PromotedCurrent, PromotedR, "ashrtmp");
      } else {
        return LogErrorV("Unknown bitwise compound assignment operator");
      }
      
      // Store the result back
      if (isAlias) {
        Builder->CreateStore(Result, StoragePtr);
      } else {
        Builder->CreateStore(Result, Variable);
      }
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
    } else if (ArrayIndexExprAST *LHSE = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
      // Array element compound assignment: arr[i] &= val
      auto accessOpt = computeArrayElementAccess(LHSE);
      if (!accessOpt)
        return nullptr;
      auto &access = *accessOpt;

      llvm::Value *ElementPtr = access.elementPtr;
      llvm::Type *ElemType = access.elementLLVMType;
      if (!ElementPtr || !ElemType || !ElementPtr->getType()->isPointerTy())
        return LogErrorV("Invalid array element pointer");

      std::string elementTypeNameForPromotion = sanitizeBaseTypeName(access.elementTypeName);
      if (elementTypeNameForPromotion.empty())
        elementTypeNameForPromotion = sanitizeBaseTypeName(LHSE->getTypeName());
      
      // Load current value
      llvm::Value *CurrentVal = Builder->CreateLoad(ElemType, ElementPtr, "arrayload");

      if (!R) {
        R = getRHS()->codegen();
        if (!R)
          return nullptr;
      }
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() ||
          R->getType()->isFloatingPointTy() ||
          isDecimalLLVMType(CurrentVal->getType()) ||
          isDecimalLLVMType(R->getType()))
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
      if (!PromotedCurrent || !PromotedR)
        return nullptr;
      auto elementUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(elementTypeNameForPromotion));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = elementUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(LHSE->getArray()->getTypeName())).value_or(false);
      }
      
      // Perform the operation
      llvm::Value *Result;
      if (Op == "&=") {
        Result = Builder->CreateAnd(PromotedCurrent, PromotedR, "andtmp");
      } else if (Op == "|=") {
        Result = Builder->CreateOr(PromotedCurrent, PromotedR, "ortmp");
      } else if (Op == "^=") {
        Result = Builder->CreateXor(PromotedCurrent, PromotedR, "xortmp");
      } else if (Op == "<<=") {
        Result = Builder->CreateShl(PromotedCurrent, PromotedR, "shltmp");
      } else if (Op == ">>=") {
        if (compoundUnsigned)
          Result = Builder->CreateLShr(PromotedCurrent, PromotedR, "lshrtmp");
        else
          Result = Builder->CreateAShr(PromotedCurrent, PromotedR, "ashrtmp");
      } else {
        return LogErrorV("Unknown bitwise compound assignment operator");
      }
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
    } else {
      return LogErrorV("destination of bitwise compound assignment must be a variable or array element");
    }
  }
  
  return LogErrorV("Binary operator '" + Op + "' is not supported",
                   "Check for typos or ensure this operator has code generation support.");
}

llvm::Value *TypeCheckExprAST::codegen() {
  BindingValue = nullptr;

  llvm::Value *value = Operand->codegen();
  if (!value)
    return nullptr;

  setTypeName("bool");

  std::optional<TypeInfo> lhsInfoOpt = resolveExprTypeInfo(Operand.get());
  TypeInfo lhsInfo;
  if (lhsInfoOpt) {
    lhsInfo = *lhsInfoOpt;
  } else if (!Operand->getTypeName().empty()) {
    lhsInfo = makeTypeInfo(Operand->getTypeName());
    finalizeTypeInfoMetadata(lhsInfo);
  }

  if (TargetIsNull) {
    llvm::Value *isNull = emitNullCheckValue(value, lhsInfo, "typecheck.null");
    if (!isNull)
      return nullptr;
    llvm::Value *result = IsNegated
                              ? Builder->CreateNot(isNull, "typecheck.not")
                              : isNull;
    return Builder->CreateZExt(result, llvm::Type::getInt8Ty(*TheContext),
                               "booltmp");
  }

  TypeInfo targetInfo = applyActiveTypeBindings(TargetType);
  finalizeTypeInfoMetadata(targetInfo);

  std::string targetTypeName = typeNameFromInfo(targetInfo);
  if (targetTypeName.empty())
    targetTypeName = targetInfo.typeName;

  std::string lhsTypeName = lhsInfo.typeName.empty()
                                ? Operand->getTypeName()
                                : lhsInfo.typeName;
  std::string lhsSanitized = stripNullableAnnotations(lhsTypeName);
  std::string targetSanitized = stripNullableAnnotations(targetTypeName);

  const CompositeTypeInfo *targetMeta = resolveCompositeTypeInfo(targetInfo);
  const CompositeTypeInfo *lhsMeta = resolveCompositeTypeInfo(lhsInfo);
  llvm::Value *match = nullptr;

  const bool lhsSupportsDescriptor =
      lhsMeta && (lhsMeta->hasARCHeader ||
                  lhsMeta->kind == AggregateKind::Interface ||
                  lhsMeta->isInterface);

  if (targetMeta && lhsSupportsDescriptor &&
      value->getType()->isPointerTy()) {
    if (targetMeta->descriptorGlobalName.empty()) {
      reportCompilerError("Internal error: missing descriptor for '" +
                          targetTypeName + "' in type check");
      return nullptr;
    }
    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(targetMeta->descriptorGlobalName, true);
    if (!descriptorGV) {
      reportCompilerError("Internal error: descriptor '" +
                          targetMeta->descriptorGlobalName +
                          "' missing for type check");
      return nullptr;
    }
    llvm::Value *targetDescriptor = llvm::ConstantExpr::getBitCast(
        descriptorGV, pointerType(getTypeDescriptorType()));

    TypeCheckMatchKind kind = TypeCheckMatchKind::Exact;
    if (targetMeta->kind == AggregateKind::Interface || targetMeta->isInterface)
      kind = TypeCheckMatchKind::Interface;
    else if (targetMeta->kind == AggregateKind::Class)
      kind = TypeCheckMatchKind::Chain;

    match = emitTypeDescriptorMatch(value, targetDescriptor, kind, "typecheck");
  } else {
    bool sameType = !lhsSanitized.empty() && !targetSanitized.empty() &&
                    lhsSanitized == targetSanitized;
    if (!sameType) {
      match = llvm::ConstantInt::getFalse(*TheContext);
    } else if (typeAllowsNull(lhsInfo) && value->getType()->isPointerTy()) {
      auto *ptrTy = llvm::cast<llvm::PointerType>(value->getType());
      llvm::Value *notNull = Builder->CreateICmpNE(
          value, llvm::ConstantPointerNull::get(ptrTy), "typecheck.notnull");
      match = notNull;
    } else {
      match = llvm::ConstantInt::getTrue(*TheContext);
    }
  }

  if (BindingName && !targetTypeName.empty()) {
    llvm::Type *bindingLLVMType = getTypeFromString(targetTypeName);
    if (bindingLLVMType) {
      llvm::Value *bindingValue = value;
      if (bindingValue->getType() != bindingLLVMType)
        bindingValue = castToType(bindingValue, bindingLLVMType,
                                  targetTypeName);
      BindingValue = bindingValue;
    }
  }

  if (!match)
    return nullptr;

  if (IsNegated)
    match = Builder->CreateNot(match, "typecheck.not");
  return Builder->CreateZExt(match, llvm::Type::getInt8Ty(*TheContext),
                             "booltmp");
}

// Generate code for unary expressions like -, !, ++, --
llvm::Value *UnaryExprAST::codegen() {
  const bool parsedUnsafe = wasParsedInUnsafe();
  if (Op == "++" || Op == "--") {
    llvm::Value *Ptr = Operand->codegen_ptr();
    if (!Ptr) {
        return LogErrorV("operand of ++/-- must be a variable");
    }

    if (auto *Member = dynamic_cast<MemberAccessExprAST*>(Operand.get())) {
      if (!ensureMemberInitializedForMutation(*Member))
        return nullptr;
    }

    llvm::Value *Val = Operand->codegen();
    llvm::Type *Ty = Val->getType();

    llvm::Value *CurVal = Builder->CreateLoad(Ty, Ptr, "loadtmp");

    if (Ty->isPointerTy()) {
        ParsedTypeDescriptor ptrDesc = parseTypeString(Operand->getTypeName());
        if (!isPointerTypeDescriptor(ptrDesc))
            return LogErrorV("Pointer arithmetic requires pointer operands");

        auto elementNameOpt = getPointerElementTypeName(ptrDesc.sanitized);
        if (!elementNameOpt)
            return LogErrorV("Cannot determine element type for pointer arithmetic");

        llvm::Type *ElementType = getTypeFromString(*elementNameOpt);
        if (!ElementType)
            return LogErrorV("Unsupported element type for pointer arithmetic");

        llvm::IntegerType *IndexType = getPointerIndexType();
        llvm::Value *Step = (Op == "++")
                                ? static_cast<llvm::Value *>(llvm::ConstantInt::get(IndexType, 1))
                                : static_cast<llvm::Value *>(llvm::ConstantInt::getSigned(IndexType, -1));

        llvm::Value *NextVal = Builder->CreateInBoundsGEP(
            ElementType, CurVal, Step, Op == "++" ? "ptrinc" : "ptrdec");
        Builder->CreateStore(NextVal, Ptr);

        setTypeName(ptrDesc.sanitized);
        return isPrefix ? NextVal : CurVal;
    }

    llvm::Value *One = nullptr;
    if (Ty->isIntegerTy()) {
        One = llvm::ConstantInt::get(Ty, 1);
    } else if (Ty->isFloatingPointTy()) {
        One = llvm::ConstantFP::get(Ty, 1.0);
    } else if (isDecimalLLVMType(Ty)) {
        One = Builder->CreateCall(
            getDecimalFromI64Function(),
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(*TheContext), 1)},
            "dec.one");
    } else {
        return LogErrorV("++/-- requires integer, floating-point, or decimal type");
    }

    llvm::Value *NextVal;

    if (Op == "++") {
        if (isDecimalLLVMType(Ty))
            NextVal = Builder->CreateCall(getDecimalAddFunction(),
                                          {CurVal, One}, "dec.inc");
        else if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFAdd(CurVal, One, "addtmp");
        else
            NextVal = Builder->CreateAdd(CurVal, One, "addtmp");
    } else { // --
        if (isDecimalLLVMType(Ty))
            NextVal = Builder->CreateCall(getDecimalSubFunction(),
                                          {CurVal, One}, "dec.dec");
        else if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFSub(CurVal, One, "subtmp");
        else
            NextVal = Builder->CreateSub(CurVal, One, "subtmp");
    }

    Builder->CreateStore(NextVal, Ptr);

    // For prefix operators, return the new value; for postfix, return the old value
    return isPrefix ? NextVal : CurVal;
  }

  if (Op == "@" || Op == "#") {
    auto opKindOpt = overloadableOperatorFromSymbol(Op);
    if (opKindOpt) {
      if (auto operandTypeInfo = resolveExprTypeInfo(Operand.get())) {
        if (const CompositeTypeInfo *ownerInfo =
                resolveCompositeTypeInfo(*operandTypeInfo)) {
          if (const CompositeMemberInfo *operatorInfo =
                  lookupOperatorMember(*ownerInfo, *opKindOpt)) {
            if (overloadableOperatorRequiresUnsafe(*opKindOpt) && !parsedUnsafe) {
              return LogErrorV("Operator '" + Op + "' requires unsafe context");
            }

            std::string ownerName =
                stripNullableAnnotations(typeNameFromInfo(*operandTypeInfo));
            if (ownerName.empty())
              ownerName = "<composite>";

            if (!ensureMemberAccessAllowed(operatorInfo->modifiers,
                                           AccessIntent::Call, ownerName, Op))
              return nullptr;

            llvm::Value *operandValue = Operand->codegen();
            if (!operandValue)
              return nullptr;

            return emitMemberCallByInfo(*ownerInfo, *operatorInfo, ownerName,
                                        Operand.get(), operandValue, {}, {},
                                        this);
          }
        }
      }
    }
  }

  // Handle other unary operators
  llvm::Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  if (Op == "-") {
    if (isDecimalLLVMType(OperandV->getType()))
      return Builder->CreateCall(getDecimalNegFunction(), {OperandV},
                                 "dec.neg");
    if (OperandV->getType()->isFloatingPointTy())
      return Builder->CreateFNeg(OperandV, "fnegtmp");
    return Builder->CreateNeg(OperandV, "negtmp");
  } else if (Op == "!") {
    // Check that operand is boolean type
    std::string operandTypeName = Operand->getTypeName();
    if (operandTypeName != "bool") {
      return LogErrorV("Boolean NOT operator '!' can only be used with bool type");
    }
    setTypeName("bool");
    // For bool type (i8), truncate to i1, apply NOT, then zero-extend back to i8
    if (OperandV->getType()->isIntegerTy(8)) {
      llvm::Value *Truncated = Builder->CreateTrunc(OperandV, llvm::Type::getInt1Ty(*TheContext), "tobool");
      llvm::Value *Negated = Builder->CreateNot(Truncated, "nottmp");
      return Builder->CreateZExt(Negated, llvm::Type::getInt8Ty(*TheContext), "booltmp");
    }
    return Builder->CreateNot(OperandV, "nottmp");
  }

  auto loadSmartPointerPayload =
      [&](TypeInfo &smartInfo,
          std::string_view label) -> std::optional<std::pair<llvm::Value *, TypeInfo>> {
        finalizeTypeInfoMetadata(smartInfo);
        const CompositeTypeInfo *smartMeta =
            resolveSmartPointerMetadata(smartInfo);
        if (!smartMeta)
          return std::nullopt;

        const InstanceFieldInfo *payloadField =
            findInstanceField(*smartMeta,
                              smartMeta->smartPointerKind == SmartPointerKind::Unique
                                  ? "value"
                                  : "payload");
        if (!payloadField)
          return std::nullopt;

        std::string smartKey =
            stripNullableAnnotations(typeNameFromInfo(smartInfo));
        auto structIt = StructTypes.find(smartKey);
        if (structIt == StructTypes.end() || !structIt->second)
          return std::nullopt;
        llvm::StructType *smartTy = structIt->second;

        llvm::Value *payloadVal = nullptr;
        if (OperandV->getType()->isPointerTy()) {
          llvm::Value *payloadPtr = Builder->CreateStructGEP(
              smartTy, OperandV, payloadField->index,
              std::string(label) + ".ptr");
          llvm::Type *payloadTy =
              getTypeFromString(typeNameFromInfo(payloadField->type));
          if (!payloadTy)
            return std::nullopt;
          payloadVal = Builder->CreateLoad(payloadTy, payloadPtr,
                                           std::string(label));
        } else {
          payloadVal = Builder->CreateExtractValue(
              OperandV, payloadField->index, std::string(label));
        }

        TypeInfo payloadInfo = payloadField->type;
        finalizeTypeInfoMetadata(payloadInfo);
        return std::make_pair(payloadVal, payloadInfo);
      };

  // Handle pointer operators
  if (Op == "#") {
    if (!parsedUnsafe)
      return LogErrorV("Address-of operator '#' requires unsafe context");

    // Address-of operator
    llvm::Value *Ptr = Operand->codegen_ptr();
    if (!Ptr)
      return LogErrorV("Cannot take address of non-lvalue");

    // Set type name to pointer type
    std::string baseType = Operand->getTypeName();
    setTypeName(baseType + "@");

    std::string lookupName =
        stripNullableAnnotations(sanitizeBaseTypeName(baseType));
    if (const CompositeTypeInfo *comp = lookupCompositeInfo(lookupName)) {
      (void)comp;
      llvm::Type *valueTy = getTypeFromString(baseType);
      if (valueTy)
        Ptr = Builder->CreateLoad(valueTy, Ptr,
                                  "addr.comp.load");
    }

    return Ptr;
  } else if (Op == "@") {
    TypeInfo operandInfo = makeTypeInfo(Operand->getTypeName());
    finalizeTypeInfoMetadata(operandInfo);
    if (!operandInfo.isSmartPointer()) {
      if (auto *var = dynamic_cast<VariableExprAST *>(Operand.get())) {
        if (const TypeInfo *symInfo = lookupTypeInfo(var->getName())) {
          operandInfo = applyActiveTypeBindings(*symInfo);
          finalizeTypeInfoMetadata(operandInfo);
        }
      }
      if (!operandInfo.isSmartPointer()) {
        std::string typeNameGuess = typeNameFromInfo(operandInfo);
        std::string base = baseCompositeName(typeNameGuess);
        SmartPointerKind detected = detectSmartPointerKind(base);
        if (detected != SmartPointerKind::None) {
          operandInfo.smartPointerKind = detected;
        }
      }
    }

    auto smartInfoOpt = operandInfo.isSmartPointer()
                            ? std::optional<TypeInfo>(operandInfo)
                            : std::nullopt;
    if (!smartInfoOpt) {
      std::string spelled = Operand->getTypeName();
      if (spelled.empty())
        spelled = typeNameFromInfo(operandInfo);
      if (!spelled.empty()) {
        TypeInfo guessed = makeTypeInfo(spelled);
        finalizeTypeInfoMetadata(guessed);
        if (guessed.isSmartPointer())
          smartInfoOpt = std::move(guessed);
      }
    }
    if (!smartInfoOpt) {
      llvm::Type *valTy = OperandV->getType();
      llvm::StructType *structTy = nullptr;
      if (valTy->isStructTy())
        structTy = llvm::cast<llvm::StructType>(valTy);
      if (structTy) {
        for (const auto &entry : StructTypes) {
          if (entry.second != structTy)
            continue;
          TypeInfo candidate = makeTypeInfo(entry.first);
          finalizeTypeInfoMetadata(candidate);
          if (candidate.isSmartPointer()) {
            Operand->setTypeName(typeNameFromInfo(candidate));
            smartInfoOpt = candidate;
            break;
          }
        }
      }
    }

    if (smartInfoOpt) {
      operandInfo = *smartInfoOpt;
      auto payload =
          loadSmartPointerPayload(operandInfo, "smart.deref.payload");
      if (!payload)
        return LogErrorV("Unable to unwrap smart pointer payload for '@'");

      setTypeName(typeNameFromInfo(payload->second));
      return payload->first;
    }

    if (!parsedUnsafe)
      return LogErrorV(
          "Dereference operator '@' requires unsafe context for raw pointers");

    // Dereference operator for raw pointers
    if (!OperandV->getType()->isPointerTy())
      return LogErrorV("Cannot dereference non-pointer type");

    // For opaque pointers in LLVM 15+, need to determine the element type
    // from the operand's type string
    std::string operandType = Operand->getTypeName();
    if (operandType.empty() || operandType.find('@') == std::string::npos)
      return LogErrorV("Invalid pointer type for dereference");

    // Remove the @ and any level suffix to get the base type
    size_t atPos = operandType.find('@');
    std::string baseType = operandType.substr(0, atPos);

    int level = 1;
    if (atPos + 1 < operandType.size()) {
      std::string levelStr = operandType.substr(atPos + 1);
      if (!levelStr.empty())
        level = std::stoi(levelStr);
    }

    if (level > 1) {
      if (level == 2) {
        setTypeName(baseType + "@");
      } else {
        setTypeName(baseType + "@" + std::to_string(level - 1));
      }
    } else {
      setTypeName(baseType);
    }

    bool treatAsCompositeReference = false;
    if (level <= 1) {
      std::string lookupName =
          stripNullableAnnotations(sanitizeBaseTypeName(baseType));
      if (!lookupName.empty()) {
        if (const CompositeTypeInfo *info =
                lookupCompositeInfo(lookupName)) {
          (void)info;
          treatAsCompositeReference = true;
        }
      }
    }

    if (treatAsCompositeReference)
      return OperandV;

    // Get the element type to load
    llvm::Type *ElementType = getTypeFromString(getTypeName());
    if (!ElementType)
      return LogErrorV("Cannot determine element type for dereference");

    return Builder->CreateLoad(ElementType, OperandV, "deref");
  }

  return LogErrorV("invalid unary operator");
}

// Generate pointer for unary expressions (for address-of operations)
llvm::Value *UnaryExprAST::codegen_ptr() {
  if (Op == "@") {
    if (auto operandTypeInfo = resolveExprTypeInfo(Operand.get())) {
      if (const CompositeTypeInfo *ownerInfo =
              resolveCompositeTypeInfo(*operandTypeInfo)) {
        if (const CompositeMemberInfo *operatorInfo = lookupOperatorMember(
                *ownerInfo, OverloadableOperator::Dereference)) {
          if (!wasParsedInUnsafe())
            return LogErrorV("Operator '@' requires unsafe context");
          if (!operatorInfo->returnsByRef) {
            reportCompilerError("Operator '@' on type '" +
                                stripNullableAnnotations(
                                    typeNameFromInfo(*operandTypeInfo)) +
                                "' must return ref to be used as an lvalue");
            return nullptr;
          }

          std::string ownerName =
              stripNullableAnnotations(typeNameFromInfo(*operandTypeInfo));
          if (ownerName.empty())
            ownerName = "<composite>";
          if (!ensureMemberAccessAllowed(operatorInfo->modifiers,
                                         AccessIntent::Call, ownerName, "@"))
            return nullptr;

          llvm::Value *operandValue = Operand->codegen();
          if (!operandValue)
            return nullptr;

          return emitMemberCallByInfo(*ownerInfo, *operatorInfo, ownerName,
                                      Operand.get(), operandValue, {}, {}, this,
                                      true);
        }
      }
    }

    TypeInfo operandInfo = makeTypeInfo(Operand->getTypeName());
    finalizeTypeInfoMetadata(operandInfo);
    if (!operandInfo.isSmartPointer()) {
      if (auto *var = dynamic_cast<VariableExprAST *>(Operand.get())) {
        if (const TypeInfo *symInfo = lookupTypeInfo(var->getName())) {
          operandInfo = applyActiveTypeBindings(*symInfo);
          finalizeTypeInfoMetadata(operandInfo);
        }
      }
      if (!operandInfo.isSmartPointer()) {
        std::string typeNameGuess = typeNameFromInfo(operandInfo);
        std::string base = baseCompositeName(typeNameGuess);
        SmartPointerKind detected = detectSmartPointerKind(base);
        if (detected != SmartPointerKind::None) {
          operandInfo.smartPointerKind = detected;
        }
      }
    }

    llvm::Value *OperandV = Operand->codegen();
    if (!OperandV)
      return nullptr;

    auto smartInfoOpt = operandInfo.isSmartPointer()
                            ? std::optional<TypeInfo>(operandInfo)
                            : std::nullopt;
    if (!smartInfoOpt) {
      std::string spelled = Operand->getTypeName();
      if (spelled.empty())
        spelled = typeNameFromInfo(operandInfo);
      if (!spelled.empty()) {
        TypeInfo guessed = makeTypeInfo(spelled);
        finalizeTypeInfoMetadata(guessed);
        if (guessed.isSmartPointer())
          smartInfoOpt = std::move(guessed);
      }
    }
    if (!smartInfoOpt) {
      llvm::Type *valTy = OperandV->getType();
      llvm::StructType *structTy = nullptr;
      if (valTy->isStructTy())
        structTy = llvm::cast<llvm::StructType>(valTy);
      if (structTy) {
        for (const auto &entry : StructTypes) {
          if (entry.second != structTy)
            continue;
          TypeInfo candidate = makeTypeInfo(entry.first);
          finalizeTypeInfoMetadata(candidate);
          if (candidate.isSmartPointer()) {
            Operand->setTypeName(typeNameFromInfo(candidate));
            smartInfoOpt = candidate;
            break;
          }
        }
      }
    }

    // Dereference operator returns the pointer itself
    if (smartInfoOpt) {
      operandInfo = *smartInfoOpt;
      const CompositeTypeInfo *smartMeta =
          resolveSmartPointerMetadata(operandInfo);
      if (!smartMeta)
        return LogErrorV("Unable to unwrap smart pointer payload for '@'");
      const InstanceFieldInfo *payloadField =
          findInstanceField(*smartMeta, smartMeta->smartPointerKind == SmartPointerKind::Unique
                                           ? "value"
                                           : "payload");
      if (!payloadField)
        return LogErrorV("Internal error: missing smart pointer payload");

      std::string smartKey =
          stripNullableAnnotations(typeNameFromInfo(operandInfo));
      auto structIt = StructTypes.find(smartKey);
      if (structIt == StructTypes.end() || !structIt->second)
        return LogErrorV("Internal error: missing smart pointer storage");
      llvm::StructType *smartTy = structIt->second;

      std::string payloadTypeName = typeNameFromInfo(payloadField->type);
      llvm::Type *payloadTy = getTypeFromString(payloadTypeName);
      setTypeName(payloadTypeName);

      auto makeContainerPtr = [&](llvm::Value *value) -> llvm::Value * {
        if (value->getType()->isPointerTy())
          return value;
        llvm::AllocaInst *tmp =
            Builder->CreateAlloca(smartTy, nullptr, "smart.deref.stack");
        Builder->CreateStore(value, tmp);
        return tmp;
      };

      llvm::Value *containerPtr = makeContainerPtr(OperandV);
      llvm::Value *payloadPtr = Builder->CreateStructGEP(
          smartTy, containerPtr, payloadField->index, "smart.deref.ptr");
      if (payloadTy && payloadTy->isPointerTy()) {
        return Builder->CreateLoad(payloadTy, payloadPtr,
                                   "smart.deref.payload.ptr");
      }
      return payloadPtr;
    }

    if (!wasParsedInUnsafe())
      return LogErrorV(
          "Dereference operator '@' requires unsafe context for raw pointers");

    if (!OperandV->getType()->isPointerTy())
      return LogErrorV("Cannot dereference non-pointer type");

    return OperandV;
  }

  // Other unary operators don't have an lvalue
  return nullptr;
}

// Generate code for type casting expressions
llvm::Value *CastExprAST::codegen() {
  // Get the operand value
  llvm::Value *OperandV = getOperand()->codegen();
  if (!OperandV)
    return nullptr;

  // Get the source type name from the operand
  std::string sourceTypeName = getOperand()->getTypeName();
  std::string cleanSource = sanitizeBaseTypeName(sourceTypeName);
  std::string cleanTarget = sanitizeBaseTypeName(getTargetType());
  if (cleanTarget.empty())
    cleanTarget = getTargetType();

  // Get the target LLVM type
  llvm::Type *TargetLLVMType = getTypeFromString(getTargetType());
  if (!TargetLLVMType)
    return LogErrorV("Invalid target type for cast");

  auto describeTypeName = [&](const std::string &name,
                              llvm::Type *type) -> std::string {
    std::string clean = sanitizeBaseTypeName(name);
    if (!clean.empty())
      return clean;
    if (!name.empty())
      return name;
    return describeTypeForDiagnostic(type);
  };

  std::string sourceLabel = describeTypeName(sourceTypeName, OperandV->getType());
  std::string targetLabel = describeTypeName(getTargetType(), TargetLLVMType);

  if (cleanTarget == "void")
    return LogErrorV("Cannot cast to void");

  if (OperandV->getType()->isVoidTy() || cleanSource == "void")
    return LogErrorV("Cannot cast void to " + targetLabel);

  if ((cleanSource == "bool" || cleanTarget == "bool") &&
      cleanSource != cleanTarget) {
    return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
  }

  // Set our type name to the target type
  setTypeName(getTargetType());

  llvm::Type *SourceType = OperandV->getType();
  
  // If same type, just return the value
  if (SourceType == TargetLLVMType)
    return OperandV;

  const bool sourceIsDecimal = isDecimalLLVMType(SourceType);
  const bool targetIsDecimal = isDecimalLLVMType(TargetLLVMType);
  if (sourceIsDecimal || targetIsDecimal) {
    if (sourceIsDecimal && targetIsDecimal)
      return OperandV;

    if (targetIsDecimal) {
      if (SourceType->isIntegerTy() && !SourceType->isIntegerTy(1)) {
        llvm::Value *converted =
            emitIntegerToDecimalValue(OperandV, cleanSource);
        if (!converted)
          return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
        return converted;
      }
      if (SourceType->isFloatingPointTy()) {
        llvm::Value *converted = emitFloatingToDecimalValue(OperandV);
        if (!converted)
          return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
        return converted;
      }
      return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
    }

    if (sourceIsDecimal) {
      if (TargetLLVMType->isIntegerTy() && !TargetLLVMType->isIntegerTy(1)) {
        llvm::Value *converted =
            emitDecimalToIntegerValue(OperandV, TargetLLVMType, cleanTarget);
        if (!converted)
          return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
        return converted;
      }
      if (TargetLLVMType->isFloatingPointTy()) {
        llvm::Value *converted =
            emitDecimalToFloatingValue(OperandV, TargetLLVMType);
        if (!converted)
          return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
        return converted;
      }
      return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
    }
  }

  if (SourceType->isIntegerTy() && TargetLLVMType->isIntegerTy() &&
      !TargetLLVMType->isIntegerTy(1)) {
    if (auto *literal = dynamic_cast<NumberExprAST *>(getOperand())) {
      if (literal->isIntegerLiteral()) {
        const llvm::APInt &literalValue = literal->getLiteral().getIntegerValue();
        if (auto rangeInfo = getIntegerRangeInfo(cleanTarget)) {
          if (!integerLiteralFitsRange(literalValue, *rangeInfo))
            return LogErrorV(buildIntegerRangeError(literalValue, cleanTarget));
        }
      }
    }
  }

  // Integer to integer casts
  if (SourceType->isIntegerTy() && TargetLLVMType->isIntegerTy()) {
    unsigned SourceBits = SourceType->getIntegerBitWidth();
    unsigned TargetBits = TargetLLVMType->getIntegerBitWidth();
    
    if (SourceBits < TargetBits) {
      // Use sign or zero extension based on source type
      if (!sourceTypeName.empty() && isUnsignedType(sourceTypeName)) {
        return Builder->CreateZExt(OperandV, TargetLLVMType, "casttmp");
      } else {
        return Builder->CreateSExt(OperandV, TargetLLVMType, "casttmp");
      }
    } else if (SourceBits > TargetBits) {
      return Builder->CreateTrunc(OperandV, TargetLLVMType, "casttmp");
    }
    return OperandV;
  }
  
  // Integer to float
  if (SourceType->isIntegerTy() && TargetLLVMType->isFloatingPointTy()) {
    // Use the source type name to use either signed or unsigned conversion
    if (!sourceTypeName.empty() && isUnsignedType(sourceTypeName)) {
      return Builder->CreateUIToFP(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateSIToFP(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  // Float to integer
  if (SourceType->isFloatingPointTy() && TargetLLVMType->isIntegerTy()) {
    // Use signed or unsigned conversion based on target type
    if (isUnsignedType(getTargetType())) {
      return Builder->CreateFPToUI(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateFPToSI(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  // Float to float (different precision)
  if (SourceType->isFloatingPointTy() && TargetLLVMType->isFloatingPointTy()) {
    if (SourceType->getPrimitiveSizeInBits() < TargetLLVMType->getPrimitiveSizeInBits()) {
      return Builder->CreateFPExt(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateFPTrunc(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  return LogErrorV("Cannot cast " + sourceLabel + " to " + targetLabel);
}

// Generate code for ref expressions (returns pointer to variable)
llvm::Value *RefExprAST::codegen() {
  // For ref expressions, return the pointer to the operand
  // Use codegen_ptr if available, otherwise get the address
  if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getOperand())) {
    return VarExpr->codegen_ptr();
  }
  else if (auto *ArrayIdxExpr = dynamic_cast<ArrayIndexExprAST*>(getOperand())) {
    return ArrayIdxExpr->codegen_ptr();
  }
  else if (auto *MemberExpr = dynamic_cast<MemberAccessExprAST*>(getOperand())) {
    return MemberExpr->codegen_ptr();
  }
  else if (auto *ThisExpr = dynamic_cast<ThisExprAST*>(getOperand())) {
    return ThisExpr->codegen_ptr();
  }
  else if (auto *UnaryExpr = dynamic_cast<UnaryExprAST*>(getOperand())) {
    return UnaryExpr->codegen_ptr();
  }
  else {
    return LogErrorV("ref can only be used with lvalues (variables, array elements, or struct members)");
  }
}

llvm::Value *RetainExprAST::codegen() {
  llvm::Value *value = Operand->codegen();
  if (!value)
    return nullptr;

  if (!value->getType()->isPointerTy()) {
    return LogErrorV("retain expressions require pointer-compatible operands");
  }

  auto *opaquePtrTy = pointerType();
  llvm::Value *castValue =
      Builder->CreateBitCast(value, opaquePtrTy, "arc.retain.cast");
  llvm::Value *retained =
      Builder->CreateCall(getHybridRetainFunction(), {castValue},
                          "arc.retain.call");
  llvm::Value *result =
      Builder->CreateBitCast(retained, value->getType(), "arc.retain.result");
  setTypeName(getOperand()->getTypeName());
  return result;
}

llvm::Value *ReleaseExprAST::codegen() {
  llvm::Value *value = Operand->codegen();
  if (!value)
    return nullptr;

  if (!value->getType()->isPointerTy()) {
    return LogErrorV("release expressions require pointer-compatible operands");
  }

  auto *opaquePtrTy = pointerType();
  llvm::Value *castValue =
      Builder->CreateBitCast(value, opaquePtrTy, "arc.release.cast");
  Builder->CreateCall(getHybridReleaseFunction(), {castValue});
  setTypeName(getOperand()->getTypeName());
  return value;
}

llvm::Value *FreeExprAST::codegen() {
  llvm::Value *value = Operand->codegen();
  if (!value)
    return nullptr;

  TypeInfo info = makeTypeInfo(Operand->getTypeName());
  finalizeTypeInfoMetadata(info);
  if (info.smartPointerKind != SmartPointerKind::None) {
    reportCompilerError(
        "Cannot free smart pointer values directly",
        "The smart pointer's control block owns the payload; use the wrapper operations instead of 'free' to avoid double releases.");
    return nullptr;
  }

  auto resolveArcInfo = [&](const TypeInfo &candidate) -> TypeInfo {
    if (candidate.requiresARC())
      return candidate;
    if ((!candidate.baseTypeName.empty()) &&
        (candidate.pointerDepth > 0 || candidate.isArray)) {
      TypeInfo peeled = makeTypeInfo(candidate.baseTypeName);
      finalizeTypeInfoMetadata(peeled);
      return peeled;
    }
    return candidate;
  };

  TypeInfo arcInfo = resolveArcInfo(info);
  const bool referenceLike = info.pointerDepth > 0 || info.isArray ||
                             info.isReference() || arcInfo.participatesInARC();
  if (!referenceLike) {
    reportCompilerError("The 'free' keyword requires a reference value",
                        "Only heap-managed references may be freed; stack values are released automatically.");
    return nullptr;
  }
  if (!arcInfo.requiresARC()) {
    reportCompilerError("The 'free' keyword is only valid for ARC-managed references");
    return nullptr;
  }
  if (!value->getType()->isPointerTy()) {
    reportCompilerError("The 'free' keyword requires a reference value");
    return nullptr;
  }

  if (auto *allocaPtr =
          llvm::dyn_cast<llvm::AllocaInst>(value->stripPointerCasts())) {
    reportCompilerError(
        "Cannot free stack-allocated value",
        "Only heap allocations produced by 'new' or ARC-managed references may be freed explicitly.");
    (void)allocaPtr;
    return nullptr;
  }

  emitArcRelease(value, arcInfo, "free");
  setTypeName("void");
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *NewExprAST::codegen() {
  std::string targetType = RequestedTypeName.empty() ? getTypeName()
                                                     : RequestedTypeName;
  if (targetType.empty()) {
    reportCompilerError(
        "Cannot infer target type for 'new' expression",
        "Provide an explicit type (e.g. 'new Box()') or assign to a typed target.");
    return nullptr;
  }

  auto materializeDescriptor = [&](const std::string &compositeName)
      -> std::pair<const CompositeTypeInfo *, llvm::Value *> {
    const CompositeTypeInfo *metadata =
        lookupCompositeInfo(compositeName, /*countHit=*/false);
    if (!metadata) {
      TypeInfo request = makeTypeInfo(compositeName);
      metadata = materializeCompositeInstantiation(request);
    }
    if (!metadata) {
      reportCompilerError("Unknown reference type '" + compositeName +
                          "' for 'new' expression");
      return {nullptr, nullptr};
    }
    if (metadata->descriptorGlobalName.empty()) {
      reportCompilerError("Internal error: missing descriptor for '" +
                          compositeName + "' during allocation");
      return {nullptr, nullptr};
    }

    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(metadata->descriptorGlobalName, true);
    if (!descriptorGV) {
      reportCompilerError("Internal error: descriptor '" +
                          metadata->descriptorGlobalName +
                          "' missing while allocating '" + compositeName + "'");
      return {nullptr, nullptr};
    }

    llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
        descriptorGV, pointerType(getTypeDescriptorType()));
    return {metadata, descriptorPtr};
  };

  if (ArrayForm) {
    const auto &bounds = getArraySizes();
    std::string arrayTypeName = targetType;
    if (arrayTypeName.find('[') == std::string::npos) {
      if (bounds.size() > 1) {
        arrayTypeName += "[";
        arrayTypeName.append(bounds.size() - 1, ',');
        arrayTypeName += "]";
      } else {
        arrayTypeName += "[]";
      }
    }

    TypeInfo arrayInfo = makeTypeInfo(arrayTypeName);
    finalizeTypeInfoMetadata(arrayInfo);
    ParsedTypeDescriptor desc = parseTypeString(arrayInfo.typeName);
    if (!desc.isArray) {
      reportCompilerError(
          "Array form of 'new' requires an array type",
          "Use 'new Type[size]' to allocate arrays.");
      return nullptr;
    }

    std::string elementTypeName = removeLastArrayGroup(desc.sanitized);
    if (elementTypeName.empty())
      elementTypeName = arrayInfo.baseTypeName;
    llvm::Type *elemType = getTypeFromString(elementTypeName);
    if (!elemType) {
      reportCompilerError("Unknown array element type '" + elementTypeName + "'");
      return nullptr;
    }
    TypeInfo elementInfo = makeTypeInfo(elementTypeName);
    finalizeTypeInfoMetadata(elementInfo);

    unsigned rank =
        desc.arrayRanks.empty() ? 1 : std::max(1u, desc.arrayRanks.back());
    if (bounds.size() != rank) {
      reportCompilerError(
          "Array bounds do not match declared rank for '" +
              typeNameFromInfo(arrayInfo) + "'",
          "Expected " + std::to_string(rank) + " bound(s) in 'new'.");
      return nullptr;
    }

    auto boundsInfoOpt =
        emitArrayBoundsInfo(bounds, buildArcOpLabel("new", "array.bounds"));
    if (!boundsInfoOpt)
      return nullptr;
    const ArrayBoundsInfo &boundsInfo = *boundsInfoOpt;

    uint64_t elemSize = getTypeSizeInBytes(elemType);
    if (elemSize == 0) {
      reportCompilerError(
          "Unable to determine element size for array allocation of '" +
          elementTypeName + "'");
      return nullptr;
    }

    llvm::Value *descriptorPtr =
        Builder->CreateCall(getHybridArrayDescriptorFunction(), {},
                            "new.array.descriptor");
    llvm::Value *releaseFn =
        selectArrayElementReleaseFunction(elementInfo, "new.array.releasefn");
    llvm::Value *elemSizeVal =
        llvm::ConstantInt::get(getSizeType(), elemSize);
    llvm::Value *rawPtr = Builder->CreateCall(
        getHybridAllocArrayFunction(),
        {elemSizeVal, boundsInfo.totalSize, descriptorPtr}, "new.array.raw");

    if (!llvm::isa<llvm::ConstantPointerNull>(releaseFn)) {
      llvm::Value *releaseFnCasted = releaseFn;
      if (releaseFn->getType() != getArrayReleaseCallbackPointerType()) {
        releaseFnCasted = Builder->CreateBitCast(
            releaseFn, getArrayReleaseCallbackPointerType(),
            "new.array.releasefn.cast");
      }
      Builder->CreateCall(getHybridArraySetReleaseFunction(),
                          {rawPtr, releaseFnCasted});
    }

    llvm::Value *rawBytePtr = Builder->CreateBitCast(
        rawPtr, pointerType(llvm::Type::getInt8Ty(*TheContext)),
        "new.array.byteptr");
    llvm::Value *payloadBytePtr = Builder->CreateInBoundsGEP(
        llvm::Type::getInt8Ty(*TheContext), rawBytePtr,
        llvm::ConstantInt::get(getSizeType(), getArrayPayloadOffsetBytes()),
        "new.array.payload.byte");
    llvm::Value *dataPtr = Builder->CreateBitCast(
        payloadBytePtr, pointerType(elemType), "new.array.payload");

    llvm::StructType *arrayStructTy = getArrayStructType(elemType, rank);
    llvm::Value *arrayValue = llvm::UndefValue::get(arrayStructTy);
    llvm::Value *opaqueDataPtr =
        Builder->CreateBitCast(dataPtr, pointerType(), "new.array.ptr");
    arrayValue = Builder->CreateInsertValue(arrayValue, opaqueDataPtr, {0});
    arrayValue = Builder->CreateInsertValue(arrayValue, boundsInfo.totalSize32, {1});

    for (unsigned i = 0; i < rank; ++i) {
      llvm::Value *dimVal = boundsInfo.dims32[i];
      arrayValue = Builder->CreateInsertValue(arrayValue, dimVal, {2u, i});
    }

    markTemporary();
    setTypeName(typeNameFromInfo(arrayInfo));
    return arrayValue;
  }

  std::string compositeName = sanitizeCompositeLookupName(targetType);
  if (compositeName.empty()) {
    reportCompilerError("Cannot determine allocation target for 'new'");
    return nullptr;
  }

  TypeInfo allocationInfo = makeTypeInfo(compositeName);
  finalizeTypeInfoMetadata(allocationInfo);

  if (allocationInfo.isSmartPointer()) {
    bool treatAsTuple = Args.size() > 1;
    ParenExprAST paren(std::move(Args), treatAsTuple);
    paren.setTypeName(typeNameFromInfo(allocationInfo));
    llvm::Value *constructed =
        emitTargetTypedConstruction(allocationInfo, paren);
    Args = paren.takeElements();
    normalizeArgMetadata();
    if (!constructed)
      return nullptr;
    markTemporary();
    setTypeName(typeNameFromInfo(allocationInfo));
    return constructed;
  }

  if (!allocationInfo.requiresARC()) {
    reportCompilerError(
        "The 'new' keyword is only supported for ARC-managed reference types",
        "Use value construction without 'new' for stack values or enable ARC support on the target type.");
    return nullptr;
  }

  auto [metadata, descriptorPtr] = materializeDescriptor(compositeName);
  if (!metadata || !descriptorPtr)
    return nullptr;

  auto structIt = StructTypes.find(compositeName);
  if (structIt == StructTypes.end()) {
    reportCompilerError("Internal error: missing struct type for '" +
                        compositeName + "' during allocation");
    return nullptr;
  }
  llvm::StructType *structTy = structIt->second;

  const llvm::DataLayout &DL = TheModule->getDataLayout();
  uint64_t typeSize = DL.getTypeAllocSize(structTy);
  llvm::Value *sizeVal = llvm::ConstantInt::get(getSizeType(), typeSize);

  std::vector<std::unique_ptr<ExprAST>> ctorArgs = std::move(Args);
  std::vector<std::string> ctorArgNames = std::move(ArgNames);
  std::vector<SourceLocation> ctorNameLocs = std::move(ArgNameLocations);
  std::vector<SourceLocation> ctorEqualLocs = std::move(ArgEqualsLocations);
  auto ctorCall =
      std::make_unique<CallExprAST>(compositeName, std::move(ctorArgs),
                                    std::move(ctorArgNames),
                                    std::move(ctorNameLocs),
                                    std::move(ctorEqualLocs));
  llvm::Value *constructed = ctorCall->codegen();
  if (!constructed)
    return nullptr;

  llvm::Value *rawPtr = Builder->CreateCall(
      getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
      "new.object.raw");
  llvm::Value *typedPtr =
      Builder->CreateBitCast(rawPtr, pointerType(structTy), "new.object");

  llvm::Value *initValue = constructed;
  if (constructed->getType()->isPointerTy())
    initValue = Builder->CreateLoad(structTy, constructed, "new.init");
  else if (constructed->getType() != structTy)
    initValue = castToType(constructed, structTy);

  if (!initValue)
    return nullptr;

  Builder->CreateStore(initValue, typedPtr);
  markTemporary();
  setTypeName(targetType);
  return typedPtr;
}

bool parseExplicitTypeArgumentSuffix(const std::string &text,
                                     std::string &baseName,
                                     std::vector<TypeInfo> &typeArguments) {
  size_t anglePos = text.find('<');
  if (anglePos == std::string::npos) {
    baseName = text;
    typeArguments.clear();
    return true;
  }

  auto closePos = findMatchingAngleInTypeName(text, anglePos);
  if (!closePos || *closePos != text.size() - 1) {
    reportCompilerError("Malformed generic argument list in '" + text + "'");
    return false;
  }

  baseName = text.substr(0, anglePos);
  std::string segment = text.substr(anglePos + 1, *closePos - anglePos - 1);
  typeArguments = buildGenericArgumentTypeInfos(segment);
  return true;
}

void CallExprAST::normalizeArgMetadata() {
  const std::size_t count = Args.size();
  ArgNames.resize(count);
  ArgNameLocations.resize(count);
  ArgEqualsLocations.resize(count);
}

void CallExprAST::resetArgs(std::vector<std::unique_ptr<ExprAST>> NewArgs) {
  Args = std::move(NewArgs);
  normalizeArgMetadata();
}
