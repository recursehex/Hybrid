#include "ast_internal.h"

//===----------------------------------------------------------------------===//
// Expression Code Generation
//===----------------------------------------------------------------------===//

static llvm::AllocaInst *createEntryAlloca(llvm::Function *Fn, llvm::Type *Ty,
                                           const std::string &Name) {
  llvm::IRBuilder<> EntryBuilder(&Fn->getEntryBlock(),
                                 Fn->getEntryBlock().begin());
  return EntryBuilder.CreateAlloca(Ty, nullptr, Name);
}

// Generate code for number expressions
llvm::Value *NumberExprAST::codegen() {
  if (Literal.isDecimal()) {
    setTypeName("decimal");
    return buildDecimalConstantFromSpelling(Literal.getSpelling());
  }

  if (Literal.isInteger()) {
    const llvm::APInt &value = Literal.getIntegerValue();
    unsigned requiredBits = Literal.getRequiredBitWidth();

    if (requiredBits <= 32 && Literal.fitsInSignedBits(32)) {
      setTypeName("int");
      llvm::APInt as32 = value.sextOrTrunc(32);
      return llvm::ConstantInt::get(*TheContext, as32);
    }

    if (requiredBits <= 64 && Literal.fitsInSignedBits(64)) {
      setTypeName("long");
      llvm::APInt as64 = value.sextOrTrunc(64);
      return llvm::ConstantInt::get(*TheContext, as64);
    }

    if (requiredBits <= 64 && Literal.fitsInUnsignedBits(64)) {
      setTypeName("ulong");
      llvm::APInt asUnsigned64 = value.zextOrTrunc(64);
      return llvm::ConstantInt::get(*TheContext, asUnsigned64);
    }

    setTypeName("double");
    return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Literal.toDouble()));
  }

  setTypeName("double");
  llvm::APFloat value = Literal.getFloatValue();
  bool losesInfo = false;
  value.convert(llvm::APFloat::IEEEdouble(), llvm::APFloat::rmNearestTiesToEven, &losesInfo);
  (void)losesInfo;
  return llvm::ConstantFP::get(*TheContext, value);
}

// Generate code for numeric literal with target type context
llvm::Value *NumberExprAST::codegen_with_target(llvm::Type *TargetType) {
  if (isDecimalLLVMType(TargetType)) {
    setTypeName("decimal");
    if (Literal.isDecimal())
      return buildDecimalConstantFromSpelling(Literal.getSpelling());
    if (Literal.isInteger()) {
      long double numeric = 0.0L;
      if (Literal.fitsInSignedBits(64))
        numeric = static_cast<long double>(Literal.getSignedValue());
      else
        numeric = static_cast<long double>(Literal.getUnsignedValue());
      return buildDecimalConstantFromLongDouble(numeric);
    }
    return buildDecimalConstantFromLongDouble(
        static_cast<long double>(Literal.toDouble()));
  }

  if (TargetType->isFloatingPointTy()) {
    setTypeName(TargetType->isFloatTy() ? "float" : "double");
    return llvm::ConstantFP::get(TargetType, Literal.toDouble());
  }

  if (TargetType->isIntegerTy() && Literal.isInteger()) {
    const llvm::APInt &value = Literal.getIntegerValue();
    unsigned bitWidth = TargetType->getIntegerBitWidth();

    if (!value.isIntN(bitWidth))
      return codegen();

    llvm::APInt adjusted = value;
    if (value.getBitWidth() != bitWidth)
      adjusted = value.zextOrTrunc(bitWidth);

    if (bitWidth == 8) setTypeName("byte");
    else if (bitWidth == 16) setTypeName("short");
    else if (bitWidth == 32) setTypeName("int");
    else if (bitWidth == 64) {
      setTypeName(value.isSignedIntN(64) ? "long" : "ulong");
    }

    return llvm::ConstantInt::get(*TheContext, adjusted);
  }

  return codegen();
}

// Generate code for boolean expressions
llvm::Value *BoolExprAST::codegen() {
  setTypeName("bool");
  return llvm::ConstantInt::get(*TheContext, llvm::APInt(8, getValue() ? 1 : 0));
}

// Generate code for null expressions
llvm::Value *NullExprAST::codegen() {
  llvm::PointerType *OpaquePtr = llvm::PointerType::get(*TheContext, 0);
  setTypeName("null");
  return llvm::ConstantPointerNull::get(OpaquePtr);
}

static std::string buildTupleTypeName(const std::vector<TypeInfo> &elementInfos) {
  std::string typeName = "tuple<";
  for (size_t i = 0; i < elementInfos.size(); ++i) {
    if (i > 0)
      typeName += ",";
    typeName += typeNameFromInfo(elementInfos[i]);
  }
  typeName += ">";
  return typeName;
}

static llvm::Value *allocateTupleLiteralStorage(
    const TypeInfo &tupleInfo, llvm::StructType **outStructTy,
    const CompositeTypeInfo **outMeta, std::string_view label) {
  TypeInfo boundTuple = applyActiveTypeBindings(tupleInfo);
  finalizeTypeInfoMetadata(boundTuple);
  if (!boundTuple.isTupleType())
    return nullptr;

  if (!materializeTupleInstantiation(boundTuple))
    return nullptr;

  std::string tupleName =
      stripNullableAnnotations(typeNameFromInfo(boundTuple));
  auto structIt = StructTypes.find(tupleName);
  if (structIt == StructTypes.end() || !structIt->second) {
    reportCompilerError("Internal error: missing tuple type '" + tupleName + "'");
    return nullptr;
  }

  const CompositeTypeInfo *metadata =
      lookupCompositeInfo(tupleName, /*countHit=*/false);
  if (!metadata) {
    reportCompilerError("Internal error: missing tuple metadata for '" +
                        tupleName + "'");
    return nullptr;
  }

  if (metadata->descriptorGlobalName.empty()) {
    reportCompilerError("Internal error: missing tuple descriptor for '" +
                        tupleName + "'");
    return nullptr;
  }

  llvm::GlobalVariable *descriptorGV = TheModule->getGlobalVariable(
      metadata->descriptorGlobalName, true);
  if (!descriptorGV) {
    reportCompilerError("Internal error: tuple descriptor '" +
                        metadata->descriptorGlobalName + "' missing");
    return nullptr;
  }

  const llvm::DataLayout &DL = TheModule->getDataLayout();
  uint64_t typeSize = DL.getTypeAllocSize(structIt->second);
  llvm::Value *sizeVal = llvm::ConstantInt::get(getSizeType(), typeSize);

  llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
      descriptorGV, pointerType(getTypeDescriptorType()));

  llvm::Value *rawPtr = Builder->CreateCall(
      getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
      std::string(label) + ".alloc");
  llvm::Value *typedPtr = Builder->CreateBitCast(
      rawPtr, pointerType(structIt->second),
      std::string(label) + ".ptr");

  if (outStructTy)
    *outStructTy = structIt->second;
  if (outMeta)
    *outMeta = metadata;
  return typedPtr;
}

static bool storeTupleElementValue(const TypeInfo &elementInfo,
                                   llvm::StructType *tupleStructTy,
                                   llvm::Value *tuplePtr,
                                   unsigned fieldIndex,
                                   ExprAST *elementExpr,
                                   llvm::Value *elementValue,
                                   std::string_view label) {
  if (!tupleStructTy || !tuplePtr || !elementValue)
    return false;

  llvm::Value *fieldPtr = Builder->CreateStructGEP(
      tupleStructTy, tuplePtr, fieldIndex,
      std::string(label) + ".field");

  if (!fieldPtr || !fieldPtr->getType()->isPointerTy())
    return false;

  bool elementIsTemporary = elementExpr && elementExpr->isTemporary();

  if (elementInfo.isSmartPointer()) {
    const std::string constructedName =
        stripNullableAnnotations(typeNameFromInfo(elementInfo));
    auto structIt = StructTypes.find(constructedName);
    llvm::StructType *structTy =
        structIt != StructTypes.end() ? structIt->second : nullptr;
    const CompositeTypeInfo *metadata =
        resolveSmartPointerMetadata(elementInfo);
    if (!structTy || !metadata) {
      reportCompilerError(
          "Initializer for tuple element has incompatible smart pointer representation");
      return false;
    }

    if (auto *rhsVar = dynamic_cast<VariableExprAST *>(elementExpr)) {
      if (emitSmartPointerInitFromVariable(elementInfo, fieldPtr, *rhsVar,
                                           "tuple")) {
        return true;
      }
    }

    if (!metadata->smartPointerDestroyHelper.empty()) {
      llvm::Function *destroyFn = TheModule->getFunction(
          metadata->smartPointerDestroyHelper);
      if (!destroyFn) {
        reportCompilerError(
            "Internal error: missing smart pointer destroy helper '" +
            metadata->smartPointerDestroyHelper + "'");
        return false;
      }
      llvm::Value *destroyArg = fieldPtr;
      llvm::Type *expectedTy =
          destroyFn->getFunctionType()->getParamType(0);
      if (expectedTy && destroyArg->getType() != expectedTy) {
        destroyArg = Builder->CreateBitCast(
            destroyArg, expectedTy,
            buildArcOpLabel(label, "smart.destroy.cast"));
      }
      Builder->CreateCall(destroyFn, {destroyArg});
    }

    llvm::Value *stored = elementValue;
    if (stored->getType()->isPointerTy()) {
      stored = Builder->CreateLoad(
          structTy, stored, buildArcOpLabel(label, "smart.assign.load"));
    }
    if (stored->getType() != structTy) {
      reportCompilerError(
          "Initializer for tuple element has incompatible smart pointer representation");
      return false;
    }

    Builder->CreateStore(stored, fieldPtr);
    return true;
  }

  if (elementInfo.requiresARC()) {
    if (!emitManagedStore(fieldPtr, elementValue, elementInfo, label,
                          elementIsTemporary))
      return false;
    return true;
  }

  Builder->CreateStore(elementValue, fieldPtr);
  return true;
}

static llvm::Value *emitTupleLiteralWithTarget(ParenExprAST &paren,
                                               const TypeInfo &targetInfo) {
  if (!paren.isTuple())
    return nullptr;

  if (paren.size() < 2) {
    reportCompilerError("Tuple literals require at least two elements");
    return nullptr;
  }

  TypeInfo tupleInfo = applyActiveTypeBindings(targetInfo);
  finalizeTypeInfoMetadata(tupleInfo);
  if (!tupleInfo.isTupleType())
    return nullptr;

  if (tupleInfo.typeArguments.size() != paren.size()) {
    reportCompilerError("Tuple literal has " +
                        std::to_string(paren.size()) +
                        " element(s), but expected " +
                        std::to_string(tupleInfo.typeArguments.size()));
    return nullptr;
  }

  llvm::StructType *tupleStructTy = nullptr;
  const CompositeTypeInfo *metadata = nullptr;
  llvm::Value *tuplePtr = allocateTupleLiteralStorage(
      tupleInfo, &tupleStructTy, &metadata, "tuple.literal");
  if (!tuplePtr || !tupleStructTy || !metadata)
    return nullptr;

  for (size_t i = 0; i < paren.size(); ++i) {
    ExprAST *elementExpr = paren.getElement(i);
    if (!elementExpr)
      return nullptr;

    TypeInfo elementInfo = applyActiveTypeBindings(tupleInfo.typeArguments[i]);
    finalizeTypeInfoMetadata(elementInfo);

    if (!validateNullableAssignment(
            elementInfo, elementExpr,
            "tuple element " + std::to_string(i))) {
      return nullptr;
    }

    llvm::Value *elementValue = nullptr;
    if (auto *elemParen = dynamic_cast<ParenExprAST *>(elementExpr)) {
      elementValue = emitTargetTypedConstruction(elementInfo, *elemParen);
    }

    if (!elementValue) {
      if (auto *newExpr = dynamic_cast<NewExprAST *>(elementExpr))
        propagateTypeToNewExpr(newExpr, elementInfo);
    }

    std::string elementTypeName = typeNameFromInfo(elementInfo);
    std::string cleanElementTypeName =
        sanitizeBaseTypeName(elementTypeName);
    if (cleanElementTypeName.empty())
      cleanElementTypeName = elementTypeName;

    llvm::Type *elementLLVMType = getTypeFromString(elementTypeName);

    if (!elementValue) {
      if (auto *num = dynamic_cast<NumberExprAST *>(elementExpr)) {
        if (elementLLVMType)
          elementValue = num->codegen_with_target(elementLLVMType);
      } else if (auto *ch = dynamic_cast<CharExprAST *>(elementExpr)) {
        if (elementLLVMType) {
          elementValue =
              ch->codegen_with_target(elementLLVMType, cleanElementTypeName);
        }
      }
    }

    if (!elementValue)
      elementValue = elementExpr->codegen();
    if (!elementValue)
      return nullptr;

    if (!elementLLVMType) {
      reportCompilerError("Unknown tuple element type '" +
                          elementTypeName + "'");
      return nullptr;
    }

    if (!validateTupleAssignmentCompatibility(
            elementInfo, elementExpr,
            "tuple element " + std::to_string(i)))
      return nullptr;

    llvm::Type *actualType = elementValue->getType();
    if (actualType != elementLLVMType) {
      bool compatible = false;
      if (actualType && elementLLVMType &&
          actualType->isPointerTy() && elementLLVMType->isPointerTy()) {
        compatible = true;
      } else if (actualType && elementLLVMType &&
                 areTypesCompatible(actualType, elementLLVMType)) {
        compatible = true;
      }
      if (!compatible) {
        std::string actualTypeName = elementExpr->getTypeName();
        if (actualTypeName.empty())
          actualTypeName = describeTypeForDiagnostic(actualType);
        reportCompilerError(
            "Tuple element " + std::to_string(i) + " expects '" +
            elementTypeName + "', got '" + actualTypeName + "'");
        return nullptr;
      }
    }

    if (diagnoseDisallowedImplicitIntegerConversion(
            elementExpr, elementValue, elementLLVMType, elementTypeName,
            "tuple element " + std::to_string(i)))
      return nullptr;

    llvm::Value *storedValue = elementValue;
    if (!elementInfo.isSmartPointer()) {
      storedValue = castToType(elementValue, elementLLVMType,
                               cleanElementTypeName);
      if (!storedValue)
        return nullptr;
    }

    unsigned fieldIndex = static_cast<unsigned>(i);
    if (metadata->hasARCHeader)
      fieldIndex += metadata->headerFieldIndex + 1;
    if (fieldIndex >= tupleStructTy->getNumElements()) {
      reportCompilerError("Tuple element index out of range");
      return nullptr;
    }

    if (!storeTupleElementValue(elementInfo, tupleStructTy, tuplePtr,
                                fieldIndex, elementExpr, storedValue,
                                "tuple.elem"))
      return nullptr;
  }

  paren.setTypeInfo(tupleInfo);
  paren.markTemporary();
  return tuplePtr;
}

static llvm::Value *emitTupleLiteralInferred(ParenExprAST &paren) {
  if (paren.size() < 2) {
    reportCompilerError("Tuple literals require at least two elements");
    return nullptr;
  }

  std::vector<llvm::Value *> elementValues;
  std::vector<TypeInfo> elementInfos;
  elementValues.reserve(paren.size());
  elementInfos.reserve(paren.size());

  for (size_t i = 0; i < paren.size(); ++i) {
    ExprAST *elementExpr = paren.getElement(i);
    if (!elementExpr)
      return nullptr;

    llvm::Value *elementValue = elementExpr->codegen();
    if (!elementValue)
      return nullptr;

    auto infoOpt = resolveExprTypeInfo(elementExpr);
    if (!infoOpt || infoOpt->typeName.empty() ||
        infoOpt->baseTypeName == "null") {
      reportCompilerError(
          "Cannot infer type for tuple element " + std::to_string(i),
          "Specify the tuple type explicitly or use a typed element.");
      return nullptr;
    }

    elementValues.push_back(elementValue);
    elementInfos.push_back(*infoOpt);
  }

  TypeInfo tupleInfo = makeTypeInfo(buildTupleTypeName(elementInfos));
  tupleInfo.typeArguments = elementInfos;
  finalizeTypeInfoMetadata(tupleInfo);

  llvm::StructType *tupleStructTy = nullptr;
  const CompositeTypeInfo *metadata = nullptr;
  llvm::Value *tuplePtr = allocateTupleLiteralStorage(
      tupleInfo, &tupleStructTy, &metadata, "tuple.literal");
  if (!tuplePtr || !tupleStructTy || !metadata)
    return nullptr;

  for (size_t i = 0; i < elementInfos.size(); ++i) {
    const TypeInfo &elementInfo = elementInfos[i];
    std::string elementTypeName = typeNameFromInfo(elementInfo);
    llvm::Type *elementLLVMType = getTypeFromString(elementTypeName);
    if (!elementLLVMType) {
      reportCompilerError("Unknown tuple element type '" +
                          elementTypeName + "'");
      return nullptr;
    }

    llvm::Value *storedValue = elementValues[i];
    if (!elementInfo.isSmartPointer()) {
      storedValue = castToType(storedValue, elementLLVMType, elementTypeName);
      if (!storedValue)
        return nullptr;
    }

    unsigned fieldIndex = static_cast<unsigned>(i);
    if (metadata->hasARCHeader)
      fieldIndex += metadata->headerFieldIndex + 1;
    if (fieldIndex >= tupleStructTy->getNumElements()) {
      reportCompilerError("Tuple element index out of range");
      return nullptr;
    }

    ExprAST *elementExpr = paren.getElement(i);
    if (!storeTupleElementValue(elementInfo, tupleStructTy, tuplePtr,
                                fieldIndex, elementExpr, storedValue,
                                "tuple.elem"))
      return nullptr;
  }

  paren.setTypeInfo(tupleInfo);
  paren.markTemporary();
  return tuplePtr;
}

llvm::Value *ParenExprAST::codegen() {
  if (IsTupleExpr) {
    if (const TypeInfo *tupleInfo = getTypeInfo()) {
      if (tupleInfo->isTupleType())
        return emitTupleLiteralWithTarget(*this, *tupleInfo);
    }
    if (!getTypeName().empty()) {
      TypeInfo tupleInfo = makeTypeInfo(getTypeName());
      finalizeTypeInfoMetadata(tupleInfo);
      if (tupleInfo.isTupleType())
        return emitTupleLiteralWithTarget(*this, tupleInfo);
    }
    return emitTupleLiteralInferred(*this);
  }

  if (Elements.empty()) {
    reportCompilerError(
        "Empty parenthesized expression is not allowed here",
        "Provide an expression inside parentheses or remove them.");
    return nullptr;
  }

  ExprAST *inner = Elements.front().get();
  llvm::Value *value = inner->codegen();
  if (!value)
    return nullptr;

  setTypeName(inner->getTypeName());
  return value;
}

llvm::Value *ParenExprAST::codegen_ptr() {
  if (IsTupleExpr || Elements.empty())
    return nullptr;
  return Elements.front()->codegen_ptr();
}

static llvm::FunctionCallee getStringFromUtf8LiteralFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  auto *bytePtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *fnTy = llvm::FunctionType::get(stringPtrTy, {bytePtrTy, sizeTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_utf8_literal",
                                        fnTy);
}

llvm::Value *emitStringLiteral(const std::string &value) {
  static std::map<std::string, llvm::GlobalVariable *> StringLiteralCache;

  // Validate UTF-8 to preserve existing diagnostics.
  std::vector<uint16_t> utf16Units;
  std::string conversionError;
  if (!convertUTF8LiteralToUTF16(value, utf16Units, conversionError))
    return LogErrorV(conversionError.c_str());

  llvm::GlobalVariable *global = nullptr;
  if (auto it = StringLiteralCache.find(value); it != StringLiteralCache.end()) {
    global = it->second;
  } else {
    std::vector<llvm::Constant *> bytes;
    bytes.reserve(value.size() + 1);
    for (unsigned char c : value) {
      bytes.push_back(
          llvm::ConstantInt::get(llvm::Type::getInt8Ty(*TheContext), c));
    }
    bytes.push_back(
        llvm::ConstantInt::get(llvm::Type::getInt8Ty(*TheContext), 0));

    auto *arrayType =
        llvm::ArrayType::get(llvm::Type::getInt8Ty(*TheContext), bytes.size());
    auto *constArray = llvm::ConstantArray::get(arrayType, bytes);

    global = new llvm::GlobalVariable(
        *TheModule, arrayType, true, llvm::GlobalValue::PrivateLinkage,
        constArray, "str");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    global->setAlignment(llvm::MaybeAlign(1));
    StringLiteralCache.emplace(value, global);
  }

  auto *arrayType = llvm::cast<llvm::ArrayType>(global->getValueType());
  llvm::Value *zero =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  llvm::Value *dataPtr = Builder->CreateInBoundsGEP(
      arrayType, global, {zero, zero}, "strptr");
  llvm::Value *lenVal =
      llvm::ConstantInt::get(getSizeType(), value.size());
  return Builder->CreateCall(getStringFromUtf8LiteralFunction(),
                             {dataPtr, lenVal}, "str.literal");
}

// Generate code for string literals
llvm::Value *StringExprAST::codegen() {
  setTypeName("string");
  markTemporary();
  return emitStringLiteral(getValue());
}

llvm::FunctionCallee getConcatStringsFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *stringPtrPtrTy = llvm::PointerType::get(*TheContext, 0);
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {stringPtrPtrTy, int32Ty},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_concat_strings", fnType);
}

llvm::FunctionCallee getStringEqualsFunction() {
  llvm::Type *intTy = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::FunctionType *fnType = llvm::FunctionType::get(intTy,
                                                       {stringPtrTy, stringPtrTy},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_string_equals", fnType);
}

static llvm::FunctionCallee getIntToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *boolTy = llvm::Type::getInt1Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {int64Ty, boolTy},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_int64", fnType);
}

static llvm::FunctionCallee getDoubleToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *doubleTy = llvm::Type::getDoubleTy(*TheContext);
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *boolTy = llvm::Type::getInt1Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {doubleTy, int32Ty, boolTy},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_double", fnType);
}

llvm::FunctionCallee getDecimalToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *boolTy = llvm::Type::getInt1Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(stringPtrTy, {decimalTy, int32Ty, boolTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_to_string", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getDecimalParseFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *charPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  llvm::Type *sizeTy = getSizeType();
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {charPtrTy, sizeTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_parse", fnType);
}

llvm::FunctionCallee getDecimalAddFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_add", fnType);
}

llvm::FunctionCallee getDecimalSubFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_sub", fnType);
}

llvm::FunctionCallee getDecimalMulFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_mul", fnType);
}

llvm::FunctionCallee getDecimalDivFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_div", fnType);
}

llvm::FunctionCallee getDecimalRemFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_rem", fnType);
}

llvm::FunctionCallee getDecimalCmpFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(int32Ty, {decimalTy, decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_cmp", fnType);
}

llvm::FunctionCallee getDecimalNegFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_neg", fnType);
}

llvm::FunctionCallee getDecimalFromI64Function() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {int64Ty}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_from_i64", fnType);
}

llvm::FunctionCallee getDecimalFromU64Function() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {int64Ty}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_from_u64", fnType);
}

llvm::FunctionCallee getDecimalToI64Function() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(int64Ty, {decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_to_i64", fnType);
}

llvm::FunctionCallee getDecimalToU64Function() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(int64Ty, {decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_to_u64", fnType);
}

llvm::FunctionCallee getDecimalFromDoubleFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *doubleTy = llvm::Type::getDoubleTy(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(decimalTy, {doubleTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_from_double", fnType);
}

llvm::FunctionCallee getDecimalToDoubleFunction() {
  llvm::Type *decimalTy = getTypeFromString("decimal");
  llvm::Type *doubleTy = llvm::Type::getDoubleTy(*TheContext);
  llvm::FunctionType *fnType =
      llvm::FunctionType::get(doubleTy, {decimalTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_decimal_to_double", fnType);
}

llvm::FunctionCallee getCharToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy, {int32Ty}, false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_char32", fnType);
}

llvm::Value *emitTargetTypedConstruction(const TypeInfo &targetInfo,
                                         ParenExprAST &paren) {
  TypeInfo boundTarget = applyActiveTypeBindings(targetInfo);
  finalizeTypeInfoMetadata(boundTarget);

  if (boundTarget.isTupleType()) {
    if (!paren.isTuple())
      return nullptr;
    return emitTupleLiteralWithTarget(paren, boundTarget);
  }

  if (boundTarget.isArray || boundTarget.pointerDepth > 0)
    return nullptr;

  const bool targetIsSmartPointer = boundTarget.isSmartPointer();
  SmartPointerKind targetSmartKind =
      targetIsSmartPointer ? detectSmartPointerKind(boundTarget.baseTypeName)
                           : SmartPointerKind::None;

  std::optional<TypeInfo> payloadInfoOpt;
  std::string payloadTypeName;
  llvm::Type *payloadLLVMType = nullptr;
  if (targetIsSmartPointer && !boundTarget.typeArguments.empty()) {
    TypeInfo payloadInfo =
        applyActiveTypeBindings(boundTarget.typeArguments.front());
    finalizeTypeInfoMetadata(payloadInfo);

    if (targetSmartKind == SmartPointerKind::Weak) {
      std::string payloadName = typeNameFromInfo(payloadInfo);
      payloadInfo = makeTypeInfo("shared<" + payloadName + ">");
      finalizeTypeInfoMetadata(payloadInfo);
    }

    payloadTypeName = typeNameFromInfo(payloadInfo);
    auto payloadStructIt =
        StructTypes.find(stripNullableAnnotations(payloadTypeName));
    if (payloadInfo.isSmartPointer())
      materializeCompositeInstantiation(payloadInfo);
    payloadStructIt =
        StructTypes.find(stripNullableAnnotations(payloadTypeName));
    if (payloadStructIt != StructTypes.end())
      payloadLLVMType = payloadStructIt->second;
    if (!payloadLLVMType)
      payloadLLVMType = getTypeFromString(payloadTypeName);
    payloadInfoOpt = std::move(payloadInfo);
  }

  std::string targetName = typeNameFromInfo(boundTarget);
  if (targetName.empty())
    targetName = boundTarget.typeName;
  if (targetName.empty())
    return nullptr;

  std::string baseName = baseCompositeName(targetName);
  if (baseName.empty()) {
    return nullptr;
  }
  if (!lookupCompositeInfo(baseName)) {
    return nullptr;
  }

  if (!materializeCompositeInstantiation(boundTarget)) {
    return nullptr;
  }

  std::string constructedKey =
      stripNullableAnnotations(typeNameFromInfo(boundTarget));
  const CompositeTypeInfo *instMeta =
      lookupCompositeInfo(constructedKey, /*countHit=*/false);
  if (!instMeta) {
    return nullptr;
  }

  paren.setTypeName(targetName);

  auto finalizePayloadValue = [&](llvm::Value *constructedPayload) -> llvm::Value * {
    if (!payloadInfoOpt)
      return constructedPayload;

    std::string payloadName = payloadTypeName.empty()
                                  ? typeNameFromInfo(*payloadInfoOpt)
                                  : payloadTypeName;
    std::string payloadKey = stripNullableAnnotations(payloadName);
    auto structIt = StructTypes.find(payloadKey);
    const CompositeTypeInfo *payloadMeta =
        lookupCompositeInfo(payloadKey, /*countHit=*/false);
    bool payloadNeedsHeap = payloadMeta && structIt != StructTypes.end() &&
                            payloadMeta->hasARCHeader && constructedPayload;
    if (payloadNeedsHeap && constructedPayload->getType()->isPointerTy()) {
      llvm::Value *stripped = constructedPayload->stripPointerCasts();
      if (llvm::isa<llvm::AllocaInst>(stripped)) {
        const llvm::DataLayout &DL = TheModule->getDataLayout();
        uint64_t typeSize = DL.getTypeAllocSize(structIt->second);
        llvm::Value *sizeVal =
            llvm::ConstantInt::get(getSizeType(), typeSize);

        if (payloadMeta->descriptorGlobalName.empty())
          return LogErrorV(("Internal error: missing descriptor for '" +
                            payloadName + "' while constructing value")
                               .c_str());

        llvm::GlobalVariable *descriptorGV = TheModule->getGlobalVariable(
            payloadMeta->descriptorGlobalName, true);
        if (!descriptorGV)
          return LogErrorV(("Internal error: descriptor '" +
                            payloadMeta->descriptorGlobalName +
                            "' missing while constructing '" + payloadName +
                            "'")
                               .c_str());

        llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
            descriptorGV, pointerType(getTypeDescriptorType()));

        llvm::Value *rawPtr = Builder->CreateCall(
            getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
            payloadName + ".alloc");
        llvm::Value *typedPtr = Builder->CreateBitCast(
            rawPtr, pointerType(structIt->second),
            payloadName + ".alloc.typed");

        llvm::Value *loadedVal = Builder->CreateLoad(
            structIt->second, constructedPayload, payloadName + ".stack");
        Builder->CreateStore(loadedVal, typedPtr);
        constructedPayload = typedPtr;
      }
    } else if (payloadNeedsHeap &&
               constructedPayload && !constructedPayload->getType()->isPointerTy()) {
      const llvm::DataLayout &DL = TheModule->getDataLayout();
      uint64_t typeSize = DL.getTypeAllocSize(structIt->second);
      llvm::Value *sizeVal =
          llvm::ConstantInt::get(getSizeType(), typeSize);

      if (payloadMeta->descriptorGlobalName.empty())
        return LogErrorV(("Internal error: missing descriptor for '" +
                          payloadName + "' while constructing value")
                             .c_str());

      llvm::GlobalVariable *descriptorGV = TheModule->getGlobalVariable(
          payloadMeta->descriptorGlobalName, true);
      if (!descriptorGV)
        return LogErrorV(("Internal error: descriptor '" +
                          payloadMeta->descriptorGlobalName +
                          "' missing while constructing '" + payloadName +
                          "'")
                             .c_str());

      llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
          descriptorGV, pointerType(getTypeDescriptorType()));

      llvm::Value *rawPtr = Builder->CreateCall(
          getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
          payloadName + ".alloc");
      llvm::Value *typedPtr = Builder->CreateBitCast(
          rawPtr, pointerType(structIt->second),
          payloadName + ".alloc.typed");
      Builder->CreateStore(constructedPayload, typedPtr);
      constructedPayload = typedPtr;
    }

    return constructedPayload;
  };

  std::vector<llvm::Value *> argVals;
  std::vector<bool> argIsRef;
  argVals.reserve(paren.size());
  argIsRef.reserve(paren.size());

  if (targetIsSmartPointer && payloadInfoOpt && paren.isTuple() &&
      !payloadInfoOpt->isSmartPointer()) {
    std::vector<llvm::Value *> payloadArgs;
    std::vector<bool> payloadIsRef;
    payloadArgs.reserve(paren.size());
    payloadIsRef.reserve(paren.size());

    for (size_t i = 0; i < paren.size(); ++i) {
      ExprAST *elem = paren.getElement(i);
      bool isRef = dynamic_cast<RefExprAST *>(elem) != nullptr;
      payloadIsRef.push_back(isRef);

      llvm::Value *val = elem->codegen();
      if (!val)
        return nullptr;
      payloadArgs.push_back(val);
    }

    llvm::Value *payloadVal = emitResolvedCallInternal(
        payloadTypeName.empty() ? typeNameFromInfo(*payloadInfoOpt)
                                : payloadTypeName,
        std::move(payloadArgs), payloadIsRef, nullptr, false, nullptr, &paren);
    if (!payloadVal)
      return nullptr;

    payloadVal = finalizePayloadValue(payloadVal);
    if (!payloadVal)
      return nullptr;

    argVals.push_back(payloadVal);
    argIsRef.push_back(false);
  } else {
    for (size_t i = 0; i < paren.size(); ++i) {
      ExprAST *elem = paren.getElement(i);
      bool isRef = dynamic_cast<RefExprAST *>(elem) != nullptr;
      argIsRef.push_back(isRef);

      llvm::Value *val = nullptr;
      bool initializerIsPayloadCall = false;

      if (payloadInfoOpt && i == 0 && !payloadInfoOpt->isSmartPointer()) {
        std::string payloadName = typeNameFromInfo(*payloadInfoOpt);
        std::string payloadBase = baseCompositeName(payloadName);

        if (auto *innerParen = dynamic_cast<ParenExprAST *>(elem)) {
          innerParen->setTypeName(payloadName);
          val = emitTargetTypedConstruction(*payloadInfoOpt, *innerParen);
        } else if (auto *num = dynamic_cast<NumberExprAST *>(elem)) {
          if (payloadLLVMType)
            val = num->codegen_with_target(payloadLLVMType);
        } else if (auto *ch = dynamic_cast<CharExprAST *>(elem)) {
          if (payloadLLVMType)
            val = ch->codegen_with_target(payloadLLVMType, payloadName);
        }

        if (!val && payloadInfoOpt)
          propagateTypeToNewExpr(elem, *payloadInfoOpt);

        // If the initializer already constructs the payload type directly,
        // just use that result instead of wrapping it again.
        if (!val && !payloadBase.empty()) {
          if (auto *call = dynamic_cast<CallExprAST *>(elem)) {
            if (!call->hasCalleeExpr()) {
              std::string calleeName = call->getCallee();
              std::string calleeBase = baseCompositeName(calleeName);
              if (calleeName == payloadName || calleeBase == payloadBase) {
                val = elem->codegen();
                initializerIsPayloadCall = true;
              }
            }
          }
        }

        // If the payload is a composite type and we didn't already build it,
        // try invoking its constructor directly with this single element.
        if (!val && !payloadBase.empty() && lookupCompositeInfo(payloadBase)) {
          std::vector<llvm::Value *> payloadArgs;
          std::vector<bool> payloadIsRef;
          llvm::Value *argVal = elem->codegen();
          if (!argVal)
            return nullptr;
          payloadArgs.push_back(argVal);
          payloadIsRef.push_back(isRef);
          llvm::Value *constructedPayload = emitResolvedCallInternal(
              payloadName, std::move(payloadArgs), payloadIsRef, nullptr, false,
              nullptr, &paren);
          if (!constructedPayload)
            return nullptr;

          constructedPayload = finalizePayloadValue(constructedPayload);
          if (!constructedPayload)
            return nullptr;

          val = constructedPayload;
          initializerIsPayloadCall = true;
        }
      }

      if (!val)
        val = elem->codegen();
      if (!val)
        return nullptr;

      if (payloadInfoOpt && i == 0 && initializerIsPayloadCall &&
          payloadInfoOpt->requiresARC()) {
        llvm::Value *wrapped = finalizePayloadValue(val);
        if (!wrapped)
          return nullptr;
        val = wrapped;
      }

      if (payloadInfoOpt && i == 0 && payloadInfoOpt->isSmartPointer() &&
          payloadLLVMType && payloadLLVMType->isPointerTy() &&
          !val->getType()->isPointerTy()) {
        std::string payloadTypeNameLocal = typeNameFromInfo(*payloadInfoOpt);
        auto structIt = StructTypes.find(
            stripNullableAnnotations(payloadTypeNameLocal));
        if (structIt != StructTypes.end()) {
          llvm::Type *elemTy = structIt->second;
          llvm::AllocaInst *tmp =
              Builder->CreateAlloca(elemTy, nullptr, "smart.payload.tmp");
          llvm::Value *stored = val;
          if (val->getType() != elemTy) {
            stored = castToType(val, elemTy, payloadTypeNameLocal);
            if (!stored)
              return nullptr;
          }
          Builder->CreateStore(stored, tmp);
          val = tmp;
        }
      }

      if (payloadInfoOpt && i == 0 && payloadLLVMType &&
          val->getType() != payloadLLVMType) {
        if (initializerIsPayloadCall) {
          llvm::Value *wrapped = finalizePayloadValue(val);
          if (!wrapped)
            return nullptr;
          val = wrapped;
        } else {
          // Smart pointer payloads (e.g. weak<T> expecting shared<T>) should
          // simply be coerced, not reconstructed.
          if (payloadInfoOpt->isSmartPointer()) {
            if (payloadLLVMType->isPointerTy()) {
              llvm::Type *expectedPtrTy = payloadLLVMType;
              llvm::Type *pointeeTy = nullptr;
              if (auto *ptr = llvm::dyn_cast<llvm::PointerType>(expectedPtrTy)) {
                if (ptr->getNumContainedTypes() > 0)
                  pointeeTy = ptr->getContainedType(0);
              }

              llvm::Value *coerced = val;
              if (pointeeTy && !val->getType()->isPointerTy()) {
                llvm::AllocaInst *tmp = Builder->CreateAlloca(
                    pointeeTy, nullptr, "smart.payload.tmp");
                llvm::Value *stored = val;
                if (val->getType() != pointeeTy) {
                  stored = castToType(val, pointeeTy,
                                      typeNameFromInfo(*payloadInfoOpt));
                  if (!stored)
                    return nullptr;
                }
                Builder->CreateStore(stored, tmp);
                coerced = tmp;
              }

              coerced = castToType(coerced, expectedPtrTy,
                                   typeNameFromInfo(*payloadInfoOpt));
              if (!coerced)
                return nullptr;
              val = coerced;
            } else {
              val = castToType(val, payloadLLVMType,
                               typeNameFromInfo(*payloadInfoOpt));
              if (!val)
                return nullptr;
            }
          } else {
            // Only try a constructor conversion for composite payloads; primitives
            // can be cast directly by the caller.
            std::string payloadName = typeNameFromInfo(*payloadInfoOpt);
            std::string payloadBase = baseCompositeName(payloadName);
            if (!payloadBase.empty() && lookupCompositeInfo(payloadBase)) {
              std::vector<llvm::Value *> payloadArgs{val};
              std::vector<bool> payloadIsRef{isRef};
              llvm::Value *convertedPayload = emitResolvedCallInternal(
                  payloadName, std::move(payloadArgs), payloadIsRef, nullptr,
                  false, nullptr, &paren);
              if (!convertedPayload)
                return nullptr;

              convertedPayload = finalizePayloadValue(convertedPayload);
              if (!convertedPayload)
                return nullptr;

              val = convertedPayload;
            } else {
              val = castToType(val, payloadLLVMType, payloadName);
              if (!val)
                return nullptr;
            }
          }
        }
      }

      argVals.push_back(val);
    }
  }

  llvm::Value *constructed =
      emitResolvedCallInternal(targetName, std::move(argVals), argIsRef,
                               nullptr, false, nullptr, &paren);
  if (!constructed)
    return nullptr;

  if (targetIsSmartPointer) {
    return constructed;
  }

  llvm::StructType *structTy = nullptr;
  if (auto it = StructTypes.find(constructedKey); it != StructTypes.end())
    structTy = it->second;

  if (!structTy || !instMeta->hasARCHeader || targetIsSmartPointer)
    return constructed;

  llvm::Type *constructedTy = constructed->getType();
  if (constructedTy->isPointerTy())
    return constructed;

  const llvm::DataLayout &DL = TheModule->getDataLayout();
  uint64_t typeSize = DL.getTypeAllocSize(structTy);
  llvm::Value *sizeVal = llvm::ConstantInt::get(getSizeType(), typeSize);

  if (instMeta->descriptorGlobalName.empty())
    return LogErrorV(("Internal error: missing descriptor for '" + baseName +
                      "' while constructing value")
                         .c_str());

  llvm::GlobalVariable *descriptorGV =
      TheModule->getGlobalVariable(instMeta->descriptorGlobalName, true);
  if (!descriptorGV)
    return LogErrorV(("Internal error: descriptor '" +
                      instMeta->descriptorGlobalName +
                      "' missing while constructing '" + baseName + "'")
                         .c_str());

  llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
      descriptorGV, pointerType(getTypeDescriptorType()));

  llvm::Value *rawPtr = Builder->CreateCall(
      getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
      baseName + ".alloc");
  llvm::Value *typedPtr =
      Builder->CreateBitCast(rawPtr, pointerType(structTy),
                             baseName + ".alloc.typed");
  Builder->CreateStore(constructed, typedPtr);
  paren.markTemporary();
  return typedPtr;
}

llvm::Value *emitThisOverrideString(const CompositeTypeInfo &info,
                                    const std::string &baseName,
                                    llvm::Value *instanceVal) {
  if (!info.thisOverride)
    return LogErrorV(("Type '" + baseName +
                      "' does not implement string this() formatter")
                         .c_str());

  llvm::Value *instancePtr = instanceVal;

  auto structIt = StructTypes.find(baseName);
  if (!instanceVal->getType()->isPointerTy()) {
    if (structIt == StructTypes.end())
      return LogErrorV(
          ("Internal error: missing struct type for '" + baseName +
           "' when converting to string")
              .c_str());
    llvm::AllocaInst *tmp =
        Builder->CreateAlloca(structIt->second, nullptr,
                              baseName + ".this.tmp");
    Builder->CreateStore(instanceVal, tmp);
    instancePtr = tmp;
  }

  if (structIt != StructTypes.end()) {
    llvm::Type *expectedPtrTy = pointerType(structIt->second);
    if (instancePtr->getType() != expectedPtrTy) {
      instancePtr =
          Builder->CreateBitCast(instancePtr, expectedPtrTy,
                                 baseName + ".this.cast");
    }
  }

  std::vector<llvm::Value *> args{instancePtr};
  std::vector<bool> argIsRef{true};
  return emitResolvedCallInternal(*info.thisOverride, std::move(args),
                                  argIsRef, nullptr, false, nullptr, nullptr);
}

llvm::Value *ensureStringPointer(llvm::Value *value) {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  if (value->getType() == stringPtrTy)
    return value;
  if (value->getType()->isPointerTy())
    return Builder->CreateBitCast(value, stringPtrTy, "str.cast");
  return value;
}

static llvm::Value *convertValueToString(llvm::Value *value,
                                         const std::string &typeName,
                                         const std::optional<std::string> &formatSpec) {
  const bool isFloatType = value->getType()->isFloatingPointTy() ||
                           typeName == "float" || typeName == "double";
  const bool isDecimalType =
      isDecimalLLVMType(value->getType()) || isDecimalTypeName(typeName);

  if (formatSpec.has_value() && !isFloatType && !isDecimalType) {
    return LogErrorV(
        "Format specifiers are only supported for floating point and decimal interpolation");
  }

  if (typeName == "string") {
    return ensureStringPointer(value);
  }

  std::string compositeName = baseCompositeName(typeName);
  if (!compositeName.empty()) {
    if (const CompositeTypeInfo *info = lookupCompositeInfo(compositeName)) {
      if (!info->thisOverride) {
        return LogErrorV(("Type '" + compositeName +
                          "' cannot be converted to string without a this() "
                          "formatter")
                             .c_str());
      }
      return emitThisOverrideString(*info, compositeName, value);
    }
  }

  if (typeName == "bool" || value->getType()->isIntegerTy(1)) {
    llvm::Value *cond = value;
    if (!cond->getType()->isIntegerTy(1)) {
      cond = Builder->CreateICmpNE(
          cond,
          llvm::ConstantInt::get(cond->getType(), 0),
          "boolcmp");
    }
    llvm::Value *trueStr = emitStringLiteral("true");
    llvm::Value *falseStr = emitStringLiteral("false");
    return Builder->CreateSelect(cond, trueStr, falseStr, "boolstr");
  }

  if (typeName == "char" || typeName == "schar" || typeName == "lchar") {
    llvm::Type *targetType = llvm::Type::getInt32Ty(*TheContext);
    if (!value->getType()->isIntegerTy(32)) {
      if (typeName == "schar")
        value = Builder->CreateSExtOrTrunc(value, targetType, "charext");
      else
        value = Builder->CreateZExtOrTrunc(value, targetType, "charext");
    }
    return Builder->CreateCall(getCharToStringFunction(), {value}, "charstr");
  }

  if (isDecimalType) {
    int precision = 0;
    bool hasPrecision = false;
    if (formatSpec.has_value()) {
      try {
        precision = std::stoi(*formatSpec);
        hasPrecision = true;
      } catch (...) {
        return LogErrorV(
            "Invalid decimal format specifier in interpolated string");
      }
    }

    llvm::Value *precisionVal = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), precision);
    llvm::Value *hasPrecisionVal = llvm::ConstantInt::get(
        llvm::Type::getInt1Ty(*TheContext), hasPrecision ? 1 : 0);
    return Builder->CreateCall(getDecimalToStringFunction(),
                               {value, precisionVal, hasPrecisionVal},
                               "decimalstr");
  }

  if (value->getType()->isFloatingPointTy()) {
    int precision = 0;
    bool hasPrecision = false;
    if (formatSpec.has_value()) {
      try {
        precision = std::stoi(*formatSpec);
        hasPrecision = true;
      } catch (...) {
        return LogErrorV("Invalid floating-point format specifier in interpolated string");
      }
    }

    if (!value->getType()->isDoubleTy()) {
      value = Builder->CreateFPExt(value, llvm::Type::getDoubleTy(*TheContext), "fpexttmp");
    }

    llvm::Value *precisionVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), precision);
    llvm::Value *hasPrecisionVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), hasPrecision ? 1 : 0);
    return Builder->CreateCall(getDoubleToStringFunction(),
                               {value, precisionVal, hasPrecisionVal},
                               "floatstr");
  }

  if (value->getType()->isIntegerTy()) {
    bool isUnsigned = isUnsignedType(typeName);
    if (!isUnsigned && !isSignedType(typeName) && typeName != "") {
      // Unrecognized integer type name; default to signed.
      isUnsigned = false;
    }

    unsigned bitWidth = value->getType()->getIntegerBitWidth();
    llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
    if (bitWidth != 64) {
      if (isUnsigned)
        value = Builder->CreateZExtOrTrunc(value, int64Ty, "zexttmp");
      else
        value = Builder->CreateSExtOrTrunc(value, int64Ty, "sexttmp");
    }

    llvm::Value *isUnsignedVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), isUnsigned ? 1 : 0);
    return Builder->CreateCall(getIntToStringFunction(), {value, isUnsignedVal}, "intstr");
  }

  if (typeName == "string")
    return ensureStringPointer(value);

  return LogErrorV("Unsupported expression type in string interpolation");
}

llvm::Value *InterpolatedStringExprAST::codegen() {
  std::vector<llvm::Value *> segmentValues;
  segmentValues.reserve(Segments.size());

  for (const auto &segment : Segments) {
    if (segment.isLiteral()) {
      segmentValues.push_back(emitStringLiteral(segment.getLiteral()));
      continue;
    }

    ExprAST *expr = segment.getExpression();
    if (!expr)
      return LogErrorV("Invalid expression in interpolated string segment");

    llvm::Value *exprValue = expr->codegen();
    if (!exprValue)
      return nullptr;

    llvm::Value *asString = convertValueToString(exprValue, expr->getTypeName(), segment.getFormatSpec());
    if (!asString)
      return nullptr;

    segmentValues.push_back(asString);
  }

  if (segmentValues.empty()) {
    setTypeName("string");
    markTemporary();
    return emitStringLiteral("");
  }

  if (segmentValues.size() == 1) {
    llvm::Value *single = ensureStringPointer(segmentValues.front());
    setTypeName("string");
    return single;
  }

  llvm::Type *stringPtrTy = getTypeFromString("string");
  unsigned count = static_cast<unsigned>(segmentValues.size());
  llvm::ArrayType *arrayTy = llvm::ArrayType::get(stringPtrTy, count);
  llvm::AllocaInst *arrayAlloca = Builder->CreateAlloca(arrayTy, nullptr, "interpSegments");

  for (unsigned i = 0; i < count; ++i) {
    llvm::Value *indices[] = {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), i)};
    llvm::Value *elementPtr = Builder->CreateInBoundsGEP(arrayTy, arrayAlloca, indices, "interp.segptr");
    llvm::Value *segmentPtr = ensureStringPointer(segmentValues[i]);
    Builder->CreateStore(segmentPtr, elementPtr);
  }

  llvm::Value *arrayPtr = Builder->CreateInBoundsGEP(
      arrayTy, arrayAlloca,
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0)},
      "interp.base");

  llvm::FunctionCallee concatFunc = getConcatStringsFunction();
  llvm::Value *countVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), count);
  llvm::Value *result = Builder->CreateCall(concatFunc, {arrayPtr, countVal}, "interp.result");

  setTypeName("string");
  markTemporary();
  return result;
}

// Generate code for character literals
llvm::Value *CharExprAST::codegen_with_target(llvm::Type *TargetType,
                                              const std::string &TargetTypeName) {
  uint32_t value = getValue();

  auto emitDefault = [&]() -> llvm::Value * {
    if (value > 0xFFFF) {
      setTypeName("lchar");
      return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, value));
    }
    setTypeName("char");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(16, value));
  };

  if (!TargetType || !TargetType->isIntegerTy())
    return emitDefault();

  unsigned bitWidth = TargetType->getIntegerBitWidth();
  if (bitWidth != 8 && bitWidth != 16 && bitWidth != 32)
    return emitDefault();

  if ((bitWidth == 32 && value > 0x10FFFF) ||
      (bitWidth == 16 && value > 0xFFFF) ||
      (bitWidth == 8 && value > 0xFF))
    return emitDefault();

  std::string cleanTarget = sanitizeBaseTypeName(TargetTypeName);
  std::optional<bool> unsignedHint = unsignedHintFromTypeName(cleanTarget);

  if (bitWidth == 8) {
    if (unsignedHint.has_value() && !unsignedHint.value() && value > 0x7F)
      return emitDefault();
  } else if (bitWidth == 16) {
    if (cleanTarget == "short" && value > 0x7FFF)
      return emitDefault();
  } else if (bitWidth == 32) {
    if (unsignedHint.has_value() && !unsignedHint.value() && value > static_cast<uint32_t>(INT32_MAX))
      return emitDefault();
  }

  llvm::APInt literal(bitWidth, value, /*isSigned=*/false);
  llvm::Value *constant = llvm::ConstantInt::get(*TheContext, literal);

  if (!cleanTarget.empty()) {
    setTypeName(cleanTarget);
  } else if (bitWidth == 8) {
    setTypeName(unsignedHint.value_or(true) ? "schar" : "sbyte");
  } else if (bitWidth == 16) {
    setTypeName("char");
  } else {
    setTypeName("lchar");
  }

  return constant;
}

llvm::Value *CharExprAST::codegen() {
  if (getValue() > 0xFFFF)
    return codegen_with_target(llvm::Type::getInt32Ty(*TheContext), "lchar");
  return codegen_with_target(llvm::Type::getInt16Ty(*TheContext), "char");
}

// Forward declaration for castToType
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType);
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType, const std::string& targetTypeName);

llvm::Value *emitIntegerToDecimalValue(llvm::Value *value,
                                       std::string_view sourceTypeName) {
  if (!value || !value->getType()->isIntegerTy())
    return nullptr;

  llvm::Value *asI64 = value;
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  unsigned bits = value->getType()->getIntegerBitWidth();
  bool useUnsigned =
      unsignedHintFromTypeName(sanitizeBaseTypeName(sourceTypeName)).value_or(false);

  if (bits < 64) {
    if (useUnsigned)
      asI64 = Builder->CreateZExt(value, int64Ty, "dec.zext");
    else
      asI64 = Builder->CreateSExt(value, int64Ty, "dec.sext");
  } else if (bits > 64) {
    asI64 = Builder->CreateTrunc(value, int64Ty, "dec.trunc");
  }

  if (useUnsigned)
    return Builder->CreateCall(getDecimalFromU64Function(), {asI64}, "dec.fromu64");
  return Builder->CreateCall(getDecimalFromI64Function(), {asI64}, "dec.fromi64");
}

llvm::Value *emitDecimalToFloatingValue(llvm::Value *value,
                                        llvm::Type *targetType) {
  if (!value || !targetType || !isDecimalLLVMType(value->getType()) ||
      !targetType->isFloatingPointTy())
    return nullptr;

  llvm::Value *asDouble =
      Builder->CreateCall(getDecimalToDoubleFunction(), {value}, "dec.todouble");
  if (targetType->isDoubleTy())
    return asDouble;
  return Builder->CreateFPTrunc(asDouble, targetType, "dec.tofloat");
}

llvm::Value *emitDecimalToIntegerValue(llvm::Value *value,
                                       llvm::Type *targetType,
                                       std::string_view targetTypeName) {
  if (!value || !targetType || !isDecimalLLVMType(value->getType()) ||
      !targetType->isIntegerTy())
    return nullptr;

  bool targetUnsigned =
      unsignedHintFromTypeName(sanitizeBaseTypeName(targetTypeName)).value_or(false);
  llvm::Value *asI64 = nullptr;
  if (targetUnsigned)
    asI64 = Builder->CreateCall(getDecimalToU64Function(), {value}, "dec.tou64");
  else
    asI64 = Builder->CreateCall(getDecimalToI64Function(), {value}, "dec.toi64");

  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  if (targetType == int64Ty)
    return asI64;
  if (targetType->getIntegerBitWidth() < 64)
    return Builder->CreateTrunc(asI64, targetType, "dec.toint.trunc");
  if (targetUnsigned)
    return Builder->CreateZExt(asI64, targetType, "dec.toint.zext");
  return Builder->CreateSExt(asI64, targetType, "dec.toint.sext");
}

llvm::Value *emitFloatingToDecimalValue(llvm::Value *value) {
  if (!value || !value->getType()->isFloatingPointTy())
    return nullptr;

  llvm::Value *asDouble = value;
  if (!value->getType()->isDoubleTy()) {
    asDouble =
        Builder->CreateFPExt(value, llvm::Type::getDoubleTy(*TheContext),
                             "dec.fromfloat");
  }
  return Builder->CreateCall(getDecimalFromDoubleFunction(), {asDouble},
                             "dec.fromdouble");
}

llvm::Value *emitDecimalArithmeticBinary(char op, llvm::Value *lhs,
                                         llvm::Value *rhs) {
  if (!lhs || !rhs)
    return nullptr;

  const bool lhsDecimal = isDecimalLLVMType(lhs->getType());
  const bool rhsDecimal = isDecimalLLVMType(rhs->getType());
  if (!lhsDecimal || !rhsDecimal) {
    if (lhsDecimal || rhsDecimal)
      LogErrorV("Cannot mix decimal with float/double without an explicit cast");
    else
      LogErrorV("Decimal arithmetic requires decimal operands");
    return nullptr;
  }

  switch (op) {
  case '+':
    return Builder->CreateCall(getDecimalAddFunction(), {lhs, rhs}, "dec.add");
  case '-':
    return Builder->CreateCall(getDecimalSubFunction(), {lhs, rhs}, "dec.sub");
  case '*':
    return Builder->CreateCall(getDecimalMulFunction(), {lhs, rhs}, "dec.mul");
  case '/':
    return Builder->CreateCall(getDecimalDivFunction(), {lhs, rhs}, "dec.div");
  case '%':
    return Builder->CreateCall(getDecimalRemFunction(), {lhs, rhs}, "dec.rem");
  default:
    LogErrorV("Unknown decimal arithmetic operator");
    return nullptr;
  }
}

llvm::Value *emitDecimalComparisonI1(std::string_view op, llvm::Value *lhs,
                                     llvm::Value *rhs) {
  if (!lhs || !rhs)
    return nullptr;

  const bool lhsDecimal = isDecimalLLVMType(lhs->getType());
  const bool rhsDecimal = isDecimalLLVMType(rhs->getType());
  if (!lhsDecimal || !rhsDecimal) {
    if (lhsDecimal || rhsDecimal)
      LogErrorV("Cannot mix decimal with float/double without an explicit cast");
    else
      LogErrorV("Decimal comparison requires decimal operands");
    return nullptr;
  }

  llvm::Value *cmpResult =
      Builder->CreateCall(getDecimalCmpFunction(), {lhs, rhs}, "dec.cmp");
  llvm::Value *zero = llvm::ConstantInt::get(cmpResult->getType(), 0);

  if (op == "==")
    return Builder->CreateICmpEQ(cmpResult, zero, "dec.cmpeq");
  if (op == "!=")
    return Builder->CreateICmpNE(cmpResult, zero, "dec.cmpne");
  if (op == "<")
    return Builder->CreateICmpSLT(cmpResult, zero, "dec.cmplt");
  if (op == ">")
    return Builder->CreateICmpSGT(cmpResult, zero, "dec.cmpgt");
  if (op == "<=")
    return Builder->CreateICmpSLE(cmpResult, zero, "dec.cmple");
  if (op == ">=")
    return Builder->CreateICmpSGE(cmpResult, zero, "dec.cmpge");

  LogErrorV("Unknown decimal comparison operator");
  return nullptr;
}

// Helper to check if types are compatible for implicit conversion
// No implicit conversion between different sized integers
bool areTypesCompatible(llvm::Type* type1, llvm::Type* type2) {
  if (type1 == type2)
    return true;

  const bool type1IsDecimal = isDecimalLLVMType(type1);
  const bool type2IsDecimal = isDecimalLLVMType(type2);
  if (type1IsDecimal || type2IsDecimal) {
    if (type1IsDecimal && type2IsDecimal)
      return true;

    if (type1IsDecimal && type2->isIntegerTy() && !type2->isIntegerTy(1))
      return true;
    if (type2IsDecimal && type1->isIntegerTy() && !type1->isIntegerTy(1))
      return true;

    // decimal never mixes implicitly with float/double or bool.
    return false;
  }
  
  if (type1->isIntegerTy() && type2->isIntegerTy()) {
    const bool isBool1 = type1->isIntegerTy(1);
    const bool isBool2 = type2->isIntegerTy(1);
    if (isBool1 || isBool2)
      return isBool1 && isBool2;
    return true;
  }
  
  // Allow implicit conversion between integer and float types
  if ((type1->isIntegerTy() && type2->isFloatingPointTy()) ||
      (type1->isFloatingPointTy() && type2->isIntegerTy()))
    return true;
  
  // Allow implicit conversion between float types
  if (type1->isFloatingPointTy() && type2->isFloatingPointTy())
    return true;
  
  return false;
}

llvm::Value *emitArrayFillValue(const TypeInfo &arrayInfo,
                                const TypeInfo &elementInfo,
                                llvm::Type *elementType,
                                llvm::Value *elementValue,
                                const std::vector<int64_t> &dimensions,
                                std::string_view label) {
  if (!elementType || !elementValue)
    return nullptr;

  unsigned rank =
      arrayInfo.arrayRanks.empty() ? 1 : std::max(1u, arrayInfo.arrayRanks.back());
  if (dimensions.size() != rank) {
    reportCompilerError("Array initializer does not match declared dimensions");
    return nullptr;
  }

  uint64_t totalCount = 1;
  for (int64_t dim : dimensions)
    totalCount *= static_cast<uint64_t>(dim);

  llvm::Value *arrayDataPtr =
      llvm::ConstantPointerNull::get(pointerType(elementType));

  if (totalCount > 0) {
    uint64_t elemSize = getTypeSizeInBytes(elementType);
    if (elemSize == 0)
      return LogErrorV("Unable to determine element size for array initialization");

    llvm::Value *elemSizeVal =
        llvm::ConstantInt::get(getSizeType(), elemSize);
    llvm::Value *lengthSize =
        llvm::ConstantInt::get(getSizeType(), totalCount);
    llvm::Value *descriptorPtr =
        Builder->CreateCall(getHybridArrayDescriptorFunction(), {},
                            buildArcOpLabel(label, "array.fill.descriptor"));
    llvm::Value *releaseFn = selectArrayElementReleaseFunction(
        elementInfo, buildArcOpLabel(label, "array.fill.releasefn"));
    llvm::Value *rawPtr = Builder->CreateCall(
        getHybridAllocArrayFunction(),
        {elemSizeVal, lengthSize, descriptorPtr},
        buildArcOpLabel(label, "array.fill.raw"));

    if (!llvm::isa<llvm::ConstantPointerNull>(releaseFn)) {
      llvm::Value *releaseFnCasted = releaseFn;
      if (releaseFn->getType() != getArrayReleaseCallbackPointerType()) {
        releaseFnCasted = Builder->CreateBitCast(
            releaseFn, getArrayReleaseCallbackPointerType(),
            buildArcOpLabel(label, "array.fill.releasefn.cast"));
      }
      Builder->CreateCall(getHybridArraySetReleaseFunction(),
                          {rawPtr, releaseFnCasted});
    }

    llvm::Value *rawBytePtr = Builder->CreateBitCast(
        rawPtr, pointerType(llvm::Type::getInt8Ty(*TheContext)),
        buildArcOpLabel(label, "array.fill.byteptr"));
    llvm::Value *payloadBytePtr = Builder->CreateInBoundsGEP(
        llvm::Type::getInt8Ty(*TheContext), rawBytePtr,
        llvm::ConstantInt::get(getSizeType(), getArrayPayloadOffsetBytes()),
        buildArcOpLabel(label, "array.fill.payload.byte"));
    arrayDataPtr = Builder->CreateBitCast(
        payloadBytePtr, pointerType(elementType),
        buildArcOpLabel(label, "array.fill.payload"));
  }

  if (totalCount > 0) {
    for (uint64_t idx = 0; idx < totalCount; ++idx) {
      llvm::Value *valueToStore = elementValue;
      if (elementInfo.requiresARC() && !elementInfo.isSmartPointer() && idx > 0) {
        valueToStore = emitArcRetain(
            elementValue, elementInfo,
            buildArcOpLabel(label, "array.fill.retain"));
        if (!valueToStore)
          return nullptr;
      }

      llvm::Value *Idx = llvm::ConstantInt::get(
          llvm::Type::getInt32Ty(*TheContext), idx);
      llvm::Value *ElemPtr = Builder->CreateGEP(
          elementType, arrayDataPtr, Idx,
          buildArcOpLabel(label, "array.fill.elemptr"));
      Builder->CreateStore(valueToStore, ElemPtr);
    }
  }

  llvm::StructType *arrayStructTy = getArrayStructType(elementType, rank);
  llvm::Value *arrayValue = llvm::UndefValue::get(arrayStructTy);

  llvm::Value *opaquePtr =
      Builder->CreateBitCast(arrayDataPtr, pointerType(),
                             buildArcOpLabel(label, "array.fill.ptrcast"));
  arrayValue = Builder->CreateInsertValue(arrayValue, opaquePtr, {0});

  llvm::Value *sizeVal = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(*TheContext), totalCount);
  arrayValue = Builder->CreateInsertValue(arrayValue, sizeVal, {1});

  for (unsigned i = 0; i < rank; ++i) {
    llvm::Value *dimVal = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext),
        dimensions.size() > i ? dimensions[i] : 0);
    arrayValue = Builder->CreateInsertValue(arrayValue, dimVal, {2u, i});
  }

  return arrayValue;
}

// Generate code for array literals
llvm::Value *ArrayExprAST::codegen_with_element_target(llvm::Type *TargetElementType,
                                                       const std::string &TargetElementTypeName,
                                                       const TypeInfo *DeclaredArrayInfo) {
  llvm::Type *ElemType = TargetElementType;
  std::string elementTypeName = TargetElementTypeName;
  TypeInfo elementInfo;
  std::string cleanElementTypeName;
  bool hasElementInfo = false;

  auto refreshElementInfo = [&](const std::string &name) {
    elementTypeName = name;
    elementInfo = makeTypeInfo(elementTypeName);
    if (DeclaredArrayInfo && DeclaredArrayInfo->elementNullable && !elementInfo.isNullable)
      elementInfo.isNullable = true;
    finalizeTypeInfoMetadata(elementInfo);
    elementTypeName = typeNameFromInfo(elementInfo);
    cleanElementTypeName = sanitizeBaseTypeName(elementTypeName);
    if (cleanElementTypeName.empty())
      cleanElementTypeName = elementTypeName;
    hasElementInfo = true;
  };

  if (elementTypeName.empty())
    elementTypeName = getElementType();

  const bool elementTypeLocked =
      TargetElementType != nullptr || !TargetElementTypeName.empty();

  unsigned outerRank = 1;
  bool treatAsMultidimensional = false;
  if (DeclaredArrayInfo && !DeclaredArrayInfo->arrayRanks.empty()) {
    outerRank = DeclaredArrayInfo->arrayRanks.back();
    treatAsMultidimensional = DeclaredArrayInfo->isMultidimensional && outerRank > 1;
    if (DeclaredArrayInfo->arrayDepth > 1)
      treatAsMultidimensional = false; // Mixed jagged/multi not yet supported
  }
  if (outerRank == 0)
    outerRank = 1;

  std::vector<ExprAST*> flatElements;
  std::vector<size_t> dimensionSizes;

  auto ensureDimension = [&](unsigned depth, size_t size) -> bool {
    if (dimensionSizes.size() <= depth)
      dimensionSizes.push_back(size);
    else if (dimensionSizes[depth] != size)
      return false;
    return true;
  };

  if (treatAsMultidimensional) {
    std::string baseElementName;
    if (DeclaredArrayInfo)
      baseElementName = DeclaredArrayInfo->typeName;
    if (baseElementName.empty())
      baseElementName = elementTypeName;

    bool hadNullableSuffix = false;
    if (!baseElementName.empty() && baseElementName.back() == '?') {
      hadNullableSuffix = true;
      baseElementName.pop_back();
    }

    while (isArrayTypeName(baseElementName))
      baseElementName = removeLastArrayGroup(baseElementName);

    if (DeclaredArrayInfo && DeclaredArrayInfo->elementNullable &&
        (baseElementName.empty() || baseElementName.back() != '?'))
      baseElementName += "?";
    else if (hadNullableSuffix)
      baseElementName += "?";

    llvm::Type *ScalarType = getTypeFromString(baseElementName);
    if (!ScalarType)
      return LogErrorV("Unknown element type in multidimensional array literal");

    ElemType = ScalarType;
    refreshElementInfo(baseElementName);

    std::function<bool(const ArrayExprAST*, unsigned)> collect =
        [&](const ArrayExprAST *node, unsigned depth) -> bool {
          size_t count = node->getElements().size();
          if (!ensureDimension(depth, count))
            return false;

          if (depth + 1 == outerRank) {
            for (const auto &Elem : node->getElements()) {
              if (dynamic_cast<ArrayExprAST*>(Elem.get()))
                return false;
              flatElements.push_back(Elem.get());
            }
            return true;
          }

          for (const auto &Elem : node->getElements()) {
            auto *SubArray = dynamic_cast<ArrayExprAST*>(Elem.get());
            if (!SubArray)
              return false;
            if (!collect(SubArray, depth + 1))
              return false;
          }
          return true;
        };

    if (!collect(this, 0))
      return LogErrorV("Multidimensional array initializer must be rectangular");
  } else {
    flatElements.reserve(getElements().size());
    for (const auto &Elem : getElements())
      flatElements.push_back(Elem.get());
    dimensionSizes.push_back(flatElements.size());
  }

  size_t ArraySize = flatElements.size();
  std::vector<llvm::Value *> elementValues(ArraySize, nullptr);

  if (!hasElementInfo && !elementTypeName.empty())
    refreshElementInfo(elementTypeName);

  if (!ElemType && hasElementInfo)
    ElemType = getTypeFromString(elementTypeName);

  auto emitElementValue = [&](ExprAST *expr) -> llvm::Value * {
    if (!expr)
      return nullptr;

    if (hasElementInfo) {
      if (auto *paren = dynamic_cast<ParenExprAST *>(expr)) {
        if (auto constructed = emitTargetTypedConstruction(elementInfo, *paren))
          return constructed;
      }
      if (auto *newExpr = dynamic_cast<NewExprAST *>(expr))
        propagateTypeToNewExpr(newExpr, elementInfo);
    }

    if (auto *Num = dynamic_cast<NumberExprAST *>(expr)) {
      if (ElemType)
        return Num->codegen_with_target(ElemType);
      return Num->codegen();
    }

    if (auto *Char = dynamic_cast<CharExprAST *>(expr)) {
      if (ElemType)
        return Char->codegen_with_target(ElemType, cleanElementTypeName);
      return Char->codegen();
    }

    return expr->codegen();
  };

  if (ArraySize > 0) {
    llvm::Value *firstVal = emitElementValue(flatElements[0]);
    if (!firstVal)
      return nullptr;
    elementValues[0] = firstVal;
    if (!ElemType ||
        (!elementTypeLocked &&
         (firstVal->getType() != ElemType ||
          dynamic_cast<ArrayExprAST *>(flatElements[0])))) {
      ElemType = firstVal->getType();
    }
    if (elementTypeName.empty() && !flatElements[0]->getTypeName().empty())
      elementTypeName = flatElements[0]->getTypeName();
    if (!hasElementInfo && !elementTypeName.empty())
      refreshElementInfo(elementTypeName);
  }

  if (!ElemType) {
    if (ArraySize == 0)
      return LogErrorV("Cannot infer element type for empty array literal",
                       "Specify the element type or add a typed element.");
    return LogErrorV("Cannot infer element type for array literal elements",
                     "Give the literal a declared element type or use typed elements.");
  }

  if (!hasElementInfo && !elementTypeName.empty())
    refreshElementInfo(elementTypeName);

  if (!hasElementInfo)
    return LogErrorV("Cannot determine element type for array literal",
                     "Specify the element type explicitly.");

  const bool elementAllowsNull =
      typeAllowsNull(elementInfo) || elementInfo.elementNullable ||
      (DeclaredArrayInfo && DeclaredArrayInfo->elementNullable);
  if (!elementAllowsNull) {
    std::string reportedElementType =
        elementTypeName.empty() ? cleanElementTypeName : elementTypeName;
    for (const auto *Elem : flatElements) {
      if (expressionIsNullable(unwrapRefExpr(Elem))) {
        return LogErrorV(
            "Array elements of type '" + reportedElementType + "' cannot be null",
            "Remove null entries or mark the element type nullable with '?'.");
      }
    }
  }

  if (dimensionSizes.empty())
    dimensionSizes.push_back(0);
  if (treatAsMultidimensional && dimensionSizes.size() < outerRank)
    dimensionSizes.resize(outerRank, 0);

  llvm::Function *ContainingFunction =
      Builder->GetInsertBlock() ? Builder->GetInsertBlock()->getParent()
                                 : nullptr;

  llvm::Value *ArrayDataPtr =
      llvm::ConstantPointerNull::get(pointerType(ElemType));
  if (ArraySize > 0) {
    uint64_t elemSize = getTypeSizeInBytes(ElemType);
    if (elemSize == 0 && !elementValues.empty() &&
        elementValues[0] && TheModule) {
      const llvm::DataLayout &DL = TheModule->getDataLayout();
      elemSize = DL.getTypeAllocSize(elementValues[0]->getType());
    }
    if (elemSize == 0) {
      reportCompilerError("Unable to determine element size for array literal");
      return nullptr;
    }
    llvm::Value *elemSizeVal =
        llvm::ConstantInt::get(getSizeType(), elemSize);
    llvm::Value *lengthSize = llvm::ConstantInt::get(
        getSizeType(), static_cast<std::uint64_t>(ArraySize));
    llvm::Value *descriptorPtr =
        Builder->CreateCall(getHybridArrayDescriptorFunction(), {},
                            "array.literal.descriptor");
    llvm::Value *releaseFn = selectArrayElementReleaseFunction(
        elementInfo, "array.literal.releasefn");
    llvm::Value *rawPtr = Builder->CreateCall(
        getHybridAllocArrayFunction(),
        {elemSizeVal, lengthSize, descriptorPtr}, "array.literal.raw");

    if (!llvm::isa<llvm::ConstantPointerNull>(releaseFn)) {
      llvm::Value *releaseFnCasted = releaseFn;
      if (releaseFn->getType() != getArrayReleaseCallbackPointerType()) {
        releaseFnCasted = Builder->CreateBitCast(
            releaseFn, getArrayReleaseCallbackPointerType(),
            "array.literal.releasefn.cast");
      }
      Builder->CreateCall(getHybridArraySetReleaseFunction(),
                          {rawPtr, releaseFnCasted});
    }

    llvm::Value *rawBytePtr = Builder->CreateBitCast(
        rawPtr, pointerType(llvm::Type::getInt8Ty(*TheContext)),
        "array.literal.byteptr");
    llvm::Value *payloadBytePtr = Builder->CreateInBoundsGEP(
        llvm::Type::getInt8Ty(*TheContext), rawBytePtr,
        llvm::ConstantInt::get(getSizeType(), getArrayPayloadOffsetBytes()),
        "array.literal.payload.byte");
    ArrayDataPtr = Builder->CreateBitCast(
        payloadBytePtr, pointerType(ElemType), "array.literal.payload");
  }

  // Store each element
  for (size_t i = 0; i < flatElements.size(); ++i) {
    ExprAST *ElementExpr = flatElements[i];
    llvm::Value *ElemVal = elementValues[i];
    if (!ElemVal) {
      ElemVal = emitElementValue(ElementExpr);
      if (!ElemVal)
        return nullptr;
    }

    ElemVal = castToType(ElemVal, ElemType, cleanElementTypeName);

    if (ArraySize > 0) {
      llvm::Value *Idx = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, i));
      llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayDataPtr, Idx, "elemptr");
      Builder->CreateStore(ElemVal, ElemPtr);
    }
  }

  llvm::StructType *ArrayStructType = getArrayStructType(ElemType, outerRank);
  llvm::Value *ArrayValue = llvm::UndefValue::get(ArrayStructType);

  llvm::Value *ArrayPtrForStruct =
      Builder->CreateBitCast(ArrayDataPtr, llvm::PointerType::get(*TheContext, 0), "array.ptrcast");
  ArrayValue = Builder->CreateInsertValue(ArrayValue, ArrayPtrForStruct, {0});

  llvm::Value *SizeVal = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, ArraySize));
  ArrayValue = Builder->CreateInsertValue(ArrayValue, SizeVal, {1});

  unsigned dimsCount = std::max(1u, outerRank);
  if (dimensionSizes.size() < dimsCount)
    dimensionSizes.resize(dimsCount, 0);

  for (unsigned i = 0; i < dimsCount; ++i) {
    llvm::Value *DimVal =
        llvm::ConstantInt::get(*TheContext, llvm::APInt(32, dimensionSizes[i]));
    ArrayValue = Builder->CreateInsertValue(ArrayValue, DimVal, {2u, i});
  }

  llvm::Value *ArrayStructPtr = nullptr;
  if (!ContainingFunction)
    return LogErrorV("array literal requires function context");
  ArrayStructPtr =
      createEntryAlloca(ContainingFunction, ArrayStructType, "arrayStruct");
  Builder->CreateStore(ArrayValue, ArrayStructPtr);

  if (DeclaredArrayInfo) {
    setTypeName(typeNameFromInfo(*DeclaredArrayInfo));
  } else {
    std::string arrayTypeName = cleanElementTypeName;
    if (!arrayTypeName.empty())
      arrayTypeName += "[]";
    setTypeName(arrayTypeName);
  }

  markTemporary();
  return Builder->CreateLoad(ArrayStructType, ArrayStructPtr,
                             "arrayStructVal");
}

llvm::Value *ArrayExprAST::codegen() {
  return codegen_with_element_target(nullptr, getElementType());
}

std::optional<TypeInfo> resolveTupleTypeInfo(const ExprAST *expr) {
  return resolveExprTypeInfo(expr);
}

std::optional<size_t> findTupleElementIndex(const TypeInfo &tupleInfo,
                                            std::string_view name) {
  if (!tupleInfo.isTupleType())
    return std::nullopt;
  if (tupleInfo.tupleElementNames.empty())
    return std::nullopt;

  for (size_t i = 0; i < tupleInfo.tupleElementNames.size(); ++i) {
    if (tupleInfo.tupleElementNames[i] == name)
      return i;
  }
  return std::nullopt;
}

std::optional<ArrayElementAccessInfo>
computeTupleElementAccess(const TypeInfo &tupleInfo,
                          llvm::Value *tupleValue,
                          size_t elementIndex,
                          std::string_view label) {
  if (!tupleValue)
    return std::nullopt;

  TypeInfo boundTuple = applyActiveTypeBindings(tupleInfo);
  finalizeTypeInfoMetadata(boundTuple);

  if (!boundTuple.isTupleType())
    return std::nullopt;

  if (elementIndex >= boundTuple.typeArguments.size()) {
    reportCompilerError("Tuple index out of range");
    return std::nullopt;
  }

  if (!materializeTupleInstantiation(boundTuple))
    return std::nullopt;

  std::string tupleName =
      stripNullableAnnotations(typeNameFromInfo(boundTuple));
  auto structIt = StructTypes.find(tupleName);
  if (structIt == StructTypes.end() || !structIt->second) {
    reportCompilerError("Internal error: missing tuple type '" + tupleName + "'");
    return std::nullopt;
  }

  llvm::StructType *structTy = structIt->second;
  const CompositeTypeInfo *metadata =
      lookupCompositeInfo(tupleName, /*countHit=*/false);
  if (!metadata) {
    reportCompilerError("Internal error: missing tuple metadata for '" +
                        tupleName + "'");
    return std::nullopt;
  }

  llvm::Value *tuplePtr = tupleValue;
  if (!tuplePtr->getType()->isPointerTy()) {
    llvm::AllocaInst *tmp = Builder->CreateAlloca(
        structTy, nullptr, std::string(label) + ".tuple.tmp");
    Builder->CreateStore(tupleValue, tmp);
    tuplePtr = tmp;
  } else if (tuplePtr->getType() != pointerType(structTy)) {
    tuplePtr = Builder->CreateBitCast(tuplePtr, pointerType(structTy),
                                      std::string(label) + ".tuple.cast");
  }

  unsigned fieldIndex = static_cast<unsigned>(elementIndex);
  if (metadata->hasARCHeader)
    fieldIndex += metadata->headerFieldIndex + 1;

  if (fieldIndex >= structTy->getNumElements()) {
    reportCompilerError("Tuple index out of range");
    return std::nullopt;
  }

  ArrayElementAccessInfo access;
  llvm::Value *elemPtr =
      Builder->CreateStructGEP(structTy, tuplePtr, fieldIndex,
                               std::string(label) + ".elem.ptr");
  access.elementPtr = elemPtr;
  access.elementLLVMType = structTy->getElementType(fieldIndex);
  access.elementTypeInfo = boundTuple.typeArguments[elementIndex];
  finalizeTypeInfoMetadata(access.elementTypeInfo);
  access.elementTypeName = typeNameFromInfo(access.elementTypeInfo);
  access.elementNullable = access.elementTypeInfo.isNullable;
  return access;
}

std::optional<ArrayElementAccessInfo>
computeArrayElementAccess(ArrayIndexExprAST *node, llvm::Value *arrayValue) {
  ArrayElementAccessInfo access;

  llvm::Value *ArrayVal = arrayValue ? arrayValue : node->getArray()->codegen();
  if (!ArrayVal)
    return std::nullopt;

  const auto &indexExprs = node->getIndices();
  if (indexExprs.empty()) {
    LogErrorV("Array index requires at least one index expression");
    return std::nullopt;
  }

  std::vector<llvm::Value*> indexValues;
  indexValues.reserve(indexExprs.size());
  for (const auto &IdxExpr : indexExprs) {
    llvm::Value *IdxVal = IdxExpr->codegen();
    if (!IdxVal)
      return std::nullopt;

    if (!IdxVal->getType()->isIntegerTy(32)) {
      if (IdxVal->getType()->isIntegerTy()) {
        IdxVal = Builder->CreateSExtOrTrunc(IdxVal, llvm::Type::getInt32Ty(*TheContext), "array.idx.cast");
      } else if (IdxVal->getType()->isFloatingPointTy()) {
        IdxVal = Builder->CreateFPToSI(IdxVal, llvm::Type::getInt32Ty(*TheContext), "array.idx.cast");
      } else {
        LogErrorV("Array index must be an integer");
        return std::nullopt;
      }
    }

    indexValues.push_back(IdxVal);
  }

  std::string arrayTypeName = node->getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (arrayDesc.isNullable) {
    LogErrorV("Cannot index nullable array without null-safe operator");
    return std::nullopt;
  }

  bool elementIsNullable = arrayDesc.elementNullable;
  unsigned outerRank = arrayDesc.arrayRanks.empty() ? 1 : arrayDesc.arrayRanks.back();
  bool treatAsMultidimensional = outerRank > 1;

  if (treatAsMultidimensional && indexValues.size() != outerRank) {
    LogErrorV("Multidimensional array access requires one index per dimension");
    return std::nullopt;
  }

  std::string elemTypeStr = arrayTypeName;
  for (size_t i = 0; i < indexValues.size(); ++i)
    elemTypeStr = removeLastArrayGroup(elemTypeStr);
  if (elemTypeStr.empty())
    elemTypeStr = "int";

  llvm::Type *ElemType = getTypeFromString(elemTypeStr);
  if (!ElemType)
    ElemType = llvm::Type::getInt32Ty(*TheContext);

  llvm::Type *ArrayType = ArrayVal->getType();
  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *TotalSize = nullptr;
  std::vector<llvm::Value*> dimValues;

  if (ArrayType->isStructTy()) {
    auto *StructType = llvm::cast<llvm::StructType>(ArrayType);
    ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "array.ptr");
    TotalSize = Builder->CreateExtractValue(ArrayVal, 1, "array.size");
    if (StructType->getNumElements() > 2) {
      if (auto *ArrTy = llvm::dyn_cast<llvm::ArrayType>(StructType->getElementType(2))) {
        unsigned dimsCount = ArrTy->getNumElements();
        dimValues.reserve(dimsCount);
        for (unsigned i = 0; i < dimsCount; ++i)
          dimValues.push_back(Builder->CreateExtractValue(ArrayVal, {2u, i}, "array.dim"));
      }
    }
  } else if (ArrayType->isPointerTy()) {
    ArrayPtr = ArrayVal;
  } else {
    LogErrorV("Array indexing requires an array type");
    return std::nullopt;
  }

  if (!ArrayPtr)
    return std::nullopt;

  std::vector<int64_t> knownDims;
  if (auto *VarExpr = dynamic_cast<VariableExprAST*>(node->getArray())) {
    auto it = ArraySizes.find(VarExpr->getName());
    if (it != ArraySizes.end())
      knownDims = it->second;
  }

  for (size_t axis = 0; axis < indexValues.size(); ++axis) {
    if (auto *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(indexValues[axis])) {
      int64_t value = ConstIndex->getSExtValue();
      if (value < 0) {
        LogErrorV("Array index cannot be negative");
        return std::nullopt;
      }

      int64_t axisBound = -1;
      if (axis < knownDims.size())
        axisBound = knownDims[axis];
      else if (axis < dimValues.size()) {
        if (auto *DimConst = llvm::dyn_cast<llvm::ConstantInt>(dimValues[axis]))
          axisBound = DimConst->getSExtValue();
      } else if (axis == 0 && TotalSize) {
        if (auto *SizeConst = llvm::dyn_cast<llvm::ConstantInt>(TotalSize))
          axisBound = SizeConst->getSExtValue();
      }

      if (axisBound >= 0 && value >= axisBound) {
        LogErrorV("Array index out of bounds");
        return std::nullopt;
      }
    }
  }

  llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  llvm::Value *BadIndex = nullptr;
  auto appendAxisCheck = [&](llvm::Value *IdxVal, llvm::Value *UpperBound,
                             const char *negName, const char *outName) {
    llvm::Value *IsNegative = Builder->CreateICmpSLT(IdxVal, Zero, negName);
    llvm::Value *IsOut = Builder->CreateICmpSGE(IdxVal, UpperBound, outName);
    llvm::Value *AxisBad = Builder->CreateOr(IsNegative, IsOut, "array.idx.badaxis");
    BadIndex = BadIndex ? Builder->CreateOr(BadIndex, AxisBad, "array.idx.bad") : AxisBad;
  };

  if (!dimValues.empty()) {
    unsigned dimsUsed = std::min(dimValues.size(), indexValues.size());
    for (unsigned axis = 0; axis < dimsUsed; ++axis)
      appendAxisCheck(indexValues[axis], dimValues[axis], "array.idx.neg", "array.idx.oor");
  } else if (TotalSize && !indexValues.empty()) {
    appendAxisCheck(indexValues[0], TotalSize, "array.idx.neg", "array.idx.oor");
  }

  if (BadIndex) {
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *SafeBB = llvm::BasicBlock::Create(*TheContext, "safe_index", TheFunction);
    llvm::BasicBlock *ErrorBB = llvm::BasicBlock::Create(*TheContext, "bounds_error", TheFunction);

    Builder->CreateCondBr(BadIndex, ErrorBB, SafeBB);

    Builder->SetInsertPoint(ErrorBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(
          llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
        "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(SafeBB);
  }

  llvm::Value *linearIndex = indexValues[0];
  if (indexValues.size() > 1) {
    for (size_t axis = 1; axis < indexValues.size(); ++axis) {
      llvm::Value *DimVal = nullptr;
      if (axis < dimValues.size())
        DimVal = dimValues[axis];
      else if (!dimValues.empty())
        DimVal = dimValues.back();
      else if (TotalSize)
        DimVal = TotalSize;

      if (DimVal)
        linearIndex = Builder->CreateMul(linearIndex, DimVal, "array.idx.mul");

      linearIndex = Builder->CreateAdd(linearIndex, indexValues[axis], "array.idx.add");
    }
  }

  llvm::Value *TypedPtr = ArrayPtr;
  if (!TypedPtr->getType()->isPointerTy()) {
    TypedPtr = Builder->CreateBitCast(ArrayPtr,
                                      llvm::PointerType::get(*TheContext, 0),
                                      "array.ptrcast.elem");
  }

  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, TypedPtr, linearIndex, "array.elemptr");

  access.elementPtr = ElemPtr;
  access.elementLLVMType = ElemType;
  access.elementTypeName = elemTypeStr;
  access.elementNullable = elementIsNullable;
  access.elementTypeInfo = makeTypeInfo(elemTypeStr);
  finalizeTypeInfoMetadata(access.elementTypeInfo);
  return access;
}
