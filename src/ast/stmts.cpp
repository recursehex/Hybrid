#include "ast_internal.h"

//===----------------------------------------------------------------------===//
// Statement Code Generation
//===----------------------------------------------------------------------===//

static bool isParameterName(llvm::Function *fn, const std::string &name) {
  if (!fn)
    return false;
  for (const auto &arg : fn->args()) {
    if (arg.getName() == name)
      return true;
  }
  return false;
}

static bool isLocalReturnReference(const std::string &name,
                                   llvm::Function *currentFunction) {
  if (!NamedValues.contains(name))
    return false;
  if (const auto *entry = lookupLifetimePlanEntry(name)) {
    if (entry->isParameter)
      return false;
  }
  return !isParameterName(currentFunction, name);
}

static std::optional<std::string>
findEscapingLocalRef(ExprAST *expr, llvm::Function *currentFunction) {
  if (!expr)
    return std::nullopt;

  if (auto *varExpr = dynamic_cast<VariableExprAST *>(expr)) {
    if (isLocalReturnReference(varExpr->getName(), currentFunction))
      return varExpr->getName();
    return std::nullopt;
  }

  if (auto *indexExpr = dynamic_cast<ArrayIndexExprAST *>(expr))
    return findEscapingLocalRef(indexExpr->getArray(), currentFunction);

  if (auto *memberExpr = dynamic_cast<MemberAccessExprAST *>(expr))
    return findEscapingLocalRef(memberExpr->getObject(), currentFunction);

  return std::nullopt;
}

static llvm::Value *codegenReturnValue(ExprAST *returnValue, bool isRef) {
  llvm::Function *currentFunction = nullptr;
  if (auto *currentBlock = Builder->GetInsertBlock())
    currentFunction = currentBlock->getParent();

  if (returnValue) {
    llvm::Value *Val;

    // Check if this is a ref return
    if (isRef) {
      if (auto escapingLocal =
              findEscapingLocalRef(returnValue, currentFunction)) {
        reportCompilerError("Cannot return local variable '" + *escapingLocal +
                            "' by reference",
                            "Return a value or a reference to caller-owned "
                            "storage instead");
        return nullptr;
      }

      // For ref return, need to return a pointer
      // Use codegen_ptr if the return value supports it
      if (auto *VarExpr = dynamic_cast<VariableExprAST*>(returnValue)) {
        Val = VarExpr->codegen_ptr();
      }
      else if (auto *ArrayIdxExpr = dynamic_cast<ArrayIndexExprAST*>(returnValue)) {
        Val = ArrayIdxExpr->codegen_ptr();
      }
      else if (auto *MemberExpr = dynamic_cast<MemberAccessExprAST*>(returnValue)) {
        Val = MemberExpr->codegen_ptr();
      }
      else {
        return LogErrorV("return ref can only be used with lvalues (variables, array elements, or struct members)");
      }
    } else {
      llvm::Type *expectedType =
          currentFunction ? currentFunction->getReturnType() : nullptr;
      if (expectedType && !expectedType->isVoidTy() &&
          isDecimalLLVMType(expectedType)) {
        if (auto *num = dynamic_cast<NumberExprAST *>(returnValue))
          Val = num->codegen_with_target(expectedType);
        else
          Val = returnValue->codegen();
      } else {
        Val = returnValue->codegen();
      }
    }

    if (!Val)
      return nullptr;

    if (!isRef) {
      auto maybePromoteStackReturn = [&](llvm::Value *value) -> llvm::Value * {
        if (!value)
          return nullptr;
        auto *allocaPtr = llvm::dyn_cast<llvm::AllocaInst>(value);
        if (!allocaPtr)
          return value;

        std::string retTypeName = returnValue->getTypeName();
        TypeInfo retInfo = makeTypeInfo(retTypeName);
        finalizeTypeInfoMetadata(retInfo);
        if (!retInfo.requiresARC() || retInfo.isSmartPointer())
          return value;

        llvm::StructType *structTy =
            llvm::dyn_cast<llvm::StructType>(allocaPtr->getAllocatedType());
        if (!structTy)
          return value;

        std::string lookupName =
            sanitizeCompositeLookupName(typeNameFromInfo(retInfo));
        const CompositeTypeInfo *meta =
            lookupCompositeInfo(lookupName, /*countHit=*/false);
        if (!meta || !meta->hasARCHeader)
          return value;

        if (meta->descriptorGlobalName.empty()) {
          reportCompilerError("Internal error: missing descriptor for '" +
                              lookupName + "' while returning value");
          return nullptr;
        }

        llvm::GlobalVariable *descriptorGV = TheModule->getGlobalVariable(
            meta->descriptorGlobalName, true);
        if (!descriptorGV) {
          reportCompilerError("Internal error: descriptor '" +
                              meta->descriptorGlobalName +
                              "' missing while returning '" + lookupName + "'");
          return nullptr;
        }

        const llvm::DataLayout &DL = TheModule->getDataLayout();
        uint64_t typeSize = DL.getTypeAllocSize(structTy);
        llvm::Value *sizeVal = llvm::ConstantInt::get(getSizeType(), typeSize);

        llvm::Value *descriptorPtr = llvm::ConstantExpr::getBitCast(
            descriptorGV, pointerType(getTypeDescriptorType()));

        llvm::Value *rawPtr = Builder->CreateCall(
            getHybridAllocObjectFunction(), {sizeVal, descriptorPtr},
            "return.alloc");
        llvm::Value *typedPtr =
            Builder->CreateBitCast(rawPtr, pointerType(structTy),
                                   "return.obj");

        llvm::Value *initValue =
            Builder->CreateLoad(structTy, allocaPtr, "return.init");
        Builder->CreateStore(initValue, typedPtr);
        return typedPtr;
      };

      Val = maybePromoteStackReturn(Val);
      if (!Val)
        return nullptr;

      if (currentFunction) {
        llvm::Type *expectedType = currentFunction->getReturnType();
        if (expectedType && !expectedType->isVoidTy() &&
            Val->getType() != expectedType) {
          Val = castToType(Val, expectedType);
          if (!Val)
            return nullptr;
        }
      }
    }

    emitArcScopeDrainAll("return");
    Builder->CreateRet(Val);
  } else {
    // Void return
    emitArcScopeDrainAll("return");
    Builder->CreateRetVoid();
  }
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

// Generate code for return statements
llvm::Value *ReturnStmtAST::codegen() {
  return codegenReturnValue(getReturnValue(), isRef());
}

// Generate code for block statements
llvm::Value *BlockStmtAST::codegen() {
  ArcScopeGuard arcScope("block");
  llvm::Value *Last = nullptr;
  // Generate code for each statement in the block
  for (auto &Stmt : getStatements()) {
    Last = Stmt->codegen();
    if (!Last)
      return nullptr;
  }
  return Last ? Last : llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

llvm::Value *VariableDeclarationStmtAST::codegen() {
  // Get the type
  TypeInfo declaredInfo = applyActiveTypeBindings(getTypeInfo());

  llvm::Type *VarType = getTypeFromString(declaredInfo.typeName);
  if (!VarType)
    return LogErrorV("Unknown type name");

  // For ref variables, need to store a pointer to the actual type
  bool isRefVar = isRef();

  propagateTypeToNewExpr(getInitializer(), declaredInfo);

  if (declaredInfo.isSmartPointer()) {
    if (auto *hashInit =
            dynamic_cast<UnaryExprAST *>(getInitializer())) {
      if (hashInit->getOp() == "#") {
        if (auto parenInit = convertHashShorthandToParen(*hashInit))
          Initializer = std::move(parenInit);
      }
    }
  }

  const ExprAST *InitializerExpr = getInitializer();
  const RefExprAST *RefInitializer = dynamic_cast<const RefExprAST*>(InitializerExpr);
  const ExprAST *NullableCheckExpr = unwrapRefExpr(InitializerExpr);
  const bool shouldCheckNullability = NullableCheckExpr && !RefInitializer;
  std::string targetDescription = "variable '" + getName() + "'";
  std::vector<int64_t> trackedArraySizes;

  if (InitializerExpr) {
    if (!validateInvariantAssignment(declaredInfo, InitializerExpr,
                                     "initializer for '" + getName() + "'"))
      return nullptr;
    if (!validateTupleAssignmentCompatibility(declaredInfo, InitializerExpr,
                                              "initializer for '" + getName() + "'"))
      return nullptr;
    if (!validateArrayNewBoundsForDeclaration(declaredInfo, InitializerExpr,
                                              getName()))
      return nullptr;
  }

  std::string declaredElementTypeName;
  llvm::Type *declaredElementType = nullptr;
  std::optional<TypeInfo> declaredElementInfo;
  if (declaredInfo.isArray) {
    declaredElementInfo = extractElementTypeInfo(declaredInfo);
    if (declaredElementInfo) {
      declaredElementTypeName = typeNameFromInfo(*declaredElementInfo);
      declaredElementType = getTypeFromString(declaredElementTypeName);
    }
  }

  const std::vector<int64_t> explicitArrayDims = [&]() {
    std::vector<int64_t> dims = parseExplicitArrayDimensions(declaredInfo);
    if (!dims.empty())
      return dims;

    dims = parseExplicitArrayDimensions(getTypeInfo());
    if (!dims.empty())
      return dims;

    TypeInfo declaredSyntaxInfo = makeTypeInfo(getType());
    return parseExplicitArrayDimensions(declaredSyntaxInfo);
  }();

  auto computeLiteralDimensions = [&](const ArrayExprAST *arrayLiteral) -> std::vector<int64_t> {
    std::vector<int64_t> dims;
    if (!arrayLiteral || !declaredInfo.isArray)
      return dims;

    if (declaredInfo.isMultidimensional && declaredInfo.arrayDepth == 1 && !declaredInfo.arrayRanks.empty()) {
      unsigned rank = declaredInfo.arrayRanks.back();
      if (rank == 0)
        rank = 1;

      std::vector<size_t> collected;
      auto ensureDimension = [&](unsigned depth, size_t size) -> bool {
        if (collected.size() <= depth)
          collected.push_back(size);
        else if (collected[depth] != size)
          return false;
        return true;
      };

      std::function<bool(const ArrayExprAST*, unsigned)> collect =
          [&](const ArrayExprAST *node, unsigned depth) -> bool {
            size_t count = node->getElements().size();
            if (!ensureDimension(depth, count))
              return false;

            if (depth + 1 == rank) {
              for (const auto &Elem : node->getElements()) {
                if (dynamic_cast<ArrayExprAST*>(Elem.get()))
                  return false;
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

      if (!collect(arrayLiteral, 0))
        return {};

      if (collected.size() < rank)
        collected.resize(rank, 0);

      dims.reserve(collected.size());
      for (size_t sz : collected)
        dims.push_back(static_cast<int64_t>(sz));
    } else {
      dims.push_back(static_cast<int64_t>(arrayLiteral->getElements().size()));
    }

    return dims;
  };

  auto formatDims = [](const std::vector<int64_t> &dims) {
    if (dims.empty())
      return std::string("unknown");
    std::string formatted;
    for (size_t i = 0; i < dims.size(); ++i) {
      if (i != 0)
        formatted += "x";
      formatted += std::to_string(dims[i]);
    }
    return formatted;
  };

  auto generateInitializerValue = [&](ExprAST *expr) -> llvm::Value * {
    if (!expr)
      return nullptr;

    if (!declaredInfo.isArray && isDecimalLLVMType(VarType)) {
      if (auto *num = dynamic_cast<NumberExprAST *>(expr))
        return num->codegen_with_target(VarType);
    }

    if (auto *paren = dynamic_cast<ParenExprAST *>(expr)) {
      if (declaredInfo.isTupleType() || declaredInfo.isSmartPointer()) {
        if (auto constructed = emitTargetTypedConstruction(declaredInfo, *paren))
          return constructed;
      }
    }
    if (auto *ArrayInit = dynamic_cast<ArrayExprAST*>(expr)) {
      if (declaredInfo.isArray) {
        llvm::Value *value = ArrayInit->codegen_with_element_target(
            declaredElementType, declaredElementTypeName, &declaredInfo);
        if (!value)
          return nullptr;

        auto literalDims = computeLiteralDimensions(ArrayInit);
        if (!explicitArrayDims.empty() && !literalDims.empty() &&
            literalDims != explicitArrayDims) {
          reportCompilerError(
              "Array initializer does not match declared size for '" +
                  getName() + "'",
              "Declared dimensions: " + formatDims(explicitArrayDims) +
                  ", initializer dimensions: " + formatDims(literalDims));
          return nullptr;
        }

        if (!literalDims.empty())
          trackedArraySizes = std::move(literalDims);
        else if (!explicitArrayDims.empty())
          trackedArraySizes = explicitArrayDims;
        return value;
      }
    }

    if (const DelegateTypeInfo *delegateInfo =
            lookupDelegateInfo(declaredInfo)) {
      bool handled = false;
      llvm::Value *delegateValue = emitDelegateValueForTarget(
          *delegateInfo, expr,
          "initializer for '" + getName() + "'", handled);
      if (handled)
        return delegateValue;
    }

    llvm::Value *value = expr->codegen();
    if (!value)
      return nullptr;

    if (declaredInfo.isArray && !explicitArrayDims.empty()) {
      if (value->getType() == VarType) {
        if (trackedArraySizes.empty())
          trackedArraySizes = explicitArrayDims;
        return value;
      }

      TypeInfo elementInfo = declaredElementInfo
                                 ? *declaredElementInfo
                                 : makeTypeInfo(removeLastArrayGroup(
                                       stripNullableAnnotations(
                                           typeNameFromInfo(declaredInfo))));
      if (!declaredElementInfo && declaredInfo.elementNullable &&
          !elementInfo.isNullable)
        elementInfo.isNullable = true;
      finalizeTypeInfoMetadata(elementInfo);
      std::string elementName = typeNameFromInfo(elementInfo);

      if (!typeAllowsNull(elementInfo) &&
          expressionIsNullable(unwrapRefExpr(expr))) {
        reportCompilerError(
            "Array elements of type '" + elementName + "' cannot be null",
            "Make the element type nullable with '?' or use a non-null value.");
        return nullptr;
      }

      llvm::Type *elementType =
          declaredElementType ? declaredElementType : getTypeFromString(elementName);
      if (!elementType)
        return LogErrorV("Unknown element type in array initializer");

      if (diagnoseDisallowedImplicitIntegerConversion(
              expr, value, elementType, elementName,
              "initializer for '" + getName() + "'"))
        return nullptr;
      llvm::Value *castValue =
          castToType(value, elementType, elementName);
      if (!castValue)
        return nullptr;

      llvm::Value *filled = emitArrayFillValue(declaredInfo, elementInfo,
                                               elementType, castValue,
                                               explicitArrayDims, getName());
      if (filled)
        trackedArraySizes = explicitArrayDims;
      return filled;
    }

    return value;
  };

  // Check if at global scope
  bool isGlobal = builderInTopLevelContext();
  if (isGlobal)
    prepareTopLevelStatementContext();

  if (isGlobal) {
    if (GlobalValues.count(getName()) || lookupGlobalTypeInfo(getName())) {
      return LogErrorV(("Variable '" + getName() + "' is already declared").c_str());
    }
  } else {
    if (NamedValues.count(getName()) || lookupLocalTypeInfo(getName())) {
      return LogErrorV(("Variable '" + getName() + "' is already declared in this scope").c_str());
    }
  }

  if (isGlobal) {
    if (isRefVar) {
      // Check if initializer is a RefExprAST (linking to another variable)
      bool initializerIsRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      // Check if initializer is a variable reference to another ref variable
      bool shouldLink = initializerIsRef;
      if (!shouldLink && dynamic_cast<VariableExprAST*>(getInitializer())) {
        VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
        std::string refVarName = VarInit->getName();
        if (isDeclaredRefGlobal(refVarName))
          shouldLink = true;
      }

      if (shouldLink) {
        // This is linking to another variable: ref int b = a or ref int b = ref a
        // Create a pointer variable
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, AliasPtrType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize to null pointer
        GV->setInitializer(llvm::ConstantPointerNull::get(AliasPtrType));

        // Generate the initializer (should return a pointer)
        llvm::Value *InitVal;
        if (initializerIsRef) {
          InitVal = getInitializer()->codegen();
        } else {
          // Get the address of the variable
          VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
          InitVal = VarInit->codegen_ptr();
        }
        if (!InitVal)
          return nullptr;

        // Store the pointer
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (stores the pointer global)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, true));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, GV, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // This is a regular initialization: ref int a = 1
        // Create a regular variable, but mark it as ref so others can reference it
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, VarType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize with zero
        llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
        GV->setInitializer(ZeroInit);

        // Generate the initializer value
        llvm::Value *InitVal = generateInitializerValue(getInitializer());
        if (!InitVal)
          return nullptr;

        if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
          return nullptr;

        // Cast and store the value
        if (diagnoseDisallowedImplicitIntegerConversion(getInitializer(), InitVal, VarType, getType(), "initializer for '" + getName() + "'"))
          return nullptr;
        InitVal = castToType(InitVal, VarType, getType());
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (as a regular variable)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefValue, true));

        // Return the value
        return Builder->CreateLoad(VarType, GV, getName());
      }
    } else {
      // Regular global variable
      // Check if initializer has explicit ref (e.g. int x = ref y)
      bool initializerHasRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      if (initializerHasRef) {
        // Create a pointer variable because of explicit ref
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, AliasPtrType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize to null pointer
        GV->setInitializer(llvm::ConstantPointerNull::get(AliasPtrType));

        // Generate the initializer (should return a pointer)
        llvm::Value *InitVal = generateInitializerValue(getInitializer());
        if (!InitVal)
          return nullptr;

        // Store the pointer
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (stores as a pointer)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, false));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, GV, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // Regular variable without ref
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, VarType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Generate the initializer
      if (getInitializer()) {
        // For globals, need constant initializers
        // For now, create a zero initializer and store the actual value
        llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
        GV->setInitializer(ZeroInit);

        // Generate code to store the actual initial value
        llvm::Value *InitVal = generateInitializerValue(getInitializer());
        if (!InitVal)
          return nullptr;

        if (const CompositeTypeInfo *composite =
                lookupCompositeInfo(declaredInfo.typeName)) {
          if (composite->kind == AggregateKind::Class &&
              InitVal->getType()->isPointerTy()) {
            auto structIt = StructTypes.find(declaredInfo.typeName);
            if (structIt == StructTypes.end()) {
              reportCompilerError("Unknown class type '" + declaredInfo.typeName +
                                  "' during global initialization");
              return nullptr;
            }

            llvm::StructType *StructTy = structIt->second;
            std::string instanceGlobalName = getName() + ".instance";
            auto *InstanceGV = new llvm::GlobalVariable(
                *TheModule, StructTy, false, llvm::GlobalValue::InternalLinkage,
                llvm::ConstantAggregateZero::get(StructTy), instanceGlobalName);

            llvm::Value *LoadedValue =
                Builder->CreateLoad(StructTy, InitVal, instanceGlobalName + ".value");
            Builder->CreateStore(LoadedValue, InstanceGV);
            InitVal = InstanceGV;
          }
        }

        if (shouldCheckNullability &&
            !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription)) {
          return nullptr;
        }

        // Cast the initializer to the variable type if needed
        if (diagnoseDisallowedImplicitIntegerConversion(getInitializer(), InitVal, VarType, getType(), "initializer for '" + getName() + "'")) {
          return nullptr;
        }

        InitVal = castToType(InitVal, VarType, getType());

        // Store the initial value
        Builder->CreateStore(InitVal, GV);
      } else {
        // Zero initialize
        llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
        GV->setInitializer(ZeroInit);
      }

        // Remember this global binding
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), declaredInfo);
      }
    }

    // Track array size for compile-time bounds checking
    if (!isRefVar && declaredInfo.isArray) {
      if (trackedArraySizes.empty()) {
        if (auto *ArrayInit = dynamic_cast<ArrayExprAST*>(getInitializer())) {
          auto dims = computeLiteralDimensions(ArrayInit);
          if (!dims.empty())
            trackedArraySizes = std::move(dims);
        }
      }
      if (!trackedArraySizes.empty())
        ArraySizes[getName()] = trackedArraySizes;
    }

    // Return the value that was stored
    if (isRefVar) {
      // For ref variables, already returned above
      return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
    } else {
      // For regular variables, check if it's a refptr
      if (const TypeInfo *info = lookupGlobalTypeInfo(getName()); info && info->isAlias()) {
        // Already returned above in the initializerHasRef branch
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
      } else {
        // Regular variable - get the GV from GlobalValues
        llvm::GlobalVariable *GV = GlobalValues[getName()];
        return Builder->CreateLoad(VarType, GV, getName());
      }
    }
  } else {
    // Local variable - use alloca
    if (isRefVar) {
      // Check if initializer is a RefExprAST (linking to another variable)
      bool initializerIsRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      // Check if initializer is a variable reference to another ref variable
      bool shouldLink = initializerIsRef;
      if (!shouldLink && dynamic_cast<VariableExprAST*>(getInitializer())) {
        VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
        std::string refVarName = VarInit->getName();
        if (isDeclaredRefLocal(refVarName) || isDeclaredRefGlobal(refVarName))
          shouldLink = true;
      }

      if (shouldLink) {
        // This is linking to another variable: ref int b = a or ref int b = ref a
        // Generate the initializer first (should return a pointer)
        llvm::Value *InitVal;
        if (initializerIsRef) {
          InitVal = getInitializer()->codegen();
        } else {
          // Get the address of the variable
          VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
          InitVal = VarInit->codegen_ptr();
        }
        if (!InitVal)
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(AliasPtrType, nullptr, getName());

        // Store the pointer in the alloca
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (stores the pointer alloca)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, true));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, Alloca, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // This is a regular initialization: ref int a = 1
        // Generate the initializer value first
        llvm::Value *InitVal = getInitializer()->codegen();
        if (!InitVal)
          return nullptr;

        if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(VarType, nullptr, getName());

        // Cast and store the value
        if (diagnoseDisallowedImplicitIntegerConversion(getInitializer(), InitVal, VarType, getType(), "initializer for '" + getName() + "'"))
          return nullptr;
        InitVal = castToType(InitVal, VarType, getType());
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (as a regular variable)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefValue, true));

        // Return the value
        return Builder->CreateLoad(VarType, Alloca, getName());
      }
    } else {
      // Regular variable
      // Check if initializer has explicit ref (e.g. int x = ref y)
      bool initializerHasRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      if (initializerHasRef) {
        // Generate the initializer first (should return a pointer)
        llvm::Value *InitVal = getInitializer()->codegen();
        if (!InitVal)
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(AliasPtrType, nullptr, getName());

        // Store the pointer in the alloca
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (stores as a pointer)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, false));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, Alloca, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // Regular variable without ref
        llvm::AllocaInst *Alloca =
            Builder->CreateAlloca(VarType, nullptr, getName());
        if (declaredInfo.isSmartPointer() || declaredInfo.requiresARC()) {
          llvm::Constant *zeroInit = llvm::Constant::getNullValue(VarType);
          Builder->CreateStore(zeroInit, Alloca);
        }
        bool nullabilityChecked = false;
        auto ensureNullability = [&]() -> bool {
          if (!shouldCheckNullability || nullabilityChecked)
            return true;
          nullabilityChecked = true;
          if (!validateNullableAssignment(declaredInfo, NullableCheckExpr,
                                          targetDescription))
            return false;
          return true;
        };

        bool initializedBySmartPointerHelper = false;
        if (getInitializer() && declaredInfo.isSmartPointer()) {
          if (auto *varInit =
                  dynamic_cast<VariableExprAST *>(getInitializer())) {
            if (!ensureNullability())
              return nullptr;
            initializedBySmartPointerHelper =
                emitSmartPointerInitFromVariable(declaredInfo, Alloca, *varInit,
                                                 getName());
          }
        }

        llvm::Value *InitVal = nullptr;
        if (!initializedBySmartPointerHelper && getInitializer()) {
          if (!ensureNullability())
            return nullptr;
          InitVal = generateInitializerValue(getInitializer());
          if (!InitVal)
            return nullptr;
        }

        // Cast and store the initial value if present
        if (InitVal) {
          if (declaredInfo.isSmartPointer()) {
            const CompositeTypeInfo *metadata =
                resolveSmartPointerMetadata(declaredInfo);
            if (!metadata) {
              reportCompilerError("Unable to materialize smart pointer '" +
                                  typeNameFromInfo(declaredInfo) + "' for '" +
                                  getName() + "'");
              return nullptr;
            }
            auto structIt =
                StructTypes.find(stripNullableAnnotations(
                    typeNameFromInfo(declaredInfo)));
            llvm::StructType *structTy =
                structIt != StructTypes.end() ? structIt->second : nullptr;
            if (!structTy) {
              reportCompilerError("Internal error: missing struct type for '" +
                                  typeNameFromInfo(declaredInfo) + "'");
              return nullptr;
            }

            llvm::Value *stored = InitVal;
            if (stored->getType()->isPointerTy()) {
              stored = Builder->CreateLoad(
                  structTy, stored,
                  buildArcOpLabel(getName(), "smart.init.load"));
            }
            if (stored->getType() != structTy) {
              reportCompilerError(
                  "Initializer for '" + getName() +
                  "' has incompatible smart pointer representation");
              return nullptr;
            }

            Builder->CreateStore(stored, Alloca);
          } else {
            if (diagnoseDisallowedImplicitIntegerConversion(
                    getInitializer(), InitVal, VarType, getType(),
                    "initializer for '" + getName() + "'"))
              return nullptr;
            InitVal = castToType(InitVal, VarType, getType());
            if (declaredInfo.requiresARC()) {
              const bool initializerIsTemporary =
                  getInitializer() && getInitializer()->isTemporary();
              if (initializerIsTemporary) {
                Builder->CreateStore(InitVal, Alloca);
              } else {
                emitManagedStore(Alloca, InitVal, declaredInfo, getName(),
                                 initializerIsTemporary);
              }
            } else {
              Builder->CreateStore(InitVal, Alloca);
            }
          }
        }

        // Remember this local binding
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), declaredInfo);
        registerArcLocal(getName(), Alloca, declaredInfo, false);

        // Track array size for compile-time bounds checking
        if (declaredInfo.isArray) {
          if (trackedArraySizes.empty()) {
            if (auto *ArrayInit =
                    dynamic_cast<ArrayExprAST *>(getInitializer())) {
              auto dims = computeLiteralDimensions(ArrayInit);
              if (!dims.empty())
                trackedArraySizes = std::move(dims);
            }
          }
          if (!trackedArraySizes.empty())
            ArraySizes[getName()] = trackedArraySizes;
        }

        // Return the value that was stored
        return Builder->CreateLoad(VarType, Alloca, getName());
      }
    }
  }
}

// Generate code for expression statements
static std::string describeCallTarget(const CallExprAST &call) {
  if (!call.getCallee().empty())
    return call.getCallee();
  if (call.hasCalleeExpr()) {
    if (auto *member =
            dynamic_cast<MemberAccessExprAST *>(call.getCalleeExpr()))
      return member->getMemberName();
  }
  return {};
}

llvm::Value *ExpressionStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Generate code for the expression
  llvm::Value *V = getExpression()->codegen();
  if (!V)
    return nullptr;

  if (auto *call = dynamic_cast<CallExprAST *>(getExpression())) {
    if (call->isBaseConstructorCall()) {
      return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
    }
    if (!V->getType()->isVoidTy()) {
      const std::string target = describeCallTarget(*call);
      const std::string message =
          target.empty() ? "Result of function call is unused"
                         : "Result of call to '" + target + "' is unused";
      reportCompilerWarning(
          message,
          "Assign the return value to a variable if you meant to use it.");
    }
  }

  // For void expressions (like assignments), return a dummy value
  if (V->getType()->isVoidTy()) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
  }

  return V;
}

llvm::Value *AccessorBodyStmtAST::codegen() {
  if (!Accessor)
    return LogErrorV("Internal error: missing accessor body");

  if (Accessor->hasBlockBody())
    return Accessor->getBlockBody()->codegen();

  if (Accessor->hasExpressionBody()) {
    if (Accessor->getKind() == AccessorKind::Get)
      return codegenReturnValue(Accessor->getExpressionBody(), false);
    return Accessor->getExpressionBody()->codegen();
  }

  if (!Accessor->isImplicit())
    return LogErrorV("Internal error: accessor body is missing");

  if (PropertyName == "this") {
    reportCompilerError("Indexer accessors must declare a body");
    return nullptr;
  }

  std::unique_ptr<ExprAST> objectExpr;
  if (IsStatic)
    objectExpr = std::make_unique<VariableExprAST>(OwnerName);
  else
    objectExpr = std::make_unique<ThisExprAST>();

  if (Accessor->getKind() == AccessorKind::Get) {
    MemberAccessExprAST member(std::move(objectExpr), PropertyName);
    return codegenReturnValue(&member, false);
  }

  auto lhs = std::make_unique<MemberAccessExprAST>(std::move(objectExpr),
                                                   PropertyName);
  auto rhs = std::make_unique<VariableExprAST>("value");
  BinaryExprAST assign("=", std::move(lhs), std::move(rhs));
  return assign.codegen();
}

// Generate code for for each statements
llvm::Value *ForEachStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Get the element type from the foreach declaration
  const std::string &TypeName = getTypeName();
  llvm::Type *ElemType = getTypeFromString(TypeName);
  if (!ElemType)
    return LogErrorV("Unknown element type in foreach loop");

  llvm::Type *SourceElemType = ElemType;
  bool DeclaredRef = isRef();

  // Get the collection (array) to iterate over
  llvm::Value *CollectionVal = nullptr;
  if (auto *ArrayLiteral = dynamic_cast<ArrayExprAST*>(Collection.get())) {
    CollectionVal = ArrayLiteral->codegen_with_element_target(ElemType, TypeName);
  } else {
    CollectionVal = Collection->codegen();
    if (!CollectionVal)
      return nullptr;

    std::string collectionTypeName = Collection->getTypeName();
    if (!collectionTypeName.empty()) {
      ParsedTypeDescriptor desc = parseTypeString(collectionTypeName);
      if (desc.isArray) {
        std::string sanitized = desc.sanitized;
        if (isArrayTypeName(sanitized)) {
          std::string baseTypeName = removeLastArrayGroup(sanitized);
          if (llvm::Type *InferredElemType = getTypeFromString(baseTypeName))
            SourceElemType = InferredElemType;
        }
      }
    }
  }
  if (!CollectionVal)
    return nullptr;
  
  // Extract array pointer and size from the collection value
  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  
  // Check if CollectionVal is a struct
  if (CollectionVal->getType()->isStructTy()) {
    llvm::StructType *StructType = llvm::cast<llvm::StructType>(CollectionVal->getType());
    if (StructType->getNumElements() >= 2) {
      // Extract pointer (field 0) and size (field 1)
      ArrayPtr = Builder->CreateExtractValue(CollectionVal, 0, "arrayPtr");
      ArraySize = Builder->CreateExtractValue(CollectionVal, 1, "arraySize");
    } else {
      return LogErrorV("Invalid array struct format");
    }
  } else if (CollectionVal->getType()->isPointerTy()) {
    // Old array representation: direct pointer
    ArrayPtr = CollectionVal;
    
    // Try to determine size statically
    if (ArrayExprAST *ArrayExpr = dynamic_cast<ArrayExprAST*>(Collection.get())) {
      size_t StaticSize = ArrayExpr->getElements().size();
      ArraySize = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, StaticSize));
    } else {
      // For variables without size info, use a default
      ArraySize = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 10));
    }
  } else {
    return LogErrorV("foreach loops require array expressions");
  }
  
  if (!ArrayPtr || !ArraySize)
    return LogErrorV("Failed to extract array information for foreach loop");
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop
  llvm::BasicBlock *InitBB = Builder->GetInsertBlock();
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "forcond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "forbody");
  llvm::BasicBlock *IncBB = llvm::BasicBlock::Create(*TheContext, "forinc");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "forcont");
  
  // Create and initialize the loop counter
  llvm::AllocaInst *CounterAlloca = Builder->CreateAlloca(
      llvm::Type::getInt32Ty(*TheContext), nullptr, "loopcounter");
  Builder->CreateStore(llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)), CounterAlloca);
  
  // Create an alloca for the loop variable
  llvm::AllocaInst *VarAlloca = nullptr;
  if (DeclaredRef) {
    llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
    VarAlloca = Builder->CreateAlloca(AliasPtrType, nullptr, VarName);
  } else {
    VarAlloca = Builder->CreateAlloca(ElemType, nullptr, VarName);
  }
  
  // Jump to the condition check
  Builder->CreateBr(CondBB);
  
  // Emit the condition check
  Builder->SetInsertPoint(CondBB);
  llvm::Value *CounterVal = Builder->CreateLoad(
      llvm::Type::getInt32Ty(*TheContext), CounterAlloca, "counter");
  llvm::Value *CondV = Builder->CreateICmpSLT(CounterVal, ArraySize, "loopcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  
  // Load or bind the current array element and store it in the loop variable
  llvm::Value *ElemPtr = Builder->CreateGEP(SourceElemType, ArrayPtr, CounterVal, "elemptr");
  if (DeclaredRef) {
    if (SourceElemType != ElemType)
      return LogErrorV("ref foreach variable type must match collection element type");

    llvm::Value *PtrForStore = ElemPtr;
    if (PtrForStore->getType() != VarAlloca->getAllocatedType())
      PtrForStore = Builder->CreateBitCast(PtrForStore, VarAlloca->getAllocatedType(), "elemref.ptrcast");
    Builder->CreateStore(PtrForStore, VarAlloca);
  } else {
    llvm::Value *ElemVal = Builder->CreateLoad(SourceElemType, ElemPtr, "elemval");

    llvm::Value *StoreVal = ElemVal;
    if (ElemVal->getType() != ElemType) {
      StoreVal = castToType(ElemVal, ElemType, TypeName);
      if (!StoreVal)
        return nullptr;
    }

    Builder->CreateStore(StoreVal, VarAlloca);
  }
  
  // Add the loop variable to the symbol table
  llvm::Value *OldVal = NamedValues[VarName];
  NamedValues[VarName] = VarAlloca;

  std::optional<TypeInfo> OldTypeInfo;
  if (const TypeInfo *ExistingInfo = lookupLocalTypeInfo(VarName))
    OldTypeInfo = *ExistingInfo;

  if (DeclaredRef) {
    rememberLocalType(VarName, runtimeTypeFrom(getTypeInfo(), RefStorageClass::RefAlias, true));
  } else {
    TypeInfo loopVarInfo = getTypeInfo();
    rememberLocalType(VarName, loopVarInfo);
    registerArcLocal(VarName, VarAlloca, loopVarInfo, false);
  }
  
  // Push the exit and continue blocks for break/skip statements
  LoopExitBlocks.push_back(AfterBB);
  LoopContinueBlocks.push_back(IncBB);
  
  // Generate code for the body
  llvm::Value *BodyV = Body->codegen();
  
  // Pop the exit and continue blocks
  LoopExitBlocks.pop_back();
  LoopContinueBlocks.pop_back();
  
  if (!BodyV) {
    // Restore the old value
    if (OldVal)
      NamedValues[VarName] = OldVal;
    else
      NamedValues.erase(VarName);
    if (OldTypeInfo)
      rememberLocalType(VarName, *OldTypeInfo);
    else
      LocalTypes.erase(VarName);
    return nullptr;
  }
  
  // Branch to increment block (unless body contains break/return)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(IncBB);
  
  // Emit the increment block
  IncBB->insertInto(TheFunction);
  Builder->SetInsertPoint(IncBB);
  
  // Increment the counter
  llvm::Value *NextCounter = Builder->CreateAdd(CounterVal, 
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 1)), "nextcounter");
  Builder->CreateStore(NextCounter, CounterAlloca);
  Builder->CreateBr(CondBB);
  
  // Emit the after-loop block
  AfterBB->insertInto(TheFunction);
  Builder->SetInsertPoint(AfterBB);
  
  // Restore the old value
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);
  if (OldTypeInfo)
    rememberLocalType(VarName, *OldTypeInfo);
  else
    LocalTypes.erase(VarName);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

static llvm::Value *coerceLoopConditionToBool(llvm::Value *CondV,
                                              const char *label) {
  if (!CondV)
    return nullptr;

  llvm::Type *CondType = CondV->getType();
  if (CondType->isIntegerTy(1))
    return CondV;

  if (CondType->isIntegerTy()) {
    return Builder->CreateICmpNE(
        CondV, llvm::ConstantInt::get(CondType, 0), label);
  }

  if (CondType->isFloatingPointTy()) {
    return Builder->CreateFCmpONE(
        CondV, llvm::ConstantFP::get(CondType, 0.0), label);
  }

  if (CondType->isPointerTy()) {
    return Builder->CreateICmpNE(
        CondV,
        llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(CondType)),
        label);
  }

  return LogErrorV(
      "For loop condition must evaluate to a numeric, boolean, or pointer type");
}

static const ExprAST *stripLoopConditionWrappers(const ExprAST *expr) {
  const ExprAST *current = expr;
  while (current) {
    if (const auto *Ref = dynamic_cast<const RefExprAST *>(current)) {
      current = Ref->getOperand();
      continue;
    }
    if (const auto *Cast = dynamic_cast<const CastExprAST *>(current)) {
      current = Cast->getOperand();
      continue;
    }
    if (const auto *Paren = dynamic_cast<const ParenExprAST *>(current)) {
      if (!Paren->isTuple() && Paren->size() == 1) {
        current = Paren->getElement(0);
        continue;
      }
    }
    break;
  }
  return current;
}

static bool conditionUsesLoopVariable(const ExprAST *expr,
                                      const std::string &varName) {
  if (!expr)
    return false;

  expr = stripLoopConditionWrappers(expr);
  if (!expr)
    return false;

  if (const auto *Var = dynamic_cast<const VariableExprAST *>(expr))
    return Var->getName() == varName;

  if (const auto *Binary = dynamic_cast<const BinaryExprAST *>(expr))
    return conditionUsesLoopVariable(Binary->getLHS(), varName) ||
           conditionUsesLoopVariable(Binary->getRHS(), varName);

  if (const auto *Unary = dynamic_cast<const UnaryExprAST *>(expr))
    return conditionUsesLoopVariable(Unary->getOperand(), varName);

  if (const auto *Call = dynamic_cast<const CallExprAST *>(expr)) {
    if (Call->hasCalleeExpr() &&
        conditionUsesLoopVariable(Call->getCalleeExpr(), varName))
      return true;
    for (const auto &Arg : Call->getArgs()) {
      if (conditionUsesLoopVariable(Arg.get(), varName))
        return true;
    }
    return false;
  }

  if (const auto *ArrayAccess = dynamic_cast<const ArrayIndexExprAST *>(expr)) {
    if (conditionUsesLoopVariable(ArrayAccess->getArray(), varName))
      return true;
    for (size_t i = 0; i < ArrayAccess->getIndexCount(); ++i) {
      if (conditionUsesLoopVariable(ArrayAccess->getIndex(i), varName))
        return true;
    }
    return false;
  }

  if (const auto *NullSafeElem =
          dynamic_cast<const NullSafeElementAccessExprAST *>(expr)) {
    return conditionUsesLoopVariable(NullSafeElem->getArray(), varName) ||
           conditionUsesLoopVariable(NullSafeElem->getIndex(), varName);
  }

  if (const auto *Member = dynamic_cast<const MemberAccessExprAST *>(expr))
    return conditionUsesLoopVariable(Member->getObject(), varName);

  if (const auto *NullSafeMember =
          dynamic_cast<const NullSafeAccessExprAST *>(expr))
    return conditionUsesLoopVariable(NullSafeMember->getObject(), varName);

  if (const auto *Retain = dynamic_cast<const RetainExprAST *>(expr))
    return conditionUsesLoopVariable(Retain->getOperand(), varName);

  if (const auto *Release = dynamic_cast<const ReleaseExprAST *>(expr))
    return conditionUsesLoopVariable(Release->getOperand(), varName);

  if (const auto *Cast = dynamic_cast<const CastExprAST *>(expr))
    return conditionUsesLoopVariable(Cast->getOperand(), varName);

  if (const auto *Ref = dynamic_cast<const RefExprAST *>(expr))
    return conditionUsesLoopVariable(Ref->getOperand(), varName);

  if (const auto *Paren = dynamic_cast<const ParenExprAST *>(expr)) {
    for (size_t i = 0; i < Paren->size(); ++i) {
      if (conditionUsesLoopVariable(Paren->getElement(i), varName))
        return true;
    }
    return false;
  }

  return false;
}

static std::optional<bool>
inferLoopDirectionFromCondition(const ExprAST *expr,
                                const std::string &varName) {
  expr = stripLoopConditionWrappers(expr);
  if (!expr)
    return std::nullopt;

  if (const auto *Binary = dynamic_cast<const BinaryExprAST *>(expr)) {
    const std::string &Op = Binary->getOp();
    if (Op == "&&" || Op == "||") {
      if (auto dir = inferLoopDirectionFromCondition(Binary->getLHS(), varName))
        return dir;
      return inferLoopDirectionFromCondition(Binary->getRHS(), varName);
    }

    if (Op == "<" || Op == "<=" || Op == ">" || Op == ">=") {
      const ExprAST *lhs = stripLoopConditionWrappers(Binary->getLHS());
      const ExprAST *rhs = stripLoopConditionWrappers(Binary->getRHS());
      bool lhsIsVar = false;
      bool rhsIsVar = false;
      if (const auto *Var = dynamic_cast<const VariableExprAST *>(lhs))
        lhsIsVar = Var->getName() == varName;
      if (const auto *Var = dynamic_cast<const VariableExprAST *>(rhs))
        rhsIsVar = Var->getName() == varName;

      if (lhsIsVar == rhsIsVar)
        return std::nullopt;

      const bool opIndicatesAscending = (Op == "<" || Op == "<=");
      if (lhsIsVar)
        return opIndicatesAscending;
      return !opIndicatesAscending;
    }
  } else if (const auto *Unary = dynamic_cast<const UnaryExprAST *>(expr)) {
    if (Unary->getOp() == "!")
      return inferLoopDirectionFromCondition(Unary->getOperand(), varName);
  }

  return std::nullopt;
}

// Generate code for for loops with 'to' syntax
llvm::Value *ForLoopStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Evaluate the initial value
  llvm::Value *InitVal = InitExpr->codegen();
  if (!InitVal)
    return nullptr;
  
  // Evaluate limit if provided (may be null if using condition)
  llvm::Value *LimitVal = nullptr;
  if (LimitExpr) {
    LimitVal = LimitExpr->codegen();
    if (!LimitVal)
      return nullptr;
  }
  
  // Get the loop variable type
  llvm::Type *VarType = getTypeFromString(Type);
  if (!VarType)
    return LogErrorV("Unknown type in for loop");

  llvm::Constant *MinFloatLoopEps = nullptr;
  llvm::Constant *FloatZero = nullptr;
  if (VarType->isFloatingPointTy()) {
    MinFloatLoopEps = llvm::ConstantFP::get(VarType, 1e-6);
    FloatZero = llvm::ConstantFP::get(VarType, 0.0);
  }
  bool useHalfStepEpsilon = true;

  auto quantizeLoopValue = [&](llvm::Value *Value) -> llvm::Value * {
    if (!VarType->isFloatingPointTy())
      return Value;
    double quantScale = VarType->isDoubleTy() ? 100.0 : 10.0;
    llvm::Value *ScaleConst =
        llvm::ConstantFP::get(VarType, quantScale);
    llvm::Value *ScaledVal =
        Builder->CreateFMul(Value, ScaleConst, "loop.quantize.mul");
    llvm::Type *QuantIntTy =
        VarType->isDoubleTy()
            ? llvm::Type::getInt64Ty(*TheContext)
            : llvm::Type::getInt32Ty(*TheContext);
    llvm::Value *ScaledInt =
        Builder->CreateFPToSI(ScaledVal, QuantIntTy, "loop.quantize.toint");
    llvm::Value *ScaledFP =
        Builder->CreateSIToFP(ScaledInt, VarType, "loop.quantize.tofp");
    return Builder->CreateFDiv(ScaledFP, ScaleConst,
                               "loop.quantize.value");
  };
  
  // Ensure values match the variable type
  if (VarType->isFloatingPointTy()) {
    // Convert init to float type if needed
    if (InitVal->getType()->isIntegerTy()) {
      InitVal = Builder->CreateSIToFP(InitVal, VarType, "initcast");
    } else if (InitVal->getType() != VarType) {
      InitVal = Builder->CreateFPCast(InitVal, VarType, "initcast");
    }
    
    // Convert limit to float type if needed
    if (LimitVal) {
      if (LimitVal->getType()->isIntegerTy()) {
        LimitVal = Builder->CreateSIToFP(LimitVal, VarType, "limitcast");
      } else if (LimitVal->getType() != VarType) {
        LimitVal = Builder->CreateFPCast(LimitVal, VarType, "limitcast");
      }
    }
  // For integer types
  } else if (VarType->isIntegerTy()) {
    // Convert init to int type if needed
    if (InitVal->getType()->isFloatingPointTy()) {
      InitVal = Builder->CreateFPToSI(InitVal, VarType, "initcast");
    } else if (InitVal->getType() != VarType) {
      InitVal = Builder->CreateIntCast(InitVal, VarType, true, "initcast");
    }
    
    // Convert limit to int type if needed
    if (LimitVal) {
      if (LimitVal->getType()->isFloatingPointTy()) {
        LimitVal = Builder->CreateFPToSI(LimitVal, VarType, "limitcast");
      } else if (LimitVal->getType() != VarType) {
        LimitVal = Builder->CreateIntCast(LimitVal, VarType, true, "limitcast");
      }
    }
  }

  ExprAST *ConditionExpr = this->getCondExpr();
  const bool hasConditionExpr = ConditionExpr != nullptr;
  const bool conditionNeedsLoopVarBinding =
      hasConditionExpr &&
      conditionUsesLoopVariable(ConditionExpr, VarName);

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop
  llvm::BasicBlock *InitBB = Builder->GetInsertBlock();
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "forcond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "forbody");
  llvm::BasicBlock *IncBB = llvm::BasicBlock::Create(*TheContext, "forinc");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "forcont");
  
  // Create an alloca for the loop variable
  llvm::AllocaInst *VarAlloca = Builder->CreateAlloca(VarType, nullptr, VarName);
  llvm::AllocaInst *StepMagnitudeAlloca = nullptr;

  // Determine if incrementing or decrementing based on init vs limit
  bool isIncrementing = true;
  if (LimitVal) {
    if (llvm::ConstantFP *InitFP = llvm::dyn_cast<llvm::ConstantFP>(InitVal)) {
      if (llvm::ConstantFP *LimitFP = llvm::dyn_cast<llvm::ConstantFP>(LimitVal)) {
        isIncrementing = InitFP->getValueAPF().convertToDouble() <= LimitFP->getValueAPF().convertToDouble();
      }
    } else if (llvm::ConstantInt *InitInt = llvm::dyn_cast<llvm::ConstantInt>(InitVal)) {
      if (llvm::ConstantInt *LimitInt = llvm::dyn_cast<llvm::ConstantInt>(LimitVal)) {
        isIncrementing = InitInt->getSExtValue() <= LimitInt->getSExtValue();
      }
    }
  }

  if (!LimitVal && hasConditionExpr) {
    if (auto inferredDirection =
            inferLoopDirectionFromCondition(ConditionExpr, VarName))
      isIncrementing = *inferredDirection;
  }

  const bool isMultiplicativeStep =
      StepExpr && (StepOp == '*' || StepOp == '/' || StepOp == '%');
  if (isMultiplicativeStep && !isIncrementing)
    useHalfStepEpsilon = false;

  bool trackFloatMagnitude =
      VarType->isFloatingPointTy() && !isIncrementing;
  if (LimitVal && trackFloatMagnitude) {
    StepMagnitudeAlloca =
        Builder->CreateAlloca(VarType, nullptr, VarName + ".stepmag");
    if (MinFloatLoopEps)
      Builder->CreateStore(MinFloatLoopEps, StepMagnitudeAlloca);
  }

  Builder->CreateStore(InitVal, VarAlloca);
  
  // Jump to the condition check
  Builder->CreateBr(CondBB);
  
  // Emit the condition check
  Builder->SetInsertPoint(CondBB);
  llvm::Value *CondV = nullptr;
  llvm::Value *VarVal = nullptr;
  
  if (hasConditionExpr) {
    llvm::Value *SavedVal = nullptr;
    bool hadSavedVal = false;
    if (conditionNeedsLoopVarBinding) {
      auto NamedIt = NamedValues.find(VarName);
      hadSavedVal = NamedIt != NamedValues.end();
      if (hadSavedVal)
        SavedVal = NamedIt->second;

      NamedValues[VarName] = VarAlloca;
    }

    CondV = ConditionExpr->codegen();

    if (conditionNeedsLoopVarBinding) {
      if (hadSavedVal)
        NamedValues[VarName] = SavedVal;
      else
        NamedValues.erase(VarName);
    }

    if (!CondV)
      return nullptr;

    CondV = coerceLoopConditionToBool(CondV, "loopcond");
    if (!CondV)
      return nullptr;
  } else if (LimitVal) {
    VarVal = Builder->CreateLoad(VarType, VarAlloca, VarName);
    if (VarType->isFloatingPointTy()) {
      llvm::Value *Epsilon =
          MinFloatLoopEps ? MinFloatLoopEps
                          : llvm::ConstantFP::get(VarType, 1e-6);
      if (StepMagnitudeAlloca) {
        llvm::Value *StoredMag =
            Builder->CreateLoad(VarType, StepMagnitudeAlloca,
                                VarName + ".stepmag");
        double scale = useHalfStepEpsilon ? 0.5 : 5.0;
        llvm::Value *EffectiveMag = Builder->CreateFMul(
            StoredMag, llvm::ConstantFP::get(VarType, scale),
            "loop.stepmag.scaled");
        llvm::Value *NeedsMin =
            Builder->CreateFCmpOLT(EffectiveMag, MinFloatLoopEps,
                                   "loop.stepmag.needsmin");
        Epsilon =
            Builder->CreateSelect(NeedsMin, MinFloatLoopEps, EffectiveMag,
                                  "loop.stepmag.eps");
      }
      llvm::Value *SignedEps =
          isIncrementing
              ? Epsilon
              : Builder->CreateFNeg(Epsilon, "loop.stepmag.negeps");
      llvm::Value *AdjustedLimit =
          Builder->CreateFAdd(LimitVal, SignedEps, "loop.limit_eps");
      if (isIncrementing) {
        CondV = Builder->CreateFCmpOLE(VarVal, AdjustedLimit, "loopcond");
      } else {
        CondV = Builder->CreateFCmpOGE(VarVal, AdjustedLimit, "loopcond");
      }
    } else {
      if (isIncrementing) {
        CondV = Builder->CreateICmpSLE(VarVal, LimitVal, "loopcond");
      } else {
        CondV = Builder->CreateICmpSGE(VarVal, LimitVal, "loopcond");
        if (isMultiplicativeStep) {
          llvm::Value *One = llvm::ConstantInt::get(VarType, 1);
          llvm::Value *Fallback =
              Builder->CreateICmpSGE(VarVal, One, "loop.minbound");
          CondV = Builder->CreateOr(CondV, Fallback, "loopcond.ext");
        }
      }
    }
  } else {
    return LogErrorV("For loop must have either a limit or condition expression");
  }
  
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  
  // Add the loop variable to the symbol table
  llvm::Value *OldVal = NamedValues[VarName];
  NamedValues[VarName] = VarAlloca;
  
  // Push the exit and continue blocks for break/skip statements
  LoopExitBlocks.push_back(AfterBB);
  LoopContinueBlocks.push_back(IncBB);
  
  // Generate code for the body
  llvm::Value *BodyV = Body->codegen();
  
  // Pop the exit and continue blocks
  LoopExitBlocks.pop_back();
  LoopContinueBlocks.pop_back();
  
  if (!BodyV) {
    // Restore the old value
    if (OldVal)
      NamedValues[VarName] = OldVal;
    else
      NamedValues.erase(VarName);
    return nullptr;
  }
  
  // Branch to increment block (unless body contains break/return)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(IncBB);
  
  // Emit the increment block
  IncBB->insertInto(TheFunction);
  Builder->SetInsertPoint(IncBB);
  
  // Increment or decrement the loop variable
  VarVal = Builder->CreateLoad(VarType, VarAlloca, VarName);
  llvm::Value *NextVal;
  
  if (StepExpr) {
    // Use custom step expression
    llvm::Value *StepVal = StepExpr->codegen();
    if (!StepVal)
      return nullptr;
    
    // Ensure step has same type as variable
    if (StepVal->getType() != VarType) {
      if (VarType->isFloatingPointTy()) {
        if (StepVal->getType()->isIntegerTy()) {
          StepVal = Builder->CreateSIToFP(StepVal, VarType, "stepcast");
        } else if (StepVal->getType()->isFloatingPointTy()) {
          StepVal = Builder->CreateFPCast(StepVal, VarType, "stepcast");
        }
      } else if (VarType->isIntegerTy()) {
        if (StepVal->getType()->isFloatingPointTy()) {
          StepVal = Builder->CreateFPToSI(StepVal, VarType, "stepcast");
        } else {
          StepVal = Builder->CreateIntCast(StepVal, VarType, true, "stepcast");
        }
      }
    }
    
    // Apply the appropriate step operation
    switch (StepOp) {
    case '+':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFAdd(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateAdd(VarVal, StepVal, "nextval");
      }
      break;
    case '-':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFSub(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSub(VarVal, StepVal, "nextval");
      }
      break;
    case '*':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFMul(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateMul(VarVal, StepVal, "nextval");
      }
      break;
    case '/':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFDiv(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSDiv(VarVal, StepVal, "nextval");
      }
      break;
    case '%':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFRem(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSRem(VarVal, StepVal, "nextval");
      }
      break;
    default:
      return LogErrorV("Unknown step operation in for loop");
    }
  } else {
    // Default step of 1 or -1
    if (VarType->isFloatingPointTy()) {
      llvm::Value *Step = llvm::ConstantFP::get(VarType, isIncrementing ? 1.0 : -1.0);
      NextVal = Builder->CreateFAdd(VarVal, Step, "nextval");
    } else {
      llvm::Value *Step = llvm::ConstantInt::get(VarType, isIncrementing ? 1 : -1);
      NextVal = Builder->CreateAdd(VarVal, Step, "nextval");
    }
  }

  if (VarType->isFloatingPointTy() && isMultiplicativeStep)
    NextVal = quantizeLoopValue(NextVal);


  if (StepMagnitudeAlloca && trackFloatMagnitude) {
    llvm::Value *StepDelta =
        Builder->CreateFSub(NextVal, VarVal, "loop.stepdelta");
    llvm::Value *IsNegative =
        Builder->CreateFCmpOLT(
            StepDelta,
            FloatZero ? FloatZero : llvm::ConstantFP::get(VarType, 0.0),
            "loop.stepdelta.isneg");
    llvm::Value *NegatedDelta =
        Builder->CreateFNeg(StepDelta, "loop.stepdelta.neg");
    llvm::Value *AbsDelta =
        Builder->CreateSelect(IsNegative, NegatedDelta, StepDelta,
                              "loop.stepdelta.abs");
    llvm::Value *NeedsMin =
        Builder->CreateFCmpOLT(AbsDelta, MinFloatLoopEps,
                               "loop.stepdelta.needsmin");
    llvm::Value *StoredMag =
        Builder->CreateSelect(NeedsMin, MinFloatLoopEps, AbsDelta,
                              "loop.stepmag.next");
    Builder->CreateStore(StoredMag, StepMagnitudeAlloca);
  }
  
  Builder->CreateStore(NextVal, VarAlloca);
  Builder->CreateBr(CondBB);
  
  // Emit the after-loop block
  AfterBB->insertInto(TheFunction);
  Builder->SetInsertPoint(AfterBB);
  
  // Restore the old value
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

// Generate code for if statements
llvm::Value *IfStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  // Check if condition is void (from assignment) - reject it
  if (CondV->getType()->isVoidTy()) {
    return LogErrorV("Cannot use assignment as condition in if statement (use == for comparison)");
  }

  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV,
      llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV,
        llvm::ConstantFP::get(CondV->getType(), 0.0), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV,
        llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
    } else {
      return LogErrorV("Condition must be a numeric type");
    }
  }
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  const TypeCheckExprAST *typeCheck =
      dynamic_cast<const TypeCheckExprAST *>(getCondition());
  std::optional<std::string> bindingName;
  std::optional<TypeInfo> bindingType;
  llvm::Value *bindingValue = nullptr;
  bool bindInThen = false;
  bool bindInElse = false;

  if (typeCheck && typeCheck->getBindingName() &&
      typeCheck->getBindingValue()) {
    bindingName = *typeCheck->getBindingName();
    bindingType = typeCheck->getTargetTypeInfo();
    bindingValue = typeCheck->getBindingValue();
    bindInThen = !typeCheck->isNegated();
    bindInElse = typeCheck->isNegated();
  }
  
  // Create blocks for the then and else cases. Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");
  
  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  ensureBaseNonNullScope();
  std::set<std::string> baseFacts = currentNonNullFactsCopy();
  std::optional<NullComparison> comparison = extractNullComparison(getCondition());

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);
  pushNonNullFactsFrom(baseFacts);
  if (comparison)
    applyNullComparisonToCurrentScope(*comparison, BranchKind::Then);
  if (bindInThen && bindingName && bindingType) {
    if (!emitTypeCheckBinding(*bindingType, *bindingName, bindingValue)) {
      popNonNullFactsScope();
      return nullptr;
    }
  }
  llvm::Value *ThenV = getThenBranch()->codegen();
  if (!ThenV) {
    popNonNullFactsScope();
    return nullptr;
  }
  llvm::BasicBlock *ThenEndBlock = Builder->GetInsertBlock();
  bool thenFallsThrough = !ThenEndBlock->getTerminator();
  std::set<std::string> thenFacts = NonNullFacts.back();
  if (thenFallsThrough)
    Builder->CreateBr(MergeBB);
  popNonNullFactsScope();
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();
  
  // Emit else block.
  ElseBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ElseBB);
  pushNonNullFactsFrom(baseFacts);
  if (comparison)
    applyNullComparisonToCurrentScope(*comparison, BranchKind::Else);
  if (bindInElse && bindingName && bindingType && getElseBranch()) {
    if (!emitTypeCheckBinding(*bindingType, *bindingName, bindingValue)) {
      popNonNullFactsScope();
      return nullptr;
    }
  }
  
  llvm::Value *ElseV = nullptr;
  if (getElseBranch()) {
    ElseV = getElseBranch()->codegen();
    if (!ElseV) {
      popNonNullFactsScope();
      return nullptr;
    }
  }
  
  llvm::BasicBlock *ElseEndBlock = Builder->GetInsertBlock();
  bool elseFallsThrough = !ElseEndBlock->getTerminator();
  std::set<std::string> elseFacts = NonNullFacts.back();
  if (elseFallsThrough)
    Builder->CreateBr(MergeBB);
  popNonNullFactsScope();
  
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();
  
  // Emit merge block.
  MergeBB->insertInto(TheFunction);
  Builder->SetInsertPoint(MergeBB);

  std::optional<std::set<std::string>> continuingFacts;
  if (thenFallsThrough)
    continuingFacts = thenFacts;
  if (elseFallsThrough) {
    if (continuingFacts)
      continuingFacts = intersectNonNullFacts(*continuingFacts, elseFacts);
    else
      continuingFacts = elseFacts;
  }

  if (continuingFacts)
    replaceCurrentNonNullFacts(*continuingFacts);
  else
    replaceCurrentNonNullFacts(baseFacts);
  
  // Return the last value (this is somewhat arbitrary for statements)
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

// Generate code for while statements
llvm::Value *WhileStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop condition, body, and exit
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "whilecond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "whilebody");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "whilecont");
  
  // Jump to the condition block
  Builder->CreateBr(CondBB);
  
  // Emit the condition block
  Builder->SetInsertPoint(CondBB);
  
  // Evaluate the condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV, 
      llvm::ConstantInt::get(CondV->getType(), 0), "whilecond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV, 
        llvm::ConstantFP::get(CondV->getType(), 0.0), "whilecond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV, 
        llvm::ConstantInt::get(CondV->getType(), 0), "whilecond");
  } else {
      return LogErrorV("Condition must be a numeric type");
    }
  }

  const TypeCheckExprAST *typeCheck =
      dynamic_cast<const TypeCheckExprAST *>(getCondition());
  std::optional<std::string> bindingName;
  std::optional<TypeInfo> bindingType;
  llvm::Value *bindingValue = nullptr;
  bool bindInLoop = false;

  if (typeCheck && typeCheck->getBindingName() &&
      typeCheck->getBindingValue() && !typeCheck->isNegated()) {
    bindingName = *typeCheck->getBindingName();
    bindingType = typeCheck->getTargetTypeInfo();
    bindingValue = typeCheck->getBindingValue();
    bindInLoop = true;
  }
  
  std::optional<NullComparison> comparison = extractNullComparison(getCondition());
  ensureBaseNonNullScope();
  std::set<std::string> baseFacts = currentNonNullFactsCopy();
  
  // Create the conditional branch
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  pushNonNullFactsFrom(baseFacts);
  if (comparison)
    applyNullComparisonToCurrentScope(*comparison, BranchKind::Then);
  if (bindInLoop && bindingName && bindingType) {
    if (!emitTypeCheckBinding(*bindingType, *bindingName, bindingValue)) {
      popNonNullFactsScope();
      return nullptr;
    }
  }
  
  // Push the exit and continue blocks for break/skip statements
  LoopExitBlocks.push_back(AfterBB);
  LoopContinueBlocks.push_back(CondBB);
  
  // Generate code for the body
  llvm::Value *BodyV = getBody()->codegen();
  
  // Pop the exit and continue blocks
  LoopExitBlocks.pop_back();
  LoopContinueBlocks.pop_back();
  
  popNonNullFactsScope();
  
  if (!BodyV)
    return nullptr;
  
  // After the body, branch back to the condition check
  // (unless the body contains a break/return)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(CondBB);
  
  // Emit the after-loop block
  AfterBB->insertInto(TheFunction);
  Builder->SetInsertPoint(AfterBB);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *UseStmtAST::codegen() {
  // TODO: Implement module importing
  // This would require linking external modules
  return LogErrorV("use statements not yet implemented in code generation");
}

llvm::Value *BreakStmtAST::codegen() {
  // Check if inside a loop
  if (LoopExitBlocks.empty())
    return LogErrorV("'break' statement not within a loop");
  
  // Get the current loop's exit block
  llvm::BasicBlock *ExitBlock = LoopExitBlocks.back();
  
  // Create an unconditional branch to the loop exit
  Builder->CreateBr(ExitBlock);
  
  // Create a new dead block for any code after the break
  // (which should be unreachable)
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *DeadBB = llvm::BasicBlock::Create(*TheContext, "afterbreak", TheFunction);
  Builder->SetInsertPoint(DeadBB);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *SkipStmtAST::codegen() {
  // Check if inside a loop
  if (LoopContinueBlocks.empty())
    return LogErrorV("'skip' statement not within a loop");
  
  // Get the current loop's continue block
  llvm::BasicBlock *ContinueBlock = LoopContinueBlocks.back();
  
  // Create an unconditional branch to the loop continue point
  Builder->CreateBr(ContinueBlock);
  
  // Create a new dead block for any code after the skip
  // (which should be unreachable)
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *DeadBB = llvm::BasicBlock::Create(*TheContext, "afterskip", TheFunction);
  Builder->SetInsertPoint(DeadBB);

  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

static bool conditionUsesLoopVariable(const ExprAST *expr,
                                      const std::string &varName);
static std::optional<bool>
inferLoopDirectionFromCondition(const ExprAST *expr,
                                const std::string &varName);

llvm::Value *AssertStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Generate code for the condition expression
  llvm::Value *CondValue = Condition->codegen();
  if (!CondValue)
    return nullptr;

  // Convert condition to boolean if necessary
  if (CondValue->getType() != llvm::Type::getInt8Ty(*TheContext)) {
    // For numeric types, check if != 0
    if (CondValue->getType()->isIntegerTy()) {
      llvm::Value *Zero = llvm::Constant::getNullValue(CondValue->getType());
      CondValue = Builder->CreateICmpNE(CondValue, Zero, "assertcond");
    } else if (CondValue->getType()->isFloatingPointTy()) {
      llvm::Value *Zero = llvm::ConstantFP::get(CondValue->getType(), 0.0);
      CondValue = Builder->CreateFCmpONE(CondValue, Zero, "assertcond");
    }
    // Convert to i8 for consistency
    CondValue = Builder->CreateIntCast(CondValue, llvm::Type::getInt8Ty(*TheContext), false, "assertbool");
  }

  // Create basic blocks for assertion success and failure
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *SuccessBB = llvm::BasicBlock::Create(*TheContext, "assert_success", TheFunction);
  llvm::BasicBlock *FailBB = llvm::BasicBlock::Create(*TheContext, "assert_fail", TheFunction);

  // Convert condition to i1 for branch
  llvm::Value *BoolCond = Builder->CreateICmpNE(CondValue, llvm::ConstantInt::get(llvm::Type::getInt8Ty(*TheContext), 0), "asserttest");

  // Branch based on condition
  Builder->CreateCondBr(BoolCond, SuccessBB, FailBB);

  // Generate failure block - call abort() to terminate program
  Builder->SetInsertPoint(FailBB);

  auto emitAssertMessage = [&]() {
    llvm::Type *Int32Ty = llvm::Type::getInt32Ty(*TheContext);
    llvm::Type *Int8PtrTy = llvm::PointerType::get(*TheContext, 0);
    llvm::FunctionType *PrintfTy =
        llvm::FunctionType::get(Int32Ty, {Int8PtrTy}, true);
    llvm::FunctionCallee PrintfFunc =
        TheModule->getOrInsertFunction("printf", PrintfTy);

    std::string message = "[assert] failed";
    if (getLine() > 0) {
      message += " at line %u";
      if (getColumn() > 0)
        message += ", column %u";
      else
        message += ", column ?";
    }
    message.push_back('\n');

    llvm::Value *fmt =
        Builder->CreateGlobalString(message, "__hybrid_assert_fmt");
    std::vector<llvm::Value *> args;
    args.push_back(fmt);
    if (getLine() > 0) {
      args.push_back(
          llvm::ConstantInt::get(Int32Ty, static_cast<uint32_t>(getLine())));
      if (getColumn() > 0)
        args.push_back(llvm::ConstantInt::get(
            Int32Ty, static_cast<uint32_t>(getColumn())));
    }

    Builder->CreateCall(PrintfFunc, args);
  };

  // Declare abort() function if not already declared
  llvm::Function *AbortFunc = TheModule->getFunction("abort");
  if (!AbortFunc) {
    llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
    AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage, "abort", TheModule.get());
  }

  emitAssertMessage();
  // Call abort() and create unreachable instruction
  Builder->CreateCall(AbortFunc);
  Builder->CreateUnreachable();

  // Continue with success block
  Builder->SetInsertPoint(SuccessBB);

  // Return a dummy value indicating successful assertion
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

// Generate code for unsafe blocks
llvm::Value *UnsafeBlockStmtAST::codegen() {
  ArcScopeGuard arcScope("unsafe");
  // Unsafe blocks simply execute their body
  // The safety checks are done at parse time, not at runtime
  return Body->codegen();
}
