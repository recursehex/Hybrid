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

static llvm::StructType *getInterfaceEntryType() {
  if (auto *existing = llvm::StructType::getTypeByName(*TheContext,
                                                       "__HybridInterfaceEntry"))
    return existing;

  auto *typeDescTy =
      llvm::StructType::create(*TheContext, "__HybridTypeDescriptor");
  auto *interfaceEntryTy =
      llvm::StructType::create(*TheContext, "__HybridInterfaceEntry");

  auto *typeDescPtrTy = pointerType(typeDescTy);
  auto *opaquePtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *opaquePtrPtrTy = pointerType(opaquePtrTy);

  interfaceEntryTy->setBody({typeDescPtrTy, opaquePtrPtrTy});
  typeDescTy->setBody({opaquePtrTy,
                       typeDescPtrTy,
                       opaquePtrPtrTy,
                       llvm::Type::getInt32Ty(*TheContext),
                       pointerType(interfaceEntryTy),
                       llvm::Type::getInt32Ty(*TheContext),
                       getDeallocFunctionPointerType()});
  return interfaceEntryTy;
}

llvm::StructType *getTypeDescriptorType() {
  if (auto *existing =
          llvm::StructType::getTypeByName(*TheContext,
                                          "__HybridTypeDescriptor"))
    return existing;
  getInterfaceEntryType();
  return llvm::StructType::getTypeByName(*TheContext,
                                         "__HybridTypeDescriptor");
}

llvm::StructType *getArcHeaderType() {
  if (auto *existing = llvm::StructType::getTypeByName(*TheContext,
                                                       "__HybridClassHeader"))
    return existing;

  auto *headerTy = llvm::StructType::create(*TheContext, "__HybridClassHeader");
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  headerTy->setBody({llvm::Type::getInt32Ty(*TheContext),
                     llvm::Type::getInt32Ty(*TheContext),
                     typeDescPtrTy});
  return headerTy;
}

llvm::PointerType *getArrayReleaseCallbackPointerType() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                                       {voidPtrTy}, false);
  return pointerType(fnTy);
}

llvm::StructType *getArrayHeaderType() {
  if (auto *existing = llvm::StructType::getTypeByName(*TheContext,
                                                       "__HybridArrayHeader"))
    return existing;
  auto *headerTy = llvm::StructType::create(*TheContext, "__HybridArrayHeader");
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *sizeTy = getSizeType();
  auto *releasePtrTy = getArrayReleaseCallbackPointerType();
  headerTy->setBody({llvm::Type::getInt32Ty(*TheContext),
                     llvm::Type::getInt32Ty(*TheContext),
                     typeDescPtrTy,
                     sizeTy,
                     sizeTy,
                     releasePtrTy});
  return headerTy;
}

llvm::StructType *getStringStorageType() {
  if (auto *existing = llvm::StructType::getTypeByName(
          *TheContext, "__HybridStringStorage"))
    return existing;
  auto *storageTy =
      llvm::StructType::create(*TheContext, "__HybridStringStorage");
  auto *headerTy = getArcHeaderType();
  auto *sizeTy = getSizeType();
  auto *opaquePtr = pointerType();
  auto *bytePtr = pointerType(llvm::Type::getInt8Ty(*TheContext));
  storageTy->setBody(
      {headerTy, sizeTy, sizeTy, sizeTy, opaquePtr, bytePtr});
  return storageTy;
}

llvm::StructType *getDecimalStorageType() {
  if (auto *existing = llvm::StructType::getTypeByName(
          *TheContext, "__HybridDecimalStorage"))
    return existing;
  auto *decimalTy =
      llvm::StructType::create(*TheContext, "__HybridDecimalStorage");
  decimalTy->setBody({llvm::Type::getInt64Ty(*TheContext),
                      llvm::Type::getInt64Ty(*TheContext)});
  return decimalTy;
}

bool isDecimalLLVMType(llvm::Type *type) {
  auto *structTy = llvm::dyn_cast_or_null<llvm::StructType>(type);
  if (!structTy)
    return false;
  llvm::StructType *decimalTy = getDecimalStorageType();
  if (structTy == decimalTy)
    return true;
  if (structTy->getNumElements() != 2)
    return false;
  return structTy->getElementType(0)->isIntegerTy(64) &&
         structTy->getElementType(1)->isIntegerTy(64);
}

std::uint64_t getArrayPayloadOffsetBytes() {
  const llvm::DataLayout &DL = TheModule->getDataLayout();
  return DL.getTypeAllocSize(getArrayHeaderType());
}

static llvm::Constant *getOrCreateTypeNameConstant(const std::string &typeName) {
  std::string symbol = makeRuntimeSymbolName("__hybrid_type_name$", typeName);
  if (auto *existing = TheModule->getNamedGlobal(symbol))
    return llvm::ConstantExpr::getPointerCast(
        existing, pointerType(llvm::Type::getInt8Ty(*TheContext)));

  auto *literal =
      llvm::ConstantDataArray::getString(*TheContext, typeName, true);
  auto *global = new llvm::GlobalVariable(
      *TheModule, literal->getType(), true, llvm::GlobalValue::PrivateLinkage,
      literal, symbol);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  global->setAlignment(llvm::MaybeAlign(1));
  return llvm::ConstantExpr::getPointerCast(
      global, pointerType(llvm::Type::getInt8Ty(*TheContext)));
}

void computeInterfaceMethodLayout(
    const std::string &typeName,
    const std::vector<const MethodDefinition *> &methods,
    CompositeTypeInfo &metadata) {
  if (metadata.kind != AggregateKind::Interface)
    return;

  std::vector<std::string> order;
  std::map<std::string, unsigned> slotMap;
  std::set<std::string> seen;

  auto appendFromInterface = [&](const CompositeTypeInfo &ifaceInfo) {
    for (const std::string &key : ifaceInfo.interfaceMethodOrder) {
      if (seen.insert(key).second) {
        slotMap[key] = static_cast<unsigned>(order.size());
        order.push_back(key);
      }
    }
  };

  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseInfo =
            lookupCompositeInfo(*metadata.baseClass)) {
      appendFromInterface(*baseInfo);
    }
  }

  for (const std::string &ifaceName : metadata.interfaces) {
    if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName))
      appendFromInterface(*ifaceInfo);
  }

  for (const MethodDefinition *MethodDef : methods) {
    if (!MethodDef)
      continue;
    if (MethodDef->getKind() != MethodKind::Regular)
      continue;
    if (MethodDef->isStatic())
      continue;
    auto it = metadata.methodInfo.find(MethodDef->getDisplayName());
    if (it == metadata.methodInfo.end())
      continue;
    const CompositeMemberInfo &member = it->second;
    if (member.dispatchKey.empty())
      continue;
    if (seen.insert(member.dispatchKey).second) {
      slotMap[member.dispatchKey] = static_cast<unsigned>(order.size());
      order.push_back(member.dispatchKey);
    }
  }

  metadata.interfaceMethodOrder = std::move(order);
  metadata.interfaceMethodSlotMap = std::move(slotMap);
}

bool computeVirtualDispatchLayout(const std::string &typeName,
                                  CompositeTypeInfo &metadata) {
  if (metadata.kind != AggregateKind::Class)
    return true;

  constexpr const char *DestructorKey = "__dtor";

  std::vector<std::string> order;
  std::vector<std::string> impls;
  std::vector<bool> isAbstract;
  std::map<std::string, unsigned> slotMap;

  if (metadata.baseClass) {
    const CompositeTypeInfo *baseInfo =
        lookupCompositeInfo(*metadata.baseClass);
    if (!baseInfo) {
      reportCompilerError("Base class '" + *metadata.baseClass +
                          "' metadata unavailable while building vtable for '" +
                          typeName + "'");
      return false;
    }
    order = baseInfo->vtableOrder;
    impls = baseInfo->vtableImplementations;
    isAbstract = baseInfo->vtableIsAbstract;
    slotMap = baseInfo->vtableSlotMap;
    metadata.destructorVtableSlot = baseInfo->destructorVtableSlot;
  }

  impls.resize(order.size());
  isAbstract.resize(order.size());

  for (auto &entry : metadata.methodInfo) {
    CompositeMemberInfo &member = entry.second;
    bool participates = member.modifiers.isVirtual ||
                        member.modifiers.isOverride ||
                        member.modifiers.isAbstract;
    if (!participates)
      continue;

    unsigned slot = std::numeric_limits<unsigned>::max();

    if (member.modifiers.isOverride) {
      std::string baseSignature =
          member.overridesSignature.empty() ? member.signature
                                            : member.overridesSignature;
      auto it = slotMap.find(baseSignature);
      if (it == slotMap.end()) {
        reportCompilerError("Override '" + member.signature + "' of class '" +
                            typeName + "' does not map to a base vtable slot");
        return false;
      }
      slot = it->second;
      if (!member.mangledName.empty())
        impls[slot] = member.mangledName;
      isAbstract[slot] = member.modifiers.isAbstract;
      slotMap[member.signature] = slot;
      if (!baseSignature.empty())
        slotMap[baseSignature] = slot;
    } else {
      auto existing = slotMap.find(member.signature);
      if (existing != slotMap.end()) {
        slot = existing->second;
        if (!member.mangledName.empty())
          impls[slot] = member.mangledName;
        isAbstract[slot] = member.modifiers.isAbstract;
      } else {
        slot = static_cast<unsigned>(order.size());
        order.push_back(member.signature);
        impls.push_back(member.mangledName);
        isAbstract.push_back(member.modifiers.isAbstract);
        slotMap[member.signature] = slot;
      }
    }

    member.vtableSlot = slot;
  }

  metadata.vtableOrder = std::move(order);
  metadata.vtableImplementations = std::move(impls);
  metadata.vtableIsAbstract = std::move(isAbstract);
  metadata.vtableSlotMap = std::move(slotMap);

  if (metadata.hasDestructor) {
    unsigned slot = metadata.destructorVtableSlot;
    if (slot == std::numeric_limits<unsigned>::max()) {
      slot = static_cast<unsigned>(metadata.vtableOrder.size());
      metadata.vtableOrder.push_back(DestructorKey);
      metadata.vtableImplementations.push_back(metadata.destructorFunctionName);
      metadata.vtableIsAbstract.push_back(false);
    } else {
      if (slot >= metadata.vtableImplementations.size()) {
        metadata.vtableImplementations.resize(slot + 1);
        metadata.vtableIsAbstract.resize(slot + 1, false);
        metadata.vtableOrder.resize(slot + 1, {});
      }
      metadata.vtableImplementations[slot] = metadata.destructorFunctionName;
      if (slot < metadata.vtableOrder.size() && metadata.vtableOrder[slot].empty())
        metadata.vtableOrder[slot] = DestructorKey;
      metadata.vtableIsAbstract[slot] = false;
    }
    metadata.vtableSlotMap[DestructorKey] = slot;
    metadata.destructorVtableSlot = slot;
  }

  return true;
}

static const CompositeMemberInfo *
findMethodInHierarchyByKey(const std::string &typeName,
                           const std::string &dispatchKey,
                           const CompositeTypeInfo **ownerInfo = nullptr) {
  std::string current = typeName;
  std::set<std::string> visited;

  while (visited.insert(current).second) {
    const CompositeTypeInfo *info = lookupCompositeInfo(current);
    if (!info)
      break;

    for (const auto &entry : info->methodInfo) {
      if (entry.second.dispatchKey == dispatchKey) {
        if (ownerInfo)
          *ownerInfo = info;
        return &entry.second;
      }
    }

    if (!info->baseClass)
      break;
    current = *info->baseClass;
  }

  return nullptr;
}

bool emitInterfaceDescriptor(const std::string &typeName,
                             CompositeTypeInfo &metadata) {
  llvm::StructType *typeDescTy = getTypeDescriptorType();
  llvm::StructType *ifaceEntryTy = getInterfaceEntryType();
  auto *charPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *typeDescPtrTy = pointerType(typeDescTy);
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *voidPtrPtrTy = pointerType(voidPtrTy);
  auto *ifaceEntryPtrTy = pointerType(ifaceEntryTy);

  llvm::GlobalVariable *descriptorGV =
      TheModule->getGlobalVariable(metadata.descriptorGlobalName, true);
  if (!descriptorGV) {
    reportCompilerError("Internal error: descriptor global '" +
                        metadata.descriptorGlobalName +
                        "' missing while building interface '" + typeName + "'");
    return false;
  }

  llvm::Constant *typeNameConst = getOrCreateTypeNameConstant(typeName);
  llvm::Constant *baseConst =
      llvm::ConstantPointerNull::get(typeDescPtrTy);
  llvm::Constant *vtableConst =
      llvm::ConstantPointerNull::get(voidPtrPtrTy);
  llvm::Constant *vtableSizeConst = llvm::ConstantInt::get(
      llvm::Type::getInt32Ty(*TheContext),
      static_cast<uint32_t>(metadata.interfaceMethodOrder.size()));
  llvm::Constant *ifaceMapConst =
      llvm::ConstantPointerNull::get(ifaceEntryPtrTy);
  llvm::Constant *ifaceCountConst =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  llvm::Constant *deallocConst =
      llvm::ConstantPointerNull::get(getDeallocFunctionPointerType());

  auto *descriptorConst = llvm::ConstantStruct::get(
      typeDescTy,
      {typeNameConst, baseConst, vtableConst, vtableSizeConst, ifaceMapConst,
       ifaceCountConst, deallocConst});

  descriptorGV->setInitializer(descriptorConst);
  descriptorGV->setConstant(true);
  return true;
}

bool emitClassRuntimeStructures(const std::string &typeName,
                                llvm::StructType *structTy,
                                CompositeTypeInfo &metadata) {
  llvm::StructType *typeDescTy = getTypeDescriptorType();
  llvm::StructType *ifaceEntryTy = getInterfaceEntryType();
  auto *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  auto *charPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *voidPtrPtrTy = pointerType(voidPtrTy);
  auto *typeDescPtrTy = pointerType(typeDescTy);
  auto *ifaceEntryPtrTy = pointerType(ifaceEntryTy);

  llvm::GlobalVariable *descriptorGV =
      TheModule->getGlobalVariable(metadata.descriptorGlobalName, true);
  if (!descriptorGV) {
    reportCompilerError("Internal error: descriptor global '" +
                        metadata.descriptorGlobalName +
                        "' missing while building class '" + typeName + "'");
    return false;
  }

  std::string sanitizedType = sanitizeForMangle(typeName);

  // Emit vtable if needed
  llvm::Constant *vtablePtrConst =
      llvm::ConstantPointerNull::get(voidPtrPtrTy);
  if (!metadata.vtableOrder.empty()) {
    std::string vtableName = "__hybrid_vtable$" + sanitizedType;
    llvm::GlobalVariable *vtableGV =
        TheModule->getGlobalVariable(vtableName, true);

    std::vector<llvm::Constant *> entries;
    entries.reserve(metadata.vtableOrder.size());

    for (std::size_t i = 0; i < metadata.vtableOrder.size(); ++i) {
      const std::string &implName = metadata.vtableImplementations[i];
      if (implName.empty()) {
        entries.push_back(llvm::ConstantPointerNull::get(voidPtrTy));
        continue;
      }

      llvm::Function *fn = TheModule->getFunction(implName);
      if (!fn) {
        reportCompilerError("Internal error: function '" + implName +
                            "' missing while building vtable for '" +
                            typeName + "'");
        return false;
      }
      entries.push_back(
          llvm::ConstantExpr::getBitCast(fn, voidPtrTy));
    }

    auto *arrayTy =
        llvm::ArrayType::get(voidPtrTy, entries.size());
    auto *init = llvm::ConstantArray::get(arrayTy, entries);

    if (!vtableGV) {
      vtableGV = new llvm::GlobalVariable(
          *TheModule, arrayTy, true, llvm::GlobalValue::InternalLinkage, init,
          vtableName);
      vtableGV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    } else {
      vtableGV->setInitializer(init);
      vtableGV->setConstant(true);
    }

    llvm::Constant *zero = llvm::ConstantInt::get(int32Ty, 0);
    llvm::Constant *vtableIndices[] = {zero, zero};
    vtablePtrConst = llvm::ConstantExpr::getInBoundsGetElementPtr(
        arrayTy, vtableGV, vtableIndices);
    metadata.vtableGlobalName = vtableName;
  }

  // Emit interface dispatch tables
  std::set<std::string> allInterfaces;
  for (const std::string &iface : metadata.interfaces)
    collectInterfaceAncestors(iface, allInterfaces);

  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseInfo =
            lookupCompositeInfo(*metadata.baseClass)) {
      for (const std::string &iface : baseInfo->interfaces)
        collectInterfaceAncestors(iface, allInterfaces);
    }
  }

  std::vector<llvm::Constant *> interfaceEntries;
  interfaceEntries.reserve(allInterfaces.size());

  for (const std::string &ifaceName : allInterfaces) {
    const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName);
    if (!ifaceInfo) {
      reportCompilerError("Interface '" + ifaceName +
                          "' metadata missing while building class '" +
                          typeName + "'");
      return false;
    }

    llvm::GlobalVariable *ifaceDescriptorGV =
        TheModule->getGlobalVariable(ifaceInfo->descriptorGlobalName, true);
    if (!ifaceDescriptorGV) {
      reportCompilerError("Interface descriptor '" +
                          ifaceInfo->descriptorGlobalName +
                          "' missing while building class '" + typeName + "'");
      return false;
    }

    std::vector<llvm::Constant *> methodPtrs;
    methodPtrs.reserve(ifaceInfo->interfaceMethodOrder.size());

    for (const std::string &key : ifaceInfo->interfaceMethodOrder) {
      const CompositeTypeInfo *declaringInfo = nullptr;
      const CompositeMemberInfo *member =
          findMethodInHierarchyByKey(typeName, key, &declaringInfo);
      if (!member) {
        reportCompilerError("Class '" + typeName +
                            "' lacks implementation for interface member key '" +
                            key + "'");
        return false;
      }

      std::string signature = member->signature;
      if (!metadata.vtableSlotMap.contains(signature) &&
          !member->overridesSignature.empty())
        signature = member->overridesSignature;

      std::string implName;
      if (auto slotIt = metadata.vtableSlotMap.find(signature);
          slotIt != metadata.vtableSlotMap.end()) {
        unsigned slot = slotIt->second;
        if (slot < metadata.vtableImplementations.size())
          implName = metadata.vtableImplementations[slot];
      } else {
        implName = member->mangledName;
      }

      if (implName.empty()) {
        reportCompilerError("Class '" + typeName +
                            "' provides no concrete implementation for '" +
                            key + "' while building interface table");
        return false;
      }

      llvm::Function *implFn = TheModule->getFunction(implName);
      if (!implFn) {
        reportCompilerError("Missing function '" + implName +
                            "' while building interface table for '" +
                            typeName + "'");
        return false;
      }

      methodPtrs.push_back(
          llvm::ConstantExpr::getBitCast(implFn, voidPtrTy));
    }

    auto *methodArrayTy =
        llvm::ArrayType::get(voidPtrTy, methodPtrs.size());
    auto *methodInit = llvm::ConstantArray::get(methodArrayTy, methodPtrs);

    std::string tableName = "__hybrid_iface_table$" + sanitizedType + "$" +
                            sanitizeForMangle(ifaceName);

    llvm::GlobalVariable *methodTableGV =
        TheModule->getGlobalVariable(tableName, true);
    if (!methodTableGV) {
      methodTableGV = new llvm::GlobalVariable(
          *TheModule, methodArrayTy, true, llvm::GlobalValue::InternalLinkage,
          methodInit, tableName);
      methodTableGV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    } else {
      methodTableGV->setInitializer(methodInit);
      methodTableGV->setConstant(true);
    }

    llvm::Constant *zero = llvm::ConstantInt::get(int32Ty, 0);
    llvm::Constant *tableIndices[] = {zero, zero};
    llvm::Constant *tablePtr =
        llvm::ConstantExpr::getInBoundsGetElementPtr(
            methodArrayTy, methodTableGV, tableIndices);

    metadata.interfaceTableGlobals[ifaceName] = tableName;

    llvm::Constant *ifaceDescriptorPtr =
        llvm::ConstantExpr::getBitCast(ifaceDescriptorGV, typeDescPtrTy);
    interfaceEntries.push_back(llvm::ConstantStruct::get(
        ifaceEntryTy, {ifaceDescriptorPtr, tablePtr}));
  }

  llvm::Constant *ifaceMapConst =
      llvm::ConstantPointerNull::get(ifaceEntryPtrTy);
  llvm::Constant *ifaceCountConst =
      llvm::ConstantInt::get(int32Ty, static_cast<uint32_t>(interfaceEntries.size()));

  if (!interfaceEntries.empty()) {
    auto *ifaceArrayTy =
        llvm::ArrayType::get(ifaceEntryTy, interfaceEntries.size());
    auto *ifaceArray =
        llvm::ConstantArray::get(ifaceArrayTy, interfaceEntries);
    std::string ifaceArrayName =
        "__hybrid_iface_map$" + sanitizedType;
    llvm::GlobalVariable *ifaceArrayGV =
        TheModule->getGlobalVariable(ifaceArrayName, true);
    if (!ifaceArrayGV) {
      ifaceArrayGV = new llvm::GlobalVariable(
          *TheModule, ifaceArrayTy, true, llvm::GlobalValue::InternalLinkage,
          ifaceArray, ifaceArrayName);
      ifaceArrayGV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    } else {
      ifaceArrayGV->setInitializer(ifaceArray);
      ifaceArrayGV->setConstant(true);
    }

    llvm::Constant *zero = llvm::ConstantInt::get(int32Ty, 0);
    llvm::Constant *ifaceIndices[] = {zero, zero};
    ifaceMapConst = llvm::ConstantExpr::getInBoundsGetElementPtr(
        ifaceArrayTy, ifaceArrayGV, ifaceIndices);
  }

  llvm::Constant *typeNameConst = getOrCreateTypeNameConstant(typeName);

  llvm::Constant *baseConst =
      llvm::ConstantPointerNull::get(typeDescPtrTy);
  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseInfo =
            lookupCompositeInfo(*metadata.baseClass)) {
      llvm::GlobalVariable *baseDescriptorGV =
          TheModule->getGlobalVariable(baseInfo->descriptorGlobalName, true);
      if (!baseDescriptorGV) {
        reportCompilerError("Descriptor for base class '" +
                            *metadata.baseClass +
                            "' missing while building class '" + typeName + "'");
        return false;
      }
      baseConst =
          llvm::ConstantExpr::getBitCast(baseDescriptorGV, typeDescPtrTy);
    }
  }

  llvm::Constant *vtableSizeConst = llvm::ConstantInt::get(
      int32Ty, static_cast<uint32_t>(metadata.vtableOrder.size()));
  llvm::Constant *deallocConst =
      llvm::ConstantPointerNull::get(getDeallocFunctionPointerType());
  if (!metadata.deallocFunctionName.empty()) {
    llvm::Function *deallocFn =
        TheModule->getFunction(metadata.deallocFunctionName);
    if (!deallocFn) {
      reportCompilerError("Internal error: dealloc helper '" +
                          metadata.deallocFunctionName +
                          "' missing while building class '" + typeName + "'");
      return false;
    }
    deallocConst = llvm::ConstantExpr::getBitCast(
        deallocFn, getDeallocFunctionPointerType());
  }

  auto *descriptorConst = llvm::ConstantStruct::get(
      typeDescTy,
      {typeNameConst, baseConst, vtablePtrConst, vtableSizeConst,
       ifaceMapConst, ifaceCountConst, deallocConst});

  descriptorGV->setInitializer(descriptorConst);
  descriptorGV->setConstant(true);

  (void)structTy; // structTy currently unused but kept for future ARC integration
  return true;
}

llvm::Value *emitDynamicFunctionCall(CallExprAST &callExpr,
                                     const CompositeMemberInfo &memberInfo,
                                     llvm::Value *functionPointer,
                                     std::vector<llvm::Value *> argValues,
                                     const std::vector<bool> &argIsRef) {
  const size_t paramCount = memberInfo.parameterTypes.size();

  std::vector<llvm::Type *> paramLLVMTypes;
  paramLLVMTypes.reserve(paramCount);
  for (std::size_t idx = 0; idx < paramCount; ++idx) {
    const std::string paramTypeName =
        typeNameFromInfo(memberInfo.parameterTypes[idx]);
    llvm::Type *paramType = getTypeFromString(paramTypeName);
    if (!paramType)
      return LogErrorV(("Internal error: unable to resolve parameter type '" +
                        paramTypeName + "' for dynamic call")
                           .c_str());
    if (memberInfo.parameterIsRef[idx])
      paramType = llvm::PointerType::get(*TheContext, 0);
    paramLLVMTypes.push_back(paramType);
  }

  const std::string returnTypeName =
      typeNameFromInfo(memberInfo.returnType);
  llvm::Type *retType = getTypeFromString(returnTypeName);
  if (!retType)
    return LogErrorV(("Internal error: unable to resolve return type '" +
                      returnTypeName + "' for dynamic call")
                         .c_str());
  if (memberInfo.returnsByRef)
    retType = llvm::PointerType::get(*TheContext, 0);

  llvm::FunctionType *fnType =
      llvm::FunctionType::get(retType, paramLLVMTypes, false);

  std::vector<ProvidedArgument> provided;
  provided.reserve(argValues.size());
  const auto &callExprArgs = callExpr.getArgs();
  const auto &argNames = callExpr.getArgNames();
  const auto &argNameLocs = callExpr.getArgNameLocations();
  const auto &argEqualsLocs = callExpr.getArgEqualsLocations();

  for (std::size_t idx = 0; idx < argValues.size(); ++idx) {
    ProvidedArgument arg;
    arg.value = argValues[idx];
    arg.isRef = idx < argIsRef.size() ? argIsRef[idx] : false;
    if (idx > 0 && idx - 1 < callExprArgs.size()) {
      arg.expr = callExprArgs[idx - 1].get();
      if (idx - 1 < argNames.size())
        arg.name = argNames[idx - 1];
      if (idx - 1 < argNameLocs.size())
        arg.nameLoc = argNameLocs[idx - 1];
      if (idx - 1 < argEqualsLocs.size())
        arg.equalsLoc = argEqualsLocs[idx - 1];
    }
    provided.push_back(std::move(arg));
  }

  auto findParamsIndex = [](const std::vector<bool> &flags) -> int {
    for (size_t i = 0; i < flags.size(); ++i) {
      if (flags[i])
        return static_cast<int>(i);
    }
    return -1;
  };

  const int paramsIndex = findParamsIndex(memberInfo.parameterIsParams);
  std::vector<int> binding(paramCount, -1);
  std::vector<int> paramsBinding;
  size_t nextPositional = 0;
  std::set<std::string> seenNames;
  bool paramsNamed = false;

  for (std::size_t i = 0; i < provided.size(); ++i) {
    const ProvidedArgument &arg = provided[i];
    if (arg.name.empty()) {
      while (nextPositional < paramCount && binding[nextPositional] != -1)
        ++nextPositional;
      if (paramsIndex >= 0 &&
          nextPositional == static_cast<size_t>(paramsIndex)) {
        if (paramsNamed) {
          reportCompilerError("Too many arguments provided to call '" +
                              memberInfo.signature + "'");
          return nullptr;
        }
        for (size_t j = i; j < provided.size(); ++j) {
          if (!provided[j].name.empty()) {
            reportCompilerError("Positional argument cannot follow a named argument");
            return nullptr;
          }
          paramsBinding.push_back(static_cast<int>(j));
        }
        nextPositional = paramCount;
        break;
      }
      if (nextPositional >= paramCount) {
        reportCompilerError("Too many arguments provided to call '" +
                            memberInfo.signature + "'");
        return nullptr;
      }
      binding[nextPositional] = static_cast<int>(i);
      ++nextPositional;
      continue;
    }

    if (!seenNames.insert(arg.name).second) {
      ScopedErrorLocation scoped(arg.nameLoc);
      reportCompilerError("Duplicate argument for parameter '" + arg.name +
                          "'");
      return nullptr;
    }

    auto nameIt = std::find(memberInfo.parameterNames.begin(),
                            memberInfo.parameterNames.end(), arg.name);
    if (nameIt == memberInfo.parameterNames.end()) {
      ScopedErrorLocation scoped(arg.nameLoc);
      reportCompilerError("Unknown parameter name '" + arg.name +
                          "' for call to '" + memberInfo.signature + "'");
      return nullptr;
    }
    size_t paramIndex =
        static_cast<size_t>(nameIt - memberInfo.parameterNames.begin());
    if (paramsIndex >= 0 &&
        paramIndex == static_cast<size_t>(paramsIndex)) {
      if (paramsNamed || !paramsBinding.empty()) {
        ScopedErrorLocation scoped(arg.nameLoc);
        reportCompilerError("Duplicate argument for parameter '" + arg.name +
                            "'");
        return nullptr;
      }
      paramsNamed = true;
      paramsBinding.push_back(static_cast<int>(i));
      continue;
    }
    if (binding[paramIndex] != -1) {
      ScopedErrorLocation scoped(arg.nameLoc);
      reportCompilerError("Duplicate argument for parameter '" + arg.name +
                          "'");
      return nullptr;
    }
    binding[paramIndex] = static_cast<int>(i);
  }

  std::vector<llvm::Value *> resolvedArgs;
  std::vector<bool> resolvedIsRef;
  std::vector<std::unique_ptr<ExprAST>> ownedDefaultExprs;
  resolvedArgs.reserve(paramCount);
  resolvedIsRef.reserve(paramCount);

  for (std::size_t idx = 0; idx < paramCount; ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex)) {
      bool directAllowed = paramsBinding.size() == 1;
      if (directAllowed) {
        const auto &arg =
            provided[static_cast<size_t>(paramsBinding.front())];
        if (arg.isRef)
          directAllowed = false;

        const ExprAST *coreArg = unwrapRefExpr(arg.expr);
        if (directAllowed && coreArg && !coreArg->getTypeName().empty()) {
          TypeInfo actualInfo =
              applyActiveTypeBindings(makeTypeInfo(coreArg->getTypeName()));
          if (!typeInfoEquals(memberInfo.parameterTypes[idx], actualInfo))
            directAllowed = false;
        }

        llvm::Type *expectedArrayType = fnType->getParamType(idx);
        llvm::Type *actualType = arg.value ? arg.value->getType() : nullptr;
        if (!actualType || actualType != expectedArrayType)
          directAllowed = false;
      }

      if (directAllowed) {
        const auto &arg =
            provided[static_cast<size_t>(paramsBinding.front())];
        resolvedArgs.push_back(arg.value);
        resolvedIsRef.push_back(arg.isRef);
      } else {
        llvm::Value *packed = emitPackedParamsArray(
            paramsBinding, provided, memberInfo.parameterTypes[idx],
            memberInfo.signature);
        if (!packed)
          return nullptr;
        resolvedArgs.push_back(packed);
        resolvedIsRef.push_back(false);
      }
      continue;
    }

    if (binding[idx] >= 0) {
      const ProvidedArgument &arg = provided[static_cast<size_t>(binding[idx])];
      resolvedArgs.push_back(arg.value);
      resolvedIsRef.push_back(arg.isRef);
      continue;
    }

    const bool hasDefault =
        idx < memberInfo.parameterDefaults.size() &&
        memberInfo.parameterDefaults[idx].isSet();
    if (!hasDefault) {
      const std::string paramName =
          idx < memberInfo.parameterNames.size()
              ? memberInfo.parameterNames[idx]
              : std::to_string(idx);
      reportCompilerError("Missing argument for parameter '" + paramName +
                          "' in call to '" + memberInfo.signature + "'");
      return nullptr;
    }

    ScopedErrorLocation scoped(
        idx < memberInfo.parameterDefaultLocations.size()
            ? memberInfo.parameterDefaultLocations[idx]
            : SourceLocation{});
    std::unique_ptr<ExprAST> defaultExpr =
        instantiateDefaultExpr(memberInfo.parameterDefaults[idx]);
    if (!defaultExpr) {
      const std::string paramName =
          idx < memberInfo.parameterNames.size()
              ? memberInfo.parameterNames[idx]
              : std::to_string(idx);
      reportCompilerError("Default value unavailable for parameter '" +
                          paramName + "'");
      return nullptr;
    }
    defaultExpr->setTypeName(typeNameFromInfo(memberInfo.parameterTypes[idx]));
    defaultExpr->markTemporary();
    llvm::Value *value = defaultExpr->codegen();
    if (!value)
      return nullptr;
    resolvedArgs.push_back(value);
    resolvedIsRef.push_back(
        idx < memberInfo.parameterIsRef.size()
            ? memberInfo.parameterIsRef[idx]
            : false);
    ownedDefaultExprs.push_back(std::move(defaultExpr));
  }

  std::vector<llvm::Value *> callOperands;
  callOperands.reserve(resolvedArgs.size());

  for (std::size_t idx = 0; idx < resolvedArgs.size(); ++idx) {
    llvm::Value *arg = resolvedArgs[idx];
    llvm::Type *expected = fnType->getParamType(idx);

    if (memberInfo.parameterIsRef[idx]) {
      if (expected && expected->isPointerTy() &&
          !arg->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = Builder->CreateAlloca(
            arg->getType(), nullptr,
            buildArcOpLabel(memberInfo.signature, "ref.arg"));
        Builder->CreateStore(arg, tmp);
        arg = tmp;
      }
      if (expected && expected->isPointerTy() &&
          arg->getType() != expected) {
        arg = Builder->CreateBitCast(
            arg, expected,
            buildArcOpLabel(memberInfo.signature, "ref.cast"));
      }
      callOperands.push_back(arg);
      continue;
    }

    if (arg->getType() != expected) {
      const std::string targetTypeName =
          typeNameFromInfo(memberInfo.parameterTypes[idx]);
      arg = castToType(arg, expected, targetTypeName);
      if (!arg)
        return nullptr;
    }
    callOperands.push_back(arg);
  }

  llvm::Value *typedFnPtr =
      Builder->CreateBitCast(functionPointer, pointerType(fnType),
                             "hybrid.dispatch.fn");

  if (fnType->getReturnType()->isVoidTy()) {
    llvm::Value *callVal =
        Builder->CreateCall(fnType, typedFnPtr, callOperands);
    callExpr.setTypeName("void");
    return callVal;
  }

  llvm::Value *callVal =
      Builder->CreateCall(fnType, typedFnPtr, callOperands, "calltmp");
  callExpr.setTypeName(typeNameFromInfo(memberInfo.returnType));
  return callVal;
}

llvm::Function *getInterfaceLookupFunction() {
  llvm::Function *fn =
      TheModule->getFunction("hybrid_lookup_interface_table");
  if (fn)
    return fn;

  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *voidPtrPtrTy = pointerType(voidPtrTy);

  llvm::FunctionType *fnTy =
      llvm::FunctionType::get(voidPtrPtrTy, {typeDescPtrTy, typeDescPtrTy}, false);
  fn = llvm::Function::Create(fnTy, llvm::Function::ExternalLinkage,
                              "hybrid_lookup_interface_table", TheModule.get());
  fn->setDoesNotThrow();
  return fn;
}

llvm::Type *getSizeType() {
  if (sizeof(void *) == 4)
    return llvm::Type::getInt32Ty(*TheContext);
  return llvm::Type::getInt64Ty(*TheContext);
}

llvm::FunctionCallee getHybridRetainFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(voidPtrTy, {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_retain", fnType);
}

llvm::FunctionCallee getHybridReleaseFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {voidPtrTy},
                              false);
  return TheModule->getOrInsertFunction("hybrid_release", fnType);
}

llvm::FunctionCallee getHybridAutoreleaseFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(voidPtrTy, {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_autorelease", fnType);
}

llvm::FunctionCallee getHybridDeallocFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {voidPtrTy},
                              false);
  return TheModule->getOrInsertFunction("hybrid_dealloc", fnType);
}

llvm::FunctionCallee getHybridAllocObjectFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType =
      llvm::FunctionType::get(voidPtrTy, {sizeTy, typeDescPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_alloc_object", fnType);
}

llvm::FunctionCallee getHybridAllocArrayFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType = llvm::FunctionType::get(
      voidPtrTy, {sizeTy, sizeTy, typeDescPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_alloc_array", fnType);
}

llvm::FunctionCallee getHybridArrayDescriptorFunction() {
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType =
      llvm::FunctionType::get(typeDescPtrTy, {}, false);
  return TheModule->getOrInsertFunction("hybrid_array_type_descriptor",
                                        fnType);
}

llvm::FunctionCallee getHybridArraySetReleaseFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *releaseFnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                              {voidPtrTy}, false);
  auto *releaseFnPtrTy = pointerType(releaseFnTy);
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext),
      {voidPtrTy, releaseFnPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_set_release",
                                        fnType);
}

llvm::FunctionCallee getHybridArrayReleaseRefSlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_release_ref_slot",
                                        fnType);
}

llvm::FunctionCallee getHybridArrayReleaseArraySlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_release_array_slot",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArrayRetainRefSlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_retain_ref_slot", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArrayRetainArraySlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_retain_array_slot",
                                        fnType);
}

llvm::FunctionCallee getHybridArrayResizeFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *cbPtrTy = getArrayReleaseCallbackPointerType();
  auto *fnType = llvm::FunctionType::get(
      voidPtrTy, {voidPtrTy, sizeTy, sizeTy, cbPtrTy, cbPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_resize", fnType);
}

llvm::FunctionCallee getHybridArcDebugConfigFunction() {
  auto *intTy = llvm::Type::getInt32Ty(*TheContext);
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext),
      {intTy, intTy, intTy, intTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_set_debug_flags",
                                        fnType);
}

llvm::FunctionCallee getHybridArcTraceLabelFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_trace_set_label",
                                        fnType);
}

llvm::FunctionCallee getHybridArcVerifyRuntimeFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_verify_object",
                                        fnType);
}

llvm::FunctionCallee getSharedControlCreateFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType =
      llvm::FunctionType::get(opaquePtrTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_create", fnType);
}

llvm::FunctionCallee getSharedControlRetainStrongFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_retain_strong", fnType);
}

llvm::FunctionCallee getSharedControlReleaseStrongFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_release_strong", fnType);
}

llvm::FunctionCallee getSharedControlReleaseWeakFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_release_weak", fnType);
}

llvm::FunctionCallee getSharedControlRetainWeakFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_retain_weak", fnType);
}

llvm::FunctionCallee getSharedControlLockFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType =
      llvm::FunctionType::get(opaquePtrTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_shared_control_lock",
                                        fnType);
}

llvm::FunctionCallee getSharedControlUseCountFunction() {
  auto *opaquePtrTy = pointerType();
  auto *intTy = llvm::Type::getInt32Ty(*TheContext);
  auto *fnType =
      llvm::FunctionType::get(intTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_use_count", fnType);
}

bool typeInfoIsConcrete(const TypeInfo &info) {
  if (info.isGenericParameter)
    return false;
  for (const auto &arg : info.typeArguments) {
    if (!typeInfoIsConcrete(arg))
      return false;
  }
  return true;
}

const CompositeTypeInfo *resolveSmartPointerMetadata(const TypeInfo &info) {
  if (!info.isSmartPointer())
    return nullptr;
  if (!typeInfoIsConcrete(info))
    return nullptr;
  std::string constructed =
      stripNullableAnnotations(typeNameFromInfo(info));
  if (constructed.empty())
    return nullptr;
  if (const CompositeTypeInfo *meta =
          lookupCompositeInfo(constructed, /*countHit=*/false))
    return meta;
  TypeInfo requested = makeTypeInfo(constructed);
  return materializeCompositeInstantiation(requested);
}

std::string buildArcOpLabel(std::string_view label, std::string_view op) {
  std::string name = "arc";
  if (!label.empty()) {
    name.push_back('.');
    name.append(label);
  }
  if (!op.empty()) {
    name.push_back('.');
    name.append(op);
  }
  return name;
}

llvm::Value *
selectArrayElementReleaseFunction(const TypeInfo &elementInfo,
                                  std::string_view label) {
  llvm::PointerType *cbPtrTy = getArrayReleaseCallbackPointerType();
  if (!elementInfo.requiresARC() || elementInfo.isSmartPointer())
    return llvm::ConstantPointerNull::get(cbPtrTy);
  llvm::FunctionCallee callee = elementInfo.isArray
                                    ? getHybridArrayReleaseArraySlotFunction()
                                    : getHybridArrayReleaseRefSlotFunction();
  llvm::Value *fnPtr = callee.getCallee();
  if (fnPtr->getType() != cbPtrTy) {
    fnPtr = Builder->CreateBitCast(
        fnPtr, cbPtrTy,
        buildArcOpLabel(label, "array.releasefn.cast"));
  }
  return fnPtr;
}

llvm::Value *
selectArrayElementRetainFunction(const TypeInfo &elementInfo,
                                 std::string_view label) {
  llvm::PointerType *cbPtrTy = getArrayReleaseCallbackPointerType();
  if (!elementInfo.requiresARC() || elementInfo.isSmartPointer())
    return llvm::ConstantPointerNull::get(cbPtrTy);
  llvm::FunctionCallee callee = elementInfo.isArray
                                    ? getHybridArrayRetainArraySlotFunction()
                                    : getHybridArrayRetainRefSlotFunction();
  llvm::Value *fnPtr = callee.getCallee();
  if (fnPtr->getType() != cbPtrTy) {
    fnPtr = Builder->CreateBitCast(
        fnPtr, cbPtrTy, buildArcOpLabel(label, "array.retainfn.cast"));
  }
  return fnPtr;
}

bool emitSmartPointerInitFromVariable(const TypeInfo &declaredInfo,
                                      llvm::Value *destPtr,
                                      VariableExprAST &sourceVar,
                                      std::string_view label) {
  if (!destPtr || !declaredInfo.isSmartPointer())
    return false;

  const CompositeTypeInfo *metadata =
      resolveSmartPointerMetadata(declaredInfo);
  if (!metadata)
    return false;

  std::string constructedName =
      stripNullableAnnotations(typeNameFromInfo(declaredInfo));
  llvm::StructType *declStructTy = nullptr;
  if (auto it = StructTypes.find(constructedName); it != StructTypes.end())
    declStructTy = it->second;

  const SmartPointerKind kind = declaredInfo.smartPointerKind;
  const bool preferMove = kind == SmartPointerKind::Unique;
  const std::string &helperName =
      preferMove ? metadata->smartPointerMoveHelper
                 : metadata->smartPointerCopyHelper;
  if (helperName.empty()) {
    if (preferMove) {
      reportCompilerError(
          "Internal error: missing smart pointer move helper for '" +
          stripNullableAnnotations(typeNameFromInfo(declaredInfo)) + "'");
    }
    return false;
  }

  llvm::Function *helperFn = TheModule->getFunction(helperName);
  if (!helperFn) {
    reportCompilerError(
        "Internal error: helper '" + helperName +
        "' has not been materialized for smart pointer '" +
        stripNullableAnnotations(typeNameFromInfo(declaredInfo)) + "'");
    return false;
  }

  llvm::Value *sourcePtr = sourceVar.codegen_ptr();
  if (!sourcePtr)
    return false;
  llvm::Type *expectedPtrTy =
      helperFn->getFunctionType()->getNumParams() >= 1
          ? helperFn->getFunctionType()->getParamType(0)
          : nullptr;
  auto *expectedPtr = llvm::dyn_cast_or_null<llvm::PointerType>(expectedPtrTy);
  if (!expectedPtr)
    return false;
  if (!sourcePtr->getType()->isPointerTy()) {
    llvm::Type *pointeeTy = declStructTy ? static_cast<llvm::Type *>(declStructTy)
                                         : sourcePtr->getType();
    llvm::Value *stored = sourcePtr;
    if (stored->getType() != pointeeTy) {
      stored = castToType(stored, pointeeTy, constructedName);
      if (!stored)
        return false;
    }

    llvm::AllocaInst *tmp = Builder->CreateAlloca(
        pointeeTy, nullptr,
        buildArcOpLabel(label, preferMove ? "smart.move.tmp"
                                          : "smart.copy.tmp"));
    Builder->CreateStore(stored, tmp);
    sourcePtr = tmp;
  }
  if (sourcePtr->getType() != expectedPtrTy) {
    sourcePtr = Builder->CreateBitCast(
        sourcePtr, expectedPtrTy,
        buildArcOpLabel(label, preferMove ? "smart.move.cast"
                                          : "smart.copy.cast"));
  }

  llvm::Value *result =
      Builder->CreateCall(helperFn, {sourcePtr},
                          buildArcOpLabel(label, preferMove ? "smart.move.call"
                                                            : "smart.copy.call"));

  llvm::StructType *structTy =
      llvm::dyn_cast<llvm::StructType>(result->getType());
  if (!structTy) {
    auto structIt = StructTypes.find(constructedName);
    if (structIt != StructTypes.end())
      structTy = structIt->second;
  }

  if (!structTy) {
    reportCompilerError("Initializer for '" +
                        stripNullableAnnotations(typeNameFromInfo(declaredInfo)) +
                        "' has incompatible smart pointer representation");
    return false;
  }

  if (!metadata->smartPointerDestroyHelper.empty()) {
    llvm::Function *destroyFn =
        TheModule->getFunction(metadata->smartPointerDestroyHelper);
    if (!destroyFn) {
      reportCompilerError("Internal error: missing smart pointer destroy helper '" +
                         metadata->smartPointerDestroyHelper + "'");
      return false;
    }

    llvm::Value *destroyArg = destPtr;
    llvm::Type *expectedTy = destroyFn->getFunctionType()->getParamType(0);
    if (expectedTy && destroyArg->getType() != expectedTy) {
      destroyArg = Builder->CreateBitCast(
          destroyArg, expectedTy, buildArcOpLabel(label, "smart.destroy.cast"));
    }
    Builder->CreateCall(destroyFn, {destroyArg});
  }

  llvm::Value *stored = result;
  if (stored->getType()->isPointerTy()) {
    stored = Builder->CreateLoad(
        structTy, stored, buildArcOpLabel(label, "smart.assign.load"));
  }
  if (stored->getType() != structTy) {
    reportCompilerError("Initializer for '" +
                        stripNullableAnnotations(typeNameFromInfo(declaredInfo)) +
                        "' has incompatible smart pointer representation");
    return false;
  }

  Builder->CreateStore(stored, destPtr);
  return true;
}

llvm::Value *computeArrayHeaderPointer(llvm::Value *arrayValue,
                                       std::string_view label) {
  if (!arrayValue)
    return nullptr;
  llvm::Value *dataPtr = nullptr;
  if (arrayValue->getType()->isStructTy()) {
    dataPtr = Builder->CreateExtractValue(
        arrayValue, 0, buildArcOpLabel(label, "array.data"));
  }
  if (!dataPtr)
    return nullptr;

  llvm::Value *voidPtr = Builder->CreateBitCast(
      dataPtr, pointerType(),
      buildArcOpLabel(label, "array.payload.void"));
  llvm::Value *isNull = Builder->CreateICmpEQ(
      voidPtr, llvm::ConstantPointerNull::get(pointerType()),
      buildArcOpLabel(label, "array.null"));

  llvm::BasicBlock *origin = Builder->GetInsertBlock();
  llvm::Function *parent = origin ? origin->getParent() : nullptr;
  if (!parent)
    return nullptr;
  llvm::BasicBlock *computeBB = llvm::BasicBlock::Create(
      *TheContext, buildArcOpLabel(label, "array.header.compute"), parent);
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(
      *TheContext, buildArcOpLabel(label, "array.header.merge"), parent);
  Builder->CreateCondBr(isNull, mergeBB, computeBB);

  Builder->SetInsertPoint(computeBB);
  llvm::Value *bytePtr = Builder->CreateBitCast(
      voidPtr, pointerType(llvm::Type::getInt8Ty(*TheContext)),
      buildArcOpLabel(label, "array.payload.byte"));
  llvm::Value *offset = llvm::ConstantInt::getSigned(
      getSizeType(), -static_cast<int64_t>(getArrayPayloadOffsetBytes()));
  llvm::Value *headerBytePtr = Builder->CreateInBoundsGEP(
      llvm::Type::getInt8Ty(*TheContext), bytePtr, offset,
      buildArcOpLabel(label, "array.header.byte"));
  llvm::Value *headerVoidPtr = Builder->CreateBitCast(
      headerBytePtr, pointerType(),
      buildArcOpLabel(label, "array.header.void"));
  Builder->CreateBr(mergeBB);

  Builder->SetInsertPoint(mergeBB);
  llvm::PHINode *phi = Builder->CreatePHI(
      pointerType(), 2, buildArcOpLabel(label, "array.header.phi"));
  phi->addIncoming(headerVoidPtr, computeBB);
  phi->addIncoming(llvm::ConstantPointerNull::get(pointerType()), origin);
  return phi;
}

llvm::Value *emitArrayRetainValue(llvm::Value *arrayValue,
                                  std::string_view label) {
  llvm::Value *headerPtr =
      computeArrayHeaderPointer(arrayValue,
                                buildArcOpLabel(label, "array.header"));
  if (!headerPtr)
    return arrayValue;

  const ArcDebugOptions &arcDebug = CG.arcDebug;
  if (arcDebug.runtimeTracing) {
    llvm::Value *labelConst = Builder->CreateGlobalString(
        std::string(label), buildArcOpLabel(label, "array.trace.label"));
    Builder->CreateCall(getHybridArcTraceLabelFunction(), {labelConst});
  }
  if (arcDebug.runtimeVerify) {
    Builder->CreateCall(getHybridArcVerifyRuntimeFunction(), {headerPtr});
  }
  Builder->CreateCall(getHybridRetainFunction(), {headerPtr},
                      buildArcOpLabel(label, "array.retain.call"));
  return arrayValue;
}

void emitArrayReleaseValue(llvm::Value *arrayValue, std::string_view label) {
  llvm::Value *headerPtr =
      computeArrayHeaderPointer(arrayValue,
                                buildArcOpLabel(label, "array.header"));
  if (!headerPtr)
    return;

  const ArcDebugOptions &arcDebug = CG.arcDebug;
  if (arcDebug.runtimeTracing) {
    llvm::Value *labelConst = Builder->CreateGlobalString(
        std::string(label), buildArcOpLabel(label, "array.trace.label"));
    Builder->CreateCall(getHybridArcTraceLabelFunction(), {labelConst});
  }
  if (arcDebug.runtimeVerify) {
    Builder->CreateCall(getHybridArcVerifyRuntimeFunction(), {headerPtr});
  }
  Builder->CreateCall(getHybridReleaseFunction(), {headerPtr});
}
