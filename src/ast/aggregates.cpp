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

static bool isCompositeSelfType(const TypeInfo &typeInfo,
                                const std::string &typeKey) {
  TypeInfo bound = applyActiveTypeBindings(typeInfo);
  finalizeTypeInfoMetadata(bound);

  if (bound.pointerDepth > 0 || bound.isArray)
    return false;

  std::string rhs = sanitizeCompositeLookupName(typeKey);
  if (!bound.baseTypeName.empty() && bound.baseTypeName == rhs)
    return true;

  std::string lhs = sanitizeCompositeLookupName(typeNameFromInfo(bound));
  return !lhs.empty() && lhs == rhs;
}

static bool validateConstRefSelfParameter(const Parameter &param,
                                          const std::string &typeKey,
                                          std::string_view opSymbol) {
  if (!param.IsRef) {
    reportCompilerError("Invalid signature for operator '" +
                        std::string(opSymbol) + "'",
                        "Parameter must be declared as 'const ref " + typeKey +
                            "'.");
    return false;
  }

  TypeInfo paramInfo = applyActiveTypeBindings(param.DeclaredType);
  finalizeTypeInfoMetadata(paramInfo);
  if (paramInfo.isMutable) {
    reportCompilerError("Invalid signature for operator '" +
                            std::string(opSymbol) + "'",
                        "Parameter must be declared as 'const ref " + typeKey +
                            "'.");
    return false;
  }

  if (!isCompositeSelfType(paramInfo, typeKey)) {
    reportCompilerError("Invalid signature for operator '" +
                            std::string(opSymbol) + "'",
                        "Parameter must be declared as 'const ref " + typeKey +
                            "'.");
    return false;
  }

  return true;
}

static bool validateOperatorOverloadSignature(const std::string &typeKey,
                                              const MethodDefinition &methodDef,
                                              const PrototypeAST &proto) {
  OverloadableOperator op = methodDef.getOperatorKind();
  if (op == OverloadableOperator::None)
    return true;

  if (methodDef.isStatic()) {
    reportCompilerError("Operator overload '" +
                        std::string(overloadableOperatorSymbol(op)) +
                        "' must be an instance method");
    return false;
  }

  const std::vector<Parameter> &params = proto.getArgs();
  const bool returnsByRef = proto.returnsByRef();
  TypeInfo returnInfo = applyActiveTypeBindings(proto.getReturnTypeInfo());
  finalizeTypeInfoMetadata(returnInfo);

  auto requireSelfReturn = [&](bool byRef) -> bool {
    if (returnsByRef != byRef || !isCompositeSelfType(returnInfo, typeKey)) {
      reportCompilerError(
          "Invalid signature for operator '" +
              std::string(overloadableOperatorSymbol(op)) + "'",
          std::string("Return type must be '") +
              (byRef ? "ref " : "") + typeKey + "'.");
      return false;
    }
    return true;
  };

  auto requireBoolReturn = [&]() -> bool {
    if (returnsByRef || stripNullableAnnotations(typeNameFromInfo(returnInfo)) !=
                            "bool") {
      reportCompilerError(
          "Invalid signature for operator '" +
              std::string(overloadableOperatorSymbol(op)) + "'",
          "Return type must be 'bool'.");
      return false;
    }
    return true;
  };

  auto requireSingleConstRefSelfParam = [&]() -> bool {
    if (params.size() != 1) {
      reportCompilerError("Invalid signature for operator '" +
                              std::string(overloadableOperatorSymbol(op)) + "'",
                          "Expected exactly one parameter declared as 'const ref " +
                              typeKey + "'.");
      return false;
    }
    return validateConstRefSelfParameter(params[0], typeKey,
                                         overloadableOperatorSymbol(op));
  };

  switch (op) {
  case OverloadableOperator::Assign:
    return requireSelfReturn(true) && requireSingleConstRefSelfParam();
  case OverloadableOperator::AddAssign:
  case OverloadableOperator::SubAssign:
  case OverloadableOperator::MulAssign:
  case OverloadableOperator::DivAssign:
  case OverloadableOperator::ModAssign:
  case OverloadableOperator::Add:
  case OverloadableOperator::Sub:
  case OverloadableOperator::Mul:
  case OverloadableOperator::Div:
  case OverloadableOperator::Mod:
    return requireSelfReturn(false) && requireSingleConstRefSelfParam();
  case OverloadableOperator::Equal:
  case OverloadableOperator::NotEqual:
  case OverloadableOperator::Less:
  case OverloadableOperator::Greater:
  case OverloadableOperator::LessEqual:
  case OverloadableOperator::GreaterEqual:
    return requireBoolReturn() && requireSingleConstRefSelfParam();
  case OverloadableOperator::Dereference:
  case OverloadableOperator::AddressOf:
    if (!params.empty()) {
      reportCompilerError("Invalid signature for operator '" +
                              std::string(overloadableOperatorSymbol(op)) + "'",
                          "Unary operator overloads must not declare parameters.");
      return false;
    }
    return true;
  case OverloadableOperator::Index:
    if (params.empty()) {
      reportCompilerError("Invalid signature for operator '[]'",
                          "Indexer overload must declare at least one parameter.");
      return false;
    }
    return true;
  case OverloadableOperator::None:
    break;
  }

  return true;
}

static ActiveCompositeContext *currentCompositeContextMutable() {
  if (CG.compositeContextStack.empty())
    return nullptr;
  return &CG.compositeContextStack.back();
}

class ScopedCompositeContext {
  CodegenContext &Ctx;
  bool Active = false;

public:
  ScopedCompositeContext(const std::string &name, MethodKind kind, bool isStatic,
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

//===----------------------------------------------------------------------===//
// Struct Code Generation
//===----------------------------------------------------------------------===//

const std::vector<bool> &StructAST::layoutParameterUsage() const {
  if (LayoutUsageComputed)
    return LayoutParameterUsage;

  LayoutUsageComputed = true;
  LayoutParameterUsage.assign(GenericParameters.size(), false);
  if (GenericParameters.empty())
    return LayoutParameterUsage;

  SemanticGenericParameterScope typeGenericScope(GenericParameters);
  std::function<void(const TypeInfo &)> markUsage =
      [&](const TypeInfo &type) {
        if (type.isGenericParameter) {
          auto it = std::find(GenericParameters.begin(),
                              GenericParameters.end(), type.baseTypeName);
          if (it != GenericParameters.end())
            LayoutParameterUsage[it - GenericParameters.begin()] = true;
        }
        for (const auto &arg : type.typeArguments)
          markUsage(arg);
      };

  for (const auto &field : Fields)
    markUsage(makeTypeInfo(field->getType()));
  for (const auto &prop : Properties) {
    if (!prop)
      continue;
    markUsage(makeTypeInfo(prop->getType()));
    for (const auto &param : prop->getParameters())
      markUsage(param.DeclaredType);
  }
  for (const auto &base : BaseTypes)
    markUsage(makeTypeInfo(base));
  if (BaseClass)
    markUsage(makeTypeInfo(*BaseClass));
  for (const auto &iface : InterfaceTypes)
    markUsage(makeTypeInfo(iface));
  for (const auto &method : Methods) {
    PrototypeAST *proto = method.getPrototype();
    if (!proto)
      continue;
    markUsage(proto->getReturnTypeInfo());
    for (const auto &param : proto->getArgs())
      markUsage(param.DeclaredType);
  }

  return LayoutParameterUsage;
}

// Generate code for struct definitions
llvm::Type *StructAST::codegen() {
  const AggregateKind kind = Kind;
  const std::string originalName = Name;

  std::optional<StructNameOverrideScope> nameOverride;
  if (isGenericTemplate()) {
    if (const auto *instCtx = currentInstantiationContext())
      nameOverride.emplace(Name, instCtx->nameOverride);
  }

  const std::string typeKey = Name;
  const std::string definitionKey =
      isGenericTemplate() ? originalName : typeKey;

  // Check if this struct type already exists
  if (StructTypes.contains(typeKey)) {
    if (isGenericTemplate() && currentInstantiationContext())
      return StructTypes[typeKey];
    reportCompilerError(std::string(kind == AggregateKind::Struct ? "Struct" : "Class") +
                        " type already defined: " + typeKey);
    return nullptr;
  }

  if (!ensureNoDuplicateGenericParameters(GenericParameters, "type '" + typeKey + "'"))
    return nullptr;

  currentAnalysis().analyzeAggregate(*this);
  
  // Save the current insertion point to restore it after generating constructors
  auto SavedInsertBlock = Builder->GetInsertBlock();
  
  // Reserve an opaque struct so recursive field lookups can see it
  llvm::StructType *StructType = llvm::StructType::create(*TheContext, typeKey);
  StructTypes[typeKey] = StructType;

  auto abandonStructDefinition = [&]() {
    StructTypes.erase(typeKey);
    StructFieldIndices.erase(typeKey);
    StructFieldTypes.erase(typeKey);
  };

  std::vector<llvm::Type *> FieldTypes;
  std::vector<std::pair<std::string, unsigned>> FieldIndices;
  std::map<std::string, std::string> FieldTypeMap;

  SemanticGenericParameterScope typeGenericScope(GenericParameters);
  GenericDefinitionInfo currentTypeDefinition{definitionKey, &GenericParameters};
  GenericDefinitionScope definitionScope(&currentTypeDefinition);

  if (!Delegates.empty() && isGenericTemplate()) {
    reportCompilerError("Delegates inside generic types are not supported yet");
    abandonStructDefinition();
    return nullptr;
  }

  for (const auto &delegateDecl : Delegates) {
    if (!delegateDecl || !delegateDecl->codegen()) {
      abandonStructDefinition();
      return nullptr;
    }
  }

  std::vector<TypeInfo> effectiveBaseTypeInfos =
      applyActiveTypeBindingsToInfos(BaseTypeInfos);
  std::optional<TypeInfo> effectiveBaseClassInfo =
      applyActiveTypeBindingsToOptionalInfo(BaseClassInfo);
  std::vector<TypeInfo> effectiveInterfaceInfos =
      applyActiveTypeBindingsToInfos(InterfaceTypeInfos);

  for (const auto &baseInfo : effectiveBaseTypeInfos) {
    const std::string sanitizedName =
        stripNullableAnnotations(typeNameFromInfo(baseInfo));
    std::string description =
        "base type '" + sanitizedName + "' of " +
        describeAggregateKind(kind) + " '" + typeKey + "'";
    if (!validateTypeForGenerics(baseInfo, description, &currentTypeDefinition)) {
      abandonStructDefinition();
      return nullptr;
    }
  }

  std::vector<std::string> effectiveBaseTypes;
  effectiveBaseTypes.reserve(effectiveBaseTypeInfos.size());
  for (const auto &info : effectiveBaseTypeInfos)
    effectiveBaseTypes.push_back(
        stripNullableAnnotations(typeNameFromInfo(info)));

  std::optional<std::string> effectiveBaseClass;
  if (effectiveBaseClassInfo)
    effectiveBaseClass =
        stripNullableAnnotations(typeNameFromInfo(*effectiveBaseClassInfo));

  std::vector<std::string> effectiveInterfaces;
  effectiveInterfaces.reserve(effectiveInterfaceInfos.size());
  for (const auto &info : effectiveInterfaceInfos)
    effectiveInterfaces.push_back(
        stripNullableAnnotations(typeNameFromInfo(info)));

  std::map<std::string, std::map<std::string, TypeInfo>>
      baseTypeArgumentBindings;
  for (const auto &info : effectiveBaseTypeInfos) {
    if (!info.hasTypeArguments())
      continue;

    std::string baseKey =
        stripNullableAnnotations(typeNameFromInfo(info));
    std::vector<std::string> parameterNames;
    if (const CompositeTypeInfo *baseMeta = lookupCompositeInfo(baseKey)) {
      if (!baseMeta->genericParameters.empty())
        parameterNames = baseMeta->genericParameters;
    }
    if (parameterNames.empty()) {
      if (StructAST *templateAst = FindGenericTemplate(info.baseTypeName))
        parameterNames = templateAst->getGenericParameters();
    }
    if (parameterNames.size() != info.typeArguments.size())
      continue;

    std::map<std::string, TypeInfo> mapping;
    for (size_t i = 0; i < parameterNames.size(); ++i)
      mapping.emplace(parameterNames[i], info.typeArguments[i]);
    baseTypeArgumentBindings.emplace(baseKey, std::move(mapping));
  }

  CompositeTypeInfo compositeInfo;
  compositeInfo.kind = kind;
  compositeInfo.baseTypes = effectiveBaseTypes;
  compositeInfo.genericParameters = GenericParameters;
  compositeInfo.baseClass = effectiveBaseClass;
  compositeInfo.interfaces = effectiveInterfaces;
  compositeInfo.resolvedBaseTypeInfos = effectiveBaseTypeInfos;
  compositeInfo.resolvedBaseClassInfo = effectiveBaseClassInfo;
  compositeInfo.resolvedInterfaceTypeInfos = effectiveInterfaceInfos;
  compositeInfo.resolvedBaseTypeArgumentBindings =
      std::move(baseTypeArgumentBindings);
  compositeInfo.isAbstract = IsAbstract;
  compositeInfo.isInterface = (kind == AggregateKind::Interface);
  if (const auto *bindings = currentTypeBindings())
    compositeInfo.typeArgumentBindings = *bindings;
  else
    compositeInfo.typeArgumentBindings.clear();
  compositeInfo.hasClassDescriptor = true;
  compositeInfo.descriptor.name = typeKey;
  compositeInfo.descriptor.kind = kind;
  compositeInfo.descriptor.baseClassName = effectiveBaseClass;
  compositeInfo.descriptor.interfaceNames = effectiveInterfaces;
  compositeInfo.descriptor.inheritanceChain =
      buildInheritanceChain(effectiveBaseClass);
  compositeInfo.descriptor.isAbstract = IsAbstract;
  compositeInfo.descriptor.isInterface = (kind == AggregateKind::Interface);
  compositeInfo.descriptor.constructors.clear();
  compositeInfo.destructorVtableSlot = std::numeric_limits<unsigned>::max();

  if (compositeInfo.hasClassDescriptor) {
    llvm::StructType *TypeDescTy = getTypeDescriptorType();
    std::string descriptorName =
        makeRuntimeSymbolName("__hybrid_type_descriptor$", typeKey);
    llvm::GlobalVariable *descriptorGV =
        TheModule->getGlobalVariable(descriptorName, true);
    if (!descriptorGV) {
      descriptorGV = new llvm::GlobalVariable(
          *TheModule, TypeDescTy, false, llvm::GlobalValue::InternalLinkage,
          llvm::Constant::getNullValue(TypeDescTy), descriptorName);
    }
    compositeInfo.descriptorGlobalName = descriptorName;
  }

  unsigned FieldIndex = 0;
  if (kind == AggregateKind::Class) {
    if (effectiveBaseClass) {
      auto baseStructIt = StructTypes.find(*effectiveBaseClass);
      if (baseStructIt == StructTypes.end()) {
        reportCompilerError("Base class '" + *effectiveBaseClass +
                            "' must be defined before derived class '" + typeKey +
                            "'");
        abandonStructDefinition();
        return nullptr;
      }

      llvm::StructType *BaseStruct = baseStructIt->second;
      auto baseElements = BaseStruct->elements();
      FieldTypes.reserve(baseElements.size() + Fields.size());
      for (llvm::Type *Elem : baseElements)
        FieldTypes.push_back(Elem);
      FieldIndex = BaseStruct->getNumElements();

      auto baseIndicesIt = StructFieldIndices.find(*effectiveBaseClass);
      if (baseIndicesIt != StructFieldIndices.end())
        FieldIndices = baseIndicesIt->second;

      auto baseTypeMapIt = StructFieldTypes.find(*effectiveBaseClass);
      if (baseTypeMapIt != StructFieldTypes.end())
        FieldTypeMap = baseTypeMapIt->second;

      if (const CompositeTypeInfo *baseInfo =
              lookupCompositeInfo(*effectiveBaseClass)) {
        compositeInfo.fieldModifiers = baseInfo->fieldModifiers;
        compositeInfo.fieldDeclarationInitializers =
            baseInfo->fieldDeclarationInitializers;
        compositeInfo.hasARCHeader = baseInfo->hasARCHeader;
        compositeInfo.headerFieldIndex = baseInfo->headerFieldIndex;
        compositeInfo.destructorVtableSlot =
            baseInfo->destructorVtableSlot;
      } else {
        compositeInfo.hasARCHeader = true;
        compositeInfo.headerFieldIndex = 0;
      }
    } else {
      llvm::StructType *HeaderType = getArcHeaderType();
      FieldTypes.reserve(Fields.size() + 1);
      FieldTypes.push_back(HeaderType);
      FieldIndex = 1;
      compositeInfo.hasARCHeader = true;
      compositeInfo.headerFieldIndex = 0;
    }
  } else if (kind == AggregateKind::Struct) {
    llvm::StructType *HeaderType = getArcHeaderType();
    FieldTypes.reserve(Fields.size() + 1);
    FieldTypes.push_back(HeaderType);
    FieldIndex = 1;
    compositeInfo.hasARCHeader = true;
    compositeInfo.headerFieldIndex = 0;
  } else {
    FieldTypes.reserve(Fields.size());
  }

  FieldIndices.reserve(FieldIndices.size() + Fields.size());
  for (const auto &Field : Fields) {
    TypeInfo fieldTypeInfo = Field->getTypeInfo();
    fieldTypeInfo = applyActiveTypeBindings(fieldTypeInfo);
    std::string FieldTypeName = typeNameFromInfo(fieldTypeInfo);
    ParsedTypeDescriptor FieldDesc = parseTypeString(FieldTypeName);
    if (FieldDesc.sanitized == typeKey && FieldDesc.pointerDepth == 0 &&
        !FieldDesc.isNullable && !FieldDesc.isArray) {
      reportCompilerError(
          "Struct '" + typeKey + "' cannot contain non-nullable field '" +
              Field->getName() + "' of its own type",
          "Use a nullable or pointer field to avoid infinite size.");
      abandonStructDefinition();
      return nullptr;
    }

    if (!validateTypeForGenerics(fieldTypeInfo,
                                 "field '" + Field->getName() + "' of type '" + typeKey + "'",
                                 &currentTypeDefinition)) {
      abandonStructDefinition();
      return nullptr;
    }

    llvm::Type *FieldType = getTypeFromString(FieldTypeName);
    if (!FieldType) {
      reportCompilerError("Unknown field type '" + FieldTypeName + "' in struct '" + Name + "'");
      abandonStructDefinition();
      return nullptr;
    }

    const bool isStatic =
        static_cast<uint8_t>(Field->getModifiers().storage & StorageFlag::Static) != 0;
    const bool hasInitializer = Field->hasInitializer();
    if (isStatic) {
      llvm::Constant *Init = llvm::Constant::getNullValue(FieldType);
      if (hasInitializer) {
        ConstantValue constVal(0LL);
        SourceLocation failLoc{};
        if (!EvaluateConstantExpression(Field->getInitializer(), constVal,
                                        &failLoc)) {
          ScopedErrorLocation scoped(failLoc);
          reportCompilerError("Static field initializer for '" + Field->getName() +
                                  "' must be a compile-time constant expression");
          abandonStructDefinition();
          return nullptr;
        }

        Init = constantValueToLLVM(constVal, FieldType, FieldTypeName);
        if (!Init) {
          abandonStructDefinition();
          return nullptr;
        }
      }

      std::string GlobalName = typeKey + "." + Field->getName();

      auto *GV = new llvm::GlobalVariable(
          *TheModule, FieldType,
          static_cast<uint8_t>(Field->getModifiers().storage & StorageFlag::Const) != 0,
          llvm::GlobalValue::InternalLinkage, Init, GlobalName);

      CG.globalValues[GlobalName] = GV;
      rememberGlobalType(GlobalName, fieldTypeInfo);

      compositeInfo.staticFieldTypes[Field->getName()] = FieldTypeName;
      compositeInfo.staticFieldModifiers[Field->getName()] = Field->getModifiers();
      compositeInfo.staticFieldGlobals[Field->getName()] = GlobalName;
      if (hasInitializer) {
        compositeInfo.staticDeclarationInitializers.insert(Field->getName());
        markStaticFieldInitialized(typeKey, Field->getName());
      }
      continue;
    }

    FieldTypes.push_back(FieldType);
    FieldIndices.emplace_back(Field->getName(), FieldIndex++);
    FieldTypeMap[Field->getName()] = FieldTypeName;
    compositeInfo.fieldModifiers[Field->getName()] = Field->getModifiers();
    if (hasInitializer)
      compositeInfo.fieldDeclarationInitializers.insert(Field->getName());
  }

  StructType->setBody(FieldTypes);
  StructFieldIndices[typeKey] = FieldIndices;
  StructFieldTypes[typeKey] = FieldTypeMap;
  compositeInfo.fieldTypes = FieldTypeMap;

  CG.compositeMetadata[typeKey] = std::move(compositeInfo);
  CompositeTypeInfo &metadata = CG.compositeMetadata[typeKey];
  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseInfo =
            lookupCompositeInfo(*metadata.baseClass)) {
      for (const auto &entry : baseInfo->methodInfo)
        metadata.methodInfo.emplace(entry.first, entry.second);
      for (const auto &entry : baseInfo->operatorMethodNames)
        metadata.operatorMethodNames.emplace(entry.first, entry.second);
    }
  }
  for (const std::string &ifaceName : metadata.interfaces) {
    if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName)) {
      for (const auto &entry : ifaceInfo->methodInfo)
        metadata.methodInfo.emplace(entry.first, entry.second);
      for (const auto &entry : ifaceInfo->operatorMethodNames)
        metadata.operatorMethodNames.emplace(entry.first, entry.second);
    }
  }
  if (!validateCompositeHierarchy(typeKey, metadata)) {
    abandonStructDefinition();
    return nullptr;
  }

  if (metadata.baseClass) {
    if (const CompositeTypeInfo *baseInfo =
            lookupCompositeInfo(*metadata.baseClass)) {
      metadata.properties = baseInfo->properties;
      if (baseInfo->indexer)
        metadata.indexer = baseInfo->indexer;
    }
  }

  if (metadata.kind == AggregateKind::Interface) {
    for (const std::string &ifaceName : metadata.interfaces) {
      if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName)) {
        for (const auto &entry : ifaceInfo->properties)
          metadata.properties.emplace(entry.first, entry.second);
        if (!metadata.indexer && ifaceInfo->indexer)
          metadata.indexer = ifaceInfo->indexer;
      }
    }
  }

  std::vector<MethodDefinition> accessorMethods;
  std::vector<std::optional<std::string>> accessorPropertyNames;
  accessorMethods.reserve(Properties.size() * 2);
  accessorPropertyNames.reserve(Properties.size() * 2);

  auto cloneIndexerParams =
      [&](const std::vector<Parameter> &params) -> std::vector<Parameter> {
    std::vector<Parameter> result;
    result.reserve(params.size());
    for (const auto &param : params) {
      Parameter copy;
      copy.Type = param.Type;
      copy.Name = param.Name;
      copy.IsRef = param.IsRef;
      copy.IsParams = param.IsParams;
      copy.DeclaredType = param.DeclaredType;
      copy.NameLocation = param.NameLocation;
      copy.ParamsLocation = param.ParamsLocation;
      copy.DefaultEqualsLocation = param.DefaultEqualsLocation;
      copy.HasDefault = false;
      result.push_back(std::move(copy));
    }
    return result;
  };

  auto appendAccessor =
      [&](PropertyAST &prop, AccessorAST *accessor, bool isGetter,
          const std::string &displayName) -> bool {
    if (!accessor)
      return true;

    std::vector<Parameter> params;
    if (prop.isIndexer())
      params = cloneIndexerParams(prop.getParameters());

    TypeInfo returnInfo = isGetter ? prop.getTypeInfo() : makeTypeInfo("void");
    if (!isGetter) {
      Parameter valueParam;
      valueParam.Type = prop.getType();
      valueParam.Name = "value";
      valueParam.IsRef = false;
      valueParam.DeclaredType = prop.getTypeInfo();
      params.push_back(std::move(valueParam));
    }

    std::string qualifiedName = typeKey + "." + displayName;
    auto proto = std::make_unique<PrototypeAST>(
        std::move(returnInfo), qualifiedName, std::move(params), false, false);

    const bool accessorIsAbstract =
        prop.getModifiers().isAbstract || metadata.kind == AggregateKind::Interface;

    if (accessorIsAbstract) {
      accessorMethods.emplace_back(std::move(proto), prop.getModifiers(),
                                   MethodKind::Regular, displayName);
    } else {
      std::vector<std::unique_ptr<StmtAST>> stmts;
      stmts.push_back(std::make_unique<AccessorBodyStmtAST>(
          accessor, typeKey, prop.getName(), prop.isStatic()));
      auto body = std::make_unique<BlockStmtAST>(std::move(stmts));
      auto func =
          std::make_unique<FunctionAST>(std::move(proto), std::move(body));
      accessorMethods.emplace_back(std::move(func), prop.getModifiers(),
                                   MethodKind::Regular, displayName);
    }

    accessorPropertyNames.push_back(prop.getName());
    return true;
  };

  auto methodNameCollides = [&](const std::string &name) -> bool {
    for (const auto &method : Methods) {
      if (method.getDisplayName() == name)
        return true;
    }
    return false;
  };

  for (const auto &propPtr : Properties) {
    if (!propPtr)
      continue;
    PropertyAST &prop = *propPtr;

    TypeInfo propType = applyActiveTypeBindings(prop.getTypeInfo());
    if (!validateTypeForGenerics(
            propType,
            "property '" + prop.getName() + "' of type '" + typeKey + "'",
            &currentTypeDefinition)) {
      abandonStructDefinition();
      return nullptr;
    }

    if (prop.isIndexer()) {
      for (const auto &param : prop.getParameters()) {
        TypeInfo paramType = applyActiveTypeBindings(param.DeclaredType);
        if (!validateTypeForGenerics(
                paramType,
                "indexer parameter '" + param.Name + "' of type '" + typeKey +
                    "'",
                &currentTypeDefinition)) {
          abandonStructDefinition();
          return nullptr;
        }
      }
    }

    const bool isIndexer = prop.isIndexer();
    const std::string propName = prop.getName();
    const std::string getterName =
        isIndexer ? "__indexer_get" : "__get_" + propName;
    const std::string setterName =
        isIndexer ? "__indexer_set" : "__set_" + propName;

    if (prop.hasGetter() && methodNameCollides(getterName)) {
      reportCompilerError("Property accessor '" + getterName +
                          "' conflicts with an existing method in '" +
                          typeKey + "'");
      abandonStructDefinition();
      return nullptr;
    }
    if (prop.hasSetter() && methodNameCollides(setterName)) {
      reportCompilerError("Property accessor '" + setterName +
                          "' conflicts with an existing method in '" +
                          typeKey + "'");
      abandonStructDefinition();
      return nullptr;
    }

    PropertyInfo propInfo;
    propInfo.type = std::move(propType);
    propInfo.modifiers = prop.getModifiers();
    propInfo.isStatic = prop.isStatic();
    propInfo.hasGetter = prop.hasGetter();
    propInfo.hasSetter = prop.hasSetter();
    propInfo.isIndexer = isIndexer;
    propInfo.getterName = prop.hasGetter() ? getterName : "";
    propInfo.setterName = prop.hasSetter() ? setterName : "";

    if (isIndexer) {
      const auto &params = prop.getParameters();
      propInfo.parameterTypes.reserve(params.size());
      propInfo.parameterIsRef.reserve(params.size());
      propInfo.parameterNames.reserve(params.size());
      propInfo.parameterDefaults.reserve(params.size());
      propInfo.parameterDefaultLocations.reserve(params.size());
      for (const auto &param : params) {
        propInfo.parameterTypes.push_back(
            applyActiveTypeBindings(param.DeclaredType));
        propInfo.parameterIsRef.push_back(param.IsRef);
        propInfo.parameterNames.push_back(param.Name);
        propInfo.parameterDefaults.push_back(param.ResolvedDefault);
        propInfo.parameterDefaultLocations.push_back(
            param.DefaultEqualsLocation);
      }
      metadata.indexer = propInfo;
    } else {
      metadata.properties[propName] = propInfo;
    }

    if (prop.hasGetter()) {
      if (!appendAccessor(prop, prop.getGetter(), true, getterName))
        return nullptr;
    }
    if (prop.hasSetter()) {
      if (!appendAccessor(prop, prop.getSetter(), false, setterName))
        return nullptr;
    }
  }

  auto preRegisterAccessor = [&](MethodDefinition &MethodDef) -> bool {
    PrototypeAST *Proto = MethodDef.getPrototype();
    if (!Proto)
      return true;

    if (MethodDef.needsInstanceThis() && !MethodDef.hasImplicitThis()) {
      Parameter thisParam;
      thisParam.Type = typeKey;
      thisParam.Name = "__hybrid_this";
      thisParam.IsRef = true;
      TypeInfo thisInfo = makeTypeInfo(typeKey);
      thisInfo.refStorage = RefStorageClass::RefAlias;
      thisInfo.declaredRef = true;
      thisParam.DeclaredType = thisInfo;
      Proto->prependImplicitParameter(std::move(thisParam));
      MethodDef.markImplicitThisInjected();
    }

    if (!resolveParameterDefaults(Proto->getMutableArgs(), Proto->getName()))
      return false;

    CompositeMemberInfo memberInfo;
    memberInfo.modifiers = MethodDef.getModifiers();
    memberInfo.overloadedOperator = MethodDef.getOperatorKind();
    memberInfo.signature = Proto->getName();
    memberInfo.dispatchKey =
        makeMethodSignatureKey(MethodDef.getDisplayName(), *Proto, true);
    memberInfo.returnType = applyActiveTypeBindings(Proto->getReturnTypeInfo());
    memberInfo.parameterTypes = gatherParamTypes(Proto->getArgs());
    memberInfo.parameterIsRef = gatherParamRefFlags(Proto->getArgs());
    memberInfo.parameterIsParams = gatherParamParamsFlags(Proto->getArgs());
    memberInfo.parameterNames.reserve(Proto->getArgs().size());
    memberInfo.parameterDefaults.reserve(Proto->getArgs().size());
    memberInfo.parameterDefaultLocations.reserve(Proto->getArgs().size());
    for (const auto &param : Proto->getArgs()) {
      memberInfo.parameterNames.push_back(param.Name);
      memberInfo.parameterDefaults.push_back(param.ResolvedDefault);
      memberInfo.parameterDefaultLocations.push_back(
          param.DefaultEqualsLocation);
    }
    memberInfo.returnsByRef = Proto->returnsByRef();
    memberInfo.isGenericTemplate = false;
    memberInfo.genericArity = 0;
    if (MethodDef.hasBody())
      memberInfo.mangledName = Proto->getMangledName();
    metadata.methodInfo[MethodDef.getDisplayName()] = std::move(memberInfo);
    return true;
  };

  for (auto &MethodDef : accessorMethods) {
    if (!preRegisterAccessor(MethodDef)) {
      abandonStructDefinition();
      return nullptr;
    }
  }

  std::map<std::string, MethodRequirement> interfaceRequirements;
  if (metadata.kind == AggregateKind::Class) {
    gatherInterfaceRequirements(metadata, interfaceRequirements);
    removeInterfaceRequirementsSatisfiedByHierarchy(typeKey, interfaceRequirements);
  }

  std::map<std::string, MethodRequirement> abstractRequirements;
  if (metadata.kind == AggregateKind::Class)
    collectAbstractBaseMethods(typeKey, abstractRequirements);
  if (!abstractRequirements.empty())
    removeAbstractRequirementsSatisfiedByHierarchy(typeKey, abstractRequirements);
  
  struct MethodDispatch {
    MethodDefinition *method = nullptr;
    std::optional<std::string> activeProperty;
  };

  std::vector<MethodDispatch> methodDispatches;
  methodDispatches.reserve(accessorMethods.size() + Methods.size());
  for (size_t i = 0; i < accessorMethods.size(); ++i) {
    methodDispatches.push_back({&accessorMethods[i],
                                accessorPropertyNames[i]});
  }
  for (auto &MethodDef : Methods)
    methodDispatches.push_back({&MethodDef, std::nullopt});

  std::vector<const MethodDefinition *> methodLayoutView;
  methodLayoutView.reserve(methodDispatches.size());
  for (const auto &entry : methodDispatches)
    methodLayoutView.push_back(entry.method);

  // Generate constructor and other methods
  for (auto &dispatch : methodDispatches) {
    MethodDefinition &MethodDef = *dispatch.method;
    const std::optional<std::string> &activeProperty =
        dispatch.activeProperty;
    if (MethodDef.getKind() == MethodKind::Constructor) {
      if (metadata.hasClassDescriptor) {
        ClassDescriptor::Constructor ctorMeta;
        ctorMeta.access = MethodDef.getModifiers().access;
        ctorMeta.isImplicit = false;
        metadata.descriptor.constructors.push_back(ctorMeta);
      }
      FunctionAST *Method = MethodDef.get();
      if (!Method)
        continue;

      PrototypeAST *CtorProto = Method->getProto();
      if (!CtorProto)
        continue;

      if (!CtorProto->getGenericParameters().empty()) {
        MethodDef.setPrototypeView(CtorProto);
        RegisterGenericFunctionTemplate(MethodDef.takeFunction());
        continue;
      }

      std::string ConstructorName = CtorProto->getMangledName();
      const auto &CtorArgs = CtorProto->getArgs();

      if (!resolveParameterDefaults(CtorProto->getMutableArgs(),
                                    CtorProto->getName()))
        return nullptr;

      std::vector<llvm::Type *> ParamTypes;
      ParamTypes.reserve(CtorArgs.size());
      for (const auto &Param : CtorArgs) {
        llvm::Type *ParamType = getTypeFromString(Param.DeclaredType.typeName);
        if (!ParamType)
          return nullptr;
        if (Param.IsRef)
          ParamType = llvm::PointerType::get(*TheContext, 0);
        ParamTypes.push_back(ParamType);
      }

      llvm::FunctionType *CtorFnType =
          llvm::FunctionType::get(StructType, ParamTypes, false);
      llvm::Function *ConstructorFunc = llvm::Function::Create(
          CtorFnType, llvm::Function::ExternalLinkage, ConstructorName,
          TheModule.get());

      unsigned argIndex = 0;
      for (auto &Arg : ConstructorFunc->args())
        Arg.setName(CtorArgs[argIndex++].Name);

      llvm::BasicBlock *BB =
          llvm::BasicBlock::Create(*TheContext, "entry", ConstructorFunc);
      Builder->SetInsertPoint(BB);

      llvm::AllocaInst *StructPtr =
          Builder->CreateAlloca(StructType, nullptr, "struct_alloc");
      Builder->CreateStore(llvm::Constant::getNullValue(StructType), StructPtr);

      llvm::Constant *DescriptorPtrConst = nullptr;
      if (metadata.hasARCHeader) {
        llvm::GlobalVariable *descriptorGV = nullptr;
        if (!metadata.descriptorGlobalName.empty())
          descriptorGV = TheModule->getGlobalVariable(
              metadata.descriptorGlobalName, true);
        if (!descriptorGV) {
          reportCompilerError("Internal error: descriptor for type '" + typeKey +
                              "' missing during constructor emission");
          ConstructorFunc->eraseFromParent();
          NamedValues.clear();
          LocalTypes.clear();
          NonNullFacts.clear();
          Builder->ClearInsertionPoint();
          return nullptr;
        }
        llvm::StructType *headerTy = getArcHeaderType();
        auto *int32Ty = llvm::Type::getInt32Ty(*TheContext);
        DescriptorPtrConst = llvm::ConstantExpr::getBitCast(
            descriptorGV, pointerType(getTypeDescriptorType()));
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            StructType, StructPtr, metadata.headerFieldIndex, "hybrid.header");
        llvm::Value *strongPtr = Builder->CreateStructGEP(
            headerTy, headerPtr, 0, "hybrid.header.strong");
        llvm::Value *weakPtr = Builder->CreateStructGEP(
            headerTy, headerPtr, 1, "hybrid.header.weak");
        llvm::Value *descPtr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, "hybrid.header.desc");
        Builder->CreateStore(llvm::ConstantInt::get(int32Ty, 1), strongPtr);
        Builder->CreateStore(llvm::ConstantInt::get(int32Ty, 0), weakPtr);
        Builder->CreateStore(DescriptorPtrConst, descPtr);
      }

      NamedValues.clear();
      LocalTypes.clear();
      NonNullFacts.clear();
      ensureBaseNonNullScope();
      NamedValues["this"] = StructPtr;
      rememberLocalType("this", makeTypeInfo(typeKey));

      ScopedCompositeContext methodScope(typeKey, MethodKind::Constructor, false);
      if (auto *ctx = currentCompositeContextMutable()) {
        ctx->baseClassName = metadata.baseClass;
        ctx->baseConstructorRequired = metadata.baseClass.has_value();
      }

      argIndex = 0;
      for (auto &Arg : ConstructorFunc->args()) {
        const auto &Param = CtorArgs[argIndex];
        std::string ArgName = Param.Name;

        llvm::AllocaInst *Alloca =
            Builder->CreateAlloca(Arg.getType(), nullptr, ArgName);
        Builder->CreateStore(&Arg, Alloca);

        NamedValues[ArgName] = Alloca;
        TypeInfo paramInfo = applyActiveTypeBindings(Param.DeclaredType);
        if (Param.IsRef) {
          paramInfo.refStorage = RefStorageClass::RefAlias;
          paramInfo.declaredRef = true;
        }
        rememberLocalType(ArgName, std::move(paramInfo));
        ++argIndex;
      }

      auto &ctorInitializers = MethodDef.getConstructorInitializers();
      if (metadata.baseClass) {
        bool hasExplicitBaseInit = std::ranges::any_of(
            ctorInitializers, [](const ConstructorInitializer &init) {
              return init.kind == ConstructorInitializer::Kind::Base;
            });
        if (!hasExplicitBaseInit &&
            hasParameterlessConstructor(*metadata.baseClass)) {
          ConstructorInitializer init;
          init.kind = ConstructorInitializer::Kind::Base;
          init.target = *metadata.baseClass;
          ctorInitializers.push_back(std::move(init));
        }
      }

      if (!emitConstructorInitializers(typeKey, StructType, StructPtr,
                                        metadata, ctorInitializers)) {
        ConstructorFunc->eraseFromParent();
        NamedValues.clear();
        LocalTypes.clear();
        NonNullFacts.clear();
        Builder->ClearInsertionPoint();
        return nullptr;
      }

      if (metadata.hasARCHeader && DescriptorPtrConst) {
        llvm::StructType *headerTy = getArcHeaderType();
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            StructType, StructPtr, metadata.headerFieldIndex,
            "hybrid.header.inits");
        llvm::Value *descPtr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, "hybrid.header.desc.inits");
        Builder->CreateStore(DescriptorPtrConst, descPtr);
      }

      if (Method->getBody()->codegen()) {
        if (!Builder->GetInsertBlock()->getTerminator()) {
          if (metadata.hasARCHeader && DescriptorPtrConst) {
            llvm::StructType *headerTy = getArcHeaderType();
            llvm::Value *headerPtr = Builder->CreateStructGEP(
                StructType, StructPtr, metadata.headerFieldIndex,
                "hybrid.header.final");
            llvm::Value *descPtr = Builder->CreateStructGEP(
                headerTy, headerPtr, 2, "hybrid.header.desc.final");
            Builder->CreateStore(DescriptorPtrConst, descPtr);
          }
          llvm::Value *StructValue =
              Builder->CreateLoad(StructType, StructPtr, "struct_value");
          Builder->CreateRet(StructValue);
        }

        llvm::verifyFunction(*ConstructorFunc);

        metadata.constructorMangledNames.push_back(ConstructorName);
        FunctionOverload *ctorEntry =
            registerFunctionOverload(*CtorProto, ConstructorName);
        if (!ctorEntry) {
          ConstructorFunc->eraseFromParent();
          NamedValues.clear();
          LocalTypes.clear();
          NonNullFacts.clear();
          Builder->ClearInsertionPoint();
          return nullptr;
        }
        ctorEntry->function = ConstructorFunc;
        if (CtorProto->getName() != typeKey) {
          std::vector<TypeInfo> ctorParamTypes = gatherParamTypes(CtorProto->getArgs());
          std::vector<bool> ctorParamIsRef = gatherParamRefFlags(CtorProto->getArgs());
          TypeInfo boundCtorReturn =
              applyActiveTypeBindings(CtorProto->getReturnTypeInfo());
          if (!findRegisteredOverload(typeKey, boundCtorReturn,
                                      CtorProto->returnsByRef(), ctorParamTypes,
                                      ctorParamIsRef)) {
            CG.functionOverloads[typeKey].push_back(*ctorEntry);
          }
        }
      } else {
        ConstructorFunc->eraseFromParent();
        NamedValues.clear();
        LocalTypes.clear();
        NonNullFacts.clear();
        Builder->ClearInsertionPoint();
        return nullptr;
      }

      NamedValues.clear();
      LocalTypes.clear();
      NonNullFacts.clear();
      continue;
    }
    if (MethodDef.getKind() == MethodKind::Destructor) {
      PrototypeAST *Proto = MethodDef.getPrototype();
      if (!Proto)
        continue;
      if (metadata.hasDestructor) {
        reportCompilerError("Type '" + typeKey +
                            "' cannot declare more than one destructor");
        return nullptr;
      }

      if (!Proto->getGenericParameters().empty()) {
        reportCompilerError("Destructor for '" + typeKey +
                            "' cannot declare generic parameters");
        return nullptr;
      }
      if (!Proto->getArgs().empty()) {
        reportCompilerError("Destructor for '" + typeKey +
                            "' cannot declare parameters");
        return nullptr;
      }
      const TypeInfo &dtorReturn = Proto->getReturnTypeInfo();
      if (Proto->returnsByRef() || dtorReturn.typeName != "void") {
        reportCompilerError("Destructor for '" + typeKey +
                            "' must return void");
        return nullptr;
      }

      if (MethodDef.needsInstanceThis() && !MethodDef.hasImplicitThis()) {
        Parameter thisParam;
        thisParam.Type = typeKey;
        thisParam.Name = "__hybrid_this";
        thisParam.IsRef = true;
        TypeInfo thisInfo = makeTypeInfo(typeKey);
        thisInfo.refStorage = RefStorageClass::RefAlias;
        thisInfo.declaredRef = true;
        thisParam.DeclaredType = thisInfo;
        Proto->prependImplicitParameter(std::move(thisParam));
        MethodDef.markImplicitThisInjected();
      }

      if (!MethodDef.hasBody()) {
        reportCompilerError("Destructor for '" + typeKey +
                            "' must declare a body");
        return nullptr;
      }

      FunctionAST *Method = MethodDef.get();
      if (!Method)
        continue;

      ScopedCompositeContext methodScope(typeKey, MethodKind::Destructor, false);
      llvm::Function *GeneratedFunction = Method->codegen();
      if (!GeneratedFunction)
        return nullptr;

      metadata.hasDestructor = true;
      metadata.destructorFunctionName = Proto->getMangledName();
      metadata.destructorModifiers = MethodDef.getModifiers();
      continue;
    }

    PrototypeAST *Proto = MethodDef.getPrototype();
    if (!Proto)
      continue;

    if (MethodDef.isOperatorOverload()) {
      if (!validateOperatorOverloadSignature(typeKey, MethodDef, *Proto))
        return nullptr;
    }

    const MemberModifiers &methodMods = MethodDef.getModifiers();

    if (metadata.kind == AggregateKind::Interface) {
      if (MethodDef.isStatic()) {
        reportCompilerError("Interface '" + Name +
                            "' cannot declare static method '" +
                            MethodDef.getDisplayName() + "'");
        return nullptr;
      }
      if (methodMods.isOverride) {
        reportCompilerError("Interface method '" + MethodDef.getDisplayName() +
                            "' cannot be marked override");
        return nullptr;
      }
    }

    if (methodMods.isAbstract && metadata.kind == AggregateKind::Class) {
      if (MethodDef.isStatic()) {
        reportCompilerError("Abstract method '" + MethodDef.getDisplayName() +
                            "' of class '" + Name +
                            "' cannot be static");
        return nullptr;
      }
      if (!metadata.isAbstract) {
        reportCompilerError("Class '" + Name + "' must be declared abstract to "
                            "contain abstract method '" +
                                MethodDef.getDisplayName() + "'");
        return nullptr;
      }
    }

    if (MethodDef.needsInstanceThis() && !MethodDef.hasImplicitThis()) {
      Parameter thisParam;
      thisParam.Type = typeKey;
      thisParam.Name = "__hybrid_this";
      thisParam.IsRef = true;
      TypeInfo thisInfo = makeTypeInfo(typeKey);
      thisInfo.refStorage = RefStorageClass::RefAlias;
      thisInfo.declaredRef = true;
      thisParam.DeclaredType = thisInfo;
      Proto->prependImplicitParameter(std::move(thisParam));
      MethodDef.markImplicitThisInjected();
    }

    if (!resolveParameterDefaults(Proto->getMutableArgs(), Proto->getName()))
      return nullptr;

    std::string overrideSignature;

    const std::string methodKey =
        makeMethodSignatureKey(MethodDef.getDisplayName(), *Proto, true);
    if (!interfaceRequirements.empty())
      interfaceRequirements.erase(methodKey);
    if (!abstractRequirements.empty())
      abstractRequirements.erase(methodKey);

    if (methodMods.isOverride) {
      if (MethodDef.isStatic()) {
        reportCompilerError("Static method '" + MethodDef.getDisplayName() +
                            "' of class '" + Name +
                            "' cannot be marked override");
        return nullptr;
      }

      auto baseMatch =
          findBaseMethodMatch(Name, MethodDef.getDisplayName(), *Proto);
      if (!baseMatch) {
        reportCompilerError("Method '" + MethodDef.getDisplayName() +
                                "' of class '" + Name +
                                "' does not override a base class method");
        return nullptr;
      }

      const CompositeMemberInfo &baseMember = *baseMatch->info;
      if (!baseMember.modifiers.isVirtual &&
          !baseMember.modifiers.isAbstract &&
          !baseMember.modifiers.isOverride) {
        reportCompilerError("Method '" + MethodDef.getDisplayName() +
                                "' of class '" + Name +
                                "' cannot override non-virtual member '" +
                                MethodDef.getDisplayName() + "' of class '" +
                                baseMatch->ownerType + "'");
        return nullptr;
      }

      if (!verifyOverrideDefaultCompatibility(
              *baseMatch->info, *Proto, baseMatch->ownerType,
              MethodDef.getDisplayName(), Name))
        return nullptr;

      overrideSignature = baseMember.signature;
    }

    if (MethodDef.getKind() == MethodKind::ThisOverride)
      metadata.thisOverride = Proto->getName();

    const bool methodIsGeneric =
        !Proto->getGenericParameters().empty();
    const bool methodHasBody = MethodDef.hasBody();

    CompositeMemberInfo memberInfo;
    memberInfo.modifiers = MethodDef.getModifiers();
    memberInfo.overloadedOperator = MethodDef.getOperatorKind();
    memberInfo.signature = Proto->getName();
    memberInfo.dispatchKey = methodKey;
    memberInfo.overridesSignature = overrideSignature;
    memberInfo.returnType = applyActiveTypeBindings(Proto->getReturnTypeInfo());
    memberInfo.parameterTypes = gatherParamTypes(Proto->getArgs());
    memberInfo.parameterIsRef = gatherParamRefFlags(Proto->getArgs());
    memberInfo.parameterIsParams = gatherParamParamsFlags(Proto->getArgs());
    memberInfo.parameterNames.reserve(Proto->getArgs().size());
    memberInfo.parameterDefaults.reserve(Proto->getArgs().size());
    memberInfo.parameterDefaultLocations.reserve(Proto->getArgs().size());
    for (const auto &param : Proto->getArgs()) {
      memberInfo.parameterNames.push_back(param.Name);
      memberInfo.parameterDefaults.push_back(param.ResolvedDefault);
      memberInfo.parameterDefaultLocations.push_back(
          param.DefaultEqualsLocation);
    }
    memberInfo.returnsByRef = Proto->returnsByRef();
    memberInfo.isGenericTemplate = methodIsGeneric;
    memberInfo.genericArity =
        static_cast<unsigned>(Proto->getGenericParameters().size());
    if (methodHasBody && !methodIsGeneric)
      memberInfo.mangledName = Proto->getMangledName();
    metadata.methodInfo[MethodDef.getDisplayName()] = std::move(memberInfo);
    if (MethodDef.isOperatorOverload()) {
      metadata.operatorMethodNames[MethodDef.getOperatorKind()] =
          MethodDef.getDisplayName();
    }

    if (!methodHasBody)
      continue;

    if (methodIsGeneric) {
      MethodDef.setPrototypeView(Proto);
      RegisterGenericFunctionTemplate(MethodDef.takeFunction());
      continue;
    }

    FunctionAST *Method = MethodDef.get();
    if (!Method)
      continue;

    ScopedCompositeContext methodScope(typeKey, MethodDef.getKind(),
                                       MethodDef.isStatic(), activeProperty);
    llvm::Function *GeneratedFunction = Method->codegen();
    if (GeneratedFunction) {
      auto infoIt = metadata.methodInfo.find(MethodDef.getDisplayName());
      if (infoIt != metadata.methodInfo.end())
        infoIt->second.directFunction = GeneratedFunction;
    }
  }

  if (metadata.kind == AggregateKind::Interface) {
    computeInterfaceMethodLayout(typeKey, methodLayoutView, metadata);
  } else if (metadata.kind == AggregateKind::Class) {
    if (!computeVirtualDispatchLayout(typeKey, metadata)) {
      abandonStructDefinition();
      return nullptr;
    }
  }
  
  // Restore the insertion point if had one, otherwise clear it
  if (SavedInsertBlock) {
    Builder->SetInsertPoint(SavedInsertBlock);
  } else {
    // No previous insertion point, at top level
    // Clear the insertion point so next top-level code creates its own context
    Builder->ClearInsertionPoint();
  }

  if (!interfaceRequirements.empty()) {
    for (const auto &entry : interfaceRequirements) {
      const MethodRequirement &req = entry.second;
      std::string signature = req.info ? req.info->signature : req.ownerType +
                                                       "." + entry.first;
      reportCompilerError("Class '" + typeKey +
                          "' does not implement interface member '" +
                          signature + "'");
    }
    return nullptr;
  }

  if (!abstractRequirements.empty() && metadata.kind == AggregateKind::Class &&
      !metadata.isAbstract) {
    for (const auto &entry : abstractRequirements) {
      const MethodRequirement &req = entry.second;
      std::string signature = req.info ? req.info->signature
                                       : req.ownerType + "." + entry.first;
      reportCompilerError("Class '" + typeKey +
                          "' must override abstract member '" + signature +
                          "' defined in '" + req.ownerType + "'");
    }
    return nullptr;
  }

  if (metadata.kind != AggregateKind::Interface) {
    if (!emitCompositeDealloc(typeKey, StructType, metadata)) {
      abandonStructDefinition();
      return nullptr;
    }
  }

  if (metadata.kind == AggregateKind::Interface) {
    if (!emitInterfaceDescriptor(typeKey, metadata)) {
      abandonStructDefinition();
      return nullptr;
    }
  } else if (metadata.kind == AggregateKind::Class ||
             metadata.kind == AggregateKind::Struct) {
    if (!emitClassRuntimeStructures(typeKey, StructType, metadata)) {
      abandonStructDefinition();
      return nullptr;
    }
  }

  return StructType;
}

// Generate code for member access expressions
llvm::Value *MemberAccessExprAST::codegen() {
  if (isDestructorAccess()) {
    reportCompilerError("Destructor access is only valid as a call expression");
    return nullptr;
  }

  const CompositeTypeInfo *info = nullptr;
  const MemberModifiers *modifiers = nullptr;
  const PropertyInfo *propertyInfo = nullptr;
  bool isStaticField = false;
  bool isTypeReference = false;
  std::string staticFieldType;
  std::string staticGlobalName;
  MemberModifiers defaultStaticModifiers;
  std::string StructLookupName;
  std::string ObjectTypeName;

  auto emitPropertyGetter =
      [&](const CompositeTypeInfo &meta, const PropertyInfo &prop,
          ExprAST *objectExpr, llvm::Value *instanceValue,
          const std::string &ownerName) -> llvm::Value * {
    if (!prop.hasGetter) {
      reportCompilerError("Property '" + MemberName +
                          "' does not define a getter");
      return nullptr;
    }
    if (!ensureMemberAccessAllowed(prop.modifiers, AccessIntent::Read,
                                   ownerName, MemberName))
      return nullptr;
    auto getterIt = meta.methodInfo.find(prop.getterName);
    if (getterIt == meta.methodInfo.end()) {
      reportCompilerError("Internal error: missing getter for property '" +
                          MemberName + "'");
      return nullptr;
    }
    return emitMemberCallByInfo(meta, getterIt->second, ownerName, objectExpr,
                                instanceValue, {}, {}, this);
  };

  if (auto *VarObj = dynamic_cast<VariableExprAST *>(Object.get())) {
    StructLookupName = VarObj->getName();
    info = lookupCompositeInfo(StructLookupName);
    if (info) {
      isTypeReference = true;
      if (!shouldBypassPropertyAccess(StructLookupName, MemberName)) {
        auto propIt = info->properties.find(MemberName);
        if (propIt != info->properties.end()) {
          propertyInfo = &propIt->second;
          if (!propertyInfo->isStatic) {
            reportCompilerError("Instance property '" + MemberName +
                                "' cannot be accessed on type '" +
                                StructLookupName + "'");
            return nullptr;
          }
          return emitPropertyGetter(*info, *propertyInfo, nullptr, nullptr,
                                    StructLookupName);
        }
      }
      if (auto staticIt = info->staticFieldModifiers.find(MemberName);
          staticIt != info->staticFieldModifiers.end()) {
        modifiers = &staticIt->second;
        isStaticField = true;
        if (auto typeIt = info->staticFieldTypes.find(MemberName);
            typeIt != info->staticFieldTypes.end())
          staticFieldType = typeIt->second;
        if (auto globIt = info->staticFieldGlobals.find(MemberName);
            globIt != info->staticFieldGlobals.end())
          staticGlobalName = globIt->second;
      }
    } else {
      StructLookupName.clear();
    }
  }

  llvm::Value *ObjectPtr = nullptr;
  if (!isStaticField) {
    ObjectPtr = Object->codegen();
    if (!ObjectPtr)
      return nullptr;

    ObjectTypeName = Object->getTypeName();
    ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
    std::string baseLookup = sanitizeBaseTypeName(ObjectTypeName);
    if (ObjectTypeDesc.isNullable) {
      return LogErrorV(
          ("Cannot access nullable type '" + ObjectTypeName +
           "' without null-safe operator")
              .c_str());
    }

    if (isSafeSmartArrow()) {
      TypeInfo arrowInfo = makeTypeInfo(ObjectTypeName);
      finalizeTypeInfoMetadata(arrowInfo);
      if (!arrowInfo.isSmartPointer()) {
        return LogErrorV(
            "'->' requires a smart pointer outside unsafe contexts");
      }
      const CompositeTypeInfo *smartMeta =
          resolveSmartPointerMetadata(arrowInfo);
      if (!smartMeta)
        return LogErrorV("Internal error: missing smart pointer metadata");
      const InstanceFieldInfo *payloadField =
          findInstanceField(*smartMeta, smartMeta->smartPointerKind == SmartPointerKind::Unique
                                           ? "value"
                                           : "payload");
      if (!payloadField)
        return LogErrorV("Internal error: missing smart pointer payload");
      llvm::StructType *smartTy =
          StructTypes[stripNullableAnnotations(typeNameFromInfo(arrowInfo))];
      if (!smartTy)
        return LogErrorV("Internal error: missing smart pointer struct type");
      llvm::Value *payloadVal = nullptr;
      if (ObjectPtr->getType()->isPointerTy()) {
        llvm::Value *payloadPtr = Builder->CreateStructGEP(
            smartTy, ObjectPtr, payloadField->index,
            "arrow.smart.payload.ptr");
        payloadVal = Builder->CreateLoad(
            getTypeFromString(typeNameFromInfo(payloadField->type)), payloadPtr,
            "arrow.smart.payload");
      } else {
        payloadVal = Builder->CreateExtractValue(
            ObjectPtr, payloadField->index, "arrow.smart.payload");
      }
      if (!payloadField->type.typeName.empty())
        ObjectTypeName = payloadField->type.typeName;
      ObjectPtr = payloadVal;
      ObjectTypeDesc = parseTypeString(ObjectTypeName);
      baseLookup = sanitizeBaseTypeName(ObjectTypeName);
    }

    if (MemberName == "size") {
      if (ObjectTypeDesc.isArray) {
        unsigned outerRank =
            ObjectTypeDesc.arrayRanks.empty() ? 1 : ObjectTypeDesc.arrayRanks.back();
        std::string elemTypeStr = removeLastArrayGroup(ObjectTypeDesc.sanitized);
        llvm::Type *elemTy = getTypeFromString(elemTypeStr);
        if (!elemTy)
          return LogErrorV("Unable to resolve array element type for size access");
        llvm::StructType *arrayStructTy = getArrayStructType(elemTy, outerRank);
        llvm::Value *arrayValue = ObjectPtr;
        if (arrayValue->getType()->isPointerTy()) {
          arrayValue = Builder->CreateLoad(arrayStructTy, ObjectPtr,
                                           "array.size.load");
        }
        llvm::Value *sizeVal =
            Builder->CreateExtractValue(arrayValue, 1, "array.size");
        setTypeName("int");
        return sizeVal;
      }

      if (baseLookup == "string") {
        llvm::Type *storageTy = getStringStorageType();
        llvm::Value *typedPtr = ObjectPtr;
        if (!typedPtr->getType()->isPointerTy()) {
          return LogErrorV("String size access requires reference-compatible storage");
        }
        typedPtr = Builder->CreateBitCast(
            typedPtr, pointerType(storageTy), "string.size.storage");
        llvm::Value *lenPtr = Builder->CreateStructGEP(
            storageTy, typedPtr, 1, "string.size.ptr");
        llvm::Value *lenVal = Builder->CreateLoad(
            getSizeType(), lenPtr, "string.size");
        llvm::Type *intTy = llvm::Type::getInt32Ty(*TheContext);
        if (lenVal->getType() != intTy)
          lenVal =
              Builder->CreateTruncOrBitCast(lenVal, intTy, "string.size.int");
        setTypeName("int");
        return lenVal;
      }
    }

    if (auto tupleInfoOpt = resolveTupleTypeInfo(Object.get());
        tupleInfoOpt && tupleInfoOpt->isTupleType()) {
      auto indexOpt = findTupleElementIndex(*tupleInfoOpt, MemberName);
      if (!indexOpt) {
        reportCompilerError("Unknown tuple field '" + MemberName + "'");
        return nullptr;
      }

      auto accessOpt = computeTupleElementAccess(
          *tupleInfoOpt, ObjectPtr, *indexOpt, "tuple.field");
      if (!accessOpt)
        return nullptr;

      auto &access = *accessOpt;
      setTypeInfo(access.elementTypeInfo);
      return Builder->CreateLoad(access.elementLLVMType, access.elementPtr,
                                 "tuple.elem");
    }
    StructLookupName = ObjectTypeDesc.sanitized;

    info = lookupCompositeInfo(StructLookupName);
    if (info) {
      if (!shouldBypassPropertyAccess(StructLookupName, MemberName)) {
        auto propIt = info->properties.find(MemberName);
        if (propIt != info->properties.end())
          propertyInfo = &propIt->second;
      }
      if (auto modIt = info->fieldModifiers.find(MemberName);
          modIt != info->fieldModifiers.end()) {
        modifiers = &modIt->second;
      } else if (auto staticIt = info->staticFieldModifiers.find(MemberName);
                 staticIt != info->staticFieldModifiers.end()) {
        modifiers = &staticIt->second;
        isStaticField = true;
        if (auto typeIt = info->staticFieldTypes.find(MemberName);
            typeIt != info->staticFieldTypes.end())
          staticFieldType = typeIt->second;
        if (auto globIt = info->staticFieldGlobals.find(MemberName);
            globIt != info->staticFieldGlobals.end())
          staticGlobalName = globIt->second;
      }
    }
  }

  if (StructLookupName.empty())
    StructLookupName = resolveCompositeName(Object.get());

  if (propertyInfo && info &&
      !shouldBypassPropertyAccess(StructLookupName, MemberName)) {
    if (!propertyInfo->isStatic && isTypeReference) {
      reportCompilerError("Instance property '" + MemberName +
                          "' cannot be accessed on type '" + StructLookupName +
                          "'");
      return nullptr;
    }
    return emitPropertyGetter(*info, *propertyInfo, Object.get(), ObjectPtr,
                              StructLookupName);
  }

  if (!isStaticField && !StructLookupName.empty()) {
    staticGlobalName = StructLookupName + "." + MemberName;
    auto globalFallback = CG.globalValues.find(staticGlobalName);
    if (globalFallback != CG.globalValues.end()) {
      isStaticField = true;
      if (!info || !modifiers) {
        defaultStaticModifiers.access = MemberAccess::ReadPublicWritePrivate();
        defaultStaticModifiers.storage |= StorageFlag::Static;
        modifiers = &defaultStaticModifiers;
      }
      if (auto typeIt = CG.globalTypes.find(staticGlobalName);
          typeIt != CG.globalTypes.end())
        staticFieldType = typeIt->second.typeName;
    } else {
      staticGlobalName.clear();
    }
  }

  if (isStaticField) {
    if (!modifiers ||
        !ensureMemberAccessAllowed(*modifiers, AccessIntent::Read,
                                   StructLookupName, MemberName))
      return nullptr;

    if (staticGlobalName.empty())
      staticGlobalName = StructLookupName + "." + MemberName;

    auto globalIt = CG.globalValues.find(staticGlobalName);
    if (globalIt == CG.globalValues.end())
      return LogErrorV(("Static field storage not found for '" + MemberName +
                        "' in type '" + StructLookupName + "'")
                           .c_str());

    llvm::GlobalVariable *GV = globalIt->second;
    if (!staticFieldType.empty())
      setTypeName(staticFieldType);
    return Builder->CreateLoad(GV->getValueType(), GV, MemberName);
  }

  if (modifiers &&
      !ensureMemberAccessAllowed(*modifiers, AccessIntent::Read,
                                 StructLookupName, MemberName))
    return nullptr;

  if (!ObjectPtr) {
    ObjectPtr = Object->codegen();
    if (!ObjectPtr)
      return nullptr;
    ObjectTypeName = Object->getTypeName();
  }

  if (StructLookupName.empty()) {
    ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
    StructLookupName = ObjectTypeDesc.sanitized;
  }

  // If ObjectTypeName is empty but have a valid pointer, it might be a nested struct access
  // In that case, the ObjectPtr is already a pointer to a struct
  if (ObjectTypeName.empty() && ObjectPtr->getType()->isPointerTy()) {
    // In newer LLVM, pointers are opaque, so track the type differently
    // For now, rely on the type name being set correctly by the previous member access
    // This is a limitation that needs a better solution
  }
  
  // Find the struct type
  auto StructIt = StructTypes.find(StructLookupName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(StructLookupName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }
  
  // Find the specific field
  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }
  
  if (!FieldFound) {
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());
  }
  
  // Generate GEP to access the field
  std::vector<llvm::Value*> Indices = {
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))
  };
  
  llvm::Value *FieldPtr = Builder->CreateGEP(
      StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  
  // Load the field value
  // Get the field type from the struct definition
  llvm::Type *FieldType = StructIt->second->getElementType(FieldIndex);
  
  // Set the type name for this expression
  std::string FieldTypeName;
  auto FieldTypesIt = StructFieldTypes.find(StructLookupName);
  if (FieldTypesIt != StructFieldTypes.end()) {
    auto TypeIt = FieldTypesIt->second.find(MemberName);
    if (TypeIt != FieldTypesIt->second.end()) {
      FieldTypeName = TypeIt->second;
      setTypeName(FieldTypeName);
    }
  }
  
  // Load the field value
  return Builder->CreateLoad(FieldType, FieldPtr, MemberName);
}

llvm::Value *MemberAccessExprAST::codegen_ptr() {
  if (isDestructorAccess()) {
    reportCompilerError("Destructor access is only valid as a call expression");
    return nullptr;
  }

  const CompositeTypeInfo *info = nullptr;
  const MemberModifiers *modifiers = nullptr;
  const PropertyInfo *propertyInfo = nullptr;
  bool isStaticField = false;
  std::string staticGlobalName;
  MemberModifiers defaultStaticModifiers;
  std::string StructLookupName;
  std::string ObjectTypeName;

  if (auto *VarObj = dynamic_cast<VariableExprAST *>(Object.get())) {
    StructLookupName = VarObj->getName();
    info = lookupCompositeInfo(StructLookupName);
    if (info) {
      if (!shouldBypassPropertyAccess(StructLookupName, MemberName)) {
        auto propIt = info->properties.find(MemberName);
        if (propIt != info->properties.end()) {
          reportCompilerError("Cannot take address of property '" +
                              MemberName + "'");
          return nullptr;
        }
      }
      if (auto staticIt = info->staticFieldModifiers.find(MemberName);
          staticIt != info->staticFieldModifiers.end()) {
        modifiers = &staticIt->second;
        isStaticField = true;
        if (auto globIt = info->staticFieldGlobals.find(MemberName);
            globIt != info->staticFieldGlobals.end())
          staticGlobalName = globIt->second;
      }
    } else {
      StructLookupName.clear();
    }
  }

  llvm::Value *ObjectPtr = nullptr;
  if (!isStaticField) {
    ObjectPtr = Object->codegen();
    if (!ObjectPtr)
      return nullptr;

    ObjectTypeName = Object->getTypeName();
    ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
    std::string baseLookup = sanitizeBaseTypeName(ObjectTypeName);
    if (ObjectTypeDesc.isNullable) {
      return LogErrorV(
          ("Cannot access nullable type '" + ObjectTypeName +
           "' without null-safe operator")
              .c_str());
    }

    if (isSafeSmartArrow()) {
      TypeInfo arrowInfo = makeTypeInfo(ObjectTypeName);
      finalizeTypeInfoMetadata(arrowInfo);
      if (!arrowInfo.isSmartPointer()) {
        return LogErrorV(
            "'->' requires a smart pointer outside unsafe contexts");
      }
      const CompositeTypeInfo *smartMeta =
          resolveSmartPointerMetadata(arrowInfo);
      if (!smartMeta)
        return LogErrorV("Internal error: missing smart pointer metadata");
      const InstanceFieldInfo *payloadField =
          findInstanceField(*smartMeta, smartMeta->smartPointerKind == SmartPointerKind::Unique
                                           ? "value"
                                           : "payload");
      if (!payloadField)
        return LogErrorV("Internal error: missing smart pointer payload");
      llvm::StructType *smartTy =
          StructTypes[stripNullableAnnotations(typeNameFromInfo(arrowInfo))];
      if (!smartTy)
        return LogErrorV("Internal error: missing smart pointer struct type");
      if (ObjectPtr->getType()->isPointerTy()) {
        llvm::Value *payloadPtr = Builder->CreateStructGEP(
            smartTy, ObjectPtr, payloadField->index,
            "arrow.smart.payload.ptr");
        ObjectPtr = Builder->CreateLoad(
            getTypeFromString(typeNameFromInfo(payloadField->type)),
            payloadPtr, "arrow.smart.payload");
      } else {
        ObjectPtr = Builder->CreateExtractValue(
            ObjectPtr, payloadField->index, "arrow.smart.payload");
      }
      ObjectTypeName = payloadField->type.typeName.empty()
                           ? typeNameFromInfo(payloadField->type)
                           : payloadField->type.typeName;
      ObjectTypeDesc = parseTypeString(ObjectTypeName);
      baseLookup = sanitizeBaseTypeName(ObjectTypeName);
    }

    if (MemberName == "size") {
      if (ObjectTypeDesc.isArray || baseLookup == "string")
        return LogErrorV("Property 'size' is read-only");
    }

    if (auto tupleInfoOpt = resolveTupleTypeInfo(Object.get());
        tupleInfoOpt && tupleInfoOpt->isTupleType()) {
      auto indexOpt = findTupleElementIndex(*tupleInfoOpt, MemberName);
      if (!indexOpt) {
        reportCompilerError("Unknown tuple field '" + MemberName + "'");
        return nullptr;
      }

      auto accessOpt = computeTupleElementAccess(
          *tupleInfoOpt, ObjectPtr, *indexOpt, "tuple.field");
      if (!accessOpt)
        return nullptr;

      auto &access = *accessOpt;
      setTypeInfo(access.elementTypeInfo);
      return access.elementPtr;
    }
    StructLookupName = ObjectTypeDesc.sanitized;

    info = lookupCompositeInfo(StructLookupName);
    if (info) {
      if (!shouldBypassPropertyAccess(StructLookupName, MemberName)) {
        auto propIt = info->properties.find(MemberName);
        if (propIt != info->properties.end()) {
          reportCompilerError("Cannot take address of property '" +
                              MemberName + "'");
          return nullptr;
        }
      }
      if (auto modIt = info->fieldModifiers.find(MemberName);
          modIt != info->fieldModifiers.end()) {
        modifiers = &modIt->second;
      } else if (auto staticIt = info->staticFieldModifiers.find(MemberName);
                 staticIt != info->staticFieldModifiers.end()) {
        modifiers = &staticIt->second;
        isStaticField = true;
        if (auto globIt = info->staticFieldGlobals.find(MemberName);
            globIt != info->staticFieldGlobals.end())
          staticGlobalName = globIt->second;
      }
    }
  }

  if (StructLookupName.empty())
    StructLookupName = resolveCompositeName(Object.get());

  if (!isStaticField && !StructLookupName.empty()) {
    staticGlobalName = StructLookupName + "." + MemberName;
    auto globalFallback = CG.globalValues.find(staticGlobalName);
    if (globalFallback != CG.globalValues.end()) {
      isStaticField = true;
      if (!info || !modifiers) {
        defaultStaticModifiers.access = MemberAccess::ReadPublicWritePrivate();
        defaultStaticModifiers.storage |= StorageFlag::Static;
        modifiers = &defaultStaticModifiers;
      }
    } else {
      staticGlobalName.clear();
    }
  }

  if (isStaticField) {
    if (!modifiers ||
        !ensureMemberAccessAllowed(*modifiers, AccessIntent::Write,
                                   StructLookupName, MemberName))
      return nullptr;

    if (staticGlobalName.empty())
      staticGlobalName = StructLookupName + "." + MemberName;

    auto globalIt = CG.globalValues.find(staticGlobalName);
    if (globalIt == CG.globalValues.end())
      return LogErrorV(("Static field storage not found for '" + MemberName +
                        "' in type '" + StructLookupName + "'")
                           .c_str());

    return globalIt->second;
  }

  if (modifiers &&
      !ensureMemberAccessAllowed(*modifiers, AccessIntent::Write,
                                 StructLookupName, MemberName))
    return nullptr;

  if (!ObjectPtr) {
    ObjectPtr = Object->codegen();
    if (!ObjectPtr)
      return nullptr;
    ObjectTypeName = Object->getTypeName();
  }

  if (StructLookupName.empty()) {
    ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
    StructLookupName = ObjectTypeDesc.sanitized;
  }

  // Find the struct type
  auto StructIt = StructTypes.find(StructLookupName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(StructLookupName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }
  
  // Find the specific field
  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }
  
  if (!FieldFound) {
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());
  }
  
  // Generate GEP to access the field
  std::vector<llvm::Value*> Indices = {
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))
  };
  
  return Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
}

llvm::Value *NullSafeAccessExprAST::codegen() {
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr)
    return nullptr;

  if (!ObjectPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe access requires pointer-compatible object type");

  std::string ObjectTypeName = Object->getTypeName();
  ParsedTypeDescriptor ObjectDesc = parseTypeString(ObjectTypeName);
  std::string StructName = ObjectDesc.sanitized;

  auto StructIt = StructTypes.find(StructName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }

  auto FieldIndicesIt = StructFieldIndices.find(StructName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }

  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }

  if (!FieldFound)
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());

  llvm::Type *FieldType = StructIt->second->getElementType(FieldIndex);
  if (!FieldType->isPointerTy()) {
    return LogErrorV("Null-safe access is only supported for reference-type fields");
  }

  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();
  llvm::Function *Func = CurrentBB->getParent();

  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.notnull", Func);
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.null", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.merge", Func);

  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ObjectPtr->getType()));
  llvm::Value *IsNull = Builder->CreateICmpEQ(ObjectPtr, NullPtr, "nullsafe.check");
  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);
  std::vector<llvm::Value *> Indices = {
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))};
  llvm::Value *FieldPtr = Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  llvm::Value *LoadedField = Builder->CreateLoad(FieldType, FieldPtr, MemberName + ".val");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullValue = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(FieldType));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(FieldType, 2, MemberName + ".maybe");
  Phi->addIncoming(LoadedField, NotNullEnd);
  Phi->addIncoming(NullValue, NullEnd);

  std::string FieldTypeName;
  if (auto FieldTypesIt = StructFieldTypes.find(StructName); FieldTypesIt != StructFieldTypes.end()) {
    if (auto TypeIt = FieldTypesIt->second.find(MemberName); TypeIt != FieldTypesIt->second.end()) {
      FieldTypeName = TypeIt->second;
    }
  }
  if (!FieldTypeName.empty())
    setTypeName(ensureOuterNullable(FieldTypeName));
  else
    setTypeName("unknown?");

  return Phi;
}

llvm::Value *NullSafeAccessExprAST::codegen_ptr() {
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr)
    return nullptr;

  if (!ObjectPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe access requires pointer-compatible object type");

  std::string ObjectTypeName = Object->getTypeName();
  ParsedTypeDescriptor ObjectDesc = parseTypeString(ObjectTypeName);
  std::string StructName = ObjectDesc.sanitized;

  auto StructIt = StructTypes.find(StructName);
  if (StructIt == StructTypes.end())
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());

  auto FieldIndicesIt = StructFieldIndices.find(StructName);
  if (FieldIndicesIt == StructFieldIndices.end())
    return LogErrorV("Internal error: struct field indices not found");

  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }

  if (!FieldFound)
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());

  std::vector<llvm::Value *> Indices = {
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))};

  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();
  llvm::Function *Func = CurrentBB->getParent();

  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.notnull", Func);
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.null", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.merge", Func);

  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ObjectPtr->getType()));
  llvm::Value *IsNull = Builder->CreateICmpEQ(ObjectPtr, NullPtr, "nullsafe.ptr.check");
  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  Builder->SetInsertPoint(NotNullBB);
  llvm::Value *FieldPtr = Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullFieldPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(FieldPtr->getType()));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(FieldPtr->getType(), 2, MemberName + ".ptrmaybe");
  Phi->addIncoming(FieldPtr, NotNullEnd);
  Phi->addIncoming(NullFieldPtr, NullEnd);

  return Phi;
}

// Generate code for 'this' expressions
llvm::Value *ThisExprAST::codegen() {
  // Look up 'this' in the named values
  auto It = NamedValues.find("this");
  if (It == NamedValues.end()) {
    return LogErrorV("'this' can only be used inside struct methods");
  }
  
  // Get type from LocalTypes
  auto TypeIt = LocalTypes.find("this");
  if (TypeIt != LocalTypes.end()) {
    setTypeName(TypeIt->second.typeName);
  }
  
  return It->second;
}

// Generate pointer code for 'this' expressions
llvm::Value *ThisExprAST::codegen_ptr() {
  // 'this' is already a pointer
  return codegen();
}

// Generate code for 'base' expressions (placeholder implementation)
llvm::Value *BaseExprAST::codegen() {
  const ActiveCompositeContext *ctx = currentCompositeContext();
  if (!ctx || ctx->isStatic) {
    if (!ReportedError)
      reportCompilerError("'base' may only be used inside instance methods");
    ReportedError = true;
    return nullptr;
  }

  const CompositeTypeInfo *info = lookupCompositeInfo(ctx->name);
  if (!info || !info->baseClass) {
    if (!ReportedError)
      reportCompilerError("Type '" + ctx->name + "' does not have a base class");
    ReportedError = true;
    return nullptr;
  }

  auto thisIt = NamedValues.find("this");
  if (thisIt == NamedValues.end()) {
    if (!ReportedError)
      reportCompilerError("'base' is only valid within methods of a class");
    ReportedError = true;
    return nullptr;
  }

  auto structIt = StructTypes.find(*info->baseClass);
  if (structIt == StructTypes.end()) {
    if (!ReportedError)
      reportCompilerError("Base class '" + *info->baseClass +
                          "' is not available for 'base' expression");
    ReportedError = true;
    return nullptr;
  }

  llvm::Value *thisValue = thisIt->second;
  llvm::Type *expectedPtr = pointerType();
  llvm::Value *basePtr = thisValue;
  if (thisValue->getType() != expectedPtr)
    basePtr = Builder->CreateBitCast(thisValue, expectedPtr, "base.ptr");

  setTypeName(*info->baseClass);
  return basePtr;
}

llvm::Value *BaseExprAST::codegen_ptr() {
  return codegen();
}

// Generate code for switch statements
llvm::Value *SwitchStmtAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Evaluate the switch condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create basic blocks for each case and the exit block
  llvm::BasicBlock *ExitBB = llvm::BasicBlock::Create(*TheContext, "switch_end");
  
  // Create a switch instruction
  llvm::SwitchInst *Switch = nullptr;
  llvm::BasicBlock *DefaultBB = nullptr;
  
  // First pass: collect all case values and create basic blocks
  std::vector<std::pair<llvm::BasicBlock*, const CaseAST*>> CaseBlocks;
  
  for (const auto &Case : getCases()) {
    llvm::BasicBlock *CaseBB = llvm::BasicBlock::Create(*TheContext, 
        Case->isDefault() ? "default" : "case", TheFunction);
    CaseBlocks.push_back({CaseBB, Case.get()});
    
    if (Case->isDefault()) {
      DefaultBB = CaseBB;
    }
  }
  
  // Create switch instruction with default case or exit block
  llvm::BasicBlock *SwitchDefaultBB = DefaultBB ? DefaultBB : ExitBB;
  Switch = Builder->CreateSwitch(CondV, SwitchDefaultBB, getCases().size());
  
  // Add case values to switch instruction
  for (size_t i = 0; i < getCases().size(); ++i) {
    const auto &Case = getCases()[i];
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    
    if (!Case->isDefault()) {
      // Add each case value
      for (const auto &Value : Case->getValues()) {
        llvm::Value *CaseVal = Value->codegen();
        if (!CaseVal)
          return nullptr;
        
        // Convert to constant integer for switch
        if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(CaseVal)) {
          Switch->addCase(CI, CaseBB);
        } else {
          return LogErrorV("Case values must be compile-time constants");
        }
      }
    }
  }
  
  // Second pass: generate code for each case body
  for (size_t i = 0; i < CaseBlocks.size(); ++i) {
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    const CaseAST *Case = CaseBlocks[i].second;
    
    // Only add the block to function if it's not already added (default case might be)
    if (CaseBB->getParent() == nullptr) {
      CaseBB->insertInto(TheFunction);
    }
    
    Builder->SetInsertPoint(CaseBB);
    
    // Generate case body
    if (Case->getBody()) {
      llvm::Value *BodyV = Case->getBody()->codegen();
      if (!BodyV)
        return nullptr;
    }
    
    // No fall-through - always branch to exit (unless there's a return/break)
    if (!Builder->GetInsertBlock()->getTerminator()) {
      Builder->CreateBr(ExitBB);
    }
  }
  
  // Add exit block and set it as insertion point
  ExitBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ExitBB);
  
  // Switch statements don't return a value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}
