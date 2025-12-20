// This file implements AST node behaviors, including semantic hooks and LLVM IR generation for Hybrid constructs.

#include "ast.h"
#include <iostream>

#include "analysis/semantics.h"
#include "compiler_session.h"
#include "codegen_context.h"
#include "parser.h"

// LLVM includes for code generation
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <cmath>
#include <climits>
#include <map>
#include <sstream>
#include <iomanip>
#include <cstdio>
#include <ranges>
#include <string_view>
#include <optional>
#include <algorithm>
#include <cctype>
#include <set>
#include <functional>
#include <limits>

#include "llvm/Support/ConvertUTF.h"

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

static std::unique_ptr<ExprAST> instantiateDefaultExpr(
    const DefaultArgInfo &info);
static std::string buildArcOpLabel(std::string_view label,
                                   std::string_view suffix);

static bool convertUTF8LiteralToUTF16(const std::string &input,
                                      std::vector<uint16_t> &output,
                                      std::string &errorMessage) {
  output.clear();

  if (input.empty())
    return true;

  const auto *sourceStart =
      reinterpret_cast<const llvm::UTF8 *>(input.data());
  const auto *sourceEnd = sourceStart + input.size();

  // UTF-16 code units will never exceed UTF-8 byte count for valid input,
  // but reserve an extra slot to keep ConvertUTF from exhausting the buffer.
  output.resize(input.size() + 1);

  auto *targetStart =
      reinterpret_cast<llvm::UTF16 *>(output.data());
  auto *target = targetStart;
  auto *targetEnd = targetStart + output.size();

  llvm::ConversionResult result = llvm::ConvertUTF8toUTF16(
      &sourceStart, sourceEnd, &target, targetEnd,
      llvm::strictConversion);

  if (result != llvm::conversionOK) {
    output.clear();
    switch (result) {
      case llvm::sourceExhausted:
        errorMessage =
            "String literal ends with an incomplete UTF-8 sequence";
        break;
      case llvm::sourceIllegal:
        errorMessage =
            "String literal contains an invalid UTF-8 sequence";
        break;
      case llvm::targetExhausted:
        errorMessage =
            "Internal error: insufficient space while converting string literal to UTF-16";
        break;
      case llvm::conversionOK:
        break;
    }
    return false;
  }

  output.resize(static_cast<std::size_t>(target - targetStart));
  return true;
}

#define CG currentCodegen()
#define TheContext (CG.llvmContext)
#define TheModule (CG.module)
#define Builder (CG.builder)
#define NamedValues (CG.namedValues)
#define GlobalValues (CG.globalValues)
#define GlobalTypes (CG.globalTypes)
#define LocalTypes (CG.localTypes)
#define StructTypes (CG.structTypes)
#define ArraySizes (CG.arraySizes)
#define StructFieldIndices (CG.structFieldIndices)
#define StructFieldTypes (CG.structFieldTypes)
#define LoopExitBlocks (CG.loopExitBlocks)
#define LoopContinueBlocks (CG.loopContinueBlocks)
#define NonNullFacts (CG.nonNullFactsStack)

bool isArcLoweringEnabled() {
  if (!hasCompilerSession())
    return true;
  return currentCodegen().arcEnabled;
}

static thread_local std::string ActiveBinaryOp;

class ActiveBinaryOpScope {
public:
  explicit ActiveBinaryOpScope(const std::string &op) : previous(ActiveBinaryOp) {
    ActiveBinaryOp = op;
  }

  ActiveBinaryOpScope(const ActiveBinaryOpScope &) = delete;
  ActiveBinaryOpScope &operator=(const ActiveBinaryOpScope &) = delete;

  ~ActiveBinaryOpScope() { ActiveBinaryOp = previous; }

private:
  std::string previous;
};

static const analysis::VariableLifetimePlan *
lookupLifetimePlanEntry(const std::string &name) {
  const analysis::LifetimePlan *plan = CG.currentLifetimePlan;
  if (!plan)
    return nullptr;
  auto it = plan->variables.find(name);
  if (it == plan->variables.end())
    return nullptr;
  return &it->second;
}

class ActiveLifetimePlanScope {
public:
  explicit ActiveLifetimePlanScope(const analysis::LifetimePlan *plan)
      : previous(CG.currentLifetimePlan) {
    CG.currentLifetimePlan = plan;
  }

  ActiveLifetimePlanScope(const ActiveLifetimePlanScope &) = delete;
  ActiveLifetimePlanScope &
  operator=(const ActiveLifetimePlanScope &) = delete;

  ~ActiveLifetimePlanScope() { CG.currentLifetimePlan = previous; }

private:
  const analysis::LifetimePlan *previous = nullptr;
};

static void noteTypeCacheHit() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.typeCacheHits;
}

static void noteTypeCacheMiss() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.typeCacheMisses;
}

static void noteFunctionCacheHit() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.functionCacheHits;
}

static void noteFunctionCacheMiss() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.functionCacheMisses;
}

static llvm::PointerType *pointerType(unsigned addressSpace = 0) {
  return llvm::PointerType::get(*TheContext, addressSpace);
}

static llvm::PointerType *pointerType(llvm::Type * /*elementType*/,
                                      unsigned addressSpace = 0) {
  return llvm::PointerType::get(*TheContext, addressSpace);
}

static llvm::Type *getSizeType();
static const TypeInfo *lookupLocalTypeInfo(const std::string &name);
static const TypeInfo *lookupGlobalTypeInfo(const std::string &name);
static const TypeInfo *lookupTypeInfo(const std::string &name);

static llvm::PointerType *getDeallocFunctionPointerType() {
  auto *opaquePtrTy = pointerType();
  auto *deallocFnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                              {opaquePtrTy}, false);
  return pointerType(deallocFnTy);
}

static llvm::FunctionCallee getHybridRetainFunction();
static llvm::FunctionCallee getHybridReleaseFunction();
static llvm::FunctionCallee getHybridAutoreleaseFunction();
static llvm::FunctionCallee getHybridDeallocFunction();
static llvm::FunctionCallee getHybridAllocObjectFunction();
static llvm::FunctionCallee getHybridAllocArrayFunction();
static llvm::FunctionCallee getHybridArrayDescriptorFunction();
static llvm::FunctionCallee getHybridArraySetReleaseFunction();
static llvm::FunctionCallee getHybridArrayReleaseRefSlotFunction();
static llvm::FunctionCallee getHybridArrayReleaseArraySlotFunction();
static llvm::FunctionCallee getHybridArcDebugConfigFunction();
static llvm::FunctionCallee getHybridArcTraceLabelFunction();
static llvm::FunctionCallee getHybridArcVerifyRuntimeFunction();
static llvm::Value *emitArcRetain(llvm::Value *value, const TypeInfo &info,
                                  std::string_view label);
static void emitArcRelease(llvm::Value *value, const TypeInfo &info,
                           std::string_view label);
static llvm::Value *emitPackedParamsArray(const std::vector<int> &paramIndices,
                                          const std::vector<ProvidedArgument> &provided,
                                          const TypeInfo &arrayInfo,
                                          std::string_view label);
static bool validateParamsParameterList(const std::vector<Parameter> &params,
                                        const std::string &functionName);
static const ExprAST *unwrapRefExpr(const ExprAST *expr);
static bool typeInfoEquals(const TypeInfo &lhs, const TypeInfo &rhs);
static llvm::Value *emitManagedStore(llvm::Value *storagePtr,
                                     llvm::Value *incoming,
                                     const TypeInfo &info,
                                     std::string_view label,
                                     bool incomingIsTemporary = false);
static bool emitSmartPointerHelpers(const std::string &constructedName,
                                    llvm::StructType *structTy,
                                    CompositeTypeInfo &metadata,
                                    SmartPointerKind kind);
static llvm::FunctionCallee getSharedControlCreateFunction();
static llvm::FunctionCallee getSharedControlRetainStrongFunction();
static llvm::FunctionCallee getSharedControlReleaseStrongFunction();
static llvm::FunctionCallee getSharedControlReleaseWeakFunction();
static llvm::FunctionCallee getSharedControlRetainWeakFunction();
static llvm::FunctionCallee getSharedControlLockFunction();
static llvm::FunctionCallee getSharedControlUseCountFunction();
static llvm::StructType *
getOrCreateSharedControlBlockType(const std::string &constructedName,
                                  llvm::Type *payloadTy);
static TypeInfo applyActiveTypeBindings(const TypeInfo &info);
static TypeInfo makeTypeInfo(std::string typeName,
                             RefStorageClass storage = RefStorageClass::None,
                             bool isMutable = true,
                             bool declaredRef = false);
static std::string sanitizeCompositeLookupName(const std::string &typeName);

static bool ensureNoDuplicateGenericParameters(const std::vector<std::string> &params,
                                               const std::string &contextDescription);

static std::string stripNullableAnnotations(const std::string &typeName);
static std::string typeNameFromInfo(const TypeInfo &info);
static bool typeHasDestructor(const TypeInfo &info);
static bool typeNeedsLifetimeTracking(const TypeInfo &info);

static void dumpGenericBindingStack(const GenericsDiagnostics &diag) {
  if (diag.bindingStack.empty())
    return;
  fprintf(stderr, "[generics-stack] Active bindings:\n");
  for (std::size_t idx = diag.bindingStack.size(); idx-- > 0;) {
    fprintf(stderr, "  #%zu %s\n",
            diag.bindingStack.size() - idx,
            diag.bindingStack[idx].c_str());
  }
}

static bool recordGenericInstantiation(bool isFunction) {
  GenericsDiagnostics &diag = currentCodegen().genericsDiagnostics;
  if (isFunction)
    ++diag.uniqueFunctionInstantiations;
  else
    ++diag.uniqueCompositeInstantiations;
  if (diag.instantiationBudget) {
    const uint64_t total =
        diag.uniqueCompositeInstantiations + diag.uniqueFunctionInstantiations;
    if (total > diag.instantiationBudget) {
      if (!diag.instantiationBudgetExceeded) {
        diag.instantiationBudgetExceeded = true;
        reportCompilerError(
            "Generic instantiation budget exceeded (limit " +
                std::to_string(diag.instantiationBudget) + ")",
            "Increase the limit with --max-generic-instantiations or simplify the generic usage.");
        if (diag.stackDumpEnabled)
          dumpGenericBindingStack(diag);
      }
      return false;
    }
  }
  return true;
}

static std::string buildGenericFrameLabel(const std::string &name,
                                          const std::vector<TypeInfo> &args) {
  if (args.empty())
    return name;
  std::string label = name;
  label += "<";
  for (std::size_t i = 0; i < args.size(); ++i) {
    if (i)
      label += ",";
    label += stripNullableAnnotations(typeNameFromInfo(args[i]));
  }
  label += ">";
  return label;
}

static unsigned computeGenericNestingDepth(const TypeInfo &info) {
  if (info.typeArguments.empty())
    return 0;
  unsigned childDepth = 0;
  for (const auto &arg : info.typeArguments)
    childDepth = std::max(childDepth, computeGenericNestingDepth(arg));
  return 1 + childDepth;
}

static bool maybeReportNestedDepthIssues(const TypeInfo &info,
                                         const std::string &contextDescription) {
  GenericsDiagnostics &diag = currentCodegen().genericsDiagnostics;
  const unsigned depth = computeGenericNestingDepth(info);
  if (diag.nestedDepthBudget && depth > diag.nestedDepthBudget) {
    if (!diag.nestedBudgetExceeded) {
      diag.nestedBudgetExceeded = true;
      reportCompilerError(
          "Nested generic depth (" + std::to_string(depth) +
              ") exceeds configured maximum of " +
              std::to_string(diag.nestedDepthBudget) + " in " +
              contextDescription,
          "Reduce the nesting or raise the limit with --max-nested-generics.");
      if (diag.stackDumpEnabled)
        dumpGenericBindingStack(diag);
    }
    return false;
  }

  if (diag.heuristicsEnabled &&
      depth > diag.nestedDepthWarningThreshold &&
      diag.nestedDepthWarningThreshold > 0) {
    reportCompilerWarning(
        "Nested generic depth of " + std::to_string(depth) + " in " +
            contextDescription + " may impact compile-time performance",
        "Flatten the generic shape or introduce helper typedefs.");
  }
  return true;
}


static const CompositeTypeInfo *lookupCompositeInfo(const std::string &name,
                                                    bool countHit = true);
static const CompositeTypeInfo *
materializeCompositeInstantiation(const TypeInfo &requestedType);

static bool isBuiltinValueTypeName(std::string_view baseName) {
  static constexpr std::string_view primitives[] = {
      "void",  "bool", "byte",  "sbyte", "short", "ushort",
      "int",   "uint", "long",  "ulong", "float", "double",
      "char",  "char16", "char32", "string"};
  for (std::string_view candidate : primitives) {
    if (candidate == baseName)
      return true;
  }
  return false;
}

static SmartPointerKind detectSmartPointerKind(const std::string &baseName) {
  if (baseName == "unique")
    return SmartPointerKind::Unique;
  if (baseName == "shared")
    return SmartPointerKind::Shared;
  if (baseName == "weak")
    return SmartPointerKind::Weak;
  return SmartPointerKind::None;
}

static void applySmartPointerSemantics(TypeInfo &info) {
  const SmartPointerKind kind = detectSmartPointerKind(info.baseTypeName);
  info.smartPointerKind = kind;
  if (kind == SmartPointerKind::Weak) {
    info.ownership = OwnershipQualifier::Weak;
  } else {
    info.ownership = OwnershipQualifier::Strong;
  }
  if (kind != SmartPointerKind::None)
    info.arcManaged = true;
}

static void rebuildGenericBindingKey(TypeInfo &info) {
  info.genericKey.typeName = stripNullableAnnotations(info.baseTypeName);
  info.genericKey.typeArguments.clear();
  info.genericKey.typeArguments.reserve(info.typeArguments.size());
  for (const auto &arg : info.typeArguments) {
    info.genericKey.typeArguments.push_back(
        stripNullableAnnotations(typeNameFromInfo(arg)));
  }
}

static void assignARCEligibility(TypeInfo &info) {
  info.classDescriptor = nullptr;
  if (info.isSmartPointer()) {
    info.arcManaged = true;
    return;
  }

  info.arcManaged = false;
  if (info.isArray) {
    info.arcManaged = true;
    return;
  }

  if (info.baseTypeName == "string") {
    info.arcManaged = true;
    return;
  }

  if (info.pointerDepth > 0)
    return;

  if (info.baseTypeName.empty())
    return;

  if (isBuiltinValueTypeName(info.baseTypeName))
    return;

  if (!hasCompilerSession())
    return;

  auto &metadata = currentCodegen().compositeMetadata;
  const CompositeTypeInfo *meta = nullptr;
  auto it = metadata.find(info.baseTypeName);
  if (it != metadata.end()) {
    meta = &it->second;
  } else {
    std::string sanitized = stripNullableAnnotations(typeNameFromInfo(info));
    auto exactIt = metadata.find(sanitized);
    if (exactIt != metadata.end())
      meta = &exactIt->second;
  }

  if (!meta)
    return;

  // Classes and structs are managed by ARC
  if (meta->kind == AggregateKind::Class || meta->kind == AggregateKind::Struct)
    info.arcManaged = true;
  if (meta->hasClassDescriptor)
    info.classDescriptor = &meta->descriptor;
}

void finalizeTypeInfoMetadata(TypeInfo &info) {
  applySmartPointerSemantics(info);
  rebuildGenericBindingKey(info);
  assignARCEligibility(info);
}

class GenericTypeBindingScope {
public:
  explicit GenericTypeBindingScope(const std::map<std::string, TypeInfo> &bindings,
                                   std::string frameLabel = {})
      : ctx(currentCodegen()), active(false),
        label(std::move(frameLabel)) {
    GenericsDiagnostics &diag = ctx.genericsDiagnostics;
    if (diag.maxBindingDepth > 0 &&
        diag.currentBindingDepth >= diag.maxBindingDepth) {
      diag.depthLimitHit = true;
      reportCompilerError("Maximum generic binding depth (" +
                          std::to_string(diag.maxBindingDepth) + ") exceeded");
      if (diag.stackDumpEnabled)
        dumpGenericBindingStack(diag);
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

static std::map<std::string, std::unique_ptr<StructAST>> &
genericTemplateRegistry() {
  static std::map<std::string, std::unique_ptr<StructAST>> registry;
  return registry;
}

struct GenericFunctionTemplate {
  std::unique_ptr<FunctionAST> function;
};

static std::map<std::string, std::vector<GenericFunctionTemplate>> &
genericFunctionTemplateRegistry() {
  static std::map<std::string, std::vector<GenericFunctionTemplate>> registry;
  return registry;
}

struct GenericInstantiationContext {
  std::string nameOverride;
};

static thread_local std::vector<GenericInstantiationContext>
    ActiveInstantiationContexts;

void RegisterGenericTemplate(std::unique_ptr<StructAST> templ) {
  if (!templ)
    return;
  if (!ensureNoDuplicateGenericParameters(
          templ->getGenericParameters(),
          "type '" + templ->getName() + "'"))
    return;
  std::string key = templ->getName();
  genericTemplateRegistry()[key] = std::move(templ);
}

StructAST *FindGenericTemplate(const std::string &name) {
  auto &registry = genericTemplateRegistry();
  auto it = registry.find(name);
  if (it == registry.end())
    return nullptr;
  return it->second.get();
}

void RegisterGenericFunctionTemplate(std::unique_ptr<FunctionAST> templ) {
  if (!templ)
    return;
  PrototypeAST *proto = templ->getProto();
  if (!proto)
    return;
  if (!validateParamsParameterList(proto->getArgs(), proto->getName()))
    return;
  genericFunctionTemplateRegistry()[proto->getName()].push_back(
      GenericFunctionTemplate{std::move(templ)});
}

FunctionAST *FindGenericFunctionTemplate(const std::string &name,
                                         std::size_t genericArity) {
  auto &registry = genericFunctionTemplateRegistry();
  auto it = registry.find(name);
  if (it == registry.end())
    return nullptr;
  for (auto &entry : it->second) {
    if (!entry.function)
      continue;
    const auto &params = entry.function->getProto()->getGenericParameters();
    if (params.size() == genericArity)
      return entry.function.get();
  }
  return nullptr;
}

static const std::vector<GenericFunctionTemplate> *
lookupGenericFunctionTemplates(const std::string &name) {
  auto &registry = genericFunctionTemplateRegistry();
  auto it = registry.find(name);
  if (it == registry.end())
    return nullptr;
  return &it->second;
}

static std::vector<std::size_t>
collectGenericArities(const std::vector<GenericFunctionTemplate> &templates) {
  std::set<std::size_t> counts;
  for (const auto &entry : templates) {
    if (!entry.function)
      continue;
    counts.insert(entry.function->getProto()->getGenericParameters().size());
  }
  return std::vector<std::size_t>(counts.begin(), counts.end());
}

static std::string formatArityList(const std::vector<std::size_t> &arities) {
  if (arities.empty())
    return "0";
  if (arities.size() == 1)
    return std::to_string(arities.front());

  std::string result;
  for (std::size_t i = 0; i < arities.size(); ++i) {
    if (i == arities.size() - 1) {
      result += std::to_string(arities[i]);
    } else if (i == arities.size() - 2) {
      result += std::to_string(arities[i]) + " or ";
    } else {
      result += std::to_string(arities[i]) + ", ";
    }
  }
  return result;
}

class GenericInstantiationScope {
public:
  explicit GenericInstantiationScope(std::string name) : active(true) {
    ActiveInstantiationContexts.push_back({std::move(name)});
  }

  GenericInstantiationScope(const GenericInstantiationScope &) = delete;
  GenericInstantiationScope &operator=(const GenericInstantiationScope &) = delete;

  ~GenericInstantiationScope() {
    if (active)
      ActiveInstantiationContexts.pop_back();
  }

private:
  bool active = false;
};

static const GenericInstantiationContext *currentInstantiationContext() {
  if (ActiveInstantiationContexts.empty())
    return nullptr;
  return &ActiveInstantiationContexts.back();
}

class StructNameOverrideScope {
public:
  StructNameOverrideScope(std::string &target, const std::string &replacement)
      : ref(target), original(target) {
    ref = replacement;
  }

  StructNameOverrideScope(const StructNameOverrideScope &) = delete;
  StructNameOverrideScope &
  operator=(const StructNameOverrideScope &) = delete;

  ~StructNameOverrideScope() { ref = original; }

private:
  std::string &ref;
  std::string original;
};

static void pushGenericParameterScope(const std::vector<std::string> &params) {
  if (params.empty())
    return;
  CodegenContext &ctx = currentCodegen();
  ctx.genericParameterStack.push_back(params);
  for (const auto &name : params)
    ++ctx.activeGenericParameters[name];
}

static void popGenericParameterScope() {
  CodegenContext &ctx = currentCodegen();
  if (ctx.genericParameterStack.empty())
    return;
  const auto &params = ctx.genericParameterStack.back();
  for (const auto &name : params) {
    auto it = ctx.activeGenericParameters.find(name);
    if (it == ctx.activeGenericParameters.end())
      continue;
    if (it->second <= 1)
      ctx.activeGenericParameters.erase(it);
    else
      --it->second;
  }
  ctx.genericParameterStack.pop_back();
}

static bool isActiveGenericParameter(const std::string &name) {
  if (!hasCompilerSession())
    return false;
  CodegenContext &ctx = currentCodegen();
  return ctx.activeGenericParameters.contains(name);
}

class SemanticGenericParameterScope {
public:
  explicit SemanticGenericParameterScope(const std::vector<std::string> &params)
      : active(!params.empty()) {
    if (active)
      pushGenericParameterScope(params);
  }

  SemanticGenericParameterScope(const SemanticGenericParameterScope &) = delete;
  SemanticGenericParameterScope &operator=(const SemanticGenericParameterScope &) = delete;

  ~SemanticGenericParameterScope() {
    if (active)
      popGenericParameterScope();
  }

private:
  bool active = false;
};

struct GenericDefinitionInfo {
  std::string_view typeName;
  const std::vector<std::string> *parameters = nullptr;
};

static thread_local std::vector<const GenericDefinitionInfo *> ActiveGenericDefinitions;

class GenericDefinitionScope {
public:
  explicit GenericDefinitionScope(const GenericDefinitionInfo *info)
      : active(info != nullptr) {
    if (active)
      ActiveGenericDefinitions.push_back(info);
  }

  GenericDefinitionScope(const GenericDefinitionScope &) = delete;
  GenericDefinitionScope &operator=(const GenericDefinitionScope &) = delete;

  ~GenericDefinitionScope() {
    if (active)
      ActiveGenericDefinitions.pop_back();
  }

private:
  bool active = false;
};

static bool ensureNoDuplicateGenericParameters(const std::vector<std::string> &params,
                                               const std::string &contextDescription) {
  if (params.empty())
    return true;
  std::set<std::string> seen;
  for (const auto &name : params) {
    if (!seen.insert(name).second) {
      reportCompilerError("Duplicate generic parameter '" + name + "' in " + contextDescription);
      return false;
    }
  }
  return true;
}

static std::optional<size_t> findMatchingAngleInTypeName(const std::string &text, size_t openPos) {
  int depth = 0;
  for (size_t i = openPos; i < text.size(); ++i) {
    char ch = text[i];
    if (ch == '<') {
      ++depth;
    } else if (ch == '>') {
      --depth;
      if (depth == 0)
        return i;
      if (depth < 0)
        break;
    }
  }
  return std::nullopt;
}

static bool splitGenericArgumentList(const std::string &segment,
                                     std::vector<std::string> &out) {
  size_t start = 0;
  int angleDepth = 0;
  int bracketDepth = 0;

  for (size_t i = 0; i <= segment.size(); ++i) {
    const bool atEnd = (i == segment.size());
    const char ch = atEnd ? ',' : segment[i];

    if (ch == '<') {
      ++angleDepth;
    } else if (ch == '>') {
      if (angleDepth > 0)
        --angleDepth;
    } else if (ch == '[') {
      ++bracketDepth;
    } else if (ch == ']') {
      if (bracketDepth > 0)
        --bracketDepth;
    }

    if (atEnd || (ch == ',' && angleDepth == 0 && bracketDepth == 0)) {
      if (i < start)
        return false;
      std::string arg = segment.substr(start, i - start);
      if (arg.empty())
        return false;
      out.push_back(std::move(arg));
      start = i + 1;
    }
  }

  return true;
}

static std::vector<TypeInfo> buildGenericArgumentTypeInfos(const std::string &segment);
static void populateTypeInfoGenerics(TypeInfo &info);

static void ensureBaseNonNullScope();
static std::vector<std::string>
buildInheritanceChain(const std::optional<std::string> &baseClassName);
static std::optional<std::string>
buildSmartPointerDescribeSummary(const TypeInfo &requested,
                                 const std::string &sanitized);

static void collectInterfaceAncestors(const std::string &interfaceName,
                                      std::set<std::string> &out);
llvm::Value *castToType(llvm::Value *value, llvm::Type *targetType,
                        const std::string &targetTypeName);
static std::string describeAggregateKind(AggregateKind kind);
static std::string baseCompositeName(const std::string &typeName);

llvm::Value *LogErrorV(const char *Str, std::string_view hint = {});
llvm::Value *LogErrorV(const std::string &Str, std::string_view hint = {});
llvm::Function *LogErrorF(const char *Str, std::string_view hint = {});
llvm::Type *getTypeFromString(const std::string &TypeStr);

static llvm::Function *TopLevelExecFunction = nullptr;
static llvm::Function *ScriptMainFunction = nullptr;
static bool ScriptMainIsSynthetic = false;
static llvm::BasicBlock *TopLevelInsertBlock = nullptr;

static llvm::AllocaInst *createEntryAlloca(llvm::Function *Fn,
                                          llvm::Type *Ty,
                                          const std::string &Name) {
  llvm::IRBuilder<> EntryBuilder(
      &Fn->getEntryBlock(), Fn->getEntryBlock().begin());
  return EntryBuilder.CreateAlloca(Ty, nullptr, Name);
}

static llvm::Function *ensureTopLevelExecFunction() {
  if (!TopLevelExecFunction) {
    auto *FnType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
    TopLevelExecFunction = llvm::Function::Create(
        FnType, llvm::GlobalValue::InternalLinkage,
        "__hybrid_top_level", TheModule.get());
    llvm::BasicBlock *Entry =
        llvm::BasicBlock::Create(*TheContext, "entry", TopLevelExecFunction);
    TopLevelInsertBlock = Entry;
  }
  return TopLevelExecFunction;
}

static void ensureSyntheticMainCalls(llvm::Function *TopFunc) {
  if (!TopFunc)
    return;

  auto *Int32Ty = llvm::Type::getInt32Ty(*TheContext);

  if (!ScriptMainFunction) {
    auto *MainType = llvm::FunctionType::get(Int32Ty, false);
    ScriptMainFunction = llvm::Function::Create(
        MainType, llvm::GlobalValue::ExternalLinkage, "main", TheModule.get());
    ScriptMainIsSynthetic = true;
    llvm::BasicBlock *Entry =
        llvm::BasicBlock::Create(*TheContext, "entry", ScriptMainFunction);
    llvm::IRBuilder<> TmpBuilder(*TheContext);
    TmpBuilder.SetInsertPoint(Entry);
    TmpBuilder.CreateRet(llvm::ConstantInt::get(Int32Ty, 0));
  }

  llvm::BasicBlock &EntryBB = ScriptMainFunction->getEntryBlock();

  if (EntryBB.empty() || !EntryBB.getTerminator()) {
    llvm::IRBuilder<> TmpBuilder(*TheContext);
    TmpBuilder.SetInsertPoint(&EntryBB);
    TmpBuilder.CreateRet(llvm::ConstantInt::get(Int32Ty, 0));
  }

  llvm::Instruction *Term = EntryBB.getTerminator();
  if (!Term) {
    llvm::IRBuilder<> TmpBuilder(*TheContext);
    TmpBuilder.SetInsertPoint(&EntryBB);
    Term = TmpBuilder.CreateRet(llvm::ConstantInt::get(Int32Ty, 0));
  }

  for (auto &Inst : EntryBB) {
    if (auto *Call = llvm::dyn_cast<llvm::CallInst>(&Inst)) {
      if (Call->getCalledFunction() == TopFunc)
        return;
    }
  }

  llvm::IRBuilder<> TmpBuilder(*TheContext);
  TmpBuilder.SetInsertPoint(Term);
  TmpBuilder.CreateCall(TopFunc);
}

static bool builderInTopLevelContext() {
  if (!Builder)
    return true;
  llvm::BasicBlock *insertBlock = Builder->GetInsertBlock();
  if (!insertBlock)
    return true;
  llvm::Function *parent = insertBlock->getParent();
  return !parent || parent->getName() == "__hybrid_top_level";
}

static void prepareTopLevelStatementContext() {
  llvm::Function *TopFunc = ensureTopLevelExecFunction();
  ensureSyntheticMainCalls(TopFunc);
  if (Builder && Builder->GetInsertBlock() &&
      Builder->GetInsertBlock()->getParent() == TopFunc) {
    TopLevelInsertBlock = Builder->GetInsertBlock();
  }
  if (!TopLevelInsertBlock || TopLevelInsertBlock->getParent() != TopFunc) {
    TopLevelInsertBlock = &TopFunc->getEntryBlock();
  }

  if (TopLevelInsertBlock->getTerminator()) {
    TopLevelInsertBlock =
        llvm::BasicBlock::Create(*TheContext, "toplevel.cont", TopFunc);
  }

  Builder->SetInsertPoint(TopLevelInsertBlock,
                          TopLevelInsertBlock->end());

  NamedValues.clear();
  LocalTypes.clear();
  NonNullFacts.clear();
  ensureBaseNonNullScope();
}

void NoteTopLevelStatementEmitted() {
  if (!Builder)
    return;

  llvm::Function *TopFunc = TopLevelExecFunction;
  if (!TopFunc)
    TopFunc = ensureTopLevelExecFunction();
  if (!TopFunc)
    return;

  llvm::BasicBlock *InsertBlock = Builder->GetInsertBlock();
  if (!InsertBlock)
    return;

  if (InsertBlock->getParent() == TopFunc)
    TopLevelInsertBlock = InsertBlock;
}

void FinalizeTopLevelExecution() {
  if (!TopLevelExecFunction || !TheContext)
    return;

  llvm::BasicBlock *InsertBlock = nullptr;
  if (Builder && Builder->GetInsertBlock() &&
      Builder->GetInsertBlock()->getParent() == TopLevelExecFunction) {
    InsertBlock = Builder->GetInsertBlock();
  } else if (TopLevelInsertBlock &&
             TopLevelInsertBlock->getParent() == TopLevelExecFunction) {
    InsertBlock = TopLevelInsertBlock;
  } else if (!TopLevelExecFunction->empty()) {
    InsertBlock = &TopLevelExecFunction->back();
  }

  if (!InsertBlock || InsertBlock->getTerminator())
    return;

  llvm::IRBuilder<> TmpBuilder(*TheContext);
  TmpBuilder.SetInsertPoint(InsertBlock);
  TmpBuilder.CreateRetVoid();
}

static unsigned computePointerDepth(const std::string &typeName) {
  size_t atPos = typeName.find('@');
  if (atPos == std::string::npos)
    return 0;

  size_t endPos = typeName.find_first_of("[]", atPos);
  std::string suffix = typeName.substr(atPos + 1, endPos == std::string::npos ? std::string::npos : endPos - atPos - 1);
  if (suffix.empty())
    return 1;

  unsigned depth = 1;
  try {
    depth = static_cast<unsigned>(std::stoul(suffix));
    if (depth == 0)
      depth = 1;
  } catch (...) {
    depth = 1;
  }
  return depth;
}

static bool isArrayTypeName(const std::string &typeName) {
  auto openPos = typeName.find('[');
  auto closePos = typeName.find(']', openPos == std::string::npos ? 0 : openPos);
  return openPos != std::string::npos && closePos != std::string::npos && closePos > openPos;
}

static std::string removeLastArrayGroup(const std::string &typeName) {
  size_t openPos = typeName.rfind('[');
  if (openPos == std::string::npos)
    return typeName;
  return typeName.substr(0, openPos);
}

static unsigned getLastArrayGroupRank(const std::string &typeName) {
  size_t openPos = typeName.rfind('[');
  if (openPos == std::string::npos)
    return 0;
  size_t closePos = typeName.find(']', openPos);
  if (closePos == std::string::npos)
    return 0;
  unsigned rank = 1;
  for (size_t i = openPos + 1; i < closePos; ++i) {
    if (typeName[i] == ',')
      ++rank;
  }
  return rank;
}

static llvm::StructType *getArrayStructType(llvm::Type *ElementType, unsigned rank);

static std::string stripNullableAnnotations(const std::string &typeName) {
  std::string cleaned;
  cleaned.reserve(typeName.size());
  for (char c : typeName) {
    if (c != '?')
      cleaned.push_back(c);
  }
  return cleaned;
}

static std::string ensureOuterNullable(const std::string &typeName) {
  if (!typeName.empty() && typeName.back() == '?')
    return typeName;
  return typeName + "?";
}

static std::string typeNameFromInfo(const TypeInfo &info) {
  if (info.pointerDepth > 0)
    return info.typeName;

  if (info.isArray) {
    std::string base = info.typeName;
    std::string suffix;
    size_t bracketPos = base.find('[');
    if (bracketPos != std::string::npos) {
      suffix = base.substr(bracketPos);
      base = base.substr(0, bracketPos);
    }

    if (info.elementNullable && (base.empty() || base.back() != '?'))
      base += "?";

    std::string result = base + suffix;
    if (info.isNullable)
      result = ensureOuterNullable(result);
    return result;
  }

  std::string result = info.typeName;
  if (info.isNullable)
    result = ensureOuterNullable(result);
  return result;
}

static std::optional<TypeInfo>
extractElementTypeInfo(const TypeInfo &arrayInfo) {
  if (!arrayInfo.isArray)
    return std::nullopt;

  std::string arrayTypeName =
      stripNullableAnnotations(typeNameFromInfo(arrayInfo));
  if (!isArrayTypeName(arrayTypeName))
    return std::nullopt;

  std::string elementTypeName = removeLastArrayGroup(arrayTypeName);
  if (elementTypeName.empty())
    return std::nullopt;

  TypeInfo elementInfo = makeTypeInfo(elementTypeName);
  if (arrayInfo.elementNullable && !elementInfo.isNullable)
    elementInfo.isNullable = true;
  finalizeTypeInfoMetadata(elementInfo);
  return elementInfo;
}

static std::vector<int64_t>
parseExplicitArrayDimensions(const TypeInfo &info) {
  if (!info.isArray || info.arrayDepth != 1)
    return {};

  std::string typeName = stripNullableAnnotations(typeNameFromInfo(info));
  size_t openPos = typeName.rfind('[');
  if (openPos == std::string::npos)
    return {};
  size_t closePos = typeName.find(']', openPos);
  if (closePos == std::string::npos || closePos <= openPos + 1)
    return {};

  std::string contents =
      typeName.substr(openPos + 1, closePos - openPos - 1);
  std::vector<int64_t> dims;
  size_t pos = 0;
  while (pos < contents.size()) {
    while (pos < contents.size() &&
           std::isspace(static_cast<unsigned char>(contents[pos])))
      ++pos;
    if (pos >= contents.size())
      break;

    size_t start = pos;
    while (pos < contents.size() &&
           std::isdigit(static_cast<unsigned char>(contents[pos])))
      ++pos;
    if (start == pos)
      return {};

    int64_t value = 0;
    try {
      value = std::stoll(contents.substr(start, pos - start));
    } catch (...) {
      return {};
    }
    if (value < 0)
      return {};

    dims.push_back(value);
    while (pos < contents.size() &&
           std::isspace(static_cast<unsigned char>(contents[pos])))
      ++pos;
    if (pos >= contents.size())
      break;
    if (contents[pos] != ',')
      return {};
    ++pos;
  }

  if (dims.empty())
    return {};

  unsigned expectedRank =
      info.arrayRanks.empty() ? 1 : std::max(1u, info.arrayRanks.back());
  if (dims.size() != expectedRank)
    return {};

  return dims;
}

static bool typeHasDestructor(const TypeInfo &info) {
  std::string base = sanitizeCompositeLookupName(typeNameFromInfo(info));
  if (base.empty())
    return false;
  if (const CompositeTypeInfo *meta = lookupCompositeInfo(base))
    return meta->hasDestructor;
  return false;
}

static bool typeNeedsLifetimeTracking(const TypeInfo &info) {
  if (info.isAlias())
    return false;
  if (info.requiresARC()) {
    std::string base = sanitizeCompositeLookupName(typeNameFromInfo(info));
    if (!base.empty()) {
      if (const CompositeTypeInfo *meta = lookupCompositeInfo(base)) {
        if (meta->kind == AggregateKind::Struct && !info.isSmartPointer())
          return meta->hasDestructor;
      }
    }
    return true;
  }
  return typeHasDestructor(info);
}

static std::string joinListOrNone(const std::vector<std::string> &items) {
  if (items.empty())
    return "none";
  std::string result;
  for (size_t i = 0; i < items.size(); ++i) {
    if (i != 0)
      result += ",";
    result += items[i];
  }
  return result;
}

static std::string formatTypeArgumentBindings(
    const std::map<std::string, TypeInfo> &bindings) {
  if (bindings.empty())
    return "none";
  std::vector<std::string> formatted;
  formatted.reserve(bindings.size());
  for (const auto &entry : bindings) {
    formatted.push_back(entry.first + "=" +
                        stripNullableAnnotations(
                            typeNameFromInfo(entry.second)));
  }
  std::sort(formatted.begin(), formatted.end());
  return joinListOrNone(formatted);
}

static std::string formatGenericMethodInstantiations(
    const std::string &typeName,
    const std::map<std::string, std::vector<std::string>> &instantiations) {
  if (instantiations.empty())
    return "none";
  std::string ownerPrefix = baseCompositeName(typeName);
  size_t anglePos = ownerPrefix.find('<');
  if (anglePos != std::string::npos)
    ownerPrefix.erase(anglePos);
  std::vector<std::string> formatted;
  formatted.reserve(instantiations.size());
  for (const auto &entry : instantiations) {
    if (entry.second.empty())
      continue;
    std::string methodName = entry.first;
    if (methodName.find('.') == std::string::npos && !ownerPrefix.empty())
      methodName = ownerPrefix + "." + methodName;
    formatted.push_back(methodName + "=" +
                        std::to_string(entry.second.size()));
  }
  if (formatted.empty())
    return "none";
  std::sort(formatted.begin(), formatted.end());
  std::string result;
  for (size_t i = 0; i < formatted.size(); ++i) {
    if (i != 0)
      result += ",";
    result += formatted[i];
  }
  return result;
}

static std::string formatBaseClass(std::optional<std::string> base) {
  if (!base || base->empty())
    return "none";
  return *base;
}

static std::string formatDescribeSummary(const std::string &typeName,
                                         AggregateKind kind,
                                         const std::vector<std::string> &interfaces,
                                         std::optional<std::string> baseClass,
                                         const std::vector<std::string> &genericParams,
                                         const std::map<std::string, TypeInfo> &bindings,
                                         const std::map<std::string, std::vector<std::string>>
                                             &methodInstantiations) {
  std::string summary;
  summary.reserve(128);
  summary += "type:" + typeName;
  summary += "|kind:" + describeAggregateKind(kind);
  summary += "|baseClass:" + formatBaseClass(baseClass);
  summary += "|interfaces:" + joinListOrNone(interfaces);
  summary += "|genericParameters:" + joinListOrNone(genericParams);
  summary += "|typeArgumentBindings:" + formatTypeArgumentBindings(bindings);
  summary += "|genericMethodInstantiations:" +
             formatGenericMethodInstantiations(typeName, methodInstantiations);
  return summary;
}

static std::string formatTemplateDescribeSummary(const std::string &typeName,
                                                 const StructAST &templ) {
  std::vector<std::string> interfaces = templ.getInterfaces();
  std::optional<std::string> baseClass;
  if (templ.getBaseClass())
    baseClass = *templ.getBaseClass();
  std::map<std::string, TypeInfo> emptyBindings;
  std::map<std::string, std::vector<std::string>> emptyMethods;
  return formatDescribeSummary(typeName, templ.getKind(), interfaces,
                               baseClass, templ.getGenericParameters(),
                               emptyBindings, emptyMethods);
}

static std::optional<std::string>
buildSmartPointerDescribeSummary(const TypeInfo &requested,
                                 const std::string &sanitized) {
  SmartPointerKind kind = detectSmartPointerKind(requested.baseTypeName);
  if (kind == SmartPointerKind::None)
    return std::nullopt;

  if (!requested.typeArguments.empty() && requested.typeArguments.size() != 1) {
    reportCompilerError("Smart pointer '" + requested.baseTypeName +
                        "' expects exactly one type argument");
    return std::nullopt;
  }

  std::vector<std::string> interfaces;
  std::optional<std::string> baseClass;
  std::vector<std::string> genericParams = {"T"};
  std::map<std::string, TypeInfo> bindings;
  if (!requested.typeArguments.empty())
    bindings.emplace("T", requested.typeArguments.front());

  std::map<std::string, std::vector<std::string>> emptyMethods;
  return formatDescribeSummary(sanitized, AggregateKind::Class, interfaces,
                               baseClass, genericParams, bindings,
                               emptyMethods);
}

static std::optional<std::string>
buildDescribeTypeSummary(const std::string &typeSpelling) {
  if (typeSpelling.empty()) {
    reportCompilerError("describeType() requires a type name literal");
    return std::nullopt;
  }

  TypeInfo requested = makeTypeInfo(typeSpelling);
  std::string sanitized =
      stripNullableAnnotations(typeNameFromInfo(requested));
  if (sanitized.empty()) {
    reportCompilerError("describeType() requires a valid type name");
    return std::nullopt;
  }

  if (const CompositeTypeInfo *info = lookupCompositeInfo(sanitized)) {
    return formatDescribeSummary(
        sanitized, info->kind, info->interfaces, info->baseClass,
        info->genericParameters, info->typeArgumentBindings,
        info->genericMethodInstantiations);
  }

  if (auto smartSummary =
          buildSmartPointerDescribeSummary(requested, sanitized)) {
    return smartSummary;
  }

  if (sanitized == "string") {
    return "type:" + sanitized + "|kind:builtin|properties:size";
  }

  if (requested.isArray) {
    return "type:" + sanitized + "|kind:array|properties:size";
  }

  if (StructAST *templ = FindGenericTemplate(sanitized))
    return formatTemplateDescribeSummary(sanitized, *templ);

  reportCompilerError("describeType(): unknown type '" + sanitized + "'");
  return std::nullopt;
}

static std::string sanitizeForMangle(const std::string &input) {
  std::string result;
  result.reserve(input.size() * 3);
  for (unsigned char c : input) {
    if (std::isalnum(c) || c == '_') {
      result.push_back(static_cast<char>(c));
    } else {
      char buffer[4];
      std::snprintf(buffer, sizeof(buffer), "%02X", c);
      result.push_back('_');
      result.append(buffer, 2);
    }
  }
  return result;
}

static std::string makeRuntimeSymbolName(const std::string &prefix,
                                         const std::string &typeName) {
  return prefix + sanitizeForMangle(typeName);
}

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

static llvm::StructType *getTypeDescriptorType() {
  if (auto *existing =
          llvm::StructType::getTypeByName(*TheContext,
                                          "__HybridTypeDescriptor"))
    return existing;
  getInterfaceEntryType();
  return llvm::StructType::getTypeByName(*TheContext,
                                         "__HybridTypeDescriptor");
}

static llvm::StructType *getArcHeaderType() {
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

static llvm::PointerType *getArrayReleaseCallbackPointerType() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnTy = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                                       {voidPtrTy}, false);
  return pointerType(fnTy);
}

static llvm::StructType *getArrayHeaderType() {
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

static llvm::StructType *getStringStorageType() {
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

static std::uint64_t getArrayPayloadOffsetBytes() {
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

static void computeInterfaceMethodLayout(const std::string &typeName,
                                         const std::vector<MethodDefinition> &methods,
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

  for (const auto &MethodDef : methods) {
    if (MethodDef.getKind() != MethodKind::Regular)
      continue;
    if (MethodDef.isStatic())
      continue;
    auto it = metadata.methodInfo.find(MethodDef.getDisplayName());
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

static bool computeVirtualDispatchLayout(const std::string &typeName,
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

static bool emitInterfaceDescriptor(const std::string &typeName,
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

static bool emitClassRuntimeStructures(const std::string &typeName,
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

static llvm::Value *emitDynamicFunctionCall(CallExprAST &callExpr,
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

static llvm::Function *getInterfaceLookupFunction() {
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

static llvm::Type *getSizeType() {
  if (sizeof(void *) == 4)
    return llvm::Type::getInt32Ty(*TheContext);
  return llvm::Type::getInt64Ty(*TheContext);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridRetainFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(voidPtrTy, {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_retain", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridReleaseFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {voidPtrTy},
                              false);
  return TheModule->getOrInsertFunction("hybrid_release", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridAutoreleaseFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(voidPtrTy, {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_autorelease", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridDeallocFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), {voidPtrTy},
                              false);
  return TheModule->getOrInsertFunction("hybrid_dealloc", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridAllocObjectFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType =
      llvm::FunctionType::get(voidPtrTy, {sizeTy, typeDescPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_alloc_object", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridAllocArrayFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *sizeTy = getSizeType();
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType = llvm::FunctionType::get(
      voidPtrTy, {sizeTy, sizeTy, typeDescPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_alloc_array", fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArrayDescriptorFunction() {
  auto *typeDescPtrTy = pointerType(getTypeDescriptorType());
  auto *fnType =
      llvm::FunctionType::get(typeDescPtrTy, {}, false);
  return TheModule->getOrInsertFunction("hybrid_array_type_descriptor",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee getHybridArraySetReleaseFunction() {
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

[[maybe_unused]] static llvm::FunctionCallee
getHybridArrayReleaseRefSlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_release_ref_slot",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArrayReleaseArraySlotFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_array_release_array_slot",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArcDebugConfigFunction() {
  auto *intTy = llvm::Type::getInt32Ty(*TheContext);
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext),
      {intTy, intTy, intTy, intTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_set_debug_flags",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArcTraceLabelFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_trace_set_label",
                                        fnType);
}

[[maybe_unused]] static llvm::FunctionCallee
getHybridArcVerifyRuntimeFunction() {
  auto *voidPtrTy = pointerType(llvm::Type::getInt8Ty(*TheContext));
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*TheContext), {voidPtrTy}, false);
  return TheModule->getOrInsertFunction("hybrid_arc_verify_object",
                                        fnType);
}

static llvm::FunctionCallee getSharedControlCreateFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType =
      llvm::FunctionType::get(opaquePtrTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_create", fnType);
}

static llvm::FunctionCallee getSharedControlRetainStrongFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_retain_strong", fnType);
}

static llvm::FunctionCallee getSharedControlReleaseStrongFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_release_strong", fnType);
}

static llvm::FunctionCallee getSharedControlReleaseWeakFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_release_weak", fnType);
}

static llvm::FunctionCallee getSharedControlRetainWeakFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_retain_weak", fnType);
}

static llvm::FunctionCallee getSharedControlLockFunction() {
  auto *opaquePtrTy = pointerType();
  auto *fnType =
      llvm::FunctionType::get(opaquePtrTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction("__hybrid_shared_control_lock",
                                        fnType);
}

static llvm::FunctionCallee getSharedControlUseCountFunction() {
  auto *opaquePtrTy = pointerType();
  auto *intTy = llvm::Type::getInt32Ty(*TheContext);
  auto *fnType =
      llvm::FunctionType::get(intTy, {opaquePtrTy}, false);
  return TheModule->getOrInsertFunction(
      "__hybrid_shared_control_use_count", fnType);
}

static bool typeInfoIsConcrete(const TypeInfo &info) {
  if (info.isGenericParameter)
    return false;
  for (const auto &arg : info.typeArguments) {
    if (!typeInfoIsConcrete(arg))
      return false;
  }
  return true;
}

static const CompositeTypeInfo *
resolveSmartPointerMetadata(const TypeInfo &info) {
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

static std::string buildArcOpLabel(std::string_view label,
                                   std::string_view op) {
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

static llvm::Value *
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

static bool emitSmartPointerInitFromVariable(const TypeInfo &declaredInfo,
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

static llvm::Value *computeArrayHeaderPointer(llvm::Value *arrayValue,
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

static llvm::Value *emitArrayRetainValue(llvm::Value *arrayValue,
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

static void emitArrayReleaseValue(llvm::Value *arrayValue,
                                  std::string_view label) {
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

static llvm::Value *emitArcRetain(llvm::Value *value, const TypeInfo &info,
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

static void emitArcRelease(llvm::Value *value, const TypeInfo &info,
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

static llvm::Value *emitManagedStore(llvm::Value *storagePtr,
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
        sanitizeCompositeLookupName(typeNameFromInfo(info));
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

static void pushArcScope() { CG.arcScopeStack.emplace_back(); }

static void drainArcSlots(const std::vector<ARCLifetimeSlot> &slots,
                          std::string_view label) {
  for (auto it = slots.rbegin(); it != slots.rend(); ++it) {
    const ARCLifetimeSlot &slot = *it;
    if (slot.isTemporary)
      continue;
    if (!slot.storage)
      continue;
    const bool hasDestructor = typeHasDestructor(slot.type);
    bool useArcRelease = slot.type.requiresARC();
    if (useArcRelease) {
      std::string typeKey =
          sanitizeCompositeLookupName(typeNameFromInfo(slot.type));
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
          sanitizeCompositeLookupName(typeNameFromInfo(slot.type));
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

static void popArcScope(bool emitReleases, std::string_view label) {
  if (CG.arcScopeStack.empty())
    return;
  std::vector<ARCLifetimeSlot> slots = std::move(CG.arcScopeStack.back());
  CG.arcScopeStack.pop_back();
  if (!emitReleases)
    return;

  drainArcSlots(slots, label);
}

static void emitArcScopeDrainAll(std::string_view label) {
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

static void markArcSlotDestroyed(llvm::Value *storage) {
  if (!storage)
    return;
  for (auto &frame : CG.arcScopeStack) {
    for (auto &slot : frame) {
      if (slot.storage == storage)
        slot.isTemporary = true;
    }
  }
}

static void registerArcLocal(const std::string &name, llvm::Value *storage,
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

class ArcScopeGuard {
public:
  explicit ArcScopeGuard(std::string label)
      : label(std::move(label)), originDepth(CG.arcScopeStack.size()),
        active(true) {
    pushArcScope();
  }

  ArcScopeGuard(const ArcScopeGuard &) = delete;
  ArcScopeGuard &operator=(const ArcScopeGuard &) = delete;

  ~ArcScopeGuard() {
    if (!active)
      return;
    if (CG.arcScopeStack.size() > originDepth) {
      llvm::IRBuilder<> *builderPtr = Builder.get();
      if (!builderPtr) {
        CG.arcScopeStack.resize(originDepth);
        return;
      }
      llvm::BasicBlock *block = builderPtr->GetInsertBlock();
      if (!block) {
        CG.arcScopeStack.resize(originDepth);
        return;
      }
      if (block->getTerminator()) {
        CG.arcScopeStack.resize(originDepth);
        return;
      }
      popArcScope(true, label);
      if (CG.arcScopeStack.size() > originDepth)
        CG.arcScopeStack.resize(originDepth);
    }
  }

  void dismissWithoutDrain() {
    if (!active)
      return;
    if (CG.arcScopeStack.size() > originDepth)
      popArcScope(false, label);
    active = false;
  }

private:
  std::string label;
  std::size_t originDepth = 0;
  bool active = false;
};

static llvm::StructType *
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

static bool typeInfoEquals(const TypeInfo &lhs, const TypeInfo &rhs) {
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

static bool validateParamsParameterList(const std::vector<Parameter> &params,
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

static bool resolveParameterDefaults(std::vector<Parameter> &params,
                                     const std::string &functionName) {
  if (!validateParamsParameterList(params, functionName))
    return false;
  for (auto &param : params) {
    if (!resolveDefaultArgument(param, functionName))
      return false;
  }
  return true;
}

static std::unique_ptr<ExprAST> instantiateDefaultExpr(
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

static std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params);
static std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params);
static std::vector<bool> gatherParamParamsFlags(const std::vector<Parameter> &params);
static std::string makeMethodSignatureKey(const std::string &methodName,
                                          const std::vector<TypeInfo> &paramTypes,
                                          const std::vector<bool> &paramIsRef,
                                          bool skipFirstParam = false) {
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

static std::string makeMethodSignatureKey(const std::string &methodName,
                                          const PrototypeAST &proto,
                                          bool skipFirstParam = false) {
  std::vector<TypeInfo> paramTypes = gatherParamTypes(proto.getArgs());
  std::vector<bool> paramIsRef = gatherParamRefFlags(proto.getArgs());
  return makeMethodSignatureKey(methodName, paramTypes, paramIsRef,
                                skipFirstParam);
}

struct MethodRequirement {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};

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

static void collectInterfaceAncestors(const std::string &interfaceName,
                                      std::set<std::string> &out) {
  if (!out.insert(interfaceName).second)
    return;
  if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(interfaceName)) {
    for (const std::string &parent : ifaceInfo->interfaces)
      collectInterfaceAncestors(parent, out);
  }
}

static void gatherInterfaceRequirements(const CompositeTypeInfo &metadata,
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

static void collectAbstractBaseMethods(const std::string &typeName,
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

static void removeInterfaceRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements) {
  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    (void)baseName;
    removeSatisfiedRequirementsFromType(baseInfo, requirements, false);
    return !requirements.empty();
  });
}

static void removeAbstractRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements) {
  visitBaseChain(typeName, [&](const std::string &baseName,
                               const CompositeTypeInfo &baseInfo) {
    (void)baseName;
    removeSatisfiedRequirementsFromType(baseInfo, requirements, true);
    return !requirements.empty();
  });
}

struct BaseMethodMatch {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};

static std::optional<BaseMethodMatch>
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

static std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params) {
  std::vector<TypeInfo> types;
  types.reserve(params.size());
  for (const auto &param : params)
    types.push_back(applyActiveTypeBindings(param.DeclaredType));
  return types;
}

static std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params) {
  std::vector<bool> flags;
  flags.reserve(params.size());
  for (const auto &param : params)
    flags.push_back(param.IsRef);
  return flags;
}

static std::vector<bool> gatherParamParamsFlags(const std::vector<Parameter> &params) {
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

static FunctionOverload *findRegisteredOverload(const std::string &name,
                                                const TypeInfo &returnType,
                                                bool returnsByRef,
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

static FunctionOverload *registerFunctionOverload(const PrototypeAST &proto,
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

static bool verifyOverrideDefaultCompatibility(
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

static FunctionOverload *lookupFunctionOverload(const PrototypeAST &proto) {
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
  auto &registry = genericFunctionTemplateRegistry();
  auto it = registry.find(name);
  if (it == registry.end())
    return nullptr;

  llvm::Function *primaryResult = nullptr;

  for (auto &entry : it->second) {
    FunctionAST *fn = entry.function.get();
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

struct ParsedTypeDescriptor {
  std::string sanitized;
  bool isArray = false;
  bool isNullable = false;
  bool elementNullable = false;
  unsigned pointerDepth = 0;
  unsigned arrayDepth = 0;
  std::vector<unsigned> arrayRanks;
  bool isMultidimensional = false;
};

static ParsedTypeDescriptor parseTypeString(const std::string &typeName) {
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

enum class AccessIntent : uint8_t { Read, Write, Call };

static std::string describeAggregateKind(AggregateKind kind) {
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

static bool validateCompositeHierarchy(const std::string &name,
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

static const ActiveCompositeContext *currentCompositeContext() {
  return currentCompositeContextMutable();
}

static std::string baseCompositeName(const std::string &typeName) {
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

static std::string resolveCompositeName(const ExprAST *expr) {
  if (!expr)
    return {};
  return baseCompositeName(expr->getTypeName());
}

static std::string sanitizeCompositeLookupName(const std::string &typeName) {
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

static llvm::Value *emitResolvedCallInternal(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef,
    const std::vector<std::unique_ptr<ExprAST>> *originalArgs,
    bool preferGeneric, FunctionOverload *forced, ExprAST *typeOwner,
    const std::vector<ProvidedArgument> *providedArgs = nullptr);

static void markStaticFieldInitialized(const std::string &owner, const std::string &member) {
  currentCodegen().initializedStaticFields.insert(makeMemberKey(owner, member));
}

static void noteMemberAssignment(const std::string &ownerName,
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

static bool hasParameterlessConstructor(const std::string &typeName) {
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

static llvm::Value *emitBaseConstructorInitialization(
    const std::string &typeKey, llvm::StructType *structTy,
    llvm::AllocaInst *structPtr, const CompositeTypeInfo &metadata,
    const std::vector<std::unique_ptr<ExprAST>> &args,
    std::optional<std::string> explicitTarget = std::nullopt) {
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

static bool emitConstructorInitializers(const std::string &typeKey,
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

static bool emitCompositeDealloc(const std::string &typeKey,
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

static bool ensureMemberInitializedForMutation(MemberAccessExprAST &member) {
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


static std::optional<std::string>
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

struct MemberFieldAssignmentInfo {
  llvm::Value *fieldPtr = nullptr;
  llvm::Type *fieldType = nullptr;
  std::string structName;
  std::string rawFieldTypeName;
  std::string sanitizedFieldTypeName;
  TypeInfo declaredFieldType;
  bool allowsNull = false;
  bool isStatic = false;
};

static llvm::Constant *constantValueToLLVM(const ConstantValue &value,
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

static bool ensureMemberAccessAllowed(const MemberModifiers &modifiers,
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

class ScopedCompositeContext {
  CodegenContext &Ctx;
  bool Active = false;

public:
  ScopedCompositeContext(const std::string &name, MethodKind kind,
                         bool isStatic)
      : Ctx(currentCodegen()) {
    ActiveCompositeContext context;
    context.name = name;
    context.kind = kind;
    context.isStatic = isStatic;
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

static const ExprAST *unwrapRefExpr(const ExprAST *expr) {
  if (const auto *Ref = dynamic_cast<const RefExprAST*>(expr))
    return Ref->getOperand();
  return expr;
}

static bool typeAllowsNull(const TypeInfo &info) {
  return info.isNullable || (!info.isArray && info.pointerDepth > 0);
}

static bool typeAllowsNull(const ParsedTypeDescriptor &desc) {
  return desc.isNullable || (!desc.isArray && desc.pointerDepth > 0);
}

static bool expressionIsNullable(const ExprAST *expr) {
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
  std::string typeName = expr->getTypeName();
  if (typeName.empty())
    return false;
  ParsedTypeDescriptor desc = parseTypeString(typeName);
  return typeAllowsNull(desc);
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

static llvm::Value *emitPackedParamsArray(
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

static llvm::Value *materializeAliasPointer(llvm::Value *storage,
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

static bool validateInvariantAssignment(const TypeInfo &targetInfo,
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

static void propagateTypeToNewExpr(ExprAST *expr, const TypeInfo &targetInfo) {
  if (!expr)
    return;
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

static std::unique_ptr<ParenExprAST>
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

static bool validateNullableAssignment(const TypeInfo &targetInfo,
                                       const ExprAST *expr,
                                       const std::string &targetDescription) {
  if (!typeAllowsNull(targetInfo) && expressionIsNullable(expr)) {
    LogErrorV(("Cannot assign nullable value to non-nullable " + targetDescription).c_str());
    return false;
  }
  return true;
}

static TypeInfo makeTypeInfo(std::string typeName,
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

static std::vector<TypeInfo> buildGenericArgumentTypeInfos(const std::string &segment) {
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

static std::vector<std::string>
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

static const InstanceFieldInfo *
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

static const CompositeTypeInfo *
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

static const CompositeTypeInfo *
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

  return materializeCompositeInstantiation(requested);
}

static const std::map<std::string, TypeInfo> *currentTypeBindings() {
  auto &stack = currentCodegen().genericTypeBindingsStack;
  if (stack.empty())
    return nullptr;
  return &stack.back();
}

static TypeInfo applyActiveTypeBindings(const TypeInfo &info) {
  if (const auto *bindings = currentTypeBindings())
    return substituteTypeInfo(info, *bindings);
  return info;
}

static std::vector<TypeInfo>
applyActiveTypeBindingsToInfos(const std::vector<TypeInfo> &infos) {
  std::vector<TypeInfo> result;
  result.reserve(infos.size());
  for (const auto &info : infos)
    result.push_back(applyActiveTypeBindings(info));
  return result;
}

static std::optional<TypeInfo>
applyActiveTypeBindingsToOptionalInfo(const std::optional<TypeInfo> &info) {
  if (!info)
    return std::nullopt;
  return applyActiveTypeBindings(*info);
}

static const TypeInfo *lookupLocalTypeInfo(const std::string &name) {
  auto It = LocalTypes.find(name);
  if (It != LocalTypes.end())
    return &It->second;
  return nullptr;
}

static const TypeInfo *lookupGlobalTypeInfo(const std::string &name) {
  auto It = GlobalTypes.find(name);
  if (It != GlobalTypes.end())
    return &It->second;
  return nullptr;
}

static const TypeInfo *lookupTypeInfo(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info;
  return lookupGlobalTypeInfo(name);
}

static void rememberGlobalType(const std::string &name, TypeInfo info) {
  GlobalTypes[name] = std::move(info);
}

static void rememberLocalType(const std::string &name, TypeInfo info) {
  LocalTypes[name] = std::move(info);
}

static TypeInfo runtimeTypeFrom(const TypeInfo &declared, RefStorageClass storage, bool declaredRefOverride) {
  TypeInfo info = declared;
  info.refStorage = storage;
  info.declaredRef = declaredRefOverride;
  return info;
}

static bool validateTypeForGenerics(const TypeInfo &info,
                                    const std::string &contextDescription,
                                    const GenericDefinitionInfo *currentDefinition = nullptr) {
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

static bool isDeclaredRefGlobal(const std::string &name) {
  if (const auto *info = lookupGlobalTypeInfo(name))
    return info->declaredRef;
  return false;
}

static bool isDeclaredRefLocal(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info->declaredRef;
  return false;
}

static void ensureBaseNonNullScope() {
  if (NonNullFacts.empty())
    NonNullFacts.emplace_back();
}

static std::set<std::string> currentNonNullFactsCopy() {
  if (NonNullFacts.empty())
    return {};
  return NonNullFacts.back();
}

static void replaceCurrentNonNullFacts(const std::set<std::string> &facts) {
  ensureBaseNonNullScope();
  NonNullFacts.back() = facts;
}

static void pushNonNullFactsFrom(const std::set<std::string> &seed) {
  NonNullFacts.push_back(seed);
}

static void popNonNullFactsScope() {
  if (!NonNullFacts.empty())
    NonNullFacts.pop_back();
}

static bool isKnownNonNull(const std::string &name) {
  for (auto It = NonNullFacts.rbegin(); It != NonNullFacts.rend(); ++It) {
    if (It->find(name) != It->end())
      return true;
  }
  return false;
}

static void markKnownNonNull(const std::string &name) {
  ensureBaseNonNullScope();
  NonNullFacts.back().insert(name);
}

static void markKnownNullable(const std::string &name) {
  if (!NonNullFacts.empty())
    NonNullFacts.back().erase(name);
}

enum class NullComparisonRelation {
  EqualsNull,
  NotEqualsNull
};

struct NullComparison {
  std::string variableName;
  NullComparisonRelation relation = NullComparisonRelation::EqualsNull;
};

static NullComparisonRelation invertRelation(NullComparisonRelation relation) {
  return relation == NullComparisonRelation::EqualsNull
             ? NullComparisonRelation::NotEqualsNull
             : NullComparisonRelation::EqualsNull;
}

static std::optional<NullComparison>
extractNullComparison(const ExprAST *expr, bool inverted = false) {
  if (!expr)
    return std::nullopt;

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

enum class BranchKind {
  Then,
  Else
};

static void applyNullComparisonToCurrentScope(const NullComparison &comparison,
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

static std::set<std::string>
intersectNonNullFacts(const std::set<std::string> &a,
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

static void updateKnownNonNullOnAssignment(const std::string &name,
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
constexpr bool isSignedType(std::string_view TypeStr) {
  // Signed types: int, sbyte, short, long, char (in C, char is signed by default)
  // Note: schar is "short char" (8-bit), not signed char
  return TypeStr == "int" || TypeStr == "sbyte" || TypeStr == "short" ||
         TypeStr == "long" || TypeStr == "char";
}

// Helper to check if a type name represents an unsigned type
constexpr bool isUnsignedType(std::string_view TypeStr) {
  // Unsigned types: byte, ushort, uint, ulong
  // Note: schar and lchar are character types, treating as unsigned
  return TypeStr == "byte" || TypeStr == "ushort" || TypeStr == "uint" ||
         TypeStr == "ulong" || TypeStr == "schar" || TypeStr == "lchar";
}

static std::string sanitizeBaseTypeName(std::string_view typeName) {
  if (typeName.empty())
    return {};

  ParsedTypeDescriptor desc = parseTypeString(std::string(typeName));
  return desc.sanitized;
}

static std::optional<bool> unsignedHintFromTypeName(const std::string &typeName) {
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

static std::optional<MemberFieldAssignmentInfo>
collectMemberFieldAssignmentInfo(MemberAccessExprAST &member) {
  MemberFieldAssignmentInfo info;

  info.fieldPtr = member.codegen_ptr();
  if (!info.fieldPtr)
    return std::nullopt;

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

static bool isPointerTypeDescriptor(const ParsedTypeDescriptor &desc) {
  return desc.pointerDepth > 0 && !desc.isArray;
}

static std::optional<std::string> getPointerElementTypeName(const std::string &pointerTypeName) {
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

static llvm::IntegerType *getPointerIndexType() {
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

static uint64_t getTypeSizeInBytes(llvm::Type *type) {
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

  return 0;
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

static llvm::Value *emitPointerOffset(llvm::Value *ptrValue,
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

static std::string describeTypeForDiagnostic(llvm::Type *type) {
  if (!type)
    return "value";

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
  if (type->isPointerTy())
    return "pointer";
  return "value";
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

static bool diagnoseDisallowedImplicitIntegerConversion(const ExprAST *sourceExpr,
                                                        llvm::Value *sourceValue,
                                                        llvm::Type *targetType,
                                                        std::string_view targetTypeName,
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

//===----------------------------------------------------------------------===//
// Expression Code Generation
//===----------------------------------------------------------------------===//

// Generate code for number expressions
llvm::Value *NumberExprAST::codegen() {
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

llvm::Value *ParenExprAST::codegen() {
  if (IsTupleExpr) {
    reportCompilerError(
        "Tuple expressions are not supported in this context",
        "Use constructor call syntax like Type(arg1, arg2) instead of relying on bare tuples.");
    return nullptr;
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

static llvm::Value *emitStringLiteral(const std::string &value) {
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

static llvm::FunctionCallee getConcatStringsFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *stringPtrPtrTy = llvm::PointerType::get(*TheContext, 0);
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {stringPtrPtrTy, int32Ty},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_concat_strings", fnType);
}

static llvm::FunctionCallee getStringEqualsFunction() {
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

static llvm::FunctionCallee getCharToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy, {int32Ty}, false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_char32", fnType);
}

static llvm::Value *emitTargetTypedConstruction(const TypeInfo &targetInfo,
                                                ParenExprAST &paren) {
  TypeInfo boundTarget = applyActiveTypeBindings(targetInfo);
  finalizeTypeInfoMetadata(boundTarget);

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

static llvm::Value *emitThisOverrideString(const CompositeTypeInfo &info,
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

static llvm::Value *ensureStringPointer(llvm::Value *value) {
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

  if (formatSpec.has_value() && !isFloatType) {
    return LogErrorV("Format specifiers are only supported for floating point interpolation");
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

// Helper to check if types are compatible for implicit conversion
// No implicit conversion between different sized integers
bool areTypesCompatible(llvm::Type* type1, llvm::Type* type2) {
  if (type1 == type2)
    return true;
  
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

static llvm::Value *emitArrayFillValue(const TypeInfo &arrayInfo,
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

  struct ArrayElementAccessInfo {
    llvm::Value *elementPtr;
    llvm::Type *elementLLVMType;
    std::string elementTypeName;
    bool elementNullable;
    TypeInfo elementTypeInfo;
  };

static std::optional<ArrayElementAccessInfo>
computeArrayElementAccess(ArrayIndexExprAST *node) {
  ArrayElementAccessInfo access;

  llvm::Value *ArrayVal = node->getArray()->codegen();
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

// Generate code for array indexing
llvm::Value *ArrayIndexExprAST::codegen() {
  auto accessOpt = computeArrayElementAccess(this);
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
      setTypeName(typeNameFromInfo(*info));
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
      setTypeName(typeNameFromInfo(*info));
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
  auto accessOpt = computeArrayElementAccess(this);
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
      setTypeName(typeNameFromInfo(effective));

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
      setTypeName(typeNameFromInfo(effective));

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
        LogErrorV(("Integer literal " + std::to_string(Val.getSExtValue()) + 
                   " is out of range for target type").c_str());
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
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
        LogErrorV(("Integer literal " + std::to_string(v) + 
                   " is out of range for target type " + targetTypeName).c_str());
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
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
    return {L, R}; // Return original values, error already logged
  }

  std::string lhsClean = sanitizeBaseTypeName(lhsTypeName);
  std::string rhsClean = sanitizeBaseTypeName(rhsTypeName);

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
  // Generate left operand first
  llvm::Value *L = getLHS()->codegen();
  if (!L)
    return nullptr;
  
  if (Op == "\?\?")
    return codegenNullCoalescing(L);
  if (Op == "\?\?=")
    return codegenNullCoalescingAssign(L);

  // For comparisons, arithmetic, and equality operators, try to regenerate literals so they
  // match the other operand's type (numbers and chars).
  llvm::Value *R = nullptr;
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
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
    } else if (MemberAccessExprAST *LHSMA = dynamic_cast<MemberAccessExprAST*>(getLHS())) {
      if (!ensureMemberInitializedForMutation(*LHSMA))
        return nullptr;

      auto fieldInfoOpt = collectMemberFieldAssignmentInfo(*LHSMA);
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
                                                : LHSMA->getTypeName();
        llvm::Value *NewPtr =
            emitPointerOffset(CurrentVal, R, pointerTypeName, getRHS()->getTypeName(),
                              Op == "-=", "ptrarith");
        if (!NewPtr)
          return nullptr;

        Builder->CreateStore(NewPtr, fieldInfo.fieldPtr);
        noteMemberAssignment(fieldInfo.structName, LHSMA->getMemberName(), fieldInfo.isStatic);
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      }

      std::string lhsPromoteType = fieldInfo.sanitizedFieldTypeName.empty()
                                       ? sanitizeBaseTypeName(LHSMA->getTypeName())
                                       : fieldInfo.sanitizedFieldTypeName;
      std::string rhsPromoteType = getRHS()->getTypeName();

      auto [PromotedCurrent, PromotedR] =
          promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      auto lhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(lhsPromoteType));
      auto rhsUnsigned = unsignedHintFromTypeName(sanitizeBaseTypeName(rhsPromoteType));
      bool compoundUnsigned = lhsUnsigned.value_or(false) || rhsUnsigned.value_or(false);
      if (!compoundUnsigned) {
        compoundUnsigned =
            unsignedHintFromTypeName(sanitizeBaseTypeName(LHSMA->getTypeName())).value_or(false);
      }

      llvm::Value *Result;
      char baseOp = Op[0];

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

      llvm::Value *ResultToStore = Result;
      if (ResultToStore->getType() != fieldInfo.fieldType) {
        std::string castTargetName =
            !lhsPromoteType.empty() ? lhsPromoteType : LHSMA->getTypeName();
        ResultToStore = castToType(ResultToStore, fieldInfo.fieldType, castTargetName);
        if (!ResultToStore)
          return nullptr;
      }

      Builder->CreateStore(ResultToStore, fieldInfo.fieldPtr);
      noteMemberAssignment(fieldInfo.structName, LHSMA->getMemberName(), fieldInfo.isStatic);
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
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

  if (Op != "=") {
    // Promote types to compatible types
    auto promoted = promoteTypes(L, R, leftTypeName, rightTypeName);
    L = promoted.first;
    R = promoted.second;

    // Check if working with floating point or integer types
    resultType = L->getType();
    isFloat = resultType->isFloatingPointTy();

    // Set result type name based on the promoted type
    if (isFloat) {
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
      if (isFloat)
        return Builder->CreateFAdd(L, R, "addtmp");
      else
        return Builder->CreateAdd(L, R, "addtmp");
    case '-':
      if (isFloat)
        return Builder->CreateFSub(L, R, "subtmp");
      else
        return Builder->CreateSub(L, R, "subtmp");
    case '*':
      if (isFloat)
        return Builder->CreateFMul(L, R, "multmp");
      else
        return Builder->CreateMul(L, R, "multmp");
    case '/':
      if (isFloat)
        return Builder->CreateFDiv(L, R, "divtmp");
      else if (preferUnsigned)
        return Builder->CreateUDiv(L, R, "divtmp");
      else
        return Builder->CreateSDiv(L, R, "divtmp");
    case '%':
      if (isFloat)
        return Builder->CreateFRem(L, R, "modtmp");
      else if (preferUnsigned)
        return Builder->CreateURem(L, R, "modtmp");
      else
        return Builder->CreateSRem(L, R, "modtmp");
    case '<':
      setTypeName("bool");
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

          if (!rhsVal)
            rhsVal = getRHS()->codegen();

          R = rhsVal;
          return rhsVal;
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
            llvm::Value *rhsValue = emitAssignmentRHS(info);
            if (!rhsValue)
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
          }

          llvm::Value *rhsValue =
              emitAssignmentRHS(lookupLocalTypeInfo(LHSE->getName()));
          if (!rhsValue)
            return nullptr;

          const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName());
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
            llvm::Value *rhsValue = emitAssignmentRHS(info);
            if (!rhsValue)
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
          }

          llvm::Value *rhsValue =
              emitAssignmentRHS(lookupGlobalTypeInfo(LHSE->getName()));
          if (!rhsValue)
            return nullptr;

          const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName());
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

        return LogErrorV(("Unknown variable name: " + LHSE->getName()).c_str());
      } else if (ArrayIndexExprAST *LHSAI = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
        // Array element assignment
        auto accessOpt = computeArrayElementAccess(LHSAI);
        if (!accessOpt)
          return nullptr;
        auto &access = *accessOpt;

        llvm::Value *ElemPtr = access.elementPtr;
        llvm::Type *ElemType = access.elementLLVMType;

        if (!ElemPtr || !ElemType || !ElemPtr->getType()->isPointerTy())
          return LogErrorV("Invalid array element pointer");

        auto *arrayElementRHSVar =
            dynamic_cast<VariableExprAST *>(getRHS());

        bool rhsIsTemporary = getRHS() && getRHS()->isTemporary();

        if (rhsIsNullable && !access.elementNullable) {
          return LogErrorV("Cannot assign nullable value to non-nullable array element");
        }

        llvm::Value *rhsValue = emitAssignmentRHS(&access.elementTypeInfo);
        if (!rhsValue)
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
        // Member access assignment (e.g. this.x = value)
        auto fieldInfoOpt = collectMemberFieldAssignmentInfo(*LHSMA);
        if (!fieldInfoOpt)
          return nullptr;
        auto &fieldInfo = *fieldInfoOpt;

        if (!validateInvariantAssignment(fieldInfo.declaredFieldType, getRHS(),
                                         "assignment to field '" + LHSMA->getMemberName() + "'"))
          return nullptr;

        if (rhsIsNullable && !fieldInfo.allowsNull) {
          return LogErrorV(("Cannot assign nullable value to non-nullable field '" + LHSMA->getMemberName() + "'").c_str());
        }

        std::string diagFieldTypeName = fieldInfo.sanitizedFieldTypeName;

        llvm::Value *rhsValue =
            emitAssignmentRHS(&fieldInfo.declaredFieldType);
        if (!rhsValue)
          return nullptr;

        std::string contextDescription = "assignment to field '" + LHSMA->getMemberName() + "'";
        if (diagnoseDisallowedImplicitIntegerConversion(getRHS(), rhsValue, fieldInfo.fieldType, diagFieldTypeName, contextDescription))
          return nullptr;

        if (!diagFieldTypeName.empty())
          rhsValue = castToType(rhsValue, fieldInfo.fieldType, diagFieldTypeName);
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
                "Initializer for '" + LHSMA->getMemberName() +
                "' has incompatible smart pointer representation");
            return nullptr;
          }

          if (rhsVarExpr) {
            if (emitSmartPointerInitFromVariable(
                    fieldInfo.declaredFieldType, fieldInfo.fieldPtr, *rhsVarExpr,
                    LHSMA->getMemberName())) {
              noteMemberAssignment(fieldInfo.structName, LHSMA->getMemberName(),
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
                  buildArcOpLabel(LHSMA->getMemberName(),
                                  "smart.destroy.cast"));
            }
            Builder->CreateCall(destroyFn, {destroyArg});
          }

          llvm::Value *stored = rhsValue;
          if (stored->getType()->isPointerTy()) {
            stored = Builder->CreateLoad(
                structTy, stored,
                buildArcOpLabel(LHSMA->getMemberName(), "smart.assign.load"));
          }
          if (stored->getType() != structTy) {
            reportCompilerError(
                "Initializer for '" + LHSMA->getMemberName() +
                "' has incompatible smart pointer representation");
            return nullptr;
          }

          Builder->CreateStore(stored, fieldInfo.fieldPtr);
        } else if (fieldInfo.declaredFieldType.requiresARC()) {
          emitManagedStore(fieldInfo.fieldPtr, rhsValue, fieldInfo.declaredFieldType,
                           LHSMA->getMemberName(), rhsIsTemporary);
        } else {
          Builder->CreateStore(rhsValue, fieldInfo.fieldPtr);
        }
        noteMemberAssignment(fieldInfo.structName, LHSMA->getMemberName(),
                             fieldInfo.isStatic);
        // Return a void value to indicate this is a statement, not an expression
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
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
    if (isFloat)
      Cmp = Builder->CreateFCmpOEQ(L, R, "eqtmp");
    else
      Cmp = Builder->CreateICmpEQ(L, R, "eqtmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "!=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpONE(L, R, "netmp");
    else
      Cmp = Builder->CreateICmpNE(L, R, "netmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "<=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpOLE(L, R, "letmp");
    else if (preferUnsigned)
      Cmp = Builder->CreateICmpULE(L, R, "letmp");
    else
      Cmp = Builder->CreateICmpSLE(L, R, "letmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == ">=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpOGE(L, R, "getmp");
    else if (preferUnsigned)
      Cmp = Builder->CreateICmpUGE(L, R, "getmp");
    else
      Cmp = Builder->CreateICmpSGE(L, R, "getmp");
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
    if (isFloat)
      return LogErrorV("Bitwise AND requires integer operands");
    return Builder->CreateAnd(L, R, "andtmp");
  } else if (Op == "|") {
    // Bitwise OR - only works on integers
    if (isFloat)
      return LogErrorV("Bitwise OR requires integer operands");
    return Builder->CreateOr(L, R, "ortmp");
  } else if (Op == "^") {
    // Bitwise XOR - only works on integers
    if (isFloat)
      return LogErrorV("Bitwise XOR requires integer operands");
    return Builder->CreateXor(L, R, "xortmp");
  } else if (Op == "<<") {
    // Left shift - only works on integers
    if (isFloat)
      return LogErrorV("Left shift requires integer operands");
    return Builder->CreateShl(L, R, "shltmp");
  } else if (Op == ">>") {
    // Right shift (arithmetic) - only works on integers
    if (isFloat)
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
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() || R->getType()->isFloatingPointTy())
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      std::string lhsPromoteType;
      if (info)
        lhsPromoteType = typeNameFromInfo(*info);
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
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
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() || R->getType()->isFloatingPointTy())
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
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
    } else {
        return LogErrorV("++/-- requires integer or floating-point type");
    }

    llvm::Value *NextVal;

    if (Op == "++") {
        if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFAdd(CurVal, One, "addtmp");
        else
            NextVal = Builder->CreateAdd(CurVal, One, "addtmp");
    } else { // --
        if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFSub(CurVal, One, "subtmp");
        else
            NextVal = Builder->CreateSub(CurVal, One, "subtmp");
    }

    Builder->CreateStore(NextVal, Ptr);

    // For prefix operators, return the new value; for postfix, return the old value
    return isPrefix ? NextVal : CurVal;
  }

  // Handle other unary operators
  llvm::Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  if (Op == "-") {
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

  // Get the target LLVM type
  llvm::Type *TargetLLVMType = getTypeFromString(getTargetType());
  if (!TargetLLVMType)
    return LogErrorV("Invalid target type for cast");

  // Set our type name to the target type
  setTypeName(getTargetType());

  llvm::Type *SourceType = OperandV->getType();
  
  // If same type, just return the value
  if (SourceType == TargetLLVMType)
    return OperandV;

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
  
  return LogErrorV("Invalid cast between incompatible types");
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
    std::string arrayTypeName = targetType;
    if (arrayTypeName.find('[') == std::string::npos)
      arrayTypeName += "[]";

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

    llvm::Value *lengthVal = nullptr;
    if (ArraySizeExpr) {
      lengthVal = ArraySizeExpr->codegen();
      if (!lengthVal)
        return nullptr;
    }
    if (!lengthVal) {
      reportCompilerError("Array size expression could not be evaluated");
      return nullptr;
    }

    if (!lengthVal->getType()->isIntegerTy()) {
      lengthVal = castToType(lengthVal, llvm::Type::getInt32Ty(*TheContext),
                             "int");
      if (!lengthVal)
        return nullptr;
    }

    llvm::Value *length32 = lengthVal;
    if (!length32->getType()->isIntegerTy(32))
      length32 = Builder->CreateTruncOrBitCast(
          length32, llvm::Type::getInt32Ty(*TheContext), "new.array.len32");

    if (auto *constLen = llvm::dyn_cast<llvm::ConstantInt>(length32)) {
      if (constLen->isNegative()) {
        reportCompilerError("Array size cannot be negative");
        return nullptr;
      }
    }

    const llvm::DataLayout &DL = TheModule->getDataLayout();
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
    llvm::Value *lengthSize =
        Builder->CreateZExt(length32, getSizeType(), "new.array.len");
    llvm::Value *rawPtr = Builder->CreateCall(
        getHybridAllocArrayFunction(),
        {elemSizeVal, lengthSize, descriptorPtr}, "new.array.raw");

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

    unsigned rank =
        desc.arrayRanks.empty() ? 1 : std::max(1u, desc.arrayRanks.back());
    llvm::StructType *arrayStructTy = getArrayStructType(elemType, rank);
    llvm::Value *arrayValue = llvm::UndefValue::get(arrayStructTy);
    llvm::Value *opaqueDataPtr =
        Builder->CreateBitCast(dataPtr, pointerType(), "new.array.ptr");
    arrayValue = Builder->CreateInsertValue(arrayValue, opaqueDataPtr, {0});
    arrayValue = Builder->CreateInsertValue(arrayValue, length32, {1});

    for (unsigned i = 0; i < rank; ++i) {
      llvm::Value *dimVal = (i == 0)
                                ? static_cast<llvm::Value *>(length32)
                                : llvm::ConstantInt::get(
                                      llvm::Type::getInt32Ty(*TheContext), 0);
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

static bool parseExplicitTypeArgumentSuffix(const std::string &text,
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

// Generate code for function calls, including struct constructors
llvm::Value *CallExprAST::codegen() {
  std::string decoratedCallee = getCallee();
  std::string baseCallee = decoratedCallee;
  std::vector<TypeInfo> explicitTypeArgs;
  if (!parseExplicitTypeArgumentSuffix(decoratedCallee, baseCallee,
                                       explicitTypeArgs))
    return nullptr;
  bool calleeIsCompositeType = false;
  if (decoratedCallee.find('<') != std::string::npos) {
    TypeInfo boundCallee = applyActiveTypeBindings(makeTypeInfo(decoratedCallee));
    decoratedCallee = typeNameFromInfo(boundCallee);
    if (lookupCompositeInfo(decoratedCallee, /*countHit=*/false))
      calleeIsCompositeType = true;
  }
  const auto *functionTemplates =
      lookupGenericFunctionTemplates(baseCallee);
  if (baseCallee == "describeType" && !hasCalleeExpr()) {
    if (getArgs().size() != 1) {
      reportCompilerError("describeType() expects exactly one argument");
      return nullptr;
    }
    auto *literalArg =
        dynamic_cast<StringExprAST *>(getArgs().front().get());
    if (!literalArg) {
      reportCompilerError(
          "describeType() requires a string literal argument");
      return nullptr;
    }
    auto summary = buildDescribeTypeSummary(literalArg->getValue());
    if (!summary)
      return nullptr;
    setTypeName("string");
    markTemporary();
    return emitStringLiteral(*summary);
  }

  auto trySmartPointerBuilder =
      [&](std::string_view helperName,
          std::string_view targetBase) -> llvm::Value * {
        if (hasCalleeExpr() || baseCallee != helperName)
          return nullptr;
        if (explicitTypeArgs.size() != 1) {
          reportCompilerError(std::string(helperName) +
                              " requires exactly one explicit type argument");
          return nullptr;
        }
        TypeInfo payloadInfo =
            applyActiveTypeBindings(explicitTypeArgs.front());
        finalizeTypeInfoMetadata(payloadInfo);
        std::string payloadName = typeNameFromInfo(payloadInfo);
        std::string targetName =
            std::string(targetBase) + "<" + payloadName + ">";
        TypeInfo targetInfo = makeTypeInfo(targetName);
        finalizeTypeInfoMetadata(targetInfo);
        bool treatAsTuple = Args.size() > 1;
        ParenExprAST paren(std::move(Args), treatAsTuple);
        paren.setTypeName(targetName);
        llvm::Value *constructed =
            emitTargetTypedConstruction(targetInfo, paren);
        resetArgs(paren.takeElements());
        if (!constructed)
          return nullptr;
        setTypeName(typeNameFromInfo(targetInfo));
        markTemporary();
        return constructed;
      };

  if (auto *built = trySmartPointerBuilder("make_unique", "unique"))
    return built;
  if (auto *built = trySmartPointerBuilder("make_shared", "shared"))
    return built;

  if (!hasCalleeExpr() && baseCallee == "weak_from_shared") {
    if (explicitTypeArgs.size() != 1) {
      reportCompilerError("weak_from_shared requires exactly one explicit type argument");
      return nullptr;
    }
    TypeInfo payloadInfo =
        applyActiveTypeBindings(explicitTypeArgs.front());
    finalizeTypeInfoMetadata(payloadInfo);
    std::string targetName =
        "weak<" + typeNameFromInfo(payloadInfo) + ">";
    TypeInfo targetInfo = makeTypeInfo(targetName);
    finalizeTypeInfoMetadata(targetInfo);
    if (getArgs().size() != 1) {
      reportCompilerError(
          "weak_from_shared expects exactly one shared<T> argument");
      return nullptr;
    }
    ParenExprAST paren(std::move(Args), false);
    paren.setTypeName(targetName);
    llvm::Value *constructed =
        emitTargetTypedConstruction(targetInfo, paren);
    resetArgs(paren.takeElements());
    if (!constructed)
      return nullptr;
    setTypeName(typeNameFromInfo(targetInfo));
    markTemporary();
    return constructed;
  }

  if (!hasCalleeExpr()) {
    TypeInfo calleeInfo = makeTypeInfo(decoratedCallee);
    finalizeTypeInfoMetadata(calleeInfo);
    if (calleeInfo.isSmartPointer()) {
      if (!materializeCompositeInstantiation(calleeInfo))
        return nullptr;
      bool treatAsTuple = Args.size() > 1;
      ParenExprAST paren(std::move(Args), treatAsTuple);
      paren.setTypeName(typeNameFromInfo(calleeInfo));
      llvm::Value *constructed =
          emitTargetTypedConstruction(calleeInfo, paren);
      resetArgs(paren.takeElements());
      if (!constructed)
        return nullptr;
      setTypeName(typeNameFromInfo(calleeInfo));
      markTemporary();
      return constructed;
    }
  }

  bool treatAsFunctionGenerics =
      !explicitTypeArgs.empty() &&
      FindGenericTemplate(baseCallee) == nullptr && !calleeIsCompositeType;
  bool preferGeneric = treatAsFunctionGenerics;

  if (!treatAsFunctionGenerics && explicitTypeArgs.empty() &&
      functionTemplates && !functionTemplates->empty()) {
    bool hasNonGenericOverload = false;
    if (auto overloadIt = CG.functionOverloads.find(baseCallee);
        overloadIt != CG.functionOverloads.end()) {
      hasNonGenericOverload = std::ranges::any_of(
          overloadIt->second,
          [](const FunctionOverload &overload) {
            return !overload.isGenericInstantiation;
          });
    }
    if (!hasNonGenericOverload) {
      reportCompilerError("Function '" + baseCallee +
                          "' requires explicit type arguments");
      return nullptr;
    }
  }

  if (hasCalleeExpr()) {
    if (auto *member = dynamic_cast<MemberAccessExprAST *>(getCalleeExpr()))
      return codegenMemberCall(*member);
    if (dynamic_cast<BaseExprAST *>(getCalleeExpr())) {
      const ActiveCompositeContext *ctx = currentCompositeContext();
      if (!ctx || ctx->kind != MethodKind::Constructor || ctx->isStatic) {
        reportCompilerError("'base(...)' is only valid inside instance constructors");
        return nullptr;
      }

      const CompositeTypeInfo *metadata = lookupCompositeInfo(ctx->name);
      if (!metadata || !metadata->baseClass) {
        reportCompilerError("Type '" + ctx->name + "' does not have a base class to construct");
        return nullptr;
      }

      auto structIt = StructTypes.find(ctx->name);
      if (structIt == StructTypes.end()) {
        reportCompilerError("Internal error: missing struct type for '" +
                            ctx->name + "' during base(...) emission");
        return nullptr;
      }

      auto thisIt = NamedValues.find("this");
      if (thisIt == NamedValues.end()) {
        reportCompilerError("Internal error: missing 'this' in constructor for '" +
                            ctx->name + "'");
        return nullptr;
      }

      auto *structPtr = llvm::dyn_cast<llvm::AllocaInst>(thisIt->second);
      if (!structPtr) {
        reportCompilerError("Internal error: unexpected 'this' storage while invoking base constructor for '" +
                            ctx->name + "'");
        return nullptr;
      }

      markBaseConstructorCall();
      setTypeName(*metadata->baseClass);
      return emitBaseConstructorInitialization(ctx->name, structIt->second,
                                               structPtr, *metadata, getArgs());
    }
    return LogErrorV("Unsupported call target expression");
  }

  std::vector<bool> ArgIsRef;
  ArgIsRef.reserve(getArgs().size());
  std::vector<llvm::Value *> ArgValues;
  ArgValues.reserve(getArgs().size());

  for (const auto &ArgExpr : getArgs()) {
    bool handled = false;

    if (baseCallee == "print") {
      auto resolveCompositeForArg = [&]() -> std::optional<std::string> {
        std::string name = baseCompositeName(ArgExpr->getTypeName());
        if (!name.empty())
          return name;

        if (auto *var = dynamic_cast<VariableExprAST *>(ArgExpr.get())) {
          if (const TypeInfo *info = lookupTypeInfo(var->getName())) {
            name = baseCompositeName(typeNameFromInfo(*info));
            if (!name.empty())
              return name;
          }
        }

        return std::nullopt;
      };

      std::optional<std::string> candidateName = resolveCompositeForArg();
      if (candidateName) {
        auto metaIt = CG.compositeMetadata.find(*candidateName);
        if (metaIt != CG.compositeMetadata.end() &&
            metaIt->second.thisOverride) {
          llvm::Value *instanceVal = ArgExpr->codegen_ptr();
          if (!instanceVal) {
            instanceVal = ArgExpr->codegen();
            if (!instanceVal)
              return nullptr;
          }

          llvm::Value *stringValue = emitThisOverrideString(
              metaIt->second, *candidateName, instanceVal);
          if (!stringValue)
            return nullptr;

          ArgIsRef.push_back(false);
          ArgValues.push_back(stringValue);
          handled = true;
        } else if (metaIt != CG.compositeMetadata.end()) {
          reportCompilerError(
              "Cannot print value of type '" + *candidateName +
                  "' because it does not implement string this()",
              "Add 'string this()' to provide a formatter or convert the value "
              "to string manually.");
          return nullptr;
        }
      }
    }

    if (handled)
      continue;

    llvm::Value *Value = ArgExpr->codegen();
    if (!Value)
      return nullptr;

    if (baseCallee == "print") {
      std::string nameAfterCodegen =
          baseCompositeName(ArgExpr->getTypeName());
      if (!nameAfterCodegen.empty()) {
        auto metaIt = CG.compositeMetadata.find(nameAfterCodegen);
        if (metaIt != CG.compositeMetadata.end() &&
            metaIt->second.thisOverride) {
          llvm::Value *stringValue = emitThisOverrideString(
              metaIt->second, nameAfterCodegen, Value);
          if (!stringValue)
            return nullptr;
          ArgIsRef.push_back(false);
          ArgValues.push_back(stringValue);
          continue;
        } else if (metaIt != CG.compositeMetadata.end()) {
          reportCompilerError(
              "Cannot print value of type '" + nameAfterCodegen +
                  "' because it does not implement string this()",
              "Add 'string this()' to provide a formatter or convert the value "
              "to string manually.");
          return nullptr;
        }
      }
    }

    bool isRef = dynamic_cast<RefExprAST *>(ArgExpr.get()) != nullptr;
    ArgIsRef.push_back(isRef);
    ArgValues.push_back(Value);
  }

  if (treatAsFunctionGenerics) {
    if (!functionTemplates) {
      reportCompilerError("Function '" + baseCallee +
                          "' does not declare generic parameters");
      return nullptr;
    }

    const auto arities = collectGenericArities(*functionTemplates);
    if (!std::ranges::any_of(
            arities, [&](std::size_t arity) { return arity == explicitTypeArgs.size(); })) {
      reportCompilerError("Function '" + baseCallee + "' expects " +
                              formatArityList(arities) + " type argument(s)",
                          "Provide exactly " + formatArityList(arities) +
                              " type argument(s) when calling this generic function.");
      return nullptr;
    }

    if (!InstantiateGenericFunction(baseCallee, explicitTypeArgs, nullptr))
      return nullptr;
  }

  if (decoratedCallee.find('<') != std::string::npos && !calleeIsCompositeType)
    lookupCompositeInfo(decoratedCallee);

  if (StructTypes.contains(decoratedCallee)) {
    markTemporary();
    llvm::Value *ConstructedValue =
        emitResolvedCall(baseCallee, std::move(ArgValues), ArgIsRef,
                         preferGeneric);
    if (!ConstructedValue)
      return nullptr;

    setTypeName(decoratedCallee);

    llvm::StructType *StructType = StructTypes[decoratedCallee];
    if (ConstructedValue->getType()->isPointerTy())
      return ConstructedValue;

    if (ConstructedValue->getType() != StructType)
      ConstructedValue = castToType(ConstructedValue, StructType);

    llvm::AllocaInst *StructAlloca = Builder->CreateAlloca(
        StructType, nullptr, decoratedCallee + "_inst");
    Builder->CreateStore(ConstructedValue, StructAlloca);
    return StructAlloca;
  }

  return emitResolvedCall(baseCallee, std::move(ArgValues), ArgIsRef,
                          preferGeneric);
}

static llvm::Value *emitResolvedCallInternal(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef,
    const std::vector<std::unique_ptr<ExprAST>> *originalArgs,
    bool preferGeneric, FunctionOverload *forced, ExprAST *typeOwner,
    const std::vector<ProvidedArgument> *providedArgs) {
  struct CandidateCall {
    FunctionOverload *overload = nullptr;
    llvm::Function *function = nullptr;
    std::vector<int> binding;
    std::vector<int> paramsBinding;
    bool paramsDirect = false;
    unsigned conversions = 0;
  };

  auto findParamsIndex = [](const std::vector<bool> &flags) -> int {
    for (size_t i = 0; i < flags.size(); ++i) {
      if (flags[i])
        return static_cast<int>(i);
    }
    return -1;
  };

  std::vector<FunctionOverload *> overloadList;
  if (forced) {
    overloadList.push_back(forced);
  } else {
    auto overloadIt = CG.functionOverloads.find(calleeBase);
    if (overloadIt == CG.functionOverloads.end()) {
      std::string baseName = baseCompositeName(calleeBase);
      if (!baseName.empty()) {
        auto baseIt = CG.functionOverloads.find(baseName);
        if (baseIt != CG.functionOverloads.end())
          overloadIt = baseIt;
      }
    }
    if (overloadIt == CG.functionOverloads.end())
      return LogErrorV(("Unknown function referenced: " + calleeBase).c_str());

    for (auto &overload : overloadIt->second)
      overloadList.push_back(&overload);
  }

  std::vector<ProvidedArgument> providedStorage;
  const std::vector<ProvidedArgument> *provided = providedArgs;
  if (!provided) {
    providedStorage.reserve(ArgValues.size());
    for (size_t i = 0; i < ArgValues.size(); ++i) {
      ProvidedArgument arg;
      arg.value = ArgValues[i];
      arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
      if (originalArgs && i < originalArgs->size())
        arg.expr = (*originalArgs)[i].get();
      providedStorage.push_back(std::move(arg));
    }
    provided = &providedStorage;
  }

  const bool hasNamedArguments = std::ranges::any_of(
      *provided, [](const ProvidedArgument &arg) { return !arg.name.empty(); });

  if (hasNamedArguments) {
    std::set<std::string> seenNames;
    for (const auto &arg : *provided) {
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
    for (auto *overload : overloadList) {
      for (const auto &name : overload->parameterNames) {
        if (!name.empty())
          knownNames.insert(name);
      }
    }
    for (const auto &arg : *provided) {
      if (arg.name.empty())
        continue;
      if (!knownNames.contains(arg.name)) {
        ScopedErrorLocation scoped(arg.nameLoc);
        reportCompilerError("Unknown parameter name '" + arg.name +
                            "' for call to '" + calleeBase + "'");
        return nullptr;
      }
    }
  }

  std::vector<CandidateCall> viable;
  viable.reserve(overloadList.size());
  std::optional<std::string> missingRequiredParam;
  bool sawTooManyArgs = false;

  for (auto *overload : overloadList) {
    if (!overload)
      continue;

    CandidateCall cand;
    cand.overload = overload;

    llvm::Function *CandidateFunc = overload->function;
    if (!CandidateFunc)
      CandidateFunc = TheModule->getFunction(overload->mangledName);
    if (!CandidateFunc)
      continue;
    overload->function = CandidateFunc;

    const size_t paramCount = overload->parameterTypes.size();
    std::vector<int> binding(paramCount, -1);
    std::vector<int> paramsBinding;
    const int paramsIndex = findParamsIndex(overload->parameterIsParams);
    size_t nextPositional = 0;
    bool failed = false;
    bool paramsNamed = false;

    for (size_t i = 0; i < provided->size(); ++i) {
      const auto &arg = (*provided)[i];
      if (arg.name.empty()) {
        while (nextPositional < paramCount && binding[nextPositional] != -1)
          ++nextPositional;
        if (paramsIndex >= 0 &&
            nextPositional == static_cast<size_t>(paramsIndex)) {
          if (paramsNamed) {
            failed = true;
            sawTooManyArgs = true;
            break;
          }
          for (size_t j = i; j < provided->size(); ++j) {
            if (!(*provided)[j].name.empty()) {
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
          sawTooManyArgs = true;
          break;
        }
        binding[nextPositional] = static_cast<int>(i);
        ++nextPositional;
        continue;
      }

      auto nameIt = std::find(overload->parameterNames.begin(),
                              overload->parameterNames.end(), arg.name);
      if (nameIt == overload->parameterNames.end()) {
        failed = true;
        break;
      }
      size_t paramIndex =
          static_cast<size_t>(nameIt - overload->parameterNames.begin());
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

    if (failed)
      continue;

    for (size_t idx = 0; idx < paramCount; ++idx) {
      if (paramsIndex >= 0 &&
          idx == static_cast<size_t>(paramsIndex)) {
        continue;
      }
      if (binding[idx] != -1)
        continue;
      const bool hasDefault =
          idx < overload->parameterDefaults.size() &&
          overload->parameterDefaults[idx].isSet();
      if (!hasDefault) {
        failed = true;
        if (!missingRequiredParam && idx < overload->parameterNames.size())
          missingRequiredParam = overload->parameterNames[idx];
        break;
      }
    }

    if (failed)
      continue;

    bool compatible = true;
    unsigned conversions = 0;

    auto checkCompatibility = [&](const ExprAST *argExpr,
                                  llvm::Value *argValue,
                                  const TypeInfo &expectedInfo,
                                  llvm::Type *expectedType,
                                  unsigned &conversionCount) -> bool {
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

      llvm::Type *ActualType = expectedType;
      if (argValue)
        ActualType = argValue->getType();

      if (ActualType == expectedType)
        return true;

      if (ActualType && expectedType && ActualType->isPointerTy() &&
          expectedType->isPointerTy()) {
        ++conversionCount;
        return true;
      }

      if (ActualType && expectedType &&
          areTypesCompatible(ActualType, expectedType)) {
        ++conversionCount;
        return true;
      }

      return false;
    };

    for (size_t idx = 0; idx < paramCount; ++idx) {
      if (paramsIndex >= 0 &&
          idx == static_cast<size_t>(paramsIndex))
        continue;

      bool argIsRef = binding[idx] >= 0
                          ? (*provided)[static_cast<size_t>(binding[idx])].isRef
                          : overload->parameterIsRef[idx];
      if (overload->parameterIsRef[idx] != argIsRef) {
        compatible = false;
        break;
      }

      const ExprAST *argExpr = nullptr;
      llvm::Value *argValue = nullptr;
      if (binding[idx] >= 0) {
        const auto &boundArg = (*provided)[static_cast<size_t>(binding[idx])];
        argExpr = boundArg.expr;
        argValue = boundArg.value;
      }

      llvm::Type *expectedType =
          CandidateFunc->getFunctionType()->getParamType(idx);
      if (!checkCompatibility(argExpr, argValue,
                              overload->parameterTypes[idx], expectedType,
                              conversions)) {
        compatible = false;
        break;
      }
    }

    if (compatible && paramsIndex >= 0) {
      if (paramsIndex >= static_cast<int>(overload->parameterIsRef.size()) ||
          overload->parameterIsRef[static_cast<size_t>(paramsIndex)]) {
        compatible = false;
      } else if (!paramsBinding.empty()) {
        TypeInfo arrayInfo = overload->parameterTypes[static_cast<size_t>(paramsIndex)];
        llvm::Type *expectedArrayType =
            CandidateFunc->getFunctionType()->getParamType(
                static_cast<size_t>(paramsIndex));

        bool directAllowed = paramsBinding.size() == 1;
        if (directAllowed) {
          const auto &arg = (*provided)[static_cast<size_t>(paramsBinding.front())];
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
          cand.paramsDirect = true;
        } else {
          auto elementInfo = extractElementTypeInfo(arrayInfo);
          if (!elementInfo) {
            compatible = false;
          } else {
            std::string elementTypeName = typeNameFromInfo(*elementInfo);
            llvm::Type *expectedElementType =
                getTypeFromString(elementTypeName);
            if (!expectedElementType) {
              compatible = false;
            } else {
              for (int argIndex : paramsBinding) {
                const auto &arg = (*provided)[static_cast<size_t>(argIndex)];
                if (arg.isRef) {
                  compatible = false;
                  break;
                }
                if (!checkCompatibility(arg.expr, arg.value, *elementInfo,
                                        expectedElementType, conversions)) {
                  compatible = false;
                  break;
                }
              }
            }
          }
        }
      }
    }

    if (!compatible)
      continue;

    cand.binding = std::move(binding);
    cand.paramsBinding = std::move(paramsBinding);
    cand.conversions = conversions;
    cand.function = CandidateFunc;
    viable.push_back(std::move(cand));
  }

  if (viable.empty()) {
    if (missingRequiredParam) {
      reportCompilerError(
          "Missing argument for parameter '" + *missingRequiredParam +
          "' in call to '" + calleeBase + "'");
      return nullptr;
    }
    if (sawTooManyArgs) {
      reportCompilerError("Too many arguments provided to call '" +
                          calleeBase + "'");
      return nullptr;
    }
    return LogErrorV(
        ("No matching overload found for call to '" + calleeBase + "'")
            .c_str());
  }

  if (preferGeneric) {
    std::vector<CandidateCall> genericViable;
    genericViable.reserve(viable.size());
    for (auto &candidate : viable) {
      if (candidate.overload && candidate.overload->isGenericInstantiation)
        genericViable.push_back(std::move(candidate));
    }
    if (!genericViable.empty())
      viable = std::move(genericViable);
  }

  auto bestIt = std::min_element(
      viable.begin(), viable.end(),
      [](const CandidateCall &lhs, const CandidateCall &rhs) {
        return lhs.conversions < rhs.conversions;
      });

  unsigned bestConversions = bestIt->conversions;
  unsigned bestCount = static_cast<unsigned>(std::count_if(
      viable.begin(), viable.end(),
      [bestConversions](const CandidateCall &candidate) {
        return candidate.conversions == bestConversions;
      }));

  if (bestCount > 1)
    return LogErrorV(("Ambiguous call to '" + calleeBase + "'").c_str());

  CandidateCall *selected = &*bestIt;
  FunctionOverload *chosen = selected->overload;

  llvm::Function *CalleeF = selected->function;
  if (!CalleeF) {
    CalleeF = TheModule->getFunction(chosen->mangledName);
    if (!CalleeF)
      return LogErrorV(("Internal error: overload for '" + calleeBase +
                        "' lacks function definition").c_str());
    chosen->function = CalleeF;
  }

  std::vector<llvm::Value *> CallArgs;
  CallArgs.reserve(selected->binding.size());
  std::vector<bool> CallArgIsRef;
  CallArgIsRef.reserve(selected->binding.size());
  std::vector<std::unique_ptr<ExprAST>> ownedDefaultExprs;
  const int paramsIndex = findParamsIndex(chosen->parameterIsParams);

  for (size_t idx = 0; idx < selected->binding.size(); ++idx) {
    if (paramsIndex >= 0 &&
        idx == static_cast<size_t>(paramsIndex)) {
      if (selected->paramsDirect) {
        const auto &arg =
            (*provided)[static_cast<size_t>(selected->paramsBinding.front())];
        CallArgs.push_back(arg.value);
        CallArgIsRef.push_back(arg.isRef);
      } else {
        llvm::Value *packed =
            emitPackedParamsArray(selected->paramsBinding, *provided,
                                  chosen->parameterTypes[idx], calleeBase);
        if (!packed)
          return nullptr;
        CallArgs.push_back(packed);
        CallArgIsRef.push_back(false);
      }
      continue;
    }
    int bindingIndex = selected->binding[idx];
    if (bindingIndex >= 0) {
      const auto &arg = (*provided)[static_cast<size_t>(bindingIndex)];
      CallArgs.push_back(arg.value);
      CallArgIsRef.push_back(arg.isRef);
    } else {
      ScopedErrorLocation scoped(
          idx < chosen->parameterDefaultLocations.size()
              ? chosen->parameterDefaultLocations[idx]
              : SourceLocation{});
      std::unique_ptr<ExprAST> defaultExpr =
          instantiateDefaultExpr(idx < chosen->parameterDefaults.size()
                                     ? chosen->parameterDefaults[idx]
                                     : DefaultArgInfo{});
      if (!defaultExpr) {
        reportCompilerError("Default value unavailable for parameter '" +
                            (idx < chosen->parameterNames.size()
                                 ? chosen->parameterNames[idx]
                                 : std::to_string(idx)) +
                            "'");
        return nullptr;
      }
      std::string targetTypeName =
          typeNameFromInfo(chosen->parameterTypes[idx]);
      defaultExpr->setTypeName(targetTypeName);
      defaultExpr->markTemporary();
      llvm::Value *value = defaultExpr->codegen();
      if (!value)
        return nullptr;
      CallArgs.push_back(value);
      CallArgIsRef.push_back(false);
      ownedDefaultExprs.push_back(std::move(defaultExpr));
    }
  }

  std::vector<llvm::Value *> FinalArgs;
  FinalArgs.reserve(CallArgs.size());

  for (size_t idx = 0; idx < CallArgs.size(); ++idx) {
    llvm::Value *ArgVal = CallArgs[idx];
    llvm::Type *ExpectedType =
        CalleeF->getFunctionType()->getParamType(idx);
    if (chosen->parameterIsRef[idx]) {
      if (ExpectedType && ExpectedType->isPointerTy() &&
          !ArgVal->getType()->isPointerTy()) {
        llvm::AllocaInst *tmp = Builder->CreateAlloca(
            ArgVal->getType(), nullptr,
            buildArcOpLabel(calleeBase, "ref.arg"));
        Builder->CreateStore(ArgVal, tmp);
        ArgVal = tmp;
      }
      if (ExpectedType && ExpectedType->isPointerTy() &&
          ArgVal->getType() != ExpectedType) {
        ArgVal = Builder->CreateBitCast(
            ArgVal, ExpectedType,
            buildArcOpLabel(calleeBase, "ref.cast"));
      }
    } else {
      const std::string targetTypeName =
          typeNameFromInfo(chosen->parameterTypes[idx]);
      ArgVal = castToType(ArgVal, ExpectedType, targetTypeName);
    }
    FinalArgs.push_back(ArgVal);
  }

  llvm::Value *CallValue = nullptr;
  if (CalleeF->getReturnType()->isVoidTy()) {
    CallValue = Builder->CreateCall(CalleeF, FinalArgs);
    if (typeOwner)
      typeOwner->setTypeName("void");
  } else {
    CallValue = Builder->CreateCall(CalleeF, FinalArgs, "calltmp");
    if (typeOwner)
      typeOwner->setTypeName(typeNameFromInfo(chosen->returnType));

    if (chosen->returnsByRef) {
      llvm::Type *valueType = getTypeFromString(typeNameFromInfo(chosen->returnType));
      if (!valueType)
        return LogErrorV("Unable to determine ref return type for call");
      CallValue = Builder->CreateLoad(valueType, CallValue, "refcalltmp");
    }
  }

  return CallValue;
}

llvm::Value *CallExprAST::emitResolvedCall(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef, bool preferGeneric,
    FunctionOverload *forced) {
  std::vector<ProvidedArgument> provided;
  provided.reserve(ArgValues.size());
  for (size_t i = 0; i < ArgValues.size(); ++i) {
    ProvidedArgument arg;
    arg.value = ArgValues[i];
    arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
    if (i < Args.size())
      arg.expr = Args[i].get();
    if (i < ArgNames.size())
      arg.name = ArgNames[i];
    if (i < ArgNameLocations.size())
      arg.nameLoc = ArgNameLocations[i];
    if (i < ArgEqualsLocations.size())
      arg.equalsLoc = ArgEqualsLocations[i];
    provided.push_back(std::move(arg));
  }

  return emitResolvedCallInternal(calleeBase, std::move(ArgValues), ArgIsRef,
                                  &Args, preferGeneric, forced, this,
                                  &provided);
}

llvm::Value *CallExprAST::codegenMemberCall(MemberAccessExprAST &member) {
  llvm::Value *instancePtr = nullptr;
  llvm::Value *instanceValue = nullptr;

  std::string ownerName =
      sanitizeCompositeLookupName(member.getObject()->getTypeName());

  if (ownerName.empty()) {
    instancePtr = member.getObject()->codegen_ptr();
    if (!instancePtr) {
      instanceValue = member.getObject()->codegen();
      if (!instanceValue)
        return nullptr;
      if (instanceValue->getType()->isPointerTy()) {
        instancePtr = instanceValue;
      } else {
        llvm::AllocaInst *Tmp =
            Builder->CreateAlloca(instanceValue->getType(), nullptr,
                                  "method.recv");
        Builder->CreateStore(instanceValue, Tmp);
        instancePtr = Tmp;
      }
    }
    ownerName = resolveCompositeName(member.getObject());
  }

  if (ownerName.empty())
    return LogErrorV("Unable to determine composite type for member call");


  const CompositeTypeInfo *info = lookupCompositeInfo(ownerName);
  if (!info)
    return LogErrorV(("Type '" + ownerName +
                      "' has no metadata for member calls")
                         .c_str());

  const bool isDtorCall =
      member.isDestructorAccess() || isDestructorCall();

  auto ensureInstancePointer = [&]() -> llvm::Value * {
    if (instancePtr)
      return instancePtr;
    instancePtr = member.getObject()->codegen_ptr();
    if (!instancePtr) {
      instanceValue = member.getObject()->codegen();
      if (!instanceValue)
        return nullptr;
      if (instanceValue->getType()->isPointerTy()) {
        instancePtr = instanceValue;
      } else {
        llvm::AllocaInst *Tmp =
            Builder->CreateAlloca(instanceValue->getType(), nullptr,
                                  "method.recv");
        Builder->CreateStore(instanceValue, Tmp);
        instancePtr = Tmp;
      }
    }
    return instancePtr;
  };

  if (isDtorCall && info->smartPointerKind != SmartPointerKind::None) {
    reportCompilerError(
        "Manual destructor calls are not allowed on smart pointers",
        "Control blocks manage the payload; calling '~" +
            member.getMemberName() +
            "()' manually can double-drop or corrupt the managed value.");
    return nullptr;
  }

  if (isDtorCall) {
    TypeInfo ownerInfoFull = makeTypeInfo(member.getObject()->getTypeName());
    ownerInfoFull = applyActiveTypeBindings(ownerInfoFull);
    finalizeTypeInfoMetadata(ownerInfoFull);
    if (ownerInfoFull.ownership != OwnershipQualifier::Strong) {
      reportCompilerError(
          "Manual destructor calls require strong-owned targets",
          "Convert weak or unowned references to strong ones before invoking a destructor.");
      return nullptr;
    }
    if (!info->hasDestructor || info->destructorFunctionName.empty()) {
      reportCompilerError("Type '" + ownerName +
                          "' does not declare a destructor");
      return nullptr;
    }
    if (!getArgs().empty()) {
      reportCompilerError("Destructor calls cannot take arguments");
      return nullptr;
    }
    std::string targetName =
        sanitizeCompositeLookupName(member.getMemberName());
    if (!targetName.empty() && targetName != ownerName) {
      reportCompilerError("Destructor '~" + targetName +
                          "()' does not match receiver of type '" + ownerName +
                          "'");
      return nullptr;
    }
    if (!ensureMemberAccessAllowed(info->destructorModifiers,
                                   AccessIntent::Call, ownerName,
                                   "~" + ownerName))
      return nullptr;

    llvm::Value *thisPtr = ensureInstancePointer();
    if (!thisPtr)
      return nullptr;

    llvm::Function *dtorFn =
        TheModule->getFunction(info->destructorFunctionName);
    if (!dtorFn) {
      reportCompilerError(
          "Internal error: destructor '" + info->destructorFunctionName +
          "' missing during call emission");
      return nullptr;
    }

    llvm::Value *dispatchTarget = nullptr;
    if (!info->baseClass &&
        info->kind == AggregateKind::Class &&
        info->destructorVtableSlot !=
            std::numeric_limits<unsigned>::max()) {
      auto structIt = StructTypes.find(ownerName);
      if (structIt != StructTypes.end() && info->hasARCHeader) {
        llvm::StructType *structTy = structIt->second;
        llvm::StructType *headerTy = getArcHeaderType();
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            structTy, thisPtr, info->headerFieldIndex, "dtor.header");
        llvm::Value *descAddr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, "dtor.desc.addr");
        llvm::Value *descriptor = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), descAddr, "dtor.desc");
        llvm::Value *vtableAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), descriptor, 2, "dtor.vtable.addr");
        llvm::Value *vtablePtr =
            Builder->CreateLoad(pointerType(), vtableAddr, "dtor.vtable");
        llvm::Value *slotIndex = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext),
            static_cast<uint64_t>(info->destructorVtableSlot));
        llvm::Value *slotPtr = Builder->CreateInBoundsGEP(
            pointerType(), vtablePtr, slotIndex, "dtor.vslot");
        llvm::Value *slotFnPtr =
            Builder->CreateLoad(pointerType(), slotPtr, "dtor.fptr");
        llvm::Type *fnPtrTy = pointerType(dtorFn->getFunctionType());
        dispatchTarget =
            Builder->CreateBitCast(slotFnPtr, fnPtrTy, "dtor.target");
      }
    }

    llvm::Value *callArg = thisPtr;
    if (!dtorFn->arg_empty()) {
      llvm::Type *expected = dtorFn->getFunctionType()->getParamType(0);
      if (callArg->getType() != expected) {
        callArg = Builder->CreateBitCast(
            thisPtr, expected, "dtor.manual.this");
      }
    }

    TypeInfo ownerInfo = ownerInfoFull;
    llvm::Value *retained =
        emitArcRetain(callArg, ownerInfo,
                      ownerName + ".dtor.manual.retain");
    llvm::Value *callValue = nullptr;
    if (dispatchTarget) {
      if (dtorFn->arg_empty()) {
        callValue = Builder->CreateCall(dtorFn->getFunctionType(),
                                        dispatchTarget, {});
      } else {
        callValue = Builder->CreateCall(dtorFn->getFunctionType(),
                                        dispatchTarget, {retained});
      }
    } else {
      callValue = dtorFn->arg_empty()
                      ? Builder->CreateCall(dtorFn)
                      : Builder->CreateCall(dtorFn, {retained});
    }
    const CompositeTypeInfo *baseCursor = info;
    llvm::Value *basePtr = thisPtr;
    while (baseCursor && baseCursor->baseClass) {
      auto baseStructIt = StructTypes.find(*baseCursor->baseClass);
      if (baseStructIt != StructTypes.end() &&
          basePtr->getType() != pointerType(baseStructIt->second)) {
        basePtr = Builder->CreateBitCast(
            basePtr, pointerType(baseStructIt->second),
            ownerName + ".dtor.base.ptr");
      }
      const CompositeTypeInfo *baseInfo =
          lookupCompositeInfo(*baseCursor->baseClass);
      if (baseInfo && baseInfo->hasDestructor &&
          !baseInfo->destructorFunctionName.empty()) {
        llvm::Function *baseDtor =
            TheModule->getFunction(baseInfo->destructorFunctionName);
        if (!baseDtor) {
          reportCompilerError("Internal error: base destructor '" +
                              baseInfo->destructorFunctionName +
                              "' missing during manual call");
          break;
        }
        llvm::Value *baseThis = basePtr;
        if (!baseDtor->arg_empty()) {
          llvm::Type *expected =
              baseDtor->getFunctionType()->getParamType(0);
          if (baseThis->getType() != expected)
            baseThis = Builder->CreateBitCast(
                basePtr, expected, ownerName + ".dtor.base.this");
          Builder->CreateCall(baseDtor, {baseThis});
        } else {
          Builder->CreateCall(baseDtor, {});
        }
      }
      baseCursor = baseInfo;
    }
    if (info->kind == AggregateKind::Class &&
        info->destructorVtableSlot !=
            std::numeric_limits<unsigned>::max()) {
      auto structIt = StructTypes.find(ownerName);
      if (structIt != StructTypes.end()) {
        llvm::StructType *structTy = structIt->second;
        llvm::StructType *headerTy = getArcHeaderType();
        llvm::Value *headerPtr = Builder->CreateStructGEP(
            structTy, thisPtr, info->headerFieldIndex,
            ownerName + ".dtor.base.header");
        llvm::Value *descAddr = Builder->CreateStructGEP(
            headerTy, headerPtr, 2, ownerName + ".dtor.base.desc.addr");
        llvm::Value *dynamicDesc = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), descAddr,
            ownerName + ".dtor.base.desc");
        llvm::Value *baseDescAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), dynamicDesc, 1,
            ownerName + ".dtor.parent.addr");
        llvm::Value *baseDescriptor = Builder->CreateLoad(
            pointerType(getTypeDescriptorType()), baseDescAddr,
            ownerName + ".dtor.parent");
        llvm::Value *hasBase = Builder->CreateICmpNE(
            baseDescriptor,
            llvm::ConstantPointerNull::get(
                pointerType(getTypeDescriptorType())),
            ownerName + ".dtor.has.parent");
        llvm::Function *parentFn = Builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *baseCallBB = llvm::BasicBlock::Create(
            *TheContext, ownerName + ".dtor.base.call", parentFn);
        llvm::BasicBlock *baseSkipBB = llvm::BasicBlock::Create(
            *TheContext, ownerName + ".dtor.base.skip", parentFn);
        Builder->CreateCondBr(hasBase, baseCallBB, baseSkipBB);
        Builder->SetInsertPoint(baseCallBB);
        llvm::Value *baseVtableAddr = Builder->CreateStructGEP(
            getTypeDescriptorType(), baseDescriptor, 2,
            ownerName + ".dtor.base.vtable.addr");
        llvm::Value *baseVtable =
            Builder->CreateLoad(pointerType(), baseVtableAddr,
                                ownerName + ".dtor.base.vtable");
        llvm::Value *slotIndex = llvm::ConstantInt::get(
            llvm::Type::getInt64Ty(*TheContext),
            static_cast<uint64_t>(info->destructorVtableSlot));
        llvm::Value *baseSlotPtr = Builder->CreateInBoundsGEP(
            pointerType(), baseVtable, slotIndex,
            ownerName + ".dtor.base.vslot");
        llvm::Value *baseSlotFn =
            Builder->CreateLoad(pointerType(), baseSlotPtr,
                                ownerName + ".dtor.base.fn");
        llvm::Value *baseTypedFn = Builder->CreateBitCast(
            baseSlotFn, pointerType(dtorFn->getFunctionType()),
            ownerName + ".dtor.base.target");
        llvm::Value *baseThis = thisPtr;
        if (!dtorFn->arg_empty()) {
          llvm::Type *expected =
              dtorFn->getFunctionType()->getParamType(0);
          if (baseThis->getType() != expected)
            baseThis = Builder->CreateBitCast(
                thisPtr, expected, ownerName + ".dtor.base.this");
          Builder->CreateCall(dtorFn->getFunctionType(), baseTypedFn,
                              {baseThis});
        } else {
          Builder->CreateCall(dtorFn->getFunctionType(), baseTypedFn, {});
        }
        Builder->CreateBr(baseSkipBB);
        Builder->SetInsertPoint(baseSkipBB);
      }
    }
    emitArcRelease(retained, ownerInfo,
                   ownerName + ".dtor.manual.release");
    if (auto *allocaPtr = llvm::dyn_cast<llvm::AllocaInst>(thisPtr))
      markArcSlotDestroyed(allocaPtr);
    setTypeName("void");
    return callValue;
  }

  auto methodIt = info->methodInfo.find(member.getMemberName());
  if (methodIt == info->methodInfo.end())
    return LogErrorV(("Member '" + member.getMemberName() + "' of type '" +
                      ownerName + "' is not a method").c_str());

  if (!ensureMemberAccessAllowed(methodIt->second.modifiers,
                                 AccessIntent::Call, ownerName,
                                 member.getMemberName()))
    return nullptr;

  bool isStaticMethod =
      static_cast<uint8_t>(methodIt->second.modifiers.storage &
                           StorageFlag::Static) != 0;

  std::vector<bool> ArgIsRef;
  std::vector<llvm::Value *> ArgValues;

  if (!isStaticMethod) {
    if (!instancePtr) {
      instancePtr = ensureInstancePointer();
      if (!instancePtr)
        return nullptr;
    }

    ArgIsRef.push_back(true);
    ArgValues.push_back(instancePtr);
  }

  for (const auto &ArgExpr : getArgs()) {
    bool isRef = dynamic_cast<RefExprAST *>(ArgExpr.get()) != nullptr;
    ArgIsRef.push_back(isRef);
    llvm::Value *Value = ArgExpr->codegen();
    if (!Value)
      return nullptr;
    ArgValues.push_back(Value);
  }

  const CompositeMemberInfo &memberInfo = methodIt->second;
  std::vector<TypeInfo> methodTypeArgs;
  if (member.hasExplicitGenerics()) {
    std::string decoratedMemberName =
        member.getMemberName() + member.getGenericArguments();
    std::string parsedBase;
    if (!parseExplicitTypeArgumentSuffix(decoratedMemberName, parsedBase,
                                         methodTypeArgs))
      return nullptr;
    if (parsedBase != member.getMemberName()) {
      reportCompilerError("Internal error: mismatched member name while parsing generic arguments");
      return nullptr;
    }
  }

  if (!methodTypeArgs.empty() && !memberInfo.isGenericTemplate) {
    reportCompilerError("Method '" + memberInfo.signature +
                        "' does not accept explicit type arguments");
    return nullptr;
  }

  if (memberInfo.isGenericTemplate) {
    if (methodTypeArgs.empty()) {
      reportCompilerError("Method '" + memberInfo.signature +
                          "' requires explicit type arguments");
      return nullptr;
    }

    const auto *templates =
        lookupGenericFunctionTemplates(memberInfo.signature);
    if (!templates || templates->empty()) {
      reportCompilerError("Internal error: generic method template '" +
                          memberInfo.signature + "' is not registered");
      return nullptr;
    }

    const auto arities = collectGenericArities(*templates);
    if (!std::ranges::any_of(
            arities, [&](std::size_t arity) { return arity == methodTypeArgs.size(); })) {
      reportCompilerError("Method '" + memberInfo.signature + "' expects " +
                              formatArityList(arities) + " type argument(s)",
                          "Provide " + formatArityList(arities) +
                              " type argument(s) for this generic method.");
      return nullptr;
    }

    const std::map<std::string, TypeInfo> *additionalBindings =
        &info->typeArgumentBindings;
    llvm::Function *instantiated =
        InstantiateGenericFunction(memberInfo.signature, methodTypeArgs,
                                   additionalBindings);
    if (!instantiated)
      return nullptr;
    if (auto infoRefIt = CG.compositeMetadata.find(ownerName);
        infoRefIt != CG.compositeMetadata.end()) {
      auto &records =
          infoRefIt->second.genericMethodInstantiations[memberInfo.signature];
      std::string mangled(instantiated->getName());
      if (std::find(records.begin(), records.end(), mangled) == records.end())
        records.push_back(std::move(mangled));
    }
  }

  if (!isStaticMethod && info->kind == AggregateKind::Interface) {
    if (!instancePtr)
      return LogErrorV("Interface member call requires an instance");

    auto slotIt =
        info->interfaceMethodSlotMap.find(memberInfo.dispatchKey);
    if (slotIt == info->interfaceMethodSlotMap.end())
      return LogErrorV(("Internal error: interface slot unresolved for '" +
                        memberInfo.dispatchKey + "'")
                           .c_str());

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateBitCast(
        instancePtr, pointerType(headerTy), "hybrid.header.iface");
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    llvm::GlobalVariable *ifaceDescriptorGV =
        TheModule->getGlobalVariable(info->descriptorGlobalName, true);
    if (!ifaceDescriptorGV)
      return LogErrorV(("Internal error: interface descriptor '" +
                        info->descriptorGlobalName +
                        "' missing during dispatch")
                           .c_str());
    llvm::Value *ifaceDescriptorConst =
        llvm::ConstantExpr::getBitCast(ifaceDescriptorGV, typeDescPtrTy);

    llvm::Function *lookupFn = getInterfaceLookupFunction();
    llvm::Value *methodTablePtr =
        Builder->CreateCall(lookupFn,
                            {descriptorValue, ifaceDescriptorConst},
                            "hybrid.iface.table");

    llvm::Value *isNull = Builder->CreateICmpEQ(
        methodTablePtr, llvm::ConstantPointerNull::get(voidPtrPtrTy),
        "hybrid.iface.table.null");

    llvm::Function *parentFunc = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *failBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.fail", parentFunc);
    llvm::BasicBlock *contBB =
        llvm::BasicBlock::Create(*TheContext, "iface.lookup.cont", parentFunc);
    Builder->CreateCondBr(isNull, failBB, contBB);

    Builder->SetInsertPoint(failBB);
    llvm::Function *abortFn = TheModule->getFunction("abort");
    if (!abortFn) {
      llvm::FunctionType *abortTy =
          llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      abortFn = llvm::Function::Create(abortTy,
                                       llvm::Function::ExternalLinkage, "abort",
                                       TheModule.get());
    }
    Builder->CreateCall(abortFn);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(contBB);

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), slotIt->second);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, methodTablePtr, slotIndex, "hybrid.iface.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.iface.fn");

    return emitDynamicFunctionCall(*this, memberInfo, fnPtrRaw, ArgValues,
                                   ArgIsRef);
  }

  bool isBaseQualifier =
      dynamic_cast<BaseExprAST *>(member.getObject()) != nullptr;
  bool canVirtual = !isStaticMethod &&
                    info->kind == AggregateKind::Class &&
                    memberInfo.vtableSlot !=
                        std::numeric_limits<unsigned>::max() &&
                    !isBaseQualifier;

  if (canVirtual) {
    auto structIt = StructTypes.find(ownerName);
    if (structIt == StructTypes.end())
      return LogErrorV(("Internal error: struct type for '" + ownerName +
                        "' unavailable during dispatch")
                           .c_str());

    llvm::StructType *ownerStructTy = structIt->second;
    llvm::Value *typedInstancePtr = instancePtr;
    if (typedInstancePtr->getType() != pointerType(ownerStructTy)) {
      typedInstancePtr = Builder->CreateBitCast(
          typedInstancePtr, pointerType(ownerStructTy),
          "hybrid.method.recv");
    }

    llvm::StructType *headerTy = getArcHeaderType();
    llvm::Value *headerPtr = Builder->CreateStructGEP(
        ownerStructTy, typedInstancePtr, 0, "hybrid.header.ptr");
    auto *typeDescPtrTy =
        pointerType(getTypeDescriptorType());
    llvm::Value *descAddr = Builder->CreateStructGEP(
        headerTy, headerPtr, 2, "hybrid.header.descptr");
    llvm::Value *descriptorValue = Builder->CreateLoad(
        typeDescPtrTy, descAddr, "hybrid.header.desc");

    auto *voidPtrTy =
        pointerType(llvm::Type::getInt8Ty(*TheContext));
    auto *voidPtrPtrTy = pointerType(voidPtrTy);
    llvm::Value *vtableAddr = Builder->CreateStructGEP(
        getTypeDescriptorType(), descriptorValue, 2, "hybrid.vtable.ptr");
    llvm::Value *vtablePtr =
        Builder->CreateLoad(voidPtrPtrTy, vtableAddr, "hybrid.vtable");

    llvm::Value *slotIndex = llvm::ConstantInt::get(
        llvm::Type::getInt32Ty(*TheContext), memberInfo.vtableSlot);
    llvm::Value *fnPtrAddr = Builder->CreateInBoundsGEP(
        voidPtrTy, vtablePtr, slotIndex, "hybrid.vtable.fnptr");
    llvm::Value *fnPtrRaw =
        Builder->CreateLoad(voidPtrTy, fnPtrAddr, "hybrid.vtable.fn");

    return emitDynamicFunctionCall(*this, memberInfo, fnPtrRaw, ArgValues,
                                   ArgIsRef);
  }

  FunctionOverload *forcedOverload = nullptr;
  const bool hasParamsParam = std::ranges::any_of(
      memberInfo.parameterIsParams,
      [](bool isParams) { return isParams; });
  if (memberInfo.directFunction &&
      ArgValues.size() == memberInfo.parameterTypes.size() &&
      !hasParamsParam) {
    llvm::Function *DirectFunc = memberInfo.directFunction;
    for (size_t idx = 0; idx < ArgValues.size(); ++idx) {
      llvm::Value *argVal = ArgValues[idx];
      llvm::Type *ExpectedType =
          DirectFunc->getFunctionType()->getParamType(idx);
      if (idx < memberInfo.parameterIsRef.size() &&
          memberInfo.parameterIsRef[idx]) {
        if (ExpectedType && ExpectedType->isPointerTy() &&
            !argVal->getType()->isPointerTy()) {
          llvm::AllocaInst *tmp = Builder->CreateAlloca(
              argVal->getType(), nullptr,
              buildArcOpLabel(memberInfo.signature, "ref.arg"));
          Builder->CreateStore(argVal, tmp);
          argVal = tmp;
        }
        if (ExpectedType && ExpectedType->isPointerTy() &&
            argVal->getType() != ExpectedType) {
          argVal = Builder->CreateBitCast(
              argVal, ExpectedType,
              buildArcOpLabel(memberInfo.signature, "ref.cast"));
        }
      } else {
        const std::string targetTypeName =
            idx < memberInfo.parameterTypes.size()
                ? typeNameFromInfo(memberInfo.parameterTypes[idx])
                : "";
        argVal = castToType(argVal, ExpectedType, targetTypeName);
      }
      ArgValues[idx] = argVal;
    }
    if (DirectFunc->getReturnType()->isVoidTy()) {
      llvm::Value *CallValue = Builder->CreateCall(DirectFunc, ArgValues);
      setTypeName("void");
      return CallValue;
    }
    llvm::Value *CallValue =
        Builder->CreateCall(DirectFunc, ArgValues, "calltmp");
    setTypeName(typeNameFromInfo(memberInfo.returnType));
    return CallValue;
  }
  if (auto overloadIt = CG.functionOverloads.find(memberInfo.signature);
      overloadIt != CG.functionOverloads.end()) {
    for (auto &candidate : overloadIt->second) {
      if (candidate.parameterTypes.size() !=
          memberInfo.parameterTypes.size())
        continue;
      if (candidate.returnsByRef != memberInfo.returnsByRef)
        continue;
      if (!typeInfoEquals(candidate.returnType, memberInfo.returnType))
        continue;
      bool match = true;
      for (size_t idx = 0; idx < candidate.parameterTypes.size(); ++idx) {
        if (candidate.parameterIsRef[idx] != memberInfo.parameterIsRef[idx] ||
            !typeInfoEquals(candidate.parameterTypes[idx],
                            memberInfo.parameterTypes[idx])) {
          match = false;
          break;
        }
      }
      if (match) {
        forcedOverload = &candidate;
        break;
      }
    }
  }

  std::vector<ProvidedArgument> provided;
  provided.reserve(ArgValues.size());
  const size_t receiverOffset =
      ArgValues.size() > Args.size() ? ArgValues.size() - Args.size() : 0;
  for (size_t i = 0; i < ArgValues.size(); ++i) {
    ProvidedArgument arg;
    arg.value = ArgValues[i];
    arg.isRef = i < ArgIsRef.size() ? ArgIsRef[i] : false;
    if (i >= receiverOffset) {
      size_t userIndex = i - receiverOffset;
      if (userIndex < Args.size())
        arg.expr = Args[userIndex].get();
      if (userIndex < ArgNames.size())
        arg.name = ArgNames[userIndex];
      if (userIndex < ArgNameLocations.size())
        arg.nameLoc = ArgNameLocations[userIndex];
      if (userIndex < ArgEqualsLocations.size())
        arg.equalsLoc = ArgEqualsLocations[userIndex];
    }
    provided.push_back(std::move(arg));
  }

  return emitResolvedCallInternal(
      memberInfo.signature, std::move(ArgValues), ArgIsRef, &Args,
      memberInfo.isGenericTemplate && member.hasExplicitGenerics(),
      forcedOverload, this, &provided);
}

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

// Generate code for return statements
llvm::Value *ReturnStmtAST::codegen() {
  llvm::Function *currentFunction = nullptr;
  if (auto *currentBlock = Builder->GetInsertBlock())
    currentFunction = currentBlock->getParent();

  if (getReturnValue()) {
    llvm::Value *Val;

    // Check if this is a ref return
    if (isRef()) {
      if (auto escapingLocal =
              findEscapingLocalRef(getReturnValue(), currentFunction)) {
        reportCompilerError("Cannot return local variable '" + *escapingLocal +
                            "' by reference",
                            "Return a value or a reference to caller-owned "
                            "storage instead");
        return nullptr;
      }

      // For ref return, need to return a pointer
      // Use codegen_ptr if the return value supports it
      if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getReturnValue())) {
        Val = VarExpr->codegen_ptr();
      }
      else if (auto *ArrayIdxExpr = dynamic_cast<ArrayIndexExprAST*>(getReturnValue())) {
        Val = ArrayIdxExpr->codegen_ptr();
      }
      else if (auto *MemberExpr = dynamic_cast<MemberAccessExprAST*>(getReturnValue())) {
        Val = MemberExpr->codegen_ptr();
      }
      else {
        return LogErrorV("return ref can only be used with lvalues (variables, array elements, or struct members)");
      }
    } else {
      Val = getReturnValue()->codegen();
    }

    if (!Val)
      return nullptr;

    if (!isRef()) {
      auto maybePromoteStackReturn = [&](llvm::Value *value) -> llvm::Value * {
        if (!value)
          return nullptr;
        auto *allocaPtr = llvm::dyn_cast<llvm::AllocaInst>(value);
        if (!allocaPtr)
          return value;

        std::string retTypeName = getReturnValue()->getTypeName();
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

  const std::vector<int64_t> explicitArrayDims =
      parseExplicitArrayDimensions(declaredInfo);

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
    if (declaredInfo.isSmartPointer()) {
      if (auto *paren = dynamic_cast<ParenExprAST *>(expr))
        return emitTargetTypedConstruction(declaredInfo, *paren);
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

// Generate code for for each statements
llvm::Value *ForEachStmtAST::codegen() {
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
    }
  }
  for (const std::string &ifaceName : metadata.interfaces) {
    if (const CompositeTypeInfo *ifaceInfo = lookupCompositeInfo(ifaceName)) {
      for (const auto &entry : ifaceInfo->methodInfo)
        metadata.methodInfo.emplace(entry.first, entry.second);
    }
  }
  if (!validateCompositeHierarchy(typeKey, metadata)) {
    abandonStructDefinition();
    return nullptr;
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
  
  // Generate constructor and other methods
  for (auto &MethodDef : Methods) {
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
                                       MethodDef.isStatic());
    llvm::Function *GeneratedFunction = Method->codegen();
    if (GeneratedFunction) {
      auto infoIt = metadata.methodInfo.find(MethodDef.getDisplayName());
      if (infoIt != metadata.methodInfo.end())
        infoIt->second.directFunction = GeneratedFunction;
    }
  }

  if (metadata.kind == AggregateKind::Interface) {
    computeInterfaceMethodLayout(typeKey, Methods, metadata);
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
  bool isStaticField = false;
  std::string staticFieldType;
  std::string staticGlobalName;
  MemberModifiers defaultStaticModifiers;
  std::string StructLookupName;
  std::string ObjectTypeName;

  if (auto *VarObj = dynamic_cast<VariableExprAST *>(Object.get())) {
    StructLookupName = VarObj->getName();
    info = lookupCompositeInfo(StructLookupName);
    if (info) {
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
    StructLookupName = ObjectTypeDesc.sanitized;

    info = lookupCompositeInfo(StructLookupName);
    if (info) {
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
  bool isStaticField = false;
  std::string staticGlobalName;
  MemberModifiers defaultStaticModifiers;
  std::string StructLookupName;
  std::string ObjectTypeName;

  if (auto *VarObj = dynamic_cast<VariableExprAST *>(Object.get())) {
    StructLookupName = VarObj->getName();
    info = lookupCompositeInfo(StructLookupName);
    if (info) {
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
    StructLookupName = ObjectTypeDesc.sanitized;

    info = lookupCompositeInfo(StructLookupName);
    if (info) {
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

// Generate code for switch expressions
llvm::Value *SwitchExprAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Evaluate the switch condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create basic blocks for each case and the exit block
  llvm::BasicBlock *ExitBB = llvm::BasicBlock::Create(*TheContext, "switch_expr_end");
  
  // Create a PHI node to collect the result values
  // We'll determine the result type from the first case
  llvm::Type *ResultType = nullptr;
  std::vector<std::pair<llvm::BasicBlock*, const CaseAST*>> CaseBlocks;
  
  // First pass: create basic blocks and determine result type
  for (const auto &Case : getCases()) {
    llvm::BasicBlock *CaseBB = llvm::BasicBlock::Create(*TheContext, 
        Case->isDefault() ? "expr_default" : "expr_case", TheFunction);
    CaseBlocks.push_back({CaseBB, Case.get()});
  }
  
  // Create switch instruction
  llvm::BasicBlock *DefaultBB = nullptr;
  for (size_t i = 0; i < getCases().size(); ++i) {
    if (getCases()[i]->isDefault()) {
      DefaultBB = CaseBlocks[i].first;
      break;
    }
  }
  
  if (!DefaultBB) {
    return LogErrorV("Switch expressions must have a default case");
  }
  
  llvm::SwitchInst *Switch = Builder->CreateSwitch(CondV, DefaultBB, getCases().size());
  
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
  
  // Generate code for each case expression and collect results
  std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> PhiValues;
  
  for (size_t i = 0; i < CaseBlocks.size(); ++i) {
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    const CaseAST *Case = CaseBlocks[i].second;
    
    Builder->SetInsertPoint(CaseBB);
    
    // Generate case expression
    llvm::Value *CaseResult = Case->getExpression()->codegen();
    if (!CaseResult)
      return nullptr;
    
    // Determine result type from first case
    if (ResultType == nullptr) {
      ResultType = CaseResult->getType();
      
      // Set the expression's type name based on result type
      if (ResultType->isIntegerTy(32)) {
        setTypeName("int");
      } else if (ResultType->isDoubleTy()) {
        setTypeName("double");
      } else if (ResultType->isFloatTy()) {
        setTypeName("float");
      } else if (ResultType->isIntegerTy(8)) {
        setTypeName("bool");
      } else if (ResultType->isPointerTy()) {
        setTypeName("string");
      }
    }
    
    // Type-cast case result to match result type if needed
    if (CaseResult->getType() != ResultType) {
      // Simple type promotions for switch expressions
      if (ResultType->isDoubleTy() && CaseResult->getType()->isIntegerTy(32)) {
        CaseResult = Builder->CreateSIToFP(CaseResult, ResultType, "int_to_double");
      } else if (ResultType->isFloatTy() && CaseResult->getType()->isIntegerTy(32)) {
        CaseResult = Builder->CreateSIToFP(CaseResult, ResultType, "int_to_float");
      } else if (ResultType != CaseResult->getType()) {
        return LogErrorV("All switch expression cases must return the same type");
      }
    }

    llvm::BasicBlock *completedCaseBB = Builder->GetInsertBlock();
    PhiValues.push_back({CaseResult, completedCaseBB});

    if (!completedCaseBB->getTerminator())
      Builder->CreateBr(ExitBB);
  }
  
  // Add exit block and create PHI node
  ExitBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ExitBB);
  
  llvm::PHINode *Result = Builder->CreatePHI(ResultType, PhiValues.size(), "switch_result");
  for (const auto &PhiVal : PhiValues) {
    Result->addIncoming(PhiVal.first, PhiVal.second);
  }
  
  return Result;
}

// Generate code for ternary expressions (a if b else c)
llvm::Value *TernaryExprAST::codegen() {
  // Evaluate the condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  ExprAST *ThenExprNode = getThenExpr();
  ExprAST *ElseExprNode = getElseExpr();

  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV,
      llvm::ConstantInt::get(CondV->getType(), 0), "ternarycond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV,
        llvm::ConstantFP::get(CondV->getType(), 0.0), "ternarycond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV,
        llvm::ConstantInt::get(CondV->getType(), 0), "ternarycond");
    } else {
      return LogErrorV("Ternary condition must be a numeric type");
    }
  }

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else branches, and merge block
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "ternary_then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "ternary_else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ternary_merge");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then expression
  Builder->SetInsertPoint(ThenBB);
  llvm::Value *ThenV = getThenExpr()->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI
  ThenBB = Builder->GetInsertBlock();

  // Emit else expression
  ElseBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ElseBB);
  llvm::Value *ElseV = getElseExpr()->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block
  MergeBB->insertInto(TheFunction);
  Builder->SetInsertPoint(MergeBB);

  // Determine result type and handle type promotion
  llvm::Type *ResultType = ThenV->getType();
  std::string thenTypeName = ThenExprNode ? ThenExprNode->getTypeName() : "";
  std::string elseTypeName = ElseExprNode ? ElseExprNode->getTypeName() : "";

  // Type-cast expressions to match if needed
  if (ThenV->getType() != ElseV->getType()) {
    // Handle type promotion for numeric types
    if (ThenV->getType()->isDoubleTy() && ElseV->getType()->isIntegerTy(32)) {
      ElseV = Builder->CreateSIToFP(ElseV, ThenV->getType(), "int_to_double");
      ResultType = ThenV->getType();
    } else if (ElseV->getType()->isDoubleTy() && ThenV->getType()->isIntegerTy(32)) {
      ThenV = Builder->CreateSIToFP(ThenV, ElseV->getType(), "int_to_double");
      ResultType = ElseV->getType();
    } else if (ThenV->getType()->isFloatTy() && ElseV->getType()->isIntegerTy(32)) {
      ElseV = Builder->CreateSIToFP(ElseV, ThenV->getType(), "int_to_float");
      ResultType = ThenV->getType();
    } else if (ElseV->getType()->isFloatTy() && ThenV->getType()->isIntegerTy(32)) {
      ThenV = Builder->CreateSIToFP(ThenV, ElseV->getType(), "int_to_float");
      ResultType = ElseV->getType();
    } else if (ResultType->isStructTy() && ElseV->getType()->isPointerTy() &&
               llvm::isa<llvm::ConstantPointerNull>(ElseV)) {
      ElseV = llvm::ConstantAggregateZero::get(llvm::cast<llvm::StructType>(ResultType));
    } else if (ElseV->getType()->isStructTy() && ThenV->getType()->isPointerTy() &&
               llvm::isa<llvm::ConstantPointerNull>(ThenV)) {
      llvm::StructType *StructTy = llvm::cast<llvm::StructType>(ElseV->getType());
      ThenV = llvm::ConstantAggregateZero::get(StructTy);
      ResultType = ElseV->getType();
    } else {
      return LogErrorV("Ternary expression branches must have compatible types");
    }
  }

  auto pickMeaningfulTypeName = [](const std::string &name) -> std::string {
    if (name.empty() || name == "null" || name == "void")
      return {};
    return name;
  };

  std::string resultTypeName = pickMeaningfulTypeName(thenTypeName);
  if (resultTypeName.empty())
    resultTypeName = pickMeaningfulTypeName(elseTypeName);
  if (resultTypeName.empty())
    resultTypeName = !thenTypeName.empty() ? thenTypeName : elseTypeName;

  const ExprAST *ThenExprForNullability = unwrapRefExpr(ThenExprNode);
  const ExprAST *ElseExprForNullability = unwrapRefExpr(ElseExprNode);
  bool thenNullable = expressionIsNullable(ThenExprForNullability);
  bool elseNullable = expressionIsNullable(ElseExprForNullability);

  if (resultTypeName.empty()) {
    if (ResultType->isIntegerTy(32))
      resultTypeName = "int";
    else if (ResultType->isDoubleTy())
      resultTypeName = "double";
    else if (ResultType->isFloatTy())
      resultTypeName = "float";
    else if (ResultType->isIntegerTy(8))
      resultTypeName = "bool";
  }

  if (!resultTypeName.empty() && (thenNullable || elseNullable))
    resultTypeName = ensureOuterNullable(resultTypeName);

  if (!resultTypeName.empty())
    setTypeName(resultTypeName);

  // Create PHI node to merge the results
  llvm::PHINode *Result = Builder->CreatePHI(ResultType, 2, "ternary_result");
  Result->addIncoming(ThenV, ThenBB);
  Result->addIncoming(ElseV, ElseBB);

  return Result;
}

#undef LoopContinueBlocks
#undef LoopExitBlocks
#undef StructFieldTypes
#undef StructFieldIndices
#undef ArraySizes
#undef StructTypes
#undef LocalTypes
#undef GlobalTypes
#undef GlobalValues
#undef NamedValues
#undef Builder
#undef TheModule
#undef TheContext
#undef CG
