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
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <cmath>
#include <cerrno>
#include <climits>
#include <map>
#include <sstream>
#include <iomanip>
#include <cstdio>
#include <cstdlib>
#include <ranges>
#include <string_view>
#include <optional>
#include <algorithm>
#include <cctype>
#include <cstring>
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

struct ArrayElementAccessInfo {
  llvm::Value *elementPtr = nullptr;
  llvm::Type *elementLLVMType = nullptr;
  std::string elementTypeName;
  bool elementNullable = false;
  TypeInfo elementTypeInfo;
};

enum class NullComparisonRelation { EqualsNull, NotEqualsNull };

struct NullComparison {
  std::string variableName;
  NullComparisonRelation relation = NullComparisonRelation::EqualsNull;
};

enum class BranchKind { Then, Else };

llvm::Value *emitMemberCallByInfo(const CompositeTypeInfo &info,
                                  const CompositeMemberInfo &memberInfo,
                                  const std::string &ownerName,
                                  ExprAST *objectExpr,
                                  llvm::Value *instanceValue,
                                  std::vector<llvm::Value *> argValues,
                                  std::vector<bool> argIsRef,
                                  ExprAST *typeOwner);

std::unique_ptr<ExprAST> instantiateDefaultExpr(const DefaultArgInfo &info);
std::string buildArcOpLabel(std::string_view label,
                            std::string_view suffix);

bool convertUTF8LiteralToUTF16(const std::string &input,
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

thread_local std::string ActiveBinaryOp;

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

const analysis::VariableLifetimePlan *
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

void noteTypeCacheHit() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.typeCacheHits;
}

void noteTypeCacheMiss() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.typeCacheMisses;
}

void noteFunctionCacheHit() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.functionCacheHits;
}

void noteFunctionCacheMiss() {
  auto &metrics = CG.genericsMetrics;
  if (metrics.enabled)
    ++metrics.functionCacheMisses;
}

llvm::PointerType *pointerType(unsigned addressSpace = 0) {
  return llvm::PointerType::get(*TheContext, addressSpace);
}

llvm::PointerType *pointerType(llvm::Type * /*elementType*/,
                               unsigned addressSpace = 0) {
  return llvm::PointerType::get(*TheContext, addressSpace);
}

llvm::Type *getSizeType();
llvm::StructType *getTypeDescriptorType();
llvm::StructType *getArcHeaderType();
llvm::StructType *getArrayHeaderType();
llvm::StructType *getStringStorageType();
llvm::StructType *getDecimalStorageType();
bool isDecimalLLVMType(llvm::Type *type);
bool isDecimalTypeName(std::string_view typeName);
const TypeInfo *lookupLocalTypeInfo(const std::string &name);
const TypeInfo *lookupGlobalTypeInfo(const std::string &name);
const TypeInfo *lookupTypeInfo(const std::string &name);

llvm::PointerType *getDeallocFunctionPointerType() {
  auto *opaquePtrTy = pointerType();
  auto *deallocFnTy =
      llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext),
                              {opaquePtrTy}, false);
  return pointerType(deallocFnTy);
}

llvm::FunctionCallee getHybridRetainFunction();
llvm::FunctionCallee getHybridReleaseFunction();
llvm::FunctionCallee getHybridAutoreleaseFunction();
llvm::FunctionCallee getHybridDeallocFunction();
llvm::FunctionCallee getHybridAllocObjectFunction();
llvm::FunctionCallee getHybridAllocArrayFunction();
llvm::FunctionCallee getHybridArrayDescriptorFunction();
llvm::FunctionCallee getHybridArraySetReleaseFunction();
llvm::FunctionCallee getHybridArrayReleaseRefSlotFunction();
llvm::FunctionCallee getHybridArrayReleaseArraySlotFunction();
llvm::FunctionCallee getHybridArrayResizeFunction();
llvm::FunctionCallee getHybridArcDebugConfigFunction();
llvm::FunctionCallee getHybridArcTraceLabelFunction();
llvm::FunctionCallee getHybridArcVerifyRuntimeFunction();
[[maybe_unused]] static llvm::FunctionCallee getDecimalParseFunction();
llvm::FunctionCallee getDecimalToStringFunction();
llvm::FunctionCallee getDecimalAddFunction();
llvm::FunctionCallee getDecimalSubFunction();
llvm::FunctionCallee getDecimalMulFunction();
llvm::FunctionCallee getDecimalDivFunction();
llvm::FunctionCallee getDecimalRemFunction();
llvm::FunctionCallee getDecimalCmpFunction();
llvm::FunctionCallee getDecimalNegFunction();
llvm::FunctionCallee getDecimalFromI64Function();
llvm::FunctionCallee getDecimalFromU64Function();
llvm::FunctionCallee getDecimalToI64Function();
llvm::FunctionCallee getDecimalToU64Function();
llvm::FunctionCallee getDecimalFromDoubleFunction();
llvm::FunctionCallee getDecimalToDoubleFunction();
bool emitClassRuntimeStructures(const std::string &typeName,
                                llvm::StructType *structTy,
                                CompositeTypeInfo &metadata);
bool computeVirtualDispatchLayout(const std::string &typeName,
                                  CompositeTypeInfo &metadata);
bool emitInterfaceDescriptor(const std::string &typeName,
                             CompositeTypeInfo &metadata);
llvm::Function *getInterfaceLookupFunction();
bool typeInfoIsConcrete(const TypeInfo &info);
const CompositeTypeInfo *resolveSmartPointerMetadata(const TypeInfo &info);
bool emitSmartPointerInitFromVariable(const TypeInfo &declaredInfo,
                                      llvm::Value *destPtr,
                                      VariableExprAST &sourceVar,
                                      std::string_view label);
llvm::Value *computeArrayHeaderPointer(llvm::Value *arrayValue,
                                       std::string_view label);
llvm::Value *emitArrayRetainValue(llvm::Value *arrayValue,
                                  std::string_view label);
void emitArrayReleaseValue(llvm::Value *arrayValue, std::string_view label);
void pushArcScope();
void popArcScope(bool emitReleases, std::string_view label);
void markArcSlotDestroyed(llvm::Value *storage);
void emitArcScopeDrainAll(std::string_view label);
struct ArrayBoundsInfo {
  std::vector<llvm::Value *> dims32;
  llvm::Value *totalSize = nullptr;
  llvm::Value *totalSize32 = nullptr;
  std::vector<int64_t> constantDims;
};
std::optional<ArrayBoundsInfo>
emitArrayBoundsInfo(const std::vector<std::unique_ptr<ExprAST>> &bounds,
                    std::string_view label);
llvm::PointerType *getArrayReleaseCallbackPointerType();
std::uint64_t getArrayPayloadOffsetBytes();
void computeInterfaceMethodLayout(
    const std::string &typeName,
    const std::vector<const MethodDefinition *> &methods,
    CompositeTypeInfo &metadata);
llvm::Value *emitDynamicFunctionCall(CallExprAST &callExpr,
                                     const CompositeMemberInfo &memberInfo,
                                     llvm::Value *functionPointer,
                                     std::vector<llvm::Value *> argValues,
                                     const std::vector<bool> &argIsRef);
llvm::Value *selectArrayElementReleaseFunction(const TypeInfo &elementInfo,
                                               std::string_view label);
llvm::Value *selectArrayElementRetainFunction(const TypeInfo &elementInfo,
                                              std::string_view label);
llvm::Value *emitArcRetain(llvm::Value *value, const TypeInfo &info,
                           std::string_view label);
void emitArcRelease(llvm::Value *value, const TypeInfo &info,
                    std::string_view label);
llvm::Value *emitPackedParamsArray(const std::vector<int> &paramIndices,
                                   const std::vector<ProvidedArgument> &provided,
                                   const TypeInfo &arrayInfo,
                                   std::string_view label);
bool validateParamsParameterList(const std::vector<Parameter> &params,
                                 const std::string &functionName);
const ExprAST *unwrapRefExpr(const ExprAST *expr);
bool typeInfoEquals(const TypeInfo &lhs, const TypeInfo &rhs);
bool resolveParameterDefaults(std::vector<Parameter> &params,
                              const std::string &functionName);
std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params);
std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params);
std::vector<bool>
gatherParamParamsFlags(const std::vector<Parameter> &params);
FunctionOverload *registerFunctionOverload(const PrototypeAST &proto,
                                           const std::string &mangledName);
FunctionOverload *findRegisteredOverload(const std::string &name,
                                         const TypeInfo &returnType,
                                         bool returnsByRef,
                                         const std::vector<TypeInfo> &paramTypes,
                                         const std::vector<bool> &paramIsRef);
std::string makeMethodSignatureKey(const std::string &methodName,
                                   const std::vector<TypeInfo> &paramTypes,
                                   const std::vector<bool> &paramIsRef,
                                   bool skipFirstParam = false);
std::string makeMethodSignatureKey(const std::string &methodName,
                                   const PrototypeAST &proto,
                                   bool skipFirstParam = false);
struct MethodRequirement {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};
void gatherInterfaceRequirements(
    const CompositeTypeInfo &metadata,
    std::map<std::string, MethodRequirement> &requirements);
void collectAbstractBaseMethods(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
void removeInterfaceRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
void removeAbstractRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
struct BaseMethodMatch {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};
std::optional<BaseMethodMatch>
findBaseMethodMatch(const std::string &typeName,
                    const std::string &methodName,
                    const PrototypeAST &proto);
bool verifyOverrideDefaultCompatibility(
    const CompositeMemberInfo &baseMember, const PrototypeAST &overrideProto,
    const std::string &baseOwner, const std::string &methodName,
    const std::string &derivedOwner);
llvm::Value *emitManagedStore(llvm::Value *storagePtr,
                                     llvm::Value *incoming,
                                     const TypeInfo &info,
                                     std::string_view label,
                                     bool incomingIsTemporary = false);
llvm::FunctionCallee getSharedControlCreateFunction();
llvm::FunctionCallee getSharedControlRetainStrongFunction();
llvm::FunctionCallee getSharedControlReleaseStrongFunction();
llvm::FunctionCallee getSharedControlReleaseWeakFunction();
llvm::FunctionCallee getSharedControlRetainWeakFunction();
llvm::FunctionCallee getSharedControlLockFunction();
llvm::FunctionCallee getSharedControlUseCountFunction();
llvm::StructType *
getOrCreateSharedControlBlockType(const std::string &constructedName,
                                  llvm::Type *payloadTy);
TypeInfo applyActiveTypeBindings(const TypeInfo &info);
TypeInfo makeTypeInfo(std::string typeName,
                             RefStorageClass storage = RefStorageClass::None,
                             bool isMutable = true,
                             bool declaredRef = false);
std::string sanitizeCompositeLookupName(const std::string &typeName);
std::string sanitizeBaseTypeName(std::string_view typeName);

bool ensureNoDuplicateGenericParameters(const std::vector<std::string> &params,
                                               const std::string &contextDescription);

std::string stripNullableAnnotations(const std::string &typeName);
llvm::Value *emitTargetTypedConstruction(const TypeInfo &targetInfo,
                                         ParenExprAST &paren);
bool areTypesCompatible(llvm::Type *type1, llvm::Type *type2);
std::optional<TypeInfo> resolveTupleTypeInfo(const ExprAST *expr);
std::optional<size_t> findTupleElementIndex(const TypeInfo &tupleInfo,
                                            std::string_view name);
static bool typeHasDestructor(const TypeInfo &info);
bool typeNeedsLifetimeTracking(const TypeInfo &info);

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

bool recordGenericInstantiation(bool isFunction) {
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

std::string buildGenericFrameLabel(const std::string &name,
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

bool maybeReportNestedDepthIssues(const TypeInfo &info,
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


const CompositeTypeInfo *lookupCompositeInfo(const std::string &name,
                                                    bool countHit = true);
const CompositeTypeInfo *
materializeCompositeInstantiation(const TypeInfo &requestedType);

static bool isBuiltinValueTypeName(std::string_view baseName) {
  static constexpr std::string_view primitives[] = {
      "void",  "bool", "byte",  "sbyte", "short", "ushort",
      "int",   "uint", "long",  "ulong", "float", "double",
      "decimal",
      "char",  "char16", "char32", "string"};
  for (std::string_view candidate : primitives) {
    if (candidate == baseName)
      return true;
  }
  return false;
}

SmartPointerKind detectSmartPointerKind(const std::string &baseName) {
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

void rebuildGenericBindingKey(TypeInfo &info) {
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

  if (info.baseTypeName == "tuple") {
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

bool pushGenericTypeBindingScope(
    const std::map<std::string, TypeInfo> &bindings, std::string frameLabel) {
  CodegenContext &ctx = currentCodegen();
  GenericsDiagnostics &diag = ctx.genericsDiagnostics;
  if (diag.maxBindingDepth > 0 &&
      diag.currentBindingDepth >= diag.maxBindingDepth) {
    diag.depthLimitHit = true;
    reportCompilerError("Maximum generic binding depth (" +
                        std::to_string(diag.maxBindingDepth) + ") exceeded");
    if (diag.stackDumpEnabled)
      dumpGenericBindingStack(diag);
    return false;
  }

  ctx.genericTypeBindingsStack.push_back(bindings);
  ++diag.currentBindingDepth;
  if (diag.currentBindingDepth > diag.peakBindingDepth)
    diag.peakBindingDepth = diag.currentBindingDepth;
  if (!frameLabel.empty())
    diag.bindingStack.push_back(std::move(frameLabel));
  return true;
}

void popGenericTypeBindingScope() {
  CodegenContext &ctx = currentCodegen();
  if (ctx.genericTypeBindingsStack.empty())
    return;
  ctx.genericTypeBindingsStack.pop_back();

  GenericsDiagnostics &diag = ctx.genericsDiagnostics;
  if (!diag.bindingStack.empty())
    diag.bindingStack.pop_back();
  if (diag.currentBindingDepth > 0)
    --diag.currentBindingDepth;
}

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

void pushInstantiationContext(std::string nameOverride) {
  ActiveInstantiationContexts.push_back({std::move(nameOverride)});
}

void popInstantiationContext() {
  if (!ActiveInstantiationContexts.empty())
    ActiveInstantiationContexts.pop_back();
}

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

const std::vector<GenericFunctionTemplate> *
lookupGenericFunctionTemplates(const std::string &name) {
  auto &registry = genericFunctionTemplateRegistry();
  auto it = registry.find(name);
  if (it == registry.end())
    return nullptr;
  return &it->second;
}

std::vector<std::size_t>
collectGenericArities(const std::vector<GenericFunctionTemplate> &templates) {
  std::set<std::size_t> counts;
  for (const auto &entry : templates) {
    if (!entry.function)
      continue;
    counts.insert(entry.function->getProto()->getGenericParameters().size());
  }
  return std::vector<std::size_t>(counts.begin(), counts.end());
}

std::string formatArityList(const std::vector<std::size_t> &arities) {
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

const GenericInstantiationContext *currentInstantiationContext() {
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

void pushGenericParameterScope(const std::vector<std::string> &params) {
  if (params.empty())
    return;
  CodegenContext &ctx = currentCodegen();
  ctx.genericParameterStack.push_back(params);
  for (const auto &name : params)
    ++ctx.activeGenericParameters[name];
}

void popGenericParameterScope() {
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

bool isActiveGenericParameter(const std::string &name) {
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

thread_local std::vector<const GenericDefinitionInfo *> ActiveGenericDefinitions;

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

bool ensureNoDuplicateGenericParameters(const std::vector<std::string> &params,
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

std::optional<size_t> findMatchingAngleInTypeName(const std::string &text,
                                                  size_t openPos) {
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

bool splitGenericArgumentList(const std::string &segment,
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

std::vector<TypeInfo> buildGenericArgumentTypeInfos(const std::string &segment);

void ensureBaseNonNullScope();
std::vector<std::string>
buildInheritanceChain(const std::optional<std::string> &baseClassName);
static std::optional<std::string>
buildSmartPointerDescribeSummary(const TypeInfo &requested,
                                 const std::string &sanitized);

void collectInterfaceAncestors(const std::string &interfaceName,
                               std::set<std::string> &out);
llvm::Value *castToType(llvm::Value *value, llvm::Type *targetType,
                        const std::string &targetTypeName);
llvm::FunctionType *ensureDelegateFunctionType(DelegateTypeInfo &info);
llvm::StructType *ensureDelegateStructType(DelegateTypeInfo &info);
std::string describeAggregateKind(AggregateKind kind);
std::string baseCompositeName(const std::string &typeName);
bool expressionIsNullable(const ExprAST *expr);

llvm::Value *LogErrorV(const char *Str, std::string_view hint = {});
llvm::Value *LogErrorV(const std::string &Str, std::string_view hint = {});
llvm::Function *LogErrorF(const char *Str, std::string_view hint = {});
llvm::Type *getTypeFromString(const std::string &TypeStr);

llvm::Function *TopLevelExecFunction = nullptr;
llvm::Function *ScriptMainFunction = nullptr;
bool ScriptMainIsSynthetic = false;
llvm::BasicBlock *TopLevelInsertBlock = nullptr;

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

bool builderInTopLevelContext() {
  if (!Builder)
    return true;
  llvm::BasicBlock *insertBlock = Builder->GetInsertBlock();
  if (!insertBlock)
    return true;
  llvm::Function *parent = insertBlock->getParent();
  return !parent || parent->getName() == "__hybrid_top_level";
}

void prepareTopLevelStatementContext() {
  llvm::Function *TopFunc = ensureTopLevelExecFunction();
  ensureSyntheticMainCalls(TopFunc);

  // Nested control-flow inside an active top-level statement should preserve
  // current insertion and local bindings (e.g., loop variables).
  if (Builder && Builder->GetInsertBlock() &&
      Builder->GetInsertBlock()->getParent() == TopFunc &&
      TopLevelInsertBlock &&
      Builder->GetInsertBlock() != TopLevelInsertBlock) {
    return;
  }

  if (Builder && Builder->GetInsertBlock() &&
      Builder->GetInsertBlock()->getParent() == TopFunc &&
      (!TopLevelInsertBlock ||
       Builder->GetInsertBlock() == TopLevelInsertBlock)) {
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

unsigned computePointerDepth(const std::string &typeName) {
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

bool isArrayTypeName(const std::string &typeName) {
  auto openPos = typeName.find('[');
  auto closePos = typeName.find(']', openPos == std::string::npos ? 0 : openPos);
  return openPos != std::string::npos && closePos != std::string::npos && closePos > openPos;
}

std::string removeLastArrayGroup(const std::string &typeName) {
  size_t openPos = typeName.rfind('[');
  if (openPos == std::string::npos)
    return typeName;
  return typeName.substr(0, openPos);
}

unsigned getLastArrayGroupRank(const std::string &typeName) {
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

llvm::StructType *getArrayStructType(llvm::Type *ElementType, unsigned rank);

std::string stripNullableAnnotations(const std::string &typeName) {
  std::string cleaned;
  cleaned.reserve(typeName.size());
  for (char c : typeName) {
    if (c != '?')
      cleaned.push_back(c);
  }
  return cleaned;
}

std::string ensureOuterNullable(const std::string &typeName) {
  if (!typeName.empty() && typeName.back() == '?')
    return typeName;
  return typeName + "?";
}

std::string typeNameFromInfo(const TypeInfo &info) {
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

std::optional<TypeInfo>
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

std::vector<int64_t>
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

static std::string formatArrayDims(const std::vector<int64_t> &dims) {
  if (dims.empty())
    return "unknown";
  std::string formatted;
  for (size_t i = 0; i < dims.size(); ++i) {
    if (i != 0)
      formatted += "x";
    formatted += std::to_string(dims[i]);
  }
  return formatted;
}

static std::vector<int64_t>
computeArrayLiteralDimensions(const ArrayExprAST *arrayLiteral,
                              const TypeInfo &declaredInfo) {
  std::vector<int64_t> dims;
  if (!arrayLiteral || !declaredInfo.isArray)
    return dims;

  if (declaredInfo.isMultidimensional && declaredInfo.arrayDepth == 1 &&
      !declaredInfo.arrayRanks.empty()) {
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
}

static std::vector<int64_t>
resolveKnownArrayDimensions(const std::string &name,
                            const TypeInfo &arrayInfo) {
  auto dims = parseExplicitArrayDimensions(arrayInfo);
  if (!dims.empty())
    return dims;
  auto it = ArraySizes.find(name);
  if (it != ArraySizes.end())
    return it->second;
  return {};
}

bool validateArrayLiteralAssignmentSize(const std::string &name,
                                        const TypeInfo &arrayInfo,
                                        const ArrayExprAST *literal) {
  if (!literal)
    return true;
  auto knownDims = resolveKnownArrayDimensions(name, arrayInfo);
  auto literalDims = computeArrayLiteralDimensions(literal, arrayInfo);
  if (!knownDims.empty() && !literalDims.empty() &&
      literalDims != knownDims) {
    reportCompilerError(
        "Array assignment does not match declared size for '" + name + "'",
        "Declared dimensions: " + formatArrayDims(knownDims) +
            ", assigned dimensions: " + formatArrayDims(literalDims));
    return false;
  }
  return true;
}

bool validateArrayNewBoundsForDeclaration(const TypeInfo &declaredInfo,
                                          const ExprAST *initializer,
                                          const std::string &name) {
  if (!initializer || !declaredInfo.isArray)
    return true;

  const ExprAST *core = unwrapRefExpr(initializer);
  auto *newExpr = dynamic_cast<const NewExprAST *>(core);
  if (!newExpr || !newExpr->isArray())
    return true;

  const auto &bounds = newExpr->getArraySizes();
  if (bounds.empty())
    return true;

  unsigned rank =
      declaredInfo.arrayRanks.empty() ? 1 : std::max(1u, declaredInfo.arrayRanks.back());
  if (bounds.size() != rank) {
    reportCompilerError(
        "Array bounds do not match declared rank for '" +
            typeNameFromInfo(declaredInfo) + "'",
        "Expected " + std::to_string(rank) + " bound(s) in 'new'.");
    return false;
  }

  return true;
}

static bool typeHasDestructor(const TypeInfo &info) {
  std::string base = sanitizeCompositeLookupName(typeNameFromInfo(info));
  if (base.empty())
    return false;
  if (const CompositeTypeInfo *meta = lookupCompositeInfo(base))
    return meta->hasDestructor;
  return false;
}

bool typeNeedsLifetimeTracking(const TypeInfo &info) {
  if (info.isAlias())
    return false;
  if (info.requiresARC()) {
    if (info.isTupleType())
      return true;
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

DelegateTypeInfo *lookupDelegateInfoMutable(const std::string &name) {
  if (name.empty())
    return nullptr;
  std::string key = stripNullableAnnotations(name);
  auto it = CG.delegateTypes.find(key);
  if (it == CG.delegateTypes.end())
    return nullptr;
  return &it->second;
}

const DelegateTypeInfo *lookupDelegateInfo(const std::string &name) {
  return lookupDelegateInfoMutable(name);
}

const DelegateTypeInfo *lookupDelegateInfo(const TypeInfo &info) {
  std::string typeName = typeNameFromInfo(info);
  if (typeName.empty())
    typeName = info.typeName;
  return lookupDelegateInfo(typeName);
}

const DelegateTypeInfo *lookupDelegateInfo(llvm::Type *type) {
  if (!type)
    return nullptr;
  auto *structTy = llvm::dyn_cast<llvm::StructType>(type);
  if (!structTy)
    return nullptr;
  for (const auto &entry : CG.delegateTypes) {
    if (entry.second.structType == structTy)
      return &entry.second;
  }
  return nullptr;
}

std::string formatDelegateSignature(const DelegateTypeInfo &info,
                                    bool includeName) {
  std::string signature;
  if (includeName) {
    signature = "delegate " + info.name + " ";
  }
  if (info.returnsByRef)
    signature += "ref ";
  signature += stripNullableAnnotations(typeNameFromInfo(info.returnType));
  signature += "(";
  for (size_t idx = 0; idx < info.parameterTypes.size(); ++idx) {
    if (idx)
      signature += ", ";
    if (idx < info.parameterIsParams.size() && info.parameterIsParams[idx])
      signature += "params ";
    if (idx < info.parameterIsRef.size() && info.parameterIsRef[idx])
      signature += "ref ";
    signature += stripNullableAnnotations(
        typeNameFromInfo(info.parameterTypes[idx]));
  }
  signature += ")";
  return signature;
}

static std::string formatDelegateDescribeSummary(const DelegateTypeInfo &info,
                                                 const std::string &typeName) {
  std::string summary = "type:" + typeName;
  summary += "|kind:delegate";
  summary += "|signature:" + formatDelegateSignature(info, false);
  return summary;
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

std::optional<std::string>
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

  if (const DelegateTypeInfo *delegateInfo = lookupDelegateInfo(sanitized)) {
    return formatDelegateDescribeSummary(*delegateInfo, sanitized);
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

std::string sanitizeForMangle(const std::string &input) {
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

std::string makeRuntimeSymbolName(const std::string &prefix,
                                  const std::string &typeName) {
  return prefix + sanitizeForMangle(typeName);
}
