#pragma once

#include "ast.h"

#include "analysis/semantics.h"
#include "compiler_session.h"
#include "codegen_context.h"
#include "parser.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <algorithm>
#include <cctype>
#include <cerrno>
#include <climits>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iomanip>
#include <cstdint>
#include <limits>
#include <map>
#include <optional>
#include <ranges>
#include <set>
#include <sstream>
#include <string_view>

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

struct ProvidedArgument;
struct GenericDefinitionInfo {
  std::string_view typeName;
  const std::vector<std::string> *parameters = nullptr;
};
extern thread_local std::vector<const GenericDefinitionInfo *>
    ActiveGenericDefinitions;

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

struct GenericInstantiationContext {
  std::string nameOverride;
};
const GenericInstantiationContext *currentInstantiationContext();

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

struct MethodRequirement {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};

struct BaseMethodMatch {
  std::string ownerType;
  const CompositeMemberInfo *info = nullptr;
};

struct GenericFunctionTemplate {
  std::unique_ptr<FunctionAST> function;
};

struct ArrayElementAccessInfo {
  llvm::Value *elementPtr = nullptr;
  llvm::Type *elementLLVMType = nullptr;
  std::string elementTypeName;
  bool elementNullable = false;
  TypeInfo elementTypeInfo;
};

struct ArrayBoundsInfo {
  std::vector<llvm::Value *> dims32;
  llvm::Value *totalSize = nullptr;
  llvm::Value *totalSize32 = nullptr;
  std::vector<int64_t> constantDims;
};

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

enum class NullComparisonRelation { EqualsNull, NotEqualsNull };
enum class TypeCheckMatchKind : uint8_t { Exact, Chain, Interface };

struct NullComparison {
  std::string variableName;
  NullComparisonRelation relation = NullComparisonRelation::EqualsNull;
};

struct IntegerRangeInfo {
  std::string typeName;
  bool isSigned = false;
  long long minSigned = 0;
  long long maxSigned = 0;
  unsigned long long maxUnsigned = 0;
  std::string minText;
  std::string maxText;
};

enum class BranchKind { Then, Else };
enum class AccessIntent : uint8_t { Read, Write, Call };

extern thread_local std::string ActiveBinaryOp;
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

extern llvm::Function *TopLevelExecFunction;
extern llvm::Function *ScriptMainFunction;
extern bool ScriptMainIsSynthetic;
extern llvm::BasicBlock *TopLevelInsertBlock;

const analysis::VariableLifetimePlan *
lookupLifetimePlanEntry(const std::string &name);

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

void pushArcScope();
void popArcScope(bool emitReleases, std::string_view label);
void markArcSlotDestroyed(llvm::Value *storage);
void registerArcLocal(const std::string &name, llvm::Value *storage,
                      const TypeInfo &info, bool isTemporary);

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

void pushGenericParameterScope(const std::vector<std::string> &params);
void popGenericParameterScope();

class SemanticGenericParameterScope {
public:
  explicit SemanticGenericParameterScope(const std::vector<std::string> &params)
      : active(!params.empty()) {
    if (active)
      pushGenericParameterScope(params);
  }

  SemanticGenericParameterScope(const SemanticGenericParameterScope &) = delete;
  SemanticGenericParameterScope &
  operator=(const SemanticGenericParameterScope &) = delete;

  ~SemanticGenericParameterScope() {
    if (active)
      popGenericParameterScope();
  }

private:
  bool active = false;
};

const std::map<std::string, TypeInfo> *currentTypeBindings();
TypeInfo applyActiveTypeBindings(const TypeInfo &info);
TypeInfo makeTypeInfo(std::string typeName,
                      RefStorageClass storage = RefStorageClass::None,
                      bool isMutable = true, bool declaredRef = false);
llvm::PointerType *pointerType(unsigned addressSpace = 0);
llvm::PointerType *pointerType(llvm::Type *elementType,
                               unsigned addressSpace = 0);
llvm::Type *getSizeType();
llvm::PointerType *getDeallocFunctionPointerType();

bool resolveParameterDefaults(std::vector<Parameter> &params,
                              const std::string &functionName);
const std::vector<GenericFunctionTemplate> *
lookupGenericFunctionTemplates(const std::string &name);
std::optional<std::string> buildDescribeTypeSummary(
    const std::string &typeSpelling);
std::vector<std::size_t>
collectGenericArities(const std::vector<GenericFunctionTemplate> &templates);
std::string formatArityList(const std::vector<std::size_t> &arities);
void noteFunctionCacheHit();
void noteFunctionCacheMiss();
void noteTypeCacheHit();
void noteTypeCacheMiss();
bool recordGenericInstantiation(bool isFunction);
bool splitGenericArgumentList(const std::string &segment,
                              std::vector<std::string> &out);
bool isActiveGenericParameter(const std::string &name);
void rebuildGenericBindingKey(TypeInfo &info);
bool maybeReportNestedDepthIssues(const TypeInfo &info,
                                  const std::string &contextDescription);
bool pushGenericTypeBindingScope(const std::map<std::string, TypeInfo> &bindings,
                                 std::string frameLabel = {});
void popGenericTypeBindingScope();
void pushInstantiationContext(std::string nameOverride);
void popInstantiationContext();
std::string buildGenericFrameLabel(const std::string &name,
                                   const std::vector<TypeInfo> &args);
std::vector<TypeInfo> gatherParamTypes(const std::vector<Parameter> &params);
std::vector<bool> gatherParamRefFlags(const std::vector<Parameter> &params);
std::vector<bool> gatherParamParamsFlags(const std::vector<Parameter> &params);
std::string makeMethodSignatureKey(const std::string &methodName,
                                   const std::vector<TypeInfo> &paramTypes,
                                   const std::vector<bool> &paramIsRef,
                                   bool skipFirstParam);
std::string makeMethodSignatureKey(const std::string &methodName,
                                   const PrototypeAST &proto,
                                   bool skipFirstParam);
void gatherInterfaceRequirements(
    const CompositeTypeInfo &metadata,
    std::map<std::string, MethodRequirement> &requirements);
void removeInterfaceRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
void collectAbstractBaseMethods(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
void removeAbstractRequirementsSatisfiedByHierarchy(
    const std::string &typeName,
    std::map<std::string, MethodRequirement> &requirements);
std::optional<BaseMethodMatch>
findBaseMethodMatch(const std::string &typeName,
                    const std::string &methodName,
                    const PrototypeAST &proto);
FunctionOverload *findRegisteredOverload(
    const std::string &name, const TypeInfo &returnType, bool returnsByRef,
    const std::vector<TypeInfo> &paramTypes,
    const std::vector<bool> &paramIsRef);
bool verifyOverrideDefaultCompatibility(
    const CompositeMemberInfo &baseMember, const PrototypeAST &overrideProto,
    const std::string &baseOwner, const std::string &methodName,
    const std::string &derivedOwner);
std::unique_ptr<ExprAST> instantiateDefaultExpr(const DefaultArgInfo &info);
bool typeInfoEquals(const TypeInfo &lhs, const TypeInfo &rhs);
void collectInterfaceAncestors(const std::string &interfaceName,
                               std::set<std::string> &out);
DelegateTypeInfo *lookupDelegateInfoMutable(const std::string &name);
std::string formatDelegateSignature(const DelegateTypeInfo &info,
                                    bool includeName);
llvm::FunctionType *ensureDelegateFunctionType(DelegateTypeInfo &info);

bool validateTypeForGenerics(
    const TypeInfo &info, const std::string &contextDescription,
    const GenericDefinitionInfo *currentDefinition = nullptr);
bool ensureNoDuplicateGenericParameters(
    const std::vector<std::string> &params,
    const std::string &contextDescription);
llvm::StructType *
getOrCreateSharedControlBlockType(const std::string &constructedName,
                                  llvm::Type *payloadTy);

llvm::StructType *ensureDelegateStructType(DelegateTypeInfo &info);
const DelegateTypeInfo *lookupDelegateInfo(const std::string &name);
const DelegateTypeInfo *lookupDelegateInfo(const TypeInfo &info);
const DelegateTypeInfo *lookupDelegateInfo(llvm::Type *type);
const CompositeTypeInfo *lookupCompositeInfo(const std::string &name,
                                             bool countHit = true);
const CompositeMemberInfo *lookupOperatorMember(const CompositeTypeInfo &info,
                                                OverloadableOperator op);
const CompositeTypeInfo *
materializeCompositeInstantiation(const TypeInfo &requestedType);
const CompositeTypeInfo *resolveSmartPointerMetadata(const TypeInfo &info);
bool typeInfoIsConcrete(const TypeInfo &info);

FunctionOverload *registerFunctionOverload(const PrototypeAST &proto,
                                           const std::string &mangledName);
FunctionOverload *lookupFunctionOverload(const PrototypeAST &proto);

const ActiveCompositeContext *currentCompositeContext();
const TypeInfo *lookupLocalTypeInfo(const std::string &name);
const TypeInfo *lookupGlobalTypeInfo(const std::string &name);
const TypeInfo *lookupTypeInfo(const std::string &name);
void rememberLocalType(const std::string &name, TypeInfo info);
void ensureBaseNonNullScope();
void markKnownNonNull(const std::string &name);
bool isKnownNonNull(const std::string &name);
bool typeAllowsNull(const TypeInfo &info);

bool typeNeedsLifetimeTracking(const TypeInfo &info);
llvm::Value *emitManagedStore(llvm::Value *storagePtr, llvm::Value *incoming,
                              const TypeInfo &info, std::string_view label,
                              bool incomingIsTemporary = false);
llvm::Value *materializeAliasPointer(llvm::Value *storage,
                                     const TypeInfo &info,
                                     const std::string &name);
std::string buildArcOpLabel(std::string_view label, std::string_view suffix);
bool emitSmartPointerInitFromVariable(const TypeInfo &declaredInfo,
                                      llvm::Value *destPtr,
                                      VariableExprAST &sourceVar,
                                      std::string_view label);
llvm::Value *computeArrayHeaderPointer(llvm::Value *arrayValue,
                                       std::string_view label);
llvm::Value *emitArrayRetainValue(llvm::Value *arrayValue,
                                  std::string_view label);
void emitArrayReleaseValue(llvm::Value *arrayValue, std::string_view label);

const ExprAST *unwrapRefExpr(const ExprAST *expr);
bool expressionIsNullable(const ExprAST *expr);
std::string ensureOuterNullable(const std::string &typeName);
std::string baseCompositeName(const std::string &typeName);
std::string resolveCompositeName(const ExprAST *expr);
std::optional<TypeInfo> resolveExprTypeInfo(const ExprAST *expr);
std::vector<TypeInfo>
applyActiveTypeBindingsToInfos(const std::vector<TypeInfo> &infos);
std::optional<TypeInfo>
applyActiveTypeBindingsToOptionalInfo(const std::optional<TypeInfo> &info);
ParsedTypeDescriptor parseTypeString(const std::string &typeName);
bool isArrayTypeName(const std::string &typeName);
unsigned computePointerDepth(const std::string &typeName);
std::string sanitizeBaseTypeName(std::string_view typeName);
bool isDecimalTypeName(std::string_view typeName);
bool isSignedType(std::string_view TypeStr);
bool isUnsignedType(std::string_view TypeStr);
std::optional<bool> unsignedHintFromTypeName(const std::string &typeName);
std::string describeTypeForDiagnostic(llvm::Type *type);
std::string buildIntegerRangeError(const llvm::APInt &value,
                                   std::string_view targetTypeName);
std::optional<size_t> findMatchingAngleInTypeName(const std::string &text,
                                                  size_t openPos);
std::vector<TypeInfo>
buildGenericArgumentTypeInfos(const std::string &segment);
std::string removeLastArrayGroup(const std::string &typeName);
unsigned getLastArrayGroupRank(const std::string &typeName);
std::string sanitizeCompositeLookupName(const std::string &typeName);
std::optional<TypeInfo> resolveTupleTypeInfo(const ExprAST *expr);
std::optional<TypeInfo> extractElementTypeInfo(const TypeInfo &arrayInfo);
std::vector<int64_t> parseExplicitArrayDimensions(const TypeInfo &info);
bool validateArrayNewBoundsForDeclaration(const TypeInfo &declaredInfo,
                                          const ExprAST *initializer,
                                          const std::string &name);
bool hasParameterlessConstructor(const std::string &typeName);
bool emitConstructorInitializers(
    const std::string &typeKey, llvm::StructType *structTy,
    llvm::AllocaInst *structPtr, const CompositeTypeInfo &metadata,
    const std::vector<ConstructorInitializer> &initializers);
bool emitCompositeDealloc(const std::string &typeKey,
                          llvm::StructType *structTy,
                          CompositeTypeInfo &metadata);
bool shouldBypassPropertyAccess(const std::string &ownerName,
                                const std::string &memberName);
const InstanceFieldInfo *findInstanceField(const CompositeTypeInfo &metadata,
                                           std::string_view fieldName);
bool validateCompositeHierarchy(const std::string &name,
                                CompositeTypeInfo &metadata);
std::string describeAggregateKind(AggregateKind kind);
void markStaticFieldInitialized(const std::string &owner,
                                const std::string &member);
void noteMemberAssignment(const std::string &ownerName,
                          const std::string &memberName,
                          bool isStatic);
llvm::Constant *constantValueToLLVM(const ConstantValue &value,
                                    llvm::Type *targetType,
                                    const std::string &typeName);
std::vector<std::string>
buildInheritanceChain(const std::optional<std::string> &baseClassName);
void emitArcScopeDrainAll(std::string_view label);
std::optional<ArrayElementAccessInfo>
computeTupleElementAccess(const TypeInfo &tupleInfo, llvm::Value *tupleValue,
                          size_t elementIndex, std::string_view label);
std::optional<ArrayElementAccessInfo>
computeArrayElementAccess(ArrayIndexExprAST *node,
                          llvm::Value *arrayValue = nullptr);
bool ensureMemberReadable(const MemberModifiers &modifiers,
                          const std::string &ownerName,
                          const std::string &memberName);
bool ensureMemberAccessAllowed(const MemberModifiers &modifiers,
                               AccessIntent intent,
                               const std::string &ownerName,
                               const std::string &memberName);
std::optional<std::string>
resolveStaticFieldOwnerInCurrentContext(const std::string &memberName);
std::optional<std::string>
resolveInstanceMemberOwnerInCurrentContext(const std::string &memberName);
llvm::Value *emitMemberCallByInfo(const CompositeTypeInfo &info,
                                  const CompositeMemberInfo &memberInfo,
                                  const std::string &ownerName,
                                  ExprAST *objectExpr,
                                  llvm::Value *instanceValue,
                                  std::vector<llvm::Value *> argValues,
                                  std::vector<bool> argIsRef,
                                  ExprAST *typeOwner,
                                  bool preserveRefReturn = false);
bool isDelegateFunctionReference(const VariableExprAST &var);
bool isDelegateMethodReference(const MemberAccessExprAST &member);
bool canBindDelegateReference(const DelegateTypeInfo &delegateInfo,
                              const ExprAST *expr);
const DelegateTypeInfo *resolveDelegateInfoForExpr(const ExprAST *expr);
llvm::Value *emitDelegateCall(CallExprAST &call, llvm::Value *calleeValue,
                              const DelegateTypeInfo &delegateInfo,
                              ExprAST *calleeExpr);
llvm::Value *emitBaseConstructorInitialization(
    const std::string &typeKey, llvm::StructType *structTy,
    llvm::AllocaInst *structPtr, const CompositeTypeInfo &metadata,
    const std::vector<std::unique_ptr<ExprAST>> &args,
    std::optional<std::string> explicitTarget = std::nullopt);
bool parseExplicitTypeArgumentSuffix(const std::string &text,
                                     std::string &baseName,
                                     std::vector<TypeInfo> &typeArguments);
llvm::Value *emitThisOverrideString(const CompositeTypeInfo &info,
                                    const std::string &baseName,
                                    llvm::Value *instanceVal);
llvm::Value *emitResolvedCallInternal(
    const std::string &calleeBase, std::vector<llvm::Value *> ArgValues,
    const std::vector<bool> &ArgIsRef,
    const std::vector<std::unique_ptr<ExprAST>> *argExprs = nullptr,
    bool preferGeneric = false, FunctionOverload *forced = nullptr,
    ExprAST *typeOwner = nullptr,
    std::vector<ProvidedArgument> *providedArgs = nullptr,
    bool preserveRefReturn = false);

bool builderInTopLevelContext();
void prepareTopLevelStatementContext();

std::string sanitizeForMangle(const std::string &input);
std::string makeRuntimeSymbolName(const std::string &prefix,
                                  const std::string &suffix);
std::string stripNullableAnnotations(const std::string &typeName);

llvm::StructType *getTypeDescriptorType();
llvm::StructType *getArcHeaderType();
llvm::StructType *getArrayHeaderType();
llvm::StructType *getStringStorageType();
llvm::StructType *getDecimalStorageType();
bool isDecimalLLVMType(llvm::Type *type);
bool emitClassRuntimeStructures(const std::string &typeName,
                                llvm::StructType *structTy,
                                CompositeTypeInfo &metadata);
bool computeVirtualDispatchLayout(const std::string &typeName,
                                  CompositeTypeInfo &metadata);
bool emitInterfaceDescriptor(const std::string &typeName,
                             CompositeTypeInfo &metadata);
llvm::Function *getInterfaceLookupFunction();

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
llvm::FunctionCallee getHybridArcDebugConfigFunction();
llvm::FunctionCallee getHybridArcTraceLabelFunction();
llvm::FunctionCallee getHybridArcVerifyRuntimeFunction();
llvm::FunctionCallee getSharedControlCreateFunction();
llvm::FunctionCallee getSharedControlRetainStrongFunction();
llvm::FunctionCallee getSharedControlReleaseStrongFunction();
llvm::FunctionCallee getSharedControlReleaseWeakFunction();
llvm::FunctionCallee getSharedControlRetainWeakFunction();
llvm::FunctionCallee getSharedControlLockFunction();
llvm::FunctionCallee getSharedControlUseCountFunction();
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

llvm::Type *getTypeFromString(const std::string &TypeStr);
llvm::StructType *getArrayStructType(llvm::Type *ElementType, unsigned rank);
bool areTypesCompatible(llvm::Type *type1, llvm::Type *type2);
llvm::Value *castToType(llvm::Value *value, llvm::Type *targetType);
llvm::Value *castToType(llvm::Value *value, llvm::Type *targetType,
                        const std::string &targetTypeName);
llvm::Value *LogErrorV(const char *Str, std::string_view hint = {});
llvm::Value *LogErrorV(const std::string &Str, std::string_view hint = {});
llvm::Function *LogErrorF(const char *Str, std::string_view hint = {});

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
llvm::Value *emitTargetTypedConstruction(const TypeInfo &targetInfo,
                                         ParenExprAST &paren);
llvm::Constant *buildDecimalConstantFromLongDouble(long double value);
llvm::Constant *buildDecimalConstantFromSpelling(const std::string &spelling);
llvm::Value *emitStringLiteral(const std::string &value);
llvm::FunctionCallee getConcatStringsFunction();
llvm::FunctionCallee getStringEqualsFunction();
llvm::FunctionCallee getCharToStringFunction();
llvm::Value *ensureStringPointer(llvm::Value *value);
llvm::Value *emitIntegerToDecimalValue(llvm::Value *value,
                                       std::string_view sourceTypeName);
llvm::Value *emitDecimalToFloatingValue(llvm::Value *value,
                                        llvm::Type *targetType);
llvm::Value *emitDecimalToIntegerValue(llvm::Value *value,
                                       llvm::Type *targetType,
                                       std::string_view targetTypeName);
llvm::Value *emitFloatingToDecimalValue(llvm::Value *value);
llvm::Value *emitDecimalArithmeticBinary(char op, llvm::Value *lhs,
                                         llvm::Value *rhs);
llvm::Value *emitDecimalComparisonI1(std::string_view op, llvm::Value *lhs,
                                     llvm::Value *rhs);
llvm::Value *emitDelegateValueForTarget(
    const DelegateTypeInfo &delegateInfo, ExprAST *expr,
    std::string_view contextDescription, bool &handled);
bool materializeTupleInstantiation(const TypeInfo &requestedType);
SmartPointerKind detectSmartPointerKind(const std::string &baseName);
std::optional<size_t> findTupleElementIndex(const TypeInfo &tupleInfo,
                                            std::string_view name);
uint64_t getTypeSizeInBytes(llvm::Type *type);
std::optional<ArrayBoundsInfo>
emitArrayBoundsInfo(const std::vector<std::unique_ptr<ExprAST>> &bounds,
                    std::string_view label);
bool validateArrayLiteralAssignmentSize(const std::string &name,
                                        const TypeInfo &arrayInfo,
                                        const ArrayExprAST *literal);
bool emitArrayResizeAssignment(llvm::Value *storagePtr,
                               const TypeInfo &arrayInfo, NewExprAST &newExpr,
                               std::string_view label,
                               std::vector<int64_t> *constantDimsOut);
bool convertUTF8LiteralToUTF16(const std::string &input,
                               std::vector<uint16_t> &output,
                               std::string &errorMessage);
llvm::Value *emitArrayFillValue(const TypeInfo &arrayInfo,
                                const TypeInfo &elementInfo,
                                llvm::Type *elementType,
                                llvm::Value *elementValue,
                                const std::vector<int64_t> &dimensions,
                                std::string_view label);
bool validateInvariantAssignment(const TypeInfo &targetInfo,
                                 const ExprAST *sourceExpr,
                                 const std::string &contextDescription);
bool validateTupleAssignmentCompatibility(
    const TypeInfo &targetInfo, const ExprAST *sourceExpr,
    const std::string &contextDescription);
void propagateTypeToNewExpr(ExprAST *expr, const TypeInfo &targetInfo);
std::unique_ptr<ParenExprAST> convertHashShorthandToParen(
    UnaryExprAST &hashExpr);
bool validateNullableAssignment(const TypeInfo &targetInfo, const ExprAST *expr,
                                const std::string &targetDescription);
void rememberGlobalType(const std::string &name, TypeInfo info);
TypeInfo runtimeTypeFrom(const TypeInfo &declared, RefStorageClass storage,
                         bool declaredRefOverride);
bool isDeclaredRefGlobal(const std::string &name);
bool isDeclaredRefLocal(const std::string &name);
std::set<std::string> currentNonNullFactsCopy();
void replaceCurrentNonNullFacts(const std::set<std::string> &facts);
void pushNonNullFactsFrom(const std::set<std::string> &seed);
void popNonNullFactsScope();
std::optional<NullComparison> extractNullComparison(const ExprAST *expr,
                                                    bool inverted = false);
void applyNullComparisonToCurrentScope(const NullComparison &comparison,
                                       BranchKind branch);
std::set<std::string> intersectNonNullFacts(const std::set<std::string> &a,
                                            const std::set<std::string> &b);
void updateKnownNonNullOnAssignment(const std::string &name,
                                    bool rhsIsNullable);
bool ensureMemberInitializedForMutation(MemberAccessExprAST &member);
std::optional<MemberFieldAssignmentInfo>
collectMemberFieldAssignmentInfo(MemberAccessExprAST &member);
bool isPointerTypeDescriptor(const ParsedTypeDescriptor &desc);
std::optional<std::string>
getPointerElementTypeName(const std::string &pointerTypeName);
llvm::IntegerType *getPointerIndexType();
NewExprAST *extractNewArrayExpr(ExprAST *expr);
const ArrayExprAST *extractArrayLiteralExpr(const ExprAST *expr);
llvm::Value *emitPointerOffset(llvm::Value *ptrValue,
                               llvm::Value *offsetValue,
                               const std::string &pointerTypeName,
                               const std::string &offsetTypeName,
                               bool negateOffset, const char *name);
const CompositeTypeInfo *resolveCompositeTypeInfo(const TypeInfo &info);
llvm::Value *emitNullCheckValue(llvm::Value *value, const TypeInfo &valueInfo,
                                std::string_view label);
llvm::Value *emitTypeDescriptorMatch(llvm::Value *objectPtr,
                                     llvm::Value *targetDescriptor,
                                     TypeCheckMatchKind kind,
                                     std::string_view label);
std::optional<IntegerRangeInfo> getIntegerRangeInfo(std::string_view typeName);
bool integerLiteralFitsRange(const llvm::APInt &value,
                             const IntegerRangeInfo &info);
bool emitTypeCheckBinding(const TypeInfo &targetInfo, const std::string &name,
                          llvm::Value *value);
bool diagnoseDisallowedImplicitIntegerConversion(
    const ExprAST *sourceExpr, llvm::Value *sourceValue,
    llvm::Type *targetType, std::string_view targetTypeName,
    std::string_view contextDescription);
llvm::Value *emitPackedParamsArray(const std::vector<int> &paramIndices,
                                   const std::vector<ProvidedArgument> &provided,
                                   const TypeInfo &arrayInfo,
                                   std::string_view label);
llvm::FunctionCallee getHybridArrayResizeFunction();
llvm::Value *selectArrayElementReleaseFunction(const TypeInfo &elementInfo,
                                               std::string_view label);
llvm::Value *selectArrayElementRetainFunction(const TypeInfo &elementInfo,
                                              std::string_view label);
llvm::Value *emitArcRetain(llvm::Value *value, const TypeInfo &info,
                           std::string_view label);
void emitArcRelease(llvm::Value *value, const TypeInfo &info,
                    std::string_view label);
