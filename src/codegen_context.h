#ifndef HYBRID_CODEGEN_CONTEXT_H
#define HYBRID_CODEGEN_CONTEXT_H

#include <cstdint>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "ast.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

struct FunctionOverload {
  std::string mangledName;
  TypeInfo returnType;
  bool returnsByRef = false;
  std::vector<TypeInfo> parameterTypes;
  std::vector<bool> parameterIsRef;
  bool isUnsafe = false;
  bool isExtern = false;
  llvm::Function *function = nullptr;
  bool isGenericInstantiation = false;
};

struct CompositeMemberInfo {
  MemberModifiers modifiers;
  std::string signature;
  std::string mangledName;
  std::string dispatchKey;
  std::string overridesSignature;
  TypeInfo returnType;
  std::vector<TypeInfo> parameterTypes;
  std::vector<bool> parameterIsRef;
  bool returnsByRef = false;
  unsigned vtableSlot = std::numeric_limits<unsigned>::max();
  bool isGenericTemplate = false;
  unsigned genericArity = 0;
  llvm::Function *directFunction = nullptr;
};

struct CompositeTypeInfo {
  AggregateKind kind = AggregateKind::Struct;
  std::map<std::string, std::string> fieldTypes;
  std::map<std::string, MemberModifiers> fieldModifiers;
  std::map<std::string, std::string> staticFieldTypes;
  std::map<std::string, MemberModifiers> staticFieldModifiers;
  std::map<std::string, std::string> staticFieldGlobals;
  std::set<std::string> fieldDeclarationInitializers;
  std::set<std::string> staticDeclarationInitializers;
  std::vector<std::string> constructorMangledNames;
  std::vector<TypeInfo> resolvedBaseTypeInfos;
  std::optional<TypeInfo> resolvedBaseClassInfo;
  std::vector<TypeInfo> resolvedInterfaceTypeInfos;
  std::map<std::string, std::map<std::string, TypeInfo>> resolvedBaseTypeArgumentBindings;
  std::map<std::string, CompositeMemberInfo> methodInfo;
  std::vector<std::string> baseTypes;
  std::vector<std::string> genericParameters;
  std::optional<std::string> baseClass;
  std::vector<std::string> interfaces;
  bool isAbstract = false;
  bool isInterface = false;
  std::vector<std::string> vtableOrder;
  std::vector<std::string> vtableImplementations;
  std::vector<bool> vtableIsAbstract;
  std::map<std::string, unsigned> vtableSlotMap;
  std::string vtableGlobalName;
  std::string descriptorGlobalName;
  std::map<std::string, std::string> interfaceTableGlobals;
  std::vector<std::string> interfaceMethodOrder;
  std::map<std::string, unsigned> interfaceMethodSlotMap;
  std::optional<std::string> thisOverride;
  std::map<std::string, TypeInfo> typeArgumentBindings;
  std::map<std::string, std::vector<std::string>> genericMethodInstantiations;
  bool hasClassDescriptor = false;
  ClassDescriptor descriptor;
};

struct GenericsDiagnostics {
  bool diagnosticsEnabled = false;
  bool stackDumpEnabled = false;
  bool heuristicsEnabled = true;
  uint64_t uniqueCompositeInstantiations = 0;
  uint64_t uniqueFunctionInstantiations = 0;
  uint64_t moduleIRBytesBeforePrint = 0;
  uint64_t moduleIRBytesAfterPrint = 0;
  unsigned currentBindingDepth = 0;
  unsigned peakBindingDepth = 0;
  unsigned maxBindingDepth = 128;
  uint64_t instantiationBudget = 0;
  unsigned nestedDepthBudget = 0;
  unsigned arityWarningThreshold = 8;
  unsigned nestedDepthWarningThreshold = 4;
  bool depthLimitHit = false;
  bool instantiationBudgetExceeded = false;
  bool nestedBudgetExceeded = false;
  std::vector<std::string> bindingStack;
};

struct GenericsMetrics {
  uint64_t typeCacheHits = 0;
  uint64_t typeCacheMisses = 0;
  uint64_t functionCacheHits = 0;
  uint64_t functionCacheMisses = 0;
  bool enabled = false;
};

struct ActiveCompositeContext {
  std::string name;
  MethodKind kind = MethodKind::Regular;
  bool isStatic = false;
  std::set<std::string> initializedInstanceFields;
};

/// CodegenContext stores all mutable IR-generation state for a compiler
/// session. It replaces the previous collection of global variables used by
/// ast.cpp.
struct CodegenContext {
  std::unique_ptr<llvm::LLVMContext> llvmContext;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> builder;

  std::map<std::string, llvm::Value *> namedValues;
  std::map<std::string, llvm::GlobalVariable *> globalValues;
  std::map<std::string, TypeInfo> globalTypes;
  std::map<std::string, TypeInfo> localTypes;
  std::map<std::string, llvm::StructType *> structTypes;
  std::map<std::string, std::vector<int64_t>> arraySizes;
  std::map<std::string, std::vector<std::pair<std::string, unsigned>>> structFieldIndices;
  std::map<std::string, std::map<std::string, std::string>> structFieldTypes;
  std::map<std::string, CompositeTypeInfo> compositeMetadata;
  std::set<std::string> initializedStaticFields;

  std::map<std::string, std::vector<FunctionOverload>> functionOverloads;
  std::set<std::string> instantiatedGenericFunctions;
  GenericsDiagnostics genericsDiagnostics;
  GenericsMetrics genericsMetrics;

  std::vector<llvm::BasicBlock *> loopExitBlocks;
  std::vector<llvm::BasicBlock *> loopContinueBlocks;
  std::vector<std::set<std::string>> nonNullFactsStack;
  std::vector<ActiveCompositeContext> compositeContextStack;
  std::vector<std::vector<std::string>> genericParameterStack;
  std::map<std::string, unsigned> activeGenericParameters;
  std::vector<std::map<std::string, TypeInfo>> genericTypeBindingsStack;
  std::map<std::string, std::string> compositeLayoutCache;
  std::map<std::string, std::string> compositeMetadataAliases;
  std::map<std::string, std::string> genericFunctionInstantiationCache;

  void reset();
};

inline void CodegenContext::reset() {
  llvmContext.reset();
  module.reset();
  builder.reset();
  namedValues.clear();
  globalValues.clear();
  globalTypes.clear();
  localTypes.clear();
  structTypes.clear();
  arraySizes.clear();
  structFieldIndices.clear();
  structFieldTypes.clear();
  compositeMetadata.clear();
  initializedStaticFields.clear();
  functionOverloads.clear();
  instantiatedGenericFunctions.clear();
  loopExitBlocks.clear();
  loopContinueBlocks.clear();
  nonNullFactsStack.clear();
  compositeContextStack.clear();
  genericParameterStack.clear();
  activeGenericParameters.clear();
  genericTypeBindingsStack.clear();
  const bool metricsEnabled = genericsMetrics.enabled;
  const bool diagnosticsEnabled = genericsDiagnostics.diagnosticsEnabled;
  const bool stackDumpEnabled = genericsDiagnostics.stackDumpEnabled;
  const bool heuristicsEnabled = genericsDiagnostics.heuristicsEnabled;
  const unsigned preservedMaxBindingDepth = genericsDiagnostics.maxBindingDepth;
  const uint64_t preservedInstantiationBudget = genericsDiagnostics.instantiationBudget;
  const unsigned preservedNestedBudget = genericsDiagnostics.nestedDepthBudget;
  const unsigned preservedArityThreshold = genericsDiagnostics.arityWarningThreshold;
  const unsigned preservedNestedWarningThreshold =
      genericsDiagnostics.nestedDepthWarningThreshold;
  genericsMetrics = {};
  genericsMetrics.enabled = metricsEnabled;
  genericsDiagnostics = {};
  genericsDiagnostics.diagnosticsEnabled = diagnosticsEnabled;
  genericsDiagnostics.stackDumpEnabled = stackDumpEnabled;
  genericsDiagnostics.heuristicsEnabled = heuristicsEnabled;
  genericsDiagnostics.maxBindingDepth = preservedMaxBindingDepth;
  genericsDiagnostics.instantiationBudget = preservedInstantiationBudget;
  genericsDiagnostics.nestedDepthBudget = preservedNestedBudget;
  genericsDiagnostics.arityWarningThreshold = preservedArityThreshold;
  genericsDiagnostics.nestedDepthWarningThreshold =
      preservedNestedWarningThreshold;
  compositeLayoutCache.clear();
  compositeMetadataAliases.clear();
  genericFunctionInstantiationCache.clear();
}

#endif // HYBRID_CODEGEN_CONTEXT_H
