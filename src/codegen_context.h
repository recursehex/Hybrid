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

  std::vector<llvm::BasicBlock *> loopExitBlocks;
  std::vector<llvm::BasicBlock *> loopContinueBlocks;
  std::vector<std::set<std::string>> nonNullFactsStack;
  std::vector<ActiveCompositeContext> compositeContextStack;
  std::vector<std::vector<std::string>> genericParameterStack;
  std::map<std::string, unsigned> activeGenericParameters;
  std::vector<std::map<std::string, TypeInfo>> genericTypeBindingsStack;

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
}

#endif // HYBRID_CODEGEN_CONTEXT_H
