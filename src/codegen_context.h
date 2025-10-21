#ifndef HYBRID_CODEGEN_CONTEXT_H
#define HYBRID_CODEGEN_CONTEXT_H

#include <cstdint>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "ast.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

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
  std::map<std::string, int64_t> arraySizes;
  std::map<std::string, std::vector<std::pair<std::string, unsigned>>> structFieldIndices;
  std::map<std::string, std::map<std::string, std::string>> structFieldTypes;

  std::vector<llvm::BasicBlock *> loopExitBlocks;
  std::vector<llvm::BasicBlock *> loopContinueBlocks;
  std::vector<std::set<std::string>> nonNullFactsStack;

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
  loopExitBlocks.clear();
  loopContinueBlocks.clear();
  nonNullFactsStack.clear();
}

#endif // HYBRID_CODEGEN_CONTEXT_H
