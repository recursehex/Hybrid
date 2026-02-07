// This file implements an ARC optimization pass that prunes redundant retain/release chains and collects ARC runtime call counts.

#include "optimizer/arc_optimizer.h"

#include <cstdlib>
#include <iterator>
#include <utility>

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

using llvm::dyn_cast;

namespace {

const llvm::Function *resolveCalledFunction(const llvm::CallBase &call) {
  if (const llvm::Function *direct = call.getCalledFunction())
    return direct;
  const llvm::Value *callee = call.getCalledOperand();
  if (!callee)
    return nullptr;
  callee = callee->stripPointerCasts();
  return llvm::dyn_cast<llvm::Function>(callee);
}

bool isHybridRetain(const llvm::CallBase &call) {
  if (const llvm::Function *fn = resolveCalledFunction(call))
    return fn->getName() == "hybrid_retain";
  return false;
}

bool isHybridRelease(const llvm::CallBase &call) {
  if (const llvm::Function *fn = resolveCalledFunction(call))
    return fn->getName() == "hybrid_release";
  return false;
}

bool isHybridAutorelease(const llvm::CallBase &call) {
  if (const llvm::Function *fn = resolveCalledFunction(call))
    return fn->getName() == "hybrid_autorelease";
  return false;
}

bool usedOnlyByReleaseChain(llvm::Value *value, llvm::CallBase *release) {
  llvm::SmallPtrSet<llvm::Value *, 8> visited;
  llvm::SmallVector<llvm::Value *, 8> worklist;
  worklist.push_back(value);

  while (!worklist.empty()) {
    llvm::Value *current = worklist.pop_back_val();
    if (!visited.insert(current).second)
      continue;

    for (llvm::User *user : current->users()) {
      if (user == release)
        continue;
      auto *inst = dyn_cast<llvm::Instruction>(user);
      if (inst &&
          (isa<llvm::BitCastInst>(inst) || isa<llvm::AddrSpaceCastInst>(inst))) {
        worklist.push_back(inst);
        continue;
      }
      return false;
    }
  }

  return true;
}

bool arcOptDebugEnabled() {
  static const bool enabled = std::getenv("HYBRID_ARC_OPT_DEBUG") != nullptr;
  return enabled;
}

class ARCOptimizationPass : public llvm::FunctionPass {
public:
  static char ID;

  ARCOptimizationPass() : FunctionPass(ID) {}

  bool runOnFunction(llvm::Function &F) override;

private:
  bool tryElideAdjacentRetainRelease(llvm::CallBase &retainCall);
};

} // namespace

char ARCOptimizationPass::ID = 0;

bool ARCOptimizationPass::tryElideAdjacentRetainRelease(
    llvm::CallBase &retainCall) {
  if (arcOptDebugEnabled()) {
    if (llvm::Function *parent = retainCall.getFunction())
      llvm::errs() << "[arc-opt] retain candidate in " << parent->getName()
                   << "\n";
  }

  llvm::Instruction *cursor = retainCall.getNextNode();
  llvm::SmallVector<llvm::Instruction *, 4> passthrough;

  while (cursor &&
         (cursor->isDebugOrPseudoInst() || isa<llvm::BitCastInst>(cursor) ||
          isa<llvm::AddrSpaceCastInst>(cursor))) {
    if (!cursor->isDebugOrPseudoInst())
      passthrough.push_back(cursor);
    cursor = cursor->getNextNode();
  }

  auto *releaseCall = dyn_cast_or_null<llvm::CallBase>(cursor);
  if (!releaseCall || !isHybridRelease(*releaseCall))
    return false;

  llvm::Value *retainArg = retainCall.getArgOperand(0)->stripPointerCasts();
  llvm::Value *releaseArg = releaseCall->getArgOperand(0)->stripPointerCasts();
  llvm::Value *retainedValue = retainCall.stripPointerCasts();

  if (releaseArg != retainArg && releaseArg != retainedValue)
    return false;

  if (!usedOnlyByReleaseChain(&retainCall, releaseCall))
    return false;

  llvm::Function *parentFn = retainCall.getFunction();
  llvm::Instruction *between = retainCall.getNextNode();
  while (between && between != releaseCall) {
    if (between->isDebugOrPseudoInst() || isa<llvm::BitCastInst>(between) ||
        isa<llvm::AddrSpaceCastInst>(between)) {
      between = between->getNextNode();
      continue;
    }
    return false;
  }

  releaseCall->eraseFromParent();
  for (llvm::Instruction *inst : passthrough) {
    if (inst->use_empty())
      inst->eraseFromParent();
  }
  if (retainCall.use_empty())
    retainCall.eraseFromParent();
  if (arcOptDebugEnabled()) {
    if (parentFn)
      llvm::errs() << "[arc-opt] elided retain/release in "
                   << parentFn->getName() << "\n";
  }
  return true;
}

bool ARCOptimizationPass::runOnFunction(llvm::Function &F) {
  if (F.isDeclaration())
    return false;

  if (arcOptDebugEnabled())
    llvm::errs() << "[arc-opt] visiting " << F.getName() << "\n";

  bool changed = false;
  for (auto &BB : F) {
    for (auto it = BB.begin(); it != BB.end();) {
      llvm::Instruction *inst = &*it;
      auto next = std::next(it);
      auto *call = dyn_cast<llvm::CallBase>(inst);
      if (call && isHybridRetain(*call)) {
        if (tryElideAdjacentRetainRelease(*call)) {
          changed = true;
          it = BB.begin();
          continue;
        }
      }
      it = next;
    }
  }

  return changed;
}

void collectArcRetainReleaseCounts(
    const llvm::Module &module,
    std::map<std::string, ArcRetainCounts> &out) {
  out.clear();
  const bool debug = arcOptDebugEnabled();
  for (const llvm::Function &func : module) {
    if (func.isDeclaration())
      continue;
    ArcRetainCounts counts;
    bool hasCounts = false;
    for (const llvm::Instruction &inst : llvm::instructions(func)) {
      auto *call = dyn_cast<llvm::CallBase>(&inst);
      if (!call)
        continue;
      if (isHybridRetain(*call)) {
        ++counts.retains;
        hasCounts = true;
      } else if (isHybridRelease(*call)) {
        ++counts.releases;
        hasCounts = true;
      } else if (isHybridAutorelease(*call)) {
        ++counts.autoreleases;
        hasCounts = true;
      }
    }
    if (hasCounts) {
      out[func.getName().str()] = counts;
      if (debug) {
        llvm::errs() << "[arc-opt] counts " << func.getName() << " r:"
                     << counts.retains << " R:" << counts.releases
                     << " a:" << counts.autoreleases << "\n";
      }
    }
  }
}

bool runARCOptimizationPass(llvm::Module &module) {
  ARCOptimizationPass pass;
  if (arcOptDebugEnabled())
    llvm::errs() << "[arc-opt] running optimizer over "
                 << static_cast<unsigned>(module.size()) << " functions\n";
  bool changed = false;
  for (llvm::Function &func : module) {
    if (func.isDeclaration())
      continue;
    changed |= pass.runOnFunction(func);
  }
  return changed;
}
