// This file implements ARC escape analysis to detect stack-only values and trim ARC runtime calls that cannot escape.

#include "optimizer/escape_analysis.h"

#include <utility>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/PromoteMemToReg.h"

using llvm::dyn_cast;

namespace {

bool isHybridRetain(const llvm::CallBase &call) {
  if (const llvm::Function *fn = call.getCalledFunction())
    return fn->getName() == "hybrid_retain";
  const llvm::Value *callee = call.getCalledOperand();
  if (!callee)
    return false;
  callee = callee->stripPointerCasts();
  if (const auto *fn = llvm::dyn_cast<llvm::Function>(callee))
    return fn->getName() == "hybrid_retain";
  return false;
}

bool isHybridRelease(const llvm::CallBase &call) {
  if (const llvm::Function *fn = call.getCalledFunction())
    return fn->getName() == "hybrid_release";
  const llvm::Value *callee = call.getCalledOperand();
  if (!callee)
    return false;
  callee = callee->stripPointerCasts();
  if (const auto *fn = llvm::dyn_cast<llvm::Function>(callee))
    return fn->getName() == "hybrid_release";
  return false;
}

bool isHybridAutorelease(const llvm::CallBase &call) {
  if (const llvm::Function *fn = call.getCalledFunction())
    return fn->getName() == "hybrid_autorelease";
  const llvm::Value *callee = call.getCalledOperand();
  if (!callee)
    return false;
  callee = callee->stripPointerCasts();
  if (const auto *fn = llvm::dyn_cast<llvm::Function>(callee))
    return fn->getName() == "hybrid_autorelease";
  return false;
}

bool isArcRuntimeCall(const llvm::CallBase &call) {
  return isHybridRetain(call) || isHybridRelease(call) ||
         isHybridAutorelease(call);
}

bool isLifetimeIntrinsic(const llvm::Function *fn) {
  if (!fn)
    return false;
  if (!fn->isIntrinsic())
    return false;
  switch (fn->getIntrinsicID()) {
  case llvm::Intrinsic::lifetime_start:
  case llvm::Intrinsic::lifetime_end:
    return true;
  default:
    return false;
  }
}

bool isNullLike(const llvm::Value *value) {
  return llvm::isa<llvm::ConstantPointerNull>(value) ||
         llvm::isa<llvm::UndefValue>(value) ||
         llvm::isa<llvm::PoisonValue>(value);
}

enum class DeriveState { Unknown, True, False };

bool isDerivedFromRoot(const llvm::Value *value, const llvm::Value *root,
                       llvm::DenseMap<const llvm::Value *, DeriveState> &cache) {
  auto it = cache.find(value);
  if (it != cache.end())
    return it->second == DeriveState::True;

  if (value == root || isNullLike(value)) {
    cache[value] = DeriveState::True;
    return true;
  }

  cache[value] = DeriveState::Unknown;

  if (const auto *castOp = dyn_cast<llvm::Operator>(value)) {
    if (castOp->getOpcode() == llvm::Instruction::BitCast ||
        castOp->getOpcode() == llvm::Instruction::AddrSpaceCast ||
        castOp->getOpcode() == llvm::Instruction::GetElementPtr) {
      bool derived =
          isDerivedFromRoot(castOp->getOperand(0), root, cache);
      cache[value] = derived ? DeriveState::True : DeriveState::False;
      return derived;
    }
  }

  if (const auto *phi = dyn_cast<llvm::PHINode>(value)) {
    for (const llvm::Value *incoming : phi->incoming_values()) {
      if (!isDerivedFromRoot(incoming, root, cache)) {
        cache[value] = DeriveState::False;
        return false;
      }
    }
    cache[value] = DeriveState::True;
    return true;
  }

  if (const auto *select = dyn_cast<llvm::SelectInst>(value)) {
    bool trueDerived =
        isDerivedFromRoot(select->getTrueValue(), root, cache);
    bool falseDerived =
        isDerivedFromRoot(select->getFalseValue(), root, cache);
    cache[value] =
        (trueDerived && falseDerived) ? DeriveState::True : DeriveState::False;
    return trueDerived && falseDerived;
  }

  if (const auto *load = dyn_cast<llvm::LoadInst>(value)) {
    const llvm::Value *ptrBase =
        llvm::getUnderlyingObject(load->getPointerOperand(), 5);
    if (!ptrBase) {
      cache[value] = DeriveState::False;
      return false;
    }
    if (!isDerivedFromRoot(ptrBase, root, cache)) {
      cache[value] = DeriveState::False;
      return false;
    }
    cache[value] = DeriveState::True;
    return true;
  }

  cache[value] = DeriveState::False;
  return false;
}

void promoteAllocas(llvm::Function &fn) {
  if (fn.isDeclaration())
    return;
  llvm::SmallVector<llvm::AllocaInst *, 8> promotable;
  llvm::BasicBlock &entry = fn.getEntryBlock();
  for (llvm::Instruction &inst : entry) {
    auto *alloca = dyn_cast<llvm::AllocaInst>(&inst);
    if (!alloca)
      continue;
    if (llvm::isAllocaPromotable(alloca))
      promotable.push_back(alloca);
  }
  if (promotable.empty())
    return;

  llvm::DominatorTree domTree(fn);
  llvm::PromoteMemToReg(promotable, domTree);
}

bool valueEscapesStack(const llvm::Value *root) {
  llvm::SmallVector<const llvm::Value *, 16> worklist;
  llvm::SmallPtrSet<const llvm::Value *, 32> visited;
  llvm::DenseMap<const llvm::Value *, DeriveState> deriveCache;

  worklist.push_back(root);
  while (!worklist.empty()) {
    const llvm::Value *value = worklist.pop_back_val();
    if (!visited.insert(value).second)
      continue;

    for (const llvm::User *user : value->users()) {
      if (const auto *op = dyn_cast<llvm::Operator>(user)) {
        if (op->getOpcode() == llvm::Instruction::BitCast ||
            op->getOpcode() == llvm::Instruction::AddrSpaceCast ||
            op->getOpcode() == llvm::Instruction::GetElementPtr) {
          worklist.push_back(op);
          continue;
        }
      }

      if (const auto *inst = dyn_cast<llvm::Instruction>(user)) {
        if (const auto *call = dyn_cast<llvm::CallBase>(inst)) {
          if (isArcRuntimeCall(*call) ||
              isLifetimeIntrinsic(call->getCalledFunction()))
            continue;
          return true;
        }

        if (const auto *store = dyn_cast<llvm::StoreInst>(inst)) {
          const llvm::Value *stored = store->getValueOperand();
          const llvm::Value *dest = store->getPointerOperand();
          const bool storedIsPointer = stored->getType()->isPointerTy();
          const bool storedDerived =
              storedIsPointer &&
              (isDerivedFromRoot(stored, root, deriveCache) ||
               isNullLike(stored));
          const bool destDerived =
              isDerivedFromRoot(dest, root, deriveCache);

          if (storedDerived) {
            if (!destDerived)
              return true;
            worklist.push_back(stored);
            continue;
          }

          if (destDerived)
            continue;

          if (stored == value)
            return true;
          continue;
        }

        if (const auto *load = dyn_cast<llvm::LoadInst>(inst)) {
          if (isDerivedFromRoot(load->getPointerOperand(), root, deriveCache))
            worklist.push_back(load);
          continue;
        }

        if (const auto *phi = dyn_cast<llvm::PHINode>(inst)) {
          bool allDerived = true;
          for (const llvm::Value *incoming : phi->incoming_values()) {
            if (!isDerivedFromRoot(incoming, root, deriveCache)) {
              allDerived = false;
              break;
            }
          }
          if (!allDerived)
            return true;
          worklist.push_back(phi);
          continue;
        }

        if (const auto *select = dyn_cast<llvm::SelectInst>(inst)) {
          if (!isDerivedFromRoot(select->getTrueValue(), root, deriveCache) ||
              !isDerivedFromRoot(select->getFalseValue(), root, deriveCache))
            return true;
          worklist.push_back(select);
          continue;
        }

        if (isa<llvm::ReturnInst>(inst))
          return true;

        if (isa<llvm::LoadInst>(inst) || isa<llvm::CmpInst>(inst) ||
            isa<llvm::DbgInfoIntrinsic>(inst))
          continue;

        return true;
      }

      return true;
    }
  }

  return false;
}

bool derivesFromAnyRoot(const llvm::Value *value,
                        const llvm::SmallPtrSetImpl<const llvm::Value *>
                            &stackRoots) {
  llvm::DenseMap<const llvm::Value *, DeriveState> cache;
  for (const llvm::Value *root : stackRoots) {
    if (isDerivedFromRoot(value, root, cache))
      return true;
  }
  return false;
}

void logSummary(const llvm::Function &fn, unsigned stackSlots,
                unsigned removed, bool changed) {
  llvm::errs() << "[arc-escape] " << fn.getName() << " stack-only="
               << stackSlots << " removed=" << removed;
  if (!changed)
    llvm::errs() << " (no-op)";
  llvm::errs() << "\n";
}

} // namespace

bool runARCEscapeAnalysis(llvm::Module &module, bool debugLogging,
                          ArcEscapeSummary *summary) {
  unsigned globalStackLocals = 0;
  unsigned globalRemoved = 0;
  bool changed = false;

  for (llvm::Function &fn : module) {
    if (fn.isDeclaration())
      continue;

    promoteAllocas(fn);

    llvm::SmallPtrSet<const llvm::Value *, 8> stackRoots;
    for (llvm::Instruction &inst : llvm::instructions(fn)) {
      auto *alloca = dyn_cast<llvm::AllocaInst>(&inst);
      if (!alloca)
        continue;
      if (alloca->isArrayAllocation())
        continue;
      if (alloca->getAllocatedType()->isPointerTy())
        continue;

      if (!valueEscapesStack(alloca))
        stackRoots.insert(alloca);
    }

    if (stackRoots.empty()) {
      if (debugLogging)
        logSummary(fn, 0, 0, false);
      continue;
    }

    unsigned removedCalls = 0;
    for (auto it = fn.begin(), end = fn.end(); it != end; ++it) {
      for (auto instIt = it->begin(); instIt != it->end();) {
        llvm::Instruction *inst = &*instIt++;
        auto *call = dyn_cast<llvm::CallBase>(inst);
        if (!call)
          continue;
        if (!isArcRuntimeCall(*call))
          continue;

        const llvm::Value *arg = call->getArgOperand(0)->stripPointerCasts();
        if (!derivesFromAnyRoot(arg, stackRoots))
          continue;

        call->eraseFromParent();
        ++removedCalls;
        changed = true;
      }
    }

    globalStackLocals += stackRoots.size();
    globalRemoved += removedCalls;

    if (debugLogging)
      logSummary(fn, stackRoots.size(), removedCalls, removedCalls > 0);
  }

  if (summary) {
    summary->stackLocals = globalStackLocals;
    summary->removedCalls = globalRemoved;
  }

  if (debugLogging) {
    llvm::errs() << "[arc-escape] module stack-only=" << globalStackLocals
                 << " removed=" << globalRemoved << "\n";
    if (llvm::verifyModule(module, &llvm::errs()))
      llvm::errs() << "[arc-escape] warning: module failed verification\n";
  }

  return changed;
}
