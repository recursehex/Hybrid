#ifndef HYBRID_OPTIMIZER_ARC_OPTIMIZER_H
#define HYBRID_OPTIMIZER_ARC_OPTIMIZER_H

#include <cstdint>
#include <map>
#include <string>

#include "llvm/IR/Module.h"

struct ArcRetainCounts {
  uint64_t retains = 0;
  uint64_t releases = 0;
  uint64_t autoreleases = 0;
};

/// Collect retain/release/autorelease call counts for each defined function in
/// the module. Functions with zero counts are omitted.
void collectArcRetainReleaseCounts(
    const llvm::Module &module,
    std::map<std::string, ArcRetainCounts> &out);

/// Run ARC-specific peephole optimizations over the module. Returns true when
/// any function changed.
bool runARCOptimizationPass(llvm::Module &module);

#endif // HYBRID_OPTIMIZER_ARC_OPTIMIZER_H
