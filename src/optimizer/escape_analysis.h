#ifndef HYBRID_OPTIMIZER_ESCAPE_ANALYSIS_H
#define HYBRID_OPTIMIZER_ESCAPE_ANALYSIS_H

#include "llvm/IR/Module.h"

struct ArcEscapeSummary {
  unsigned stackLocals = 0;
  unsigned removedCalls = 0;
};

/// Run ARC escape analysis over the module, removing retain/release/autorelease
/// calls on values proven to be stack-only. Returns true if the module changed.
/// When debugLogging is true, writes a short summary to stderr.
bool runARCEscapeAnalysis(llvm::Module &module, bool debugLogging,
                          ArcEscapeSummary *summary = nullptr);

#endif // HYBRID_OPTIMIZER_ESCAPE_ANALYSIS_H
