#ifndef HYBRID_ANALYSIS_SEMANTICS_H
#define HYBRID_ANALYSIS_SEMANTICS_H

#include <memory>
#include <map>

#include "analysis/cycles.h"
#include "analysis/lifetime.h"

namespace analysis {

class SemanticAnalysis {
public:
  SemanticAnalysis() = default;

  const LifetimePlan *analyzeFunction(FunctionAST &function);
  const LifetimePlan *planFor(const FunctionAST &function) const;
  void analyzeAggregate(const StructAST &aggregate);

  void reset();

private:
  LifetimeAnalyzer lifetimeAnalyzer;
  CycleDetector cycleDetector;
  std::map<const FunctionAST *, std::unique_ptr<LifetimePlan>>
      cachedPlans;
};

} // namespace analysis

#endif // HYBRID_ANALYSIS_SEMANTICS_H
