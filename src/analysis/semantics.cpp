#include "analysis/semantics.h"

namespace analysis {

const LifetimePlan *SemanticAnalysis::analyzeFunction(FunctionAST &function) {
  auto plan = lifetimeAnalyzer.analyzeFunction(function);
  const FunctionAST *key = &function;
  cachedPlans[key] = std::move(plan);
  return cachedPlans[key].get();
}

const LifetimePlan *SemanticAnalysis::planFor(const FunctionAST &function) const {
  auto it = cachedPlans.find(&function);
  if (it == cachedPlans.end())
    return nullptr;
  return it->second.get();
}

void SemanticAnalysis::analyzeAggregate(const StructAST &aggregate) {
  cycleDetector.registerAggregate(aggregate);
}

void SemanticAnalysis::reset() {
  lifetimeAnalyzer.reset();
  cycleDetector.reset();
  cachedPlans.clear();
}

} // namespace analysis
