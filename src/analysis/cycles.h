#ifndef HYBRID_ANALYSIS_CYCLES_H
#define HYBRID_ANALYSIS_CYCLES_H

#include <string>
#include <map>
#include <set>
#include <vector>

#include "ast.h"

namespace analysis {

struct CycleEdge {
  std::string ownerType;
  std::string fieldName;
  std::string targetType;
  bool weak = false;
  bool viaContainer = false;
};

struct CycleReport {
  std::vector<CycleEdge> path;
  std::string hint;
};

class CycleDetector {
public:
  CycleDetector() = default;

  void reset();
  void registerAggregate(const StructAST &aggregate);

  const std::vector<CycleReport> &reports() const { return warnings; }

private:
  std::map<std::string, std::vector<CycleEdge>> adjacency;
  std::vector<CycleReport> warnings;
  std::set<std::string> reportedFingerprints;

  void analyzeEdges(const std::string &typeName);
};

} // namespace analysis

#endif // HYBRID_ANALYSIS_CYCLES_H
