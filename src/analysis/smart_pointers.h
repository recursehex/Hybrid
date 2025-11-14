#ifndef HYBRID_ANALYSIS_SMART_POINTERS_H
#define HYBRID_ANALYSIS_SMART_POINTERS_H

#include <string>
#include <map>

#include "ast.h"

namespace analysis {

struct SmartPointerInfo {
  SmartPointerKind kind = SmartPointerKind::None;
  bool strongOwner = false;
  bool weakOwner = false;
  bool exclusive = false;
  bool participatesInARC = false;
};

class SmartPointerSemantics {
public:
  SmartPointerSemantics() = default;

  SmartPointerInfo classify(const TypeInfo &type) const;

  bool shouldRetain(const TypeInfo &type) const;
  bool shouldRelease(const TypeInfo &type) const;
  bool requiresMoveOnAssign(const TypeInfo &type) const;
  bool allowsZeroing(const TypeInfo &type) const;

  void noteExplicitRetain(const std::string &symbol);
  void noteExplicitRelease(const std::string &symbol);
  bool sawExplicitRetain(const std::string &symbol) const;
  bool sawExplicitRelease(const std::string &symbol) const;

  void markUniqueMoved(const std::string &symbol);
  void clearUniqueMove(const std::string &symbol);
  bool isUniqueMoved(const std::string &symbol) const;

  void reset();

private:
  std::map<std::string, unsigned> retainCounts;
  std::map<std::string, unsigned> releaseCounts;
  std::map<std::string, bool> uniqueMoves;
};

} // namespace analysis

#endif // HYBRID_ANALYSIS_SMART_POINTERS_H
