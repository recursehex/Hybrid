#include "analysis/smart_pointers.h"

#include <utility>

namespace analysis {

SmartPointerInfo SmartPointerSemantics::classify(const TypeInfo &type) const {
  SmartPointerInfo info;
  info.kind = type.smartPointerKind;
  info.participatesInARC = type.participatesInARC();
  info.strongOwner = info.kind == SmartPointerKind::Shared ||
                     info.kind == SmartPointerKind::Unique ||
                     (info.kind == SmartPointerKind::None &&
                      type.requiresARC());
  info.weakOwner = info.kind == SmartPointerKind::Weak ||
                   type.ownership == OwnershipQualifier::Weak;
  info.exclusive = info.kind == SmartPointerKind::Unique;
  return info;
}

bool SmartPointerSemantics::shouldRetain(const TypeInfo &type) const {
  SmartPointerInfo info = classify(type);
  if (info.exclusive)
    return false;
  return info.strongOwner;
}

bool SmartPointerSemantics::shouldRelease(const TypeInfo &type) const {
  SmartPointerInfo info = classify(type);
  if (info.exclusive)
    return false;
  return info.strongOwner;
}

bool SmartPointerSemantics::requiresMoveOnAssign(const TypeInfo &type) const {
  SmartPointerInfo info = classify(type);
  return info.exclusive;
}

bool SmartPointerSemantics::allowsZeroing(const TypeInfo &type) const {
  SmartPointerInfo info = classify(type);
  return info.kind == SmartPointerKind::Weak ||
         type.ownership == OwnershipQualifier::Weak;
}

void SmartPointerSemantics::noteExplicitRetain(const std::string &symbol) {
  ++retainCounts[symbol];
}

void SmartPointerSemantics::noteExplicitRelease(const std::string &symbol) {
  ++releaseCounts[symbol];
}

bool SmartPointerSemantics::sawExplicitRetain(const std::string &symbol) const {
  auto it = retainCounts.find(symbol);
  return it != retainCounts.end() && it->second > 0;
}

bool SmartPointerSemantics::sawExplicitRelease(const std::string &symbol) const {
  auto it = releaseCounts.find(symbol);
  return it != releaseCounts.end() && it->second > 0;
}

void SmartPointerSemantics::markUniqueMoved(const std::string &symbol) {
  uniqueMoves[symbol] = true;
}

void SmartPointerSemantics::clearUniqueMove(const std::string &symbol) {
  auto it = uniqueMoves.find(symbol);
  if (it != uniqueMoves.end())
    it->second = false;
}

bool SmartPointerSemantics::isUniqueMoved(const std::string &symbol) const {
  auto it = uniqueMoves.find(symbol);
  return it != uniqueMoves.end() && it->second;
}

void SmartPointerSemantics::reset() {
  retainCounts.clear();
  releaseCounts.clear();
  uniqueMoves.clear();
}

} // namespace analysis
