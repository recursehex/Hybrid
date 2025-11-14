#include "analysis/cycles.h"

#include <algorithm>
#include <cctype>
#include <functional>
#include <set>
#include <sstream>

#include "compiler_session.h"

namespace {

using analysis::CycleEdge;
using analysis::CycleReport;

std::string stripNullable(std::string value) {
  value.erase(std::remove(value.begin(), value.end(), '?'), value.end());
  return value;
}

std::string sanitizeTypeName(const TypeInfo &info) {
  if (!info.baseTypeName.empty())
    return stripNullable(info.baseTypeName);
  return stripNullable(info.typeName);
}

std::string resolveEdgeType(const TypeInfo &info) {
  if (!info.typeArguments.empty())
    return sanitizeTypeName(info.typeArguments.front());
  return sanitizeTypeName(info);
}

std::string toLowerCopy(const std::string &value) {
  std::string out;
  out.reserve(value.size());
  for (char ch : value)
    out.push_back(static_cast<char>(std::tolower(static_cast<unsigned char>(ch))));
  return out;
}

bool looksLikeBackReference(const std::string &fieldName) {
  std::string lower = toLowerCopy(fieldName);
  return lower.find("parent") != std::string::npos ||
         lower.find("owner") != std::string::npos ||
         lower.find("delegate") != std::string::npos ||
         lower.find("handler") != std::string::npos;
}

std::string buildCycleMessage(const CycleReport &report,
                              const std::string &rootType) {
  std::ostringstream oss;
  oss << "Potential retain cycle detected: " << rootType;
  for (const auto &edge : report.path) {
    oss << " --(" << edge.fieldName << ")--> " << edge.targetType;
  }
  oss << " -- back to " << rootType;
  return oss.str();
}

std::string buildCycleHint(const CycleReport &report) {
  for (const auto &edge : report.path) {
    if (looksLikeBackReference(edge.fieldName)) {
      return "Consider marking '" + edge.fieldName +
             "' as 'weak' to break the retain cycle.";
    }
  }
  if (!report.path.empty()) {
    return "Mark back-references like '" + report.path.back().fieldName +
           "' as 'weak' or move them into weak smart pointers.";
  }
  return "Use weak references for delegate/parent fields to avoid retain cycles.";
}

std::string makeFingerprint(const std::vector<const CycleEdge *> &path,
                            const std::string &rootType) {
  std::string fingerprint = rootType;
  for (const auto *edge : path) {
    fingerprint.push_back('|');
    fingerprint += edge->ownerType;
    fingerprint.push_back('.');
    fingerprint += edge->fieldName;
    fingerprint.push_back('>');
    fingerprint += edge->targetType;
  }
  return fingerprint;
}

} // namespace

namespace analysis {

void CycleDetector::reset() {
  adjacency.clear();
  warnings.clear();
  reportedFingerprints.clear();
}

void CycleDetector::registerAggregate(const StructAST &aggregate) {
  const std::string owner = aggregate.getName();
  auto &edges = adjacency[owner];
  edges.clear();

  for (const auto &field : aggregate.getFields()) {
    if (!field)
      continue;
    const TypeInfo &type = field->getTypeInfo();
    if (!type.participatesInARC())
      continue;

    CycleEdge edge;
    edge.ownerType = owner;
    edge.fieldName = field->getName();
    edge.targetType = resolveEdgeType(type);
    edge.weak = type.ownership == OwnershipQualifier::Weak ||
                type.smartPointerKind == SmartPointerKind::Weak;
    edge.viaContainer = type.isArray || type.isSmartPointer();

    if (!edge.targetType.empty())
      edges.push_back(std::move(edge));
  }

  analyzeEdges(owner);
}

void CycleDetector::analyzeEdges(const std::string &typeName) {
  auto it = adjacency.find(typeName);
  if (it == adjacency.end())
    return;

  constexpr unsigned kMaxDepth = 5;
  std::vector<const CycleEdge *> path;
  std::set<std::string> visiting;

  std::function<void(const std::string &, unsigned)> dfs =
      [&](const std::string &current, unsigned depth) {
        if (depth > kMaxDepth)
          return;
        auto edgesIt = adjacency.find(current);
        if (edgesIt == adjacency.end())
          return;
        for (const CycleEdge &edge : edgesIt->second) {
          if (edge.weak || edge.targetType.empty())
            continue;
          bool closesCycle = edge.targetType == typeName;
          if (!closesCycle && visiting.count(edge.targetType))
            continue;
          path.push_back(&edge);
          if (closesCycle && !path.empty()) {
            std::string fingerprint = makeFingerprint(path, typeName);
            if (reportedFingerprints.insert(fingerprint).second) {
              CycleReport report;
              report.path.reserve(path.size());
              for (const CycleEdge *ptr : path)
                report.path.push_back(*ptr);
              report.hint = buildCycleHint(report);
              warnings.push_back(report);
              reportCompilerWarning(buildCycleMessage(report, typeName),
                                    report.hint);
            }
          } else {
            visiting.insert(edge.targetType);
            dfs(edge.targetType, depth + 1);
            visiting.erase(edge.targetType);
          }
          path.pop_back();
        }
      };

  visiting.insert(typeName);
  dfs(typeName, 0);
  visiting.erase(typeName);
}

} // namespace analysis
