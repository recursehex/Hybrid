#ifndef HYBRID_ANALYSIS_LIFETIME_H
#define HYBRID_ANALYSIS_LIFETIME_H

#include <memory>
#include <string>
#include <map>
#include <vector>

#include "analysis/smart_pointers.h"

namespace analysis {

struct LifetimeEvent {
  enum class Kind {
    Retain,
    Release,
    Autorelease,
    Escape,
    Move,
    ManualRetain,
    ManualRelease
  };

  Kind kind = Kind::Retain;
  std::string symbol;
  std::string note;
};

struct VariableLifetimePlan {
  // Tracks the ownership state of unique<T> variables so we can flag
  // use-after-move while still allowing reassignment after reinit.
  enum class UniqueState {
    Unknown,
    Alive,
    Moved
  };

  std::string name;
  TypeInfo type{};
  bool escapes = false;
  bool isParameter = false;
  bool isCaptured = false;
  bool needsZeroing = false;
  bool explicitRetainSeen = false;
  bool explicitReleaseSeen = false;
  bool manualDestructorCalled = false;
  bool manualDestructorDoubleReported = false;
  bool manuallyReleased = false;
  bool destroyed = false;
  bool manualDoubleReleaseReported = false;
  bool useAfterManualReleaseReported = false;
  std::string lastManualReleaseNote;
  UniqueState uniqueState = UniqueState::Unknown;
  std::vector<LifetimeEvent> events;
};

struct LifetimePlan {
  const FunctionAST *function = nullptr;
  std::map<std::string, VariableLifetimePlan> variables;
  std::vector<LifetimeEvent> globalEvents;
  std::vector<std::string> diagnostics;
};

class LifetimeAnalyzer {
public:
  LifetimeAnalyzer();

  std::unique_ptr<LifetimePlan> analyzeFunction(FunctionAST &function);
  void insertRetainRelease(BlockStmtAST *block, LifetimePlan &plan);
  void optimizeRedundantCalls(LifetimePlan &plan);
  void reset();

private:
  SmartPointerSemantics pointerSemantics;
  LifetimePlan *activePlan = nullptr;
  std::vector<std::vector<std::string>> blockVariableStack;
  std::vector<std::vector<std::pair<std::string, TypeInfo>>> pendingBlockVariables;

  void seedParameters(FunctionAST &function, LifetimePlan &plan);
  void walkBlock(BlockStmtAST &block, LifetimePlan &plan);
  void walkStatement(StmtAST &stmt, LifetimePlan &plan);
  void walkExpression(ExprAST &expr, LifetimePlan &plan);
  void handleVariableDeclaration(VariableDeclarationStmtAST &decl,
                                 LifetimePlan &plan);
  void handleReturn(ReturnStmtAST &ret, LifetimePlan &plan);
  void handleAssignment(BinaryExprAST &assign, LifetimePlan &plan);
  void emitEvent(const std::string &symbol, LifetimePlan &plan,
                 LifetimeEvent::Kind kind, const std::string &note = {});
  VariableLifetimePlan &ensureVariable(const std::string &symbol,
                                       const TypeInfo *type,
                                       LifetimePlan &plan);
  void enterBlock();
  void exitBlock(LifetimePlan &plan);
  void releaseSymbol(VariableLifetimePlan &var, LifetimePlan &plan,
                     const std::string &note);
  bool shouldTrack(const VariableLifetimePlan &var) const;
  void markEscape(const std::string &symbol, const std::string &reason,
                  LifetimePlan &plan);
  void finalizePlan(LifetimePlan &plan);
  void visitVariableExpr(VariableExprAST &varExpr, LifetimePlan &plan);
  void queueBlockVariable(const std::string &name, const TypeInfo &type);
  void injectPendingBlockVariables(LifetimePlan &plan);
  void validateSmartPointerTransfer(VariableLifetimePlan &target,
                                    VariableLifetimePlan &source,
                                    const std::string &context);
  void markUniqueMoved(VariableLifetimePlan &var);
  void markUniqueReinitialized(VariableLifetimePlan &var);
  bool isUniqueMoved(const VariableLifetimePlan &var) const;
  void recordManualRelease(VariableLifetimePlan &entry, LifetimePlan &plan,
                           const std::string &note,
                           bool suppressDuplicateWarning = false);
};

} // namespace analysis

#endif // HYBRID_ANALYSIS_LIFETIME_H
