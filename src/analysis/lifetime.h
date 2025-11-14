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
  std::string name;
  TypeInfo type{};
  bool escapes = false;
  bool isParameter = false;
  bool isCaptured = false;
  bool needsZeroing = false;
  bool explicitRetainSeen = false;
  bool explicitReleaseSeen = false;
  bool uniqueMoved = false;
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
};

} // namespace analysis

#endif // HYBRID_ANALYSIS_LIFETIME_H
