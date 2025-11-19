#include "analysis/lifetime.h"

#include <utility>

#include "compiler_session.h"

namespace analysis {

LifetimeAnalyzer::LifetimeAnalyzer() = default;

std::unique_ptr<LifetimePlan> LifetimeAnalyzer::analyzeFunction(FunctionAST &function) {
  pointerSemantics.reset();
  blockVariableStack.clear();
  auto plan = std::make_unique<LifetimePlan>();
  plan->function = &function;
  activePlan = plan.get();
  seedParameters(function, *plan);
  if (auto *body = function.getBody()) {
    walkBlock(*body, *plan);
  }
  finalizePlan(*plan);
  activePlan = nullptr;
  return plan;
}

void LifetimeAnalyzer::reset() {
  pointerSemantics.reset();
  activePlan = nullptr;
  blockVariableStack.clear();
  pendingBlockVariables.clear();
}

void LifetimeAnalyzer::insertRetainRelease(BlockStmtAST * /*block*/,
                                           LifetimePlan & /*plan*/) {}

void LifetimeAnalyzer::optimizeRedundantCalls(LifetimePlan & /*plan*/) {}

void LifetimeAnalyzer::seedParameters(FunctionAST &function,
                                      LifetimePlan &plan) {
  if (!function.getProto())
    return;
  for (const auto &param : function.getProto()->getArgs()) {
    if (!param.DeclaredType.participatesInARC())
      continue;
    auto &entry = ensureVariable(param.Name, &param.DeclaredType, plan);
    entry.isParameter = true;
    entry.needsZeroing = pointerSemantics.allowsZeroing(param.DeclaredType);
  }
}

void LifetimeAnalyzer::walkBlock(BlockStmtAST &block, LifetimePlan &plan) {
  enterBlock();
  injectPendingBlockVariables(plan);
  for (const auto &stmt : block.getStatements()) {
    if (!stmt)
      continue;
    walkStatement(*stmt, plan);
  }
  exitBlock(plan);
}

void LifetimeAnalyzer::walkStatement(StmtAST &stmt, LifetimePlan &plan) {
  if (auto *block = dynamic_cast<BlockStmtAST *>(&stmt)) {
    walkBlock(*block, plan);
    return;
  }
  if (auto *decl = dynamic_cast<VariableDeclarationStmtAST *>(&stmt)) {
    handleVariableDeclaration(*decl, plan);
    return;
  }
  if (auto *exprStmt = dynamic_cast<ExpressionStmtAST *>(&stmt)) {
    if (auto *expr = exprStmt->getExpression())
      walkExpression(*expr, plan);
    return;
  }
  if (auto *ret = dynamic_cast<ReturnStmtAST *>(&stmt)) {
    handleReturn(*ret, plan);
    return;
  }
  if (auto *ifStmt = dynamic_cast<IfStmtAST *>(&stmt)) {
    if (auto *cond = ifStmt->getCondition())
      walkExpression(*cond, plan);
    if (auto *thenBranch = ifStmt->getThenBranch())
      walkStatement(*thenBranch, plan);
    if (auto *elseBranch = ifStmt->getElseBranch())
      walkStatement(*elseBranch, plan);
    return;
  }
  if (auto *whileStmt = dynamic_cast<WhileStmtAST *>(&stmt)) {
    if (auto *cond = whileStmt->getCondition())
      walkExpression(*cond, plan);
    if (auto *body = whileStmt->getBody())
      walkStatement(*body, plan);
    return;
  }
  if (auto *forEach = dynamic_cast<ForEachStmtAST *>(&stmt)) {
    if (auto *collection = forEach->getCollection())
      walkExpression(*collection, plan);
    queueBlockVariable(forEach->getVarName(), forEach->getTypeInfo());
    if (auto *body = forEach->getBody())
      walkBlock(*body, plan);
    return;
  }
  if (auto *forLoop = dynamic_cast<ForLoopStmtAST *>(&stmt)) {
    if (auto *init = forLoop->getInitExpr())
      walkExpression(*init, plan);
    if (auto *limit = forLoop->getLimitExpr())
      walkExpression(*limit, plan);
    if (auto *cond = forLoop->getCondExpr())
      walkExpression(*cond, plan);
    if (auto *step = forLoop->getStepExpr())
      walkExpression(*step, plan);
    if (auto *body = forLoop->getBody())
      walkBlock(*body, plan);
    return;
  }
  if (auto *switchStmt = dynamic_cast<SwitchStmtAST *>(&stmt)) {
    if (auto *condition = switchStmt->getCondition())
      walkExpression(*condition, plan);
    for (const auto &caseAst : switchStmt->getCases()) {
      if (!caseAst)
        continue;
      for (const auto &value : caseAst->getValues()) {
        if (value)
          walkExpression(*value, plan);
      }
      if (auto *body = caseAst->getBody())
        walkStatement(*body, plan);
    }
    return;
  }
  if (auto *unsafeBlock = dynamic_cast<UnsafeBlockStmtAST *>(&stmt)) {
    if (auto *body = unsafeBlock->getBody())
      walkBlock(*body, plan);
    return;
  }
  if (auto *assertStmt = dynamic_cast<AssertStmtAST *>(&stmt)) {
    if (auto *cond = assertStmt->getCondition())
      walkExpression(*cond, plan);
    return;
  }
}

void LifetimeAnalyzer::walkExpression(ExprAST &expr, LifetimePlan &plan) {
  if (auto *varExpr = dynamic_cast<VariableExprAST *>(&expr)) {
    visitVariableExpr(*varExpr, plan);
    return;
  }
  if (auto *binary = dynamic_cast<BinaryExprAST *>(&expr)) {
    if (binary->getOp() == "=")
      handleAssignment(*binary, plan);
    if (auto *lhs = binary->getLHS()) {
      if (!dynamic_cast<VariableExprAST *>(lhs))
        walkExpression(*lhs, plan);
    }
    if (auto *rhs = binary->getRHS()) {
      if (!dynamic_cast<VariableExprAST *>(rhs))
        walkExpression(*rhs, plan);
    }
    return;
  }
  if (auto *call = dynamic_cast<CallExprAST *>(&expr)) {
    for (const auto &arg : call->getArgs()) {
      if (arg)
        walkExpression(*arg, plan);
    }
    if (call->hasCalleeExpr() && call->getCalleeExpr())
      walkExpression(*call->getCalleeExpr(), plan);
    return;
  }
  if (auto *member = dynamic_cast<MemberAccessExprAST *>(&expr)) {
    if (auto *object = member->getObject())
      walkExpression(*object, plan);
    return;
  }
  if (auto *ternary = dynamic_cast<TernaryExprAST *>(&expr)) {
    if (auto *cond = ternary->getCondition())
      walkExpression(*cond, plan);
    if (auto *thenExpr = ternary->getThenExpr())
      walkExpression(*thenExpr, plan);
    if (auto *elseExpr = ternary->getElseExpr())
      walkExpression(*elseExpr, plan);
    return;
  }
  if (auto *switchExpr = dynamic_cast<SwitchExprAST *>(&expr)) {
    if (auto *condition = switchExpr->getCondition())
      walkExpression(*condition, plan);
    for (const auto &caseAst : switchExpr->getCases()) {
      if (!caseAst)
        continue;
      for (const auto &value : caseAst->getValues()) {
        if (value)
          walkExpression(*value, plan);
      }
      if (auto *bodyStmt = caseAst->getBody())
        walkStatement(*bodyStmt, plan);
      if (auto *bodyExpr = caseAst->getExpression())
        walkExpression(*bodyExpr, plan);
    }
    return;
  }
  if (auto *retainExpr = dynamic_cast<RetainExprAST *>(&expr)) {
    if (auto *operand = retainExpr->getOperand()) {
      if (auto *var = dynamic_cast<VariableExprAST *>(operand)) {
        auto &entry = ensureVariable(var->getName(), nullptr, plan);
        pointerSemantics.noteExplicitRetain(entry.name);
        entry.explicitRetainSeen = true;
        emitEvent(entry.name, plan, LifetimeEvent::Kind::ManualRetain,
                  "explicit retain()");
      }
      walkExpression(*operand, plan);
    }
    return;
  }
  if (auto *releaseExpr = dynamic_cast<ReleaseExprAST *>(&expr)) {
    if (auto *operand = releaseExpr->getOperand()) {
      if (auto *var = dynamic_cast<VariableExprAST *>(operand)) {
        auto &entry = ensureVariable(var->getName(), nullptr, plan);
        pointerSemantics.noteExplicitRelease(entry.name);
        entry.explicitReleaseSeen = true;
        emitEvent(entry.name, plan, LifetimeEvent::Kind::ManualRelease,
                  "explicit release()");
      }
      walkExpression(*operand, plan);
    }
    return;
  }
  if (auto *unary = dynamic_cast<UnaryExprAST *>(&expr)) {
    if (auto *operand = unary->getOperand())
      walkExpression(*operand, plan);
    return;
  }
  if (auto *cast = dynamic_cast<CastExprAST *>(&expr)) {
    if (auto *operand = cast->getOperand())
      walkExpression(*operand, plan);
    return;
  }
  if (auto *arrayLiteral = dynamic_cast<ArrayExprAST *>(&expr)) {
    for (const auto &element : arrayLiteral->getElements()) {
      if (element)
        walkExpression(*element, plan);
    }
    return;
  }
  if (auto *nullSafeArray = dynamic_cast<NullSafeElementAccessExprAST *>(&expr)) {
    if (auto *array = nullSafeArray->getArray())
      walkExpression(*array, plan);
    if (auto *index = nullSafeArray->getIndex())
      walkExpression(*index, plan);
    return;
  }
  if (auto *nullSafeAccess = dynamic_cast<NullSafeAccessExprAST *>(&expr)) {
    if (auto *object = nullSafeAccess->getObject())
      walkExpression(*object, plan);
    return;
  }
  if (auto *indexExpr = dynamic_cast<ArrayIndexExprAST *>(&expr)) {
    if (auto *array = indexExpr->getArray())
      walkExpression(*array, plan);
    for (const auto &offset : indexExpr->getIndices()) {
      if (offset)
        walkExpression(*offset, plan);
    }
    return;
  }
}

void LifetimeAnalyzer::handleVariableDeclaration(VariableDeclarationStmtAST &decl,
                                                 LifetimePlan &plan) {
  auto &entry = ensureVariable(decl.getName(), &decl.getTypeInfo(), plan);
  entry.needsZeroing = pointerSemantics.allowsZeroing(decl.getTypeInfo());
  if (blockVariableStack.empty())
    blockVariableStack.emplace_back();
  blockVariableStack.back().push_back(entry.name);
  if (shouldTrack(entry)) {
    emitEvent(entry.name, plan, LifetimeEvent::Kind::Retain, "initialized");
  }
  if (auto *init = decl.getInitializer()) {
    if (auto *varInit = dynamic_cast<VariableExprAST *>(init)) {
      auto &source = ensureVariable(varInit->getName(), nullptr, plan);
      if (&source != &entry)
        validateSmartPointerTransfer(entry, source, "initialization");
      if (pointerSemantics.requiresMoveOnAssign(source.type)) {
        if (isUniqueMoved(source)) {
          reportCompilerError("unique pointer '" + source.name +
                              "' was moved and cannot be used again");
        }
        markUniqueMoved(source);
      }
      markUniqueReinitialized(entry);
    } else {
      walkExpression(*init, plan);
      markUniqueReinitialized(entry);
    }
  }
}

void LifetimeAnalyzer::handleReturn(ReturnStmtAST &ret, LifetimePlan &plan) {
  if (!ret.getReturnValue())
    return;
  if (auto *varExpr = dynamic_cast<VariableExprAST *>(ret.getReturnValue()))
    markEscape(varExpr->getName(), "returned", plan);
  walkExpression(*ret.getReturnValue(), plan);
}

void LifetimeAnalyzer::handleAssignment(BinaryExprAST &assign,
                                        LifetimePlan &plan) {
  VariableExprAST *lhsVar = nullptr;
  VariableExprAST *rhsVar = nullptr;
  VariableLifetimePlan *lhsInfoPtr = nullptr;
  VariableLifetimePlan *rhsInfoPtr = nullptr;
  if (auto *lhs = assign.getLHS()) {
    lhsVar = dynamic_cast<VariableExprAST *>(lhs);
    if (!lhsVar)
      walkExpression(*lhs, plan);
  }
  if (auto *rhs = assign.getRHS()) {
    rhsVar = dynamic_cast<VariableExprAST *>(rhs);
    if (!rhsVar)
      walkExpression(*rhs, plan);
  }

  if (lhsVar) {
    auto &lhsInfo = ensureVariable(lhsVar->getName(), nullptr, plan);
    lhsInfoPtr = &lhsInfo;
    markUniqueReinitialized(lhsInfo);
    if (shouldTrack(lhsInfo)) {
      emitEvent(lhsInfo.name, plan, LifetimeEvent::Kind::Release,
                "overwritten");
    }
  }
  if (rhsVar) {
    auto &rhsInfo = ensureVariable(rhsVar->getName(), nullptr, plan);
    rhsInfoPtr = &rhsInfo;
    if ((!lhsInfoPtr || lhsInfoPtr != rhsInfoPtr) &&
        pointerSemantics.requiresMoveOnAssign(rhsInfo.type)) {
      if (isUniqueMoved(rhsInfo)) {
        reportCompilerError("unique pointer '" + rhsInfo.name +
                            "' was moved and cannot be reassigned");
      }
      markUniqueMoved(rhsInfo);
    }
    if (shouldTrack(rhsInfo)) {
      std::string note = "assigned to ";
      note += lhsVar ? lhsVar->getName() : "target";
      emitEvent(rhsInfo.name, plan, LifetimeEvent::Kind::Retain, note);
    }
  }
  if (lhsInfoPtr && rhsInfoPtr && lhsInfoPtr != rhsInfoPtr) {
    validateSmartPointerTransfer(*lhsInfoPtr, *rhsInfoPtr, "assignment");
  }
}

void LifetimeAnalyzer::emitEvent(const std::string &symbol, LifetimePlan &plan,
                                 LifetimeEvent::Kind kind,
                                 const std::string &note) {
  auto &entry = ensureVariable(symbol, nullptr, plan);
  if (!shouldTrack(entry))
    return;
  LifetimeEvent event;
  event.kind = kind;
  event.symbol = symbol;
  event.note = note;
  plan.globalEvents.push_back(event);
  entry.events.push_back(event);
}

VariableLifetimePlan &LifetimeAnalyzer::ensureVariable(
    const std::string &symbol,
    const TypeInfo *type,
    LifetimePlan &plan) {
  auto it = plan.variables.find(symbol);
  if (it == plan.variables.end()) {
    VariableLifetimePlan info;
    info.name = symbol;
    if (type)
      info.type = *type;
    auto [inserted, _] = plan.variables.emplace(symbol, std::move(info));
    return inserted->second;
  }
  if (type)
    it->second.type = *type;
  return it->second;
}

void LifetimeAnalyzer::enterBlock() {
  blockVariableStack.emplace_back();
}

void LifetimeAnalyzer::exitBlock(LifetimePlan &plan) {
  if (blockVariableStack.empty())
    return;
  auto locals = std::move(blockVariableStack.back());
  blockVariableStack.pop_back();
  for (auto it = locals.rbegin(); it != locals.rend(); ++it) {
    auto varIt = plan.variables.find(*it);
    if (varIt == plan.variables.end())
      continue;
    releaseSymbol(varIt->second, plan, "end of scope");
  }
}

void LifetimeAnalyzer::releaseSymbol(VariableLifetimePlan &var,
                                     LifetimePlan &plan,
                                     const std::string &note) {
  if (!shouldTrack(var))
    return;
  if (var.escapes)
    return;
  emitEvent(var.name, plan, LifetimeEvent::Kind::Release, note);
}

bool LifetimeAnalyzer::shouldTrack(const VariableLifetimePlan &var) const {
  if (!var.type.participatesInARC())
    return false;
  if (var.type.ownership == OwnershipQualifier::Weak ||
      var.type.ownership == OwnershipQualifier::Unowned)
    return false;
  if (var.type.smartPointerKind == SmartPointerKind::Weak)
    return false;
  return pointerSemantics.shouldRelease(var.type);
}

void LifetimeAnalyzer::markEscape(const std::string &symbol,
                                  const std::string &reason,
                                  LifetimePlan &plan) {
  auto &entry = ensureVariable(symbol, nullptr, plan);
  entry.escapes = true;
  emitEvent(symbol, plan, LifetimeEvent::Kind::Escape, reason);
}

void LifetimeAnalyzer::finalizePlan(LifetimePlan &plan) {
  while (!blockVariableStack.empty())
    exitBlock(plan);
  for (auto &[name, var] : plan.variables) {
    if (!var.isParameter)
      continue;
    if (!shouldTrack(var))
      continue;
    if (!var.escapes)
      emitEvent(name, plan, LifetimeEvent::Kind::Release,
                "parameter scope end");
  }
  pendingBlockVariables.clear();
}

void LifetimeAnalyzer::visitVariableExpr(VariableExprAST &varExpr,
                                         LifetimePlan &plan) {
  auto &entry = ensureVariable(varExpr.getName(), nullptr, plan);
  if (isUniqueMoved(entry)) {
    reportCompilerError("unique pointer '" + entry.name +
                        "' was moved and cannot be used until reassigned");
  }
}

void LifetimeAnalyzer::queueBlockVariable(const std::string &name,
                                          const TypeInfo &type) {
  pendingBlockVariables.emplace_back();
  pendingBlockVariables.back().emplace_back(name, type);
}

void LifetimeAnalyzer::injectPendingBlockVariables(LifetimePlan &plan) {
  if (pendingBlockVariables.empty())
    return;
  auto pending = std::move(pendingBlockVariables.back());
  pendingBlockVariables.pop_back();
  if (blockVariableStack.empty())
    blockVariableStack.emplace_back();
  for (auto &entry : pending) {
    auto &var = ensureVariable(entry.first, &entry.second, plan);
    var.needsZeroing = pointerSemantics.allowsZeroing(var.type);
    blockVariableStack.back().push_back(var.name);
  }
}

void LifetimeAnalyzer::validateSmartPointerTransfer(
    VariableLifetimePlan &target, VariableLifetimePlan &source,
    const std::string &context) {
  if (!source.type.isSmartPointer())
    return;
  if (target.type.isSmartPointer()) {
    if (target.type.smartPointerKind != source.type.smartPointerKind) {
      reportCompilerError("Cannot assign " + source.type.typeName + " to " +
                          target.type.typeName + " during " + context);
    }
    return;
  }
  if (!target.type.participatesInARC())
    return;
  if (target.type.ownership == OwnershipQualifier::Weak ||
      target.type.ownership == OwnershipQualifier::Unowned)
    return;
  reportCompilerError("Cannot assign smart pointer '" + source.name +
                      "' to raw ARC-managed variable '" + target.name +
                      "' during " + context,
                      "Use explicit smart pointer helpers or unwrap the value.");
}

void LifetimeAnalyzer::markUniqueMoved(VariableLifetimePlan &var) {
  if (!pointerSemantics.requiresMoveOnAssign(var.type))
    return;
  var.uniqueState = VariableLifetimePlan::UniqueState::Moved;
}

void LifetimeAnalyzer::markUniqueReinitialized(VariableLifetimePlan &var) {
  if (!pointerSemantics.requiresMoveOnAssign(var.type))
    return;
  var.uniqueState = VariableLifetimePlan::UniqueState::Alive;
}

bool LifetimeAnalyzer::isUniqueMoved(const VariableLifetimePlan &var) const {
  if (!pointerSemantics.requiresMoveOnAssign(var.type))
    return false;
  return var.uniqueState == VariableLifetimePlan::UniqueState::Moved;
}

} // namespace analysis
