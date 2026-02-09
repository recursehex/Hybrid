#include "ast_internal.h"

// Generate code for switch expressions
llvm::Value *SwitchExprAST::codegen() {
  if (builderInTopLevelContext())
    prepareTopLevelStatementContext();

  // Evaluate the switch condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create basic blocks for each case and the exit block
  llvm::BasicBlock *ExitBB = llvm::BasicBlock::Create(*TheContext, "switch_expr_end");
  
  // Create a PHI node to collect the result values
  // We'll determine the result type from the first case
  llvm::Type *ResultType = nullptr;
  std::vector<std::pair<llvm::BasicBlock*, const CaseAST*>> CaseBlocks;
  
  // First pass: create basic blocks and determine result type
  for (const auto &Case : getCases()) {
    llvm::BasicBlock *CaseBB = llvm::BasicBlock::Create(*TheContext, 
        Case->isDefault() ? "expr_default" : "expr_case", TheFunction);
    CaseBlocks.push_back({CaseBB, Case.get()});
  }
  
  // Create switch instruction
  llvm::BasicBlock *DefaultBB = nullptr;
  for (size_t i = 0; i < getCases().size(); ++i) {
    if (getCases()[i]->isDefault()) {
      DefaultBB = CaseBlocks[i].first;
      break;
    }
  }
  
  if (!DefaultBB) {
    return LogErrorV("Switch expressions must have a default case");
  }
  
  llvm::SwitchInst *Switch = Builder->CreateSwitch(CondV, DefaultBB, getCases().size());
  
  // Add case values to switch instruction
  for (size_t i = 0; i < getCases().size(); ++i) {
    const auto &Case = getCases()[i];
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    
    if (!Case->isDefault()) {
      // Add each case value
      for (const auto &Value : Case->getValues()) {
        llvm::Value *CaseVal = Value->codegen();
        if (!CaseVal)
          return nullptr;
        
        // Convert to constant integer for switch
        if (auto *CI = llvm::dyn_cast<llvm::ConstantInt>(CaseVal)) {
          Switch->addCase(CI, CaseBB);
        } else {
          return LogErrorV("Case values must be compile-time constants");
        }
      }
    }
  }
  
  // Generate code for each case expression and collect results
  std::vector<std::pair<llvm::Value*, llvm::BasicBlock*>> PhiValues;
  
  for (size_t i = 0; i < CaseBlocks.size(); ++i) {
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    const CaseAST *Case = CaseBlocks[i].second;
    
    Builder->SetInsertPoint(CaseBB);
    
    // Generate case expression
    llvm::Value *CaseResult = Case->getExpression()->codegen();
    if (!CaseResult)
      return nullptr;
    
    // Determine result type from first case
    if (ResultType == nullptr) {
      ResultType = CaseResult->getType();
      
      // Set the expression's type name based on result type
      if (ResultType->isIntegerTy(32)) {
        setTypeName("int");
      } else if (ResultType->isDoubleTy()) {
        setTypeName("double");
      } else if (ResultType->isFloatTy()) {
        setTypeName("float");
      } else if (ResultType->isIntegerTy(8)) {
        setTypeName("bool");
      } else if (ResultType->isPointerTy()) {
        setTypeName("string");
      }
    }
    
    // Type-cast case result to match result type if needed
    if (CaseResult->getType() != ResultType) {
      // Simple type promotions for switch expressions
      if (ResultType->isDoubleTy() && CaseResult->getType()->isIntegerTy(32)) {
        CaseResult = Builder->CreateSIToFP(CaseResult, ResultType, "int_to_double");
      } else if (ResultType->isFloatTy() && CaseResult->getType()->isIntegerTy(32)) {
        CaseResult = Builder->CreateSIToFP(CaseResult, ResultType, "int_to_float");
      } else if (ResultType != CaseResult->getType()) {
        return LogErrorV("All switch expression cases must return the same type");
      }
    }

    llvm::BasicBlock *completedCaseBB = Builder->GetInsertBlock();
    PhiValues.push_back({CaseResult, completedCaseBB});

    if (!completedCaseBB->getTerminator())
      Builder->CreateBr(ExitBB);
  }
  
  // Add exit block and create PHI node
  ExitBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ExitBB);
  
  llvm::PHINode *Result = Builder->CreatePHI(ResultType, PhiValues.size(), "switch_result");
  for (const auto &PhiVal : PhiValues) {
    Result->addIncoming(PhiVal.first, PhiVal.second);
  }
  
  return Result;
}

// Generate code for ternary expressions (a if b else c)
llvm::Value *TernaryExprAST::codegen() {
  // Evaluate the condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  ExprAST *ThenExprNode = getThenExpr();
  ExprAST *ElseExprNode = getElseExpr();

  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV,
      llvm::ConstantInt::get(CondV->getType(), 0), "ternarycond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV,
        llvm::ConstantFP::get(CondV->getType(), 0.0), "ternarycond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV,
        llvm::ConstantInt::get(CondV->getType(), 0), "ternarycond");
    } else {
      return LogErrorV("Ternary condition must be numeric");
    }
  }

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else branches, and merge block
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "ternary_then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "ternary_else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ternary_merge");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then expression
  Builder->SetInsertPoint(ThenBB);
  llvm::Value *ThenV = getThenExpr()->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI
  ThenBB = Builder->GetInsertBlock();

  // Emit else expression
  ElseBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ElseBB);
  llvm::Value *ElseV = getElseExpr()->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block
  MergeBB->insertInto(TheFunction);
  Builder->SetInsertPoint(MergeBB);

  // Determine result type and handle type promotion
  llvm::Type *ResultType = ThenV->getType();
  std::string thenTypeName = ThenExprNode ? ThenExprNode->getTypeName() : "";
  std::string elseTypeName = ElseExprNode ? ElseExprNode->getTypeName() : "";

  // Type-cast expressions to match if needed
  if (ThenV->getType() != ElseV->getType()) {
    // Handle type promotion for numeric types
    if (ThenV->getType()->isDoubleTy() && ElseV->getType()->isIntegerTy(32)) {
      ElseV = Builder->CreateSIToFP(ElseV, ThenV->getType(), "int_to_double");
      ResultType = ThenV->getType();
    } else if (ElseV->getType()->isDoubleTy() && ThenV->getType()->isIntegerTy(32)) {
      ThenV = Builder->CreateSIToFP(ThenV, ElseV->getType(), "int_to_double");
      ResultType = ElseV->getType();
    } else if (ThenV->getType()->isFloatTy() && ElseV->getType()->isIntegerTy(32)) {
      ElseV = Builder->CreateSIToFP(ElseV, ThenV->getType(), "int_to_float");
      ResultType = ThenV->getType();
    } else if (ElseV->getType()->isFloatTy() && ThenV->getType()->isIntegerTy(32)) {
      ThenV = Builder->CreateSIToFP(ThenV, ElseV->getType(), "int_to_float");
      ResultType = ElseV->getType();
    } else if (ResultType->isStructTy() && ElseV->getType()->isPointerTy() &&
               llvm::isa<llvm::ConstantPointerNull>(ElseV)) {
      ElseV = llvm::ConstantAggregateZero::get(llvm::cast<llvm::StructType>(ResultType));
    } else if (ElseV->getType()->isStructTy() && ThenV->getType()->isPointerTy() &&
               llvm::isa<llvm::ConstantPointerNull>(ThenV)) {
      llvm::StructType *StructTy = llvm::cast<llvm::StructType>(ElseV->getType());
      ThenV = llvm::ConstantAggregateZero::get(StructTy);
      ResultType = ElseV->getType();
    } else {
      return LogErrorV("Branches must have compatible types");
    }
  }

  auto pickMeaningfulTypeName = [](const std::string &name) -> std::string {
    if (name.empty() || name == "null" || name == "void")
      return {};
    return name;
  };

  std::string resultTypeName = pickMeaningfulTypeName(thenTypeName);
  if (resultTypeName.empty())
    resultTypeName = pickMeaningfulTypeName(elseTypeName);
  if (resultTypeName.empty())
    resultTypeName = !thenTypeName.empty() ? thenTypeName : elseTypeName;

  const ExprAST *ThenExprForNullability = unwrapRefExpr(ThenExprNode);
  const ExprAST *ElseExprForNullability = unwrapRefExpr(ElseExprNode);
  bool thenNullable = expressionIsNullable(ThenExprForNullability);
  bool elseNullable = expressionIsNullable(ElseExprForNullability);

  if (resultTypeName.empty()) {
    if (ResultType->isIntegerTy(32))
      resultTypeName = "int";
    else if (ResultType->isDoubleTy())
      resultTypeName = "double";
    else if (ResultType->isFloatTy())
      resultTypeName = "float";
    else if (ResultType->isIntegerTy(8))
      resultTypeName = "bool";
  }

  if (!resultTypeName.empty() && (thenNullable || elseNullable))
    resultTypeName = ensureOuterNullable(resultTypeName);

  if (!resultTypeName.empty())
    setTypeName(resultTypeName);

  // Create PHI node to merge the results
  llvm::PHINode *Result = Builder->CreatePHI(ResultType, 2, "ternary_result");
  Result->addIncoming(ThenV, ThenBB);
  Result->addIncoming(ElseV, ElseBB);

  return Result;
}

#undef LoopContinueBlocks
#undef LoopExitBlocks
#undef StructFieldTypes
#undef StructFieldIndices
#undef ArraySizes
#undef StructTypes
#undef LocalTypes
#undef GlobalTypes
#undef GlobalValues
#undef NamedValues
#undef Builder
#undef TheModule
#undef TheContext
#undef CG
