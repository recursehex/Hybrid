#include "ast.h"
#include <iostream>

// LLVM includes for code generation
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <map>
#include <cmath>
#include <climits>

// Global LLVM objects
static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::Value *> NamedValues;

// Initialize LLVM
void InitializeModule() {
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("Hybrid JIT", *TheContext);
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
  
  // Add a simple print function that takes an int
  std::vector<llvm::Type*> PrintArgs = {llvm::Type::getInt32Ty(*TheContext)};
  llvm::FunctionType *PrintType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), PrintArgs, false);
  llvm::Function::Create(PrintType, llvm::Function::ExternalLinkage,
                        "print", TheModule.get());
}

// Get the LLVM module for printing
llvm::Module *getModule() {
  return TheModule.get();
}

// Error handling utilities
llvm::Value *LogErrorV(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

llvm::Function *LogErrorF(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

// Type conversion helper
llvm::Type *getTypeFromString(const std::string &TypeStr) {
  if (TypeStr == "int")
    return llvm::Type::getInt32Ty(*TheContext);
  else if (TypeStr == "float")
    return llvm::Type::getFloatTy(*TheContext);
  else if (TypeStr == "double")
    return llvm::Type::getDoubleTy(*TheContext);
  else if (TypeStr == "char")
    return llvm::Type::getInt8Ty(*TheContext);
  else if (TypeStr == "bool")
    return llvm::Type::getInt1Ty(*TheContext);
  else if (TypeStr == "void")
    return llvm::Type::getVoidTy(*TheContext);
  else if (TypeStr == "string")
    return llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0); // Strings as char*
  return nullptr;
}

// AST implementations (currently all inline in the header)

// Add print method for debugging
void ForEachStmtAST::print() const {
  std::cout << "Parsed a foreach loop: for " << Type << " " << VarName << " in <expression>" << std::endl;
}

//===----------------------------------------------------------------------===//
// Expression Code Generation
//===----------------------------------------------------------------------===//

llvm::Value *NumberExprAST::codegen() {
  // Check if the number is a whole number that could be an integer
  double val = getValue();
  if (val == floor(val) && val >= INT32_MIN && val <= INT32_MAX) {
    // It's a whole number in int32 range, create as integer
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, (int32_t)val));
  } else {
    // It's a floating point number
    return llvm::ConstantFP::get(*TheContext, llvm::APFloat(val));
  }
}

llvm::Value *BoolExprAST::codegen() {
  return llvm::ConstantInt::get(*TheContext, llvm::APInt(1, getValue() ? 1 : 0));
}

llvm::Value *NullExprAST::codegen() {
  // Create a null pointer for string type (char*)
  llvm::Type *StringType = llvm::PointerType::get(llvm::Type::getInt8Ty(*TheContext), 0);
  return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(StringType));
}

llvm::Value *StringExprAST::codegen() {
  // Create a global string constant
  return Builder->CreateGlobalString(getValue(), "str");
}

llvm::Value *CharExprAST::codegen() {
  return llvm::ConstantInt::get(*TheContext, llvm::APInt(8, getValue()));
}

llvm::Value *VariableExprAST::codegen() {
  // Look this variable up in the function
  llvm::Value *V = NamedValues[getName()];
  if (!V)
    return LogErrorV("Unknown variable name");
  // Load the value - V is an AllocaInst*, we need to get the allocated type
  llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
  return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
}

// Helper function to cast a value to a target type
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType) {
  llvm::Type* sourceType = value->getType();
  
  if (sourceType == targetType)
    return value;
  
  // Integer to float
  if (sourceType->isIntegerTy() && targetType->isFloatingPointTy()) {
    if (sourceType->isIntegerTy(1)) // bool
      return Builder->CreateUIToFP(value, targetType, "casttmp");
    else
      return Builder->CreateSIToFP(value, targetType, "casttmp");
  }
  
  // Float to integer
  if (sourceType->isFloatingPointTy() && targetType->isIntegerTy()) {
    if (targetType->isIntegerTy(1)) // bool
      return Builder->CreateFPToUI(value, targetType, "casttmp");
    else
      return Builder->CreateFPToSI(value, targetType, "casttmp");
  }
  
  // Integer to integer (different sizes)
  if (sourceType->isIntegerTy() && targetType->isIntegerTy()) {
    unsigned sourceBits = sourceType->getIntegerBitWidth();
    unsigned targetBits = targetType->getIntegerBitWidth();
    
    if (sourceBits < targetBits) {
      if (sourceType->isIntegerTy(1)) // bool
        return Builder->CreateZExt(value, targetType, "casttmp");
      else
        return Builder->CreateSExt(value, targetType, "casttmp");
    } else if (sourceBits > targetBits) {
      return Builder->CreateTrunc(value, targetType, "casttmp");
    }
  }
  
  // If we can't cast, return the original value (shouldn't happen in well-typed programs)
  return value;
}

// Helper function to promote types for binary operations
std::pair<llvm::Value*, llvm::Value*> promoteTypes(llvm::Value* L, llvm::Value* R) {
  llvm::Type* LType = L->getType();
  llvm::Type* RType = R->getType();
  
  // If both are the same type, no promotion needed
  if (LType == RType)
    return {L, R};
  
  // If one is float and other is int, promote int to float
  if (LType->isFloatingPointTy() && RType->isIntegerTy()) {
    if (RType->isIntegerTy(1)) // bool to float
      R = Builder->CreateUIToFP(R, LType, "promtmp");
    else // int to float
      R = Builder->CreateSIToFP(R, LType, "promtmp");
    return {L, R};
  }
  
  if (LType->isIntegerTy() && RType->isFloatingPointTy()) {
    if (LType->isIntegerTy(1)) // bool to float
      L = Builder->CreateUIToFP(L, RType, "promtmp");
    else // int to float
      L = Builder->CreateSIToFP(L, RType, "promtmp");
    return {L, R};
  }
  
  // If both are integers but different sizes, promote to larger
  if (LType->isIntegerTy() && RType->isIntegerTy()) {
    if (LType->getIntegerBitWidth() < RType->getIntegerBitWidth()) {
      if (LType->isIntegerTy(1)) // bool
        L = Builder->CreateZExt(L, RType, "promtmp");
      else
        L = Builder->CreateSExt(L, RType, "promtmp");
    } else if (RType->getIntegerBitWidth() < LType->getIntegerBitWidth()) {
      if (RType->isIntegerTy(1)) // bool
        R = Builder->CreateZExt(R, LType, "promtmp");
      else
        R = Builder->CreateSExt(R, LType, "promtmp");
    }
  }
  
  return {L, R};
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = getLHS()->codegen();
  llvm::Value *R = getRHS()->codegen();
  if (!L || !R)
    return nullptr;

  // Promote types to compatible types
  auto promoted = promoteTypes(L, R);
  L = promoted.first;
  R = promoted.second;

  // Check if we're working with floating point or integer types
  bool isFloat = L->getType()->isFloatingPointTy();

  // Handle single-character operators
  if (Op.length() == 1) {
    switch (Op[0]) {
    case '+':
      if (isFloat)
        return Builder->CreateFAdd(L, R, "addtmp");
      else
        return Builder->CreateAdd(L, R, "addtmp");
    case '-':
      if (isFloat)
        return Builder->CreateFSub(L, R, "subtmp");
      else
        return Builder->CreateSub(L, R, "subtmp");
    case '*':
      if (isFloat)
        return Builder->CreateFMul(L, R, "multmp");
      else
        return Builder->CreateMul(L, R, "multmp");
    case '<':
      if (isFloat) {
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
      } else {
        L = Builder->CreateICmpSLT(L, R, "cmptmp");
      }
      // Return the boolean result as i1
      return L;
    case '>':
      if (isFloat) {
        L = Builder->CreateFCmpUGT(L, R, "cmptmp");
      } else {
        L = Builder->CreateICmpSGT(L, R, "cmptmp");
      }
      // Return the boolean result as i1
      return L;
    case '=':
      // Assignment operator - LHS must be a variable
      VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS());
      if (!LHSE)
        return LogErrorV("destination of '=' must be a variable");
      
      // Look up the variable in the symbol table
      llvm::Value *Variable = NamedValues[LHSE->getName()];
      if (!Variable)
        return LogErrorV("Unknown variable name");
      
      // Cast RHS to the variable's type
      llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(Variable);
      llvm::Type *VarType = Alloca->getAllocatedType();
      R = castToType(R, VarType);
      
      // Store the value
      Builder->CreateStore(R, Variable);
      return R;
    }
  }
  
  // Handle multi-character operators
  if (Op == "==") {
    if (isFloat)
      return Builder->CreateFCmpOEQ(L, R, "eqtmp");
    else
      return Builder->CreateICmpEQ(L, R, "eqtmp");
  } else if (Op == "!=") {
    if (isFloat)
      return Builder->CreateFCmpONE(L, R, "netmp");
    else
      return Builder->CreateICmpNE(L, R, "netmp");
  } else if (Op == "<=") {
    if (isFloat)
      return Builder->CreateFCmpOLE(L, R, "letmp");
    else
      return Builder->CreateICmpSLE(L, R, "letmp");
  } else if (Op == ">=") {
    if (isFloat)
      return Builder->CreateFCmpOGE(L, R, "getmp");
    else
      return Builder->CreateICmpSGE(L, R, "getmp");
  } else if (Op == "&&") {
    // Convert operands to booleans if needed
    if (!L->getType()->isIntegerTy(1)) {
      if (L->getType()->isFloatingPointTy())
        L = Builder->CreateFCmpONE(L, llvm::ConstantFP::get(L->getType(), 0.0), "tobool");
      else if (L->getType()->isIntegerTy())
        L = Builder->CreateICmpNE(L, llvm::ConstantInt::get(L->getType(), 0), "tobool");
    }
    if (!R->getType()->isIntegerTy(1)) {
      if (R->getType()->isFloatingPointTy())
        R = Builder->CreateFCmpONE(R, llvm::ConstantFP::get(R->getType(), 0.0), "tobool");
      else if (R->getType()->isIntegerTy())
        R = Builder->CreateICmpNE(R, llvm::ConstantInt::get(R->getType(), 0), "tobool");
    }
    return Builder->CreateAnd(L, R, "andtmp");
  } else if (Op == "||") {
    // Convert operands to booleans if needed
    if (!L->getType()->isIntegerTy(1)) {
      if (L->getType()->isFloatingPointTy())
        L = Builder->CreateFCmpONE(L, llvm::ConstantFP::get(L->getType(), 0.0), "tobool");
      else if (L->getType()->isIntegerTy())
        L = Builder->CreateICmpNE(L, llvm::ConstantInt::get(L->getType(), 0), "tobool");
    }
    if (!R->getType()->isIntegerTy(1)) {
      if (R->getType()->isFloatingPointTy())
        R = Builder->CreateFCmpONE(R, llvm::ConstantFP::get(R->getType(), 0.0), "tobool");
      else if (R->getType()->isIntegerTy())
        R = Builder->CreateICmpNE(R, llvm::ConstantInt::get(R->getType(), 0), "tobool");
    }
    return Builder->CreateOr(L, R, "ortmp");
  }
  
  return LogErrorV("invalid binary operator");
}

llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table
  llvm::Function *CalleeF = TheModule->getFunction(getCallee());
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error
  if (CalleeF->arg_size() != getArgs().size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<llvm::Value *> ArgsV;
  unsigned i = 0;
  for (auto &Arg : CalleeF->args()) {
    llvm::Value *ArgVal = getArgs()[i]->codegen();
    if (!ArgVal)
      return nullptr;
    
    // Cast the argument to the expected parameter type
    ArgVal = castToType(ArgVal, Arg.getType());
    ArgsV.push_back(ArgVal);
    ++i;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

//===----------------------------------------------------------------------===//
// Statement Code Generation
//===----------------------------------------------------------------------===//

llvm::Value *ReturnStmtAST::codegen() {
  if (getReturnValue()) {
    llvm::Value *Val = getReturnValue()->codegen();
    if (!Val)
      return nullptr;
    Builder->CreateRet(Val);
  } else {
    // Void return
    Builder->CreateRetVoid();
  }
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

llvm::Value *BlockStmtAST::codegen() {
  llvm::Value *Last = nullptr;
  for (auto &Stmt : getStatements()) {
    Last = Stmt->codegen();
    if (!Last)
      return nullptr;
  }
  return Last ? Last : llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

llvm::Value *VariableDeclarationStmtAST::codegen() {
  // Get the type
  llvm::Type *VarType = getTypeFromString(getType());
  if (!VarType)
    return LogErrorV("Unknown type name");
  
  // Generate the initializer
  llvm::Value *InitVal = nullptr;
  if (getInitializer()) {
    InitVal = getInitializer()->codegen();
    if (!InitVal)
      return nullptr;
    
    // Cast the initializer to the variable type if needed
    InitVal = castToType(InitVal, VarType);
  }
  
  // Create an alloca for this variable
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::AllocaInst *Alloca = Builder->CreateAlloca(VarType, nullptr, getName());
  
  // Store the initial value
  if (InitVal) {
    Builder->CreateStore(InitVal, Alloca);
  }
  
  // Remember this binding
  NamedValues[getName()] = Alloca;
  
  return Alloca;
}

llvm::Value *ExpressionStmtAST::codegen() {
  // Just generate code for the expression
  return getExpression()->codegen();
}

llvm::Value *ForEachStmtAST::codegen() {
  // TODO: Implement foreach loop code generation
  // This is complex and would require runtime support for collections
  return LogErrorV("foreach loops not yet implemented in code generation");
}

llvm::Value *IfStmtAST::codegen() {
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  // Convert condition to a bool
  if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV, 
        llvm::ConstantFP::get(CondV->getType(), 0.0), "ifcond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV, 
        llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
    } else {
      return LogErrorV("Condition must be a numeric type");
    }
  }
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the then and else cases. Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");
  
  Builder->CreateCondBr(CondV, ThenBB, ElseBB);
  
  // Emit then value.
  Builder->SetInsertPoint(ThenBB);
  
  llvm::Value *ThenV = getThenBranch()->codegen();
  if (!ThenV)
    return nullptr;
  
  // Check if the block already has a terminator (e.g., return statement)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(MergeBB);
  
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();
  
  // Emit else block.
  ElseBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ElseBB);
  
  llvm::Value *ElseV = nullptr;
  if (getElseBranch()) {
    ElseV = getElseBranch()->codegen();
    if (!ElseV)
      return nullptr;
  }
  
  // Check if the block already has a terminator
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(MergeBB);
  
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();
  
  // Emit merge block.
  MergeBB->insertInto(TheFunction);
  Builder->SetInsertPoint(MergeBB);
  
  // Return the last value (this is somewhat arbitrary for statements)
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *WhileStmtAST::codegen() {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop condition, body, and exit
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "whilecond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "whilebody");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "whilecont");
  
  // Jump to the condition block
  Builder->CreateBr(CondBB);
  
  // Emit the condition block
  Builder->SetInsertPoint(CondBB);
  
  // Evaluate the condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  // Convert condition to a bool
  if (!CondV->getType()->isIntegerTy(1)) {
    if (CondV->getType()->isFloatingPointTy()) {
      // For floating point, compare with 0.0
      CondV = Builder->CreateFCmpONE(CondV, 
        llvm::ConstantFP::get(CondV->getType(), 0.0), "whilecond");
    } else if (CondV->getType()->isIntegerTy()) {
      // For integers, compare with 0
      CondV = Builder->CreateICmpNE(CondV, 
        llvm::ConstantInt::get(CondV->getType(), 0), "whilecond");
    } else {
      return LogErrorV("Condition must be a numeric type");
    }
  }
  
  // Create the conditional branch
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  
  // Generate code for the body
  llvm::Value *BodyV = getBody()->codegen();
  if (!BodyV)
    return nullptr;
  
  // After the body, branch back to the condition check
  // (unless the body contains a break/return)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(CondBB);
  
  // Emit the after-loop block
  AfterBB->insertInto(TheFunction);
  Builder->SetInsertPoint(AfterBB);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *UseStmtAST::codegen() {
  // TODO: Implement module importing
  // This would require linking external modules
  return LogErrorV("use statements not yet implemented in code generation");
}

//===----------------------------------------------------------------------===//
// Function and Prototype Code Generation
//===----------------------------------------------------------------------===//

llvm::Function *PrototypeAST::codegen() {
  // Convert parameter types
  std::vector<llvm::Type*> ParamTypes;
  for (const auto &Param : Args) {
    llvm::Type *ParamType = getTypeFromString(Param.Type);
    if (!ParamType)
      return LogErrorF("Unknown parameter type");
    ParamTypes.push_back(ParamType);
  }
  
  // Get return type
  llvm::Type *RetType = getTypeFromString(ReturnType);
  if (!RetType)
    return LogErrorF("Unknown return type");
  
  // Create the function type
  llvm::FunctionType *FT = llvm::FunctionType::get(RetType, ParamTypes, false);
  
  // Create the function
  llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
  
  // Set names for all arguments
  unsigned Idx = 0;
  for (auto &Arg : F->args())
    Arg.setName(Args[Idx++].Name);
  
  return F;
}

llvm::Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration
  llvm::Function *TheFunction = TheModule->getFunction(getProto()->getName());
  
  if (!TheFunction)
    TheFunction = getProto()->codegen();
  
  if (!TheFunction)
    return nullptr;
  
  // Create a new basic block to start insertion into
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);
  
  // Record the function arguments in the NamedValues map
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable
    llvm::AllocaInst *Alloca = Builder->CreateAlloca(Arg.getType(), nullptr, Arg.getName());
    
    // Store the initial value into the alloca
    Builder->CreateStore(&Arg, Alloca);
    
    // Add arguments to variable symbol table
    NamedValues[std::string(Arg.getName())] = Alloca;
  }
  
  // Generate code for the function body
  if (getBody()->codegen()) {
    // Finish off the function if no explicit return
    if (!Builder->GetInsertBlock()->getTerminator()) {
      if (TheFunction->getReturnType()->isVoidTy()) {
        Builder->CreateRetVoid();
      } else {
        // For non-void functions, return a default value
        Builder->CreateRet(llvm::Constant::getNullValue(TheFunction->getReturnType()));
      }
    }
    
    // Validate the generated code, checking for consistency
    llvm::verifyFunction(*TheFunction);
    
    return TheFunction;
  }
  
  // Error reading body, remove function
  TheFunction->eraseFromParent();
  return nullptr;
}