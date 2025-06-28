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

  switch (getOp()) {
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
  default:
    return LogErrorV("invalid binary operator");
  }
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