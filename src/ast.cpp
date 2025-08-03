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
#include "llvm/Support/Casting.h"

#include <map>
#include <cmath>
#include <climits>

// Global LLVM objects
static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::Value *> NamedValues;
static std::map<std::string, llvm::GlobalVariable *> GlobalValues;
static std::map<std::string, std::string> GlobalTypes; // Track types of globals
static std::map<std::string, std::string> LocalTypes;  // Track types of locals

// Stack to track loop exit blocks for break statements
static std::vector<llvm::BasicBlock *> LoopExitBlocks;

// Stack to track loop continue blocks for skip statements
static std::vector<llvm::BasicBlock *> LoopContinueBlocks;

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
  // New sized integer types
  else if (TypeStr == "byte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit unsigned
  else if (TypeStr == "sbyte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit signed
  else if (TypeStr == "short")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit signed
  else if (TypeStr == "ushort")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit unsigned
  else if (TypeStr == "uint")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit unsigned
  else if (TypeStr == "long")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit signed
  else if (TypeStr == "ulong")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit unsigned
  // Sized character types
  else if (TypeStr == "schar")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit character
  else if (TypeStr == "lchar")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit character (Unicode)
  else if (TypeStr.size() > 2 && TypeStr.substr(TypeStr.size() - 2) == "[]") {
    // Array type: return the array struct type {ptr, size}
    std::string ElementType = TypeStr.substr(0, TypeStr.size() - 2);
    llvm::Type *ElemType = getTypeFromString(ElementType);
    if (ElemType) {
      // Create struct type for array: { element_ptr, size }
      llvm::Type *PtrType = llvm::PointerType::get(ElemType, 0);
      llvm::Type *SizeType = llvm::Type::getInt32Ty(*TheContext);
      return llvm::StructType::get(*TheContext, {PtrType, SizeType});
    }
    return nullptr;
  }
  return nullptr;
}

// Helper to get array struct type for a given element type
llvm::StructType *getArrayStructType(llvm::Type *ElementType) {
  llvm::Type *PtrType = llvm::PointerType::get(ElementType, 0);
  llvm::Type *SizeType = llvm::Type::getInt32Ty(*TheContext);
  return llvm::StructType::get(*TheContext, {PtrType, SizeType});
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

// Forward declaration for castToType
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType);

// Helper to check if types are compatible for implicit conversion
// No implicit conversion between different sized integers
bool areTypesCompatible(llvm::Type* type1, llvm::Type* type2) {
  if (type1 == type2)
    return true;
  
  // Bool is its own type - no implicit conversions
  if (type1->isIntegerTy(1) || type2->isIntegerTy(1))
    return false;
  
  // Allow implicit conversion between integer and float types
  if ((type1->isIntegerTy() && type2->isFloatingPointTy()) ||
      (type1->isFloatingPointTy() && type2->isIntegerTy()))
    return true;
  
  // Allow implicit conversion between float types
  if (type1->isFloatingPointTy() && type2->isFloatingPointTy())
    return true;
  
  return false;
}

llvm::Value *ArrayExprAST::codegen() {
  // Get the element type
  llvm::Type *ElemType = getTypeFromString(getElementType());
  if (!ElemType)
    return LogErrorV("Unknown element type in array literal");
  
  // Get the array size
  size_t ArraySize = getElements().size();
  
  // Allocate space for the array on the stack
  llvm::AllocaInst *ArrayAlloca = Builder->CreateAlloca(
      ElemType, llvm::ConstantInt::get(*TheContext, llvm::APInt(32, ArraySize)), "arraytmp");
  
  // Store each element
  for (size_t i = 0; i < ArraySize; ++i) {
    llvm::Value *ElemVal = getElements()[i]->codegen();
    if (!ElemVal)
      return nullptr;
    
    // Cast element to the correct type if needed
    ElemVal = castToType(ElemVal, ElemType);
    
    // Calculate the address of the i-th element
    llvm::Value *Idx = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, i));
    llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayAlloca, Idx, "elemptr");
    
    // Store the element
    Builder->CreateStore(ElemVal, ElemPtr);
  }
  
  // Create the array struct {ptr, size}
  llvm::StructType *ArrayStructType = getArrayStructType(ElemType);
  llvm::AllocaInst *ArrayStruct = Builder->CreateAlloca(ArrayStructType, nullptr, "arrayStruct");
  
  // Store the pointer (field 0)
  llvm::Value *PtrField = Builder->CreateStructGEP(ArrayStructType, ArrayStruct, 0, "ptrField");
  Builder->CreateStore(ArrayAlloca, PtrField);
  
  // Store the size (field 1)
  llvm::Value *SizeField = Builder->CreateStructGEP(ArrayStructType, ArrayStruct, 1, "sizeField");
  llvm::Value *SizeVal = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, ArraySize));
  Builder->CreateStore(SizeVal, SizeField);
  
  // Return the struct
  return Builder->CreateLoad(ArrayStructType, ArrayStruct, "arrayStructVal");
}

llvm::Value *ArrayIndexExprAST::codegen() {
  // Generate code for the array expression
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;
  
  // Generate code for the index
  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;
  
  // Make sure index is an integer
  if (!IndexVal->getType()->isIntegerTy(32)) {
    // Cast to i32 if it's not already
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }
  
  // Check if ArrayVal is a struct (new array representation)
  llvm::Type *ArrayType = ArrayVal->getType();
  llvm::Value *ArrayPtr = nullptr;
  llvm::Type *ElemType = nullptr;
  
  if (ArrayType->isStructTy()) {
    // New array representation: extract pointer from struct
    llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
    if (StructType->getNumElements() == 2) {
      // Extract the pointer (field 0)
      ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
      
      // Determine element type from the context
      // First, check if it's from a direct array literal
      if (ArrayExprAST *ArrayExpr = dynamic_cast<ArrayExprAST*>(getArray())) {
        ElemType = getTypeFromString(ArrayExpr->getElementType());
      }
      // Otherwise, check if it's from a variable
      else if (VariableExprAST *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
        std::string varName = VarExpr->getName();
        
        // Check local types first, then global types
        std::string typeStr;
        if (LocalTypes.count(varName)) {
          typeStr = LocalTypes[varName];
        } else if (GlobalTypes.count(varName)) {
          typeStr = GlobalTypes[varName];
        }
        
        if (!typeStr.empty() && typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
          std::string elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
          ElemType = getTypeFromString(elemTypeStr);
        }
      }
      
      // Default to int if we can't determine
      if (!ElemType) {
        ElemType = llvm::Type::getInt32Ty(*TheContext);
      }
    }
  } else if (ArrayType->isPointerTy()) {
    // Old array representation: direct pointer
    ArrayPtr = ArrayVal;
    
    // For arrays, we need to infer the element type from context
    ElemType = llvm::Type::getInt32Ty(*TheContext); // default
    
    // If the array is from a variable, check its type
    if (VariableExprAST *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      std::string varName = VarExpr->getName();
      
      // Check if it's a global variable with known type
      if (GlobalTypes.count(varName)) {
        std::string typeStr = GlobalTypes[varName];
        if (typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
          // It's an array type, extract the element type
          std::string elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
          if (elemTypeStr == "int") {
            ElemType = llvm::Type::getInt32Ty(*TheContext);
          } else if (elemTypeStr == "float" || elemTypeStr == "double") {
            ElemType = llvm::Type::getDoubleTy(*TheContext);
          } else if (elemTypeStr == "char") {
            ElemType = llvm::Type::getInt8Ty(*TheContext);
          } else if (elemTypeStr == "bool") {
            ElemType = llvm::Type::getInt1Ty(*TheContext);
          }
        }
      }
    }
  } else {
    return LogErrorV("Array indexing requires an array type");
  }
  
  if (!ArrayPtr || !ElemType)
    return LogErrorV("Invalid array type for indexing");
  
  // Calculate the address of the indexed element
  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "elemptr");
  
  // Load and return the value
  return Builder->CreateLoad(ElemType, ElemPtr, "elemval");
}

llvm::Value *VariableExprAST::codegen_ptr() {
  llvm::Value *V = NamedValues[getName()];
  if (V) return V;

  llvm::Value *G = GlobalValues[getName()];
  if (G) return G;

  return LogErrorV("Unknown variable name for increment/decrement");
}

llvm::Value *ArrayIndexExprAST::codegen_ptr() {
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;

  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;

  if (!IndexVal->getType()->isIntegerTy(32)) {
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }

  llvm::Value *ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayptr");
  
  // For now, we'll determine the element type based on the array expression type
  // In a real implementation, we'd track this information in the AST
  llvm::Type *ElemTy = llvm::Type::getInt32Ty(*TheContext); // Default to i32
  
  // Try to infer the actual element type from the array
  if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
    // Look up the variable's type information
    // For now, we'll use a simple heuristic
    llvm::Value *V = NamedValues[VarExpr->getName()];
    if (!V) V = GlobalValues[VarExpr->getName()];
    if (V && V->getType()->isPointerTy()) {
      // Assume it's a pointer to the element type
      // In practice, we'd need proper type tracking
    }
  }

  return Builder->CreateGEP(ElemTy, ArrayPtr, IndexVal, "elemptr");
}

llvm::Value *VariableExprAST::codegen() {
  // First look this variable up in the local function scope
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    // Local variable - load from alloca
    llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
    return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
  }
  
  // Not found locally, check global scope
  llvm::GlobalVariable *GV = GlobalValues[getName()];
  if (GV) {
    // Global variable - load from global
    return Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
  }
  
  return LogErrorV("Unknown variable name");
}

// Helper function to cast a value to a target type
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType) {
  llvm::Type* sourceType = value->getType();
  
  if (sourceType == targetType)
    return value;
  
  // Special case: allow casting constant integers to smaller integer types
  // This is needed for literals like: byte b = 100
  if (llvm::ConstantInt* CI = llvm::dyn_cast<llvm::ConstantInt>(value)) {
    if (sourceType->isIntegerTy() && targetType->isIntegerTy() && 
        !targetType->isIntegerTy(1)) { // Don't allow to bool
      // Check if the constant fits in the target type
      llvm::APInt Val = CI->getValue();
      unsigned targetBits = targetType->getIntegerBitWidth();
      
      // Check ranges based on target type
      bool inRange = false;
      if (targetBits == 8) {
        // byte (unsigned) or sbyte/char (signed)
        int64_t v = Val.getSExtValue();
        // Assuming unsigned types use the full range
        inRange = (v >= 0 && v <= 255); // For byte
        // For sbyte/char, would need to track signedness
      } else if (targetBits == 16) {
        // short (signed) or ushort (unsigned)
        int64_t v = Val.getSExtValue();
        inRange = (v >= -32768 && v <= 65535); // Conservative: covers both signed and unsigned
      } else if (targetBits == 32) {
        // int (signed) or uint (unsigned)
        inRange = true; // Source is already 32-bit
      } else if (targetBits == 64) {
        // long (signed) or ulong (unsigned)
        inRange = true; // Can always extend to 64-bit
      }
      
      if (!inRange) {
        LogErrorV(("Integer literal " + std::to_string(Val.getSExtValue()) + 
                   " is out of range for target type").c_str());
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }
  
  // Check if types are compatible for implicit casting
  if (!areTypesCompatible(sourceType, targetType)) {
    // For explicit casts in the future, we might allow this
    // For now, implicit casts must follow compatibility rules
    return value; // Return original value, let the caller handle the error
  }
  
  // Integer to float
  if (sourceType->isIntegerTy() && targetType->isFloatingPointTy()) {
    return Builder->CreateSIToFP(value, targetType, "casttmp");
  }
  
  // Float to integer
  if (sourceType->isFloatingPointTy() && targetType->isIntegerTy()) {
    return Builder->CreateFPToSI(value, targetType, "casttmp");
  }
  
  // Float to float (different precision)
  if (sourceType->isFloatingPointTy() && targetType->isFloatingPointTy()) {
    if (sourceType->getPrimitiveSizeInBits() < targetType->getPrimitiveSizeInBits()) {
      return Builder->CreateFPExt(value, targetType, "casttmp");
    } else {
      return Builder->CreateFPTrunc(value, targetType, "casttmp");
    }
  }
  
  // If we can't cast, return the original value
  return value;
}

// Helper function to promote types for binary operations
std::pair<llvm::Value*, llvm::Value*> promoteTypes(llvm::Value* L, llvm::Value* R) {
  llvm::Type* LType = L->getType();
  llvm::Type* RType = R->getType();
  
  // If both are the same type, no promotion needed
  if (LType == RType)
    return {L, R};
  
  // Check if types are compatible
  if (!areTypesCompatible(LType, RType)) {
    // Generate a more helpful error message
    std::string LTypeStr = "unknown", RTypeStr = "unknown";
    
    // Determine type names
    if (LType->isIntegerTy(1)) LTypeStr = "bool";
    else if (LType->isIntegerTy(8)) LTypeStr = "byte/sbyte/char";
    else if (LType->isIntegerTy(16)) LTypeStr = "short/ushort";
    else if (LType->isIntegerTy(32)) LTypeStr = "int/uint";
    else if (LType->isIntegerTy(64)) LTypeStr = "long/ulong";
    else if (LType->isFloatTy()) LTypeStr = "float";
    else if (LType->isDoubleTy()) LTypeStr = "double";
    else if (LType->isFP128Ty()) LTypeStr = "fp128";
    
    if (RType->isIntegerTy(1)) RTypeStr = "bool";
    else if (RType->isIntegerTy(8)) RTypeStr = "byte/sbyte/char";
    else if (RType->isIntegerTy(16)) RTypeStr = "short/ushort";
    else if (RType->isIntegerTy(32)) RTypeStr = "int/uint";
    else if (RType->isIntegerTy(64)) RTypeStr = "long/ulong";
    else if (RType->isFloatTy()) RTypeStr = "float";
    else if (RType->isDoubleTy()) RTypeStr = "double";
    else if (RType->isFP128Ty()) RTypeStr = "fp128";
    
    LogErrorV(("Type mismatch: cannot implicitly convert between '" + LTypeStr + "' and '" + RTypeStr + "'").c_str());
    return {L, R}; // Return original values, error already logged
  }
  
  // If one is float and other is int, promote int to float
  if (LType->isFloatingPointTy() && RType->isIntegerTy()) {
    R = Builder->CreateSIToFP(R, LType, "promtmp");
    return {L, R};
  }
  
  if (LType->isIntegerTy() && RType->isFloatingPointTy()) {
    L = Builder->CreateSIToFP(L, RType, "promtmp");
    return {L, R};
  }
  
  // If both are floats but different precision, promote to higher precision
  if (LType->isFloatingPointTy() && RType->isFloatingPointTy()) {
    if (LType->isFloatTy() && RType->isDoubleTy()) {
      L = Builder->CreateFPExt(L, RType, "promtmp");
    } else if (LType->isDoubleTy() && RType->isFloatTy()) {
      R = Builder->CreateFPExt(R, LType, "promtmp");
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
    case '/':
      if (isFloat)
        return Builder->CreateFDiv(L, R, "divtmp");
      else
        return Builder->CreateSDiv(L, R, "divtmp");
    case '%':
      if (isFloat)
        return Builder->CreateFRem(L, R, "modtmp");
      else
        return Builder->CreateSRem(L, R, "modtmp");
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
      // Assignment operator - LHS must be a variable or array element
      if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
        // Simple variable assignment - check local first, then global
        llvm::Value *Variable = NamedValues[LHSE->getName()];
        if (Variable) {
          // Local variable
          llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(Variable);
          llvm::Type *VarType = Alloca->getAllocatedType();
          R = castToType(R, VarType);
          Builder->CreateStore(R, Variable);
          return R;
        }
        
        // Check global scope
        llvm::GlobalVariable *GV = GlobalValues[LHSE->getName()];
        if (GV) {
          // Global variable
          llvm::Type *VarType = GV->getValueType();
          R = castToType(R, VarType);
          Builder->CreateStore(R, GV);
          return R;
        }
        
        return LogErrorV("Unknown variable name");
      } else if (ArrayIndexExprAST *LHSAI = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
        // Array element assignment
        llvm::Value *ArrayVal = LHSAI->getArray()->codegen();
        if (!ArrayVal)
          return nullptr;
        
        llvm::Value *IndexVal = LHSAI->getIndex()->codegen();
        if (!IndexVal)
          return nullptr;
        
        // Make sure index is an integer
        if (!IndexVal->getType()->isIntegerTy(32)) {
          if (IndexVal->getType()->isIntegerTy()) {
            IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
          } else if (IndexVal->getType()->isFloatingPointTy()) {
            IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "idxtmp");
          } else {
            return LogErrorV("Array index must be an integer");
          }
        }
        
        // Get element type
        llvm::Type *ArrayType = ArrayVal->getType();
        llvm::Value *ArrayPtr = nullptr;
        llvm::Type *ElemType = nullptr;
        
        if (ArrayType->isStructTy()) {
          // New array representation: extract pointer from struct
          llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
          if (StructType->getNumElements() == 2) {
            // Extract the pointer (field 0)
            ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
            
            // Determine element type from the context
            if (VariableExprAST *VarExpr = dynamic_cast<VariableExprAST*>(LHSAI->getArray())) {
              std::string varName = VarExpr->getName();
              
              // Check local types first, then global types
              std::string typeStr;
              if (LocalTypes.count(varName)) {
                typeStr = LocalTypes[varName];
              } else if (GlobalTypes.count(varName)) {
                typeStr = GlobalTypes[varName];
              }
              
              if (!typeStr.empty() && typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
                std::string elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
                ElemType = getTypeFromString(elemTypeStr);
              }
            }
          }
        } else if (ArrayType->isPointerTy()) {
          // Old array representation
          ArrayPtr = ArrayVal;
          ElemType = R->getType(); // Use type of RHS
        } else {
          return LogErrorV("Array indexing requires an array type");
        }
        
        if (!ArrayPtr || !ElemType)
          return LogErrorV("Invalid array type for indexing");
        
        // Calculate the address and store
        llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "elemptr");
        Builder->CreateStore(R, ElemPtr);
        return R;
      } else {
        return LogErrorV("destination of '=' must be a variable or array element");
      }
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
  } else if (Op == "+=" || Op == "-=" || Op == "*=" || Op == "/=" || Op == "%=") {
    // Compound assignment operators: a += b is equivalent to a = a + b
    if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
      // Simple variable compound assignment
      llvm::Value *Variable = NamedValues[LHSE->getName()];
      if (!Variable) {
        Variable = GlobalValues[LHSE->getName()];
        if (!Variable)
          return LogErrorV("Unknown variable name for compound assignment");
      }
      
      // Load current value
      llvm::Value *CurrentVal;
      if (NamedValues[LHSE->getName()]) {
        // Local variable - alloca
        llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(Variable);
        CurrentVal = Builder->CreateLoad(Alloca->getAllocatedType(), Variable, LHSE->getName());
      } else {
        // Global variable
        llvm::GlobalVariable *GV = static_cast<llvm::GlobalVariable*>(Variable);
        CurrentVal = Builder->CreateLoad(GV->getValueType(), Variable, LHSE->getName());
      }
      
      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)
      
      // Promote types for the operation
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R);
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      
      // Store the result back
      Builder->CreateStore(Result, Variable);
      return Result;
      
    } else if (ArrayIndexExprAST *LHSE = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
      // Array element compound assignment: arr[i] += val
      llvm::Value *ArrayVal = LHSE->getArray()->codegen();
      if (!ArrayVal) return nullptr;
      
      llvm::Value *Index = LHSE->getIndex()->codegen();
      if (!Index) return nullptr;
      
      // Handle struct arrays
      llvm::Type *ArrayType = ArrayVal->getType();
      llvm::Value *ArrayPtr = nullptr;
      llvm::Type *ElemType = nullptr;
      
      if (ArrayType->isStructTy()) {
        // New array representation: extract pointer from struct
        llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
        if (StructType->getNumElements() == 2) {
          // Extract the pointer (field 0)
          ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
          
          // Determine element type from the context
          if (VariableExprAST *ArrayVar = dynamic_cast<VariableExprAST*>(LHSE->getArray())) {
            std::string varName = ArrayVar->getName();
            
            // Check local types first, then global types
            std::string typeStr;
            if (LocalTypes.count(varName)) {
              typeStr = LocalTypes[varName];
            } else if (GlobalTypes.count(varName)) {
              typeStr = GlobalTypes[varName];
            }
            
            if (!typeStr.empty() && typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
              std::string elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
              ElemType = getTypeFromString(elemTypeStr);
            }
          }
        }
      } else if (ArrayType->isPointerTy()) {
        // Old array representation
        ArrayPtr = ArrayVal;
        // Default element type
        ElemType = llvm::Type::getInt32Ty(*TheContext);
      }
      
      if (!ArrayPtr || !ElemType)
        return LogErrorV("Invalid array type for compound assignment");
      
      // Get the element pointer
      llvm::Value *ElementPtr = Builder->CreateGEP(ElemType, ArrayPtr, Index, "arrayptr");
      
      // Load current value
      llvm::Value *CurrentVal = Builder->CreateLoad(ElemType, ElementPtr, "arrayload");
      
      // Perform the operation
      llvm::Value *Result;
      char baseOp = Op[0]; // Get the base operator (+, -, *, /, %)
      
      // Promote types for the operation
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R);
      bool isFloat = PromotedCurrent->getType()->isFloatingPointTy();
      
      switch (baseOp) {
        case '+':
          if (isFloat)
            Result = Builder->CreateFAdd(PromotedCurrent, PromotedR, "addtmp");
          else
            Result = Builder->CreateAdd(PromotedCurrent, PromotedR, "addtmp");
          break;
        case '-':
          if (isFloat)
            Result = Builder->CreateFSub(PromotedCurrent, PromotedR, "subtmp");
          else
            Result = Builder->CreateSub(PromotedCurrent, PromotedR, "subtmp");
          break;
        case '*':
          if (isFloat)
            Result = Builder->CreateFMul(PromotedCurrent, PromotedR, "multmp");
          else
            Result = Builder->CreateMul(PromotedCurrent, PromotedR, "multmp");
          break;
        case '/':
          if (isFloat)
            Result = Builder->CreateFDiv(PromotedCurrent, PromotedR, "divtmp");
          else
            Result = Builder->CreateSDiv(PromotedCurrent, PromotedR, "divtmp");
          break;
        case '%':
          if (isFloat)
            Result = Builder->CreateFRem(PromotedCurrent, PromotedR, "modtmp");
          else
            Result = Builder->CreateSRem(PromotedCurrent, PromotedR, "modtmp");
          break;
        default:
          return LogErrorV("Unknown compound assignment operator");
      }
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      return Result;
      
    } else {
      return LogErrorV("destination of compound assignment must be a variable or array element");
    }
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
  } else if (Op == "&") {
    // Bitwise AND - only works on integers
    if (isFloat)
      return LogErrorV("Bitwise AND requires integer operands");
    return Builder->CreateAnd(L, R, "andtmp");
  } else if (Op == "|") {
    // Bitwise OR - only works on integers
    if (isFloat)
      return LogErrorV("Bitwise OR requires integer operands");
    return Builder->CreateOr(L, R, "ortmp");
  } else if (Op == "^") {
    // Bitwise XOR - only works on integers
    if (isFloat)
      return LogErrorV("Bitwise XOR requires integer operands");
    return Builder->CreateXor(L, R, "xortmp");
  } else if (Op == "<<") {
    // Left shift - only works on integers
    if (isFloat)
      return LogErrorV("Left shift requires integer operands");
    return Builder->CreateShl(L, R, "shltmp");
  } else if (Op == ">>") {
    // Right shift (arithmetic) - only works on integers
    if (isFloat)
      return LogErrorV("Right shift requires integer operands");
    return Builder->CreateAShr(L, R, "ashrtmp");
  } else if (Op == "&=" || Op == "|=" || Op == "^=" || Op == "<<=" || Op == ">>=") {
    // Bitwise compound assignment operators
    if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
      // Simple variable compound assignment
      llvm::Value *Variable = NamedValues[LHSE->getName()];
      if (!Variable) {
        Variable = GlobalValues[LHSE->getName()];
        if (!Variable)
          return LogErrorV("Unknown variable name for compound assignment");
      }
      
      // Load current value
      llvm::Value *CurrentVal;
      if (NamedValues[LHSE->getName()]) {
        // Local variable - alloca
        llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(Variable);
        CurrentVal = Builder->CreateLoad(Alloca->getAllocatedType(), Variable, LHSE->getName());
      } else {
        // Global variable
        llvm::GlobalVariable *GV = static_cast<llvm::GlobalVariable*>(Variable);
        CurrentVal = Builder->CreateLoad(GV->getValueType(), Variable, LHSE->getName());
      }
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() || R->getType()->isFloatingPointTy())
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R);
      
      // Perform the operation based on the compound operator
      llvm::Value *Result;
      if (Op == "&=") {
        Result = Builder->CreateAnd(PromotedCurrent, PromotedR, "andtmp");
      } else if (Op == "|=") {
        Result = Builder->CreateOr(PromotedCurrent, PromotedR, "ortmp");
      } else if (Op == "^=") {
        Result = Builder->CreateXor(PromotedCurrent, PromotedR, "xortmp");
      } else if (Op == "<<=") {
        Result = Builder->CreateShl(PromotedCurrent, PromotedR, "shltmp");
      } else if (Op == ">>=") {
        Result = Builder->CreateAShr(PromotedCurrent, PromotedR, "ashrtmp");
      } else {
        return LogErrorV("Unknown bitwise compound assignment operator");
      }
      
      // Store the result back
      Builder->CreateStore(Result, Variable);
      return Result;
      
    } else if (ArrayIndexExprAST *LHSE = dynamic_cast<ArrayIndexExprAST*>(getLHS())) {
      // Array element compound assignment: arr[i] &= val
      llvm::Value *ArrayVal = LHSE->getArray()->codegen();
      if (!ArrayVal) return nullptr;
      
      llvm::Value *Index = LHSE->getIndex()->codegen();
      if (!Index) return nullptr;
      
      // Handle struct arrays
      llvm::Type *ArrayType = ArrayVal->getType();
      llvm::Value *ArrayPtr = nullptr;
      llvm::Type *ElemType = nullptr;
      
      if (ArrayType->isStructTy()) {
        // New array representation: extract pointer from struct
        llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
        if (StructType->getNumElements() == 2) {
          // Extract the pointer (field 0)
          ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
          
          // Determine element type from the context
          if (VariableExprAST *ArrayVar = dynamic_cast<VariableExprAST*>(LHSE->getArray())) {
            std::string varName = ArrayVar->getName();
            
            // Check local types first, then global types
            std::string typeStr;
            if (LocalTypes.count(varName)) {
              typeStr = LocalTypes[varName];
            } else if (GlobalTypes.count(varName)) {
              typeStr = GlobalTypes[varName];
            }
            
            if (!typeStr.empty() && typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
              std::string elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
              ElemType = getTypeFromString(elemTypeStr);
            }
          }
        }
      } else if (ArrayType->isPointerTy()) {
        // Old array representation
        ArrayPtr = ArrayVal;
        // Default element type
        ElemType = llvm::Type::getInt32Ty(*TheContext);
      }
      
      if (!ArrayPtr || !ElemType)
        return LogErrorV("Invalid array type for bitwise compound assignment");
      
      // Get the element pointer
      llvm::Value *ElementPtr = Builder->CreateGEP(ElemType, ArrayPtr, Index, "arrayptr");
      
      // Load current value
      llvm::Value *CurrentVal = Builder->CreateLoad(ElemType, ElementPtr, "arrayload");
      
      // Check that operands are integers
      if (CurrentVal->getType()->isFloatingPointTy() || R->getType()->isFloatingPointTy())
        return LogErrorV("Bitwise compound assignment requires integer operands");
      
      // Promote types for the operation
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R);
      
      // Perform the operation
      llvm::Value *Result;
      if (Op == "&=") {
        Result = Builder->CreateAnd(PromotedCurrent, PromotedR, "andtmp");
      } else if (Op == "|=") {
        Result = Builder->CreateOr(PromotedCurrent, PromotedR, "ortmp");
      } else if (Op == "^=") {
        Result = Builder->CreateXor(PromotedCurrent, PromotedR, "xortmp");
      } else if (Op == "<<=") {
        Result = Builder->CreateShl(PromotedCurrent, PromotedR, "shltmp");
      } else if (Op == ">>=") {
        Result = Builder->CreateAShr(PromotedCurrent, PromotedR, "ashrtmp");
      } else {
        return LogErrorV("Unknown bitwise compound assignment operator");
      }
      
      // Store the result back
      Builder->CreateStore(Result, ElementPtr);
      return Result;
      
    } else {
      return LogErrorV("destination of bitwise compound assignment must be a variable or array element");
    }
  }
  
  return LogErrorV("invalid binary operator");
}

llvm::Value *UnaryExprAST::codegen() {
  if (Op == "++" || Op == "--") {
    llvm::Value *Ptr = Operand->codegen_ptr();
    if (!Ptr) {
        return LogErrorV("operand of ++/-- must be a variable");
    }

    llvm::Value *Val = Operand->codegen();
    llvm::Type *Ty = Val->getType();

    llvm::Value *One = nullptr;
    if (Ty->isIntegerTy()) {
        One = llvm::ConstantInt::get(Ty, 1);
    } else if (Ty->isFloatingPointTy()) {
        One = llvm::ConstantFP::get(Ty, 1.0);
    } else {
        return LogErrorV("++/-- requires integer or floating-point type");
    }

    llvm::Value *CurVal = Builder->CreateLoad(Ty, Ptr, "loadtmp");
    llvm::Value *NextVal;

    if (Op == "++") {
        if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFAdd(CurVal, One, "addtmp");
        else
            NextVal = Builder->CreateAdd(CurVal, One, "addtmp");
    } else { // --
        if (Ty->isFloatingPointTy())
            NextVal = Builder->CreateFSub(CurVal, One, "subtmp");
        else
            NextVal = Builder->CreateSub(CurVal, One, "subtmp");
    }

    Builder->CreateStore(NextVal, Ptr);

    // For prefix operators, return the new value; for postfix, return the old value
    return isPrefix ? NextVal : CurVal;
  }

  // Handle other unary operators
  llvm::Value *OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  if (Op == "-") {
    return Builder->CreateNeg(OperandV, "negtmp");
  } else if (Op == "!") {
    return Builder->CreateNot(OperandV, "nottmp");
  }

  return LogErrorV("invalid unary operator");
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
  
  // Check if we're at global scope (in __anon_var_decl function)
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  bool isGlobal = (TheFunction->getName() == "__anon_var_decl");
  
  if (isGlobal) {
    // Create a global variable
    llvm::GlobalVariable *GV = new llvm::GlobalVariable(
        *TheModule, VarType, false, llvm::GlobalValue::ExternalLinkage,
        nullptr, getName());
    
    // Generate the initializer
    if (getInitializer()) {
      // For globals, we need constant initializers
      // For now, create a zero initializer and store the actual value
      llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
      GV->setInitializer(ZeroInit);
      
      // Generate code to store the actual initial value
      llvm::Value *InitVal = getInitializer()->codegen();
      if (!InitVal)
        return nullptr;
      
      // Cast the initializer to the variable type if needed
      InitVal = castToType(InitVal, VarType);
      
      // Store the initial value
      Builder->CreateStore(InitVal, GV);
    } else {
      // Zero initialize
      llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
      GV->setInitializer(ZeroInit);
    }
    
    // Remember this global binding
    GlobalValues[getName()] = GV;
    GlobalTypes[getName()] = getType();
    
    // Return the value that was stored
    return Builder->CreateLoad(VarType, GV, getName());
  } else {
    // Local variable - use alloca as before
    llvm::AllocaInst *Alloca = Builder->CreateAlloca(VarType, nullptr, getName());
    
    // Generate and store the initial value
    if (getInitializer()) {
      llvm::Value *InitVal = getInitializer()->codegen();
      if (!InitVal)
        return nullptr;
      
      // Cast the initializer to the variable type if needed
      InitVal = castToType(InitVal, VarType);
      
      // Store the initial value
      Builder->CreateStore(InitVal, Alloca);
    }
    
    // Remember this local binding
    NamedValues[getName()] = Alloca;
    LocalTypes[getName()] = getType();  // Track the type
    
    // Return the value that was stored
    return Builder->CreateLoad(VarType, Alloca, getName());
  }
}

llvm::Value *ExpressionStmtAST::codegen() {
  // Just generate code for the expression
  return getExpression()->codegen();
}

llvm::Value *ForEachStmtAST::codegen() {
  // Get the collection (array) to iterate over
  llvm::Value *CollectionVal = Collection->codegen();
  if (!CollectionVal)
    return nullptr;
  
  // Get the element type from the foreach declaration
  llvm::Type *ElemType = getTypeFromString(Type);
  if (!ElemType)
    return LogErrorV("Unknown element type in foreach loop");
  
  // Extract array pointer and size from the collection value
  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  
  // Check if CollectionVal is a struct (new array representation)
  if (CollectionVal->getType()->isStructTy()) {
    llvm::StructType *StructType = llvm::cast<llvm::StructType>(CollectionVal->getType());
    if (StructType->getNumElements() == 2) {
      // Extract pointer (field 0) and size (field 1)
      ArrayPtr = Builder->CreateExtractValue(CollectionVal, 0, "arrayPtr");
      ArraySize = Builder->CreateExtractValue(CollectionVal, 1, "arraySize");
    } else {
      return LogErrorV("Invalid array struct format");
    }
  } else if (CollectionVal->getType()->isPointerTy()) {
    // Old array representation: direct pointer (for backward compatibility)
    ArrayPtr = CollectionVal;
    
    // Try to determine size statically
    if (ArrayExprAST *ArrayExpr = dynamic_cast<ArrayExprAST*>(Collection.get())) {
      size_t StaticSize = ArrayExpr->getElements().size();
      ArraySize = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, StaticSize));
    } else {
      // For variables without size info, use a default
      ArraySize = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 10));
    }
  } else {
    return LogErrorV("foreach loops require array expressions");
  }
  
  if (!ArrayPtr || !ArraySize)
    return LogErrorV("Failed to extract array information for foreach loop");
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop
  llvm::BasicBlock *InitBB = Builder->GetInsertBlock();
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "forcond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "forbody");
  llvm::BasicBlock *IncBB = llvm::BasicBlock::Create(*TheContext, "forinc");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "forcont");
  
  // Create and initialize the loop counter
  llvm::AllocaInst *CounterAlloca = Builder->CreateAlloca(
      llvm::Type::getInt32Ty(*TheContext), nullptr, "loopcounter");
  Builder->CreateStore(llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)), CounterAlloca);
  
  // Create an alloca for the loop variable
  llvm::AllocaInst *VarAlloca = Builder->CreateAlloca(ElemType, nullptr, VarName);
  
  // Jump to the condition check
  Builder->CreateBr(CondBB);
  
  // Emit the condition check
  Builder->SetInsertPoint(CondBB);
  llvm::Value *CounterVal = Builder->CreateLoad(
      llvm::Type::getInt32Ty(*TheContext), CounterAlloca, "counter");
  llvm::Value *CondV = Builder->CreateICmpSLT(CounterVal, ArraySize, "loopcond");
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  
  // Load the current array element and store it in the loop variable
  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, CounterVal, "elemptr");
  llvm::Value *ElemVal = Builder->CreateLoad(ElemType, ElemPtr, "elemval");
  Builder->CreateStore(ElemVal, VarAlloca);
  
  // Add the loop variable to the symbol table
  llvm::Value *OldVal = NamedValues[VarName];
  NamedValues[VarName] = VarAlloca;
  
  // Push the exit and continue blocks for break/skip statements
  LoopExitBlocks.push_back(AfterBB);
  LoopContinueBlocks.push_back(IncBB);
  
  // Generate code for the body
  llvm::Value *BodyV = Body->codegen();
  
  // Pop the exit and continue blocks
  LoopExitBlocks.pop_back();
  LoopContinueBlocks.pop_back();
  
  if (!BodyV) {
    // Restore the old value
    if (OldVal)
      NamedValues[VarName] = OldVal;
    else
      NamedValues.erase(VarName);
    return nullptr;
  }
  
  // Branch to increment block (unless body contains break/return)
  if (!Builder->GetInsertBlock()->getTerminator())
    Builder->CreateBr(IncBB);
  
  // Emit the increment block
  IncBB->insertInto(TheFunction);
  Builder->SetInsertPoint(IncBB);
  
  // Increment the counter
  llvm::Value *NextCounter = Builder->CreateAdd(CounterVal, 
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 1)), "nextcounter");
  Builder->CreateStore(NextCounter, CounterAlloca);
  Builder->CreateBr(CondBB);
  
  // Emit the after-loop block
  AfterBB->insertInto(TheFunction);
  Builder->SetInsertPoint(AfterBB);
  
  // Restore the old value
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
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
  
  // Push the exit and continue blocks for break/skip statements
  LoopExitBlocks.push_back(AfterBB);
  LoopContinueBlocks.push_back(CondBB);
  
  // Generate code for the body
  llvm::Value *BodyV = getBody()->codegen();
  
  // Pop the exit and continue blocks
  LoopExitBlocks.pop_back();
  LoopContinueBlocks.pop_back();
  
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

llvm::Value *BreakStmtAST::codegen() {
  // Check if we're inside a loop
  if (LoopExitBlocks.empty())
    return LogErrorV("'break' statement not within a loop");
  
  // Get the current loop's exit block
  llvm::BasicBlock *ExitBlock = LoopExitBlocks.back();
  
  // Create an unconditional branch to the loop exit
  Builder->CreateBr(ExitBlock);
  
  // Create a new dead block for any code after the break
  // (which should be unreachable)
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *DeadBB = llvm::BasicBlock::Create(*TheContext, "afterbreak", TheFunction);
  Builder->SetInsertPoint(DeadBB);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

llvm::Value *SkipStmtAST::codegen() {
  // Check if we're inside a loop
  if (LoopContinueBlocks.empty())
    return LogErrorV("'skip' statement not within a loop");
  
  // Get the current loop's continue block
  llvm::BasicBlock *ContinueBlock = LoopContinueBlocks.back();
  
  // Create an unconditional branch to the loop continue point
  Builder->CreateBr(ContinueBlock);
  
  // Create a new dead block for any code after the skip
  // (which should be unreachable)
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *DeadBB = llvm::BasicBlock::Create(*TheContext, "afterskip", TheFunction);
  Builder->SetInsertPoint(DeadBB);
  
  // Return a dummy value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
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
  LocalTypes.clear();  // Clear local types for new function
  
  // Get parameter info from the prototype
  const auto &Params = getProto()->getArgs();
  size_t i = 0;
  
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable
    llvm::AllocaInst *Alloca = Builder->CreateAlloca(Arg.getType(), nullptr, Arg.getName());
    
    // Store the initial value into the alloca
    Builder->CreateStore(&Arg, Alloca);
    
    // Add arguments to variable symbol table
    NamedValues[std::string(Arg.getName())] = Alloca;
    
    // Track the type of the parameter
    if (i < Params.size()) {
      LocalTypes[std::string(Arg.getName())] = Params[i].Type;
    }
    i++;
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