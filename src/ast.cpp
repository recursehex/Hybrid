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
static std::map<std::string, llvm::StructType *> StructTypes; // Track struct types
static std::map<std::string, std::vector<std::pair<std::string, unsigned>>> StructFieldIndices; // Track field names and indices
static std::map<std::string, std::map<std::string, std::string>> StructFieldTypes; // Track field types: structName -> (fieldName -> typeName)

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

// Helper to check if a type name represents a signed type
bool isSignedType(const std::string &TypeStr) {
  // Signed types: int, sbyte, short, long, char (in C, char is signed by default)
  // Note: schar is "short char" (8-bit), not signed char
  return TypeStr == "int" || TypeStr == "sbyte" || TypeStr == "short" || 
         TypeStr == "long" || TypeStr == "char";
}

// Helper to check if a type name represents an unsigned type
bool isUnsignedType(const std::string &TypeStr) {
  // Unsigned types: byte, ushort, uint, ulong
  // Note: schar and lchar are character types, treating as unsigned
  return TypeStr == "byte" || TypeStr == "ushort" || TypeStr == "uint" || 
         TypeStr == "ulong" || TypeStr == "schar" || TypeStr == "lchar";
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
    return llvm::Type::getInt16Ty(*TheContext);
  else if (TypeStr == "bool")
    return llvm::Type::getInt8Ty(*TheContext);
  else if (TypeStr == "void")
    return llvm::Type::getVoidTy(*TheContext);
  else if (TypeStr == "string")
    return llvm::PointerType::get(llvm::Type::getInt16Ty(*TheContext), 0); // Strings as 16-bit char*
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
  
  // Check if it's a struct type
  auto structIt = StructTypes.find(TypeStr);
  if (structIt != StructTypes.end()) {
    return llvm::PointerType::get(structIt->second, 0); // Struct instances are pointers
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

void ForLoopStmtAST::print() const {
  std::cout << "Parsed a for loop: for " << Type << " " << VarName << " = <init> to <limit>" << std::endl;
}

//===----------------------------------------------------------------------===//
// Expression Code Generation
//===----------------------------------------------------------------------===//

// Generate code for number expressions
llvm::Value *NumberExprAST::codegen() {
  // Check if the number is a whole number that could be an integer
  double val = getValue();
  if (val == floor(val) && val >= INT32_MIN && val <= INT32_MAX) {
    // It's a whole number in int32 range, create as integer
    setTypeName("int");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, (int32_t)val));
  } else {
    // It's a floating point number
    setTypeName("double");
    return llvm::ConstantFP::get(*TheContext, llvm::APFloat(val));
  }
}

// Generate code for boolean expressions
llvm::Value *BoolExprAST::codegen() {
  setTypeName("bool");
  return llvm::ConstantInt::get(*TheContext, llvm::APInt(8, getValue() ? 1 : 0));
}

// Generate code for null expressions
llvm::Value *NullExprAST::codegen() {
  // Create a null pointer for string type (16-bit char*)
  llvm::Type *StringType = llvm::PointerType::get(llvm::Type::getInt16Ty(*TheContext), 0);
  return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(StringType));
}

// Generate code for string literals
llvm::Value *StringExprAST::codegen() {
  // Create a global 16-bit Unicode character array constant
  setTypeName("string");
  
  const std::string &str = getValue();
  std::vector<llvm::Constant*> CharValues;
  
  // TODO: Proper UTF-8 to UTF-16 conversion should be implemented here
  // For now, we'll treat each byte as a separate 16-bit character
  // This allows for proper Unicode support when the lexer is updated
  for (unsigned char c : str) {
    CharValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt16Ty(*TheContext), 
                                                static_cast<uint16_t>(c)));
  }
  
  // Add null terminator
  CharValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt16Ty(*TheContext), 0));
  
  // Create array type and constant
  llvm::ArrayType *ArrayType = llvm::ArrayType::get(llvm::Type::getInt16Ty(*TheContext), CharValues.size());
  llvm::Constant *StringArray = llvm::ConstantArray::get(ArrayType, CharValues);
  
  // Create global variable
  llvm::GlobalVariable *GlobalStr = new llvm::GlobalVariable(
    *TheModule, ArrayType, true, llvm::GlobalValue::PrivateLinkage, StringArray, "str");
  
  // Return pointer to the first element
  llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  return Builder->CreateInBoundsGEP(ArrayType, GlobalStr, {Zero, Zero}, "strptr");
}

// Generate code for character literals
llvm::Value *CharExprAST::codegen() {
  uint32_t val = getValue();
  
  // Determine the appropriate type based on the context
  // For now, use the default char (16-bit) unless the value requires more
  if (val > 0xFFFF) {
    // Value requires 32-bit (lchar)
    setTypeName("lchar");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, val));
  } else if (val > 0xFF) {
    // Value requires 16-bit (char)
    setTypeName("char");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(16, val));
  } else {
    // Value fits in 16-bit (default char)
    setTypeName("char");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(16, val));
  }
}

// Forward declaration for castToType
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType);
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType, const std::string& targetTypeName);

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

// Generate code for array literals
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

// Generate code for array indexing
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
  std::string elemTypeStr = "int"; // Track the element type string for setTypeName
  
  if (ArrayType->isStructTy()) {
    // New array representation: extract pointer from struct
    llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
    if (StructType->getNumElements() == 2) {
      // Extract the pointer (field 0)
      ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
      
      // Determine element type from the context
      // First, check if it's from a direct array literal
      if (ArrayExprAST *ArrayExpr = dynamic_cast<ArrayExprAST*>(getArray())) {
        elemTypeStr = ArrayExpr->getElementType();
        ElemType = getTypeFromString(elemTypeStr);
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
          elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
          ElemType = getTypeFromString(elemTypeStr);
        }
      }
      
      // Default to int if can't determine
      if (!ElemType) {
        ElemType = llvm::Type::getInt32Ty(*TheContext);
      }
    }
  } else if (ArrayType->isPointerTy()) {
    // Old array representation: direct pointer
    ArrayPtr = ArrayVal;
    
    // For arrays, infer the element type from context
    ElemType = llvm::Type::getInt32Ty(*TheContext); // default
    
    // If the array is from a variable, check its type
    if (VariableExprAST *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      std::string varName = VarExpr->getName();
      
      // Check local types first, then global types
      std::string typeStr;
      if (LocalTypes.count(varName)) {
        typeStr = LocalTypes[varName];
      } else if (GlobalTypes.count(varName)) {
        typeStr = GlobalTypes[varName];
      }
      
      if (!typeStr.empty() && typeStr.size() > 2 && typeStr.substr(typeStr.size() - 2) == "[]") {
        // It's an array type, extract the element type
        elemTypeStr = typeStr.substr(0, typeStr.size() - 2);
        if (elemTypeStr == "int") {
          ElemType = llvm::Type::getInt32Ty(*TheContext);
        } else if (elemTypeStr == "float" || elemTypeStr == "double") {
          ElemType = llvm::Type::getDoubleTy(*TheContext);
        } else if (elemTypeStr == "char") {
          ElemType = llvm::Type::getInt16Ty(*TheContext);
        } else if (elemTypeStr == "bool") {
          ElemType = llvm::Type::getInt8Ty(*TheContext);
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
  
  // Set the type name for proper type checking
  setTypeName(elemTypeStr);
  
  // Load and return the value
  return Builder->CreateLoad(ElemType, ElemPtr, "elemval");
}

// Variable pointer code generation for increment/decrement
llvm::Value *VariableExprAST::codegen_ptr() {
  llvm::Value *V = NamedValues[getName()];
  if (V) return V;

  llvm::Value *G = GlobalValues[getName()];
  if (G) return G;

  return LogErrorV("Unknown variable name for increment/decrement");
}

// Array index pointer code generation for increment/decrement
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

  // Extract the pointer from the array struct
  llvm::Value *ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayptr");
  
  // For now, determine the element type based on the array expression type
  // In a real implementation, track this information in the AST
  llvm::Type *ElemTy = llvm::Type::getInt32Ty(*TheContext); // Default to i32
  
  // Try to infer the actual element type from the array
  if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
    // Look up the variable's type information
    // For now, use a simple heuristic
    llvm::Value *V = NamedValues[VarExpr->getName()];
    if (!V) V = GlobalValues[VarExpr->getName()];
    if (V && V->getType()->isPointerTy()) {
      // Assume it's a pointer to the element type
      // In practice, need proper type tracking
    }
  }

  return Builder->CreateGEP(ElemTy, ArrayPtr, IndexVal, "elemptr");
}

// Generate code for variable expressions
llvm::Value *VariableExprAST::codegen() {
  // First look this variable up in the local function scope
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    // Local variable - load from alloca
    llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
    // Set type name from local types
    if (LocalTypes.count(getName())) {
      setTypeName(LocalTypes[getName()]);
    }
    return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
  }
  
  // Not found locally, check global scope
  llvm::GlobalVariable *GV = GlobalValues[getName()];
  if (GV) {
    // Global variable - load from global
    // Set type name from global types
    if (GlobalTypes.count(getName())) {
      setTypeName(GlobalTypes[getName()]);
    }
    return Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
  }
  
  return LogErrorV(("Unknown variable name: " + getName()).c_str());
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
    // For explicit casts in the future, might allow this
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
  
  // Can't cast, return the original value
  return value;
}

// Overloaded castToType that knows the target type name for proper range checking
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType, const std::string& targetTypeName) {
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
      int64_t v = Val.getSExtValue();
      
      // Check ranges based on target type name (which tells us signedness)
      bool inRange = false;
      if (targetBits == 8) {
        if (isSignedType(targetTypeName)) {
          // sbyte, char: -128 to 127
          inRange = (v >= -128 && v <= 127);
        } else {
          // byte, schar: 0 to 255
          inRange = (v >= 0 && v <= 255);
        }
      } else if (targetBits == 16) {
        if (isSignedType(targetTypeName)) {
          // short: -32768 to 32767
          inRange = (v >= -32768 && v <= 32767);
        } else {
          // ushort: 0 to 65535
          inRange = (v >= 0 && v <= 65535);
        }
      } else if (targetBits == 32) {
        if (isSignedType(targetTypeName)) {
          // int: -2147483648 to 2147483647
          inRange = (v >= INT32_MIN && v <= INT32_MAX);
        } else {
          // uint: 0 to 4294967295
          inRange = (v >= 0 && v <= UINT32_MAX);
        }
      } else if (targetBits == 64) {
        // For 64-bit, always can fit a 32-bit source
        inRange = true;
      }
      
      if (!inRange) {
        LogErrorV(("Integer literal " + std::to_string(v) + 
                   " is out of range for target type " + targetTypeName).c_str());
        return value;
      }
      
      return llvm::ConstantInt::get(targetType, Val.trunc(targetBits));
    }
  }
  
  // For non-constant values, use the regular castToType
  return castToType(value, targetType);
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

// Generate code for binary expressions like +, -, *, /, <, >
llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = getLHS()->codegen();
  llvm::Value *R = getRHS()->codegen();
  if (!L || !R)
    return nullptr;

  // Get type names from operands
  std::string leftTypeName = getLHS()->getTypeName();
  std::string rightTypeName = getRHS()->getTypeName();

  // Promote types to compatible types
  auto promoted = promoteTypes(L, R);
  L = promoted.first;
  R = promoted.second;

  // Check if working with floating point or integer types
  bool isFloat = L->getType()->isFloatingPointTy();
  
  // Set result type name based on the promoted type
  if (isFloat) {
    setTypeName(L->getType()->isFloatTy() ? "float" : "double");
  } else {
    // For integers, use the larger type's name or left type if same size
    if (!leftTypeName.empty()) {
      setTypeName(leftTypeName);
    } else {
      setTypeName("int"); // Default
    }
  }

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
      setTypeName("bool");
      if (isFloat) {
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
      } else {
        L = Builder->CreateICmpSLT(L, R, "cmptmp");
      }
      // Zero-extend i1 to i8 for bool type
      return Builder->CreateZExt(L, llvm::Type::getInt8Ty(*TheContext), "booltmp");
    case '>':
      setTypeName("bool");
      if (isFloat) {
        L = Builder->CreateFCmpUGT(L, R, "cmptmp");
      } else {
        L = Builder->CreateICmpSGT(L, R, "cmptmp");
      }
      // Zero-extend i1 to i8 for bool type
      return Builder->CreateZExt(L, llvm::Type::getInt8Ty(*TheContext), "booltmp");
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
        
        return LogErrorV(("Unknown variable name: " + LHSE->getName()).c_str());
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
      } else if (MemberAccessExprAST *LHSMA = dynamic_cast<MemberAccessExprAST*>(getLHS())) {
        // Member access assignment (e.g., this.x = value)
        llvm::Value *FieldPtr = LHSMA->codegen_ptr();
        if (!FieldPtr)
          return nullptr;
        
        // Cast RHS to the field type if necessary
        // Need to determine the field type from the struct definition
        // For now, trust that R has the correct type
        // TODO: Add proper type checking
        // R = castToType(R, FieldType);
        
        Builder->CreateStore(R, FieldPtr);
        return R;
      } else {
        return LogErrorV("destination of '=' must be a variable, array element, or struct member");
      }
    }
  }
  
  // Handle multi-character operators
  if (Op == "==") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpOEQ(L, R, "eqtmp");
    else
      Cmp = Builder->CreateICmpEQ(L, R, "eqtmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "!=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpONE(L, R, "netmp");
    else
      Cmp = Builder->CreateICmpNE(L, R, "netmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "<=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpOLE(L, R, "letmp");
    else
      Cmp = Builder->CreateICmpSLE(L, R, "letmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == ">=") {
    setTypeName("bool");
    llvm::Value *Cmp;
    if (isFloat)
      Cmp = Builder->CreateFCmpOGE(L, R, "getmp");
    else
      Cmp = Builder->CreateICmpSGE(L, R, "getmp");
    return Builder->CreateZExt(Cmp, llvm::Type::getInt8Ty(*TheContext), "booltmp");
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
    // Check that both operands are boolean types
    if (leftTypeName != "bool" || rightTypeName != "bool") {
      return LogErrorV("Boolean AND operator '&&' can only be used with bool types");
    }
    // Convert i8 bool operands to i1 for logical operations
    if (L->getType()->isIntegerTy(8)) {
      L = Builder->CreateTrunc(L, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    if (R->getType()->isIntegerTy(8)) {
      R = Builder->CreateTrunc(R, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    setTypeName("bool");
    llvm::Value *Result = Builder->CreateAnd(L, R, "andtmp");
    return Builder->CreateZExt(Result, llvm::Type::getInt8Ty(*TheContext), "booltmp");
  } else if (Op == "||") {
    // Check that both operands are boolean types
    if (leftTypeName != "bool" || rightTypeName != "bool") {
      return LogErrorV("Boolean OR operator '||' can only be used with bool types");
    }
    // Convert i8 bool operands to i1 for logical operations
    if (L->getType()->isIntegerTy(8)) {
      L = Builder->CreateTrunc(L, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    if (R->getType()->isIntegerTy(8)) {
      R = Builder->CreateTrunc(R, llvm::Type::getInt1Ty(*TheContext), "tobool");
    }
    setTypeName("bool");
    llvm::Value *Result = Builder->CreateOr(L, R, "ortmp");
    return Builder->CreateZExt(Result, llvm::Type::getInt8Ty(*TheContext), "booltmp");
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

// Generate code for unary expressions like -, !, ++, --
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
    // Check that operand is boolean type
    std::string operandTypeName = Operand->getTypeName();
    if (operandTypeName != "bool") {
      return LogErrorV("Boolean NOT operator '!' can only be used with bool type");
    }
    setTypeName("bool");
    // For bool type (i8), truncate to i1, apply NOT, then zero-extend back to i8
    if (OperandV->getType()->isIntegerTy(8)) {
      llvm::Value *Truncated = Builder->CreateTrunc(OperandV, llvm::Type::getInt1Ty(*TheContext), "tobool");
      llvm::Value *Negated = Builder->CreateNot(Truncated, "nottmp");
      return Builder->CreateZExt(Negated, llvm::Type::getInt8Ty(*TheContext), "booltmp");
    }
    return Builder->CreateNot(OperandV, "nottmp");
  }

  return LogErrorV("invalid unary operator");
}

// Generate code for type casting expressions
llvm::Value *CastExprAST::codegen() {
  // Get the operand value
  llvm::Value *OperandV = getOperand()->codegen();
  if (!OperandV)
    return nullptr;

  // Get the source type name from the operand
  std::string sourceTypeName = getOperand()->getTypeName();

  // Get the target LLVM type
  llvm::Type *TargetLLVMType = getTypeFromString(getTargetType());
  if (!TargetLLVMType)
    return LogErrorV("Invalid target type for cast");

  // Set our type name to the target type
  setTypeName(getTargetType());

  llvm::Type *SourceType = OperandV->getType();
  
  // If same type, just return the value
  if (SourceType == TargetLLVMType)
    return OperandV;

  // Integer to integer casts
  if (SourceType->isIntegerTy() && TargetLLVMType->isIntegerTy()) {
    unsigned SourceBits = SourceType->getIntegerBitWidth();
    unsigned TargetBits = TargetLLVMType->getIntegerBitWidth();
    
    if (SourceBits < TargetBits) {
      // Use sign or zero extension based on source type
      if (!sourceTypeName.empty() && isUnsignedType(sourceTypeName)) {
        return Builder->CreateZExt(OperandV, TargetLLVMType, "casttmp");
      } else {
        return Builder->CreateSExt(OperandV, TargetLLVMType, "casttmp");
      }
    } else if (SourceBits > TargetBits) {
      return Builder->CreateTrunc(OperandV, TargetLLVMType, "casttmp");
    }
    return OperandV;
  }
  
  // Integer to float
  if (SourceType->isIntegerTy() && TargetLLVMType->isFloatingPointTy()) {
    // Use the source type name to use either signed or unsigned conversion
    if (!sourceTypeName.empty() && isUnsignedType(sourceTypeName)) {
      return Builder->CreateUIToFP(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateSIToFP(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  // Float to integer
  if (SourceType->isFloatingPointTy() && TargetLLVMType->isIntegerTy()) {
    // Use signed or unsigned conversion based on target type
    if (isUnsignedType(getTargetType())) {
      return Builder->CreateFPToUI(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateFPToSI(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  // Float to float (different precision)
  if (SourceType->isFloatingPointTy() && TargetLLVMType->isFloatingPointTy()) {
    if (SourceType->getPrimitiveSizeInBits() < TargetLLVMType->getPrimitiveSizeInBits()) {
      return Builder->CreateFPExt(OperandV, TargetLLVMType, "casttmp");
    } else {
      return Builder->CreateFPTrunc(OperandV, TargetLLVMType, "casttmp");
    }
  }
  
  return LogErrorV("Invalid cast between incompatible types");
}

// Generate code for function calls, including struct constructors
llvm::Value *CallExprAST::codegen() {
  // Check if this is a struct constructor call
  if (StructTypes.find(getCallee()) != StructTypes.end()) {
    // This is a struct constructor call
    std::string ConstructorName = getCallee() + "_new";
    llvm::Function *ConstructorF = TheModule->getFunction(ConstructorName);
    if (!ConstructorF)
      return LogErrorV(("Unknown struct constructor: " + getCallee()).c_str());
    
    // Set the type name for this expression
    setTypeName(getCallee());
    
    // Call the constructor
    if (ConstructorF->arg_size() != getArgs().size())
      return LogErrorV("Incorrect # arguments passed to constructor");
    
    std::vector<llvm::Value *> ArgsV;
    unsigned i = 0;
    for (auto &Arg : ConstructorF->args()) {
      llvm::Value *ArgVal = getArgs()[i]->codegen();
      if (!ArgVal)
        return nullptr;
      
      // Cast the argument to the expected parameter type
      ArgVal = castToType(ArgVal, Arg.getType());
      ArgsV.push_back(ArgVal);
      ++i;
    }
    return Builder->CreateCall(ConstructorF, ArgsV, "structtmp");
  }
  
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

// Generate code for return statements
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

// Generate code for block statements
llvm::Value *BlockStmtAST::codegen() {
  llvm::Value *Last = nullptr;
  // Generate code for each statement in the block
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
  
  // Check if at global scope
  llvm::Function *TheFunction = nullptr;
  bool isGlobal = false;
  
  if (Builder->GetInsertBlock()) {
    TheFunction = Builder->GetInsertBlock()->getParent();
    isGlobal = (TheFunction->getName() == "__anon_var_decl");
  } else {
    // No insertion point means at top level
    isGlobal = true;
    
    // Create or get the __anon_var_decl function for global variable initialization
    TheFunction = TheModule->getFunction("__anon_var_decl");
    if (!TheFunction) {
      // Create the function
      llvm::FunctionType *FT = llvm::FunctionType::get(
          llvm::Type::getVoidTy(*TheContext), false);
      TheFunction = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                          "__anon_var_decl", TheModule.get());
    }
    
    // Create a basic block if needed
    if (TheFunction->empty()) {
      llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
      Builder->SetInsertPoint(BB);
    } else {
      // Set insert point to the end of the existing block
      Builder->SetInsertPoint(&TheFunction->getEntryBlock());
    }
  }
  
  
  if (isGlobal) {
    // Create a global variable
    llvm::GlobalVariable *GV = new llvm::GlobalVariable(
        *TheModule, VarType, false, llvm::GlobalValue::ExternalLinkage,
        nullptr, getName());
    
    // Generate the initializer
    if (getInitializer()) {
      // For globals, need constant initializers
      // For now, create a zero initializer and store the actual value
      llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
      GV->setInitializer(ZeroInit);
      
      // Generate code to store the actual initial value
      llvm::Value *InitVal = getInitializer()->codegen();
      if (!InitVal)
        return nullptr;
      
      // Cast the initializer to the variable type if needed
      InitVal = castToType(InitVal, VarType, getType());
      
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
      InitVal = castToType(InitVal, VarType, getType());
      
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

// Generate code for expression statements
llvm::Value *ExpressionStmtAST::codegen() {
  // Just generate code for the expression
  return getExpression()->codegen();
}

// Generate code for for each statements
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

// Generate code for for loops with 'to' syntax
llvm::Value *ForLoopStmtAST::codegen() {
  // Evaluate the initial value
  llvm::Value *InitVal = InitExpr->codegen();
  if (!InitVal)
    return nullptr;
  
  // Evaluate limit if provided (may be null if using condition)
  llvm::Value *LimitVal = nullptr;
  if (LimitExpr) {
    LimitVal = LimitExpr->codegen();
    if (!LimitVal)
      return nullptr;
  }
  
  // Get the loop variable type
  llvm::Type *VarType = getTypeFromString(Type);
  if (!VarType)
    return LogErrorV("Unknown type in for loop");
  
  // Ensure values match the variable type
  if (VarType->isFloatingPointTy()) {
    // Convert init to float type if needed
    if (InitVal->getType()->isIntegerTy()) {
      InitVal = Builder->CreateSIToFP(InitVal, VarType, "initcast");
    } else if (InitVal->getType() != VarType) {
      InitVal = Builder->CreateFPCast(InitVal, VarType, "initcast");
    }
    
    // Convert limit to float type if needed
    if (LimitVal) {
      if (LimitVal->getType()->isIntegerTy()) {
        LimitVal = Builder->CreateSIToFP(LimitVal, VarType, "limitcast");
      } else if (LimitVal->getType() != VarType) {
        LimitVal = Builder->CreateFPCast(LimitVal, VarType, "limitcast");
      }
    }
  // For integer types
  } else if (VarType->isIntegerTy()) {
    // Convert init to int type if needed
    if (InitVal->getType()->isFloatingPointTy()) {
      InitVal = Builder->CreateFPToSI(InitVal, VarType, "initcast");
    } else if (InitVal->getType() != VarType) {
      InitVal = Builder->CreateIntCast(InitVal, VarType, true, "initcast");
    }
    
    // Convert limit to int type if needed
    if (LimitVal) {
      if (LimitVal->getType()->isFloatingPointTy()) {
        LimitVal = Builder->CreateFPToSI(LimitVal, VarType, "limitcast");
      } else if (LimitVal->getType() != VarType) {
        LimitVal = Builder->CreateIntCast(LimitVal, VarType, true, "limitcast");
      }
    }
  }
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create blocks for the loop
  llvm::BasicBlock *InitBB = Builder->GetInsertBlock();
  llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(*TheContext, "forcond", TheFunction);
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "forbody");
  llvm::BasicBlock *IncBB = llvm::BasicBlock::Create(*TheContext, "forinc");
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "forcont");
  
  // Create an alloca for the loop variable
  llvm::AllocaInst *VarAlloca = Builder->CreateAlloca(VarType, nullptr, VarName);
  Builder->CreateStore(InitVal, VarAlloca);
  
  // Determine if incrementing or decrementing based on init vs limit
  bool isIncrementing = true;
  if (LimitVal) {
    if (llvm::ConstantFP *InitFP = llvm::dyn_cast<llvm::ConstantFP>(InitVal)) {
      if (llvm::ConstantFP *LimitFP = llvm::dyn_cast<llvm::ConstantFP>(LimitVal)) {
        isIncrementing = InitFP->getValueAPF().convertToDouble() <= LimitFP->getValueAPF().convertToDouble();
      }
    } else if (llvm::ConstantInt *InitInt = llvm::dyn_cast<llvm::ConstantInt>(InitVal)) {
      if (llvm::ConstantInt *LimitInt = llvm::dyn_cast<llvm::ConstantInt>(LimitVal)) {
        isIncrementing = InitInt->getSExtValue() <= LimitInt->getSExtValue();
      }
    }
  }
  
  // Jump to the condition check
  Builder->CreateBr(CondBB);
  
  // Emit the condition check
  Builder->SetInsertPoint(CondBB);
  llvm::Value *VarVal = Builder->CreateLoad(VarType, VarAlloca, VarName);
  llvm::Value *CondV;
  
  if (CondExpr) {
    // Use the custom condition expression
    // First, make sure the loop variable is available for the condition
    llvm::Value *OldVal = NamedValues[VarName];
    NamedValues[VarName] = VarAlloca;
    
    CondV = CondExpr->codegen();
    
    // Restore old value (in case it shadows something)
    if (OldVal)
      NamedValues[VarName] = OldVal;
    
    if (!CondV)
      return nullptr;
    
    // Convert to boolean if needed
    if (!CondV->getType()->isIntegerTy(1)) {
      if (CondV->getType()->isIntegerTy(8)) {
        // For i8 bool, compare with 0
        CondV = Builder->CreateICmpNE(CondV, 
          llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
      } else if (CondV->getType()->isFloatingPointTy()) {
        CondV = Builder->CreateFCmpONE(CondV,
          llvm::ConstantFP::get(CondV->getType(), 0.0), "loopcond");
      } else if (CondV->getType()->isIntegerTy()) {
        CondV = Builder->CreateICmpNE(CondV,
          llvm::ConstantInt::get(CondV->getType(), 0), "loopcond");
      }
    }
  } else if (LimitVal) {
    // Use the standard limit-based condition
    if (VarType->isFloatingPointTy()) {
      if (isIncrementing) {
        CondV = Builder->CreateFCmpOLE(VarVal, LimitVal, "loopcond");
      } else {
        CondV = Builder->CreateFCmpOGE(VarVal, LimitVal, "loopcond");
      }
    } else {
      if (isIncrementing) {
        CondV = Builder->CreateICmpSLE(VarVal, LimitVal, "loopcond");
      } else {
        CondV = Builder->CreateICmpSGE(VarVal, LimitVal, "loopcond");
      }
    }
  } else {
    return LogErrorV("For loop must have either a limit or condition expression");
  }
  
  Builder->CreateCondBr(CondV, LoopBB, AfterBB);
  
  // Emit the loop body
  LoopBB->insertInto(TheFunction);
  Builder->SetInsertPoint(LoopBB);
  
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
  
  // Increment or decrement the loop variable
  VarVal = Builder->CreateLoad(VarType, VarAlloca, VarName);
  llvm::Value *NextVal;
  
  if (StepExpr) {
    // Use custom step expression
    llvm::Value *StepVal = StepExpr->codegen();
    if (!StepVal)
      return nullptr;
    
    // Ensure step has same type as variable
    if (StepVal->getType() != VarType) {
      if (VarType->isFloatingPointTy() && StepVal->getType()->isIntegerTy()) {
        StepVal = Builder->CreateSIToFP(StepVal, VarType, "stepcast");
      } else if (VarType->isIntegerTy() && StepVal->getType()->isDoubleTy()) {
        StepVal = Builder->CreateFPToSI(StepVal, VarType, "stepcast");
      }
    }
    
    // Apply the appropriate step operation
    switch (StepOp) {
    case '+':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFAdd(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateAdd(VarVal, StepVal, "nextval");
      }
      break;
    case '-':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFSub(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSub(VarVal, StepVal, "nextval");
      }
      break;
    case '*':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFMul(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateMul(VarVal, StepVal, "nextval");
      }
      break;
    case '/':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFDiv(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSDiv(VarVal, StepVal, "nextval");
      }
      break;
    case '%':
      if (VarType->isFloatingPointTy()) {
        NextVal = Builder->CreateFRem(VarVal, StepVal, "nextval");
      } else {
        NextVal = Builder->CreateSRem(VarVal, StepVal, "nextval");
      }
      break;
    default:
      return LogErrorV("Unknown step operation in for loop");
    }
  } else {
    // Default step of 1 or -1
    if (VarType->isFloatingPointTy()) {
      llvm::Value *Step = llvm::ConstantFP::get(VarType, isIncrementing ? 1.0 : -1.0);
      NextVal = Builder->CreateFAdd(VarVal, Step, "nextval");
    } else {
      llvm::Value *Step = llvm::ConstantInt::get(VarType, isIncrementing ? 1 : -1);
      NextVal = Builder->CreateAdd(VarVal, Step, "nextval");
    }
  }
  
  Builder->CreateStore(NextVal, VarAlloca);
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

// Generate code for if statements
llvm::Value *IfStmtAST::codegen() {
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV, 
      llvm::ConstantInt::get(CondV->getType(), 0), "ifcond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
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

// Generate code for while statements
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
  
  // Convert condition to i1 bool
  if (CondV->getType()->isIntegerTy(8)) {
    // For i8 bool, compare with 0 (any non-zero is true)
    CondV = Builder->CreateICmpNE(CondV, 
      llvm::ConstantInt::get(CondV->getType(), 0), "whilecond");
  } else if (!CondV->getType()->isIntegerTy(1)) {
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
  // Check if inside a loop
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
  // Check if inside a loop
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

// Generate code for function prototypes
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

// Generate code for function definitions
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

//===----------------------------------------------------------------------===//
// Struct Code Generation
//===----------------------------------------------------------------------===//

// Generate code for struct definitions
llvm::Type *StructAST::codegen() {
  // Check if this struct type already exists
  if (StructTypes.find(Name) != StructTypes.end()) {
    fprintf(stderr, "Error: Struct type already defined: %s\n", Name.c_str());
    return nullptr;
  }
  
  // Save the current insertion point to restore it after generating constructors
  auto SavedInsertBlock = Builder->GetInsertBlock();
  
  // Create the struct type
  std::vector<llvm::Type*> FieldTypes;
  std::vector<std::pair<std::string, unsigned>> FieldIndices;
  
  // Process fields
  unsigned FieldIndex = 0;
  std::map<std::string, std::string> FieldTypeMap;
  for (const auto &Field : Fields) {
    llvm::Type *FieldType = getTypeFromString(Field->getType());
    if (!FieldType) {
      fprintf(stderr, "Error: Unknown field type: %s\n", Field->getType().c_str());
      return nullptr;
    }
    FieldTypes.push_back(FieldType);
    FieldIndices.push_back({Field->getName(), FieldIndex++});
    FieldTypeMap[Field->getName()] = Field->getType();
  }
  
  // Create the struct type
  llvm::StructType *StructType = llvm::StructType::create(*TheContext, FieldTypes, Name);
  
  // Store the struct type
  StructTypes[Name] = StructType;
  StructFieldIndices[Name] = FieldIndices;
  StructFieldTypes[Name] = FieldTypeMap;
  
  // Generate constructor and other methods
  for (const auto &Method : Methods) {
    // For constructors, need to handle them specially
    if (Method->getProto()->getName() == Name) {
      // This is a constructor, generate it as a static method that returns a new instance
      std::string ConstructorName = Name + "_new";
      auto ConstructorProto = std::make_unique<PrototypeAST>(
          Name, ConstructorName, Method->getProto()->getArgs());
      
      // Generate the constructor function
      llvm::Function *ConstructorFunc = ConstructorProto->codegen();
      if (!ConstructorFunc) {
        return nullptr;
      }
      
      // Create a basic block to start insertion into
      llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", ConstructorFunc);
      Builder->SetInsertPoint(BB);
      
      // Allocate memory for the struct
      llvm::Value *StructPtr = Builder->CreateAlloca(StructType, nullptr, "struct_alloc");
      
      // Store the struct pointer as 'this'
      NamedValues["this"] = StructPtr;
      LocalTypes["this"] = Name;
      
      // Set up the argument values
      int i = 0;
      for (auto &Arg : ConstructorFunc->args()) {
        std::string ArgName = Method->getProto()->getArgs()[i].Name;
        
        // Create an alloca for this argument
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(
            Arg.getType(), nullptr, ArgName);
        
        // Store the initial value into the alloca
        Builder->CreateStore(&Arg, Alloca);
        
        // Add arguments to variable symbol table
        NamedValues[ArgName] = Alloca;
        LocalTypes[ArgName] = Method->getProto()->getArgs()[i].Type;
        i++;
      }
      
      // Generate the constructor body
      if (Method->getBody()->codegen()) {
        // Return the struct pointer
        Builder->CreateRet(StructPtr);
        
        // Validate the generated code
        llvm::verifyFunction(*ConstructorFunc);
      } else {
        ConstructorFunc->eraseFromParent();
        return nullptr;
      }
      
      // Clear local values
      NamedValues.clear();
      LocalTypes.clear();
    } else {
      // Regular method - generate as-is but with implicit 'this' parameter
      Method->codegen();
    }
  }
  
  // Restore the insertion point if had one, otherwise clear it
  if (SavedInsertBlock) {
    Builder->SetInsertPoint(SavedInsertBlock);
  } else {
    // No previous insertion point, at top level
    // Clear the insertion point so next top-level code creates its own context
    Builder->ClearInsertionPoint();
  }
  
  return StructType;
}

// Generate code for member access expressions
llvm::Value *MemberAccessExprAST::codegen() {
  // Get the type name of the object BEFORE codegen (might be empty for nested access)
  std::string ObjectTypeNameBefore = Object->getTypeName();
  
  // Generate code for the object
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr) {
    return nullptr;
  }
  
  // Get the type name of the object AFTER codegen (should be set now)
  std::string ObjectTypeName = Object->getTypeName();
  
  
  // If ObjectTypeName is empty but have a valid pointer, it might be a nested struct access
  // In that case, the ObjectPtr is already a pointer to a struct
  if (ObjectTypeName.empty() && ObjectPtr->getType()->isPointerTy()) {
    // In newer LLVM, pointers are opaque, so track the type differently
    // For now, rely on the type name being set correctly by the previous member access
    // This is a limitation that needs a better solution
  }
  
  // Find the struct type
  auto StructIt = StructTypes.find(ObjectTypeName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(ObjectTypeName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }
  
  // Find the specific field
  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }
  
  if (!FieldFound) {
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());
  }
  
  // Generate GEP to access the field
  std::vector<llvm::Value*> Indices = {
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))
  };
  
  llvm::Value *FieldPtr = Builder->CreateGEP(
      StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  
  // Load the field value
  // Get the field type from the struct definition
  llvm::Type *FieldType = StructIt->second->getElementType(FieldIndex);
  
  // Set the type name for this expression
  std::string FieldTypeName;
  auto FieldTypesIt = StructFieldTypes.find(ObjectTypeName);
  if (FieldTypesIt != StructFieldTypes.end()) {
    auto TypeIt = FieldTypesIt->second.find(MemberName);
    if (TypeIt != FieldTypesIt->second.end()) {
      FieldTypeName = TypeIt->second;
      setTypeName(FieldTypeName);
    }
  }
  
  // Load the field value
  return Builder->CreateLoad(FieldType, FieldPtr, MemberName);
}

llvm::Value *MemberAccessExprAST::codegen_ptr() {
  // Generate code for the object
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr) {
    return nullptr;
  }
  
  // Get the type name of the object
  std::string ObjectTypeName = Object->getTypeName();
  
  // Find the struct type
  auto StructIt = StructTypes.find(ObjectTypeName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(ObjectTypeName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }
  
  // Find the specific field
  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }
  
  if (!FieldFound) {
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());
  }
  
  // Generate GEP to access the field
  std::vector<llvm::Value*> Indices = {
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
    llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))
  };
  
  return Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
}

// Generate code for 'this' expressions
llvm::Value *ThisExprAST::codegen() {
  // Look up 'this' in the named values
  auto It = NamedValues.find("this");
  if (It == NamedValues.end()) {
    return LogErrorV("'this' can only be used inside struct methods");
  }
  
  // Get type from LocalTypes
  auto TypeIt = LocalTypes.find("this");
  if (TypeIt != LocalTypes.end()) {
    setTypeName(TypeIt->second);
  }
  
  return It->second;
}

// Generate pointer code for 'this' expressions
llvm::Value *ThisExprAST::codegen_ptr() {
  // 'this' is already a pointer
  return codegen();
}

// Generate code for switch statements
llvm::Value *SwitchStmtAST::codegen() {
  // Evaluate the switch condition
  llvm::Value *CondV = getCondition()->codegen();
  if (!CondV)
    return nullptr;
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  
  // Create basic blocks for each case and the exit block
  llvm::BasicBlock *ExitBB = llvm::BasicBlock::Create(*TheContext, "switch_end");
  
  // Create a switch instruction
  llvm::SwitchInst *Switch = nullptr;
  llvm::BasicBlock *DefaultBB = nullptr;
  
  // First pass: collect all case values and create basic blocks
  std::vector<std::pair<llvm::BasicBlock*, const CaseAST*>> CaseBlocks;
  
  for (const auto &Case : getCases()) {
    llvm::BasicBlock *CaseBB = llvm::BasicBlock::Create(*TheContext, 
        Case->isDefault() ? "default" : "case", TheFunction);
    CaseBlocks.push_back({CaseBB, Case.get()});
    
    if (Case->isDefault()) {
      DefaultBB = CaseBB;
    }
  }
  
  // Create switch instruction with default case or exit block
  llvm::BasicBlock *SwitchDefaultBB = DefaultBB ? DefaultBB : ExitBB;
  Switch = Builder->CreateSwitch(CondV, SwitchDefaultBB, getCases().size());
  
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
  
  // Second pass: generate code for each case body
  for (size_t i = 0; i < CaseBlocks.size(); ++i) {
    llvm::BasicBlock *CaseBB = CaseBlocks[i].first;
    const CaseAST *Case = CaseBlocks[i].second;
    
    // Only add the block to function if it's not already added (default case might be)
    if (CaseBB->getParent() == nullptr) {
      CaseBB->insertInto(TheFunction);
    }
    
    Builder->SetInsertPoint(CaseBB);
    
    // Generate case body
    if (Case->getBody()) {
      llvm::Value *BodyV = Case->getBody()->codegen();
      if (!BodyV)
        return nullptr;
    }
    
    // No fall-through - always branch to exit (unless there's a return/break)
    if (!Builder->GetInsertBlock()->getTerminator()) {
      Builder->CreateBr(ExitBB);
    }
  }
  
  // Add exit block and set it as insertion point
  ExitBB->insertInto(TheFunction);
  Builder->SetInsertPoint(ExitBB);
  
  // Switch statements don't return a value
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

// Generate code for switch expressions
llvm::Value *SwitchExprAST::codegen() {
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
    
    PhiValues.push_back({CaseResult, CaseBB});
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