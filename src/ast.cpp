#include "ast.h"
#include <iostream>

#include "compiler_session.h"
#include "codegen_context.h"

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

#include <cmath>
#include <climits>
#include <map>
#include <ranges>
#include <string_view>
#include <optional>
#include <algorithm>
#include <cctype>

#define CG currentCodegen()
#define TheContext (CG.llvmContext)
#define TheModule (CG.module)
#define Builder (CG.builder)
#define NamedValues (CG.namedValues)
#define GlobalValues (CG.globalValues)
#define GlobalTypes (CG.globalTypes)
#define LocalTypes (CG.localTypes)
#define StructTypes (CG.structTypes)
#define ArraySizes (CG.arraySizes)
#define StructFieldIndices (CG.structFieldIndices)
#define StructFieldTypes (CG.structFieldTypes)
#define LoopExitBlocks (CG.loopExitBlocks)
#define LoopContinueBlocks (CG.loopContinueBlocks)

llvm::Value *LogErrorV(const char *Str);

static unsigned computePointerDepth(const std::string &typeName) {
  size_t atPos = typeName.find('@');
  if (atPos == std::string::npos)
    return 0;

  size_t endPos = typeName.find_first_of("[]", atPos);
  std::string suffix = typeName.substr(atPos + 1, endPos == std::string::npos ? std::string::npos : endPos - atPos - 1);
  if (suffix.empty())
    return 1;

  unsigned depth = 1;
  try {
    depth = static_cast<unsigned>(std::stoul(suffix));
    if (depth == 0)
      depth = 1;
  } catch (...) {
    depth = 1;
  }
  return depth;
}

static bool isArrayTypeName(const std::string &typeName) {
  return typeName.size() > 2 && typeName.substr(typeName.size() - 2) == "[]";
}

static std::string stripNullableAnnotations(const std::string &typeName) {
  std::string cleaned;
  cleaned.reserve(typeName.size());
  for (char c : typeName) {
    if (c != '?')
      cleaned.push_back(c);
  }
  return cleaned;
}

static std::string ensureOuterNullable(const std::string &typeName) {
  if (!typeName.empty() && typeName.back() == '?')
    return typeName;
  return typeName + "?";
}

static std::string typeNameFromInfo(const TypeInfo &info) {
  if (info.pointerDepth > 0)
    return info.typeName;

  if (info.isArray) {
    std::string elementBase = info.typeName;
    if (elementBase.size() > 2 && elementBase.substr(elementBase.size() - 2) == "[]")
      elementBase = elementBase.substr(0, elementBase.size() - 2);
    if (info.elementNullable && (elementBase.empty() || elementBase.back() != '?'))
      elementBase += "?";

    std::string result = elementBase + "[]";
    if (info.isNullable)
      result = ensureOuterNullable(result);
    return result;
  }

  std::string result = info.typeName;
  if (info.isNullable)
    result = ensureOuterNullable(result);
  return result;
}

struct ParsedTypeDescriptor {
  std::string sanitized;
  bool isArray = false;
  bool isNullable = false;
  bool elementNullable = false;
  unsigned pointerDepth = 0;
};

static ParsedTypeDescriptor parseTypeString(const std::string &typeName) {
  ParsedTypeDescriptor desc;
  std::string sanitized;
  sanitized.reserve(typeName.size());

  bool pendingNullable = false;
  bool arraySeen = false;

  for (size_t i = 0; i < typeName.size(); ++i) {
    char c = typeName[i];

    if (c == '?') {
      pendingNullable = true;
      continue;
    }

    if (c == '@') {
      sanitized.push_back(c);
      ++i;
      while (i < typeName.size() && std::isdigit(static_cast<unsigned char>(typeName[i]))) {
        sanitized.push_back(typeName[i]);
        ++i;
      }
      if (i > 0)
        --i;
      if (pendingNullable) {
        desc.isNullable = true;
        pendingNullable = false;
      }
      continue;
    }

    if (c == '[' && i + 1 < typeName.size() && typeName[i + 1] == ']') {
      sanitized.append("[]");
      arraySeen = true;
      if (pendingNullable) {
        desc.elementNullable = true;
        pendingNullable = false;
      }
      ++i;
      continue;
    }

    sanitized.push_back(c);
  }

  if (pendingNullable)
    desc.isNullable = true;

  desc.sanitized = sanitized;
  desc.isArray = arraySeen || isArrayTypeName(desc.sanitized);
  desc.pointerDepth = computePointerDepth(desc.sanitized);
  if (desc.pointerDepth > 0 && !desc.isArray)
    desc.isNullable = true;

  return desc;
}

static bool exprIsNullLiteral(const ExprAST *expr) {
  return dynamic_cast<const NullExprAST*>(expr) != nullptr;
}

static const ExprAST *unwrapRefExpr(const ExprAST *expr) {
  if (const auto *Ref = dynamic_cast<const RefExprAST*>(expr))
    return Ref->getOperand();
  return expr;
}

static bool typeAllowsNull(const TypeInfo &info) {
  return info.isNullable || (!info.isArray && info.pointerDepth > 0);
}

static bool typeAllowsNull(const ParsedTypeDescriptor &desc) {
  return desc.isNullable || (!desc.isArray && desc.pointerDepth > 0);
}

static bool expressionIsNullable(const ExprAST *expr) {
  if (!expr)
    return false;
  if (exprIsNullLiteral(expr))
    return true;
  if (const auto *Binary = dynamic_cast<const BinaryExprAST *>(expr)) {
    const std::string &Op = Binary->getOp();
    if (Op == "\?\?" || Op == "\?\?=") {
      const ExprAST *rhsExpr = unwrapRefExpr(Binary->getRHS());
      if (!expressionIsNullable(rhsExpr))
        return false;
    }
  }
  std::string typeName = expr->getTypeName();
  if (typeName.empty())
    return false;
  ParsedTypeDescriptor desc = parseTypeString(typeName);
  return typeAllowsNull(desc);
}

static bool validateNullableAssignment(const TypeInfo &targetInfo,
                                       const ExprAST *expr,
                                       const std::string &targetDescription) {
  if (!typeAllowsNull(targetInfo) && expressionIsNullable(expr)) {
    LogErrorV(("Cannot assign nullable value to non-nullable " + targetDescription).c_str());
    return false;
  }
  return true;
}

static TypeInfo makeTypeInfo(std::string typeName,
                             RefStorageClass storage = RefStorageClass::None,
                             bool isMutable = true,
                             bool declaredRef = false) {
  TypeInfo info;
  info.typeName = std::move(typeName);
  info.pointerDepth = computePointerDepth(info.typeName);
  info.isArray = isArrayTypeName(info.typeName);
  info.isNullable = info.pointerDepth > 0 && !info.isArray;
  info.elementNullable = false;
  info.refStorage = storage;
  info.isMutable = isMutable;
  info.declaredRef = declaredRef;
  return info;
}

static const TypeInfo *lookupLocalTypeInfo(const std::string &name) {
  auto It = LocalTypes.find(name);
  if (It != LocalTypes.end())
    return &It->second;
  return nullptr;
}

static const TypeInfo *lookupGlobalTypeInfo(const std::string &name) {
  auto It = GlobalTypes.find(name);
  if (It != GlobalTypes.end())
    return &It->second;
  return nullptr;
}

static const TypeInfo *lookupTypeInfo(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info;
  return lookupGlobalTypeInfo(name);
}

static void rememberGlobalType(const std::string &name, TypeInfo info) {
  GlobalTypes[name] = std::move(info);
}

static void rememberLocalType(const std::string &name, TypeInfo info) {
  LocalTypes[name] = std::move(info);
}

static TypeInfo runtimeTypeFrom(const TypeInfo &declared, RefStorageClass storage, bool declaredRefOverride) {
  TypeInfo info = declared;
  info.refStorage = storage;
  info.declaredRef = declaredRefOverride;
  return info;
}

static bool isDeclaredRefGlobal(const std::string &name) {
  if (const auto *info = lookupGlobalTypeInfo(name))
    return info->declaredRef;
  return false;
}

static bool isDeclaredRefLocal(const std::string &name) {
  if (const auto *info = lookupLocalTypeInfo(name))
    return info->declaredRef;
  return false;
}

// Initialize LLVM
void InitializeModule() {
  CG.reset();
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
constexpr bool isSignedType(std::string_view TypeStr) {
  // Signed types: int, sbyte, short, long, char (in C, char is signed by default)
  // Note: schar is "short char" (8-bit), not signed char
  return TypeStr == "int" || TypeStr == "sbyte" || TypeStr == "short" ||
         TypeStr == "long" || TypeStr == "char";
}

// Helper to check if a type name represents an unsigned type
constexpr bool isUnsignedType(std::string_view TypeStr) {
  // Unsigned types: byte, ushort, uint, ulong
  // Note: schar and lchar are character types, treating as unsigned
  return TypeStr == "byte" || TypeStr == "ushort" || TypeStr == "uint" ||
         TypeStr == "ulong" || TypeStr == "schar" || TypeStr == "lchar";
}

static std::string sanitizeBaseTypeName(std::string_view typeName) {
  if (typeName.empty())
    return {};

  ParsedTypeDescriptor desc = parseTypeString(std::string(typeName));
  return desc.sanitized;
}

static std::optional<bool> unsignedHintFromTypeName(const std::string &typeName) {
  if (typeName.empty())
    return std::nullopt;

  if (typeName == "char" || typeName == "lchar")
    return true;

  if (isUnsignedType(typeName))
    return true;

  if (isSignedType(typeName))
    return false;

  return std::nullopt;
}

// Type conversion helper
llvm::Type *getTypeFromString(const std::string &TypeStr) {
  std::string CleanType = stripNullableAnnotations(TypeStr);
  if (CleanType == "int")
    return llvm::Type::getInt32Ty(*TheContext);
  else if (CleanType == "float")
    return llvm::Type::getFloatTy(*TheContext);
  else if (CleanType == "double")
    return llvm::Type::getDoubleTy(*TheContext);
  else if (CleanType == "char")
    return llvm::Type::getInt16Ty(*TheContext);
  else if (CleanType == "bool")
    return llvm::Type::getInt8Ty(*TheContext);
  else if (CleanType == "void")
    return llvm::Type::getVoidTy(*TheContext);
  else if (CleanType == "string")
    return llvm::PointerType::get(*TheContext, 0); // Strings as opaque pointer
  // New sized integer types
  else if (CleanType == "byte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit unsigned
  else if (CleanType == "sbyte")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit signed
  else if (CleanType == "short")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit signed
  else if (CleanType == "ushort")
    return llvm::Type::getInt16Ty(*TheContext);  // 16-bit unsigned
  else if (CleanType == "uint")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit unsigned
  else if (CleanType == "long")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit signed
  else if (CleanType == "ulong")
    return llvm::Type::getInt64Ty(*TheContext);  // 64-bit unsigned
  // Sized character types
  else if (CleanType == "schar")
    return llvm::Type::getInt8Ty(*TheContext);   // 8-bit character
  else if (CleanType == "lchar")
    return llvm::Type::getInt32Ty(*TheContext);  // 32-bit character (Unicode)
  else if (CleanType.size() > 2 && CleanType.substr(CleanType.size() - 2) == "[]") {
    // Array type: return the array struct type {ptr, size}
    std::string ElementType = CleanType.substr(0, CleanType.size() - 2);
    llvm::Type *ElemType = getTypeFromString(ElementType);
    if (ElemType) {
      // Create struct type for array: { element_ptr, size }
      llvm::Type *PtrType = llvm::PointerType::get(*TheContext, 0);
      llvm::Type *SizeType = llvm::Type::getInt32Ty(*TheContext);
      return llvm::StructType::get(*TheContext, {PtrType, SizeType});
    }
    return nullptr;
  }
  // Check for pointer types (e.g., "int@", "float@2")
  else if (CleanType.find('@') != std::string::npos) {
    size_t atPos = CleanType.find('@');
    std::string BaseType = CleanType.substr(0, atPos);

    // Parse pointer level (default is 1)
    int level = 1;
    if (atPos + 1 < CleanType.size()) {
      std::string levelStr = CleanType.substr(atPos + 1);
      if (!levelStr.empty()) {
        level = std::stoi(levelStr);
      }
    }

    // Get the base type
    llvm::Type *BaseLLVMType = getTypeFromString(BaseType);
    if (!BaseLLVMType)
      return nullptr;

    // Create nested pointer types based on level
    llvm::Type *Result = BaseLLVMType;
    for (int i = 0; i < level; i++) {
      Result = llvm::PointerType::get(*TheContext, 0);
    }

    return Result;
  }

  // Check if it's a struct type
  auto structIt = StructTypes.find(CleanType);
  if (structIt != StructTypes.end()) {
    return llvm::PointerType::get(*TheContext, 0); // Struct instances are opaque pointers
  }

  return nullptr;
}

// Helper to get array struct type for a given element type
llvm::StructType *getArrayStructType(llvm::Type *ElementType) {
  llvm::Type *PtrType = llvm::PointerType::get(*TheContext, 0);
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
  if (Literal.isInteger()) {
    const llvm::APInt &value = Literal.getIntegerValue();
    unsigned requiredBits = Literal.getRequiredBitWidth();

    if (requiredBits <= 32 && Literal.fitsInSignedBits(32)) {
      setTypeName("int");
      llvm::APInt as32 = value.sextOrTrunc(32);
      return llvm::ConstantInt::get(*TheContext, as32);
    }

    if (requiredBits <= 64 && Literal.fitsInSignedBits(64)) {
      setTypeName("long");
      llvm::APInt as64 = value.sextOrTrunc(64);
      return llvm::ConstantInt::get(*TheContext, as64);
    }

    if (requiredBits <= 64 && Literal.fitsInUnsignedBits(64)) {
      setTypeName("ulong");
      llvm::APInt asUnsigned64 = value.zextOrTrunc(64);
      return llvm::ConstantInt::get(*TheContext, asUnsigned64);
    }

    setTypeName("double");
    return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Literal.toDouble()));
  }

  setTypeName("double");
  llvm::APFloat value = Literal.getFloatValue();
  bool losesInfo = false;
  value.convert(llvm::APFloat::IEEEdouble(), llvm::APFloat::rmNearestTiesToEven, &losesInfo);
  (void)losesInfo;
  return llvm::ConstantFP::get(*TheContext, value);
}

// Generate code for numeric literal with target type context
llvm::Value *NumberExprAST::codegen_with_target(llvm::Type *TargetType) {
  if (TargetType->isFloatingPointTy()) {
    setTypeName(TargetType->isFloatTy() ? "float" : "double");
    return llvm::ConstantFP::get(TargetType, Literal.toDouble());
  }

  if (TargetType->isIntegerTy() && Literal.isInteger()) {
    const llvm::APInt &value = Literal.getIntegerValue();
    unsigned bitWidth = TargetType->getIntegerBitWidth();

    if (!value.isIntN(bitWidth))
      return codegen();

    llvm::APInt adjusted = value;
    if (value.getBitWidth() != bitWidth)
      adjusted = value.zextOrTrunc(bitWidth);

    if (bitWidth == 8) setTypeName("byte");
    else if (bitWidth == 16) setTypeName("short");
    else if (bitWidth == 32) setTypeName("int");
    else if (bitWidth == 64) {
      setTypeName(value.isSignedIntN(64) ? "long" : "ulong");
    }

    return llvm::ConstantInt::get(*TheContext, adjusted);
  }

  return codegen();
}

// Generate code for boolean expressions
llvm::Value *BoolExprAST::codegen() {
  setTypeName("bool");
  return llvm::ConstantInt::get(*TheContext, llvm::APInt(8, getValue() ? 1 : 0));
}

// Generate code for null expressions
llvm::Value *NullExprAST::codegen() {
  llvm::PointerType *OpaquePtr = llvm::PointerType::get(*TheContext, 0);
  setTypeName("null");
  return llvm::ConstantPointerNull::get(OpaquePtr);
}

static llvm::Value *emitUTF16StringLiteral(const std::string &value) {
  static std::map<std::string, llvm::GlobalVariable*> StringLiteralCache;

  llvm::GlobalVariable *global = nullptr;

  if (auto it = StringLiteralCache.find(value); it != StringLiteralCache.end()) {
    global = it->second;
  } else {
    std::vector<llvm::Constant *> charValues;
    charValues.reserve(value.size() + 1);

    // TODO: Proper UTF-8 to UTF-16 conversion should be implemented here
    // For now, treat each byte as a separate 16-bit character.
    for (unsigned char c : value) {
      charValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt16Ty(*TheContext),
                                                 static_cast<uint16_t>(c)));
    }

    // Append null terminator
    charValues.push_back(llvm::ConstantInt::get(llvm::Type::getInt16Ty(*TheContext), 0));

    auto *arrayType = llvm::ArrayType::get(llvm::Type::getInt16Ty(*TheContext), charValues.size());
    auto *stringArray = llvm::ConstantArray::get(arrayType, charValues);

    global = new llvm::GlobalVariable(*TheModule, arrayType, true,
                                      llvm::GlobalValue::PrivateLinkage,
                                      stringArray, "str");
    StringLiteralCache.emplace(value, global);
  }

  auto *arrayType = llvm::cast<llvm::ArrayType>(global->getValueType());

  llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
  return Builder->CreateInBoundsGEP(arrayType, global, {zero, zero}, "strptr");
}

// Generate code for string literals
llvm::Value *StringExprAST::codegen() {
  setTypeName("string");
  return emitUTF16StringLiteral(getValue());
}

static llvm::FunctionCallee getConcatStringsFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *stringPtrPtrTy = llvm::PointerType::get(*TheContext, 0);
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {stringPtrPtrTy, int32Ty},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_concat_strings", fnType);
}

static llvm::FunctionCallee getIntToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
  llvm::Type *boolTy = llvm::Type::getInt1Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {int64Ty, boolTy},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_int64", fnType);
}

static llvm::FunctionCallee getDoubleToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *doubleTy = llvm::Type::getDoubleTy(*TheContext);
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::Type *boolTy = llvm::Type::getInt1Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy,
                                                       {doubleTy, int32Ty, boolTy},
                                                       false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_double", fnType);
}

static llvm::FunctionCallee getCharToStringFunction() {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  llvm::Type *int32Ty = llvm::Type::getInt32Ty(*TheContext);
  llvm::FunctionType *fnType = llvm::FunctionType::get(stringPtrTy, {int32Ty}, false);
  return TheModule->getOrInsertFunction("__hybrid_string_from_char32", fnType);
}

static llvm::Value *ensureStringPointer(llvm::Value *value) {
  llvm::Type *stringPtrTy = getTypeFromString("string");
  if (value->getType() == stringPtrTy)
    return value;
  if (value->getType()->isPointerTy())
    return Builder->CreateBitCast(value, stringPtrTy, "str.cast");
  return value;
}

static llvm::Value *convertValueToString(llvm::Value *value,
                                         const std::string &typeName,
                                         const std::optional<std::string> &formatSpec) {
  const bool isFloatType = value->getType()->isFloatingPointTy() ||
                           typeName == "float" || typeName == "double";

  if (formatSpec.has_value() && !isFloatType) {
    return LogErrorV("Format specifiers are only supported for floating point interpolation");
  }

  if (typeName == "string") {
    return ensureStringPointer(value);
  }

  if (typeName == "bool" || value->getType()->isIntegerTy(1)) {
    llvm::Value *cond = value;
    if (!cond->getType()->isIntegerTy(1)) {
      cond = Builder->CreateICmpNE(
          cond,
          llvm::ConstantInt::get(cond->getType(), 0),
          "boolcmp");
    }
    llvm::Value *trueStr = emitUTF16StringLiteral("true");
    llvm::Value *falseStr = emitUTF16StringLiteral("false");
    return Builder->CreateSelect(cond, trueStr, falseStr, "boolstr");
  }

  if (typeName == "char" || typeName == "schar" || typeName == "lchar") {
    llvm::Type *targetType = llvm::Type::getInt32Ty(*TheContext);
    if (!value->getType()->isIntegerTy(32)) {
      if (typeName == "schar")
        value = Builder->CreateSExtOrTrunc(value, targetType, "charext");
      else
        value = Builder->CreateZExtOrTrunc(value, targetType, "charext");
    }
    return Builder->CreateCall(getCharToStringFunction(), {value}, "charstr");
  }

  if (value->getType()->isFloatingPointTy()) {
    int precision = 0;
    bool hasPrecision = false;
    if (formatSpec.has_value()) {
      try {
        precision = std::stoi(*formatSpec);
        hasPrecision = true;
      } catch (...) {
        return LogErrorV("Invalid floating-point format specifier in interpolated string");
      }
    }

    if (!value->getType()->isDoubleTy()) {
      value = Builder->CreateFPExt(value, llvm::Type::getDoubleTy(*TheContext), "fpexttmp");
    }

    llvm::Value *precisionVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), precision);
    llvm::Value *hasPrecisionVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), hasPrecision ? 1 : 0);
    return Builder->CreateCall(getDoubleToStringFunction(),
                               {value, precisionVal, hasPrecisionVal},
                               "floatstr");
  }

  if (value->getType()->isIntegerTy()) {
    bool isUnsigned = isUnsignedType(typeName);
    if (!isUnsigned && !isSignedType(typeName) && typeName != "") {
      // Unrecognized integer type name; default to signed.
      isUnsigned = false;
    }

    unsigned bitWidth = value->getType()->getIntegerBitWidth();
    llvm::Type *int64Ty = llvm::Type::getInt64Ty(*TheContext);
    if (bitWidth != 64) {
      if (isUnsigned)
        value = Builder->CreateZExtOrTrunc(value, int64Ty, "zexttmp");
      else
        value = Builder->CreateSExtOrTrunc(value, int64Ty, "sexttmp");
    }

    llvm::Value *isUnsignedVal = llvm::ConstantInt::get(llvm::Type::getInt1Ty(*TheContext), isUnsigned ? 1 : 0);
    return Builder->CreateCall(getIntToStringFunction(), {value, isUnsignedVal}, "intstr");
  }

  if (typeName == "string")
    return ensureStringPointer(value);

  return LogErrorV("Unsupported expression type in string interpolation");
}

llvm::Value *InterpolatedStringExprAST::codegen() {
  std::vector<llvm::Value *> segmentValues;
  segmentValues.reserve(Segments.size());

  for (const auto &segment : Segments) {
    if (segment.isLiteral()) {
      segmentValues.push_back(emitUTF16StringLiteral(segment.getLiteral()));
      continue;
    }

    ExprAST *expr = segment.getExpression();
    if (!expr)
      return LogErrorV("Invalid expression in interpolated string segment");

    llvm::Value *exprValue = expr->codegen();
    if (!exprValue)
      return nullptr;

    llvm::Value *asString = convertValueToString(exprValue, expr->getTypeName(), segment.getFormatSpec());
    if (!asString)
      return nullptr;

    segmentValues.push_back(asString);
  }

  if (segmentValues.empty()) {
    setTypeName("string");
    return emitUTF16StringLiteral("");
  }

  if (segmentValues.size() == 1) {
    llvm::Value *single = ensureStringPointer(segmentValues.front());
    setTypeName("string");
    return single;
  }

  llvm::Type *stringPtrTy = getTypeFromString("string");
  unsigned count = static_cast<unsigned>(segmentValues.size());
  llvm::ArrayType *arrayTy = llvm::ArrayType::get(stringPtrTy, count);
  llvm::AllocaInst *arrayAlloca = Builder->CreateAlloca(arrayTy, nullptr, "interpSegments");

  for (unsigned i = 0; i < count; ++i) {
    llvm::Value *indices[] = {
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), i)};
    llvm::Value *elementPtr = Builder->CreateInBoundsGEP(arrayTy, arrayAlloca, indices, "interp.segptr");
    llvm::Value *segmentPtr = ensureStringPointer(segmentValues[i]);
    Builder->CreateStore(segmentPtr, elementPtr);
  }

  llvm::Value *arrayPtr = Builder->CreateInBoundsGEP(
      arrayTy, arrayAlloca,
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0)},
      "interp.base");

  llvm::FunctionCallee concatFunc = getConcatStringsFunction();
  llvm::Value *countVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), count);
  llvm::Value *result = Builder->CreateCall(concatFunc, {arrayPtr, countVal}, "interp.result");

  setTypeName("string");
  return result;
}

// Generate code for character literals
llvm::Value *CharExprAST::codegen_with_target(llvm::Type *TargetType,
                                              const std::string &TargetTypeName) {
  uint32_t value = getValue();

  auto emitDefault = [&]() -> llvm::Value * {
    if (value > 0xFFFF) {
      setTypeName("lchar");
      return llvm::ConstantInt::get(*TheContext, llvm::APInt(32, value));
    }
    setTypeName("char");
    return llvm::ConstantInt::get(*TheContext, llvm::APInt(16, value));
  };

  if (!TargetType || !TargetType->isIntegerTy())
    return emitDefault();

  unsigned bitWidth = TargetType->getIntegerBitWidth();
  if (bitWidth != 8 && bitWidth != 16 && bitWidth != 32)
    return emitDefault();

  if ((bitWidth == 32 && value > 0x10FFFF) ||
      (bitWidth == 16 && value > 0xFFFF) ||
      (bitWidth == 8 && value > 0xFF))
    return emitDefault();

  std::string cleanTarget = sanitizeBaseTypeName(TargetTypeName);
  std::optional<bool> unsignedHint = unsignedHintFromTypeName(cleanTarget);

  if (bitWidth == 8) {
    if (unsignedHint.has_value() && !unsignedHint.value() && value > 0x7F)
      return emitDefault();
  } else if (bitWidth == 16) {
    if (cleanTarget == "short" && value > 0x7FFF)
      return emitDefault();
  } else if (bitWidth == 32) {
    if (unsignedHint.has_value() && !unsignedHint.value() && value > static_cast<uint32_t>(INT32_MAX))
      return emitDefault();
  }

  llvm::APInt literal(bitWidth, value, /*isSigned=*/false);
  llvm::Value *constant = llvm::ConstantInt::get(*TheContext, literal);

  if (!cleanTarget.empty()) {
    setTypeName(cleanTarget);
  } else if (bitWidth == 8) {
    setTypeName(unsignedHint.value_or(true) ? "schar" : "sbyte");
  } else if (bitWidth == 16) {
    setTypeName("char");
  } else {
    setTypeName("lchar");
  }

  return constant;
}

llvm::Value *CharExprAST::codegen() {
  if (getValue() > 0xFFFF)
    return codegen_with_target(llvm::Type::getInt32Ty(*TheContext), "lchar");
  return codegen_with_target(llvm::Type::getInt16Ty(*TheContext), "char");
}

// Forward declaration for castToType
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType);
llvm::Value* castToType(llvm::Value* value, llvm::Type* targetType, const std::string& targetTypeName);

// Helper to check if types are compatible for implicit conversion
// No implicit conversion between different sized integers
bool areTypesCompatible(llvm::Type* type1, llvm::Type* type2) {
  if (type1 == type2)
    return true;
  
  if (type1->isIntegerTy() && type2->isIntegerTy()) {
    const bool isBool1 = type1->isIntegerTy(1);
    const bool isBool2 = type2->isIntegerTy(1);
    if (isBool1 || isBool2)
      return isBool1 && isBool2;
    return true;
  }
  
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
llvm::Value *ArrayExprAST::codegen_with_element_target(llvm::Type *TargetElementType,
                                                       const std::string &TargetElementTypeName) {
  llvm::Type *ElemType = TargetElementType;
  std::string elementTypeName = TargetElementTypeName;

  if (!ElemType) {
    ElemType = getTypeFromString(getElementType());
    elementTypeName = getElementType();
  }

  if (!ElemType)
    return LogErrorV("Unknown element type in array literal");

  std::string cleanElementTypeName = sanitizeBaseTypeName(elementTypeName);
  if (cleanElementTypeName.empty())
    cleanElementTypeName = elementTypeName;

  // Get the array size
  size_t ArraySize = getElements().size();
  bool useGlobalStorage = false;
  llvm::Function *CurrentFunction = nullptr;
  if (auto *CurrentBB = Builder->GetInsertBlock()) {
    CurrentFunction = CurrentBB->getParent();
    if (CurrentFunction && CurrentFunction->getName() == "__anon_var_decl") {
      useGlobalStorage = true;
    }
  }

  llvm::Value *ArrayDataPtr = nullptr;
  llvm::ArrayType *ArrayMemType = llvm::ArrayType::get(ElemType, ArraySize);

  static unsigned GlobalArrayLiteralCounter = 0;

  if (useGlobalStorage) {
    std::string GlobalName = "__array_literal_" + std::to_string(GlobalArrayLiteralCounter++);
    llvm::GlobalVariable *GlobalArray = new llvm::GlobalVariable(
        *TheModule, ArrayMemType, false, llvm::GlobalValue::PrivateLinkage,
        llvm::ConstantAggregateZero::get(ArrayMemType), GlobalName);

    llvm::Value *Zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
    ArrayDataPtr = Builder->CreateInBoundsGEP(
        ArrayMemType, GlobalArray, {Zero, Zero}, GlobalName + ".ptr");
  } else {
    ArrayDataPtr = Builder->CreateAlloca(
        ElemType, llvm::ConstantInt::get(*TheContext, llvm::APInt(32, ArraySize)), "arraytmp");
  }

  // Store each element
  for (size_t i = 0; i < ArraySize; ++i) {
    llvm::Value *ElemVal = nullptr;
    if (auto *Num = dynamic_cast<NumberExprAST*>(getElements()[i].get())) {
      ElemVal = Num->codegen_with_target(ElemType);
    } else if (auto *Char = dynamic_cast<CharExprAST*>(getElements()[i].get())) {
      ElemVal = Char->codegen_with_target(ElemType, cleanElementTypeName);
    } else {
      ElemVal = getElements()[i]->codegen();
    }

    if (!ElemVal)
      return nullptr;

    // Cast element to the correct type if needed
    ElemVal = castToType(ElemVal, ElemType, cleanElementTypeName);

    // Calculate the address of the i-th element
    llvm::Value *Idx = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, i));
    llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayDataPtr, Idx, "elemptr");

    // Store the element
    Builder->CreateStore(ElemVal, ElemPtr);
  }

  // Create the array struct {ptr, size}
  llvm::StructType *ArrayStructType = getArrayStructType(ElemType);
  llvm::AllocaInst *ArrayStruct = Builder->CreateAlloca(ArrayStructType, nullptr, "arrayStruct");

  // Store the pointer (field 0)
  llvm::Value *PtrField = Builder->CreateStructGEP(ArrayStructType, ArrayStruct, 0, "ptrField");
  llvm::Value *ArrayPtrForStruct =
      Builder->CreateBitCast(ArrayDataPtr, llvm::PointerType::get(*TheContext, 0), "array.ptrcast");
  Builder->CreateStore(ArrayPtrForStruct, PtrField);

  // Store the size (field 1)
  llvm::Value *SizeField = Builder->CreateStructGEP(ArrayStructType, ArrayStruct, 1, "sizeField");
  llvm::Value *SizeVal = llvm::ConstantInt::get(*TheContext, llvm::APInt(32, ArraySize));
  Builder->CreateStore(SizeVal, SizeField);

  std::string arrayTypeName = cleanElementTypeName;
  if (!arrayTypeName.empty())
    arrayTypeName += "[]";
  setTypeName(arrayTypeName);

  // Return the struct
  return Builder->CreateLoad(ArrayStructType, ArrayStruct, "arrayStructVal");
}

llvm::Value *ArrayExprAST::codegen() {
  return codegen_with_element_target(nullptr, getElementType());
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

  bool elementIsNullable = false;
  std::string arrayTypeName = getArray()->getTypeName();
  if (!arrayTypeName.empty()) {
    ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
    if (arrayDesc.isNullable) {
      return LogErrorV("Cannot index nullable array without null-safe operator");
    }
    elementIsNullable = arrayDesc.elementNullable;
  }
  
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
  llvm::Value *ArraySize = nullptr; // Track the array size for bounds checking
  std::string elemTypeStr = "int"; // Track the element type string for setTypeName

  if (ArrayType->isStructTy()) {
    // New array representation: extract pointer from struct
    llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
    if (StructType->getNumElements() == 2) {
      // Extract the pointer (field 0)
      ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");

      // Extract the size (field 1) for bounds checking
      ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "arraySize");
      
      // Determine element type from the context
      // First, check if it's from a direct array literal
      if (ArrayExprAST *ArrayExpr = dynamic_cast<ArrayExprAST*>(getArray())) {
        elemTypeStr = ArrayExpr->getElementType();
        ElemType = getTypeFromString(elemTypeStr);

        // For direct array literals, do compile-time bounds checking
        if (llvm::ConstantInt *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
          int64_t IndexValue = ConstIndex->getSExtValue();
          int64_t ArrayLiteralSize = ArrayExpr->getElements().size();

          // Check for negative index at compile time
          if (IndexValue < 0) {
            return LogErrorV("Array index cannot be negative");
          }

          // Check for out of bounds at compile time
          if (IndexValue >= ArrayLiteralSize) {
            return LogErrorV("Array index out of bounds");
          }
        }
      }
      // Otherwise, check if it's from a variable
      else if (VariableExprAST *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
        std::string varName = VarExpr->getName();

        const TypeInfo *info = lookupTypeInfo(varName);
        if (info && info->isArray) {
          if (info->isNullable) {
            return LogErrorV("Cannot index nullable array without null-safe operator");
          }
          elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
          ElemType = getTypeFromString(elemTypeStr);
          elementIsNullable = info->elementNullable;

          // Check for compile-time bounds checking if array size is known
          if (ArraySizes.count(varName)) {
            if (llvm::ConstantInt *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
              int64_t IndexValue = ConstIndex->getSExtValue();
              int64_t KnownArraySize = ArraySizes[varName];

              // Check for negative index at compile time
              if (IndexValue < 0) {
                return LogErrorV("Array index cannot be negative");
              }

              // Check for out of bounds at compile time
              if (IndexValue >= KnownArraySize) {
                return LogErrorV("Array index out of bounds");
              }
            }
          }
        }
      }
      // Check if it's from a member access (e.g., holder.pointers)
      else if (dynamic_cast<MemberAccessExprAST*>(getArray())) {
        // Get the type from the member access expression
        std::string memberType = getArray()->getTypeName();

        ParsedTypeDescriptor memberDesc = parseTypeString(memberType);
        if (memberDesc.isNullable) {
          return LogErrorV("Cannot index nullable array without null-safe operator");
        }
        if (memberDesc.isArray && memberDesc.sanitized.size() > 2 &&
            memberDesc.sanitized.substr(memberDesc.sanitized.size() - 2) == "[]") {
          elemTypeStr = memberDesc.sanitized.substr(0, memberDesc.sanitized.size() - 2);
          ElemType = getTypeFromString(elemTypeStr);
          elementIsNullable = memberDesc.elementNullable;
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

      const TypeInfo *info = lookupTypeInfo(varName);
      if (info && info->isArray) {
        if (info->isNullable) {
          return LogErrorV("Cannot index nullable array without null-safe operator");
        }
        // It's an array type, extract the element type
        elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
        if (elemTypeStr == "int") {
          ElemType = llvm::Type::getInt32Ty(*TheContext);
        } else if (elemTypeStr == "float" || elemTypeStr == "double") {
          ElemType = llvm::Type::getDoubleTy(*TheContext);
        } else if (elemTypeStr == "char") {
          ElemType = llvm::Type::getInt16Ty(*TheContext);
        } else if (elemTypeStr == "bool") {
          ElemType = llvm::Type::getInt8Ty(*TheContext);
        }
        elementIsNullable = info->elementNullable;
      }
    }
  } else {
    return LogErrorV("Array indexing requires an array type");
  }
  
  if (!ArrayPtr || !ElemType)
    return LogErrorV("Invalid array type for indexing");

  // Add compile-time bounds checking for constant indices
  if (ArraySize) {
    // Check if both index and size are constants
    if (llvm::ConstantInt *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
      if (llvm::ConstantInt *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
        int64_t IndexValue = ConstIndex->getSExtValue();
        int64_t SizeValue = ConstSize->getSExtValue();

        // Check for negative index at compile time
        if (IndexValue < 0) {
          return LogErrorV("Array index cannot be negative");
        }

        // Check for out of bounds at compile time
        if (IndexValue >= SizeValue) {
          return LogErrorV("Array index out of bounds");
        }

        // Constant index is valid, no need for runtime check
        // Calculate the address of the indexed element
        llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "elemptr");

        // Set the type name for proper type checking
        std::string displayTypeName = elemTypeStr;
        if (elementIsNullable)
          displayTypeName = ensureOuterNullable(displayTypeName);
        setTypeName(displayTypeName);

        // Load and return the value
        return Builder->CreateLoad(ElemType, ElemPtr, "elemval");
      }
    }

    // Runtime bounds checking for non-constant indices
    // Create bounds check: 0 <= index < size
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

    // Check for negative index
    llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "is_negative");

    // Check for index >= size
    llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "is_out_of_bounds");

    // Combine the checks
    llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "bad_index");

    // Create blocks for error handling
    llvm::BasicBlock *SafeBB = llvm::BasicBlock::Create(*TheContext, "safe_index", TheFunction);
    llvm::BasicBlock *ErrorBB = llvm::BasicBlock::Create(*TheContext, "bounds_error", TheFunction);

    Builder->CreateCondBr(IsBadIndex, ErrorBB, SafeBB);

    // Emit error block - call abort() to terminate the program
    Builder->SetInsertPoint(ErrorBB);

    // Get or declare the abort function
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
        "abort", TheModule.get());
    }

    // Call abort to terminate the program
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    // Continue with safe execution
    Builder->SetInsertPoint(SafeBB);
  }

  // Calculate the address of the indexed element
  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "elemptr");

  // Set the type name for proper type checking
  std::string displayTypeName = elemTypeStr;
  if (elementIsNullable)
    displayTypeName = ensureOuterNullable(displayTypeName);
  setTypeName(displayTypeName);

  // Load and return the value
  return Builder->CreateLoad(ElemType, ElemPtr, "elemval");
}

llvm::Value *NullSafeElementAccessExprAST::codegen() {
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;

  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;

  if (!IndexVal->getType()->isIntegerTy(32)) {
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (!arrayDesc.isArray) {
    return LogErrorV("Null-safe array indexing requires an array value");
  }
  if (!arrayDesc.isNullable) {
    return LogErrorV("Null-safe array indexing requires nullable array type");
  }

  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  if (ArrayVal->getType()->isStructTy()) {
    ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "nsarray.ptr");
    ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "nsarray.size");
  } else if (ArrayVal->getType()->isPointerTy()) {
    ArrayPtr = ArrayVal;
  } else {
    return LogErrorV("Null-safe array indexing requires array storage");
  }

  if (!ArrayPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe array indexing requires pointer-compatible storage");

  bool elementAllowsNull = arrayDesc.elementNullable;
  std::string elemTypeStr;
  if (!arrayTypeName.empty()) {
    std::string sanitized = stripNullableAnnotations(arrayTypeName);
    if (sanitized.size() > 2 && sanitized.substr(sanitized.size() - 2) == "[]") {
      elemTypeStr = sanitized.substr(0, sanitized.size() - 2);
    }
  }

  if (elemTypeStr.empty()) {
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      if (const TypeInfo *info = lookupTypeInfo(VarExpr->getName())) {
        if (info->isArray && info->typeName.size() > 2) {
          elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
          elementAllowsNull = elementAllowsNull || info->elementNullable;
        }
      }
    }
  }

  if (elemTypeStr.empty())
    elemTypeStr = "int";

  llvm::Type *ElemType = getTypeFromString(elemTypeStr);
  if (!ElemType)
    return LogErrorV("Unknown element type in array");

  if (!ElemType->isPointerTy())
    return LogErrorV("Null-safe array indexing is only supported for reference-type elements");

  llvm::PointerType *ArrayPtrType = llvm::cast<llvm::PointerType>(ArrayPtr->getType());
  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(ArrayPtrType);
  llvm::Value *IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nsarray.isnull");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nsarray.merge", Func);

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);

  if (ArraySize) {
    if (auto *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
      if (ConstIndex->getSExtValue() < 0)
        return LogErrorV("Array index cannot be negative");
      if (auto *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
        if (ConstIndex->getSExtValue() >= ConstSize->getSExtValue())
          return LogErrorV("Array index out of bounds");
      }
    }
  }

  llvm::BasicBlock *ValueBB = NotNullBB;
  if (ArraySize) {
    llvm::BasicBlock *BoundsErrorBB = llvm::BasicBlock::Create(*TheContext, "nsarray.bounds_error", Func);
    ValueBB = llvm::BasicBlock::Create(*TheContext, "nsarray.value", Func);

    llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "nsarray.isneg");
    llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "nsarray.isoob");
    llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "nsarray.badindex");
    Builder->CreateCondBr(IsBadIndex, BoundsErrorBB, ValueBB);

    Builder->SetInsertPoint(BoundsErrorBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
                                         "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(ValueBB);
  }

  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "nsarray.elemptr");
  llvm::Value *LoadedValue = Builder->CreateLoad(ElemType, ElemPtr, "nsarray.elem");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullValue = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ElemType));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(ElemType, 2, "nsarray.maybe");
  Phi->addIncoming(LoadedValue, NotNullEnd);
  Phi->addIncoming(NullValue, NullEnd);

  std::string resultTypeName = elemTypeStr;
  if (elementAllowsNull)
    resultTypeName = ensureOuterNullable(resultTypeName);
  resultTypeName = ensureOuterNullable(resultTypeName);
  setTypeName(resultTypeName);

  return Phi;
}

// Variable pointer code generation for increment/decrement
llvm::Value *VariableExprAST::codegen_ptr() {
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    if (const TypeInfo *info = lookupLocalTypeInfo(getName())) {
      if (info->isAlias()) {
        llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
        return Builder->CreateLoad(Alloca->getAllocatedType(), V, (getName() + "_ptr").c_str());
      }
      // For ref value owners we can just return the alloca itself
    }
    return V;
  }

  llvm::Value *G = GlobalValues[getName()];
  if (G) {
    if (const TypeInfo *info = lookupGlobalTypeInfo(getName())) {
      if (info->isAlias()) {
        llvm::GlobalVariable *GV = static_cast<llvm::GlobalVariable*>(G);
        return Builder->CreateLoad(GV->getValueType(), G, (getName() + "_ptr").c_str());
      }
      // For ref value owners we can just return the global itself
    }
    return G;
  }

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

  // Extract the size for bounds checking
  llvm::Value *ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "arraysize");

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

  // Add compile-time bounds checking for constant indices
  if (llvm::ConstantInt *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
    if (llvm::ConstantInt *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
      int64_t IndexValue = ConstIndex->getSExtValue();
      int64_t SizeValue = ConstSize->getSExtValue();

      // Check for negative index at compile time
      if (IndexValue < 0) {
        return LogErrorV("Array index cannot be negative");
      }

      // Check for out of bounds at compile time
      if (IndexValue >= SizeValue) {
        return LogErrorV("Array index out of bounds");
      }

      // Constant index is valid, no need for runtime check
      return Builder->CreateGEP(ElemTy, ArrayPtr, IndexVal, "elemptr");
    }
  }

  // Runtime bounds checking for non-constant indices
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Check for negative index
  llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "is_negative");

  // Check for index >= size
  llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "is_out_of_bounds");

  // Combine the checks
  llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "bad_index");

  // Create blocks for error handling
  llvm::BasicBlock *SafeBB = llvm::BasicBlock::Create(*TheContext, "safe_index_ptr", TheFunction);
  llvm::BasicBlock *ErrorBB = llvm::BasicBlock::Create(*TheContext, "bounds_error_ptr", TheFunction);

  Builder->CreateCondBr(IsBadIndex, ErrorBB, SafeBB);

  // Emit error block - call abort() to terminate the program
  Builder->SetInsertPoint(ErrorBB);

  // Get or declare the abort function
  llvm::Function *AbortFunc = TheModule->getFunction("abort");
  if (!AbortFunc) {
    llvm::FunctionType *AbortType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*TheContext), false);
    AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
      "abort", TheModule.get());
  }

  // Call abort to terminate the program
  Builder->CreateCall(AbortFunc);
  Builder->CreateUnreachable();

  // Continue with safe execution
  Builder->SetInsertPoint(SafeBB);

  return Builder->CreateGEP(ElemTy, ArrayPtr, IndexVal, "elemptr");
}

llvm::Value *NullSafeElementAccessExprAST::codegen_ptr() {
  llvm::Value *ArrayVal = getArray()->codegen();
  if (!ArrayVal)
    return nullptr;

  llvm::Value *IndexVal = getIndex()->codegen();
  if (!IndexVal)
    return nullptr;

  if (!IndexVal->getType()->isIntegerTy(32)) {
    if (IndexVal->getType()->isIntegerTy()) {
      IndexVal = Builder->CreateSExtOrTrunc(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else if (IndexVal->getType()->isFloatingPointTy()) {
      IndexVal = Builder->CreateFPToSI(IndexVal, llvm::Type::getInt32Ty(*TheContext), "nsarray.idxtmp");
    } else {
      return LogErrorV("Array index must be an integer");
    }
  }

  std::string arrayTypeName = getArray()->getTypeName();
  ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
  if (!arrayDesc.isArray) {
    return LogErrorV("Null-safe array indexing requires an array value");
  }
  if (!arrayDesc.isNullable) {
    return LogErrorV("Null-safe array indexing requires nullable array type");
  }

  llvm::Value *ArrayPtr = nullptr;
  llvm::Value *ArraySize = nullptr;
  if (ArrayVal->getType()->isStructTy()) {
    ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "nsarray.ptr");
    ArraySize = Builder->CreateExtractValue(ArrayVal, 1, "nsarray.size");
  } else if (ArrayVal->getType()->isPointerTy()) {
    ArrayPtr = ArrayVal;
  } else {
    return LogErrorV("Null-safe array indexing requires array storage");
  }

  if (!ArrayPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe array indexing requires pointer-compatible storage");

  std::string elemTypeStr;
  if (!arrayTypeName.empty()) {
    std::string sanitized = stripNullableAnnotations(arrayTypeName);
    if (sanitized.size() > 2 && sanitized.substr(sanitized.size() - 2) == "[]") {
      elemTypeStr = sanitized.substr(0, sanitized.size() - 2);
    }
  }

  if (elemTypeStr.empty()) {
    if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getArray())) {
      if (const TypeInfo *info = lookupTypeInfo(VarExpr->getName())) {
        if (info->isArray && info->typeName.size() > 2) {
          elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
        }
      }
    }
  }

  if (elemTypeStr.empty())
    elemTypeStr = "int";

  llvm::Type *ElemType = getTypeFromString(elemTypeStr);
  if (!ElemType)
    return LogErrorV("Unknown element type in array");

  llvm::PointerType *ArrayPtrType = llvm::cast<llvm::PointerType>(ArrayPtr->getType());
  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(ArrayPtrType);
  llvm::Value *IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nsarray.isnull");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nsarray.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nsarray.merge", Func);

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);

  if (ArraySize) {
    if (auto *ConstIndex = llvm::dyn_cast<llvm::ConstantInt>(IndexVal)) {
      if (ConstIndex->getSExtValue() < 0)
        return LogErrorV("Array index cannot be negative");
      if (auto *ConstSize = llvm::dyn_cast<llvm::ConstantInt>(ArraySize)) {
        if (ConstIndex->getSExtValue() >= ConstSize->getSExtValue())
          return LogErrorV("Array index out of bounds");
      }
    }
  }

  llvm::BasicBlock *ValueBB = NotNullBB;
  if (ArraySize) {
    llvm::BasicBlock *BoundsErrorBB = llvm::BasicBlock::Create(*TheContext, "nsarray.bounds_error", Func);
    ValueBB = llvm::BasicBlock::Create(*TheContext, "nsarray.value", Func);

    llvm::Value *IsNegative = Builder->CreateICmpSLT(IndexVal,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0), "nsarray.isneg");
    llvm::Value *IsOutOfBounds = Builder->CreateICmpSGE(IndexVal, ArraySize, "nsarray.isoob");
    llvm::Value *IsBadIndex = Builder->CreateOr(IsNegative, IsOutOfBounds, "nsarray.badindex");
    Builder->CreateCondBr(IsBadIndex, BoundsErrorBB, ValueBB);

    Builder->SetInsertPoint(BoundsErrorBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage,
                                         "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    Builder->SetInsertPoint(ValueBB);
  }

  llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "nsarray.elemptr");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullElementPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ElemPtr->getType()));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(ElemPtr->getType(), 2, "nsarray.ptrmaybe");
  Phi->addIncoming(ElemPtr, NotNullEnd);
  Phi->addIncoming(NullElementPtr, NullEnd);

  return Phi;
}

// Generate code for variable expressions
llvm::Value *VariableExprAST::codegen() {
  // First look this variable up in the local function scope
  llvm::Value *V = NamedValues[getName()];
  if (V) {
    // Local variable - load from alloca
    llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(V);
    // Set type name from local types
    if (const TypeInfo *info = lookupLocalTypeInfo(getName())) {
      setTypeName(typeNameFromInfo(*info));

      if (info->isAlias()) {
        llvm::Value *Ptr = Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
        llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
        if (!ActualLLVMType)
          return LogErrorV("Invalid type for ref variable");
        return Builder->CreateLoad(ActualLLVMType, Ptr, (getName() + "_deref").c_str());
      }

      return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
    }

    return Builder->CreateLoad(Alloca->getAllocatedType(), V, getName().c_str());
  }

  // Not found locally, check global scope
  llvm::GlobalVariable *GV = GlobalValues[getName()];
  if (GV) {
    // Global variable - load from global
    // Set type name from global types
    if (const TypeInfo *info = lookupGlobalTypeInfo(getName())) {
      setTypeName(typeNameFromInfo(*info));

      if (info->isAlias()) {
        llvm::Value *Ptr = Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
        llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
        if (!ActualLLVMType)
          return LogErrorV("Invalid type for ref variable");
        return Builder->CreateLoad(ActualLLVMType, Ptr, (getName() + "_deref").c_str());
      }

      return Builder->CreateLoad(GV->getValueType(), GV, getName().c_str());
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

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
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

  if (sourceType->isPointerTy() && targetType->isPointerTy()) {
    return Builder->CreateBitCast(value, targetType, "ptrcast");
  }

  // For non-constant values, add runtime range checking for integer narrowing
  if (sourceType->isIntegerTy() && targetType->isIntegerTy() &&
      !targetType->isIntegerTy(1) && // Don't allow to bool
      sourceType->getIntegerBitWidth() > targetType->getIntegerBitWidth()) {

    unsigned targetBits = targetType->getIntegerBitWidth();

    // Determine range based on target type name
    int64_t minVal = 0, maxVal = 0;
    if (targetBits == 8) {
      if (isSignedType(targetTypeName)) {
        minVal = -128; maxVal = 127;  // sbyte, char
      } else {
        minVal = 0; maxVal = 255;  // byte
      }
    } else if (targetBits == 16) {
      if (isSignedType(targetTypeName)) {
        minVal = -32768; maxVal = 32767;  // short
      } else {
        minVal = 0; maxVal = 65535;  // ushort
      }
    } else if (targetBits == 32) {
      if (isSignedType(targetTypeName)) {
        minVal = INT32_MIN; maxVal = INT32_MAX;  // int
      } else {
        minVal = 0; maxVal = UINT32_MAX;  // uint
      }
    }

    // Generate runtime range check
    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *InRangeBB = llvm::BasicBlock::Create(*TheContext, "range_ok", TheFunction);
    llvm::BasicBlock *OutOfRangeBB = llvm::BasicBlock::Create(*TheContext, "range_fail", TheFunction);

    // Create comparison: value >= minVal && value <= maxVal
    llvm::Value *MinCheck = Builder->CreateICmpSGE(value,
        llvm::ConstantInt::get(sourceType, minVal), "mincheck");
    llvm::Value *MaxCheck = Builder->CreateICmpSLE(value,
        llvm::ConstantInt::get(sourceType, maxVal), "maxcheck");
    llvm::Value *InRange = Builder->CreateAnd(MinCheck, MaxCheck, "inrange");

    // Branch based on range check
    Builder->CreateCondBr(InRange, InRangeBB, OutOfRangeBB);

    // Out of range block - call abort()
    Builder->SetInsertPoint(OutOfRangeBB);
    llvm::Function *AbortFunc = TheModule->getFunction("abort");
    if (!AbortFunc) {
      llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
      AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage, "abort", TheModule.get());
    }
    Builder->CreateCall(AbortFunc);
    Builder->CreateUnreachable();

    // In range block - perform the truncation
    Builder->SetInsertPoint(InRangeBB);
    return Builder->CreateTrunc(value, targetType, "rangecast");
  }

  // For non-constant values without range checking, use the regular castToType
  return castToType(value, targetType);
}

// Helper function to promote types for binary operations
std::pair<llvm::Value*, llvm::Value*> promoteTypes(llvm::Value* L, llvm::Value* R,
                                                   std::string_view lhsTypeName,
                                                   std::string_view rhsTypeName) {
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

  std::string lhsClean = sanitizeBaseTypeName(lhsTypeName);
  std::string rhsClean = sanitizeBaseTypeName(rhsTypeName);

  // Handle integer promotions explicitly
  if (LType->isIntegerTy() && RType->isIntegerTy()) {
    if (LType->isIntegerTy(1) || RType->isIntegerTy(1))
      return {L, R};

    unsigned lBits = LType->getIntegerBitWidth();
    unsigned rBits = RType->getIntegerBitWidth();

    auto chooseUnsignedHint = [&](const std::string &primary,
                                  const std::string &secondary) -> std::optional<bool> {
      if (auto hint = unsignedHintFromTypeName(primary); hint.has_value())
        return hint;
      return unsignedHintFromTypeName(secondary);
    };

    auto extendValue = [&](llvm::Value *Value, llvm::Type *TargetType,
                           std::optional<bool> unsignedHint) -> llvm::Value * {
      if (unsignedHint.value_or(false))
        return Builder->CreateZExt(Value, TargetType, "promext");
      return Builder->CreateSExt(Value, TargetType, "promext");
    };

    if (lBits < rBits) {
      auto hint = chooseUnsignedHint(lhsClean, rhsClean);
      L = extendValue(L, RType, hint);
    } else if (rBits < lBits) {
      auto hint = chooseUnsignedHint(rhsClean, lhsClean);
      R = extendValue(R, LType, hint);
    }

    return {L, R};
  }

  // If one is float and other is int, promote int to float
  if (LType->isFloatingPointTy() && RType->isIntegerTy()) {
    std::optional<bool> hint = unsignedHintFromTypeName(rhsClean);
    if (hint.value_or(false))
      R = Builder->CreateUIToFP(R, LType, "promtmp");
    else
      R = Builder->CreateSIToFP(R, LType, "promtmp");
    return {L, R};
  }

  if (LType->isIntegerTy() && RType->isFloatingPointTy()) {
    std::optional<bool> hint = unsignedHintFromTypeName(lhsClean);
    if (hint.value_or(false))
      L = Builder->CreateUIToFP(L, RType, "promtmp");
    else
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

llvm::Value *BinaryExprAST::codegenNullCoalescing(llvm::Value *lhsValue) {
  const ExprAST *lhsExpr = unwrapRefExpr(getLHS());
  if (!expressionIsNullable(lhsExpr))
    return LogErrorV("Null-coalescing operator '\?\?' requires nullable left-hand side");

  std::string leftTypeName = getLHS()->getTypeName();
  ParsedTypeDescriptor leftDesc = parseTypeString(leftTypeName);

  bool lhsIsArray = leftDesc.isArray;
  bool lhsIsPointer = lhsValue->getType()->isPointerTy();

  if (!lhsIsPointer && !lhsIsArray)
    return LogErrorV("Null-coalescing operator '\?\?' requires reference or array type on the left-hand side");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.null", Func);
  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.notnull", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullcoal.merge", Func);

  llvm::Value *IsNull = nullptr;
  if (lhsIsArray) {
    if (!lhsValue->getType()->isStructTy())
      return LogErrorV("Null-coalescing operator '\?\?' expected array struct value");
    llvm::Value *ArrayPtr = Builder->CreateExtractValue(lhsValue, 0, "nullcoal.arrptr");
    if (!ArrayPtr->getType()->isPointerTy())
      return LogErrorV("Nullable array pointer field must be a pointer");
    llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ArrayPtr->getType()));
    IsNull = Builder->CreateICmpEQ(ArrayPtr, NullPtr, "nullcoal.check");
  } else {
    llvm::PointerType *PtrTy = llvm::cast<llvm::PointerType>(lhsValue->getType());
    llvm::Value *NullPtr = llvm::ConstantPointerNull::get(PtrTy);
    IsNull = Builder->CreateICmpEQ(lhsValue, NullPtr, "nullcoal.check");
  }

  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *rhsValueRaw = getRHS()->codegen();
  if (!rhsValueRaw)
    return nullptr;
  std::string rhsTypeName = getRHS()->getTypeName();
  llvm::Value *ResultWhenNull = rhsValueRaw;

  if (lhsIsArray) {
    if (rhsValueRaw->getType() != lhsValue->getType())
      return LogErrorV("Null-coalescing operator '\?\?' requires fallback to match array type");
  } else {
    if (!rhsValueRaw->getType()->isPointerTy())
      return LogErrorV("Null-coalescing operator '\?\?' fallback must be a reference type");
    if (rhsValueRaw->getType() != lhsValue->getType())
      ResultWhenNull = Builder->CreateBitCast(rhsValueRaw, lhsValue->getType(), "nullcoal.cast");
  }

  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(lhsValue->getType(), 2, "nullcoal.result");
  Phi->addIncoming(lhsValue, NotNullEnd);
  Phi->addIncoming(ResultWhenNull, NullEnd);

  bool rhsNullable = expressionIsNullable(unwrapRefExpr(getRHS()));
  std::string baseTypeName = leftDesc.sanitized;
  if (baseTypeName.empty()) {
    ParsedTypeDescriptor rhsDesc = parseTypeString(rhsTypeName);
    baseTypeName = rhsDesc.sanitized;
  }
  if (baseTypeName.empty())
    baseTypeName = rhsTypeName;

  if (!baseTypeName.empty()) {
    if (rhsNullable)
      setTypeName(ensureOuterNullable(baseTypeName));
    else
      setTypeName(baseTypeName);
  }

  return Phi;
}

llvm::Value *BinaryExprAST::codegenNullCoalescingAssign(llvm::Value *lhsValue) {
  const ExprAST *lhsExpr = unwrapRefExpr(getLHS());
  if (!expressionIsNullable(lhsExpr))
    return LogErrorV("Null-coalescing assignment '\?\?=' requires nullable left-hand side");

  std::string leftTypeName = getLHS()->getTypeName();
  ParsedTypeDescriptor leftDesc = parseTypeString(leftTypeName);

  bool lhsIsArray = leftDesc.isArray;
  bool lhsIsPointerValue = lhsValue->getType()->isPointerTy();

  if (!lhsIsPointerValue && !lhsIsArray)
    return LogErrorV("Null-coalescing assignment '\?\?=' requires reference or array type on the left-hand side");

  llvm::Value *lhsPtr = getLHS()->codegen_ptr();
  if (!lhsPtr)
    return nullptr;
  if (!lhsPtr->getType()->isPointerTy())
    return LogErrorV("destination of '\?\?=' must be assignable");

  llvm::PointerType *lhsPtrType = llvm::cast<llvm::PointerType>(lhsPtr->getType());
  llvm::Value *NullStoragePtr = llvm::ConstantPointerNull::get(lhsPtrType);
  llvm::Value *PtrValid = Builder->CreateICmpNE(lhsPtr, NullStoragePtr, "nullassign.ptrvalid");

  llvm::Value *ValueIsNull = nullptr;
  if (lhsIsArray) {
    if (!lhsValue->getType()->isStructTy())
      return LogErrorV("Null-coalescing assignment '\?\?=' expected array struct value");
    llvm::Value *ArrayPtr = Builder->CreateExtractValue(lhsValue, 0, "nullassign.arrptr");
    if (!ArrayPtr->getType()->isPointerTy())
      return LogErrorV("Nullable array pointer field must be a pointer");
    llvm::Value *NullArrayPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ArrayPtr->getType()));
    ValueIsNull = Builder->CreateICmpEQ(ArrayPtr, NullArrayPtr, "nullassign.value");
  } else {
    llvm::PointerType *PtrTy = llvm::cast<llvm::PointerType>(lhsValue->getType());
    llvm::Value *NullPtrValue = llvm::ConstantPointerNull::get(PtrTy);
    ValueIsNull = Builder->CreateICmpEQ(lhsValue, NullPtrValue, "nullassign.value");
  }

  llvm::Value *ShouldAssign = Builder->CreateAnd(ValueIsNull, PtrValid, "nullassign.should");

  llvm::Function *Func = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *AssignBB = llvm::BasicBlock::Create(*TheContext, "nullassign.assign", Func);
  llvm::BasicBlock *DoneBB = llvm::BasicBlock::Create(*TheContext, "nullassign.done", Func);
  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();

  Builder->CreateCondBr(ShouldAssign, AssignBB, DoneBB);

  Builder->SetInsertPoint(AssignBB);
  llvm::Value *rhsValueRaw = getRHS()->codegen();
  if (!rhsValueRaw)
    return nullptr;
  std::string rhsTypeName = getRHS()->getTypeName();
  llvm::Value *AssignedValue = rhsValueRaw;

  if (lhsIsArray) {
    if (rhsValueRaw->getType() != lhsValue->getType())
      return LogErrorV("Null-coalescing assignment '\?\?=' requires fallback to match array type");
  } else {
    if (!rhsValueRaw->getType()->isPointerTy())
      return LogErrorV("Null-coalescing assignment '\?\?=' fallback must be a reference type");
    if (rhsValueRaw->getType() != lhsValue->getType())
      AssignedValue = Builder->CreateBitCast(rhsValueRaw, lhsValue->getType(), "nullassign.cast");
  }

  Builder->CreateStore(AssignedValue, lhsPtr);
  Builder->CreateBr(DoneBB);
  llvm::BasicBlock *AssignEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(DoneBB);
  llvm::PHINode *Phi = Builder->CreatePHI(lhsValue->getType(), 2, "nullassign.result");
  Phi->addIncoming(lhsValue, CurrentBB);
  Phi->addIncoming(AssignedValue, AssignEnd);

  bool rhsNullable = expressionIsNullable(unwrapRefExpr(getRHS()));
  std::string baseTypeName = leftDesc.sanitized;
  if (baseTypeName.empty()) {
    ParsedTypeDescriptor rhsDesc = parseTypeString(rhsTypeName);
    baseTypeName = rhsDesc.sanitized;
  }
  if (baseTypeName.empty())
    baseTypeName = rhsTypeName;

  if (!baseTypeName.empty()) {
    if (rhsNullable)
      setTypeName(ensureOuterNullable(baseTypeName));
    else
      setTypeName(baseTypeName);
  }

  return Phi;
}

// Generate code for binary expressions like +, -, *, /, <, >
llvm::Value *BinaryExprAST::codegen() {
  // Generate left operand first
  llvm::Value *L = getLHS()->codegen();
  if (!L)
    return nullptr;
  
  if (Op == "\?\?")
    return codegenNullCoalescing(L);
  if (Op == "\?\?=")
    return codegenNullCoalescingAssign(L);

  // For comparisons, arithmetic, and equality operators, try to regenerate literals so they
  // match the other operand's type (numbers and chars).
  llvm::Value *R = nullptr;
  bool isComparisonOrArithmetic = (Op == "==" || Op == "!=" || Op == "<" || Op == ">" || Op == "<=" || Op == ">=" ||
                                   Op == "+" || Op == "-" || Op == "*" || Op == "/" || Op == "%");
  if (isComparisonOrArithmetic) {
    if (NumberExprAST *NumRHS = dynamic_cast<NumberExprAST*>(getRHS())) {
      // Right side is a number literal - generate it with left's type as target
      R = NumRHS->codegen_with_target(L->getType());
    } else if (NumberExprAST *NumLHS = dynamic_cast<NumberExprAST*>(getLHS())) {
      // Left side is a number literal - regenerate it and the right side
      // This handles cases like: 255 == byte_var or 10 + byte_var
      llvm::Value *RTemp = getRHS()->codegen();
      if (RTemp) {
        L = NumLHS->codegen_with_target(RTemp->getType());
        R = RTemp;
      }
    }

    if (!R) {
      if (CharExprAST *CharRHS = dynamic_cast<CharExprAST*>(getRHS())) {
        R = CharRHS->codegen_with_target(L->getType(), getLHS()->getTypeName());
      } else if (CharExprAST *CharLHS = dynamic_cast<CharExprAST*>(getLHS())) {
        llvm::Value *RTemp = getRHS()->codegen();
        if (RTemp) {
          L = CharLHS->codegen_with_target(RTemp->getType(), getRHS()->getTypeName());
          R = RTemp;
        }
      }
    }
  }

  // If R wasn't generated yet (not a comparison or not a number literal), generate normally
  if (!R) {
    R = getRHS()->codegen();
  }

  if (!R)
    return nullptr;

  // Get type names from operands after potential regeneration
  std::string rawLeftTypeName = getLHS()->getTypeName();
  std::string rawRightTypeName = getRHS()->getTypeName();
  std::string leftTypeName = sanitizeBaseTypeName(rawLeftTypeName);
  std::string rightTypeName = sanitizeBaseTypeName(rawRightTypeName);

  // Promote types to compatible types
  auto promoted = promoteTypes(L, R, leftTypeName, rightTypeName);
  L = promoted.first;
  R = promoted.second;

  // Check if working with floating point or integer types
  llvm::Type *resultType = L->getType();
  bool isFloat = resultType->isFloatingPointTy();
  
  // Set result type name based on the promoted type
  if (isFloat) {
    setTypeName(resultType->isFloatTy() ? "float" : "double");
  } else {
    const unsigned bitWidth = resultType->getIntegerBitWidth();
    auto pickMatchingName = [&](const std::string &candidate) -> std::string {
      if (candidate.empty())
        return {};
      std::string clean = sanitizeBaseTypeName(candidate);
      switch (bitWidth) {
      case 8:
        if (clean == "byte" || clean == "sbyte" || clean == "schar")
          return clean;
        break;
      case 16:
        if (clean == "short" || clean == "ushort" || clean == "char")
          return clean;
        break;
      case 32:
        if (clean == "int" || clean == "uint" || clean == "lchar")
          return clean;
        break;
      case 64:
        if (clean == "long" || clean == "ulong")
          return clean;
        break;
      default:
        break;
      }
      return {};
    };

    std::string chosen = pickMatchingName(rawLeftTypeName);
    if (chosen.empty())
      chosen = pickMatchingName(rawRightTypeName);
    if (chosen.empty()) {
      switch (bitWidth) {
      case 8:
        chosen = "sbyte";
        break;
      case 16:
        chosen = "char";
        break;
      case 32:
        chosen = "int";
        break;
      case 64:
        chosen = "long";
        break;
      default:
        chosen = "int";
        break;
      }
    }

    setTypeName(chosen);
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
      {
        const ExprAST *rhsCheckExpr = unwrapRefExpr(getRHS());
        bool rhsIsNullable = expressionIsNullable(rhsCheckExpr);
      if (VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(getLHS())) {
        // Simple variable assignment - check local first, then global
        llvm::Value *Variable = NamedValues[LHSE->getName()];
        if (Variable) {
          // Local variable
          llvm::AllocaInst *Alloca = static_cast<llvm::AllocaInst*>(Variable);

          if (const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName()); info && info->isAlias()) {
            llvm::Value *Ptr = Builder->CreateLoad(Alloca->getAllocatedType(), Variable, (LHSE->getName() + "_ptr").c_str());
            llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
            if (!ActualLLVMType)
              return LogErrorV("Invalid type for ref variable");
            R = castToType(R, ActualLLVMType);
            Builder->CreateStore(R, Ptr);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }

          // Regular variable assignment
          llvm::Type *VarType = Alloca->getAllocatedType();
          if (const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName())) {
            if (rhsIsNullable && !typeAllowsNull(*info)) {
              return LogErrorV(("Cannot assign nullable value to non-nullable variable '" + LHSE->getName() + "'").c_str());
            }
          }
          // Get type name for proper range checking
          const TypeInfo *info = lookupLocalTypeInfo(LHSE->getName());
    if (info && !info->typeName.empty()) {
      R = castToType(R, VarType, info->typeName);
    } else {
      R = castToType(R, VarType);
    }
          Builder->CreateStore(R, Variable);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        }

        // Check global scope
        llvm::GlobalVariable *GV = GlobalValues[LHSE->getName()];
        if (GV) {
          if (const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName()); info && info->isAlias()) {
            llvm::Value *Ptr = Builder->CreateLoad(GV->getValueType(), GV, (LHSE->getName() + "_ptr").c_str());
            llvm::Type *ActualLLVMType = getTypeFromString(info->typeName);
            if (!ActualLLVMType)
              return LogErrorV("Invalid type for ref variable");
            R = castToType(R, ActualLLVMType);
            Builder->CreateStore(R, Ptr);
            setTypeName("void");
            return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
          }

          // Regular global variable
          llvm::Type *VarType = GV->getValueType();
          if (const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName())) {
            if (!info->isAlias() && rhsIsNullable && !typeAllowsNull(*info)) {
              return LogErrorV(("Cannot assign nullable value to non-nullable variable '" + LHSE->getName() + "'").c_str());
            }
          }
          // Get type name for proper range checking
          const TypeInfo *info = lookupGlobalTypeInfo(LHSE->getName());
          if (info && !info->typeName.empty()) {
            R = castToType(R, VarType, info->typeName);
          } else {
            R = castToType(R, VarType);
          }
          Builder->CreateStore(R, GV);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
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
              
              if (const TypeInfo *info = lookupTypeInfo(varName); info && info->isArray) {
                std::string elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
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

        bool elementAllowsNull = false;
        if (auto *VarExpr = dynamic_cast<VariableExprAST*>(LHSAI->getArray())) {
          if (const TypeInfo *info = lookupTypeInfo(VarExpr->getName()))
            elementAllowsNull = info->elementNullable;
        } else {
          std::string arrayTypeName = LHSAI->getArray()->getTypeName();
          if (!arrayTypeName.empty()) {
            ParsedTypeDescriptor arrayDesc = parseTypeString(arrayTypeName);
            elementAllowsNull = arrayDesc.elementNullable;
          }
        }
        if (rhsIsNullable && !elementAllowsNull) {
          return LogErrorV("Cannot assign nullable value to non-nullable array element");
        }

        // Calculate the address and store
        llvm::Value *ElemPtr = Builder->CreateGEP(ElemType, ArrayPtr, IndexVal, "elemptr");
        Builder->CreateStore(R, ElemPtr);
        // Return a void value to indicate this is a statement, not an expression
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      } else if (MemberAccessExprAST *LHSMA = dynamic_cast<MemberAccessExprAST*>(getLHS())) {
        // Member access assignment (e.g., this.x = value)
        llvm::Value *FieldPtr = LHSMA->codegen_ptr();
        if (!FieldPtr)
          return nullptr;

        bool fieldAllowsNull = false;
        std::string objectTypeName = LHSMA->getObject()->getTypeName();
        ParsedTypeDescriptor objectDesc = parseTypeString(objectTypeName);
        std::string structName = objectDesc.sanitized;
        if (auto FieldTypesIt = StructFieldTypes.find(structName); FieldTypesIt != StructFieldTypes.end()) {
          if (auto TypeIt = FieldTypesIt->second.find(LHSMA->getMemberName()); TypeIt != FieldTypesIt->second.end()) {
            ParsedTypeDescriptor fieldDesc = parseTypeString(TypeIt->second);
            fieldAllowsNull = typeAllowsNull(fieldDesc);
          }
        }
        if (rhsIsNullable && !fieldAllowsNull) {
          return LogErrorV(("Cannot assign nullable value to non-nullable field '" + LHSMA->getMemberName() + "'").c_str());
        }

        // Cast RHS to the field type if necessary
        // Need to determine the field type from the struct definition
        // For now, trust that R has the correct type
        // TODO: Add proper type checking
        // R = castToType(R, FieldType);

        Builder->CreateStore(R, FieldPtr);
        // Return a void value to indicate this is a statement, not an expression
        setTypeName("void");
        return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      } else if (UnaryExprAST *LHSU = dynamic_cast<UnaryExprAST*>(getLHS())) {
        // Check if it's a dereference operator (pointer assignment)
        if (LHSU->getOp() == "@") {
          // Get the pointer from the dereference operation
          llvm::Value *Ptr = LHSU->codegen_ptr();
          if (!Ptr)
            return nullptr;

          // Store the value to the pointer location
          Builder->CreateStore(R, Ptr);
          // Return a void value to indicate this is a statement, not an expression
          setTypeName("void");
          return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
        } else {
          return LogErrorV("destination of '=' must be a variable, array element, struct member, or dereferenced pointer");
        }
      } else {
        return LogErrorV("destination of '=' must be a variable, array element, struct member, or dereferenced pointer");
      }
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
      std::string lhsPromoteType;
      if (const TypeInfo *info = lookupTypeInfo(LHSE->getName()))
        lhsPromoteType = typeNameFromInfo(*info);
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
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
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
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
      std::string elementTypeNameForPromotion;
      
      if (ArrayType->isStructTy()) {
        // New array representation: extract pointer from struct
        llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
        if (StructType->getNumElements() == 2) {
          // Extract the pointer (field 0)
          ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
          
          // Determine element type from the context
          if (VariableExprAST *ArrayVar = dynamic_cast<VariableExprAST*>(LHSE->getArray())) {
            std::string varName = ArrayVar->getName();
            
            if (const TypeInfo *info = lookupTypeInfo(varName); info && info->isArray) {
              std::string elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
              ElemType = getTypeFromString(elemTypeStr);
              elementTypeNameForPromotion = elemTypeStr;
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
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
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
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
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
      std::string lhsPromoteType;
      if (const TypeInfo *info = lookupTypeInfo(LHSE->getName()))
        lhsPromoteType = typeNameFromInfo(*info);
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, lhsPromoteType, rhsPromoteType);
      
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
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
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
      std::string elementTypeNameForPromotion;
      
      if (ArrayType->isStructTy()) {
        // New array representation: extract pointer from struct
        llvm::StructType *StructType = llvm::cast<llvm::StructType>(ArrayType);
        if (StructType->getNumElements() == 2) {
          // Extract the pointer (field 0)
          ArrayPtr = Builder->CreateExtractValue(ArrayVal, 0, "arrayPtr");
          
          // Determine element type from the context
          if (VariableExprAST *ArrayVar = dynamic_cast<VariableExprAST*>(LHSE->getArray())) {
            std::string varName = ArrayVar->getName();
            
            if (const TypeInfo *info = lookupTypeInfo(varName); info && info->isArray) {
              std::string elemTypeStr = info->typeName.substr(0, info->typeName.size() - 2);
              ElemType = getTypeFromString(elemTypeStr);
              elementTypeNameForPromotion = elemTypeStr;
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
      std::string rhsPromoteType = getRHS()->getTypeName();
      auto [PromotedCurrent, PromotedR] = promoteTypes(CurrentVal, R, elementTypeNameForPromotion, rhsPromoteType);
      
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
      // Return a void value to indicate this is a statement, not an expression
      setTypeName("void");
      return llvm::UndefValue::get(llvm::Type::getVoidTy(*TheContext));
      
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

  // Handle pointer operators
  if (Op == "#") {
    // Address-of operator
    llvm::Value *Ptr = Operand->codegen_ptr();
    if (!Ptr)
      return LogErrorV("Cannot take address of non-lvalue");

    // Set type name to pointer type
    std::string baseType = Operand->getTypeName();
    setTypeName(baseType + "@");

    return Ptr;
  } else if (Op == "@") {
    // Dereference operator
    if (!OperandV->getType()->isPointerTy())
      return LogErrorV("Cannot dereference non-pointer type");

    // For opaque pointers in LLVM 15+, need to determine the element type
    // from the operand's type string
    std::string operandType = Operand->getTypeName();
    if (operandType.empty() || operandType.find('@') == std::string::npos)
      return LogErrorV("Invalid pointer type for dereference");

    // Remove the @ and any level suffix to get the base type
    size_t atPos = operandType.find('@');
    std::string baseType = operandType.substr(0, atPos);

    // If it's a multi-level pointer, the result is still a pointer
    if (atPos + 1 < operandType.size()) {
      std::string levelStr = operandType.substr(atPos + 1);
      if (!levelStr.empty()) {
        int level = std::stoi(levelStr);
        if (level > 1) {
          // Result type is pointer with level-1
          if (level == 2) {
            setTypeName(baseType + "@");
          } else {
            setTypeName(baseType + "@" + std::to_string(level - 1));
          }
        } else {
          setTypeName(baseType);
        }
      } else {
        setTypeName(baseType);
      }
    } else {
      setTypeName(baseType);
    }

    // Get the element type to load
    llvm::Type *ElementType = getTypeFromString(getTypeName());
    if (!ElementType)
      return LogErrorV("Cannot determine element type for dereference");

    return Builder->CreateLoad(ElementType, OperandV, "deref");
  }

  return LogErrorV("invalid unary operator");
}

// Generate pointer for unary expressions (for address-of operations)
llvm::Value *UnaryExprAST::codegen_ptr() {
  if (Op == "@") {
    // Dereference operator returns the pointer itself
    llvm::Value *OperandV = Operand->codegen();
    if (!OperandV)
      return nullptr;

    if (!OperandV->getType()->isPointerTy())
      return LogErrorV("Cannot dereference non-pointer type");

    return OperandV;
  }

  // Other unary operators don't have an lvalue
  return nullptr;
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

// Generate code for ref expressions (returns pointer to variable)
llvm::Value *RefExprAST::codegen() {
  // For ref expressions, return the pointer to the operand
  // Use codegen_ptr if available, otherwise get the address
  if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getOperand())) {
    return VarExpr->codegen_ptr();
  }
  else if (auto *ArrayIdxExpr = dynamic_cast<ArrayIndexExprAST*>(getOperand())) {
    return ArrayIdxExpr->codegen_ptr();
  }
  else if (auto *MemberExpr = dynamic_cast<MemberAccessExprAST*>(getOperand())) {
    return MemberExpr->codegen_ptr();
  }
  else if (auto *ThisExpr = dynamic_cast<ThisExprAST*>(getOperand())) {
    return ThisExpr->codegen_ptr();
  }
  else if (auto *UnaryExpr = dynamic_cast<UnaryExprAST*>(getOperand())) {
    return UnaryExpr->codegen_ptr();
  }
  else {
    return LogErrorV("ref can only be used with lvalues (variables, array elements, or struct members)");
  }
}

// Generate code for function calls, including struct constructors
llvm::Value *CallExprAST::codegen() {
  // Check if this is a struct constructor call
  if (StructTypes.contains(getCallee())) {
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
    llvm::Value *ConstructedValue = Builder->CreateCall(ConstructorF, ArgsV, "structtmp");

    llvm::StructType *StructType = StructTypes[getCallee()];
    llvm::AllocaInst *StructAlloca = Builder->CreateAlloca(
        StructType, nullptr, getCallee() + "_inst");
    Builder->CreateStore(ConstructedValue, StructAlloca);

    return StructAlloca;
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

  // Don't assign a name to void function calls
  if (CalleeF->getReturnType()->isVoidTy())
    return Builder->CreateCall(CalleeF, ArgsV);
  else
    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

//===----------------------------------------------------------------------===//
// Statement Code Generation
//===----------------------------------------------------------------------===//

// Generate code for return statements
llvm::Value *ReturnStmtAST::codegen() {
  if (getReturnValue()) {
    llvm::Value *Val;

    // Check if this is a ref return
    if (isRef()) {
      // For ref return, need to return a pointer
      // Use codegen_ptr if the return value supports it
      if (auto *VarExpr = dynamic_cast<VariableExprAST*>(getReturnValue())) {
        Val = VarExpr->codegen_ptr();
      }
      else if (auto *ArrayIdxExpr = dynamic_cast<ArrayIndexExprAST*>(getReturnValue())) {
        Val = ArrayIdxExpr->codegen_ptr();
      }
      else if (auto *MemberExpr = dynamic_cast<MemberAccessExprAST*>(getReturnValue())) {
        Val = MemberExpr->codegen_ptr();
      }
      else {
        return LogErrorV("return ref can only be used with lvalues (variables, array elements, or struct members)");
      }
    } else {
      Val = getReturnValue()->codegen();
    }

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

  // For ref variables, need to store a pointer to the actual type
  bool isRefVar = isRef();
  TypeInfo declaredInfo = getTypeInfo();

  const ExprAST *InitializerExpr = getInitializer();
  const RefExprAST *RefInitializer = dynamic_cast<const RefExprAST*>(InitializerExpr);
  const ExprAST *NullableCheckExpr = unwrapRefExpr(InitializerExpr);
  const bool shouldCheckNullability = NullableCheckExpr && !RefInitializer;
  std::string targetDescription = "variable '" + getName() + "'";

  std::string declaredElementTypeName;
  llvm::Type *declaredElementType = nullptr;
  if (declaredInfo.isArray) {
    if (declaredInfo.typeName.size() > 2 &&
        declaredInfo.typeName.substr(declaredInfo.typeName.size() - 2) == "[]") {
      declaredElementTypeName = declaredInfo.typeName.substr(0, declaredInfo.typeName.size() - 2);
    } else {
      ParsedTypeDescriptor declaredDesc = parseTypeString(declaredInfo.typeName);
      if (declaredDesc.sanitized.size() > 2 &&
          declaredDesc.sanitized.substr(declaredDesc.sanitized.size() - 2) == "[]") {
        declaredElementTypeName = declaredDesc.sanitized.substr(0, declaredDesc.sanitized.size() - 2);
      }
    }

    if (!declaredElementTypeName.empty())
      declaredElementType = getTypeFromString(declaredElementTypeName);
  }

  auto generateInitializerValue = [&](ExprAST *expr) -> llvm::Value * {
    if (!expr)
      return nullptr;
    if (auto *ArrayInit = dynamic_cast<ArrayExprAST*>(expr)) {
      if (declaredInfo.isArray)
        return ArrayInit->codegen_with_element_target(declaredElementType, declaredElementTypeName);
    }
    return expr->codegen();
  };

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
    if (isRefVar) {
      // Check if initializer is a RefExprAST (linking to another variable)
      bool initializerIsRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      // Check if initializer is a variable reference to another ref variable
      bool shouldLink = initializerIsRef;
      if (!shouldLink && dynamic_cast<VariableExprAST*>(getInitializer())) {
        VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
        std::string refVarName = VarInit->getName();
        if (isDeclaredRefGlobal(refVarName))
          shouldLink = true;
      }

      if (shouldLink) {
        // This is linking to another variable: ref int b = a or ref int b = ref a
        // Create a pointer variable
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, AliasPtrType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize to null pointer
        GV->setInitializer(llvm::ConstantPointerNull::get(AliasPtrType));

        // Generate the initializer (should return a pointer)
        llvm::Value *InitVal;
        if (initializerIsRef) {
          InitVal = getInitializer()->codegen();
        } else {
          // Get the address of the variable
          VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
          InitVal = VarInit->codegen_ptr();
        }
        if (!InitVal)
          return nullptr;

        // Store the pointer
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (stores the pointer global)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, true));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, GV, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // This is a regular initialization: ref int a = 1
        // Create a regular variable, but mark it as ref so others can reference it
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, VarType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize with zero
        llvm::Constant *ZeroInit = llvm::Constant::getNullValue(VarType);
        GV->setInitializer(ZeroInit);

        // Generate the initializer value
        llvm::Value *InitVal = generateInitializerValue(getInitializer());
        if (!InitVal)
          return nullptr;

        if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
          return nullptr;

        // Cast and store the value
        InitVal = castToType(InitVal, VarType, getType());
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (as a regular variable)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefValue, true));

        // Return the value
        return Builder->CreateLoad(VarType, GV, getName());
      }
    } else {
      // Regular global variable
      // Check if initializer has explicit ref (e.g., int x = ref y)
      bool initializerHasRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      if (initializerHasRef) {
        // Create a pointer variable because of explicit ref
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::GlobalVariable *GV = new llvm::GlobalVariable(
            *TheModule, AliasPtrType, false, llvm::GlobalValue::ExternalLinkage,
            nullptr, getName());

        // Initialize to null pointer
        GV->setInitializer(llvm::ConstantPointerNull::get(AliasPtrType));

        // Generate the initializer (should return a pointer)
        llvm::Value *InitVal = generateInitializerValue(getInitializer());
        if (!InitVal)
          return nullptr;

        // Store the pointer
        Builder->CreateStore(InitVal, GV);

        // Remember this global binding (stores as a pointer)
        GlobalValues[getName()] = GV;
        rememberGlobalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, false));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, GV, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // Regular variable without ref
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
          llvm::Value *InitVal = generateInitializerValue(getInitializer());
          if (!InitVal)
            return nullptr;

          if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
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
        rememberGlobalType(getName(), declaredInfo);
      }
    }

    // Track array size for compile-time bounds checking
    if (!isRefVar && getType().size() > 2 && getType().substr(getType().size() - 2) == "[]") {
      // If initializer is an array literal, track its size
      if (getInitializer()) {
        if (ArrayExprAST *ArrayInit = dynamic_cast<ArrayExprAST*>(getInitializer())) {
          ArraySizes[getName()] = ArrayInit->getElements().size();
        }
      }
    }

    // Return the value that was stored
    if (isRefVar) {
      // For ref variables, already returned above
      return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
    } else {
      // For regular variables, check if it's a refptr
      if (const TypeInfo *info = lookupGlobalTypeInfo(getName()); info && info->isAlias()) {
        // Already returned above in the initializerHasRef branch
        return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
      } else {
        // Regular variable - get the GV from GlobalValues
        llvm::GlobalVariable *GV = GlobalValues[getName()];
        return Builder->CreateLoad(VarType, GV, getName());
      }
    }
  } else {
    // Local variable - use alloca
    if (isRefVar) {
      // Check if initializer is a RefExprAST (linking to another variable)
      bool initializerIsRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      // Check if initializer is a variable reference to another ref variable
      bool shouldLink = initializerIsRef;
      if (!shouldLink && dynamic_cast<VariableExprAST*>(getInitializer())) {
        VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
        std::string refVarName = VarInit->getName();
        if (isDeclaredRefLocal(refVarName) || isDeclaredRefGlobal(refVarName))
          shouldLink = true;
      }

      if (shouldLink) {
        // This is linking to another variable: ref int b = a or ref int b = ref a
        // Generate the initializer first (should return a pointer)
        llvm::Value *InitVal;
        if (initializerIsRef) {
          InitVal = getInitializer()->codegen();
        } else {
          // Get the address of the variable
          VariableExprAST *VarInit = static_cast<VariableExprAST*>(getInitializer());
          InitVal = VarInit->codegen_ptr();
        }
        if (!InitVal)
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(AliasPtrType, nullptr, getName());

        // Store the pointer in the alloca
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (stores the pointer alloca)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, true));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, Alloca, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // This is a regular initialization: ref int a = 1
        // Generate the initializer value first
        llvm::Value *InitVal = getInitializer()->codegen();
        if (!InitVal)
          return nullptr;

        if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(VarType, nullptr, getName());

        // Cast and store the value
        InitVal = castToType(InitVal, VarType, getType());
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (as a regular variable)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefValue, true));

        // Return the value
        return Builder->CreateLoad(VarType, Alloca, getName());
      }
    } else {
      // Regular variable
      // Check if initializer has explicit ref (e.g., int x = ref y)
      bool initializerHasRef = dynamic_cast<RefExprAST*>(getInitializer()) != nullptr;

      if (initializerHasRef) {
        // Generate the initializer first (should return a pointer)
        llvm::Value *InitVal = getInitializer()->codegen();
        if (!InitVal)
          return nullptr;

        // Only create the alloca after successful initialization
        llvm::PointerType *AliasPtrType = llvm::PointerType::get(*TheContext, 0);
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(AliasPtrType, nullptr, getName());

        // Store the pointer in the alloca
        Builder->CreateStore(InitVal, Alloca);

        // Remember this local binding (stores as a pointer)
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), runtimeTypeFrom(declaredInfo, RefStorageClass::RefAlias, false));

        // Return the dereferenced value (load pointer, then load value from it)
        llvm::Value *Ptr = Builder->CreateLoad(AliasPtrType, Alloca, getName());
        return Builder->CreateLoad(VarType, Ptr, (getName() + "_deref"));
      } else {
        // Regular variable without ref
        // Generate and validate the initial value first
        llvm::Value *InitVal = nullptr;
        if (getInitializer()) {
          InitVal = generateInitializerValue(getInitializer());
          if (!InitVal)
            return nullptr;

          if (shouldCheckNullability && !validateNullableAssignment(declaredInfo, NullableCheckExpr, targetDescription))
            return nullptr;
        }

        // Only create the alloca after successful initialization
        llvm::AllocaInst *Alloca = Builder->CreateAlloca(VarType, nullptr, getName());

        // Cast and store the initial value if present
        if (InitVal) {
          InitVal = castToType(InitVal, VarType, getType());
          Builder->CreateStore(InitVal, Alloca);
        }

        // Remember this local binding
        NamedValues[getName()] = Alloca;
        rememberLocalType(getName(), declaredInfo);

        // Track array size for compile-time bounds checking
        if (getType().size() > 2 && getType().substr(getType().size() - 2) == "[]") {
          // If initializer is an array literal, track its size
          if (getInitializer()) {
            if (ArrayExprAST *ArrayInit = dynamic_cast<ArrayExprAST*>(getInitializer())) {
              ArraySizes[getName()] = ArrayInit->getElements().size();
            }
          }
        }

        // Return the value that was stored
        return Builder->CreateLoad(VarType, Alloca, getName());
      }
    }
  }
}

// Generate code for expression statements
llvm::Value *ExpressionStmtAST::codegen() {
  // Generate code for the expression
  llvm::Value *V = getExpression()->codegen();
  if (!V)
    return nullptr;

  // For void expressions (like assignments), return a dummy value
  if (V->getType()->isVoidTy()) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
  }

  return V;
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
  
  // Check if CollectionVal is a struct
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
    // Old array representation: direct pointer
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
  
  // Check if condition is void (from assignment) - reject it
  if (CondV->getType()->isVoidTy()) {
    return LogErrorV("Cannot use assignment as condition in if statement (use == for comparison)");
  }

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

llvm::Value *AssertStmtAST::codegen() {
  // Generate code for the condition expression
  llvm::Value *CondValue = Condition->codegen();
  if (!CondValue)
    return nullptr;

  // Convert condition to boolean if necessary
  if (CondValue->getType() != llvm::Type::getInt8Ty(*TheContext)) {
    // For numeric types, check if != 0
    if (CondValue->getType()->isIntegerTy()) {
      llvm::Value *Zero = llvm::Constant::getNullValue(CondValue->getType());
      CondValue = Builder->CreateICmpNE(CondValue, Zero, "assertcond");
    } else if (CondValue->getType()->isFloatingPointTy()) {
      llvm::Value *Zero = llvm::ConstantFP::get(CondValue->getType(), 0.0);
      CondValue = Builder->CreateFCmpONE(CondValue, Zero, "assertcond");
    }
    // Convert to i8 for consistency
    CondValue = Builder->CreateIntCast(CondValue, llvm::Type::getInt8Ty(*TheContext), false, "assertbool");
  }

  // Create basic blocks for assertion success and failure
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *SuccessBB = llvm::BasicBlock::Create(*TheContext, "assert_success", TheFunction);
  llvm::BasicBlock *FailBB = llvm::BasicBlock::Create(*TheContext, "assert_fail", TheFunction);

  // Convert condition to i1 for branch
  llvm::Value *BoolCond = Builder->CreateICmpNE(CondValue, llvm::ConstantInt::get(llvm::Type::getInt8Ty(*TheContext), 0), "asserttest");

  // Branch based on condition
  Builder->CreateCondBr(BoolCond, SuccessBB, FailBB);

  // Generate failure block - call abort() to terminate program
  Builder->SetInsertPoint(FailBB);

  // Declare abort() function if not already declared
  llvm::Function *AbortFunc = TheModule->getFunction("abort");
  if (!AbortFunc) {
    llvm::FunctionType *AbortType = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), false);
    AbortFunc = llvm::Function::Create(AbortType, llvm::Function::ExternalLinkage, "abort", TheModule.get());
  }

  // Call abort() and create unreachable instruction
  Builder->CreateCall(AbortFunc);
  Builder->CreateUnreachable();

  // Continue with success block
  Builder->SetInsertPoint(SuccessBB);

  // Return a dummy value indicating successful assertion
  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*TheContext));
}

// Generate code for unsafe blocks
llvm::Value *UnsafeBlockStmtAST::codegen() {
  // Unsafe blocks simply execute their body
  // The safety checks are done at parse time, not at runtime
  return Body->codegen();
}

//===----------------------------------------------------------------------===//
// Function and Prototype Code Generation
//===----------------------------------------------------------------------===//

// Generate code for function prototypes
llvm::Function *PrototypeAST::codegen() {
  // Convert parameter types
  std::vector<llvm::Type*> ParamTypes;
  for (const auto &Param : Args) {
    llvm::Type *ParamType = getTypeFromString(Param.DeclaredType.typeName);
    if (!ParamType)
      return LogErrorF("Unknown parameter type");

    // If it's a ref parameter, use pointer type
    if (Param.IsRef) {
      ParamTypes.push_back(llvm::PointerType::get(*TheContext, 0));
    } else {
      ParamTypes.push_back(ParamType);
    }
  }

  // Get return type
  llvm::Type *RetType = getTypeFromString(ReturnTypeInfo.typeName);
  if (!RetType)
    return LogErrorF("Unknown return type");

  // If it returns by ref, use pointer type
  if (returnsByRef()) {
    RetType = llvm::PointerType::get(*TheContext, 0);
  }

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
  // Clear local array sizes, but keep global ones
  for (auto it = ArraySizes.begin(); it != ArraySizes.end(); ) {
    if (NamedValues.count(it->first) || LocalTypes.count(it->first)) {
      it = ArraySizes.erase(it);
    } else {
      ++it;
    }
  }
  
  // Get parameter info from the prototype
  const auto &Params = getProto()->getArgs();
  size_t i = 0;

  for (auto &Arg : TheFunction->args()) {
    // Check if this is a ref parameter
    bool isRefParam = (i < Params.size() && Params[i].IsRef);

    // Create an alloca for this variable
    llvm::AllocaInst *Alloca = Builder->CreateAlloca(Arg.getType(), nullptr, Arg.getName());

    // Store the initial value into the alloca
    Builder->CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table
    NamedValues[std::string(Arg.getName())] = Alloca;

    // Track the type of the parameter
    if (i < Params.size()) {
      if (isRefParam) {
        TypeInfo paramInfo = Params[i].DeclaredType;
        paramInfo.refStorage = RefStorageClass::RefAlias;
        paramInfo.declaredRef = true;
        rememberLocalType(std::string(Arg.getName()), std::move(paramInfo));
      } else {
        rememberLocalType(std::string(Arg.getName()), Params[i].DeclaredType);
      }
    }
    i++;
  }
  
  // Emit global variable initializers once at the beginning of main()
  static bool InitializedGlobalsEmitted = false;
  if (!InitializedGlobalsEmitted && TheFunction->getName() == "main") {
    if (auto *InitFunc = TheModule->getFunction("__anon_var_decl")) {
      if (!InitFunc->empty()) {
        Builder->CreateCall(InitFunc);
        InitializedGlobalsEmitted = true;
      }
    }
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
  Builder->ClearInsertionPoint();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// Struct Code Generation
//===----------------------------------------------------------------------===//

// Generate code for struct definitions
llvm::Type *StructAST::codegen() {
  // Check if this struct type already exists
  if (StructTypes.contains(Name)) {
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
      // This is a constructor, generate it as a static method that materializes a new instance
      std::string ConstructorName = Name + "_new";

      // Build parameter list matching the constructor prototype
      const auto &CtorArgs = Method->getProto()->getArgs();
      std::vector<llvm::Type*> ParamTypes;
      ParamTypes.reserve(CtorArgs.size());
      for (const auto &Param : CtorArgs) {
        llvm::Type *ParamType = getTypeFromString(Param.DeclaredType.typeName);
        if (!ParamType)
          return nullptr;

        if (Param.IsRef)
          ParamType = llvm::PointerType::get(*TheContext, 0);

        ParamTypes.push_back(ParamType);
      }

      llvm::FunctionType *CtorFnType = llvm::FunctionType::get(StructType, ParamTypes, false);

      llvm::Function *ConstructorFunc = llvm::Function::Create(
          CtorFnType, llvm::Function::ExternalLinkage, ConstructorName, TheModule.get());

      // Name parameters for easier IR inspection and debugging
      unsigned argIndex = 0;
      for (auto &Arg : ConstructorFunc->args())
        Arg.setName(CtorArgs[argIndex++].Name);

      // Create a basic block to start insertion into
      llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", ConstructorFunc);
      Builder->SetInsertPoint(BB);

      // Allocate stack storage for the struct instance we are building
      llvm::AllocaInst *StructPtr = Builder->CreateAlloca(StructType, nullptr, "struct_alloc");

      // 'this' refers to the stack allocation we just created
      NamedValues.clear();
      LocalTypes.clear();
      NamedValues["this"] = StructPtr;
      rememberLocalType("this", makeTypeInfo(Name));

      // Spill parameters to the stack just like regular functions
      argIndex = 0;
      for (auto &Arg : ConstructorFunc->args()) {
        const auto &Param = CtorArgs[argIndex];
        std::string ArgName = Param.Name;

        llvm::AllocaInst *Alloca = Builder->CreateAlloca(
            Arg.getType(), nullptr, ArgName);
        Builder->CreateStore(&Arg, Alloca);

        NamedValues[ArgName] = Alloca;
        if (Param.IsRef) {
          TypeInfo paramInfo = Param.DeclaredType;
          paramInfo.refStorage = RefStorageClass::RefAlias;
          paramInfo.declaredRef = true;
          rememberLocalType(ArgName, std::move(paramInfo));
        } else {
          rememberLocalType(ArgName, Param.DeclaredType);
        }
        ++argIndex;
      }

      // Generate the constructor body
      if (Method->getBody()->codegen()) {
        if (!Builder->GetInsertBlock()->getTerminator()) {
          llvm::Value *StructValue = Builder->CreateLoad(
              StructType, StructPtr, "struct_value");
          Builder->CreateRet(StructValue);
        }

        llvm::verifyFunction(*ConstructorFunc);
      } else {
        ConstructorFunc->eraseFromParent();
        NamedValues.clear();
        LocalTypes.clear();
        Builder->ClearInsertionPoint();
        return nullptr;
      }

      // Clear local values now that the constructor is done
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
  ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
  if (ObjectTypeDesc.isNullable) {
    return LogErrorV(("Cannot access nullable type '" + ObjectTypeName + "' without null-safe operator").c_str());
  }
  std::string StructLookupName = ObjectTypeDesc.sanitized;


  // If ObjectTypeName is empty but have a valid pointer, it might be a nested struct access
  // In that case, the ObjectPtr is already a pointer to a struct
  if (ObjectTypeName.empty() && ObjectPtr->getType()->isPointerTy()) {
    // In newer LLVM, pointers are opaque, so track the type differently
    // For now, rely on the type name being set correctly by the previous member access
    // This is a limitation that needs a better solution
  }
  
  // Find the struct type
  auto StructIt = StructTypes.find(StructLookupName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(StructLookupName);
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
  auto FieldTypesIt = StructFieldTypes.find(StructLookupName);
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
  ParsedTypeDescriptor ObjectTypeDesc = parseTypeString(ObjectTypeName);
  if (ObjectTypeDesc.isNullable) {
    return LogErrorV(("Cannot access nullable type '" + ObjectTypeName + "' without null-safe operator").c_str());
  }
  std::string StructLookupName = ObjectTypeDesc.sanitized;
  
  // Find the struct type
  auto StructIt = StructTypes.find(StructLookupName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }
  
  // Find the field index
  auto FieldIndicesIt = StructFieldIndices.find(StructLookupName);
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

llvm::Value *NullSafeAccessExprAST::codegen() {
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr)
    return nullptr;

  if (!ObjectPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe access requires pointer-compatible object type");

  std::string ObjectTypeName = Object->getTypeName();
  ParsedTypeDescriptor ObjectDesc = parseTypeString(ObjectTypeName);
  std::string StructName = ObjectDesc.sanitized;

  auto StructIt = StructTypes.find(StructName);
  if (StructIt == StructTypes.end()) {
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());
  }

  auto FieldIndicesIt = StructFieldIndices.find(StructName);
  if (FieldIndicesIt == StructFieldIndices.end()) {
    return LogErrorV("Internal error: struct field indices not found");
  }

  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }

  if (!FieldFound)
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());

  llvm::Type *FieldType = StructIt->second->getElementType(FieldIndex);
  if (!FieldType->isPointerTy()) {
    return LogErrorV("Null-safe access is only supported for reference-type fields");
  }

  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();
  llvm::Function *Func = CurrentBB->getParent();

  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.notnull", Func);
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.null", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.merge", Func);

  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ObjectPtr->getType()));
  llvm::Value *IsNull = Builder->CreateICmpEQ(ObjectPtr, NullPtr, "nullsafe.check");
  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  // Not-null branch
  Builder->SetInsertPoint(NotNullBB);
  std::vector<llvm::Value *> Indices = {
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))};
  llvm::Value *FieldPtr = Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  llvm::Value *LoadedField = Builder->CreateLoad(FieldType, FieldPtr, MemberName + ".val");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  // Null branch
  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullValue = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(FieldType));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  // Merge
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(FieldType, 2, MemberName + ".maybe");
  Phi->addIncoming(LoadedField, NotNullEnd);
  Phi->addIncoming(NullValue, NullEnd);

  std::string FieldTypeName;
  if (auto FieldTypesIt = StructFieldTypes.find(StructName); FieldTypesIt != StructFieldTypes.end()) {
    if (auto TypeIt = FieldTypesIt->second.find(MemberName); TypeIt != FieldTypesIt->second.end()) {
      FieldTypeName = TypeIt->second;
    }
  }
  if (!FieldTypeName.empty())
    setTypeName(ensureOuterNullable(FieldTypeName));
  else
    setTypeName("unknown?");

  return Phi;
}

llvm::Value *NullSafeAccessExprAST::codegen_ptr() {
  llvm::Value *ObjectPtr = Object->codegen();
  if (!ObjectPtr)
    return nullptr;

  if (!ObjectPtr->getType()->isPointerTy())
    return LogErrorV("Null-safe access requires pointer-compatible object type");

  std::string ObjectTypeName = Object->getTypeName();
  ParsedTypeDescriptor ObjectDesc = parseTypeString(ObjectTypeName);
  std::string StructName = ObjectDesc.sanitized;

  auto StructIt = StructTypes.find(StructName);
  if (StructIt == StructTypes.end())
    return LogErrorV(("Object is not a struct type: " + ObjectTypeName).c_str());

  auto FieldIndicesIt = StructFieldIndices.find(StructName);
  if (FieldIndicesIt == StructFieldIndices.end())
    return LogErrorV("Internal error: struct field indices not found");

  unsigned FieldIndex = 0;
  bool FieldFound = false;
  for (const auto &Field : FieldIndicesIt->second) {
    if (Field.first == MemberName) {
      FieldIndex = Field.second;
      FieldFound = true;
      break;
    }
  }

  if (!FieldFound)
    return LogErrorV(("Field not found in struct: " + MemberName).c_str());

  std::vector<llvm::Value *> Indices = {
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, 0)),
      llvm::ConstantInt::get(*TheContext, llvm::APInt(32, FieldIndex))};

  llvm::BasicBlock *CurrentBB = Builder->GetInsertBlock();
  llvm::Function *Func = CurrentBB->getParent();

  llvm::BasicBlock *NotNullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.notnull", Func);
  llvm::BasicBlock *NullBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.null", Func);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "nullsafe.ptr.merge", Func);

  llvm::Value *NullPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ObjectPtr->getType()));
  llvm::Value *IsNull = Builder->CreateICmpEQ(ObjectPtr, NullPtr, "nullsafe.ptr.check");
  Builder->CreateCondBr(IsNull, NullBB, NotNullBB);

  Builder->SetInsertPoint(NotNullBB);
  llvm::Value *FieldPtr = Builder->CreateGEP(StructIt->second, ObjectPtr, Indices, MemberName + "_ptr");
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NotNullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(NullBB);
  llvm::Value *NullFieldPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(FieldPtr->getType()));
  Builder->CreateBr(MergeBB);
  llvm::BasicBlock *NullEnd = Builder->GetInsertBlock();

  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *Phi = Builder->CreatePHI(FieldPtr->getType(), 2, MemberName + ".ptrmaybe");
  Phi->addIncoming(FieldPtr, NotNullEnd);
  Phi->addIncoming(NullFieldPtr, NullEnd);

  return Phi;
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
    setTypeName(TypeIt->second.typeName);
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
      return LogErrorV("Ternary condition must be a numeric type");
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
      return LogErrorV("Ternary expression branches must have compatible types");
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
