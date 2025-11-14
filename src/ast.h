#ifndef AST_H
#define AST_H

#include <memory>
#include <span>
#include <string>
#include <vector>
#include <cstdint>
#include <map>
#include <optional>
#include <utility>
#include "concepts.h"
#include "numeric_literal.h"

struct SourceLocation;
struct FunctionOverload;

enum class RefStorageClass {
  None,
  RefValue,
  RefAlias
};

struct ClassDescriptor;

enum class OwnershipQualifier : uint8_t {
  Strong,
  Weak,
  Unowned
};

enum class SmartPointerKind : uint8_t {
  None,
  Unique,
  Shared,
  Weak
};

struct GenericBindingKey {
  std::string typeName;
  std::vector<std::string> typeArguments;

  bool empty() const { return typeName.empty() && typeArguments.empty(); }
};

struct TypeInfo {
  std::string typeName;           // canonical language-visible type
  std::string baseTypeName;       // base identifier without generic arguments or suffixes
  std::vector<TypeInfo> typeArguments; // explicit generic type arguments, if any
  unsigned pointerDepth = 0;      // number of explicit pointer levels (@)
  bool isArray = false;           // whether type is an array ("[]")
  unsigned arrayDepth = 0;        // number of array segments ([], [,,], etc.)
  std::vector<unsigned> arrayRanks; // rank of each array segment (1 = jagged, >1 = multidimensional)
  bool isMultidimensional = false; // true if any array segment has rank > 1
  bool isNullable = false;        // whether the type itself can be null
  bool elementNullable = false;   // whether array elements can be null
  RefStorageClass refStorage = RefStorageClass::None;
  bool isMutable = true;
  bool declaredRef = false;       // whether declared via `ref`
  bool isGenericParameter = false; // true when the type refers to a generic parameter
  OwnershipQualifier ownership = OwnershipQualifier::Strong;
  SmartPointerKind smartPointerKind = SmartPointerKind::None;
  bool arcManaged = false;
  const ClassDescriptor *classDescriptor = nullptr;
  GenericBindingKey genericKey;

  bool isReference() const { return refStorage != RefStorageClass::None; }
  bool ownsStorage() const { return refStorage == RefStorageClass::RefValue; }
  bool isAlias() const { return refStorage == RefStorageClass::RefAlias; }
  bool hasTypeArguments() const { return !typeArguments.empty(); }
  bool isSmartPointer() const { return smartPointerKind != SmartPointerKind::None; }
  bool participatesInARC() const { return arcManaged; }
  bool requiresARC() const {
    return participatesInARC() && ownership == OwnershipQualifier::Strong;
  }
  const GenericBindingKey &bindingKey() const { return genericKey; }
};

void finalizeTypeInfoMetadata(TypeInfo &info);

enum class AggregateKind : uint8_t {
  Struct,
  Class,
  Interface
};

enum class AccessFlag : uint8_t {
  None = 0,
  ReadPublic = 1 << 0,
  WritePublic = 1 << 1,
  ReadProtected = 1 << 2,
  WriteProtected = 1 << 3,
  ReadPrivate = 1 << 4,
  WritePrivate = 1 << 5
};

constexpr inline AccessFlag operator|(AccessFlag lhs, AccessFlag rhs) {
  return static_cast<AccessFlag>(
      static_cast<uint8_t>(lhs) | static_cast<uint8_t>(rhs));
}

constexpr inline AccessFlag operator&(AccessFlag lhs, AccessFlag rhs) {
  return static_cast<AccessFlag>(
      static_cast<uint8_t>(lhs) & static_cast<uint8_t>(rhs));
}

constexpr inline AccessFlag operator~(AccessFlag flag) {
  return static_cast<AccessFlag>(~static_cast<uint8_t>(flag));
}

inline AccessFlag &operator|=(AccessFlag &lhs, AccessFlag rhs) {
  lhs = lhs | rhs;
  return lhs;
}

inline AccessFlag &operator&=(AccessFlag &lhs, AccessFlag rhs) {
  lhs = lhs & rhs;
  return lhs;
}

enum class StorageFlag : uint8_t {
  None = 0,
  Static = 1 << 0,
  Const = 1 << 1
};

constexpr inline StorageFlag operator|(StorageFlag lhs, StorageFlag rhs) {
  return static_cast<StorageFlag>(
      static_cast<uint8_t>(lhs) | static_cast<uint8_t>(rhs));
}

constexpr inline StorageFlag operator&(StorageFlag lhs, StorageFlag rhs) {
  return static_cast<StorageFlag>(
      static_cast<uint8_t>(lhs) & static_cast<uint8_t>(rhs));
}

constexpr inline StorageFlag operator~(StorageFlag flag) {
  return static_cast<StorageFlag>(~static_cast<uint8_t>(flag));
}

inline StorageFlag &operator|=(StorageFlag &lhs, StorageFlag rhs) {
  lhs = lhs | rhs;
  return lhs;
}

inline StorageFlag &operator&=(StorageFlag &lhs, StorageFlag rhs) {
  lhs = lhs & rhs;
  return lhs;
}

struct MemberAccess {
  AccessFlag flags = AccessFlag::None;

  bool has(AccessFlag flag) const {
    return static_cast<uint8_t>(flags & flag) != 0;
  }

  void add(AccessFlag flag) { flags |= flag; }

  static MemberAccess None() { return MemberAccess{AccessFlag::None}; }
  static MemberAccess PublicReadWrite() {
    return MemberAccess{AccessFlag::ReadPublic | AccessFlag::WritePublic};
  }
  static MemberAccess ReadPublicWritePrivate() {
    return MemberAccess{AccessFlag::ReadPublic | AccessFlag::WritePrivate};
  }
  static MemberAccess PrivateOnly() {
    return MemberAccess{AccessFlag::ReadPrivate | AccessFlag::WritePrivate};
  }
  static MemberAccess ProtectedReadWrite() {
    return MemberAccess{AccessFlag::ReadProtected | AccessFlag::WriteProtected};
  }
};

struct MemberModifiers {
  MemberAccess access{};
  StorageFlag storage = StorageFlag::None;
  bool isOverride = false;
  bool isVirtual = false;
  bool isAbstract = false;
  bool isProperty = false;
};

// LLVM forward declarations
namespace llvm {
  class Value;
  class Function;
}

#include "llvm/IR/Value.h"

void FinalizeTopLevelExecution();

/// ExprAST - Base class for all expression nodes.
class ExprAST {
protected:
  mutable std::string TypeName; // The type name of this expression (e.g. "int", "byte", "float")
  
public:
  virtual ~ExprAST() = default;
  [[nodiscard]] virtual llvm::Value *codegen() = 0;
  [[nodiscard]] virtual llvm::Value *codegen_ptr() { return nullptr; }
  
  // Get the type name of this expression
  [[nodiscard]] virtual std::string getTypeName() const { return TypeName; }
  
  // Set the type name (used during codegen)
  void setTypeName(const std::string &TN) const { TypeName = TN; }
};

/// StmtAST - Base class for all statement nodes.
class StmtAST {
public:
  virtual ~StmtAST() = default;
  [[nodiscard]] virtual llvm::Value *codegen() = 0;
};

/// ReturnStmtAST - Statement class for return statements.
class ReturnStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> ReturnValue;
  bool IsRef;

public:
  ReturnStmtAST(std::unique_ptr<ExprAST> ReturnValue, bool IsRef = false)
      : ReturnValue(std::move(ReturnValue)), IsRef(IsRef) {}

  llvm::Value *codegen() override;
  ExprAST *getReturnValue() const { return ReturnValue.get(); }
  bool isRef() const { return IsRef; }
};

/// BlockStmtAST - Statement class for statement blocks.
class BlockStmtAST : public StmtAST {
  std::vector<std::unique_ptr<StmtAST>> Statements;

public:
  BlockStmtAST(std::vector<std::unique_ptr<StmtAST>> Statements)
      : Statements(std::move(Statements)) {}
  
  llvm::Value *codegen() override;
  const std::vector<std::unique_ptr<StmtAST>> &getStatements() const { return Statements; }
};

/// VariableDeclarationStmtAST - Statement class for variable declarations.
class VariableDeclarationStmtAST : public StmtAST {
  TypeInfo DeclaredType;
  std::string Name;
  std::unique_ptr<ExprAST> Initializer;
  bool IsRef;

public:
  VariableDeclarationStmtAST(TypeInfo DeclaredType, const std::string &Name,
                             std::unique_ptr<ExprAST> Initializer, bool IsRef = false)
      : DeclaredType(std::move(DeclaredType)), Name(Name), Initializer(std::move(Initializer)), IsRef(IsRef) {}

  llvm::Value *codegen() override;
  const std::string &getType() const { return DeclaredType.typeName; }
  const TypeInfo &getTypeInfo() const { return DeclaredType; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  ExprAST *getInitializer() const { return Initializer.get(); }
  bool isRef() const { return IsRef; }
};

/// ExpressionStmtAST - Statement class for expression statements.
class ExpressionStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Expression;

public:
  ExpressionStmtAST(std::unique_ptr<ExprAST> Expression)
      : Expression(std::move(Expression)) {}
  
  llvm::Value *codegen() override;
  ExprAST *getExpression() const { return Expression.get(); }
};

/// ForEachStmtAST - Statement class for foreach loops.
class ForEachStmtAST : public StmtAST {
  TypeInfo DeclaredType;
  std::string VarName;
  std::unique_ptr<ExprAST> Collection;
  std::unique_ptr<BlockStmtAST> Body;

public:
  ForEachStmtAST(TypeInfo DeclaredType, const std::string &VarName,
                 std::unique_ptr<ExprAST> Collection,
                 std::unique_ptr<BlockStmtAST> Body)
      : DeclaredType(std::move(DeclaredType)), VarName(VarName), Collection(std::move(Collection)),
        Body(std::move(Body)) {}

  const TypeInfo &getTypeInfo() const { return DeclaredType; }
  const std::string &getTypeName() const { return DeclaredType.typeName; }
  const std::string &getVarName() const { return VarName; }
  bool isRef() const { return DeclaredType.declaredRef; }
  
  void print() const;
  llvm::Value *codegen() override;
};

/// ForLoopStmtAST - Statement class for for loops with 'to' syntax.
class ForLoopStmtAST : public StmtAST {
  std::string Type;
  std::string VarName;
  std::unique_ptr<ExprAST> InitExpr;
  std::unique_ptr<ExprAST> LimitExpr;  // Either a limit value or null if using CondExpr
  std::unique_ptr<ExprAST> CondExpr;   // Optional condition expression (for i < size)
  std::unique_ptr<ExprAST> StepExpr;   // Optional step expression
  char StepOp;  // Step operation: '+' (default), '*', '/', '%', '-'
  std::unique_ptr<BlockStmtAST> Body;

public:
  ForLoopStmtAST(const std::string &Type, const std::string &VarName,
                 std::unique_ptr<ExprAST> InitExpr,
                 std::unique_ptr<ExprAST> LimitExpr,
                 std::unique_ptr<BlockStmtAST> Body,
                 std::unique_ptr<ExprAST> StepExpr = nullptr,
                 char StepOp = '+',
                 std::unique_ptr<ExprAST> CondExpr = nullptr)
      : Type(Type), VarName(VarName), InitExpr(std::move(InitExpr)),
        LimitExpr(std::move(LimitExpr)), CondExpr(std::move(CondExpr)),
        StepExpr(std::move(StepExpr)), StepOp(StepOp), Body(std::move(Body)) {}
  
  void print() const;
  llvm::Value *codegen() override;
};

/// IfStmtAST - Statement class for if-else statements.
class IfStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<StmtAST> ThenBranch;
  std::unique_ptr<StmtAST> ElseBranch;

public:
  IfStmtAST(std::unique_ptr<ExprAST> Condition,
            std::unique_ptr<StmtAST> ThenBranch,
            std::unique_ptr<StmtAST> ElseBranch = nullptr)
      : Condition(std::move(Condition)), ThenBranch(std::move(ThenBranch)),
        ElseBranch(std::move(ElseBranch)) {}
  
  llvm::Value *codegen() override;
  ExprAST *getCondition() const { return Condition.get(); }
  StmtAST *getThenBranch() const { return ThenBranch.get(); }
  StmtAST *getElseBranch() const { return ElseBranch.get(); }
};

/// WhileStmtAST - Statement class for while loops.
class WhileStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<StmtAST> Body;

public:
  WhileStmtAST(std::unique_ptr<ExprAST> Condition,
               std::unique_ptr<StmtAST> Body)
      : Condition(std::move(Condition)), Body(std::move(Body)) {}
  
  llvm::Value *codegen() override;
  ExprAST *getCondition() const { return Condition.get(); }
  StmtAST *getBody() const { return Body.get(); }
};

/// BreakStmtAST - Statement class for break statements.
class BreakStmtAST : public StmtAST {
public:
  BreakStmtAST() {}
  
  llvm::Value *codegen() override;
};

/// SkipStmtAST - Statement class for skip statements (continue).
class SkipStmtAST : public StmtAST {
public:
  SkipStmtAST() {}

  llvm::Value *codegen() override;
};

/// AssertStmtAST - Statement class for assert statements.
class AssertStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Condition;

public:
  AssertStmtAST(std::unique_ptr<ExprAST> Condition)
      : Condition(std::move(Condition)) {}

  llvm::Value *codegen() override;
  ExprAST *getCondition() const { return Condition.get(); }
};

/// UnsafeBlockStmtAST - Statement class for unsafe blocks.
class UnsafeBlockStmtAST : public StmtAST {
  std::unique_ptr<BlockStmtAST> Body;

public:
  UnsafeBlockStmtAST(std::unique_ptr<BlockStmtAST> Body)
      : Body(std::move(Body)) {}

  llvm::Value *codegen() override;
  BlockStmtAST *getBody() const { return Body.get(); }
};

/// CaseAST - Represents a single case clause in a switch.
class CaseAST {
  std::vector<std::unique_ptr<ExprAST>> Values;  // Multiple values for same case
  std::unique_ptr<StmtAST> Body;                 // Block for statement switch
  std::unique_ptr<ExprAST> Expression;           // Expression for expression switch
  bool IsDefault;

public:
  // Constructor for statement cases
  CaseAST(std::vector<std::unique_ptr<ExprAST>> Values, std::unique_ptr<StmtAST> Body, bool IsDefault = false)
      : Values(std::move(Values)), Body(std::move(Body)), IsDefault(IsDefault) {}
  
  // Constructor for expression cases
  CaseAST(std::vector<std::unique_ptr<ExprAST>> Values, std::unique_ptr<ExprAST> Expression, bool IsDefault = false)
      : Values(std::move(Values)), Expression(std::move(Expression)), IsDefault(IsDefault) {}
  
  const std::vector<std::unique_ptr<ExprAST>> &getValues() const { return Values; }
  StmtAST *getBody() const { return Body.get(); }
  ExprAST *getExpression() const { return Expression.get(); }
  bool isDefault() const { return IsDefault; }
};

/// SwitchStmtAST - Statement class for switch statements with block syntax.
class SwitchStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Condition;
  std::vector<std::unique_ptr<CaseAST>> Cases;

public:
  SwitchStmtAST(std::unique_ptr<ExprAST> Condition,
                std::vector<std::unique_ptr<CaseAST>> Cases)
      : Condition(std::move(Condition)), Cases(std::move(Cases)) {}
  
  llvm::Value *codegen() override;
  ExprAST *getCondition() const { return Condition.get(); }
  const std::vector<std::unique_ptr<CaseAST>> &getCases() const { return Cases; }
};

/// SwitchExprAST - Expression class for switch expressions with arrow syntax.
class SwitchExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Condition;
  std::vector<std::unique_ptr<CaseAST>> Cases;

public:
  SwitchExprAST(std::unique_ptr<ExprAST> Condition,
                std::vector<std::unique_ptr<CaseAST>> Cases)
      : Condition(std::move(Condition)), Cases(std::move(Cases)) {}

  llvm::Value *codegen() override;
  ExprAST *getCondition() const { return Condition.get(); }
  const std::vector<std::unique_ptr<CaseAST>> &getCases() const { return Cases; }
};

/// TernaryExprAST - Expression class for ternary conditional expressions (a if b else c).
class TernaryExprAST : public ExprAST {
  std::unique_ptr<ExprAST> ThenExpr;
  std::unique_ptr<ExprAST> Condition;
  std::unique_ptr<ExprAST> ElseExpr;

public:
  TernaryExprAST(std::unique_ptr<ExprAST> ThenExpr,
                 std::unique_ptr<ExprAST> Condition,
                 std::unique_ptr<ExprAST> ElseExpr)
      : ThenExpr(std::move(ThenExpr)), Condition(std::move(Condition)),
        ElseExpr(std::move(ElseExpr)) {}

  llvm::Value *codegen() override;
  ExprAST *getThenExpr() const { return ThenExpr.get(); }
  ExprAST *getCondition() const { return Condition.get(); }
  ExprAST *getElseExpr() const { return ElseExpr.get(); }
};

/// UseStmtAST - Statement class for use (import) statements.
class UseStmtAST : public StmtAST {
  std::string Module;

public:
  UseStmtAST(const std::string &Module) : Module(Module) {}
  
  const std::string &getModule() const { return Module; }
  llvm::Value *codegen() override;
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  NumericLiteral Literal;

public:
  explicit NumberExprAST(NumericLiteral Literal)
      : Literal(std::move(Literal)) {}

  llvm::Value *codegen() override;
  llvm::Value *codegen_with_target(llvm::Type *TargetType);

  [[nodiscard]] const NumericLiteral &getLiteral() const { return Literal; }
  [[nodiscard]] bool isIntegerLiteral() const { return Literal.isInteger(); }
  [[nodiscard]] double getValue() const { return Literal.toDouble(); }
};

/// BoolExprAST - Expression class for boolean literals (true/false).
class BoolExprAST : public ExprAST {
  bool Val;

public:
  BoolExprAST(bool Val) : Val(Val) {}
  
  llvm::Value *codegen() override;
  [[nodiscard]] bool getValue() const { return Val; }
};

/// NullExprAST - Expression class for null literal.
class NullExprAST : public ExprAST {
public:
  NullExprAST() {}
  
  llvm::Value *codegen() override;
};

/// ParenExprAST - Expression class for parenthesized expressions and tuples.
class ParenExprAST : public ExprAST {
  std::vector<std::unique_ptr<ExprAST>> Elements;
  bool IsTupleExpr = false;

public:
  ParenExprAST(std::vector<std::unique_ptr<ExprAST>> Elements, bool IsTuple)
      : Elements(std::move(Elements)), IsTupleExpr(IsTuple) {}

  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;

  bool isTuple() const { return IsTupleExpr; }
  size_t size() const { return Elements.size(); }
  ExprAST *getElement(size_t index) const { return Elements[index].get(); }

  std::vector<std::unique_ptr<ExprAST>> takeElements() {
    return std::move(Elements);
  }

  std::unique_ptr<ExprAST> takeSingleElement() {
    if (Elements.empty())
      return nullptr;
    auto Result = std::move(Elements.front());
    Elements.clear();
    return Result;
  }
};

/// StringExprAST - Expression class for string literals like "hello".
class StringExprAST : public ExprAST {
  std::string Val;

public:
  StringExprAST(const std::string &Val) : Val(Val) {}
  
  llvm::Value *codegen() override;
  const std::string &getValue() const { return Val; }
};

/// InterpolatedStringExprAST - Expression class for interpolated strings.
class InterpolatedStringExprAST : public ExprAST {
public:
  struct Segment {
    enum class Kind { Literal, Expression };

    Kind kind = Kind::Literal;
    std::string literalValue;
    std::unique_ptr<ExprAST> expressionValue;
    std::optional<std::string> formatSpec;

    static Segment makeLiteral(std::string text) {
      Segment seg;
      seg.kind = Kind::Literal;
      seg.literalValue = std::move(text);
      return seg;
    }

    static Segment makeExpression(std::unique_ptr<ExprAST> expr,
                                  std::optional<std::string> fmt) {
      Segment seg;
      seg.kind = Kind::Expression;
      seg.expressionValue = std::move(expr);
      seg.formatSpec = std::move(fmt);
    return seg;
    }

    bool isLiteral() const { return kind == Kind::Literal; }
    bool isExpression() const { return kind == Kind::Expression; }

    const std::string &getLiteral() const { return literalValue; }
    void appendLiteral(const std::string &more) { literalValue += more; }

    ExprAST *getExpression() const { return expressionValue.get(); }
    std::unique_ptr<ExprAST> takeExpression() {
      return std::move(expressionValue);
    }

    const std::optional<std::string> &getFormatSpec() const {
      return formatSpec;
    }
  };

private:
  std::vector<Segment> Segments;

public:
  explicit InterpolatedStringExprAST(std::vector<Segment> Segments)
      : Segments(std::move(Segments)) {}

  llvm::Value *codegen() override;
  const std::vector<Segment> &getSegments() const { return Segments; }
};

/// CharExprAST - Expression class for character literals like 'a'.
class CharExprAST : public ExprAST {
  uint32_t Val;

public:
  CharExprAST(uint32_t Val) : Val(Val) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_with_target(llvm::Type *TargetType,
                                   const std::string &TargetTypeName = "");
  uint32_t getValue() const { return Val; }
};

/// ArrayExprAST - Expression class for array literals like [1, 2, 3].
class ArrayExprAST : public ExprAST {
  std::string ElementType;
  std::vector<std::unique_ptr<ExprAST>> Elements;

public:
  ArrayExprAST(const std::string &ElementType,
               std::vector<std::unique_ptr<ExprAST>> Elements)
      : ElementType(ElementType), Elements(std::move(Elements)) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_with_element_target(llvm::Type *TargetElementType = nullptr,
                                           const std::string &TargetElementTypeName = "",
                                           const TypeInfo *DeclaredArrayInfo = nullptr);
  [[nodiscard]] const std::string &getElementType() const { return ElementType; }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>> &getElements() const { return Elements; }
  [[nodiscard]] std::span<const std::unique_ptr<ExprAST>> getElementsSpan() const { return Elements; }
};

/// ArrayIndexExprAST - Expression class for array indexing like arr[0].
class ArrayIndexExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Array;
  std::vector<std::unique_ptr<ExprAST>> Indices;

public:
  ArrayIndexExprAST(std::unique_ptr<ExprAST> Array,
                    std::vector<std::unique_ptr<ExprAST>> Indices)
      : Array(std::move(Array)), Indices(std::move(Indices)) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  ExprAST *getArray() const { return Array.get(); }
  const std::vector<std::unique_ptr<ExprAST>> &getIndices() const { return Indices; }
  size_t getIndexCount() const { return Indices.size(); }
  ExprAST *getIndex(size_t idx = 0) const {
    if (idx >= Indices.size())
      return nullptr;
    return Indices[idx].get();
  }
};

/// NullSafeElementAccessExprAST - Expression class for null-safe element access like arr?[0].
class NullSafeElementAccessExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Array;
  std::unique_ptr<ExprAST> Index;

public:
  NullSafeElementAccessExprAST(std::unique_ptr<ExprAST> Array,
                            std::unique_ptr<ExprAST> Index)
      : Array(std::move(Array)), Index(std::move(Index)) {}

  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  ExprAST *getArray() const { return Array.get(); }
  ExprAST *getIndex() const { return Index.get(); }
};

/// VariableExprAST - Expression class for referencing a variable, like "var".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  [[nodiscard]] const std::string &getName() const { return Name; }
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  std::string Op;  // Supports multi-char operators
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  
  llvm::Value *codegen() override;
  [[nodiscard]] const std::string &getOp() const { return Op; }
  ExprAST *getLHS() const { return LHS.get(); }
  ExprAST *getRHS() const { return RHS.get(); }

private:
  llvm::Value *codegenNullCoalescing(llvm::Value *lhsValue);
  llvm::Value *codegenNullCoalescingAssign(llvm::Value *lhsValue);
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST {
  std::string Op;
  std::unique_ptr<ExprAST> Operand;
  bool isPrefix;

public:
  UnaryExprAST(const std::string &Op, std::unique_ptr<ExprAST> Operand, bool isPrefix)
      : Op(Op), Operand(std::move(Operand)), isPrefix(isPrefix) {}

  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  [[nodiscard]] const std::string &getOp() const { return Op; }
  ExprAST *getOperand() const { return Operand.get(); }
  bool getIsPrefix() const { return isPrefix; }
};

/// CastExprAST - Expression class for type casting.
class CastExprAST : public ExprAST {
  std::string TargetType;
  std::unique_ptr<ExprAST> Operand;

public:
  CastExprAST(const std::string &TargetType, std::unique_ptr<ExprAST> Operand)
      : TargetType(TargetType), Operand(std::move(Operand)) {}

  llvm::Value *codegen() override;
  const std::string &getTargetType() const { return TargetType; }
  ExprAST *getOperand() const { return Operand.get(); }
};

/// RefExprAST - Expression class for ref argument expressions (e.g. ref x in function calls).
class RefExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Operand;

public:
  RefExprAST(std::unique_ptr<ExprAST> Operand)
      : Operand(std::move(Operand)) {}

  llvm::Value *codegen() override;
  ExprAST *getOperand() const { return Operand.get(); }
};

class MemberAccessExprAST;

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::unique_ptr<ExprAST> CalleeExpr;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  CallExprAST(std::unique_ptr<ExprAST> CalleeExpr,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : CalleeExpr(std::move(CalleeExpr)), Args(std::move(Args)) {}

  llvm::Value *codegen() override;
  [[nodiscard]] const std::string &getCallee() const { return Callee; }
  ExprAST *getCalleeExpr() const { return CalleeExpr.get(); }
  bool hasCalleeExpr() const { return static_cast<bool>(CalleeExpr); }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>> &getArgs() const { return Args; }

private:
  llvm::Value *emitResolvedCall(const std::string &callee,
                                std::vector<llvm::Value *> ArgValues,
                                const std::vector<bool> &ArgIsRef,
                                bool preferGeneric = false,
                                FunctionOverload *forced = nullptr);
  llvm::Value *codegenMemberCall(MemberAccessExprAST &member);
};

/// Parameter - Represents a function parameter with type and name
struct Parameter {
  std::string Type;
  std::string Name;
  bool IsRef = false;
  TypeInfo DeclaredType;
};

/// PrototypeAST - Represents the "prototype" for a function,
/// which captures its name, and its argument names and types.
class PrototypeAST {
  TypeInfo ReturnTypeInfo;
  std::string Name;
  std::vector<Parameter> Args;
  std::vector<std::string> GenericParameters;
  bool IsUnsafe;
  bool ReturnsByRef;
  bool IsExtern = false;
  mutable std::string MangledName;

public:
  PrototypeAST(TypeInfo ReturnTypeInfo, const std::string &Name,
               std::vector<Parameter> Args, bool IsUnsafe = false, bool ReturnsByRef = false,
               std::vector<std::string> GenericParams = {})
      : ReturnTypeInfo(std::move(ReturnTypeInfo)), Name(Name), Args(std::move(Args)),
        GenericParameters(std::move(GenericParams)), IsUnsafe(IsUnsafe), ReturnsByRef(ReturnsByRef) {}

  const std::string &getReturnType() const { return ReturnTypeInfo.typeName; }
  const TypeInfo &getReturnTypeInfo() const { return ReturnTypeInfo; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  const std::vector<Parameter> &getArgs() const { return Args; }
  const std::vector<std::string> &getGenericParameters() const { return GenericParameters; }
  bool isUnsafe() const { return IsUnsafe; }
  bool returnsByRef() const { return ReturnsByRef; }
  bool isExtern() const { return IsExtern; }
  void markAsExtern() { IsExtern = true; }
  void prependImplicitParameter(Parameter Param);

  const std::string &getMangledName() const;

  llvm::Function *codegen();
};

/// FunctionAST - Represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<BlockStmtAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<BlockStmtAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}
  
  llvm::Function *codegen();
  
  PrototypeAST *getProto() const { return Proto.get(); }
  BlockStmtAST *getBody() const { return Body.get(); }
};

/// FieldAST - Represents a field in a struct.
class FieldAST {
  std::string Type;
  std::string Name;
  MemberModifiers Modifiers;
  std::unique_ptr<ExprAST> Initializer;

public:
  FieldAST(std::string Type,
           std::string Name,
           MemberModifiers Modifiers,
           std::unique_ptr<ExprAST> Initializer = nullptr)
      : Type(std::move(Type)),
        Name(std::move(Name)),
        Modifiers(Modifiers),
        Initializer(std::move(Initializer)) {}
  
  const std::string &getType() const { return Type; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  const MemberModifiers &getModifiers() const { return Modifiers; }
  bool hasInitializer() const { return static_cast<bool>(Initializer); }
  ExprAST *getInitializer() const { return Initializer.get(); }
  std::unique_ptr<ExprAST> takeInitializer() { return std::move(Initializer); }
};

/// MemberAccessExprAST - Expression class for struct member access (e.g. point.x).
class MemberAccessExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Object;
  std::string MemberName;
  std::string GenericArguments;

public:
  MemberAccessExprAST(std::unique_ptr<ExprAST> Object, std::string MemberName,
                      std::string GenericArguments = {})
      : Object(std::move(Object)),
        MemberName(std::move(MemberName)),
        GenericArguments(std::move(GenericArguments)) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  ExprAST *getObject() const { return Object.get(); }
  const std::string &getMemberName() const { return MemberName; }
  bool hasExplicitGenerics() const { return !GenericArguments.empty(); }
  const std::string &getGenericArguments() const { return GenericArguments; }
};

/// NullSafeAccessExprAST - Expression class for null-safe member access (e.g. obj?.field).
class NullSafeAccessExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Object;
  std::string MemberName;

public:
  NullSafeAccessExprAST(std::unique_ptr<ExprAST> Object, std::string MemberName)
      : Object(std::move(Object)), MemberName(std::move(MemberName)) {}

  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  ExprAST *getObject() const { return Object.get(); }
  const std::string &getMemberName() const { return MemberName; }
};

/// ThisExprAST - Expression class for 'this' keyword in struct methods.
class ThisExprAST : public ExprAST {
public:
  ThisExprAST() {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
};

/// BaseExprAST - Expression class for 'base' keyword in class methods.
class BaseExprAST : public ExprAST {
public:
  BaseExprAST() = default;

  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;

private:
  mutable bool ReportedError = false;
};

enum class MethodKind : uint8_t {
  Constructor,
  Regular,
  ThisOverride
};

struct MethodDefinition {
  std::unique_ptr<FunctionAST> Function;
  std::unique_ptr<PrototypeAST> Prototype;
  MemberModifiers Modifiers;
  MethodKind Kind = MethodKind::Regular;
  std::string DisplayName;
  bool HasImplicitThis = false;
  PrototypeAST *PrototypeView = nullptr;

  MethodDefinition(std::unique_ptr<FunctionAST> Function,
                   MemberModifiers Modifiers,
                   MethodKind Kind = MethodKind::Regular,
                   std::string DisplayName = {})
      : Function(std::move(Function)), Modifiers(Modifiers), Kind(Kind),
        DisplayName(std::move(DisplayName)) {}

  MethodDefinition(std::unique_ptr<PrototypeAST> Prototype,
                   MemberModifiers Modifiers,
                   MethodKind Kind,
                   std::string DisplayName)
      : Prototype(std::move(Prototype)), Modifiers(Modifiers), Kind(Kind),
        DisplayName(std::move(DisplayName)) {}

  FunctionAST *get() const { return Function.get(); }
  std::unique_ptr<FunctionAST> takeFunction() { return std::move(Function); }
  PrototypeAST *getPrototype() const {
    if (Function)
      return Function->getProto();
    if (Prototype)
      return Prototype.get();
    return PrototypeView;
  }
  const MemberModifiers &getModifiers() const { return Modifiers; }
  MethodKind getKind() const { return Kind; }
  const std::string &getDisplayName() const { return DisplayName; }
  void markImplicitThisInjected() { HasImplicitThis = true; }
  bool hasImplicitThis() const { return HasImplicitThis; }
  bool hasBody() const { return static_cast<bool>(Function); }
  void setPrototypeView(PrototypeAST *proto) { PrototypeView = proto; }
  bool isStatic() const {
    return static_cast<uint8_t>(Modifiers.storage & StorageFlag::Static) != 0;
  }
  bool needsInstanceThis() const {
    return !isStatic() && Kind != MethodKind::Constructor;
  }
};

/// StructAST - Represents a struct definition.
class StructAST {
  AggregateKind Kind = AggregateKind::Struct;
  std::string Name;
  std::vector<std::unique_ptr<FieldAST>> Fields;
  std::vector<MethodDefinition> Methods;
  std::vector<std::string> BaseTypes;
  std::vector<std::string> GenericParameters;
  std::optional<std::string> BaseClass;
  std::vector<std::string> InterfaceTypes;
  std::vector<TypeInfo> BaseTypeInfos;
  std::optional<TypeInfo> BaseClassInfo;
  std::vector<TypeInfo> InterfaceTypeInfos;
  bool IsAbstract = false;
  mutable bool LayoutUsageComputed = false;
  mutable std::vector<bool> LayoutParameterUsage;

public:
  StructAST(AggregateKind Kind,
            std::string Name,
            std::vector<std::unique_ptr<FieldAST>> Fields,
            std::vector<MethodDefinition> Methods,
            std::vector<std::string> BaseTypes = {},
            std::vector<std::string> GenericParameters = {})
      : Kind(Kind), Name(std::move(Name)), Fields(std::move(Fields)),
        Methods(std::move(Methods)), BaseTypes(std::move(BaseTypes)),
        GenericParameters(std::move(GenericParameters)) {}
  
  llvm::Type *codegen();
  
  AggregateKind getKind() const { return Kind; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  const std::vector<std::unique_ptr<FieldAST>> &getFields() const { return Fields; }
  const std::vector<MethodDefinition> &getMethods() const { return Methods; }
  std::vector<MethodDefinition> &getMethods() { return Methods; }
  const std::vector<std::string> &getBaseTypes() const { return BaseTypes; }
  const std::vector<std::string> &getGenericParameters() const { return GenericParameters; }
  const std::vector<TypeInfo> &getBaseTypeInfos() const { return BaseTypeInfos; }
  const std::optional<std::string> &getBaseClass() const { return BaseClass; }
  const std::optional<TypeInfo> &getBaseClassInfo() const { return BaseClassInfo; }
  const std::vector<std::string> &getInterfaces() const { return InterfaceTypes; }
  const std::vector<TypeInfo> &getInterfaceTypeInfos() const { return InterfaceTypeInfos; }
  bool isAbstract() const { return IsAbstract; }
  bool isInterface() const { return Kind == AggregateKind::Interface; }
  bool isGenericTemplate() const { return !GenericParameters.empty(); }
  const std::vector<bool> &layoutParameterUsage() const;

  void setBaseClass(std::optional<std::string> Base) { BaseClass = std::move(Base); }
  void setBaseClassInfo(std::optional<TypeInfo> Info) { BaseClassInfo = std::move(Info); }
  void setInterfaces(std::vector<std::string> Interfaces) { InterfaceTypes = std::move(Interfaces); }
  void setInterfaceTypeInfos(std::vector<TypeInfo> Infos) { InterfaceTypeInfos = std::move(Infos); }
  void setAbstract(bool Abstract) { IsAbstract = Abstract; }
  void appendBaseTypes(std::vector<std::string> bases) { BaseTypes = std::move(bases); }
  void setBaseTypeInfos(std::vector<TypeInfo> Infos) { BaseTypeInfos = std::move(Infos); }
  void setGenericParameters(std::vector<std::string> Generics) { GenericParameters = std::move(Generics); }
};

// Initialize LLVM module
void InitializeModule();

// Get the LLVM module for printing
llvm::Module *getModule();

void RegisterGenericTemplate(std::unique_ptr<StructAST> templ);
StructAST *FindGenericTemplate(const std::string &name);
void RegisterGenericFunctionTemplate(std::unique_ptr<FunctionAST> templ);
FunctionAST *FindGenericFunctionTemplate(const std::string &name,
                                         std::size_t genericArity);
llvm::Function *InstantiateGenericFunction(
    const std::string &name, const std::vector<TypeInfo> &typeArguments,
    const std::map<std::string, TypeInfo> *additionalBindings = nullptr);

// Check if currently in an unsafe context
bool isInUnsafeContext();

// Set/clear unsafe context
void setUnsafeContext(bool unsafe);

#endif // AST_H
