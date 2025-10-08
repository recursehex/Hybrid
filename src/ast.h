#ifndef AST_H
#define AST_H

#include <memory>
#include <span>
#include <string>
#include <vector>
#include <cstdint>
#include <optional>
#include <utility>
#include "concepts.h"
#include "numeric_literal.h"

enum class RefStorageClass {
  None,
  RefValue,
  RefAlias
};

struct TypeInfo {
  std::string typeName;           // canonical language-visible type
  unsigned pointerDepth = 0;      // number of explicit pointer levels (@)
  bool isArray = false;           // whether type is an array ("[]")
  bool isNullable = false;        // whether the type itself can be null
  bool elementNullable = false;   // whether array elements can be null
  RefStorageClass refStorage = RefStorageClass::None;
  bool isMutable = true;
  bool declaredRef = false;       // whether declared via `ref`

  bool isReference() const { return refStorage != RefStorageClass::None; }
  bool ownsStorage() const { return refStorage == RefStorageClass::RefValue; }
  bool isAlias() const { return refStorage == RefStorageClass::RefAlias; }
};

// LLVM forward declarations
namespace llvm {
  class Value;
  class Function;
}

#include "llvm/IR/Value.h"

/// ExprAST - Base class for all expression nodes.
class ExprAST {
protected:
  mutable std::string TypeName; // The type name of this expression (e.g., "int", "byte", "float")
  
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
  std::string Type;
  std::string VarName;
  std::unique_ptr<ExprAST> Collection;
  std::unique_ptr<BlockStmtAST> Body;

public:
  ForEachStmtAST(const std::string &Type, const std::string &VarName,
                 std::unique_ptr<ExprAST> Collection,
                 std::unique_ptr<BlockStmtAST> Body)
      : Type(Type), VarName(VarName), Collection(std::move(Collection)),
        Body(std::move(Body)) {}
  
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
  [[nodiscard]] const std::string &getElementType() const { return ElementType; }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>> &getElements() const { return Elements; }
  [[nodiscard]] std::span<const std::unique_ptr<ExprAST>> getElementsSpan() const { return Elements; }
};

/// ArrayIndexExprAST - Expression class for array indexing like arr[0].
class ArrayIndexExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Array;
  std::unique_ptr<ExprAST> Index;

public:
  ArrayIndexExprAST(std::unique_ptr<ExprAST> Array,
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

/// RefExprAST - Expression class for ref argument expressions (e.g., ref x in function calls).
class RefExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Operand;

public:
  RefExprAST(std::unique_ptr<ExprAST> Operand)
      : Operand(std::move(Operand)) {}

  llvm::Value *codegen() override;
  ExprAST *getOperand() const { return Operand.get(); }
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}

  llvm::Value *codegen() override;
  [[nodiscard]] const std::string &getCallee() const { return Callee; }
  [[nodiscard]] const std::vector<std::unique_ptr<ExprAST>> &getArgs() const { return Args; }
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
  bool IsUnsafe;
  bool ReturnsByRef;

public:
  PrototypeAST(TypeInfo ReturnTypeInfo, const std::string &Name,
               std::vector<Parameter> Args, bool IsUnsafe = false, bool ReturnsByRef = false)
      : ReturnTypeInfo(std::move(ReturnTypeInfo)), Name(Name), Args(std::move(Args)), IsUnsafe(IsUnsafe), ReturnsByRef(ReturnsByRef) {}

  const std::string &getReturnType() const { return ReturnTypeInfo.typeName; }
  const TypeInfo &getReturnTypeInfo() const { return ReturnTypeInfo; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  const std::vector<Parameter> &getArgs() const { return Args; }
  bool isUnsafe() const { return IsUnsafe; }
  bool returnsByRef() const { return ReturnsByRef; }

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

public:
  FieldAST(const std::string &Type, const std::string &Name)
      : Type(Type), Name(Name) {}
  
  const std::string &getType() const { return Type; }
  [[nodiscard]] const std::string &getName() const { return Name; }
};

/// MemberAccessExprAST - Expression class for struct member access (e.g., point.x).
class MemberAccessExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Object;
  std::string MemberName;

public:
  MemberAccessExprAST(std::unique_ptr<ExprAST> Object, const std::string &MemberName)
      : Object(std::move(Object)), MemberName(MemberName) {}
  
  llvm::Value *codegen() override;
  llvm::Value *codegen_ptr() override;
  ExprAST *getObject() const { return Object.get(); }
  const std::string &getMemberName() const { return MemberName; }
};

/// NullSafeAccessExprAST - Expression class for null-safe member access (e.g., obj?.field).
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

/// StructAST - Represents a struct definition.
class StructAST {
  std::string Name;
  std::vector<std::unique_ptr<FieldAST>> Fields;
  std::vector<std::unique_ptr<FunctionAST>> Methods;

public:
  StructAST(const std::string &Name,
            std::vector<std::unique_ptr<FieldAST>> Fields,
            std::vector<std::unique_ptr<FunctionAST>> Methods)
      : Name(Name), Fields(std::move(Fields)), Methods(std::move(Methods)) {}
  
  llvm::Type *codegen();
  
  [[nodiscard]] const std::string &getName() const { return Name; }
  const std::vector<std::unique_ptr<FieldAST>> &getFields() const { return Fields; }
  const std::vector<std::unique_ptr<FunctionAST>> &getMethods() const { return Methods; }
};

// Initialize LLVM module
void InitializeModule();

// Get the LLVM module for printing
llvm::Module *getModule();

// Check if currently in an unsafe context
bool isInUnsafeContext();

// Set/clear unsafe context
void setUnsafeContext(bool unsafe);

#endif // AST_H
