#ifndef AST_H
#define AST_H

#include <memory>
#include <string>
#include <vector>

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
};

/// StmtAST - Base class for all statement nodes.
class StmtAST {
public:
  virtual ~StmtAST() = default;
};

/// ReturnStmtAST - Statement class for return statements.
class ReturnStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> ReturnValue;

public:
  ReturnStmtAST(std::unique_ptr<ExprAST> ReturnValue)
      : ReturnValue(std::move(ReturnValue)) {}
};

/// BlockStmtAST - Statement class for statement blocks.
class BlockStmtAST : public StmtAST {
  std::vector<std::unique_ptr<StmtAST>> Statements;

public:
  BlockStmtAST(std::vector<std::unique_ptr<StmtAST>> Statements)
      : Statements(std::move(Statements)) {}
};

/// VariableDeclarationStmtAST - Statement class for variable declarations.
class VariableDeclarationStmtAST : public StmtAST {
  std::string Type;
  std::string Name;
  std::unique_ptr<ExprAST> Initializer;

public:
  VariableDeclarationStmtAST(const std::string &Type, const std::string &Name,
                             std::unique_ptr<ExprAST> Initializer)
      : Type(Type), Name(Name), Initializer(std::move(Initializer)) {}
};

/// ExpressionStmtAST - Statement class for expression statements.
class ExpressionStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> Expression;

public:
  ExpressionStmtAST(std::unique_ptr<ExprAST> Expression)
      : Expression(std::move(Expression)) {}
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
};

/// UseStmtAST - Statement class for use (import) statements.
class UseStmtAST : public StmtAST {
  std::string Module;

public:
  UseStmtAST(const std::string &Module) : Module(Module) {}
  
  const std::string &getModule() const { return Module; }
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
};

/// Parameter - Represents a function parameter with type and name
struct Parameter {
  std::string Type;
  std::string Name;
  
  Parameter(const std::string &Type, const std::string &Name) 
      : Type(Type), Name(Name) {}
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names and types.
class PrototypeAST {
  std::string ReturnType;
  std::string Name;
  std::vector<Parameter> Args;

public:
  PrototypeAST(const std::string &ReturnType, const std::string &Name, 
               std::vector<Parameter> Args)
      : ReturnType(ReturnType), Name(Name), Args(std::move(Args)) {}

  const std::string &getReturnType() const { return ReturnType; }
  const std::string &getName() const { return Name; }
  const std::vector<Parameter> &getArgs() const { return Args; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<BlockStmtAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<BlockStmtAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

#endif // AST_H