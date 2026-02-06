#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include <map>
#include <memory>

#include "compiler_session.h"

// Helper functions
int getNextToken();
int GetTokPrecedence();
bool isInUnsafeContext();
void enterUnsafeContext();
void exitUnsafeContext();

// Error handling
std::unique_ptr<ExprAST> LogError(const char *Str, std::string_view hint = {});
std::unique_ptr<ExprAST> LogError(const std::string &Str, std::string_view hint = {});
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str, std::string_view hint = {});
std::unique_ptr<StmtAST> LogErrorS(const char *Str, std::string_view hint = {});

// Parsing functions
std::unique_ptr<ExprAST> ParseNumberExpr();
std::unique_ptr<ExprAST> ParseParenExpr();
std::unique_ptr<ExprAST> ParseIdentifierExpr();
std::unique_ptr<ExprAST> ParseArrayExpr();
std::unique_ptr<ExprAST> ParseArrayIndex(std::unique_ptr<ExprAST> Array);
std::unique_ptr<ExprAST> ParseUnaryExpr();
std::unique_ptr<ExprAST> ParsePrimary();
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);
std::unique_ptr<ExprAST> ParseExpression();
std::unique_ptr<PrototypeAST> ParsePrototype(bool isUnsafe = false);
std::unique_ptr<FunctionAST> ParseDefinition();
std::unique_ptr<PrototypeAST> ParseExtern();
std::unique_ptr<DelegateDeclAST> ParseDelegateDefinition();

// Statement parsing functions
std::unique_ptr<StmtAST> ParseStatement();
std::unique_ptr<ReturnStmtAST> ParseReturnStatement();
std::unique_ptr<BlockStmtAST> ParseBlock();
std::unique_ptr<VariableDeclarationStmtAST> ParseVariableDeclaration(bool isRef = false);
std::unique_ptr<ForEachStmtAST> ParseForEachStatement();
std::unique_ptr<StmtAST> ParseForStatement();
std::unique_ptr<ForLoopStmtAST> ParseForLoopStatement();
std::unique_ptr<UseStmtAST> ParseUseStatement();
std::unique_ptr<IfStmtAST> ParseIfStatement();
std::unique_ptr<WhileStmtAST> ParseWhileStatement();
std::unique_ptr<BreakStmtAST> ParseBreakStatement();
std::unique_ptr<SkipStmtAST> ParseSkipStatement();
std::unique_ptr<AssertStmtAST> ParseAssertStatement();
std::unique_ptr<UnsafeBlockStmtAST> ParseUnsafeBlock();
std::unique_ptr<SwitchStmtAST> ParseSwitchStatement();
std::unique_ptr<SwitchExprAST> ParseSwitchExpression();
std::unique_ptr<TernaryExprAST> ParseTernaryExpression(std::unique_ptr<ExprAST> ThenExpr);
std::unique_ptr<CaseAST> ParseCase(bool isExpression);

// Top-level parsing that can handle both variable declarations and function definitions
bool ParseTypeIdentifier(bool isRef = false);

// Struct parsing functions
std::unique_ptr<StructAST> ParseStructDefinition(AggregateKind kind = AggregateKind::Struct,
                                                 bool isAbstract = false);
std::unique_ptr<FieldAST> ParseField();

// Constant expression evaluation
struct ConstantValue {
  enum Type { INTEGER, UNSIGNED_INTEGER, FLOAT, BOOLEAN } type;
  union {
    long long intVal;
    unsigned long long uintVal;
    double floatVal;
    bool boolVal;
  };

  ConstantValue(long long val) : type(INTEGER), intVal(val) {}
  ConstantValue(unsigned long long val) : type(UNSIGNED_INTEGER), uintVal(val) {}
  ConstantValue(double val) : type(FLOAT), floatVal(val) {}
  ConstantValue(bool val) : type(BOOLEAN), boolVal(val) {}
};

bool EvaluateConstantExpression(const ExprAST* expr, ConstantValue& result,
                                SourceLocation *firstErrorLocation = nullptr);

#endif // PARSER_H
