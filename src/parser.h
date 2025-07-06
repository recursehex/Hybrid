#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include <map>
#include <memory>

// Parser variables
extern int CurTok;
extern std::map<std::string, int> BinopPrecedence;

// Helper functions
int getNextToken();
int GetTokPrecedence();

// Error handling
std::unique_ptr<ExprAST> LogError(const char *Str);
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str);
std::unique_ptr<StmtAST> LogErrorS(const char *Str);

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
std::unique_ptr<PrototypeAST> ParsePrototype();
std::unique_ptr<FunctionAST> ParseDefinition();
std::unique_ptr<FunctionAST> ParseTopLevelExpr();
std::unique_ptr<PrototypeAST> ParseExtern();

// Statement parsing functions
std::unique_ptr<StmtAST> ParseStatement();
std::unique_ptr<ReturnStmtAST> ParseReturnStatement();
std::unique_ptr<BlockStmtAST> ParseBlock();
std::unique_ptr<VariableDeclarationStmtAST> ParseVariableDeclaration();
std::unique_ptr<ForEachStmtAST> ParseForEachStatement();
std::unique_ptr<UseStmtAST> ParseUseStatement();
std::unique_ptr<IfStmtAST> ParseIfStatement();
std::unique_ptr<WhileStmtAST> ParseWhileStatement();
std::unique_ptr<BreakStmtAST> ParseBreakStatement();
std::unique_ptr<SkipStmtAST> ParseSkipStatement();

// Top-level parsing that can handle both variable declarations and function definitions
void ParseTypeIdentifier();

#endif // PARSER_H