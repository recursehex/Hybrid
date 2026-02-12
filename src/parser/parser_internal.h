#ifndef HYBRID_PARSER_INTERNAL_H
#define HYBRID_PARSER_INTERNAL_H

#include "compiler_session.h"
#include "lexer.h"
#include "parser.h"
#include <string>
#include <string_view>
#include <vector>

#define CurTok (currentParser().curTok)
#define BinopPrecedence (currentParser().binopPrecedence)
#define StructNames (currentParser().structNames)
#define ClassNames (currentParser().classNames)
#define DelegateNames (currentParser().delegateNames)
#define StructDefinitionStack (currentParser().structDefinitionStack)
#define ClassDefinitionStack (currentParser().classDefinitionStack)
#define LoopNestingDepth (currentParser().loopNestingDepth)
#define UnsafeContextLevel (currentParser().unsafeContextLevel)

#define IdentifierStr (currentLexer().identifierStr)
#define LexedNumericLiteral (currentLexer().numericLiteral)
#define StringVal (currentLexer().stringLiteral)
#define CharVal (currentLexer().charLiteral)

class GenericParameterScope {
public:
  explicit GenericParameterScope(const std::vector<std::string> &params)
      : active(!params.empty()) {
    if (active)
      currentParser().pushGenericParameters(params);
  }

  GenericParameterScope(const GenericParameterScope &) = delete;
  GenericParameterScope &operator=(const GenericParameterScope &) = delete;

  ~GenericParameterScope() {
    if (active)
      currentParser().popGenericParameters();
  }

private:
  bool active = false;
};

class TemplateAngleScope {
public:
  TemplateAngleScope() : active(true) { currentParser().templateAngleDepth++; }

  TemplateAngleScope(const TemplateAngleScope &) = delete;
  TemplateAngleScope &operator=(const TemplateAngleScope &) = delete;

  ~TemplateAngleScope() {
    if (active)
      currentParser().templateAngleDepth--;
  }

private:
  bool active = true;
};

class ScopedTypeCheckContext {
  ParserContext &parser;
  bool previous = false;

public:
  explicit ScopedTypeCheckContext(bool enable)
      : parser(currentParser()), previous(parser.allowTypeCheck) {
    parser.allowTypeCheck = enable;
  }

  ~ScopedTypeCheckContext() { parser.allowTypeCheck = previous; }
};

template <typename T>
std::unique_ptr<T> withLocation(std::unique_ptr<T> expr, SourceLocation loc) {
  if (expr)
    expr->setSourceLocation(loc);
  return expr;
}

class TokenReplayScope {
public:
  explicit TokenReplayScope(bool enabled);

  TokenReplayScope(const TokenReplayScope &) = delete;
  TokenReplayScope &operator=(const TokenReplayScope &) = delete;

  ~TokenReplayScope();

  void rollback();
  void commit();

private:
  bool active = false;
  int originalToken = tok_eof;
  SourceLocation originalLocation{};
  int originalPreviousToken = 0;
  SourceLocation originalPreviousLocation{};
  std::string originalIdentifier;
  std::string originalStringLiteral;
  NumericLiteral originalNumericLiteral;
  char originalCharLiteral = 0;
  std::vector<ParserContext::PendingToken> capturedTokens;
};

void SkipNewlines();
bool IsBuiltInType();
bool IsValidType();
bool IsActiveStructName(const std::string &name);
bool IsActiveClassName(const std::string &name);
bool AppendTypeSuffix(std::string &typeName, bool &pointerSeen,
                      bool allowOperatorDisambiguation = false);
bool ParseOptionalGenericArgumentList(std::string &typeSpelling,
                                      bool allowDisambiguation = false);
bool ParseGenericParameterList(std::vector<std::string> &parameters);
bool ParseCompleteTypeInfo(TypeInfo &outInfo, bool declaredRef = false,
                           bool allowTypeArgDisambiguation = false);
TypeInfo buildDeclaredTypeInfo(const std::string &typeName, bool declaredRef);
void maybeWarnGenericArity(const std::vector<std::string> &params,
                           const std::string &ownerName,
                           std::string_view context);
bool ValidateParameterDefaults(const std::vector<Parameter> &args);
bool ConvertParenInitializerToConstructor(std::unique_ptr<ExprAST> &initializer,
                                          const TypeInfo &declInfo);
bool ParseArgumentList(std::vector<std::unique_ptr<ExprAST>> &args,
                       std::vector<std::string> &argNames,
                       std::vector<SourceLocation> &argNameLocations,
                       std::vector<SourceLocation> &argEqualsLocations);
std::unique_ptr<ExprAST> ParsePrimaryWithPostfix();
std::unique_ptr<ExprAST> ParseConditionExpression();
void RecoverAfterExpressionError();

#endif // HYBRID_PARSER_INTERNAL_H
