#ifndef HYBRID_PARSER_INTERNAL_H
#define HYBRID_PARSER_INTERNAL_H

#include "compiler_session.h"
#include "lexer.h"
#include "parser.h"

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

template <typename T>
std::unique_ptr<T> withLocation(std::unique_ptr<T> expr, SourceLocation loc) {
  if (expr)
    expr->setSourceLocation(loc);
  return expr;
}

#endif // HYBRID_PARSER_INTERNAL_H
