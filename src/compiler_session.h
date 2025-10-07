#ifndef HYBRID_COMPILER_SESSION_H
#define HYBRID_COMPILER_SESSION_H

#include <cstddef>
#include <cstdint>
#include <memory>
#include <set>
#include <stack>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
#include <map>

#include "numeric_literal.h"
#include "codegen_context.h"

/// LexerContext holds all mutable lexer state for a compilation unit.
struct LexerContext {
  std::string identifierStr;
  NumericLiteral numericLiteral;
  std::string stringLiteral;
  uint32_t charLiteral = 0;
  int lastChar = ' ';

  bool inInterpolatedString = false;
  bool inInterpolatedExpression = false;
  bool pendingInterpolatedExprStart = false;
  bool pendingInterpolatedStringEnd = false;
  bool pendingInterpolatedLiteralSegment = false;
  std::string currentInterpolatedLiteral;

  // Optional in-memory source buffer used when the caller wants to feed the
  // lexer from a pre-loaded string rather than stdin.
  std::string bufferedInput;
  std::size_t bufferedCursor = 0;
  bool useBufferedInput = false;

  void reset();
  void setInputBuffer(std::string_view contents);
  int consumeChar();
  void unconsumeChar(int ch);
};

/// ParserContext wraps the parser's per-run state, replacing the previous
/// collection of global variables.
struct ParserContext {
  int curTok = 0;
  std::map<std::string, int> binopPrecedence;
  std::set<std::string> structNames;
  int loopNestingDepth = 0;
  int unsafeContextLevel = 0;
  bool hadError = false;

  void reset(bool clearSymbols = true);
  void clearPrecedence();
};

/// CompilerSession groups lexer and parser context for a single compilation
/// unit. Code generation state will be associated here as well so that the
/// entire front-end is explicitly session-scoped.
class CompilerSession {
public:
  CompilerSession();

  LexerContext &lexer();
  ParserContext &parser();
  CodegenContext &codegen();

  void resetParser();
  void resetAll();
  void beginUnit(bool preserveSymbols);

private:
  LexerContext lexerState;
  ParserContext parserState;
  std::unique_ptr<CodegenContext> codegenState;
};

/// Session stack management -------------------------------------------------

void pushCompilerSession(CompilerSession &session);
void popCompilerSession();
CompilerSession &currentCompilerSession();
bool hasCompilerSession();

LexerContext &currentLexer();
ParserContext &currentParser();
CodegenContext &currentCodegen();

#endif // HYBRID_COMPILER_SESSION_H
