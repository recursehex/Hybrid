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

struct SourceLocation {
  std::size_t line = 0;
  std::size_t column = 0;

  constexpr bool isValid() const noexcept { return line != 0; }
};

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

  struct PendingChar {
    int ch = EOF;
    SourceLocation location{};
  };

  std::vector<PendingChar> pushbackBuffer;
  std::size_t nextLine = 1;
  std::size_t nextColumn = 1;
  std::size_t lastCharLine = 0;
  std::size_t lastCharColumn = 0;
  std::size_t tokenLine = 1;
  std::size_t tokenColumn = 1;
  SourceLocation interpolatedSegmentLocation{};
  SourceLocation interpolatedExprStartLocation{};
  SourceLocation interpolatedStringEndLocation{};

  void reset();
  void setInputBuffer(std::string_view contents);
  int consumeChar();
  void unconsumeChar(int ch);
  void setTokenStart(SourceLocation loc);
  SourceLocation tokenStart() const;
  SourceLocation lastCharLocation() const;
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
  SourceLocation currentTokenLocation{};
  SourceLocation previousTokenLocation{};

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

std::string describeTokenForDiagnostics(int token);
void reportCompilerError(const std::string &message, std::string_view hint = {});

#endif // HYBRID_COMPILER_SESSION_H
