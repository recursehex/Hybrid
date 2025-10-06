#include "compiler_session.h"

#include <cstdio>
#include <utility>
#include <vector>

#include "codegen_context.h"

namespace {
thread_local std::vector<CompilerSession *> SessionStack;
}

// LexerContext ----------------------------------------------------------------

void LexerContext::reset() {
  identifierStr.clear();
  numericLiteral = NumericLiteral();
  stringLiteral.clear();
  charLiteral = 0;
  lastChar = ' ';
  bufferedInput.clear();
  bufferedCursor = 0;
  useBufferedInput = false;
}

void LexerContext::setInputBuffer(std::string_view contents) {
  bufferedInput.assign(contents.begin(), contents.end());
  bufferedCursor = 0;
  useBufferedInput = true;
}

int LexerContext::consumeChar() {
  if (useBufferedInput) {
    if (bufferedCursor >= bufferedInput.size())
      return EOF;
    return static_cast<unsigned char>(bufferedInput[bufferedCursor++]);
  }
  return std::getchar();
}

void LexerContext::unconsumeChar(int ch) {
  if (ch == EOF)
    return;

  if (useBufferedInput) {
    if (bufferedCursor == 0) {
      bufferedInput.insert(bufferedInput.begin(), static_cast<char>(ch));
    } else {
      bufferedInput[--bufferedCursor] = static_cast<char>(ch);
    }
    return;
  }

  std::ungetc(ch, stdin);
}

// ParserContext ----------------------------------------------------------------

void ParserContext::reset() {
  curTok = 0;
  binopPrecedence.clear();
  structNames.clear();
  loopNestingDepth = 0;
  unsafeContextLevel = 0;
}

// CompilerSession --------------------------------------------------------------

CompilerSession::CompilerSession() = default;

LexerContext &CompilerSession::lexer() { return lexerState; }
ParserContext &CompilerSession::parser() { return parserState; }
CodegenContext &CompilerSession::codegen() {
  if (!codegenState)
    codegenState = std::make_unique<CodegenContext>();
  return *codegenState;
}

void CompilerSession::resetParser() { parserState.reset(); }

void CompilerSession::resetAll() {
  lexerState.reset();
  parserState.reset();
  if (codegenState)
    codegenState->reset();
}

// Session stack helpers --------------------------------------------------------

void pushCompilerSession(CompilerSession &session) {
  SessionStack.push_back(&session);
}

void popCompilerSession() {
  if (SessionStack.empty())
    throw std::runtime_error("No active compiler session to pop");
  SessionStack.pop_back();
}

CompilerSession &currentCompilerSession() {
  if (SessionStack.empty())
    throw std::runtime_error("No active compiler session");
  return *SessionStack.back();
}

bool hasCompilerSession() { return !SessionStack.empty(); }

LexerContext &currentLexer() {
  return currentCompilerSession().lexer();
}

ParserContext &currentParser() {
  return currentCompilerSession().parser();
}

CodegenContext &currentCodegen() {
  return currentCompilerSession().codegen();
}
