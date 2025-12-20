// This file implements compiler session management, wiring per-thread lexer, parser, and codegen state.

#include "compiler_session.h"

#include <cctype>
#include <cstdio>
#include <sstream>
#include <utility>
#include <vector>

#include "analysis/semantics.h"
#include "codegen_context.h"
#include "lexer.h"
#include "parser.h"

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
  pushbackBuffer.clear();
  nextLine = 1;
  nextColumn = 1;
  lastCharLine = 1;
  lastCharColumn = 0;
  tokenLine = 1;
  tokenColumn = 1;
  inInterpolatedString = false;
  inInterpolatedExpression = false;
  pendingInterpolatedExprStart = false;
  pendingInterpolatedStringEnd = false;
  pendingInterpolatedLiteralSegment = false;
  currentInterpolatedLiteral.clear();
  interpolatedSegmentLocation = {};
  interpolatedExprStartLocation = {};
  interpolatedStringEndLocation = {};
  bufferedInput.clear();
  bufferedCursor = 0;
  useBufferedInput = false;
}

void LexerContext::setInputBuffer(std::string_view contents) {
  bufferedInput.assign(contents.begin(), contents.end());
  bufferedCursor = 0;
  useBufferedInput = true;
  pushbackBuffer.clear();
  nextLine = 1;
  nextColumn = 1;
  lastCharLine = 1;
  lastCharColumn = 0;
  tokenLine = 1;
  tokenColumn = 1;
  interpolatedSegmentLocation = {};
  interpolatedExprStartLocation = {};
  interpolatedStringEndLocation = {};
}

int LexerContext::consumeChar() {
  PendingChar data;
  if (!pushbackBuffer.empty()) {
    data = pushbackBuffer.back();
    pushbackBuffer.pop_back();
  } else {
    int ch;
    if (useBufferedInput) {
      if (bufferedCursor >= bufferedInput.size())
        ch = EOF;
      else
        ch = static_cast<unsigned char>(bufferedInput[bufferedCursor++]);
    } else {
      ch = std::getchar();
    }
    data.ch = ch;
    data.location = {nextLine, nextColumn};
  }

  lastCharLine = data.location.line == 0 ? lastCharLine : data.location.line;
  lastCharColumn = data.location.column == 0 ? lastCharColumn : data.location.column;

  if (data.ch == '\n' || data.ch == '\r') {
    nextLine = (data.location.line == 0 ? nextLine : data.location.line) + 1;
    nextColumn = 1;
  } else if (data.ch != EOF) {
    nextLine = data.location.line;
    nextColumn = data.location.column + 1;
  }

  return data.ch;
}

void LexerContext::unconsumeChar(int ch) {
  if (ch == EOF)
    return;

  pushbackBuffer.push_back(PendingChar{ch, {lastCharLine, lastCharColumn}});
  nextLine = lastCharLine;
  nextColumn = lastCharColumn;
}

void LexerContext::setTokenStart(SourceLocation loc) {
  if (loc.isValid()) {
    tokenLine = loc.line;
    tokenColumn = loc.column;
  } else {
    tokenLine = lastCharLine == 0 ? tokenLine : lastCharLine;
    tokenColumn = lastCharColumn == 0 ? tokenColumn : lastCharColumn;
  }
}

SourceLocation LexerContext::tokenStart() const {
  return {tokenLine, tokenColumn};
}

SourceLocation LexerContext::lastCharLocation() const {
  return {lastCharLine, lastCharColumn};
}

static SourceLocation bestErrorLocation() {
  ParserContext &parser = currentParser();
  SourceLocation lexerLoc = currentLexer().tokenStart();
  if (lexerLoc.isValid())
    return lexerLoc;
  if (parser.currentTokenLocation.isValid())
    return parser.currentTokenLocation;
  if (parser.previousTokenLocation.isValid())
    return parser.previousTokenLocation;
  return {};
}

std::string describeTokenForDiagnostics(int token) {
  const LexerContext &lex = currentLexer();

  auto makeKeyword = [](const char *word) {
    return std::string("keyword '") + word + "'";
  };

  auto makeOperator = [](std::string op) {
    return std::string("operator '") + op + "'";
  };

  switch (token) {
    case tok_eof:
      return "end of file";
    case tok_newline:
      return "newline";
    case tok_identifier:
      if (!lex.identifierStr.empty())
        return "identifier '" + lex.identifierStr + "'";
      return "identifier";
    case tok_tilde_identifier:
      if (!lex.identifierStr.empty())
        return "destructor name '~" + lex.identifierStr + "'";
      return "destructor name";
    case tok_number:
      return "numeric literal '" + lex.numericLiteral.getSpelling() + "'";
    case tok_string_literal:
      return "string literal";
    case tok_char_literal:
      return "character literal";
    case tok_true:
      return makeKeyword("true");
    case tok_false:
      return makeKeyword("false");
    case tok_null:
      return makeKeyword("null");
    case tok_use:
      return makeKeyword("use");
    case tok_extern:
      return makeKeyword("extern");
    case tok_return:
      return makeKeyword("return");
    case tok_for:
      return makeKeyword("for");
    case tok_in:
      return makeKeyword("in");
    case tok_to:
      return makeKeyword("to");
    case tok_by:
      return makeKeyword("by");
    case tok_if:
      return makeKeyword("if");
    case tok_else:
      return makeKeyword("else");
    case tok_while:
      return makeKeyword("while");
    case tok_break:
      return makeKeyword("break");
    case tok_skip:
      return makeKeyword("skip");
    case tok_struct:
      return makeKeyword("struct");
    case tok_class:
      return makeKeyword("class");
    case tok_interface:
      return makeKeyword("interface");
    case tok_this:
      return makeKeyword("this");
    case tok_base:
      return makeKeyword("base");
    case tok_switch:
      return makeKeyword("switch");
    case tok_case:
      return makeKeyword("case");
    case tok_default:
      return makeKeyword("default");
    case tok_assert:
      return makeKeyword("assert");
    case tok_abstract:
      return makeKeyword("abstract");
    case tok_inherits:
      return makeKeyword("inherits");
    case tok_virtual:
      return makeKeyword("virtual");
    case tok_override:
      return makeKeyword("override");
    case tok_public:
      return makeKeyword("public");
    case tok_private:
      return makeKeyword("private");
    case tok_protected:
      return makeKeyword("protected");
    case tok_static:
      return makeKeyword("static");
    case tok_const:
      return makeKeyword("const");
    case tok_int:
      return makeKeyword("int");
    case tok_float:
      return makeKeyword("float");
    case tok_double:
      return makeKeyword("double");
    case tok_char:
      return makeKeyword("char");
    case tok_void:
      return makeKeyword("void");
    case tok_bool:
      return makeKeyword("bool");
    case tok_string:
      return makeKeyword("string");
    case tok_byte:
      return makeKeyword("byte");
    case tok_short:
      return makeKeyword("short");
    case tok_long:
      return makeKeyword("long");
    case tok_sbyte:
      return makeKeyword("sbyte");
    case tok_ushort:
      return makeKeyword("ushort");
    case tok_uint:
      return makeKeyword("uint");
    case tok_ulong:
      return makeKeyword("ulong");
    case tok_schar:
      return makeKeyword("schar");
    case tok_lchar:
      return makeKeyword("lchar");
    case tok_unsafe:
      return makeKeyword("unsafe");
    case tok_ref:
      return makeKeyword("ref");
    case tok_params:
      return makeKeyword("params");
    case tok_weak:
      return makeKeyword("weak");
    case tok_unique:
      return makeKeyword("unique");
    case tok_shared:
      return makeKeyword("shared");
    case tok_autoreleasepool:
      return makeKeyword("@autoreleasepool");
    case tok_new:
      return makeKeyword("new");
    case tok_free:
      return makeKeyword("free");
    case tok_lambda:
      return makeOperator("=>");
    case tok_eq:
      return makeOperator("==");
    case tok_ne:
      return makeOperator("!=");
    case tok_le:
      return makeOperator("<=");
    case tok_ge:
      return makeOperator(">=");
    case tok_lt:
      return makeOperator("<");
    case tok_gt:
      return makeOperator(">");
    case tok_and:
      return makeOperator("&&");
    case tok_or:
      return makeOperator("||");
    case tok_not:
      return makeOperator("!");
    case tok_plus_eq:
      return makeOperator("+=");
    case tok_minus_eq:
      return makeOperator("-=");
    case tok_mult_eq:
      return makeOperator("*=");
    case tok_div_eq:
      return makeOperator("/=");
    case tok_mod_eq:
      return makeOperator("%=");
    case tok_bitwise_and:
      return makeOperator("&");
    case tok_bitwise_or:
      return makeOperator("|");
    case tok_bitwise_xor:
      return makeOperator("^");
    case tok_left_shift:
      return makeOperator("<<");
    case tok_right_shift:
      return makeOperator(">>");
    case tok_and_eq:
      return makeOperator("&=");
    case tok_or_eq:
      return makeOperator("|=");
    case tok_xor_eq:
      return makeOperator("^=");
    case tok_left_shift_eq:
      return makeOperator("<<=");
    case tok_right_shift_eq:
      return makeOperator(">>=");
    case tok_inc:
      return makeOperator("++");
    case tok_dec:
      return makeOperator("--");
    case tok_colon:
      return "':'";
    case tok_dot:
      return "'.'";
    case tok_interpolated_string_start:
      return "start of interpolated string";
    case tok_interpolated_string_segment:
      return "interpolated string segment";
    case tok_interpolated_string_end:
      return "end of interpolated string";
    case tok_interpolated_expr_start:
      return "start of interpolated expression";
    case tok_interpolated_expr_end:
      return "end of interpolated expression";
    case tok_nullable:
      return makeOperator("?");
    case tok_null_safe_access:
      return makeOperator("?.");
    case tok_null_array_access:
      return makeOperator("?[");
    case tok_null_coalescing:
      return makeOperator(std::string("?") + "?");
    case tok_null_coalescing_assign:
      return makeOperator(std::string("?") + "?=");
    case tok_at:
      return "'@'";
    case tok_hash:
      return "'#'";
    case tok_arrow:
      return makeOperator("->");
  }

  if (token >= 0 && token < 128 && std::isprint(token)) {
    return std::string("symbol '") + static_cast<char>(token) + "'";
  }

  return "token #" + std::to_string(token);
}

void reportCompilerError(const std::string &message, std::string_view hint) {
  ParserContext &parser = currentParser();
  const SourceLocation loc = bestErrorLocation();
  std::ostringstream oss;
  oss << "Error";
  if (loc.isValid()) {
    oss << " at line " << loc.line << ", column " << loc.column;
  }
  oss << ": " << message;

  const std::string tokenDescription = describeTokenForDiagnostics(parser.curTok);
  if (!tokenDescription.empty()) {
    oss << " (near " << tokenDescription << ")";
  }

  std::string formatted = oss.str();
  fprintf(stderr, "%s\n", formatted.c_str());

  if (!hint.empty()) {
    fprintf(stderr, "  hint: %.*s\n", static_cast<int>(hint.size()), hint.data());
  }

  parser.hadError = true;
}

void reportCompilerWarning(const std::string &message, std::string_view hint) {
  const SourceLocation loc = bestErrorLocation();
  std::ostringstream oss;
  oss << "Warning";
  if (loc.isValid()) {
    oss << " at line " << loc.line << ", column " << loc.column;
  }
  oss << ": " << message;

  const std::string tokenDescription = describeTokenForDiagnostics(currentParser().curTok);
  if (!tokenDescription.empty()) {
    oss << " (near " << tokenDescription << ")";
  }

  std::string formatted = oss.str();
  fprintf(stderr, "%s\n", formatted.c_str());

  if (!hint.empty()) {
    fprintf(stderr, "  hint: %.*s\n", static_cast<int>(hint.size()), hint.data());
  }
}

// ParserContext ----------------------------------------------------------------

void ParserContext::reset(bool clearSymbols) {
  curTok = 0;
  loopNestingDepth = 0;
  unsafeContextLevel = 0;
  hadError = false;
  currentTokenLocation = {};
  previousTokenLocation = {};
  previousToken = 0;
  structDefinitionStack.clear();
  classDefinitionStack.clear();
  tokenReplayBuffer.clear();
  genericParameterStack.clear();
  activeGenericParameters.clear();
  templateAngleDepth = 0;
  if (clearSymbols)
    structNames.clear();
  if (clearSymbols)
    classNames.clear();
}

void ParserContext::clearPrecedence() {
  binopPrecedence.clear();
}

void ParserContext::pushGenericParameters(const std::vector<std::string> &params) {
  if (params.empty())
    return;
  genericParameterStack.push_back(params);
  for (const auto &name : params)
    activeGenericParameters.insert(name);
}

void ParserContext::popGenericParameters() {
  if (genericParameterStack.empty())
    return;
  const auto &params = genericParameterStack.back();
  for (const auto &name : params)
    activeGenericParameters.erase(name);
  genericParameterStack.pop_back();
}

bool ParserContext::isGenericParameter(const std::string &name) const {
  return activeGenericParameters.contains(name);
}

void ParserContext::pushReplayToken(int token, SourceLocation location) {
  PendingToken pending;
  pending.token = token;
  pending.location = location;
  tokenReplayBuffer.push_back(std::move(pending));
}

void ParserContext::pushReplayToken(const PendingToken &token) {
  tokenReplayBuffer.push_back(token);
}

bool ParserContext::hasReplayTokens() const {
  return !tokenReplayBuffer.empty();
}

ParserContext::PendingToken ParserContext::popReplayToken() {
  PendingToken tok = tokenReplayBuffer.back();
  tokenReplayBuffer.pop_back();
  return tok;
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
analysis::SemanticAnalysis &CompilerSession::analysis() {
  if (!analysisState)
    analysisState = std::make_unique<analysis::SemanticAnalysis>();
  return *analysisState;
}

void CompilerSession::resetParser() {
  parserState.reset();
  parserState.clearPrecedence();
}

void CompilerSession::resetAll() {
  lexerState.reset();
  parserState.reset();
  parserState.clearPrecedence();
  if (codegenState)
    codegenState->reset();
  if (analysisState)
    analysisState->reset();
}

void CompilerSession::beginUnit(bool preserveSymbols) {
  lexerState.reset();
  parserState.reset(!preserveSymbols);
  if (analysisState)
    analysisState->reset();
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

analysis::SemanticAnalysis &currentAnalysis() {
  return currentCompilerSession().analysis();
}
