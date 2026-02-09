#include "parser/parser_internal.h"
#include <cctype>

static std::vector<std::vector<ParserContext::PendingToken> *> &
tokenCaptureStack() {
  static std::vector<std::vector<ParserContext::PendingToken> *> stack;
  return stack;
}

static ParserContext::PendingToken snapshotCurrentToken() {
  ParserContext::PendingToken pending;
  pending.token = currentParser().curTok;
  pending.location = currentParser().currentTokenLocation;
  pending.identifier = currentLexer().identifierStr;
  pending.stringLiteral = currentLexer().stringLiteral;
  pending.numericLiteral = currentLexer().numericLiteral;
  pending.charLiteral = currentLexer().charLiteral;
  return pending;
}

TokenReplayScope::TokenReplayScope(bool enabled)
    : active(enabled),
      originalToken(currentParser().curTok),
      originalLocation(currentParser().currentTokenLocation),
      originalPreviousToken(currentParser().previousToken),
      originalPreviousLocation(currentParser().previousTokenLocation),
      originalIdentifier(currentLexer().identifierStr),
      originalStringLiteral(currentLexer().stringLiteral),
      originalNumericLiteral(currentLexer().numericLiteral),
      originalCharLiteral(currentLexer().charLiteral) {
  if (active)
    tokenCaptureStack().push_back(&capturedTokens);
}

TokenReplayScope::~TokenReplayScope() {
  if (active)
    tokenCaptureStack().pop_back();
}

void TokenReplayScope::rollback() {
  if (!active)
    return;
  ParserContext &parser = currentParser();
  parser.curTok = originalToken;
  parser.currentTokenLocation = originalLocation;
  parser.previousToken = originalPreviousToken;
  parser.previousTokenLocation = originalPreviousLocation;
  currentLexer().identifierStr = originalIdentifier;
  currentLexer().stringLiteral = originalStringLiteral;
  currentLexer().numericLiteral = originalNumericLiteral;
  currentLexer().charLiteral = originalCharLiteral;
  for (auto it = capturedTokens.rbegin(); it != capturedTokens.rend(); ++it)
    parser.pushReplayToken(*it);
  tokenCaptureStack().pop_back();
  active = false;
}

void TokenReplayScope::commit() {
  if (!active)
    return;
  tokenCaptureStack().pop_back();
  active = false;
}

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
int getNextToken() {
  ParserContext &parser = currentParser();
  int prevToken = parser.curTok;
  if (parser.hasReplayTokens()) {
    ParserContext::PendingToken pending = parser.popReplayToken();
    parser.previousTokenLocation = parser.currentTokenLocation;
    parser.previousToken = prevToken;
    parser.curTok = pending.token;
    parser.currentTokenLocation = pending.location;
    currentLexer().identifierStr = pending.identifier;
    currentLexer().stringLiteral = pending.stringLiteral;
    currentLexer().numericLiteral = pending.numericLiteral;
    currentLexer().charLiteral = pending.charLiteral;
    if (!tokenCaptureStack().empty())
      tokenCaptureStack().back()->push_back(snapshotCurrentToken());
    return parser.curTok;
  }
  parser.previousTokenLocation = parser.currentTokenLocation;
  parser.previousToken = prevToken;
  int next = gettok();
  parser.curTok = next;
  parser.currentTokenLocation = currentLexer().tokenStart();
  if (next == tok_right_shift && parser.templateAngleDepth > 0) {
    SourceLocation loc = parser.currentTokenLocation;
    parser.curTok = tok_gt;
    parser.pushReplayToken(tok_gt, loc);
  }
  if (!tokenCaptureStack().empty())
    tokenCaptureStack().back()->push_back(snapshotCurrentToken());
  return parser.curTok;
}

void SkipNewlines() {
  while (CurTok == tok_newline)
    getNextToken();
}

bool isInUnsafeContext() { return UnsafeContextLevel > 0; }

void enterUnsafeContext() { UnsafeContextLevel++; }

void exitUnsafeContext() {
  if (UnsafeContextLevel > 0)
    UnsafeContextLevel--;
}

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
int GetTokPrecedence() {
  using enum Token;

  // Handle comparison operator tokens
  if (CurTok == tok_eq) {
    return BinopPrecedence["=="];
  } else if (CurTok == tok_ne) {
    return BinopPrecedence["!="];
  } else if (CurTok == tok_le) {
    return BinopPrecedence["<="];
  } else if (CurTok == tok_ge) {
    return BinopPrecedence[">="];
  } else if (CurTok == tok_lt) {
    return BinopPrecedence["<"];
  } else if (CurTok == tok_gt) {
    return BinopPrecedence[">"];
  } else if (CurTok == tok_and) {
    return BinopPrecedence["&&"];
  } else if (CurTok == tok_or) {
    return BinopPrecedence["||"];
  } else if (CurTok == tok_plus_eq) {
    return BinopPrecedence["+="];
  } else if (CurTok == tok_minus_eq) {
    return BinopPrecedence["-="];
  } else if (CurTok == tok_mult_eq) {
    return BinopPrecedence["*="];
  } else if (CurTok == tok_div_eq) {
    return BinopPrecedence["/="];
  } else if (CurTok == tok_mod_eq) {
    return BinopPrecedence["%="];
  } else if (CurTok == tok_bitwise_and) {
    return BinopPrecedence["&"];
  } else if (CurTok == tok_bitwise_or) {
    return BinopPrecedence["|"];
  } else if (CurTok == tok_bitwise_xor) {
    return BinopPrecedence["^"];
  } else if (CurTok == tok_left_shift) {
    return BinopPrecedence["<<"];
  } else if (CurTok == tok_right_shift) {
    return BinopPrecedence[">>"];
  } else if (CurTok == tok_and_eq) {
    return BinopPrecedence["&="];
  } else if (CurTok == tok_or_eq) {
    return BinopPrecedence["|="];
  } else if (CurTok == tok_xor_eq) {
    return BinopPrecedence["^="];
  } else if (CurTok == tok_left_shift_eq) {
    return BinopPrecedence["<<="];
  } else if (CurTok == tok_right_shift_eq) {
    return BinopPrecedence[">>="];
  } else if (CurTok == tok_null_coalescing) {
    return BinopPrecedence["\?\?"];
  } else if (CurTok == tok_null_coalescing_assign) {
    return BinopPrecedence["\?\?="];
  } else if (CurTok == tok_if) {
    return 4; // Ternary operator precedence (higher than assignment, lower than logical OR)
  } else if (CurTok == tok_is) {
    return BinopPrecedence["=="];
  }

  // Handle single character operators
  if (isascii(CurTok)) {
    std::string Op(1, (char)CurTok);
    int TokPrec = BinopPrecedence[Op];
    if (TokPrec > 0)
      return TokPrec;
  }

  return -1;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const std::string &Str, std::string_view hint) {
  reportCompilerError(Str, hint);
  return nullptr;
}

std::unique_ptr<ExprAST> LogError(const char *Str, std::string_view hint) {
  return LogError(std::string(Str), hint);
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str, std::string_view hint) {
  LogError(Str, hint);
  return nullptr;
}

std::unique_ptr<StmtAST> LogErrorS(const char *Str, std::string_view hint) {
  LogError(Str, hint);
  return nullptr;
}
