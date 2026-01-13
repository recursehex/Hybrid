// This file implements the Hybrid lexer that tokenizes source text while tracking source locations.

#include "lexer.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdint>

#include "llvm/Support/Error.h"
#include "llvm/Support/ConvertUTF.h"

#include "compiler_session.h"

namespace {

static unsigned requiredBitsForDigits(size_t digitCount, unsigned radix) {
  unsigned bitsPerDigit = 4;
  switch (radix) {
    case 2: bitsPerDigit = 1; break;
    case 8: bitsPerDigit = 3; break;
    case 16: bitsPerDigit = 4; break;
    default: bitsPerDigit = 4; break;
  }

  unsigned parsedBits = static_cast<unsigned>(digitCount * bitsPerDigit + 2);
  return std::max(parsedBits, 64u);
}

static bool buildIntegerLiteral(const std::string &digits,
                                unsigned radix,
                                const std::string &spelling,
                                NumericLiteral &outLiteral) {
  if (digits.empty()) {
    reportCompilerError("Numeric literal '" + spelling + "' is missing digits");
    return false;
  }

  unsigned parseBits = requiredBitsForDigits(digits.size(), radix);

  llvm::APInt value(parseBits, digits, radix);
  if (value.getActiveBits() > 64) {
    reportCompilerError("Integer literal '" + spelling +
                        "' exceeds maximum supported 64-bit range");
    return false;
  }

  unsigned requiredBits = std::max(1u, value.getActiveBits());
  outLiteral = NumericLiteral::makeInteger(value, radix, spelling, requiredBits);
  return true;
}

static bool buildFloatingLiteral(const std::string &literal,
                                 bool hadDecimal,
                                 bool hadExponent,
                                 NumericLiteral &outLiteral) {
  llvm::APFloat value(0.0);
  auto statusOr = value.convertFromString(llvm::StringRef(literal),
                                          llvm::APFloat::rmNearestTiesToEven);

  if (!statusOr) {
    llvm::consumeError(statusOr.takeError());
    reportCompilerError("Invalid floating-point literal '" + literal + "'");
    return false;
  }

  llvm::APFloat::opStatus status = *statusOr;

  if (status & llvm::APFloat::opInvalidOp) {
    reportCompilerError("Invalid floating-point literal '" + literal + "'");
    return false;
  }

  if (status & llvm::APFloat::opOverflow) {
    reportCompilerError("Floating-point literal '" + literal +
                        "' overflowed the representable range");
    return false;
  }

  if (status & llvm::APFloat::opUnderflow) {
    reportCompilerError("Floating-point literal '" + literal +
                        "' underflowed the representable range");
    return false;
  }

  outLiteral = NumericLiteral::makeFloating(value, literal, hadDecimal, hadExponent);
  return true;
}

static unsigned utf8SequenceLength(unsigned char firstByte) {
  if ((firstByte & 0x80u) == 0)
    return 1;
  if ((firstByte & 0xE0u) == 0xC0u)
    return 2;
  if ((firstByte & 0xF0u) == 0xE0u)
    return 3;
  if ((firstByte & 0xF8u) == 0xF0u)
    return 4;
  return 0;
}

static bool decodeSingleUTF8CodePoint(const std::string &bytes,
                                      uint32_t &codePoint,
                                      std::string &errorMessage) {
  errorMessage.clear();

  const auto *sourceStart =
      reinterpret_cast<const llvm::UTF8 *>(bytes.data());
  const auto *sourceEnd = sourceStart + bytes.size();
  llvm::UTF32 temp = 0;
  llvm::UTF32 *targetStart = &temp;
  llvm::UTF32 *target = targetStart;
  llvm::ConversionResult result = llvm::ConvertUTF8toUTF32(
      &sourceStart, sourceEnd, &target, targetStart + 1,
      llvm::strictConversion);

  if (result != llvm::conversionOK || sourceStart != sourceEnd ||
      target != targetStart + 1) {
    switch (result) {
      case llvm::sourceExhausted:
        errorMessage =
            "Invalid UTF-8 character literal: unexpected end of sequence";
        break;
      case llvm::sourceIllegal:
        errorMessage =
            "Invalid UTF-8 character literal: illegal byte sequence";
        break;
      case llvm::targetExhausted:
        errorMessage =
            "Internal error: UTF-32 conversion buffer exhausted while decoding character literal";
        break;
      default:
        errorMessage = "Invalid UTF-8 character literal";
        break;
    }
    return false;
  }

  codePoint = temp;
  return true;
}

static bool isBinaryDigit(int ch) {
  return ch == '0' || ch == '1';
}

static bool isOctalDigit(int ch) {
  return ch >= '0' && ch <= '7';
}

static bool isDecimalDigit(int ch) {
  return ch >= '0' && ch <= '9';
}

static bool isHexDigit(int ch) {
  return std::isxdigit(static_cast<unsigned char>(ch));
}

static int lexPrefixedIntegerLiteral(LexerContext &lex,
                                     int &LastChar,
                                     unsigned radix,
                                     char prefixChar,
                                     SourceLocation tokenStart) {
  auto isValidDigit = [&](int ch) {
    switch (radix) {
      case 2: return isBinaryDigit(ch);
      case 8: return isOctalDigit(ch);
      case 10: return isDecimalDigit(ch);
      case 16: return isHexDigit(ch);
      default: return false;
    }
  };

  std::string digits;
  int CurrentChar = lex.consumeChar();

  while (isValidDigit(CurrentChar)) {
    digits += static_cast<char>(CurrentChar);
    CurrentChar = lex.consumeChar();
  }

  if (digits.empty()) {
    lex.setTokenStart(tokenStart);
    reportCompilerError("Numeric literal '0" + std::string(1, prefixChar) +
                        "' is missing digits");
    LastChar = CurrentChar;
    return tok_error;
  }

  if (std::isalnum(static_cast<unsigned char>(CurrentChar)) || CurrentChar == '_') {
    std::string spelling = "0";
    spelling += static_cast<char>(prefixChar);
    spelling += digits;
    spelling += static_cast<char>(CurrentChar);
    lex.setTokenStart(tokenStart);
    reportCompilerError("Invalid digit '" + std::string(1, static_cast<char>(CurrentChar)) +
                        "' in numeric literal '" + spelling + "'");
    LastChar = CurrentChar;
    return tok_error;
  }

  LastChar = CurrentChar;
  std::string spelling = "0";
  spelling += static_cast<char>(prefixChar);
  spelling += digits;

  if (!buildIntegerLiteral(digits, radix, spelling, lex.numericLiteral)) {
    lex.setTokenStart(tokenStart);
    return tok_error;
  }

  lex.setTokenStart(tokenStart);
  return tok_number;
}

} // namespace

static bool prepareInterpolatedLiteralSegment(LexerContext &lex, int &LastChar) {
  SourceLocation segmentStart = lex.lastCharLocation();
  std::string segment;

  while (true) {
    if (LastChar == EOF) {
      lex.setTokenStart(segmentStart);
      reportCompilerError("Unterminated interpolated string literal");
      lex.inInterpolatedString = false;
      return false;
    }

    if (LastChar == '`') {
      SourceLocation tickLocation = lex.lastCharLocation();
      LastChar = lex.consumeChar();
      lex.currentInterpolatedLiteral = std::move(segment);
      lex.pendingInterpolatedExprStart = true;
      lex.pendingInterpolatedStringEnd = false;
      lex.interpolatedSegmentLocation = segmentStart;
      lex.interpolatedExprStartLocation = tickLocation;
      lex.inInterpolatedString = true;
      return true;
    }

    if (LastChar == '"') {
      SourceLocation quoteLocation = lex.lastCharLocation();
      LastChar = lex.consumeChar();
      lex.currentInterpolatedLiteral = std::move(segment);
      lex.pendingInterpolatedExprStart = false;
      lex.pendingInterpolatedStringEnd = true;
      lex.interpolatedSegmentLocation = segmentStart;
      lex.interpolatedStringEndLocation = quoteLocation;
      lex.inInterpolatedString = false;
      return true;
    }

    if (LastChar == '\\') {
      LastChar = lex.consumeChar();
      switch (LastChar) {
        case 'n': segment += '\n'; break;
        case 't': segment += '\t'; break;
        case 'r': segment += '\r'; break;
        case '\\': segment += '\\'; break;
        case '"': segment += '"'; break;
        case '`': segment += '`'; break;
        case 'u': {
          uint32_t unicode = 0;
          bool valid = true;
          for (int i = 0; i < 4; i++) {
            LastChar = lex.consumeChar();
            if (LastChar >= '0' && LastChar <= '9') {
              unicode = (unicode << 4) | (LastChar - '0');
            } else if (LastChar >= 'a' && LastChar <= 'f') {
              unicode = (unicode << 4) | (LastChar - 'a' + 10);
            } else if (LastChar >= 'A' && LastChar <= 'F') {
              unicode = (unicode << 4) | (LastChar - 'A' + 10);
            } else {
              valid = false;
              break;
            }
          }
          if (!valid) {
            segment += '?';
            break;
          }

          if (unicode <= 0x7F) {
            segment += static_cast<char>(unicode);
          } else if (unicode <= 0x7FF) {
            segment += static_cast<char>(0xC0 | (unicode >> 6));
            segment += static_cast<char>(0x80 | (unicode & 0x3F));
          } else {
            segment += static_cast<char>(0xE0 | (unicode >> 12));
            segment += static_cast<char>(0x80 | ((unicode >> 6) & 0x3F));
            segment += static_cast<char>(0x80 | (unicode & 0x3F));
          }
          break;
        }
        default:
          segment += static_cast<char>(LastChar);
          break;
      }
      LastChar = lex.consumeChar();
      continue;
    }

    segment += static_cast<char>(LastChar);
    LastChar = lex.consumeChar();
  }
}

/// gettok - Return the next token from standard input.
int gettok() {
  LexerContext &lex = currentLexer();
  int &LastChar = lex.lastChar;

  // Pending interpolated string literal segment
  if (lex.pendingInterpolatedLiteralSegment) {
    lex.pendingInterpolatedLiteralSegment = false;
    lex.setTokenStart(lex.interpolatedSegmentLocation);
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    return tok_interpolated_string_segment;
  }

  // Pending interpolated expression start
  if (lex.pendingInterpolatedExprStart) {
    lex.pendingInterpolatedExprStart = false;
    lex.setTokenStart(lex.interpolatedExprStartLocation);
    lex.inInterpolatedExpression = true;
    return tok_interpolated_expr_start;
  }

  // Pending interpolated string end
  if (lex.pendingInterpolatedStringEnd) {
    lex.pendingInterpolatedStringEnd = false;
    lex.setTokenStart(lex.interpolatedStringEndLocation);
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    return tok_interpolated_string_end;
  }

  // Handle end of interpolated expression
  if (lex.inInterpolatedString && lex.inInterpolatedExpression && LastChar == '`') {
    SourceLocation endTickLocation = lex.lastCharLocation();
    LastChar = lex.consumeChar();
    lex.inInterpolatedExpression = false;
    if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
      lex.setTokenStart(endTickLocation);
      return tok_error;
    }
    lex.pendingInterpolatedLiteralSegment = true;
    lex.interpolatedSegmentLocation = lex.lastCharLocation();
    lex.setTokenStart(endTickLocation);
    return tok_interpolated_expr_end;
  }

  // Skip any whitespace except newlines.
  while (isspace(LastChar) && LastChar != '\n' && LastChar != '\r')
    LastChar = lex.consumeChar();

  if (lex.inInterpolatedString && !lex.inInterpolatedExpression) {
    if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
      lex.setTokenStart(lex.interpolatedSegmentLocation);
      return tok_error;
    }
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    lex.setTokenStart(lex.interpolatedSegmentLocation);
    return tok_interpolated_string_segment;
  }

  if (!lex.inInterpolatedString && LastChar == '$') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '"') {
      lex.inInterpolatedString = true;
      lex.inInterpolatedExpression = false;
      lex.pendingInterpolatedExprStart = false;
      lex.pendingInterpolatedStringEnd = false;
      lex.pendingInterpolatedLiteralSegment = false;
      LastChar = lex.consumeChar();
      if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
        lex.setTokenStart(start);
        return tok_error;
      }
      lex.stringLiteral = lex.currentInterpolatedLiteral;
      lex.setTokenStart(start);
      return tok_interpolated_string_start;
    }
    lex.unconsumeChar(NextChar);
  }

  if (isalpha(LastChar) || LastChar == '_') { // identifier: [a-zA-Z_][a-zA-Z0-9_]*
    SourceLocation start = lex.lastCharLocation();
    lex.setTokenStart(start);
    lex.identifierStr = LastChar;
    while (isalnum((LastChar = lex.consumeChar())) || LastChar == '_')
      lex.identifierStr += LastChar;

    if (lex.identifierStr == "use")
      return tok_use;
    if (lex.identifierStr == "extern")
      return tok_extern;
    if (lex.identifierStr == "return")
      return tok_return;
    if (lex.identifierStr == "delegate")
      return tok_delegate;
    if (lex.identifierStr == "for")
      return tok_for;
    if (lex.identifierStr == "in")
      return tok_in;
    if (lex.identifierStr == "to")
      return tok_to;
    if (lex.identifierStr == "by")
      return tok_by;
    if (lex.identifierStr == "int")
      return tok_int;
    if (lex.identifierStr == "float")
      return tok_float;
    if (lex.identifierStr == "double")
      return tok_double;
    if (lex.identifierStr == "char")
      return tok_char;
    if (lex.identifierStr == "void")
      return tok_void;
    if (lex.identifierStr == "bool")
      return tok_bool;
    if (lex.identifierStr == "string")
      return tok_string;
    if (lex.identifierStr == "byte")
      return tok_byte;
    if (lex.identifierStr == "short")
      return tok_short;
    if (lex.identifierStr == "long")
      return tok_long;
    if (lex.identifierStr == "sbyte")
      return tok_sbyte;
    if (lex.identifierStr == "ushort")
      return tok_ushort;
    if (lex.identifierStr == "uint")
      return tok_uint;
    if (lex.identifierStr == "ulong")
      return tok_ulong;
    if (lex.identifierStr == "schar")
      return tok_schar;
    if (lex.identifierStr == "lchar")
      return tok_lchar;
    if (lex.identifierStr == "true")
      return tok_true;
    if (lex.identifierStr == "false")
      return tok_false;
    if (lex.identifierStr == "null")
      return tok_null;
    if (lex.identifierStr == "if")
      return tok_if;
    if (lex.identifierStr == "is")
      return tok_is;
    if (lex.identifierStr == "not")
      return tok_not_kw;
    if (lex.identifierStr == "else")
      return tok_else;
    if (lex.identifierStr == "while")
      return tok_while;
    if (lex.identifierStr == "break")
      return tok_break;
    if (lex.identifierStr == "skip")
      return tok_skip;
    if (lex.identifierStr == "struct")
      return tok_struct;
    if (lex.identifierStr == "class")
      return tok_class;
    if (lex.identifierStr == "interface")
      return tok_interface;
    if (lex.identifierStr == "this")
      return tok_this;
    if (lex.identifierStr == "base")
      return tok_base;
    if (lex.identifierStr == "switch")
      return tok_switch;
    if (lex.identifierStr == "case")
      return tok_case;
    if (lex.identifierStr == "default")
      return tok_default;
    if (lex.identifierStr == "assert")
      return tok_assert;
    if (lex.identifierStr == "unsafe")
      return tok_unsafe;
    if (lex.identifierStr == "ref")
      return tok_ref;
    if (lex.identifierStr == "params")
      return tok_params;
    if (lex.identifierStr == "get")
      return tok_get;
    if (lex.identifierStr == "set")
      return tok_set;
    if (lex.identifierStr == "weak")
      return tok_weak;
    if (lex.identifierStr == "unique")
      return tok_unique;
    if (lex.identifierStr == "shared")
      return tok_shared;
    if (lex.identifierStr == "new")
      return tok_new;
    if (lex.identifierStr == "free")
      return tok_free;
    if (lex.identifierStr == "abstract")
      return tok_abstract;
    if (lex.identifierStr == "inherits")
      return tok_inherits;
    if (lex.identifierStr == "public")
      return tok_public;
    if (lex.identifierStr == "private")
      return tok_private;
    if (lex.identifierStr == "protected")
      return tok_protected;
    if (lex.identifierStr == "static")
      return tok_static;
    if (lex.identifierStr == "const")
      return tok_const;
    if (lex.identifierStr == "virtual")
      return tok_virtual;
    if (lex.identifierStr == "override")
      return tok_override;
    return tok_identifier;
  }

  if (isdigit(LastChar)) {
    SourceLocation start = lex.lastCharLocation();
    lex.setTokenStart(start);
    if (LastChar == '0') {
      int NextChar = lex.consumeChar();
      if (NextChar == 'b' || NextChar == 'B') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 2, static_cast<char>(NextChar), start);
      } else if (NextChar == 'o' || NextChar == 'O') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 8, static_cast<char>(NextChar), start);
      } else if (NextChar == 'x' || NextChar == 'X') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 16, static_cast<char>(NextChar), start);
      } else {
        lex.unconsumeChar(NextChar);
      }
    }

    std::string literal;
    std::string integerDigits;
    bool hasDecimal = false;
    bool hasExponent = false;

    while (isDecimalDigit(LastChar)) {
      literal += static_cast<char>(LastChar);
      integerDigits += static_cast<char>(LastChar);
      LastChar = lex.consumeChar();
    }

    if (LastChar == '.') {
      hasDecimal = true;
      literal += '.';
      LastChar = lex.consumeChar();
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = lex.consumeChar();
      }
    }

    if (LastChar == 'e' || LastChar == 'E') {
      hasExponent = true;
      literal += static_cast<char>(LastChar);
      LastChar = lex.consumeChar();
      if (LastChar == '+' || LastChar == '-') {
        literal += static_cast<char>(LastChar);
        LastChar = lex.consumeChar();
      }
      if (!isDecimalDigit(LastChar)) {
        reportCompilerError("Invalid exponent in floating-point literal '" + literal + "'");
        return tok_error;
      }
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = lex.consumeChar();
      }
    }

    if (!hasDecimal && !hasExponent) {
      if (!buildIntegerLiteral(integerDigits, 10, literal, lex.numericLiteral))
        return tok_error;
      return tok_number;
    }

    if (!buildFloatingLiteral(literal, hasDecimal, hasExponent, lex.numericLiteral))
      return tok_error;
    return tok_number;
  }

  if (LastChar == '.') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (isdigit(NextChar)) {
      std::string literal = ".";
      literal += static_cast<char>(NextChar);
      bool hasExponent = false;
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = lex.consumeChar();
      }

      if (LastChar == 'e' || LastChar == 'E') {
        hasExponent = true;
        literal += static_cast<char>(LastChar);
        LastChar = lex.consumeChar();
        if (LastChar == '+' || LastChar == '-') {
          literal += static_cast<char>(LastChar);
          LastChar = lex.consumeChar();
        }
        if (!isDecimalDigit(LastChar)) {
          reportCompilerError("Invalid exponent in floating-point literal '" + literal + "'");
          lex.setTokenStart(start);
          return tok_error;
        }
        while (isDecimalDigit(LastChar)) {
          literal += static_cast<char>(LastChar);
          LastChar = lex.consumeChar();
        }
      }

      if (!buildFloatingLiteral(literal, true, hasExponent, lex.numericLiteral)) {
        lex.setTokenStart(start);
        return tok_error;
      }
      lex.setTokenStart(start);
      return tok_number;
    } else {
      lex.unconsumeChar(NextChar);
    }
  }

  if (LastChar == '"') { // String literal: "..."
    SourceLocation start = lex.lastCharLocation();
    lex.setTokenStart(start);
    lex.stringLiteral = "";
    LastChar = lex.consumeChar();
    while (LastChar != '"' && LastChar != EOF) {
      if (LastChar == '\\') {
        // Handle escape sequences
        LastChar = lex.consumeChar();
        switch (LastChar) {
          case 'n': lex.stringLiteral += '\n'; break;
          case 't': lex.stringLiteral += '\t'; break;
          case 'r': lex.stringLiteral += '\r'; break;
          case '\\': lex.stringLiteral += '\\'; break;
          case '"': lex.stringLiteral += '"'; break;
          case 'u': { // Unicode escape: \uXXXX
            uint32_t unicode = 0;
            for (int i = 0; i < 4; i++) {
              LastChar = lex.consumeChar();
              if (LastChar >= '0' && LastChar <= '9') {
                unicode = (unicode << 4) | (LastChar - '0');
              } else if (LastChar >= 'a' && LastChar <= 'f') {
                unicode = (unicode << 4) | (LastChar - 'a' + 10);
              } else if (LastChar >= 'A' && LastChar <= 'F') {
                unicode = (unicode << 4) | (LastChar - 'A' + 10);
              } else {
                lex.stringLiteral += '?'; // Invalid escape
                goto string_continue;
              }
            }
            // Convert Unicode codepoint to UTF-8
            if (unicode <= 0x7F) {
              lex.stringLiteral += static_cast<char>(unicode);
            } else if (unicode <= 0x7FF) {
              lex.stringLiteral += static_cast<char>(0xC0 | (unicode >> 6));
              lex.stringLiteral += static_cast<char>(0x80 | (unicode & 0x3F));
            } else {
              lex.stringLiteral += static_cast<char>(0xE0 | (unicode >> 12));
              lex.stringLiteral += static_cast<char>(0x80 | ((unicode >> 6) & 0x3F));
              lex.stringLiteral += static_cast<char>(0x80 | (unicode & 0x3F));
            }
            break;
          }
          default: lex.stringLiteral += LastChar; break;
        }
      } else {
        lex.stringLiteral += LastChar;
      }
string_continue:
      LastChar = lex.consumeChar();
    }
    if (LastChar == '"') {
      LastChar = lex.consumeChar(); // eat closing "
      return tok_string_literal;
    }
  }

  if (LastChar == '\'') { // Character literal: '.'
    SourceLocation start = lex.lastCharLocation();
    lex.setTokenStart(start);
    LastChar = lex.consumeChar();
    if (LastChar == EOF) {
      return LastChar; // Error case - EOF in character literal
    }
    
    if (LastChar == '\\') {
      // Handle escape sequences
      LastChar = lex.consumeChar();
      switch (LastChar) {
        case 'n': lex.charLiteral = '\n'; break;
        case 't': lex.charLiteral = '\t'; break;
        case 'r': lex.charLiteral = '\r'; break;
        case '\\': lex.charLiteral = '\\'; break;
        case '\'': lex.charLiteral = '\''; break;
        case '0': lex.charLiteral = '\0'; break;
        case 'u': { // Unicode escape: \uXXXX (16-bit)
          uint16_t unicode = 0;
          for (int i = 0; i < 4; i++) {
            LastChar = lex.consumeChar();
            if (LastChar >= '0' && LastChar <= '9') {
              unicode = (unicode << 4) | (LastChar - '0');
            } else if (LastChar >= 'a' && LastChar <= 'f') {
              unicode = (unicode << 4) | (LastChar - 'a' + 10);
            } else if (LastChar >= 'A' && LastChar <= 'F') {
              unicode = (unicode << 4) | (LastChar - 'A' + 10);
            } else {
              // Invalid Unicode escape
              lex.charLiteral = '?';
              goto char_literal_end;
            }
          }
          // Validate Unicode code point: reject UTF-16 surrogates (U+D800 to U+DFFF)
          if (unicode >= 0xD800 && unicode <= 0xDFFF) {
            char buffer[64];
            std::snprintf(buffer, sizeof(buffer),
                          "Invalid Unicode code point U+%04X (UTF-16 surrogate)",
                          unicode);
            reportCompilerError(buffer);
            lex.charLiteral = '?';
          } else {
            lex.charLiteral = unicode;
          }
          break;
        }
        case 'U': { // Unicode escape: \UXXXXXXXX (32-bit)
          uint32_t unicode = 0;
          for (int i = 0; i < 8; i++) {
            LastChar = lex.consumeChar();
            if (LastChar >= '0' && LastChar <= '9') {
              unicode = (unicode << 4) | (LastChar - '0');
            } else if (LastChar >= 'a' && LastChar <= 'f') {
              unicode = (unicode << 4) | (LastChar - 'a' + 10);
            } else if (LastChar >= 'A' && LastChar <= 'F') {
              unicode = (unicode << 4) | (LastChar - 'A' + 10);
            } else {
              // Invalid Unicode escape
              lex.charLiteral = '?';
              goto char_literal_end;
            }
          }
          // Validate Unicode code point
          // Valid range: U+0000 to U+10FFFF, excluding surrogates U+D800 to U+DFFF
          if (unicode > 0x10FFFF || (unicode >= 0xD800 && unicode <= 0xDFFF)) {
            char buffer[64];
            std::snprintf(buffer, sizeof(buffer),
                          "Invalid Unicode code point U+%08X", unicode);
            reportCompilerError(buffer);
            lex.charLiteral = '?';
          } else {
            lex.charLiteral = unicode;
          }
          break;
        }
        default: lex.charLiteral = LastChar; break;
      }
    } else {
      std::string utf8Bytes;
      utf8Bytes.push_back(static_cast<char>(LastChar));
      unsigned expectedLength =
          utf8SequenceLength(static_cast<unsigned char>(utf8Bytes[0]));

      if (expectedLength == 0) {
        reportCompilerError("Invalid UTF-8 start byte in character literal");
        lex.charLiteral = '?';
        goto char_literal_end;
      }

      for (unsigned i = 1; i < expectedLength; ++i) {
        LastChar = lex.consumeChar();
        if (LastChar == EOF) {
          reportCompilerError("Incomplete UTF-8 sequence in character literal");
          lex.charLiteral = '?';
          goto char_literal_end;
        }
        if ((LastChar & 0xC0) != 0x80) {
          reportCompilerError("Invalid UTF-8 continuation byte in character literal");
          lex.charLiteral = '?';
          goto char_literal_end;
        }
        utf8Bytes.push_back(static_cast<char>(LastChar));
      }

      uint32_t decoded = 0;
      std::string decodeError;
      if (!decodeSingleUTF8CodePoint(utf8Bytes, decoded, decodeError)) {
        if (!decodeError.empty())
          reportCompilerError(decodeError);
        else
          reportCompilerError("Invalid UTF-8 character literal");
        lex.charLiteral = '?';
      } else {
        lex.charLiteral = decoded;
      }
    }
    
char_literal_end:
    LastChar = lex.consumeChar();
    if (LastChar == '\'') {
      LastChar = lex.consumeChar(); // eat closing '
      return tok_char_literal;
    }
    reportCompilerError("Unterminated character literal");
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r' && LastChar != '\'')
      LastChar = lex.consumeChar();
    if (LastChar == '\'')
      LastChar = lex.consumeChar();
    return tok_error;
  }

  // Check for arithmetic operators and their compound assignments
  if (LastChar == '+' || LastChar == '-' || LastChar == '*' || LastChar == '/' || LastChar == '%') {
    char Op = LastChar;
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    
    // Handle // comment special case
    if (Op == '/' && NextChar == '/') {
      // Comment until end of line.
      do
        LastChar = lex.consumeChar();
      while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    }

    // Handle /* */ block comments
    if (Op == '/' && NextChar == '*') {
      SourceLocation commentStart = start;
      LastChar = lex.consumeChar(); // move past the initial "/*"
      while (true) {
        if (LastChar == EOF) {
          lex.setTokenStart(commentStart);
          reportCompilerError("Unterminated block comment");
          return tok_error;
        }

        if (LastChar == '*') {
          int maybeSlash = lex.consumeChar();
          if (maybeSlash == '/') {
            LastChar = lex.consumeChar(); // consume '/' and continue lexing
            break;
          }
          LastChar = maybeSlash;
          continue;
        }

        LastChar = lex.consumeChar();
      }

      return gettok();
    }
    
    // Handle increment operator
    if (Op == '+' && NextChar == '+') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_inc; // ++
    }

    // Handle decrement operator
    if (Op == '-' && NextChar == '-') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_dec; // --
    }

    // Handle arrow operator for pointer member access
    if (Op == '-' && NextChar == '>') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_arrow; // ->
    }

    // Check for compound assignment
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      switch (Op) {
        case '+': lex.setTokenStart(start); return tok_plus_eq;   // +=
        case '-': lex.setTokenStart(start); return tok_minus_eq;  // -=
        case '*': lex.setTokenStart(start); return tok_mult_eq;   // *=
        case '/': lex.setTokenStart(start); return tok_div_eq;    // /=
        case '%': lex.setTokenStart(start); return tok_mod_eq;    // %=
      }
    } else {
      // Not a compound assignment, put the character back
      lex.unconsumeChar(NextChar);
    }
  }

  // Check for comparison operators
  if (LastChar == '=') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_eq;  // ==
    } else if (NextChar == '>') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_lambda;  // =>
    } else {
      // Not == or =>, put the character back
      lex.unconsumeChar(NextChar);
    }
  }
  
  if (LastChar == '!') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_ne;  // !=
    } else {
      // Not !=, put the character back and return ! token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_not;  // !
    }
  }
  
  if (LastChar == '<') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_le;  // <=
    } else if (NextChar == '<') {
      NextChar = lex.consumeChar();
      if (NextChar == '=') {
        LastChar = lex.consumeChar();
        lex.setTokenStart(start);
        return tok_left_shift_eq;  // <<=
      } else {
        // Not <<=, put the character back and return << token
        lex.unconsumeChar(NextChar);
        LastChar = lex.consumeChar();
        lex.setTokenStart(start);
        return tok_left_shift;  // <<
      }
    } else {
      // Not <= or <<, put the character back and return < token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_lt;  // <
    }
  }
  
  if (LastChar == '>') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_ge;  // >=
    } else if (NextChar == '>') {
      NextChar = lex.consumeChar();
      if (NextChar == '=') {
        LastChar = lex.consumeChar();
        lex.setTokenStart(start);
        return tok_right_shift_eq;  // >>=
      } else {
        // Not >>=, put the character back and return >> token
        lex.unconsumeChar(NextChar);
        LastChar = lex.consumeChar();
        lex.setTokenStart(start);
        return tok_right_shift;  // >>
      }
    } else {
      // Not >= or >>, put the character back and return > token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_gt;  // >
    }
  }
  
  if (LastChar == '&') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '&') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_and;  // &&
    } else if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_and_eq;  // &=
    } else {
      // Not && or &=, put the character back and return & token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_bitwise_and;  // &
    }
  }
  
  if (LastChar == '|') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '|') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_or;  // ||
    } else if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_or_eq;  // |=
    } else {
      // Not || or |=, put the character back and return | token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_bitwise_or;  // |
    }
  }
  
  if (LastChar == '^') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_xor_eq;  // ^=
    } else {
      // Not ^=, put the character back and return ^ token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_bitwise_xor;  // ^
    }
  }

  if (LastChar == '~') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (std::isalpha(static_cast<unsigned char>(NextChar)) || NextChar == '_') {
      std::string ident;
      ident.push_back(static_cast<char>(NextChar));
      int ch = lex.consumeChar();
      while (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
        ident.push_back(static_cast<char>(ch));
        ch = lex.consumeChar();
      }
      lex.identifierStr = ident;
      LastChar = ch;
      lex.setTokenStart(start);
      return tok_tilde_identifier;
    }
    lex.unconsumeChar(NextChar);
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return '~';
  }

  // Check for colon (type casting operator)
  if (LastChar == ':') {
    SourceLocation start = lex.lastCharLocation();
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return tok_colon;
  }

  // Check for dot (member access).
  if (LastChar == '.') {
    SourceLocation start = lex.lastCharLocation();
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return tok_dot;
  }

  // Check for @ (dereference/pointer type or autoreleasepool)
  if (LastChar == '@') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();
    if (std::isalpha(static_cast<unsigned char>(NextChar)) || NextChar == '_') {
      std::string ident;
      ident.push_back(static_cast<char>(NextChar));
      int ch = lex.consumeChar();
      while (std::isalnum(static_cast<unsigned char>(ch)) || ch == '_') {
        ident.push_back(static_cast<char>(ch));
        ch = lex.consumeChar();
      }
      if (ident == "autoreleasepool") {
        lex.identifierStr = ident;
        LastChar = ch;
        lex.setTokenStart(start);
        return tok_autoreleasepool;
      }
      lex.unconsumeChar(ch);
      for (auto it = ident.rbegin(); it != ident.rend(); ++it)
        lex.unconsumeChar(*it);
    } else {
      lex.unconsumeChar(NextChar);
    }
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return tok_at;
  }

  // Check for # (address-of)
  if (LastChar == '#') {
    SourceLocation start = lex.lastCharLocation();
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return tok_hash;
  }

  // Check for nullable related tokens ( ?, ?. , ?[, ??, ??= )
  if (LastChar == '?') {
    SourceLocation start = lex.lastCharLocation();
    int NextChar = lex.consumeChar();

    if (NextChar == '.') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_null_safe_access;
    }

    if (NextChar == '[') {
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_null_array_access;
    }

    if (NextChar == '?') {
      int ThirdChar = lex.consumeChar();
      if (ThirdChar == '=') {
        LastChar = lex.consumeChar();
        lex.setTokenStart(start);
       return tok_null_coalescing_assign;
      }
      lex.unconsumeChar(ThirdChar);
      LastChar = lex.consumeChar();
      lex.setTokenStart(start);
      return tok_null_coalescing;
    }

    lex.unconsumeChar(NextChar);
    LastChar = lex.consumeChar();
    lex.setTokenStart(start);
    return tok_nullable;
  }

  // Check for newline.
  if (LastChar == '\n' || LastChar == '\r') {
    SourceLocation start = lex.lastCharLocation();
    if (LastChar == '\r') {
      LastChar = lex.consumeChar();
      if (LastChar == '\n') {
        LastChar = lex.consumeChar();
      }
    } else {
      LastChar = lex.consumeChar();
    }
    lex.setTokenStart(start);
    return tok_newline;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    lex.setTokenStart(lex.lastCharLocation());
    return tok_eof;
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  SourceLocation start = lex.lastCharLocation();
  LastChar = lex.consumeChar();
  lex.setTokenStart(start);
  return ThisChar;
}
