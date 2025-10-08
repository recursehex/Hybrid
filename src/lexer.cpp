#include "lexer.h"

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <iostream>

#include "llvm/Support/Error.h"

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
    std::cerr << "Error: Numeric literal '" << spelling << "' is missing digits\n";
    return false;
  }

  unsigned parseBits = requiredBitsForDigits(digits.size(), radix);

  llvm::APInt value(parseBits, digits, radix);
  if (value.getActiveBits() > 64) {
    std::cerr << "Error: Integer literal '" << spelling
              << "' exceeds maximum supported 64-bit range\n";
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
    std::cerr << "Error: Invalid floating-point literal '" << literal << "'\n";
    return false;
  }

  llvm::APFloat::opStatus status = *statusOr;

  if (status & llvm::APFloat::opInvalidOp) {
    std::cerr << "Error: Invalid floating-point literal '" << literal << "'\n";
    return false;
  }

  if (status & llvm::APFloat::opOverflow) {
    std::cerr << "Error: Floating-point literal '" << literal << "' overflowed the representable range\n";
    return false;
  }

  if (status & llvm::APFloat::opUnderflow) {
    std::cerr << "Error: Floating-point literal '" << literal << "' underflowed the representable range\n";
    return false;
  }

  outLiteral = NumericLiteral::makeFloating(value, literal, hadDecimal, hadExponent);
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
                                     char prefixChar) {
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
    std::cerr << "Error: Numeric literal '0" << static_cast<char>(prefixChar)
              << "' is missing digits\n";
    LastChar = CurrentChar;
    return tok_error;
  }

  if (std::isalnum(static_cast<unsigned char>(CurrentChar)) || CurrentChar == '_') {
    std::string spelling = "0";
    spelling += static_cast<char>(prefixChar);
    spelling += digits;
    spelling += static_cast<char>(CurrentChar);
    std::cerr << "Error: Invalid digit '" << static_cast<char>(CurrentChar)
              << "' in numeric literal '" << spelling << "'\n";
    LastChar = CurrentChar;
    return tok_error;
  }

  LastChar = CurrentChar;
  std::string spelling = "0";
  spelling += static_cast<char>(prefixChar);
  spelling += digits;

  if (!buildIntegerLiteral(digits, radix, spelling, lex.numericLiteral))
    return tok_error;

  return tok_number;
}

} // namespace

static bool prepareInterpolatedLiteralSegment(LexerContext &lex, int &LastChar) {
  std::string segment;

  while (true) {
    if (LastChar == EOF) {
      std::cerr << "Error: Unterminated interpolated string literal\n";
      lex.inInterpolatedString = false;
      return false;
    }

    if (LastChar == '`') {
      LastChar = lex.consumeChar();
      lex.currentInterpolatedLiteral = std::move(segment);
      lex.pendingInterpolatedExprStart = true;
      lex.pendingInterpolatedStringEnd = false;
      lex.inInterpolatedString = true;
      return true;
    }

    if (LastChar == '"') {
      LastChar = lex.consumeChar();
      lex.currentInterpolatedLiteral = std::move(segment);
      lex.pendingInterpolatedExprStart = false;
      lex.pendingInterpolatedStringEnd = true;
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
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    return tok_interpolated_string_segment;
  }

  // Pending interpolated expression start
  if (lex.pendingInterpolatedExprStart) {
    lex.pendingInterpolatedExprStart = false;
    lex.inInterpolatedExpression = true;
    return tok_interpolated_expr_start;
  }

  // Pending interpolated string end
  if (lex.pendingInterpolatedStringEnd) {
    lex.pendingInterpolatedStringEnd = false;
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    return tok_interpolated_string_end;
  }

  // Handle end of interpolated expression
  if (lex.inInterpolatedString && lex.inInterpolatedExpression && LastChar == '`') {
    LastChar = lex.consumeChar();
    lex.inInterpolatedExpression = false;
    if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
      return tok_error;
    }
    lex.pendingInterpolatedLiteralSegment = true;
    return tok_interpolated_expr_end;
  }

  // Skip any whitespace except newlines.
  while (isspace(LastChar) && LastChar != '\n' && LastChar != '\r')
    LastChar = lex.consumeChar();

  if (lex.inInterpolatedString && !lex.inInterpolatedExpression) {
    if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
      return tok_error;
    }
    lex.stringLiteral = lex.currentInterpolatedLiteral;
    return tok_interpolated_string_segment;
  }

  if (!lex.inInterpolatedString && LastChar == '$') {
    int NextChar = lex.consumeChar();
    if (NextChar == '"') {
      lex.inInterpolatedString = true;
      lex.inInterpolatedExpression = false;
      lex.pendingInterpolatedExprStart = false;
      lex.pendingInterpolatedStringEnd = false;
      lex.pendingInterpolatedLiteralSegment = false;
      LastChar = lex.consumeChar();
      if (!prepareInterpolatedLiteralSegment(lex, LastChar)) {
        return tok_error;
      }
      lex.stringLiteral = lex.currentInterpolatedLiteral;
      return tok_interpolated_string_start;
    }
    lex.unconsumeChar(NextChar);
  }

  if (isalpha(LastChar) || LastChar == '_') { // identifier: [a-zA-Z_][a-zA-Z0-9_]*
    lex.identifierStr = LastChar;
    while (isalnum((LastChar = lex.consumeChar())) || LastChar == '_')
      lex.identifierStr += LastChar;

    if (lex.identifierStr == "use")
      return tok_use;
    if (lex.identifierStr == "extern")
      return tok_extern;
    if (lex.identifierStr == "return")
      return tok_return;
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
    if (lex.identifierStr == "this")
      return tok_this;
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
    return tok_identifier;
  }

  if (isdigit(LastChar)) {
    if (LastChar == '0') {
      int NextChar = lex.consumeChar();
      if (NextChar == 'b' || NextChar == 'B') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 2, static_cast<char>(NextChar));
      } else if (NextChar == 'o' || NextChar == 'O') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 8, static_cast<char>(NextChar));
      } else if (NextChar == 'x' || NextChar == 'X') {
        return lexPrefixedIntegerLiteral(lex, LastChar, 16, static_cast<char>(NextChar));
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
        std::cerr << "Error: Invalid exponent in floating-point literal '" << literal << "'\n";
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
    int NextChar = lex.consumeChar();
    if (isdigit(NextChar)) {
      std::string literal = ".";
      literal += static_cast<char>(NextChar);
      bool hasExponent = false;
      LastChar = lex.consumeChar();
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
          std::cerr << "Error: Invalid exponent in floating-point literal '" << literal << "'\n";
          return tok_error;
        }
        while (isDecimalDigit(LastChar)) {
          literal += static_cast<char>(LastChar);
          LastChar = lex.consumeChar();
        }
      }

      if (!buildFloatingLiteral(literal, true, hasExponent, lex.numericLiteral))
        return tok_error;
      return tok_number;
    } else {
      lex.unconsumeChar(NextChar);
    }
  }

  if (LastChar == '"') { // String literal: "..."
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
            fprintf(stderr, "Error: Invalid Unicode code point U+%04X (UTF-16 surrogate)\n", unicode);
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
            fprintf(stderr, "Error: Invalid Unicode code point U+%08X\n", unicode);
            lex.charLiteral = '?';
          } else {
            lex.charLiteral = unicode;
          }
          break;
        }
        default: lex.charLiteral = LastChar; break;
      }
    } else if ((LastChar & 0x80) != 0) {
      // UTF-8 multi-byte character
      uint32_t unicode = 0;
      int bytes_to_read = 0;
      
      if ((LastChar & 0xE0) == 0xC0) { // 2-byte UTF-8
        unicode = LastChar & 0x1F;
        bytes_to_read = 1;
      } else if ((LastChar & 0xF0) == 0xE0) { // 3-byte UTF-8
        unicode = LastChar & 0x0F;
        bytes_to_read = 2;
      } else if ((LastChar & 0xF8) == 0xF0) { // 4-byte UTF-8
        unicode = LastChar & 0x07;
        bytes_to_read = 3;
      } else {
        // Invalid UTF-8 start byte
        lex.charLiteral = '?';
        goto char_literal_end;
      }
      
      for (int i = 0; i < bytes_to_read; i++) {
        LastChar = lex.consumeChar();
        if ((LastChar & 0xC0) != 0x80) {
          // Invalid UTF-8 continuation byte
          lex.charLiteral = '?';
          goto char_literal_end;
        }
        unicode = (unicode << 6) | (LastChar & 0x3F);
      }
      
      lex.charLiteral = unicode;
    } else {
      lex.charLiteral = LastChar;
    }
    
char_literal_end:
    LastChar = lex.consumeChar();
    if (LastChar == '\'') {
      LastChar = lex.consumeChar(); // eat closing '
      return tok_char_literal;
    }
    std::cerr << "Error: Unterminated character literal\n";
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r' && LastChar != '\'')
      LastChar = lex.consumeChar();
    if (LastChar == '\'')
      LastChar = lex.consumeChar();
    return tok_error;
  }

  // Check for arithmetic operators and their compound assignments
  if (LastChar == '+' || LastChar == '-' || LastChar == '*' || LastChar == '/' || LastChar == '%') {
    char Op = LastChar;
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
    
    // Handle increment operator
    if (Op == '+' && NextChar == '+') {
      LastChar = lex.consumeChar();
      return tok_inc; // ++
    }

    // Handle decrement operator
    if (Op == '-' && NextChar == '-') {
      LastChar = lex.consumeChar();
      return tok_dec; // --
    }

    // Handle arrow operator for pointer member access
    if (Op == '-' && NextChar == '>') {
      LastChar = lex.consumeChar();
      return tok_arrow; // ->
    }

    // Check for compound assignment
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      switch (Op) {
        case '+': return tok_plus_eq;   // +=
        case '-': return tok_minus_eq;  // -=
        case '*': return tok_mult_eq;   // *=
        case '/': return tok_div_eq;    // /=
        case '%': return tok_mod_eq;    // %=
      }
    } else {
      // Not a compound assignment, put the character back
      lex.unconsumeChar(NextChar);
    }
  }

  // Check for comparison operators
  if (LastChar == '=') {
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_eq;  // ==
    } else if (NextChar == '>') {
      LastChar = lex.consumeChar();
      return tok_lambda;  // =>
    } else {
      // Not == or =>, put the character back
      lex.unconsumeChar(NextChar);
    }
  }
  
  if (LastChar == '!') {
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_ne;  // !=
    } else {
      // Not !=, put the character back and return ! token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_not;  // !
    }
  }
  
  if (LastChar == '<') {
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_le;  // <=
    } else if (NextChar == '<') {
      NextChar = lex.consumeChar();
      if (NextChar == '=') {
        LastChar = lex.consumeChar();
        return tok_left_shift_eq;  // <<=
      } else {
        // Not <<=, put the character back and return << token
        lex.unconsumeChar(NextChar);
        LastChar = lex.consumeChar();
        return tok_left_shift;  // <<
      }
    } else {
      // Not <= or <<, put the character back and return < token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_lt;  // <
    }
  }
  
  if (LastChar == '>') {
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_ge;  // >=
    } else if (NextChar == '>') {
      NextChar = lex.consumeChar();
      if (NextChar == '=') {
        LastChar = lex.consumeChar();
        return tok_right_shift_eq;  // >>=
      } else {
        // Not >>=, put the character back and return >> token
        lex.unconsumeChar(NextChar);
        LastChar = lex.consumeChar();
        return tok_right_shift;  // >>
      }
    } else {
      // Not >= or >>, put the character back and return > token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_gt;  // >
    }
  }
  
  if (LastChar == '&') {
    int NextChar = lex.consumeChar();
    if (NextChar == '&') {
      LastChar = lex.consumeChar();
      return tok_and;  // &&
    } else if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_and_eq;  // &=
    } else {
      // Not && or &=, put the character back and return & token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_bitwise_and;  // &
    }
  }
  
  if (LastChar == '|') {
    int NextChar = lex.consumeChar();
    if (NextChar == '|') {
      LastChar = lex.consumeChar();
      return tok_or;  // ||
    } else if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_or_eq;  // |=
    } else {
      // Not || or |=, put the character back and return | token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_bitwise_or;  // |
    }
  }
  
  if (LastChar == '^') {
    int NextChar = lex.consumeChar();
    if (NextChar == '=') {
      LastChar = lex.consumeChar();
      return tok_xor_eq;  // ^=
    } else {
      // Not ^=, put the character back and return ^ token
      lex.unconsumeChar(NextChar);
      LastChar = lex.consumeChar();
      return tok_bitwise_xor;  // ^
    }
  }

  // Check for colon (type casting operator)
  if (LastChar == ':') {
    LastChar = lex.consumeChar();
    return tok_colon;
  }

  // Check for dot (member access).
  if (LastChar == '.') {
    LastChar = lex.consumeChar();
    return tok_dot;
  }

  // Check for @ (dereference/pointer type)
  if (LastChar == '@') {
    LastChar = lex.consumeChar();
    return tok_at;
  }

  // Check for # (address-of)
  if (LastChar == '#') {
    LastChar = lex.consumeChar();
    return tok_hash;
  }

  // Check for nullable related tokens ( ?, ?. , ?[, ??, ??= )
  if (LastChar == '?') {
    int NextChar = lex.consumeChar();

    if (NextChar == '.') {
      LastChar = lex.consumeChar();
      return tok_null_safe_access;
    }

    if (NextChar == '[') {
      LastChar = lex.consumeChar();
      return tok_null_array_access;
    }

    if (NextChar == '?') {
      int ThirdChar = lex.consumeChar();
      if (ThirdChar == '=') {
        LastChar = lex.consumeChar();
        return tok_null_coalescing_assign;
      }
      lex.unconsumeChar(ThirdChar);
      LastChar = lex.consumeChar();
      return tok_null_coalescing;
    }

    lex.unconsumeChar(NextChar);
    LastChar = lex.consumeChar();
    return tok_nullable;
  }

  // Check for newline.
  if (LastChar == '\n' || LastChar == '\r') {
    if (LastChar == '\r') {
      LastChar = lex.consumeChar();
      if (LastChar == '\n') {
        LastChar = lex.consumeChar();
      }
    } else {
      LastChar = lex.consumeChar();
    }
    return tok_newline;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = lex.consumeChar();
  return ThisChar;
}
