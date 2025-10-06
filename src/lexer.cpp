#include "lexer.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <iostream>

#include "llvm/Support/Error.h"

std::string IdentifierStr;      // Filled in if tok_identifier
NumericLiteral LexedNumericLiteral; // Filled in if tok_number
std::string StringVal;          // Filled in if tok_string_literal
uint32_t CharVal;               // Filled in if tok_char_literal (supports full Unicode)

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

static int lexPrefixedIntegerLiteral(int &LastChar,
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
  int CurrentChar = getchar();

  while (isValidDigit(CurrentChar)) {
    digits += static_cast<char>(CurrentChar);
    CurrentChar = getchar();
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

  if (!buildIntegerLiteral(digits, radix, spelling, LexedNumericLiteral))
    return tok_error;

  return tok_number;
}

} // namespace

/// gettok - Return the next token from standard input.
int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace except newlines.
  while (isspace(LastChar) && LastChar != '\n' && LastChar != '\r')
    LastChar = getchar();

  if (isalpha(LastChar) || LastChar == '_') { // identifier: [a-zA-Z_][a-zA-Z0-9_]*
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())) || LastChar == '_')
      IdentifierStr += LastChar;

    if (IdentifierStr == "use")
      return tok_use;
    if (IdentifierStr == "extern")
      return tok_extern;
    if (IdentifierStr == "return")
      return tok_return;
    if (IdentifierStr == "for")
      return tok_for;
    if (IdentifierStr == "in")
      return tok_in;
    if (IdentifierStr == "to")
      return tok_to;
    if (IdentifierStr == "by")
      return tok_by;
    if (IdentifierStr == "int")
      return tok_int;
    if (IdentifierStr == "float")
      return tok_float;
    if (IdentifierStr == "double")
      return tok_double;
    if (IdentifierStr == "char")
      return tok_char;
    if (IdentifierStr == "void")
      return tok_void;
    if (IdentifierStr == "bool")
      return tok_bool;
    if (IdentifierStr == "string")
      return tok_string;
    if (IdentifierStr == "byte")
      return tok_byte;
    if (IdentifierStr == "short")
      return tok_short;
    if (IdentifierStr == "long")
      return tok_long;
    if (IdentifierStr == "sbyte")
      return tok_sbyte;
    if (IdentifierStr == "ushort")
      return tok_ushort;
    if (IdentifierStr == "uint")
      return tok_uint;
    if (IdentifierStr == "ulong")
      return tok_ulong;
    if (IdentifierStr == "schar")
      return tok_schar;
    if (IdentifierStr == "lchar")
      return tok_lchar;
    if (IdentifierStr == "true")
      return tok_true;
    if (IdentifierStr == "false")
      return tok_false;
    if (IdentifierStr == "null")
      return tok_null;
    if (IdentifierStr == "if")
      return tok_if;
    if (IdentifierStr == "else")
      return tok_else;
    if (IdentifierStr == "while")
      return tok_while;
    if (IdentifierStr == "break")
      return tok_break;
    if (IdentifierStr == "skip")
      return tok_skip;
    if (IdentifierStr == "struct")
      return tok_struct;
    if (IdentifierStr == "this")
      return tok_this;
    if (IdentifierStr == "switch")
      return tok_switch;
    if (IdentifierStr == "case")
      return tok_case;
    if (IdentifierStr == "default")
      return tok_default;
    if (IdentifierStr == "assert")
      return tok_assert;
    if (IdentifierStr == "unsafe")
      return tok_unsafe;
    if (IdentifierStr == "ref")
      return tok_ref;
    return tok_identifier;
  }

  if (isdigit(LastChar)) {
    if (LastChar == '0') {
      int NextChar = getchar();
      if (NextChar == 'b' || NextChar == 'B') {
        return lexPrefixedIntegerLiteral(LastChar, 2, static_cast<char>(NextChar));
      } else if (NextChar == 'o' || NextChar == 'O') {
        return lexPrefixedIntegerLiteral(LastChar, 8, static_cast<char>(NextChar));
      } else if (NextChar == 'x' || NextChar == 'X') {
        return lexPrefixedIntegerLiteral(LastChar, 16, static_cast<char>(NextChar));
      } else {
        ungetc(NextChar, stdin);
      }
    }

    std::string literal;
    std::string integerDigits;
    bool hasDecimal = false;
    bool hasExponent = false;

    while (isDecimalDigit(LastChar)) {
      literal += static_cast<char>(LastChar);
      integerDigits += static_cast<char>(LastChar);
      LastChar = getchar();
    }

    if (LastChar == '.') {
      hasDecimal = true;
      literal += '.';
      LastChar = getchar();
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = getchar();
      }
    }

    if (LastChar == 'e' || LastChar == 'E') {
      hasExponent = true;
      literal += static_cast<char>(LastChar);
      LastChar = getchar();
      if (LastChar == '+' || LastChar == '-') {
        literal += static_cast<char>(LastChar);
        LastChar = getchar();
      }
      if (!isDecimalDigit(LastChar)) {
        std::cerr << "Error: Invalid exponent in floating-point literal '" << literal << "'\n";
        return tok_error;
      }
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = getchar();
      }
    }

    if (!hasDecimal && !hasExponent) {
      if (!buildIntegerLiteral(integerDigits, 10, literal, LexedNumericLiteral))
        return tok_error;
      return tok_number;
    }

    if (!buildFloatingLiteral(literal, hasDecimal, hasExponent, LexedNumericLiteral))
      return tok_error;
    return tok_number;
  }

  if (LastChar == '.') {
    int NextChar = getchar();
    if (isdigit(NextChar)) {
      std::string literal = ".";
      literal += static_cast<char>(NextChar);
      bool hasExponent = false;
      LastChar = getchar();
      while (isDecimalDigit(LastChar)) {
        literal += static_cast<char>(LastChar);
        LastChar = getchar();
      }

      if (LastChar == 'e' || LastChar == 'E') {
        hasExponent = true;
        literal += static_cast<char>(LastChar);
        LastChar = getchar();
        if (LastChar == '+' || LastChar == '-') {
          literal += static_cast<char>(LastChar);
          LastChar = getchar();
        }
        if (!isDecimalDigit(LastChar)) {
          std::cerr << "Error: Invalid exponent in floating-point literal '" << literal << "'\n";
          return tok_error;
        }
        while (isDecimalDigit(LastChar)) {
          literal += static_cast<char>(LastChar);
          LastChar = getchar();
        }
      }

      if (!buildFloatingLiteral(literal, true, hasExponent, LexedNumericLiteral))
        return tok_error;
      return tok_number;
    } else {
      ungetc(NextChar, stdin);
    }
  }

  if (LastChar == '"') { // String literal: "..."
    StringVal = "";
    LastChar = getchar();
    while (LastChar != '"' && LastChar != EOF) {
      if (LastChar == '\\') {
        // Handle escape sequences
        LastChar = getchar();
        switch (LastChar) {
          case 'n': StringVal += '\n'; break;
          case 't': StringVal += '\t'; break;
          case 'r': StringVal += '\r'; break;
          case '\\': StringVal += '\\'; break;
          case '"': StringVal += '"'; break;
          case 'u': { // Unicode escape: \uXXXX
            uint32_t unicode = 0;
            for (int i = 0; i < 4; i++) {
              LastChar = getchar();
              if (LastChar >= '0' && LastChar <= '9') {
                unicode = (unicode << 4) | (LastChar - '0');
              } else if (LastChar >= 'a' && LastChar <= 'f') {
                unicode = (unicode << 4) | (LastChar - 'a' + 10);
              } else if (LastChar >= 'A' && LastChar <= 'F') {
                unicode = (unicode << 4) | (LastChar - 'A' + 10);
              } else {
                StringVal += '?'; // Invalid escape
                goto string_continue;
              }
            }
            // Convert Unicode codepoint to UTF-8
            if (unicode <= 0x7F) {
              StringVal += static_cast<char>(unicode);
            } else if (unicode <= 0x7FF) {
              StringVal += static_cast<char>(0xC0 | (unicode >> 6));
              StringVal += static_cast<char>(0x80 | (unicode & 0x3F));
            } else {
              StringVal += static_cast<char>(0xE0 | (unicode >> 12));
              StringVal += static_cast<char>(0x80 | ((unicode >> 6) & 0x3F));
              StringVal += static_cast<char>(0x80 | (unicode & 0x3F));
            }
            break;
          }
          default: StringVal += LastChar; break;
        }
      } else {
        StringVal += LastChar;
      }
string_continue:
      LastChar = getchar();
    }
    if (LastChar == '"') {
      LastChar = getchar(); // eat closing "
      return tok_string_literal;
    }
  }

  if (LastChar == '\'') { // Character literal: '.'
    LastChar = getchar();
    if (LastChar == EOF) {
      return LastChar; // Error case - EOF in character literal
    }
    
    if (LastChar == '\\') {
      // Handle escape sequences
      LastChar = getchar();
      switch (LastChar) {
        case 'n': CharVal = '\n'; break;
        case 't': CharVal = '\t'; break;
        case 'r': CharVal = '\r'; break;
        case '\\': CharVal = '\\'; break;
        case '\'': CharVal = '\''; break;
        case '0': CharVal = '\0'; break;
        case 'u': { // Unicode escape: \uXXXX (16-bit)
          uint16_t unicode = 0;
          for (int i = 0; i < 4; i++) {
            LastChar = getchar();
            if (LastChar >= '0' && LastChar <= '9') {
              unicode = (unicode << 4) | (LastChar - '0');
            } else if (LastChar >= 'a' && LastChar <= 'f') {
              unicode = (unicode << 4) | (LastChar - 'a' + 10);
            } else if (LastChar >= 'A' && LastChar <= 'F') {
              unicode = (unicode << 4) | (LastChar - 'A' + 10);
            } else {
              // Invalid Unicode escape
              CharVal = '?';
              goto char_literal_end;
            }
          }
          // Validate Unicode code point: reject UTF-16 surrogates (U+D800 to U+DFFF)
          if (unicode >= 0xD800 && unicode <= 0xDFFF) {
            fprintf(stderr, "Error: Invalid Unicode code point U+%04X (UTF-16 surrogate)\n", unicode);
            CharVal = '?';
          } else {
            CharVal = unicode;
          }
          break;
        }
        case 'U': { // Unicode escape: \UXXXXXXXX (32-bit)
          uint32_t unicode = 0;
          for (int i = 0; i < 8; i++) {
            LastChar = getchar();
            if (LastChar >= '0' && LastChar <= '9') {
              unicode = (unicode << 4) | (LastChar - '0');
            } else if (LastChar >= 'a' && LastChar <= 'f') {
              unicode = (unicode << 4) | (LastChar - 'a' + 10);
            } else if (LastChar >= 'A' && LastChar <= 'F') {
              unicode = (unicode << 4) | (LastChar - 'A' + 10);
            } else {
              // Invalid Unicode escape
              CharVal = '?';
              goto char_literal_end;
            }
          }
          // Validate Unicode code point
          // Valid range: U+0000 to U+10FFFF, excluding surrogates U+D800 to U+DFFF
          if (unicode > 0x10FFFF || (unicode >= 0xD800 && unicode <= 0xDFFF)) {
            fprintf(stderr, "Error: Invalid Unicode code point U+%08X\n", unicode);
            CharVal = '?';
          } else {
            CharVal = unicode;
          }
          break;
        }
        default: CharVal = LastChar; break;
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
        CharVal = '?';
        goto char_literal_end;
      }
      
      for (int i = 0; i < bytes_to_read; i++) {
        LastChar = getchar();
        if ((LastChar & 0xC0) != 0x80) {
          // Invalid UTF-8 continuation byte
          CharVal = '?';
          goto char_literal_end;
        }
        unicode = (unicode << 6) | (LastChar & 0x3F);
      }
      
      CharVal = unicode;
    } else {
      CharVal = LastChar;
    }
    
char_literal_end:
    LastChar = getchar();
    if (LastChar == '\'') {
      LastChar = getchar(); // eat closing '
      return tok_char_literal;
    }
    // Error case - missing closing quote, but continue parsing
  }

  // Check for arithmetic operators and their compound assignments
  if (LastChar == '+' || LastChar == '-' || LastChar == '*' || LastChar == '/' || LastChar == '%') {
    char Op = LastChar;
    int NextChar = getchar();
    
    // Handle // comment special case
    if (Op == '/' && NextChar == '/') {
      // Comment until end of line.
      do
        LastChar = getchar();
      while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    }
    
    // Handle increment operator
    if (Op == '+' && NextChar == '+') {
      LastChar = getchar();
      return tok_inc; // ++
    }

    // Handle decrement operator
    if (Op == '-' && NextChar == '-') {
      LastChar = getchar();
      return tok_dec; // --
    }

    // Handle arrow operator for pointer member access
    if (Op == '-' && NextChar == '>') {
      LastChar = getchar();
      return tok_arrow; // ->
    }

    // Check for compound assignment
    if (NextChar == '=') {
      LastChar = getchar();
      switch (Op) {
        case '+': return tok_plus_eq;   // +=
        case '-': return tok_minus_eq;  // -=
        case '*': return tok_mult_eq;   // *=
        case '/': return tok_div_eq;    // /=
        case '%': return tok_mod_eq;    // %=
      }
    } else {
      // Not a compound assignment, put the character back
      ungetc(NextChar, stdin);
    }
  }

  // Check for comparison operators
  if (LastChar == '=') {
    int NextChar = getchar();
    if (NextChar == '=') {
      LastChar = getchar();
      return tok_eq;  // ==
    } else if (NextChar == '>') {
      LastChar = getchar();
      return tok_lambda;  // =>
    } else {
      // Not == or =>, put the character back
      ungetc(NextChar, stdin);
    }
  }
  
  if (LastChar == '!') {
    int NextChar = getchar();
    if (NextChar == '=') {
      LastChar = getchar();
      return tok_ne;  // !=
    } else {
      // Not !=, put the character back and return ! token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_not;  // !
    }
  }
  
  if (LastChar == '<') {
    int NextChar = getchar();
    if (NextChar == '=') {
      LastChar = getchar();
      return tok_le;  // <=
    } else if (NextChar == '<') {
      NextChar = getchar();
      if (NextChar == '=') {
        LastChar = getchar();
        return tok_left_shift_eq;  // <<=
      } else {
        // Not <<=, put the character back and return << token
        ungetc(NextChar, stdin);
        LastChar = getchar();
        return tok_left_shift;  // <<
      }
    } else {
      // Not <= or <<, put the character back and return < token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_lt;  // <
    }
  }
  
  if (LastChar == '>') {
    int NextChar = getchar();
    if (NextChar == '=') {
      LastChar = getchar();
      return tok_ge;  // >=
    } else if (NextChar == '>') {
      NextChar = getchar();
      if (NextChar == '=') {
        LastChar = getchar();
        return tok_right_shift_eq;  // >>=
      } else {
        // Not >>=, put the character back and return >> token
        ungetc(NextChar, stdin);
        LastChar = getchar();
        return tok_right_shift;  // >>
      }
    } else {
      // Not >= or >>, put the character back and return > token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_gt;  // >
    }
  }
  
  if (LastChar == '&') {
    int NextChar = getchar();
    if (NextChar == '&') {
      LastChar = getchar();
      return tok_and;  // &&
    } else if (NextChar == '=') {
      LastChar = getchar();
      return tok_and_eq;  // &=
    } else {
      // Not && or &=, put the character back and return & token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_bitwise_and;  // &
    }
  }
  
  if (LastChar == '|') {
    int NextChar = getchar();
    if (NextChar == '|') {
      LastChar = getchar();
      return tok_or;  // ||
    } else if (NextChar == '=') {
      LastChar = getchar();
      return tok_or_eq;  // |=
    } else {
      // Not || or |=, put the character back and return | token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_bitwise_or;  // |
    }
  }
  
  if (LastChar == '^') {
    int NextChar = getchar();
    if (NextChar == '=') {
      LastChar = getchar();
      return tok_xor_eq;  // ^=
    } else {
      // Not ^=, put the character back and return ^ token
      ungetc(NextChar, stdin);
      LastChar = getchar();
      return tok_bitwise_xor;  // ^
    }
  }

  // Check for colon (type casting operator)
  if (LastChar == ':') {
    LastChar = getchar();
    return tok_colon;
  }

  // Check for dot (member access).
  if (LastChar == '.') {
    LastChar = getchar();
    return tok_dot;
  }

  // Check for @ (dereference/pointer type)
  if (LastChar == '@') {
    LastChar = getchar();
    return tok_at;
  }

  // Check for # (address-of)
  if (LastChar == '#') {
    LastChar = getchar();
    return tok_hash;
  }

  // Check for newline.
  if (LastChar == '\n' || LastChar == '\r') {
    if (LastChar == '\r') {
      LastChar = getchar();
      if (LastChar == '\n') {
        LastChar = getchar();
      }
    } else {
      LastChar = getchar();
    }
    return tok_newline;
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}
