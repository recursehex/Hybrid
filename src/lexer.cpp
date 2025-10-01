#include "lexer.h"
#include <cctype>
#include <cstdio>
#include <cerrno>
#include <cmath>
#include <cstdint>
#include <limits>
#include <iostream>

std::string IdentifierStr; // Filled in if tok_identifier
double NumVal;             // Filled in if tok_number
std::string StringVal;     // Filled in if tok_string_literal
uint32_t CharVal;          // Filled in if tok_char_literal (supports full Unicode)

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
    // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    // Parse the number and check for overflow
    errno = 0;
    char* endptr;
    NumVal = strtod(NumStr.c_str(), &endptr);

    // Check for overflow/underflow
    if (errno == ERANGE) {
      std::cerr << "Error: Number literal '" << NumStr << "' is out of range (overflow/underflow)\n";
      return tok_error;
    }

    // Check if entire string was parsed
    if (*endptr != '\0') {
      std::cerr << "Error: Invalid number format '" << NumStr << "'\n";
      return tok_error;
    }

    // Additional validation: Check if the number can fit in int64 range when it's a whole number
    // This prevents issues where strtod accepts very large integers that overflow when cast
    if (NumVal == floor(NumVal)) {
      // It's a whole number - verify it fits in int64 range
      // We use int64 as the largest integer type we support
      constexpr double INT64_MIN_D = static_cast<double>(INT64_MIN);
      constexpr double INT64_MAX_D = static_cast<double>(INT64_MAX);

      if (NumVal < INT64_MIN_D || NumVal > INT64_MAX_D) {
        std::cerr << "Error: Integer literal '" << NumStr
                  << "' exceeds maximum supported integer range (-9223372036854775808 to 9223372036854775807)\n";
        return tok_error;
      }
    }

    return tok_number;
  }
  
  // Check for decimal numbers starting with '.'
  if (LastChar == '.') {
    int NextChar = getchar();
    if (isdigit(NextChar)) {
      // It's a decimal number like .5
      std::string NumStr = ".";
      NumStr += NextChar;
      LastChar = getchar();
      while (isdigit(LastChar)) {
        NumStr += LastChar;
        LastChar = getchar();
      }

      // Parse with overflow checking
      errno = 0;
      char* endptr;
      NumVal = strtod(NumStr.c_str(), &endptr);

      if (errno == ERANGE) {
        std::cerr << "Error: Number literal '" << NumStr << "' is out of range (overflow/underflow)\n";
        return tok_error;
      }

      return tok_number;
    } else {
      // Not a number, put back the character and return '.'
      ungetc(NextChar, stdin);
      // Fall through to return '.' as a token
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
          CharVal = unicode;
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
          CharVal = unicode;
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