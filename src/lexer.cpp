#include "lexer.h"
#include <cctype>
#include <cstdio>

std::string IdentifierStr; // Filled in if tok_identifier
double NumVal;             // Filled in if tok_number
std::string StringVal;     // Filled in if tok_string_literal
char CharVal;              // Filled in if tok_char_literal

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
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), nullptr);
    return tok_number;
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
          default: StringVal += LastChar; break;
        }
      } else {
        StringVal += LastChar;
      }
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
        default: CharVal = LastChar; break;
      }
    } else {
      CharVal = LastChar;
    }
    
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
    } else {
      // Not ==, put the character back
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
    } else {
      // Not <=, put the character back and return < token
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
    } else {
      // Not >=, put the character back and return > token
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
    } else {
      // Not &&, put the character back
      ungetc(NextChar, stdin);
    }
  }
  
  if (LastChar == '|') {
    int NextChar = getchar();
    if (NextChar == '|') {
      LastChar = getchar();
      return tok_or;  // ||
    } else {
      // Not ||, put the character back
      ungetc(NextChar, stdin);
    }
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