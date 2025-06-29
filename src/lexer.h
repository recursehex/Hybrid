#ifndef LEXER_H
#define LEXER_H

#include <string>

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
  tok_eof = -1,

  // commands
  tok_use = -2,
  tok_extern = -3,
  tok_return = -4,
  
  // control flow
  tok_for = -13,
  tok_in = -14,
  tok_if = -22,
  tok_else = -23,

  // types
  tok_int = -8,
  tok_float = -9,
  tok_double = -10,
  tok_char = -11,
  tok_void = -12,
  tok_bool = -15,
  tok_string = -18,

  // primary
  tok_identifier = -5,
  tok_number = -6,
  tok_string_literal = -19,
  tok_char_literal = -20,
  
  // boolean literals
  tok_true = -16,
  tok_false = -17,
  tok_null = -21,
  
  // delimiters
  tok_newline = -7,
  
  // comparison operators
  tok_eq = -24,        // ==
  tok_ne = -25,        // !=
  tok_le = -26,        // <=
  tok_ge = -27,        // >=
  tok_lt = -28,        // <
  tok_gt = -29,        // >
  
  // boolean operators
  tok_and = -30,       // &&
  tok_or = -31,        // ||
  tok_not = -32        // !
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern char CharVal;              // Filled in if tok_char_literal

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H