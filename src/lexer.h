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
  tok_for = -5,
  tok_in = -6,
  tok_if = -7,
  tok_else = -8,
  tok_while = -9,

  // types
  tok_int = -10,
  tok_float = -11,
  tok_double = -12,
  tok_char = -13,
  tok_void = -14,
  tok_bool = -15,
  tok_string = -16,

  // primary
  tok_identifier = -17,
  tok_number = -18,
  tok_string_literal = -19,
  tok_char_literal = -20,
  
  // boolean literals
  tok_true = -21,
  tok_false = -22,
  tok_null = -23,
  
  // delimiters
  tok_newline = -24,
  
  // comparison operators
  tok_eq = -25,        // ==
  tok_ne = -26,        // !=
  tok_le = -27,        // <=
  tok_ge = -28,        // >=
  tok_lt = -29,        // <
  tok_gt = -30,        // >
  
  // boolean operators
  tok_and = -31,       // &&
  tok_or = -32,        // ||
  tok_not = -33        // !
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern char CharVal;              // Filled in if tok_char_literal

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H