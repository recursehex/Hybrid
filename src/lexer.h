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
  tok_break = -10,

  // types
  tok_int = -11,
  tok_float = -12,
  tok_double = -13,
  tok_char = -14,
  tok_void = -15,
  tok_bool = -16,
  tok_string = -17,

  // primary
  tok_identifier = -18,
  tok_number = -19,
  tok_string_literal = -20,
  tok_char_literal = -21,
  
  // boolean literals
  tok_true = -22,
  tok_false = -23,
  tok_null = -24,
  
  // delimiters
  tok_newline = -25,
  
  // comparison operators
  tok_eq = -26,        // ==
  tok_ne = -27,        // !=
  tok_le = -28,        // <=
  tok_ge = -29,        // >=
  tok_lt = -30,        // <
  tok_gt = -31,        // >
  
  // boolean operators
  tok_and = -32,       // &&
  tok_or = -33,        // ||
  tok_not = -34,       // !
  
  // compound assignment operators
  tok_plus_eq = -35,   // +=
  tok_minus_eq = -36,  // -=
  tok_mult_eq = -37,   // *=
  tok_div_eq = -38,    // /=
  tok_mod_eq = -39     // %=
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern char CharVal;              // Filled in if tok_char_literal

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H