#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <cstdint>

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
  tok_skip = -11,
  tok_struct = -94,
  tok_this = -95,

  // types
  tok_int = -12,
  tok_float = -13,
  tok_double = -14,
  tok_char = -15,
  tok_void = -16,
  tok_bool = -17,
  tok_string = -18,
  
  // sized integer types
  tok_byte = -85,
  tok_short = -86,
  tok_long = -87,
  tok_sbyte = -88,
  tok_ushort = -89,
  tok_uint = -90,
  tok_ulong = -91,
  
  // sized character types
  tok_schar = -92,
  tok_lchar = -93,

  // primary
  tok_identifier = -19,
  tok_number = -20,
  tok_string_literal = -21,
  tok_char_literal = -22,
  
  // boolean literals
  tok_true = -23,
  tok_false = -24,
  tok_null = -25,
  
  // delimiters
  tok_newline = -26,
  
  // comparison operators
  tok_eq = -27,        // ==
  tok_ne = -28,        // !=
  tok_le = -29,        // <=
  tok_ge = -30,        // >=
  tok_lt = -31,        // <
  tok_gt = -32,        // >
  
  // boolean operators
  tok_and = -33,       // &&
  tok_or = -34,        // ||
  tok_not = -35,       // !
  
  // compound assignment operators
  tok_plus_eq = -36,   // +=
  tok_minus_eq = -37,  // -=
  tok_mult_eq = -38,   // *=
  tok_div_eq = -39,    // /=
  tok_mod_eq = -40,    // %=
  
  // bitwise operators
  tok_bitwise_and = -41,     // &
  tok_bitwise_or = -42,      // |
  tok_bitwise_xor = -43,     // ^
  tok_left_shift = -44,      // <<
  tok_right_shift = -45,     // >>
  
  // bitwise compound assignment operators
  tok_and_eq = -46,          // &=
  tok_or_eq = -47,           // |=
  tok_xor_eq = -48,          // ^=
  tok_left_shift_eq = -49,   // <<=
  tok_right_shift_eq = -50,  // >>=

  // increment/decrement
  tok_inc = -51,             // ++
  tok_dec = -52,             // --
  
  // type casting
  tok_colon = -53,           // :
  
  // member access
  tok_dot = -54              // .
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern uint32_t CharVal;          // Filled in if tok_char_literal (supports full Unicode)

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H