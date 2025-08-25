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
  tok_to = -7,
  tok_by = -8,
  tok_if = -9,
  tok_else = -10,
  tok_while = -11,
  tok_break = -12,
  tok_skip = -13,
  tok_struct = -14,
  tok_this = -15,

  // types
  tok_int = -16,
  tok_float = -17,
  tok_double = -18,
  tok_char = -19,
  tok_void = -20,
  tok_bool = -21,
  tok_string = -22,
  
  // sized integer types
  tok_byte = -23,
  tok_short = -24,
  tok_long = -25,
  tok_sbyte = -26,
  tok_ushort = -27,
  tok_uint = -28,
  tok_ulong = -29,
  
  // sized character types
  tok_schar = -30,
  tok_lchar = -31,

  // primary
  tok_identifier = -32,
  tok_number = -33,
  tok_string_literal = -34,
  tok_char_literal = -35,
  
  // boolean literals
  tok_true = -36,
  tok_false = -37,
  tok_null = -38,
  
  // delimiters
  tok_newline = -39,
  
  // comparison operators
  tok_eq = -40,        // ==
  tok_ne = -41,        // !=
  tok_le = -42,        // <=
  tok_ge = -43,        // >=
  tok_lt = -44,        // <
  tok_gt = -45,        // >
  
  // boolean operators
  tok_and = -46,       // &&
  tok_or = -47,        // ||
  tok_not = -48,       // !
  
  // compound assignment operators
  tok_plus_eq = -49,   // +=
  tok_minus_eq = -50,  // -=
  tok_mult_eq = -51,   // *=
  tok_div_eq = -52,    // /=
  tok_mod_eq = -53,    // %=
  
  // bitwise operators
  tok_bitwise_and = -54,     // &
  tok_bitwise_or = -55,      // |
  tok_bitwise_xor = -56,     // ^
  tok_left_shift = -57,      // <<
  tok_right_shift = -58,     // >>
  
  // bitwise compound assignment operators
  tok_and_eq = -59,          // &=
  tok_or_eq = -60,           // |=
  tok_xor_eq = -61,          // ^=
  tok_left_shift_eq = -62,   // <<=
  tok_right_shift_eq = -63,  // >>=

  // increment/decrement
  tok_inc = -64,             // ++
  tok_dec = -65,             // --
  
  // type casting
  tok_colon = -66,           // :
  
  // member access
  tok_dot = -67              // .
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern uint32_t CharVal;          // Filled in if tok_char_literal (supports full Unicode)

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H