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
  tok_switch = -16,
  tok_case = -17,
  tok_default = -18,

  // types
  tok_int = -19,
  tok_float = -20,
  tok_double = -21,
  tok_char = -22,
  tok_void = -23,
  tok_bool = -24,
  tok_string = -25,
  
  // sized integer types
  tok_byte = -26,
  tok_short = -27,
  tok_long = -28,
  tok_sbyte = -29,
  tok_ushort = -30,
  tok_uint = -31,
  tok_ulong = -32,
  
  // sized character types
  tok_schar = -33,
  tok_lchar = -34,

  // primary
  tok_identifier = -35,
  tok_number = -36,
  tok_string_literal = -37,
  tok_char_literal = -38,
  
  // boolean literals
  tok_true = -39,
  tok_false = -40,
  tok_null = -41,
  
  // delimiters
  tok_newline = -42,
  
  // comparison operators
  tok_eq = -43,        // ==
  tok_ne = -44,        // !=
  tok_le = -45,        // <=
  tok_ge = -46,        // >=
  tok_lt = -47,        // <
  tok_gt = -48,        // >
  
  // boolean operators
  tok_and = -49,       // &&
  tok_or = -50,        // ||
  tok_not = -51,       // !
  
  // compound assignment operators
  tok_plus_eq = -52,   // +=
  tok_minus_eq = -53,  // -=
  tok_mult_eq = -54,   // *=
  tok_div_eq = -55,    // /=
  tok_mod_eq = -56,    // %=
  
  // bitwise operators
  tok_bitwise_and = -57,     // &
  tok_bitwise_or = -58,      // |
  tok_bitwise_xor = -59,     // ^
  tok_left_shift = -60,      // <<
  tok_right_shift = -61,     // >>
  
  // bitwise compound assignment operators
  tok_and_eq = -62,          // &=
  tok_or_eq = -63,           // |=
  tok_xor_eq = -64,          // ^=
  tok_left_shift_eq = -65,   // <<=
  tok_right_shift_eq = -66,  // >>=

  // increment/decrement
  tok_inc = -67,             // ++
  tok_dec = -68,             // --
  
  // type casting
  tok_colon = -69,           // :
  
  // member access
  tok_dot = -70,              // .
  
  // switch statement operators
  tok_lambda = -71           // =>
};

extern std::string IdentifierStr; // Filled in if tok_identifier
extern double NumVal;             // Filled in if tok_number
extern std::string StringVal;     // Filled in if tok_string_literal
extern uint32_t CharVal;          // Filled in if tok_char_literal (supports full Unicode)

// gettok - Return the next token from standard input.
int gettok();

#endif // LEXER_H