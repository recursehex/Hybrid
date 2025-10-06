#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <cstdint>

#include "numeric_literal.h"

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
  tok_assert = -19,

  // basic types
  tok_int = -20,
  tok_float = -21,
  tok_double = -22,
  tok_char = -23,
  tok_void = -24,
  tok_bool = -25,
  tok_string = -26,

  // sized integer types
  tok_byte = -27,
  tok_short = -28,
  tok_long = -29,
  tok_sbyte = -30,
  tok_ushort = -31,
  tok_uint = -32,
  tok_ulong = -33,

  // sized character types
  tok_schar = -34,
  tok_lchar = -35,

  // primary literals
  tok_identifier = -36,
  tok_number = -37,
  tok_string_literal = -38,
  tok_char_literal = -39,

  // boolean literals
  tok_true = -40,
  tok_false = -41,
  tok_null = -42,

  // delimiters
  tok_newline = -43,

  // comparison operators
  tok_eq = -44,         // ==
  tok_ne = -45,         // !=
  tok_le = -46,         // <=
  tok_ge = -47,         // >=
  tok_lt = -48,         // <
  tok_gt = -49,         // >

  // boolean operators
  tok_and = -50,        // &&
  tok_or = -51,         // ||
  tok_not = -52,        // !

  // compound assignment operators
  tok_plus_eq = -53,    // +=
  tok_minus_eq = -54,   // -=
  tok_mult_eq = -55,    // *=
  tok_div_eq = -56,     // /=
  tok_mod_eq = -57,     // %=

  // bitwise operators
  tok_bitwise_and = -58,      // &
  tok_bitwise_or = -59,       // |
  tok_bitwise_xor = -60,      // ^
  tok_left_shift = -61,       // <<
  tok_right_shift = -62,      // >>

  // bitwise compound assignment operators
  tok_and_eq = -63,           // &=
  tok_or_eq = -64,            // |=
  tok_xor_eq = -65,           // ^=
  tok_left_shift_eq = -66,    // <<=
  tok_right_shift_eq = -67,   // >>=

  // increment/decrement
  tok_inc = -68,              // ++
  tok_dec = -69,              // --

  // type casting
  tok_colon = -70,            // :

  // member access
  tok_dot = -71,              // .

  // switch statement operators
  tok_lambda = -72,           // =>

  // pointer and unsafe operators
  tok_unsafe = -73,           // unsafe
  tok_at = -74,               // @ (dereference/pointer type)
  tok_hash = -75,             // \# (address-of)
  tok_arrow = -76,            // -> (pointer member access)

  // reference type
  tok_ref = -77,              // ref

  // error token
  tok_error = -78             // error during lexing
};

// gettok - Return the next token from standard input.
int gettok();


#endif // LEXER_H
