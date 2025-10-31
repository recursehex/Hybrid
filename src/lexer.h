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
  tok_class = -15,
  tok_this = -16,
  tok_switch = -17,
  tok_case = -18,
  tok_default = -19,
  tok_assert = -20,

  // basic types
  tok_int = -21,
  tok_float = -22,
  tok_double = -23,
  tok_char = -24,
  tok_void = -25,
  tok_bool = -26,
  tok_string = -27,

  // sized integer types
  tok_byte = -28,
  tok_short = -29,
  tok_long = -30,
  tok_sbyte = -31,
  tok_ushort = -32,
  tok_uint = -33,
  tok_ulong = -34,

  // sized character types
  tok_schar = -35,
  tok_lchar = -36,

  // primary literals
  tok_identifier = -37,     // e.g. foo, bar, baz
  tok_number = -38,         // e.g. 12345, 3.14
  tok_string_literal = -39, // e.g. "hello"
  tok_char_literal = -40,   // e.g. 'a'

  // boolean literals
  tok_true = -41,       // true
  tok_false = -42,      // false
  tok_null = -43,       // null

  // delimiters
  tok_newline = -44,

  // comparison operators
  tok_eq = -45,         // ==
  tok_ne = -46,         // !=
  tok_le = -47,         // <=
  tok_ge = -48,         // >=
  tok_lt = -49,         // <
  tok_gt = -50,         // >

  // boolean operators
  tok_and = -51,        // &&
  tok_or = -52,         // ||
  tok_not = -53,        // !

  // compound assignment operators
  tok_plus_eq = -54,    // +=
  tok_minus_eq = -55,   // -=
  tok_mult_eq = -56,    // *=
  tok_div_eq = -57,     // /=
  tok_mod_eq = -58,     // %=

  // bitwise operators
  tok_bitwise_and = -59,      // &
  tok_bitwise_or = -60,       // |
  tok_bitwise_xor = -61,      // ^
  tok_left_shift = -62,       // <<
  tok_right_shift = -63,      // >>

  // bitwise compound assignment operators
  tok_and_eq = -64,           // &=
  tok_or_eq = -65,            // |=
  tok_xor_eq = -66,           // ^=
  tok_left_shift_eq = -67,    // <<=
  tok_right_shift_eq = -68,   // >>=

  // increment/decrement
  tok_inc = -69,              // ++
  tok_dec = -70,              // --

  // type casting
  tok_colon = -71,            // :

  // member access
  tok_dot = -72,              // .

  // switch statement operators
  tok_lambda = -73,           // =>

  // access and storage modifiers
  tok_public = -74,
  tok_private = -75,
  tok_protected = -76,
  tok_static = -77,
  tok_const = -78,

  // pointer and unsafe operators
  tok_unsafe = -79,           // unsafe
  tok_at = -80,               // @ (dereference/pointer type)
  tok_hash = -81,             // \# (address-of)
  tok_arrow = -82,            // -> (pointer member access)

  // reference type
  tok_ref = -83,              // ref

  // interpolated string tokens
  tok_interpolated_string_start = -84,    // $"
  tok_interpolated_string_segment = -85,
  tok_interpolated_string_end = -86,      // "
  tok_interpolated_expr_start = -87,      // `
  tok_interpolated_expr_end = -88,        // `

  // nullity operators
  tok_nullable = -89,               // ? (nullable type)
  tok_null_safe_access = -90,       // ?. (null-safe member access)
  tok_null_array_access = -91,      // ?[ (null-safe array access)
  tok_null_coalescing = -92,        // ?? (null-coalescing operator)
  tok_null_coalescing_assign = -93, // ??= (null-coalescing assignment)

  // error token
  tok_error = -94             // error during lexing
};

// gettok - Return the next token from standard input.
int gettok();


#endif // LEXER_H
