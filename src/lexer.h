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
  tok_use = -2,             // import statement
  tok_extern = -3,          // external function declaration
  tok_return = -4,          // return statement
  
  // control flow
  tok_for = -5,             // for loop
  tok_in = -6,              // in keyword for foreach loops
  tok_to = -7,              // to keyword for for loops
  tok_by = -8,              // by keyword for step size in for loops
  tok_if = -9,              // if statement
  tok_else = -10,           // else statement
  tok_while = -11,          // while loop
  tok_break = -12,          // break statement
  tok_skip = -13,           // continue statement
  tok_struct = -14,         // struct definition
  tok_class = -15,          // class definition
  tok_this = -16,           // this pointer/reference
  tok_switch = -17,         // switch statement or expression
  tok_case = -18,           // case in switch
  tok_default = -19,        // default case in switch
  tok_assert = -20,         // assert statement

  // basic types
  tok_int = -21,            // int (32-bit signed integer)
  tok_float = -22,          // float (32-bit floating point)
  tok_double = -23,         // double (64-bit floating point)
  tok_char = -24,           // char (16-bit Unicode character)
  tok_void = -25,           // void (no type)
  tok_bool = -26,           // bool (true/false)
  tok_string = -27,         // string (array of chars with int length)

  // sized integer types
  tok_byte = -28,           // byte (8-bit unsigned integer)
  tok_short = -29,          // short (16-bit signed integer)
  tok_long = -30,           // long (64-bit signed integer)
  tok_sbyte = -31,          // sbyte (8-bit signed integer)
  tok_ushort = -32,         // ushort (16-bit unsigned integer)
  tok_uint = -33,           // uint (32-bit unsigned integer)
  tok_ulong = -34,          // ulong (64-bit unsigned integer)

  // sized character types
  tok_schar = -35,          // schar (8-bit character)
  tok_lchar = -36,          // lchar (32-bit character)

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
  tok_newline = -44,    // \n

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
  tok_public = -74,           // public
  tok_private = -75,          // private
  tok_protected = -76,        // protected
  tok_static = -77,           // static
  tok_const = -78,            // const

  // inheritance & polymorphism
  tok_inherits = -79,         // inherits
  tok_abstract = -80,         // abstract
  tok_interface = -81,        // interface
  tok_virtual = -82,          // virtual
  tok_override = -83,         // override
  tok_base = -84,             // base

  // pointer and unsafe operators
  tok_unsafe = -85,           // unsafe
  tok_at = -86,               // @ (dereference/pointer type)
  tok_hash = -87,             // \# (address-of)
  tok_arrow = -88,            // -> (pointer member access)

  // reference type
  tok_ref = -89,              // ref

  // interpolated string tokens
  tok_interpolated_string_start = -90,    // $"
  tok_interpolated_string_segment = -91,
  tok_interpolated_string_end = -92,      // "
  tok_interpolated_expr_start = -93,      // `
  tok_interpolated_expr_end = -94,        // `

  // nullity operators
  tok_nullable = -95,               // ? (nullable type)
  tok_null_safe_access = -96,       // ?. (null-safe member access)
  tok_null_array_access = -97,      // ?[ (null-safe array access)
  tok_null_coalescing = -98,        // ?? (null-coalescing operator)
  tok_null_coalescing_assign = -99, // ??= (null-coalescing assignment)

  // ARC / ownership keywords
  tok_weak = -101,             // weak ownership qualifier or smart pointer
  tok_unowned = -102,          // unowned ownership qualifier
  tok_unique = -103,           // unique<T> smart pointer helper
  tok_shared = -104,           // shared<T> smart pointer helper
  tok_autoreleasepool = -105,  // @autoreleasepool
  tok_new = -106,              // new keyword (reserved)
  tok_free = -107,             // free keyword (reserved)

  // error token
  tok_error = -100            // error during lexing
};

// gettok - Return the next token from standard input.
int gettok();


#endif // LEXER_H
