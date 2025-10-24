# Integer Overflow Protection

Hybrid prevents integer overflow via detection for literal values.

## Supported Integer Ranges

| Type | Signedness | Width | Range |
|------|------------|-------|-------|
| `byte` | Unsigned | 8-bit | 0 to 255 |
| `sbyte` | Signed | 8-bit | -128 to 127 |
| `short` | Signed | 16-bit | -32,768 to 32,767 |
| `ushort` | Unsigned | 16-bit | 0 to 65,535 |
| `int` | Signed | 32-bit | -2,147,483,648 to 2,147,483,647 |
| `uint` | Unsigned | 32-bit | 0 to 4,294,967,295 |
| `long` | Signed | 64-bit | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |
| `ulong` | Unsigned | 64-bit | 0 to 18,446,744,073,709,551,615 |

Literal checking uses these limits to reject out-of-range tokens before they reach parsing.

## Lexer-Level Overflow Detection

The lexer detects integer literals that exceed the maximum representable value:

```cs
// Valid literals - within range
int maxInt = 2147483647                 // OK - maximum i32 value
long maxLong = 9223372036854775807      // OK - maximum i64 value

// Invalid literals - overflow detected at tokenization
int overflow = 2147483648               // Error: exceeds 32-bit integer range
long overflow2 = 9223372036854775808    // Error: exceeds 64-bit integer range
```

Prefixed forms (`0x`, `0b`, `0o`) go through the exact same verification. For example:

```cs
ulong mask = 0x1FFFFFFFFFFFFFFFF        // Error: exceeds unsigned 64-bit range
```

## Compiler Diagnostics and Tests

When the lexer rejects a literal it prints the diagnostic emitted in `src/lexer.cpp`:

```
Error: Integer literal '0x1FFFFFFFFFFFFFFFF' exceeds maximum supported 64-bit range
```

The behavior is covered by `test/types/overflow.hy`, which compiles at the limits, and the failure-case regression in `test/errors/numeric_literal_overflow_fail.hy`.

## Overflow Detection Features

- **Early detection**: Overflow is caught during lexical analysis, before parsing
- **Precise error messages**: Clear indication of which value overflowed and the valid range
- **64-bit limit**: Maximum supported literal is `9,223,372,036,854,775,807` (2^63 - 1)
- **Negative numbers**: Handled correctly via unary negation (e.g. `-2147483648` for minimum i32)
- **Type-specific checking**: Range validation respects target type when assigning

## Current Limitations

- Unary negation is applied after lexing, so `-2147483648` is accepted by combining the `2147483648` token with a leading `-`, even though the positive literal would overflow on its own.
- Runtime arithmetic still follows LLVM's two's-complement semantics. The compiler halts only on overflowing literals; expressions such as `int x = 2147483647 + 1` currently wrap rather than trapping.