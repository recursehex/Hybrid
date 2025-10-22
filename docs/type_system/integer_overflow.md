# Integer Overflow Protection

Hybrid prevents integer overflow via detection for literal values.

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

## Overflow Detection Features

- **Early detection**: Overflow is caught during lexical analysis, before parsing
- **Precise error messages**: Clear indication of which value overflowed and the valid range
- **64-bit limit**: Maximum supported literal is `9,223,372,036,854,775,807` (2^63 - 1)
- **Negative numbers**: Handled correctly via unary negation (e.g. `-2147483648` for minimum i32)
- **Type-specific checking**: Range validation respects target type when assigning