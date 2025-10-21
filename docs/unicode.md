# Unicode Support

## Overview

Hybrid provides comprehensive Unicode support across all character types and string literals. The language supports UTF-8 input, Unicode escape sequences, and proper handling of characters from the entire Unicode range.

## Character Types and Unicode Ranges

| Type | Size | Unicode Range | Description |
|------|------|--------------|-------------|
| `schar` | 8-bit | U+0000 - U+007F | ASCII characters only |
| `char` | 16-bit | U+0000 - U+FFFF | Basic Multilingual Plane (BMP) |
| `lchar` | 32-bit | U+0000 - U+10FFFF | Full Unicode range including emoji |

## Unicode Input Methods

### 1. Direct UTF-8 Input

You can directly type or paste Unicode characters in your source code:

```c
char euro = '€'         // Euro sign
char omega = 'Ω'        // Greek capital omega
char chinese = '中'     // Chinese character
lchar emoji = '😀'      // Emoji (requires lchar for > U+FFFF)
```

### 2. Unicode Escape Sequences

#### 16-bit Unicode Escapes (`\uXXXX`)

For characters in the Basic Multilingual Plane (U+0000 to U+FFFF):

```c
char omega = '\u03a9'       // Ω (Greek capital omega)
char euro = '\u20ac'        // € (Euro sign)
char chinese = '\u4e2d'     // 中 (Chinese character)
```

#### 32-bit Unicode Escapes (`\UXXXXXXXX`)

For characters beyond the BMP, including emoji and rare scripts:

```c
lchar emoji = '\U0001f600'      // 😀 (Grinning face)
lchar cuneiform = '\U00012000'  // 𒀀 (Cuneiform sign)
```

### 3. Escape Sequences in Strings

Strings support both direct UTF-8 and Unicode escapes:

```c
string greeting = "Hello, 世界!"                // Direct UTF-8
string escaped = "Greek: \u03a9\u03b1\u03b2"    // Using escapes
string mixed = "Symbol: € Price: ¥100"          // Mixed content
```

## Character Literal Rules

### Automatic Type Selection

When using character literals without explicit type declaration, the compiler automatically selects the appropriate size:

```c
'A'       // Fits in 16-bit, uses char
'€'       // Fits in 16-bit, uses char  
'😀'      // Requires 32-bit, uses lchar
```

Character literals also adapt to the surrounding context. When an expression expects an 8-bit `schar`/`byte`, 16-bit `char`/`short`, or 32-bit `lchar`/`int`, the literal is regenerated at that width so comparisons like `'A' == byteValue` and assignments such as `schar small = 'A'` work without manual casts. If the literal cannot fit the target width, the compiler falls back to the wider default and reports a type error.

### Type-Specific Assignment

You can explicitly specify the character type:

```c
schar ascii_only = 'A'      // 8-bit, ASCII only
char bmp_char = '中'        // 16-bit, BMP characters
lchar full_unicode = '🎨'   // 32-bit, full Unicode
```

## String Handling

### UTF-8 Encoding

Strings in Hybrid are stored as UTF-8 encoded byte sequences:

```c
string text = "Hello αβγ 你好 🌍"
// Stored as UTF-8 bytes internally
```

### String Escape Sequences

Strings support all standard escape sequences plus Unicode:

```c
string escaped = "Line 1\nLine 2\tTabbed"
string unicode = "Omega: \u03a9, Emoji: \U0001f600"
string mixed = "Quote: \" Backslash: \\ Unicode: \u2764"
```

## Arrays of Unicode Characters

Character arrays support Unicode elements:

```c
char[] symbols = ['€', '£', '¥', '₹']     // Currency symbols
char[] greek = ['α', 'β', 'γ', 'δ']       // Greek letters
lchar[] emoji = ['😀', '😎', '🎉', '🚀']  // Emoji array
```

## Best Practices

### 1. Choose the Right Character Type

- Use `schar` for ASCII-only data (memory efficient)
- Use `char` for most text processing (covers 99% of use cases)
- Use `lchar` only when needed for emoji or rare scripts

### 2. Consistent Encoding

- Source files should be saved as UTF-8
- Use Unicode escapes for non-visible characters
- Be consistent with direct UTF-8 vs escape sequences

### 3. Character Validation

```c
// Check if character is in ASCII range
bool isAscii(char c) {
    return c <= 127
}

// Check if character requires lchar
bool needsLchar(lchar c) {
    return c > 65535
}
```

## Examples

### International Greeting Program

```c
string[] greetings = [
    "Hello",           // English
    "你好",            // Chinese
    "こんにちは",      // Japanese
    "مرحبا",          // Arabic
    "Здравствуйте",   // Russian
    "🌍 World"        // With emoji
]

void printGreetings() {
    // Print each greeting
    // (Implementation depends on print function)
}
```

### Unicode Character Information

```c
struct CharInfo {
    lchar character
    string name
    int codepoint
}

CharInfo omega = CharInfo('Ω', "Greek Capital Omega", 937)
CharInfo euro = CharInfo('€', "Euro Sign", 8364)
CharInfo emoji = CharInfo('😀', "Grinning Face", 128512)
```

### Currency Converter

```c
struct Currency {
    char symbol
    string code
    float rate
}

Currency[] currencies = [
    Currency('$', "USD", 1.0),
    Currency('€', "EUR", 0.85),
    Currency('£', "GBP", 0.73),
    Currency('¥', "JPY", 110.0),
    Currency('₹', "INR", 74.5)
]
```

## Limitations

### Current Limitations

1. **No Normalization**: Unicode normalization is not performed
2. **No Collation**: String comparison is byte-based, not locale-aware
3. **No Grapheme Clusters**: Multi-codepoint characters (like flags) may not display correctly
4. **Limited Validation**: Invalid UTF-8 sequences may cause undefined behavior

### Future Enhancements

- Unicode normalization (NFC, NFD)
- Locale-aware string operations
- Grapheme cluster support
- Better validation and error handling
- Regular expressions with Unicode support

## Technical Details

### UTF-8 Decoding

The lexer performs UTF-8 decoding for character literals:

1. **1-byte sequences** (U+0000 - U+007F): Direct ASCII
2. **2-byte sequences** (U+0080 - U+07FF): 110xxxxx 10xxxxxx
3. **3-byte sequences** (U+0800 - U+FFFF): 1110xxxx 10xxxxxx 10xxxxxx
4. **4-byte sequences** (U+10000 - U+10FFFF): 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

### Storage in LLVM IR

- `schar`: `i8` (8-bit integer)
- `char`: `i16` (16-bit integer)
- `lchar`: `i32` (32-bit integer)
- Strings: `ptr` to UTF-8 byte sequence

### Escape Sequence Parsing

The lexer recognizes:
- `\uXXXX`: 4 hexadecimal digits for 16-bit Unicode
- `\UXXXXXXXX`: 8 hexadecimal digits for 32-bit Unicode
- Standard escapes: `\n`, `\t`, `\r`, `\\`, `\'`, `\"`

## Testing

Test files for Unicode support:
- `test/types/unicode.hy` - Comprehensive Unicode tests
- `test/types/char.hy` - Character type tests