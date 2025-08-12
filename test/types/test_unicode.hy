// Test Unicode support in Hybrid

// ASCII characters (fit in all char types)
char ascii = 'A'
schar ascii_s = 'B'
lchar ascii_l = 'C'

// Unicode escapes (16-bit)
char omega = '\u03a9'       // Greek capital omega (Ω)
char euro = '\u20ac'        // Euro sign (€)
char chinese = '\u4e2d'     // Chinese character 中

// Direct UTF-8 input
char pi_symbol = 'π'        // Greek pi
char copyright = '©'        // Copyright symbol

// 32-bit Unicode (emoji - requires lchar)
lchar emoji_smile = '😀'    // U+1F600
lchar emoji_star = '⭐'     // U+2B50

// Unicode in strings
string hello_world = "Hello, 世界!"  // English + Chinese
string mixed = "αβγ δεζ"            // Greek letters
string unicode_escape = "Test: \u03a9\u20ac"  // Using escapes

// Test character arrays
char[] symbols = ['€', '£', '¥', '₹']
char[] greek = ['α', 'β', 'γ', 'δ']

// Function with Unicode parameters
int getUnicodeValue(char c) {
    return 1  // Placeholder
}

// Test expressions with Unicode
int omega_value = getUnicodeValue(omega)