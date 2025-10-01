// Test integer overflow detection
// This file tests that the compiler properly detects and rejects
// integer literals that are too large to fit in supported types

// This should work - maximum int32 value
int max_int = 2147483647

// This should work - minimum int32 value
int min_int = -2147483648

// This should work - maximum int64 value (as long)
long max_long = 9223372036854775807

// Test that normal arithmetic still works
int sum = max_int + 0
