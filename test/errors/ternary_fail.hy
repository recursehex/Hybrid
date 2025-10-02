// Test file for ternary operator error cases

// Missing else clause
int invalid1 = 10 if true

// Invalid condition type (string should not be allowed as condition)
string text = "hello"
int invalid2 = 5 if text else 10

// Type mismatch between branches (no automatic promotion between string and int)
int invalid3 = "hello" if true else 42