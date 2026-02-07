// EXPECT_DIAGNOSTIC: Integer literal '99999999999999999999' exceeds maximum supported 64-bit range
// EXPECT_DIAGNOSTIC: Expected expression after '='
// Test integer overflow detection - should fail
// This file contains integer literals that exceed the maximum supported range

// This should fail - exceeds int64 maximum
int too_large = 99999999999999999999
