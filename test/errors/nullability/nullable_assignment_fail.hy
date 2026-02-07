// EXPECT_DIAGNOSTIC: Cannot assign nullable value to non-nullable variable 'definiteText'
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
string? maybeText = null

// This should fail because maybeText is nullable
string definiteText = maybeText
