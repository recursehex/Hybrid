// EXPECT_DIAGNOSTIC: Type mismatch in '__hybrid_top_level' for op '+': cannot implicitly convert between 'unknown' and 'float'
// EXPECT_DIAGNOSTIC: Failed to generate IR for variable declaration
float ratio = 2.5
decimal one = 1
decimal value = one + ratio
