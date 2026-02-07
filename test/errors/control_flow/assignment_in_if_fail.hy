// EXPECT_DIAGNOSTIC: Cannot use assignment as condition in if statement (use == for comparison)
// EXPECT_DIAGNOSTIC: Failed to generate IR for if statement
int x = 5

// This should fail - assignment returns void, not boolean
if x = 0 {
    print(999)
}
