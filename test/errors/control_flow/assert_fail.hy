// EXPECT_DIAGNOSTIC: Assert condition evaluates to false at compile time
// Test assert statements that should fail at compilation time
int main() {
    // This should fail - assert with invalid expression syntax
    assert 1 == 2   // Invalid syntax: double equals operator

    return 0
}
