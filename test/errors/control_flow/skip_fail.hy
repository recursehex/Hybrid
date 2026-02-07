// EXPECT_DIAGNOSTIC: 'skip' statement can only be used inside a loop
// Test that should fail - skip statement outside of a loop
int test_skip_fail()
{
    int x = 5
    
    if x > 0
    {
        skip  // Error: skip not within a loop
    }
    
    return x
}

int main() {
    return test_skip_fail()
}
