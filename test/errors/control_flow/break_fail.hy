// EXPECT_DIAGNOSTIC: 'break' statement can only be used inside a loop
// Test that should fail - break outside of loop

int test_break_outside_loop()
{
    int x = 10
    if x > 5
    {
        break  // Error: break not in a loop
    }
    return x
}

int main()
{
    return test_break_outside_loop()
}
