// Test loop context validation for break and skip statements
// This file demonstrates that break/skip statements are validated at parse time

// Test 1: break in top-level function (should fail)
void test_break_toplevel()
{
    break  // Error: 'break' statement can only be used inside a loop
}

// Test 2: skip in if statement (should fail)
void test_skip_in_if()
{
    if true
    {
        skip  // Error: 'skip' statement can only be used inside a loop
    }
}

// Test 3: break in nested if inside function (should fail)
void test_break_nested()
{
    int x = 5
    if x > 0
    {
        if x < 10
        {
            break  // Error: 'break' statement can only be used inside a loop
        }
    }
}
