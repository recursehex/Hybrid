// Test cases for if-else statements and comparison operators

// If-else with integer comparison
int testFunc(int x)
{
    if x == 0
    {
        return 1
    }
    else
    {
        return 2
    }
}

// If-else-if chain
int grade(int score)
{
    if score >= 90
    {
        return 4  // A
    }
    else if score >= 80
    {
        return 3  // B
    }
    else if score >= 70
    {
        return 2  // C
    }
    else
    {
        return 1  // F
    }
}

// Testing all comparison operators
int compareTest(int a, int b)
{
    if a == b
    {
        return 0
    }
    if a != b
    {
        return 1
    }
    if a < b
    {
        return 2
    }
    if a > b
    {
        return 3
    }
    if a <= b
    {
        return 4
    }
    if a >= b
    {
        return 5
    }
    return -1
}

// Float comparison
double floatCompare(double x, double y)
{
    if x == y
    {
        return 1.0
    }
    else if x > y
    {
        return 2.0
    }
    else
    {
        return 0.0
    }
}

// Character comparison
bool charTest(char ch)
{
    if ch == 'a'
    {
        return true
    }
    else if ch == 'b'
    {
        return false
    }
    else
    {
        return true
    }
}

// Boolean logic in conditions
int boolTest(bool flag1, bool flag2)
{
    if flag1 == true
    {
        if flag2 == false
        {
            return 1
        }
        else
        {
            return 2
        }
    }
    else
    {
        return 0
    }
}

// Nested if statements
int nestedTest(int x, int y)
{
    if x > 0
    {
        if y > 0
        {
            return 1
        }
        else
        {
            return 2
        }
    }
    else
    {
        if y > 0
        {
            return 3
        }
        else
        {
            return 4
        }
    }
}

// Main function to test all control flow with assertions
int main()
{
    // Test basic if-else
    assert testFunc(0) == 1
    assert testFunc(5) == 2

    // Test if-else-if chain (grading function)
    assert grade(95) == 4  // A grade
    assert grade(85) == 3  // B grade
    assert grade(75) == 2  // C grade
    assert grade(65) == 1  // F grade

    // Test all comparison operators
    assert compareTest(5, 5) == 0   // Equal case (a == b)
    assert compareTest(5, 3) == 1   // Not equal case (a != b)
    assert compareTest(3, 5) == 2   // Less than case (a < b)
    assert compareTest(7, 5) == 3   // Greater than case (a > b)
    assert compareTest(5, 7) == 4   // Less or equal case (a <= b)
    assert compareTest(8, 5) == 5   // Greater or equal case (a >= b)

    // Test float comparisons
    assert int: floatCompare(5.0, 5.0) == 1  // Equal floats
    assert int: floatCompare(7.5, 5.0) == 2  // Greater float
    assert int: floatCompare(3.0, 5.0) == 0  // Lesser float

    // Test character comparisons
    assert charTest('a') == true
    assert charTest('b') == false
    assert charTest('x') == true  // Default case

    // Test boolean logic
    assert boolTest(true, false) == 1
    assert boolTest(true, true) == 2
    assert boolTest(false, true) == 0
    assert boolTest(false, false) == 0

    // Test nested if statements (quadrant logic)
    assert nestedTest(5, 10) == 1   // Both positive (quadrant I)
    assert nestedTest(5, -10) == 2  // x positive, y negative (quadrant IV)
    assert nestedTest(-5, 10) == 3  // x negative, y positive (quadrant II)
    assert nestedTest(-5, -10) == 4 // Both negative (quadrant III)

    return 0
}