// Test cases for boolean operators (!, &&, ||)

// Test AND operator (&&)
bool testAnd(bool a, bool b)
{
    if a && b
    {
        return true
    }
    else
    {
        return false
    }
}

// Test OR operator (||)
bool testOr(bool a, bool b)
{
    if a || b
    {
        return true
    }
    else
    {
        return false
    }
}

// Test NOT operator (!)
bool testNot(bool flag)
{
    if !flag
    {
        return true
    }
    else
    {
        return false
    }
}

// Test complex boolean expressions
bool testComplex(int x, int y, bool flag)
{
    if x > 0 && y > 0
    {
        return true
    }
    if x == 0 || y == 0
    {
        return false
    }
    if !flag && x < y
    {
        return true
    }
    return false
}

// Test precedence: && has higher precedence than ||
bool testPrecedence(bool a, bool b, bool c)
{
    // This should be interpreted as: a || (b && c)
    if a || b && c
    {
        return true
    }
    else
    {
        return false
    }
}

// Test with function calls
bool testWithFunctions(int x)
{
    if x == 5 && x < 10
    {
        return true
    }
    if x != 5 || x >= 10
    {
        return false
    }
    return true
}

// Test nested boolean logic
bool testNested(bool a, bool b, bool c, bool d)
{
    if (a && b) || (c && d)
    {
        return true
    }
    if !(a || b) && !(c || d)
    {
        return false
    }
    return true
}

// Main function to test all boolean operations with assertions
int main()
{
    // Test AND operator
    assert testAnd(true, true) == true
    assert testAnd(true, false) == false
    assert testAnd(false, true) == false
    assert testAnd(false, false) == false

    // Test OR operator
    assert testOr(true, true) == true
    assert testOr(true, false) == true
    assert testOr(false, true) == true
    assert testOr(false, false) == false

    // Test NOT operator
    assert testNot(true) == false
    assert testNot(false) == true

    // Test complex boolean expressions
    assert testComplex(5, 10, false) == true   // x > 0 && y > 0
    assert testComplex(0, 5, false) == false   // x == 0 || y == 0
    assert testComplex(-5, 10, false) == true  // !flag && x < y

    // Test precedence (a || b && c should be a || (b && c))
    assert testPrecedence(true, false, false) == true   // true || (false && false) = true
    assert testPrecedence(false, true, true) == true    // false || (true && true) = true
    assert testPrecedence(false, false, true) == false  // false || (false && true) = false

    // Test with function calls
    assert testWithFunctions(5) == true    // 5 == 5 && 5 < 10
    assert testWithFunctions(15) == false  // 15 != 5 || 15 >= 10

    // Test nested boolean logic
    assert testNested(true, true, false, false) == true   // (true && true) || (false && false) = true
    assert testNested(false, false, false, false) == false // !(false || false) && !(false || false) = false

    return 0
}