// Test boolean type and literals
bool flag = true
bool active = false
assert flag == true
assert active == false

// Test in functions
bool isValid(int x)
{
    return x > 0
}

void testBooleans()
{
    bool a = true
    bool b = false
    bool c = a
    assert a == true
    assert b == false
    assert c == true
}

// Test boolean expressions in function context
bool testBooleanOps()
{
    bool result = true
    result = false
    assert result == false
    return result
}

int main()
{
    bool valid = isValid(5)
    assert valid == true
    assert isValid(-3) == false
    testBooleans()
    bool final_result = testBooleanOps()
    assert final_result == false
    return 0
}