// Test boolean type and literals
bool flag = true
bool active = false

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
}

// Test boolean expressions in function context
bool testBooleanOps()
{
    bool result = true
    result = false
    return result
}

int main()
{
    bool valid = isValid(5)
    testBooleans()
    bool final_result = testBooleanOps()
    return 0
}