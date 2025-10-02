// Test that boolean variable declarations return the correct value
bool flag1 = true
bool flag2 = false
bool flag3 = flag1

// Test boolean variable declarations in functions
bool getBool()
{
    bool local = true
    return local
}

bool getOtherBool()
{
    bool local = false
    return local
}

// Test with boolean operations
bool flag4 = true && false
bool flag5 = true || false
bool flag6 = !true

int main()
{
    bool result1 = getBool()
    bool result2 = getOtherBool()
    return 0
}