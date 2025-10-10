// Test that boolean variable declarations return the correct value
bool flag1 = true
bool flag2 = false
bool flag3 = flag1
assert flag1
assert !flag2
assert flag3

// Test boolean variable declarations in functions
bool getBool()
{
    bool local = true
    assert local
    return local
}

bool getOtherBool()
{
    bool local = false
    assert !local
    return local
}

// Test with boolean operations
bool flag4 = true && false
bool flag5 = true || false
bool flag6 = !true
assert flag4 == false
assert flag5 == true
assert flag6 == false

int main()
{
    bool result1 = getBool()
    bool result2 = getOtherBool()
    assert result1
    assert !result2
    return 0
}