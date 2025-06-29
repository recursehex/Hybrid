// Test cases for boolean operators (!, &&, ||)

// Test AND operator (&&)
bool testAnd(bool a, bool b) {
    if a && b {
        return true
    } else {
        return false
    }
}

// Test OR operator (||)
bool testOr(bool a, bool b) {
    if a || b {
        return true
    } else {
        return false
    }
}

// Test NOT operator (!)
bool testNot(bool flag) {
    if !flag {
        return true
    } else {
        return false
    }
}

// Test complex boolean expressions
bool testComplex(int x, int y, bool flag) {
    if x > 0 && y > 0 {
        return true
    }
    if x == 0 || y == 0 {
        return false
    }
    if !flag && x < y {
        return true
    }
    return false
}

// Test boolean operators with numbers
int testNumericBooleans(int x, int y) {
    if x && y {
        // Non-zero numbers are truthy
        return 1
    }
    if x || y {
        // At least one is non-zero
        return 2
    }
    if !x {
        // x is zero (falsy)
        return 3
    }
    return 0
}

// Test precedence: && has higher precedence than ||
bool testPrecedence(bool a, bool b, bool c) {
    // This should be interpreted as: a || (b && c)
    if a || b && c {
        return true
    } else {
        return false
    }
}

// Test with function calls
bool testWithFunctions(int x) {
    if x == 5 && x < 10 {
        return true
    }
    if x != 5 || x >= 10 {
        return false
    }
    return true
}

// Test nested boolean logic
bool testNested(bool a, bool b, bool c, bool d) {
    if (a && b) || (c && d) {
        return true
    }
    if !(a || b) && !(c || d) {
        return false
    }
    return true
}