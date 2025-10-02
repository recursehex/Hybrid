// Basic assert statement tests
int main() {
    // Test assert with simple boolean expression
    assert true
    assert 1 == 1
    assert 2 + 2 == 4

    // Test assert with variable comparisons
    int x = 10
    int y = 5
    assert x > y
    assert x != y
    assert x >= 10

    // Test assert with boolean operations
    bool flag = true
    assert flag
    assert flag && x > 0
    assert !false

    // Test assert with arithmetic expressions
    assert x + y == 15
    assert x - y == 5
    assert x * 2 == 20
    assert x / 2 == 5

    // Test assert with nested expressions
    assert (x > 0) && (y > 0)
    assert (x * y) == 50

    return 0
}