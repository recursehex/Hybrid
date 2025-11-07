// Test that all variables must be initialized
int x = 5
float y = 3.14
bool flag = true
char c = 'a'

// Validate global initializations
assert x == 5
assert flag == true
assert c == 'a'

// Test in functions
void testInit()
{
    int count = 0
    bool done = false
    float value = 1.5
    assert count == 0
    assert done == false
}

// Test initialization with expressions
int sum = 2 + 3
bool isGreater = 5 > 3
assert sum == 5
assert isGreater == true