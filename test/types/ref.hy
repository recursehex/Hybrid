// Comprehensive test for ref keyword functionality

// Test 1: Basic ref variable linking
ref int a = 1
ref int b = a
b = 2
assert a == 2 && b == 2         // Both a and b should be 2 (linked)

// Test 2: Regular variable vs ref variable (no linking)
int x = 10
int y = x
y = 20
assert x == 10 && y == 20       // x should still be 10, y should be 20

// Test 3: Ref variable linking
ref int x2 = 10
ref int y2 = x2
y2 = 20
assert x2 == 20 && y2 == 20     // Both x2 and y2 should be 20 (linked)

// Test 4: Regular variable with ref linking
int num1 = 100
int num2 = ref num1
num2 = 200
assert num1 == 200 && num2 == 200   // Both num1 and num2 should be 200 (linked)

// Test 5: Ref function parameters - swap
void swap(ref int a, ref int b)
{
    int temp = a
    a = b
    b = temp
}

int val1 = 5
int val2 = 10
swap(ref val1, ref val2)
assert val1 == 10 && val2 == 5    // val1 should be 10, val2 should be 5

// Test 6: Ref function parameters - increment
void increment(ref int n)
{
    n++
}

int count = 0
increment(ref count)
assert count == 1   // count should be 1
increment(ref count)
assert count == 2   // count should be 2

// Test 7: Reading through ref variables
ref int original = 42
ref int linked = original
int copy = linked
assert copy == 42   // copy should be 42 (value copied, not linked)

// Test 8: Reference type returning function
ref int getRef()
{
    int local = 42
    return ref local
}

assert getRef() == 42