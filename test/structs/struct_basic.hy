// Comprehensive test for basic struct functionality

// Test 1: Basic struct definition with constructor
struct Point
{
    int x
    int y
    
    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

// Test 2: Struct instantiation with assignment
Point p1 = Point(10, 20)

// Test 3: Member field access
int xVal = p1.x
int yVal = p1.y
assert xVal == 10
assert yVal == 20

// Test 4: Struct instantiation without assignment (standalone call)
Point(30, 40)

// Test 5: Multiple struct instances
Point p2 = Point(100, 200)
Point p3 = Point(-5, 15)

// Test 6: Using struct fields in expressions
int sum = p1.x + p1.y  // 30
int diff = p2.x - p2.y // -100
int product = p3.x * p3.y // -75
assert sum == 30
assert diff == -100
assert product == -75

// Test 7: Struct with single field
struct Counter
{
    int count
    
    Counter(int initial)
    {
        this.count = initial
    }
}

Counter c = Counter(42)
int val = c.count  // 42
assert val == 42