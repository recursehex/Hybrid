// Comprehensive test for advanced struct functionality

// Define basic structs
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

struct Color
{
    int r
    int g
    int b
    
    Color(int r, int g, int b)
    {
        this.r = r
        this.g = g
        this.b = b
    }
}

// Test 1: Structs containing other structs
struct Rectangle
{
    Point topLeft
    Point bottomRight
    
    Rectangle(Point tl, Point br)
    {
        this.topLeft = tl
        this.bottomRight = br
    }
}

// Test 2: Nested struct instantiation
Point p1 = Point(0, 0)
Point p2 = Point(100, 50)
Rectangle rect = Rectangle(p1, p2)

// Test 3: Nested member access (single level)
int x1 = rect.topLeft.x         // 0
int y1 = rect.topLeft.y         // 0
int x2 = rect.bottomRight.x     // 100
int y2 = rect.bottomRight.y     // 50

// Test 4: Complex expressions with nested member access
int width = rect.bottomRight.x - rect.topLeft.x     // 100
int height = rect.bottomRight.y - rect.topLeft.y    // 50
int area = width * height                           // 5000

// Test 5: More complex struct composition
struct ColoredRectangle
{
    Rectangle bounds
    Color fillColor
    
    ColoredRectangle(Rectangle r, Color c)
    {
        this.bounds = r
        this.fillColor = c
    }
}

Color red = Color(255, 0, 0)
ColoredRectangle coloredRect = ColoredRectangle(rect, red)

// Test 6: Deep nested member access
int rectX = coloredRect.bounds.topLeft.x        // 0
int rectColor = coloredRect.fillColor.r         // 255

// Test 7: Using nested members in expressions
int diagonal = coloredRect.bounds.bottomRight.x + coloredRect.bounds.bottomRight.y      // 150

// Test 8: Struct used as function parameter (when functions are fully supported)
// This demonstrates the struct can be passed around
Point origin = Point(0, 0)

// Assertions to verify struct behavior
assert x1 == 0
assert y1 == 0
assert x2 == 100
assert y2 == 50
assert width == 100
assert height == 50
assert area == 5000
assert rectX == 0
assert rectColor == 255
assert diagonal == 150
assert coloredRect.bounds.bottomRight.y == 50