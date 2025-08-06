// Test subtraction of nested member accesses
struct Point {
    int x
    int y
    Point(int x, int y) { this.x = x; this.y = y }
}

struct Rectangle {
    Point topLeft
    Point bottomRight
    Rectangle(Point tl, Point br) {
        this.topLeft = tl
        this.bottomRight = br
    }
}

Point p1 = Point(0, 0)
Point p2 = Point(100, 50)
Rectangle rect = Rectangle(p1, p2)

// This is the problematic line from test_struct_complex.hy
int width = rect.bottomRight.x - rect.topLeft.x