// Test with parentheses
struct Point {
    int x
    Point(int x) { this.x = x }
}

struct Rectangle {
    Point topLeft
    Point bottomRight
    Rectangle(Point tl, Point br) {
        this.topLeft = tl
        this.bottomRight = br
    }
}

Point p1 = Point(0)
Point p2 = Point(100)
Rectangle rect = Rectangle(p1, p2)

// Try with parentheses
int width = (rect.bottomRight.x) - (rect.topLeft.x)