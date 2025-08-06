// Test complex struct functionality

struct Point {
    int x
    int y
    
    Point(int x, int y) {
        this.x = x
        this.y = y
    }
}

struct Rectangle {
    Point topLeft
    Point bottomRight
    
    Rectangle(Point tl, Point br) {
        this.topLeft = tl
        this.bottomRight = br
    }
}

// Create points
Point p1 = Point(0, 0)
Point p2 = Point(100, 50)

// Create a rectangle
Rectangle rect = Rectangle(p1, p2)

// Access nested fields
int width = rect.bottomRight.x - rect.topLeft.x
int height = rect.bottomRight.y - rect.topLeft.y

// Calculate area
width * height  // Should be 5000