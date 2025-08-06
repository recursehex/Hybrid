// Test complete struct functionality

struct Point {
    int x
    int y
    
    Point(int x, int y) {
        this.x = x
        this.y = y
    }
}

// Create a Point instance
Point p = Point(10, 20)

// Access struct fields
int xVal = p.x
int yVal = p.y

// Test that fields were set correctly
xVal + yVal  // Should be 30