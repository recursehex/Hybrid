// Test simple member access

struct Point {
    int x
    
    Point(int x) {
        this.x = x
    }
}

Point p = Point(10)
int val = p.x  // This works