// Simplified test
struct Point {
    int x
    
    Point(int x) {
        this.x = x
    }
}

struct Container {
    Point p
    
    Container(Point p) {
        this.p = p
    }
}

// These should work
Point pt = Point(10)
Container c = Container(pt)

// Test member access step by step
Point p2 = c.p     // Get the Point from Container
int val = p2.x     // Get x from the Point