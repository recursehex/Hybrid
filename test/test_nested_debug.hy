// Debug nested member access

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

Point pt = Point(10)
Container c = Container(pt)

// This should work
int val = c.p.x