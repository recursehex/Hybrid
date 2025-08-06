// Test inline nested member access in expression
struct Point {
    int x
    Point(int x) { this.x = x }
}

struct Container {
    Point p
    Container(Point p) { this.p = p }
}

Point pt = Point(10)
Container c = Container(pt)

// Test inline expression with nested access
int result = c.p.x + 5