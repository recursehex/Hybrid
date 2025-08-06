// Test direct nested access
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

// Direct nested access (THIS is what fails)
c.p.x