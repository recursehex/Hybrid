class Point
{
    int x
    int y

    Point()
    {
        this.x = 0
        this.y = 0
    }

    Point(int value)
    {
        this.x = value
        this.y = value
    }

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

Point origin = Point()
Point square = Point(5)
Point pair = Point(2, 3)

assert origin.x == 0
assert origin.y == 0
assert square.x == 5
assert square.y == 5
assert pair.x == 2
assert pair.y == 3