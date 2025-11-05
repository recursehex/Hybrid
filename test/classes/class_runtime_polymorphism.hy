abstract class Shape
{
    protected int cached

    Shape()
    {
        this.cached = 0
    }

    virtual int Measure()
    {
        return this.cached
    }

    abstract string Kind()
}

interface Renderer
{
    int Value()
}

class Square inherits Shape, Renderer
{
    int side

    Square(int side)
    {
        this.side = side
        this.cached = side * side
    }

    override int Measure()
    {
        return base.Measure()
    }

    override string Kind()
    {
        return "Square"
    }

    int Value()
    {
        return 100 + this.side
    }
}

class Circle inherits Shape, Renderer
{
    int radius

    Circle(int radius)
    {
        this.radius = radius
        this.cached = radius * radius
    }

    override int Measure()
    {
        return base.Measure()
    }

    override string Kind()
    {
        return "Circle"
    }

    int Value()
    {
        return 200 + this.radius
    }
}

Shape squareShape = Square(4)
Shape circleShape = Circle(3)

assert squareShape.Measure() == 16
assert circleShape.Measure() == 9
assert squareShape.Kind() == "Square"
assert circleShape.Kind() == "Circle"

Renderer squareRenderer = Square(5)
Renderer circleRenderer = Circle(7)

assert squareRenderer.Value() == 105
assert circleRenderer.Value() == 207

Shape anotherSquare = squareShape
assert anotherSquare.Measure() == 16