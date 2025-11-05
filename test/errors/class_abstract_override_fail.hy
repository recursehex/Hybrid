abstract class Shape
{
    abstract void Draw()
}

class BadSquare inherits Shape
{
    BadSquare() {}
}

// expect: Class 'BadSquare' must override abstract member 'Shape.Draw' defined in 'Shape'