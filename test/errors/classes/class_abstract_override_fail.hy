// EXPECT_DIAGNOSTIC: Constructor for class 'BadSquare' must invoke base constructor of 'Shape'
// EXPECT_DIAGNOSTIC: Class 'BadSquare' must override abstract member 'Shape.Draw' defined in 'Shape'
abstract class Shape
{
    abstract void Draw()
}

class BadSquare inherits Shape
{
    BadSquare() {}
}

// expect: Class 'BadSquare' must override abstract member 'Shape.Draw' defined in 'Shape'
