class Rectangle
{
    int width
    int height

    Rectangle(int width, int height)
    {
        this.width = width
        this.height = height
    }

    int Area()
    {
        return this.width * this.height
    }

    Rectangle Scale(int factor)
    {
        return Rectangle(this.width * factor, this.height * factor)
    }

    string this()
    {
        return $"Rectangle `this.width`x`this.height`"
    }
}

Rectangle unit = Rectangle(1, 1)
Rectangle rect = Rectangle(4, 3)
int area = rect.Area()
assert area == 12
Rectangle bigger = rect.Scale(2)
assert bigger.Area() == 48

// Accessors should be read-only from outside
int width = rect.width
assert width == 4

// Method calls and print should succeed
string label = rect.this()
print(rect)
print(bigger)
assert rect.this() == $"Rectangle `width`x3"