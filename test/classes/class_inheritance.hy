abstract class Shape
{
    protected int cachedArea

    Shape()
    {
        this.cachedArea = 0
    }

    virtual int Area()
    {
        return this.cachedArea
    }

    abstract void Draw()
}

interface Drawable
{
    void Draw()
}

class Square inherits Shape, Drawable
{
    int side

    Square(int side)
    {
        this.side = side
        this.cachedArea = this.side * this.side
    }

    void Resize(int newSide)
    {
        this.side = newSide
        this.cachedArea = this.side * this.side
    }

    override int Area()
    {
        return base.Area()
    }

    override void Draw()
    {
        // drawing omitted in tests
    }
}

Square square = (4)
assert square.Area() == 16

square.Resize(5)
assert square.Area() == 25