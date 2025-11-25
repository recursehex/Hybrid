struct Point
{
    int x
    int y

    Point(int x, int y)
    {
        this.x = x
        this.y = y
    }
}

int main()
{
    Point p = new Point(3, 4)
    int total = p.x + p.y
    free p
    assert total == 7
    return 0
}