int freed = 0

class Widget
{
    int weight

    Widget(int w)
    {
        this.weight = w
    }

    ~Widget()
    {
        freed += this.weight
    }
}

int main()
{
    Widget first = new Widget(3)
    Widget second = new Widget(7)
    Widget inferred = new(10)

    free first
    free second
    free inferred

    assert freed == 20
    return 0
}