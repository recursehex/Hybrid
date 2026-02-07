int drops = 0

class Widget
{
    int id

    Widget(int value)
    {
        this.id = value
    }

    ~Widget()
    {
        drops += this.id
    }
}

int main()
{
    {
        Widget[] items = [(1), (2), (3), (4)]

        // Shrink: keep first two, release trimmed tail.
        items = new[2]
        assert drops == 7

        // Overwrite should release the displaced values.
        items[0] = (10)
        items[1] = (20)
        assert drops == 10

        // Shrink again: keep first entry, drop second.
        items = new[1]
        assert drops == 30

        items[0] = (30)
        assert drops == 40
    }

    // Final live element is released at scope exit.
    assert drops == 70
    return 0
}
