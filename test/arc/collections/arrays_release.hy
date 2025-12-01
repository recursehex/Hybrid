int drops = 0

class Widget
{
    int id

    Widget(int id)
    {
        this.id = id
    }

    ~Widget()
    {
        drops += this.id
    }
}

void build_with_new()
{
    Widget[] items = new[2]
    items[0] = (3)
    items[1] = (4)
    // Scope exit should release the backing store and elements.
    return
}

void build_with_literal()
{
    Widget[] items = [(5), (6)]
    return
}

int main()
{
    build_with_new()
    assert drops == 7

    build_with_literal()
    assert drops == 18

    return 0
}