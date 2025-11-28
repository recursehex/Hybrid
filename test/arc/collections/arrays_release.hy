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
    Widget[] items = new Widget[2]
    items[0] = Widget(3)
    items[1] = Widget(4)
    // Scope exit should release the backing store and elements.
    return
}

void build_with_literal()
{
    Widget[] items = [Widget(5), Widget(6)]
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