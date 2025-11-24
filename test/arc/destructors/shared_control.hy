int drops = 0

class Widget
{
    int value

    Widget(int v)
    {
        this.value = v
    }

    ~Widget()
    {
        drops += this.value
    }
}

void buildShared()
{
    shared<Widget> primary = shared<Widget>(Widget(5))
    shared<Widget> secondary = primary
    {
        shared<Widget> tertiary = secondary
        assert drops == 0
    }
    assert drops == 0
}

int main()
{
    buildShared()
    assert drops == 5
    return 0
}