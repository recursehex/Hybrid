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

void promoteWeakManyTimes()
{
    weak<Widget> observer = ()
    {
        shared<Widget> owner = (9)
        observer = owner.weak()
        int i = 0
        while i < 5
        {
            shared<Widget> promoted = observer.lock()
            assert promoted.arcUseCount() == 2
            i++
        }
        assert drops == 0
    }

    assert drops == 9

    shared<Widget> expired = observer.lock()
    assert expired.arcUseCount() == 0
    assert drops == 9
}

int main()
{
    promoteWeakManyTimes()
    return 0
}
