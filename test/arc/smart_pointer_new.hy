int freed = 0

class Tracker
{
    int value

    Tracker(int v)
    {
        this.value = v
    }

    ~Tracker()
    {
        freed += this.value
    }
}

int main()
{
    shared<int> number = new(11)
    assert @number == 11

    {
        shared<Tracker> owner = new(5)
        assert owner->value == 5

        shared<Tracker> copy = owner
        assert owner.arcUseCount() == 2
        assert copy.arcUseCount() == 2

        owner = new(7)
        assert owner.arcUseCount() == 1
        assert copy.arcUseCount() == 1
    }

    assert freed == 12

    shared<int> explicitShared = new shared<int>(3)
    assert @explicitShared == 3

    return 0
}