int drops = 0

class Tracker
{
    int value

    Tracker(int v)
    {
        this.value = v
    }

    ~Tracker()
    {
        drops += this.value
    }
}

void makeTracker(int value)
{
    Tracker temp = Tracker(value)
    temp.~Tracker()
}

int main()
{
    makeTracker(1)
    assert drops == 1

    makeTracker(5)
    makeTracker(10)
    assert drops == 16

    return 0
}