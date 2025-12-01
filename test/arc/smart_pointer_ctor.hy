// Smart pointer constructor smoke test.

class Gadget
{
    int value

    Gadget(int v)
    {
        this.value = v
    }

    Gadget(Gadget g)
    {
        this.value = g.value
    }
}

int main()
{
    unique<Gadget> owned = (7)
    shared<Gadget> sharedOwner = (9)
    weak<Gadget> observer = (sharedOwner)
    shared<int> sharedNumber = (42)
    weak<int> numberObserver = (sharedNumber)
    return 0
}