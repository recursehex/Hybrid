// Smart pointer constructor smoke test.

class Gadget
{
    int value

    Gadget(int v)
    {
        this.value = v
    }
}

int main()
{
    unique<Gadget> owned = (Gadget(7))
    shared<Gadget> sharedOwner = (Gadget(9))
    weak<Gadget> observer = (sharedOwner)
    shared<int> sharedNumber = (42)
    weak<int> numberObserver = (sharedNumber)
    return 0
}