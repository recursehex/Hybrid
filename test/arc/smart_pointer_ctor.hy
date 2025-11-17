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
    unique<Gadget> owned = unique<Gadget>(Gadget(7))
    shared<Gadget> sharedOwner = shared<Gadget>(Gadget(9))
    weak<Gadget> observer = weak<Gadget>(sharedOwner)
    return 0
}