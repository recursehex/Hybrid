// Stress smart pointer reassignments to ensure wrapper storage is reused

class Payload
{
    int value

    Payload(int v)
    {
        this.value = v
    }
}

void stressShared()
{
    shared<Payload> slot = shared<Payload>(Payload(0))
    shared<Payload> spare = shared<Payload>(Payload(1))
    slot = spare

    int i = 0
    while i < 50
    {
        shared<Payload> next = shared<Payload>(Payload(i))
        slot = next
        i++
    }
}

void stressWeak()
{
    shared<int> anchor = shared<int>(0)
    weak<int> watcher = weak<int>(anchor)

    int i = 0
    while i < 25
    {
        shared<int> replacement = shared<int>(i)
        weak<int> freshWatch = weak<int>(replacement)
        watcher = freshWatch
        anchor = replacement
        i++
    }
}

void stressUnique()
{
    unique<Payload> donor = unique<Payload>(Payload(10))
    unique<Payload> slot = unique<Payload>(Payload(20))

    int i = 0
    while i < 30
    {
        slot = donor
        donor = unique<Payload>(Payload(100 + i))

        unique<Payload> scratch = unique<Payload>(Payload(200 + i))
        slot = scratch
        donor = unique<Payload>(Payload(300 + i))
        i++
    }

    slot = donor
    donor = unique<Payload>(Payload(999))
}

int main()
{
    stressShared()
    stressWeak()
    stressUnique()
    return 0
}