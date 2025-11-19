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

int main()
{
    stressShared()
    stressWeak()
    return 0
}