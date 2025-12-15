class Payload
{
    int id

    Payload(int id)
    {
        this.id = id
    }
}

int main()
{
    shared<Payload> owner = (7)
    shared<Payload> copy = owner
    assert owner.arcUseCount() == 2
    assert copy.arcUseCount() == 2

    weak<Payload> watcher = owner.weak()
    shared<Payload> promoted = watcher.lock()
    assert promoted.arcUseCount() == 3

    return 0
}
