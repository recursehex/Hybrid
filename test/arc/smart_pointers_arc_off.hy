// RUN_OPTS: --arc-enabled=false
// Smart pointer helpers should degrade to ARC-free stubs when ARC is disabled

class Dummy
{
    int value

    Dummy(int v)
    {
        this.value = v
    }

    int getValue()
    {
        return this.value
    }
}

int main()
{
    shared<Dummy> owner = make_shared<Dummy>(5)
    shared<Dummy> copy = owner

    weak<Dummy> watcher = owner.weak()
    weak<Dummy> watcher2 = weak_from_shared<Dummy>(owner)

    shared<Dummy> locked = watcher.lock()
    shared<Dummy> locked2 = watcher2.lock()

    // ARC is disabled, so the helpers return stub counts
    assert owner.arcUseCount() == 0
    assert copy.arcUseCount() == 0
    assert locked.arcUseCount() == 0
    assert locked2.arcUseCount() == 0
    assert (@owner).value == 5

    return 0
}