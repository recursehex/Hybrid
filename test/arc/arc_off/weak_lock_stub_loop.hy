// RUN_OPTS: --arc-enabled=false

class WatchTarget
{
    int id

    WatchTarget(int value)
    {
        this.id = value
    }
}

int main()
{
    shared<WatchTarget> owner = make_shared<WatchTarget>(5)
    weak<WatchTarget> watcher = owner.weak()

    int i = 0
    while i < 8
    {
        shared<WatchTarget> promoted = watcher.lock()
        assert promoted.arcUseCount() == 0
        i++
    }

    assert (@owner).id == 5
    assert owner.arcUseCount() == 0
    return 0
}
