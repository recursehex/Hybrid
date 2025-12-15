int drops = 0

class WatchTarget
{
    int id

    WatchTarget(int id)
    {
        this.id = id
    }

    ~WatchTarget()
    {
        drops += this.id
    }
}

int main()
{
    shared<WatchTarget> owner = (30)
    weak<WatchTarget> watcher = owner.weak()
    {
        shared<WatchTarget> locked1 = watcher.lock()
        assert locked1.arcUseCount() == 2
    }

    weak<WatchTarget> orphan = ()
    {
        shared<WatchTarget> temp = (40)
        orphan = temp.weak()
        shared<WatchTarget> locked2 = orphan.lock()
        assert locked2.arcUseCount() == 2
    }

    shared<WatchTarget> missing = orphan.lock()
    assert missing.arcUseCount() == 0
    return 0
}
