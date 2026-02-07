extern unsafe int __hybrid_debug_strong_count(byte@ object)

int destroyed = 0

class Watched
{
    int id

    Watched(int id)
    {
        this.id = id
    }

    ~Watched()
    {
        destroyed += this.id
    }
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

int main()
{
    shared<Watched> owner = (10)
    {
        Watched raw = owner.get()
        unsafe
        {
            byte@ rawPtr = raw
            assert strongCount(rawPtr) == 2
        }
    }

    weak<Watched> observer = #owner
    {
        shared<Watched> locked1 = observer.lock()
        assert locked1.arcUseCount() == 2
    }

    weak<Watched> expiring = ()
    {
        shared<Watched> temp = (20)
        expiring = #temp
        shared<Watched> locked2 = expiring.lock()
        assert locked2.arcUseCount() == 2
    }

    shared<Watched> expired = expiring.lock()
    assert expired.arcUseCount() == 0
    return 0
}
