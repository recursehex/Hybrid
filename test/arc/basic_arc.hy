extern unsafe int __hybrid_debug_strong_count(byte@ object)

int destroyed = 0

class RefTarget
{
    int id

    RefTarget(int value)
    {
        this.id = value
    }

    ~RefTarget()
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
    RefTarget root = (1)
    unsafe
    {
        byte@ rootPtr1 = root
        assert strongCount(rootPtr1) == 1
    }

    {
        RefTarget alias = root
        unsafe
        {
            byte@ rootPtr2 = root
            assert strongCount(rootPtr2) == 2
        }

        {
            RefTarget copy = alias
            unsafe
            {
                byte@ rootPtr3 = root
                assert strongCount(rootPtr3) == 3
            }
        }

        unsafe
        {
            byte@ rootPtr4 = root
            assert strongCount(rootPtr4) == 2
        }
    }

    unsafe
    {
        byte@ rootPtr5 = root
        assert strongCount(rootPtr5) == 1
    }

    assert destroyed == 0
    return 0
}
