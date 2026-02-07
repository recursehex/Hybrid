extern unsafe int __hybrid_debug_strong_count(byte@ object)

class Element
{
    int id

    Element(int id)
    {
        this.id = id
    }
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

int main()
{
    Element first = (1)
    Element second = (2)

    {
        Element[] items = new[2]
        items[0] = first
        items[1] = second

        unsafe
        {
            byte@ firstPtr1 = first
            byte@ secondPtr1 = second
            assert strongCount(firstPtr1) == 2
            assert strongCount(secondPtr1) == 2
        }

        items[0] = second

        unsafe
        {
            byte@ firstPtr2 = first
            byte@ secondPtr2 = second
            assert strongCount(firstPtr2) == 1
            assert strongCount(secondPtr2) == 3
        }
    }

    unsafe
    {
        byte@ firstPtr3 = first
        byte@ secondPtr3 = second
        assert strongCount(firstPtr3) == 1
        assert strongCount(secondPtr3) == 1
    }

    return 0
}
