extern unsafe int __hybrid_debug_strong_count(byte@ object)

class Payload
{
    int id

    Payload(int id)
    {
        this.id = id
    }
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

struct Holder
{
    Payload item

    Holder(Payload payload)
    {
        this.item = payload
    }
}

int main()
{
    Payload payload = (5)
    {
        Holder first = Holder(payload)
        unsafe
        {
            byte@ payloadPtr1 = payload
            assert strongCount(payloadPtr1) >= 2
        }

        {
            Holder clone = first
            unsafe
            {
                byte@ payloadPtr2 = payload
                assert strongCount(payloadPtr2) >= 2
            }
        }

        unsafe
        {
            byte@ payloadPtr3 = payload
            assert strongCount(payloadPtr3) >= 2
        }
    }

    unsafe
    {
        byte@ payloadPtr4 = payload
        assert strongCount(payloadPtr4) >= 1
    }

    return 0
}
