extern unsafe bool __hybrid_debug_descriptor_matches(byte@ object, string expectedName)
extern unsafe int __hybrid_debug_strong_count(byte@ object)
extern unsafe void "__hybrid_dealloc$DerivedTracker"(byte@ object)

unsafe bool descriptorMatches(byte@ object, string expectedName)
{
    return __hybrid_debug_descriptor_matches(object, expectedName)
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

unsafe void invokeDerivedDealloc(byte@ object)
{
    "__hybrid_dealloc$DerivedTracker"(object)
}

class Payload
{
    static int nextId
    int id

    Payload()
    {
        this.id = Payload.nextId + 1
        Payload.nextId = this.id
    }
}

class BaseTracker
{
    Payload primary

    BaseTracker(Payload first)
    {
        this.primary = first
    }
}

class DerivedTracker inherits BaseTracker
{
    Payload secondary

    DerivedTracker(Payload first, Payload second)
    {
        base(first)
        this.secondary = second
    }
}

int main()
{
    Payload first = ()
    Payload second = ()
    DerivedTracker tracker = (first, second)

    BaseTracker upcast = tracker

    unsafe
    {
        byte@ basePtr = upcast
        assert descriptorMatches(basePtr, "DerivedTracker")

        byte@ derivedPtr = tracker
        invokeDerivedDealloc(derivedPtr)
    }

    unsafe
    {
        byte@ firstPtr = first
        byte@ secondPtr = second
        assert strongCount(firstPtr) == 1
        assert strongCount(secondPtr) == 1
    }

    return 0
}