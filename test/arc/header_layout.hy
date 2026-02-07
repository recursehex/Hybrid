extern unsafe bool __hybrid_debug_descriptor_matches(byte@ object, string expectedName)
extern unsafe int __hybrid_debug_strong_count(byte@ object)

unsafe bool descriptorMatches(byte@ object, string expectedName)
{
    return __hybrid_debug_descriptor_matches(object, expectedName)
}

unsafe int strongCount(byte@ object)
{
    return __hybrid_debug_strong_count(object)
}

struct HeaderProbe
{
    int data

    HeaderProbe()
    {
        this.data = 99
    }
}

class Gadget
{
    int label

    Gadget(int value)
    {
        this.label = value
    }
}

int main()
{
    HeaderProbe probe = ()
    unsafe
    {
        byte@ probePtr = #probe
        assert descriptorMatches(probePtr, "HeaderProbe")
        assert strongCount(probePtr) == 1
    }

    Gadget gizmo = (7)
    unsafe
    {
        byte@ gizmoPtr = gizmo
        assert descriptorMatches(gizmoPtr, "Gadget")
    }

    return 0
}