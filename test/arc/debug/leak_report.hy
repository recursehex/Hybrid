// RUN_OPTS: --arc-leak-detect
// EXPECT_RUNTIME: [arc-leak] ptr=

extern unsafe byte@ hybrid_retain(byte@ object)
extern unsafe void hybrid_arc_dump_leaks_default()

class LeakTarget
{
    int value

    LeakTarget(int v)
    {
        this.value = v
    }
}

LeakTarget make_leak()
{
    LeakTarget temp = LeakTarget(42)
    unsafe
    {
        hybrid_retain(temp)
    }
    return temp
}

int main()
{
    LeakTarget leaked = make_leak()
    hybrid_arc_dump_leaks_default()
    return 0
}