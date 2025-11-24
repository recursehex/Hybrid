// RUN_OPTS: --arc-optimizer --arc-trace-retains --arc-escape-debug
// EXPECT_OUTPUT: [arc-escape] main stack-only=0 removed=0 (no-op)
// EXPECT_OUTPUT: [arc-trace] pre main retains=0 releases=1 autoreleases=0
// EXPECT_OUTPUT: [arc-trace] post main retains=0 releases=1 autoreleases=0

extern unsafe void sink(byte@ object)

class Gadget
{
    int value

    Gadget(int v)
    {
        this.value = v
    }
}

int main()
{
    Gadget local = Gadget(4)
    unsafe
    {
        sink(#local)
    }
    return 0
}