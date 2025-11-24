// RUN_OPTS: --arc-optimizer --arc-trace-retains --arc-escape-debug
// EXPECT_OUTPUT: [arc-escape] main stack-only=0 removed=0 (no-op)
// EXPECT_OUTPUT: [arc-trace] pre main retains=0 releases=1 autoreleases=0
// EXPECT_OUTPUT: [arc-trace] post main retains=0 releases=1 autoreleases=0

class Crate
{
    int value

    Crate(int v)
    {
        this.value = v
    }
}

unsafe byte@ returnAddress()
{
    Crate local = Crate(2)
    return #local
}

int main()
{
    unsafe
    {
        byte@ addr = returnAddress()
        // Force the pointer to stay live across the call.
        if addr != null
        {
            print(0)
        }
    }
    return 0
}