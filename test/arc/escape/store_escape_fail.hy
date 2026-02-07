// RUN_OPTS: --arc-optimizer --arc-trace-retains --arc-escape-debug
// EXPECT_PASS
// EXPECT_OUTPUT: [arc-escape] main stack-only=0 removed=0 (no-op)
// EXPECT_OUTPUT: [arc-trace] pre main retains=0 releases=1 autoreleases=0
// EXPECT_OUTPUT: [arc-trace] post main retains=0 releases=1 autoreleases=0

unsafe void capture(byte@ object)
{
}

class Parcel
{
    int payload

    Parcel(int v)
    {
        this.payload = v
    }
}

int main()
{
    Parcel local = (3)
    unsafe
    {
        capture(#local)
    }
    return 0
}
