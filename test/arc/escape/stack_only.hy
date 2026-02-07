// RUN_OPTS: --arc-optimizer --arc-trace-retains --arc-escape-debug
// EXPECT_OUTPUT: [arc-escape] main stack-only=1 removed=1
// EXPECT_OUTPUT: [arc-trace] pre main retains=0 releases=1 autoreleases=0
// EXPECT_OUTPUT: [arc-trace] post main retains=0 releases=0 autoreleases=0

class Box
{
    int value

    Box(int v)
    {
        this.value = v
    }
}

int main()
{
    Box local = Box(1)
    return 0
}