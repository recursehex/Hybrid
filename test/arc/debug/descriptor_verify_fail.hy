// RUN_OPTS: --arc-verify-runtime
// EXPECT_RUNTIME: [arc-verify]

extern unsafe void hybrid_release(byte@ object)

int main()
{
    int stack = 7
    unsafe
    {
        byte@ bogus = #stack
        hybrid_release(bogus)
    }
    return 0
}