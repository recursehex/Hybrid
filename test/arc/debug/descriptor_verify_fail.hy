// RUN_OPTS: --arc-verify-runtime
// EXPECT_FAIL: runtime
// EXPECT_EXIT: nonzero
// EXPECT_RUNTIME: [arc-verify]

extern unsafe void hybrid_release(byte@ object)
extern unsafe byte@ hybrid_alloc_object(int size, byte@ descriptor)

int main()
{
    unsafe
    {
        byte@ bogus = hybrid_alloc_object(24, null)
        byte@ shifted = bogus + 1
        hybrid_release(shifted)
    }
    return 0
}
