// RUN_OPTS: --arc-optimizer --arc-trace-retains --emit-llvm -o -
// EXPECT_OUTPUT: [arc-trace] pre main retains=1 releases=1 autoreleases=0
// EXPECT_OUTPUT: [arc-trace] post main retains=0 releases=0 autoreleases=0

extern unsafe byte@ hybrid_retain(byte@ object)
extern unsafe void hybrid_release(byte@ object)

unsafe int main()
{
    byte@ raw = null
    hybrid_release(hybrid_retain(raw))
    return 0
}