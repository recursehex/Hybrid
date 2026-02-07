// RUN_OPTS: --arc-debug
// EXPECT_RUNTIME: [arc-pool] depth=1
// EXPECT_RUNTIME: [arc-pool] depth=0

extern unsafe void hybrid_autorelease_pool_push()
extern unsafe void hybrid_autorelease_pool_pop()
extern unsafe void hybrid_autorelease_pool_scoped_debug(byte@ label)

int main()
{
    hybrid_autorelease_pool_push()
    hybrid_autorelease_pool_scoped_debug(null)
    hybrid_autorelease_pool_pop()
    return 0
}