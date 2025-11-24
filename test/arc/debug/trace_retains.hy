// RUN_OPTS: --arc-debug
// EXPECT_RUNTIME: [arc-trace] op=retain
// EXPECT_RUNTIME: [arc-trace] op=release

class Tracer
{
    int value

    Tracer(int v)
    {
        this.value = v
    }
}

extern unsafe void hybrid_arc_trace_flush_default()
extern unsafe byte@ hybrid_retain(byte@ object)
extern unsafe void hybrid_release(byte@ object)
extern unsafe byte@ hybrid_alloc_object(int size, byte@ descriptor)

int main()
{
    unsafe
    {
        byte@ obj = hybrid_alloc_object(24, null)
        hybrid_retain(obj)
        hybrid_release(obj)
    }

    hybrid_arc_trace_flush_default()
    return 0
}