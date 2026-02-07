# ARC Best Practices

Hybrid's ARC frees heap allocations automatically. Use these patterns to keep ownership predictable and avoid leaks.

## Default ownership
- Prefer plain class/struct references for most graphs.
    - ARC retains on assignment and releases at scope exit, running destructors when the last strong reference dies.
- Use `free` only when you need deterministic teardown (e.g. closing files, flushing buffers) on heap references. It is rejected for stack values and smart pointer handles because ARC already manages them.
- Compare behaviors with `--arc-enabled=true|false` or `./run_tests.sh -a on|off` when tooling needs ARC-on vs ARC-off baselines.
- The compiler will implicitly promote a stack-allocated ARC value to a heap object when it would otherwise be retained past the stack frame (e.g. storing a stack-built class/struct into an ARC field or returning it by value). This avoids dangling pointers without changing source, but you still pay one allocation + copy only in those escaping cases.

## Avoiding cycles with weak references
- Mark back references `weak<T>` when children hold strong owners (trees, UI hierarchies) so the parent can be reclaimed.
- Store observers and caches as `weak<T>` to prevent accidental retention of producers.

```cs
class Node
{
    int value
    shared<Node>[] children
    weak<Node> parent

    Node(shared<Node> parent, int value)
    {
        this.parent = parent
        this.value = value
        this.children = []
    }
}
```

## Smart pointer choices
- ARC already frees payloads; prefer plain references inside a scope-bound object graph.
- Use `shared<T>` when ownership must be shared across async work or registries.
    - Pair observers with `weak<T>` via `shared<T>.weak()` or `weak_from_shared`.
- Use `unique<T>` for move-only ownership (for example, transferring a buffer between queues) without exposing raw pointers.

```cs
Door local = new Door()                        // ARC cleans up at scope exit
shared<Door> sharedDoor = make_shared<Door>()  // Shared ownership across tasks
weak<Door> watcher = sharedDoor.weak()         // Does not keep the Door alive
```

## Manual release timing
- Use `free` only when needed for explicit lifecycle points.
    - Avoid mixing it with manual destructor calls.
- If a value must be torn down before leaving a long-lived scope, `free` it immediately after last use and reassign the variable to avoid reusing the released handle.

## Debugging and tests
- Set `HYBRID_ARC_DEBUG=1` or `HYBRID_ARC_TRACE_RUNTIME=1` when running `./run_tests.sh` to trace retains/releases; `HYBRID_ARC_VERIFY_RUNTIME=1` or `HYBRID_ARC_LEAK_DETECT=1` force the harness to link the real runtime and assert bookkeeping.
- See `docs/testing.md` for runtime selection details and guidance on ARC-on vs ARC-off diagnostics.
