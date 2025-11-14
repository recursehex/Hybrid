# Perf Suite Notes

Set `HYBRID_GENERICS_METRICS=1` when running the compiler to print cache hit/miss counts at the end of the build. The commands below capture both `/usr/bin/time` output and the generics metrics by redirecting stderr to a log file.

## `generics_burst.hy`
- **Command:** `HYBRID_GENERICS_METRICS=1 /usr/bin/time -p ./build/hybrid --emit-llvm -o /tmp/generics_burst.ll test/perf/generics_burst.hy`
- **Compile time (real/user/sys):** `0.22s / 0.00s / 0.01s` on the current Apple Silicon Release build.
- **Module size:** `38,604 bytes` for the emitted LLVM IR (`stat -f%z /tmp/generics_burst.ll`).
- **Generics metrics:** `functions hits:0 misses:0 | types hits:191 misses:9`
  (this stress test deliberately exercises unique bindings, so type misses should equal the number of `[instantiate]` lines and hits stay minimal).

## `generics_cache_reuse.hy`
- **Command:** `HYBRID_GENERICS_METRICS=1 /usr/bin/time -p ./build/hybrid --emit-llvm -o /tmp/generics_cache_reuse.ll test/perf/generics_cache_reuse.hy`
- **Compile time (real/user/sys):** `0.01s / 0.00s / 0.00s`.
- **Module size:** `8,207 bytes` for the emitted LLVM IR.
- **Generics metrics:** `functions hits:4 misses:1 | types hits:35 misses:1`
  (this case repeatedly instantiates the same binding to guarantee observable cache hits).

Re-run the commands above after compiler changes that touch generics or instantiation caching and update these figures so we can spot regressions quickly.