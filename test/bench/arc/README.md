# ARC Benchmarks

This directory contains ARC-focused runtime benchmarks used by `scripts/arc_bench.sh`.

## Workloads
- `object_churn.hy`: repeated class reassignment in tight loops.
- `array_slot_churn.hy`: array overwrite churn for ARC-managed class elements.
- `generic_box_churn.hy`: generic class instantiation/reassignment churn.
- `service_pipeline.hy`: macro-style service queue workload using constructor-initialized `JobQueue` storage.

Each benchmark is written to pass under both ARC-on and ARC-off modes so the benchmark driver can compare timing deltas directly.

Use `./scripts/arc_bench.sh --pass-timing` to also capture compiler stage timings into `build/arc-bench/arc_bench_pass_timing_<timestamp>.csv`.
