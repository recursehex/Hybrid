# Generics Runtime Strategy

Hybrid's generics commit to **per-instantiation specialization** instead of type erasure.

## Why specialization?
- Every `class` / `struct` / `interface` instantiation receives its own LLVM IR body, so fields keep their concrete types (no boxing) and vtables embed exact dispatch slots. This keeps existing ABI guarantees because a `Box<int>` has the same lowering every time it appears.
- Codegen reuses cached instantiations. `InstantiateGenericFunction` consults the overload registry using mangled binding suffix (for example `Copy$RV_void_P1R_int<T=int>`), so calling `Copy<int>` twice never clones IR.
- Since the mangled name records both class-level and method-level bindings, even generic methods that only mention their type parameters in the body can be instantiated safely. The overload table also keys off the same binding string so collisions are impossible.

## Managing bloat
Specialization can inflate the module if a template is instantiated for dozens of combos. We mitigate that in three ways:

1. **Overload registry binding keys** - `FunctionOverload` tracks a `genericBindingKey` that forces a cache miss for different bindings while ensuring reuse when we revisit the exact same binding.
2. **Registry reset hooks** - `CompilerSession::resetAll()` calls `ResetGenericTemplateRegistries()` so stale templates from previous compilations cannot leak into the next session.
3. **Describe + inspect tooling** - the `describeType("Box<int>")` intrinsic reads the runtime descriptor and reports bound arguments, base classes, interfaces, and generic method instantiations. Tests under `test/generics/basic/metadata_dump.hy` pin the exact wording so future regressions are visible.

> [!WARNING]
> Deeply nested or wildly varied instantiations will eventually hit depth/instantiation caps and halt compilation. Trim unused bindings or raise `--max-generic-depth` / `--max-generic-instantiations`.

## Relationship to modules & diagnostics
All diagnostics flow through `reportCompilerError(...)`, and the parser relies on the `TemplateAngleScope` lookahead to decide when `<...>` denotes type arguments versus comparisons or shifts.

The combination of these changes gives us the safety of C#/Java-style syntax with the predictable codegen of C++ templates.

## Troubleshooting & Telemetry

- `--diagnostics generics` prints a one-line summary covering cache hits, the number of unique type/function specialisations, peak binding depth, and the size of the generated LLVM module. Set `HYBRID_SHOW_GENERIC_METRICS=1` when running `run_tests.sh` to enable this automatically.
- `--dump-generic-stack` dumps the active binding frames when the compiler aborts because the recursive instantiation depth (`--max-generic-depth`, default 128) is exceeded.
- `--max-generic-instantiations` and `--max-nested-generics` offer hard caps on total specialisations emitted and individual type-expression depth, respectively. They complement the default heuristics that warn about >8 generic parameters or >4 nested `<>` segments.
- If a particular template expands faster than expected, call `describeType()` for the bound type and inspect the `genericMethodInstantiations` field to see which members are triggering new LLVM bodies.

These diagnostics keep large codebases honest while preserving the zero-cost model that specialisation provides.