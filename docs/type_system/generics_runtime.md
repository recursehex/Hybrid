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

## Relationship to modules & diagnostics
All diagnostics flow through `reportCompilerError(...)`, and the parser relies on the `TemplateAngleScope` lookahead to decide when `<...>` denotes type arguments versus comparisons or shifts.

The combination of these changes gives us the safety of C#/Java-style syntax with the predictable codegen of C++ templates.