# Outstanding Bugs — Hybrid Compiler

Audit date: 2026-03-30. Ordered by severity.

---

## Critical

### 1. LLVM Context destroyed before Module/Builder (use-after-free in destructors)
**`src/codegen_context.h:271-274`**

`CodegenContext::reset()` destroys `llvmContext` first, then `module`, then `builder`. But `module` and `builder` hold internal pointers into `llvmContext`. Their destructors will touch freed memory.

**Fix:** Reverse the order — destroy `builder`, then `module`, then `llvmContext`.

---

### 2. Ternary type-promotion emits instructions in MergeBB but PHI claims they come from ThenBB/ElseBB (invalid LLVM IR)
**`src/ast/expr_ternary_switch.cpp:200-219`**

When a ternary's then/else arms need numeric promotion (e.g. `int` → `double`), the `CreateSIToFP` etc. calls are emitted in MergeBB (insert point set at line 190), but the PHI node at line 260 lists ThenBB/ElseBB as the incoming blocks. LLVM requires PHI incoming values to dominate the end of their predecessor block — this violates that and will fail verification or silently miscompile.

**Fix:** Emit promotion instructions at the end of the respective ThenBB/ElseBB before the branch to MergeBB.

---

### 3. Missing short-circuit evaluation for `&&` and `||`
**`src/ast/expr_ops.cpp:3346-3375`**

Both operands are eagerly evaluated before the `&&`/`||` IR is built. This means `a && b` evaluates `b` even when `a` is false. Any program that relies on short-circuit guards (e.g. `ptr != null && ptr.field`) will crash or misbehave.

**Fix:** Emit control-flow-based short-circuit logic (branch on LHS, conditionally evaluate RHS).

---

### 4. Unsigned underflow in ARC `release` when refcount is already 0 (data race)
**`runtime/include/hybrid_runtime.h:133-136`, `src/runtime/arc.cpp:640-643`**

`hybrid_refcount_release_strong` does `fetch_sub(1)` unconditionally, then checks `if (previous == 0)` and tries to `store(0)`. The `fetch_sub` already wrapped the atomic to `UINT32_MAX`; another thread can observe this corrupted value before the corrective store.

**Fix:** Use `compare_exchange` instead of `fetch_sub` when the count might be 0, or assert/trap on double-release.

---

### 5. Compound assignment on array elements stores promoted-width value into narrow slot (memory corruption)
**`src/ast/expr_ops.cpp:1989`, `3337-3338`, `3514`, `3586`**

After computing e.g. `byte_array[i] += 1` with integer promotion to `i32`, the `i32` result is stored directly into the `i8` element slot via `CreateStore` — writing 4 bytes into 1 byte of storage, corrupting adjacent memory.

**Fix:** Truncate the result back to `ElemType` before storing (like the variable compound-assignment path at lines 1874-1880 already does).

---

### 6. `foreach` over raw pointer defaults to hardcoded size of 10 (buffer overread)
**`src/ast/stmts.cpp:1028`**

When the compiler cannot determine the array size statically, it silently defaults to `ArraySize = 10`. Arrays smaller than 10 cause out-of-bounds reads; arrays larger than 10 lose elements.

**Fix:** Require an explicit size, or store the length alongside the pointer.

---

### 7. `castToType` always uses signed conversions — wrong for unsigned types
**`src/ast/expr_ops.cpp:87, 91-93`**

`CreateSIToFP` (signed-int-to-float) is used for all int→float casts, and `CreateFPToSI` for all float→int casts. For `uint`, `ulong`, `byte` etc., large unsigned values are interpreted as negative, producing wrong results.

**Fix:** Check signedness and use `CreateUIToFP` / `CreateFPToUI` for unsigned types.

---

### 8. `isspace`/`isalpha`/`isalnum` called with potentially negative `int` (UB)
**`src/lexer.cpp:354, 388, 392`**

`LastChar` is `int` and may be negative (e.g. high-byte UTF-8). Passing a negative value (other than `EOF`) to ctype functions is undefined behavior per the C standard.

**Fix:** Cast to `static_cast<unsigned char>(LastChar)` before calling ctype functions.

---

### 9. Signed integer overflow in compile-time constant evaluation (UB)
**`src/parser/parser_consteval.cpp:117-135`**

`lhs.intVal + rhs.intVal` etc. on `long long` can overflow — undefined behavior in C++. Division of `LLONG_MIN / -1` at line 128 is also UB.

**Fix:** Check for overflow before arithmetic, or use `__builtin_add_overflow` etc.

---

### 10. Undefined shift behavior in constant evaluation
**`src/parser/parser_consteval.cpp:194-203`**

`lhs.intVal << rhs.intVal` is UB when `rhs` is negative or ≥ 64. No validation of the shift amount.

**Fix:** Validate shift amount is in range `[0, 63]` before shifting.

---

## High

### 11. Unsafe `static_cast<AllocaInst*>` without `dyn_cast` check
**`src/ast/expr_access.cpp:340`, `src/ast/functions.cpp:340`**

Raw `static_cast<llvm::AllocaInst*>(V)` is used without verifying `V` is actually an `AllocaInst`. For `ref` parameters, `V` is an `llvm::Argument`. Calling `getAllocatedType()` on the bogus pointer is UB.

**Fix:** Use `llvm::dyn_cast<llvm::AllocaInst>(V)` and handle the null case (as already done correctly in `VariableExprAST::codegen()` at line 672).

---

### 12. Data races on ARC debug flags
**`src/runtime/arc.cpp:24-27`**

`hybrid_debug_leaks`, `hybrid_debug_reftrace`, etc. are plain `int` globals read from runtime hot paths and written from `hybrid_arc_set_debug_flags` without synchronization. This is UB under the C++ memory model.

**Fix:** Make them `std::atomic<int>` (or `std::atomic<bool>`).

---

### 13. `ArcScopeStack.clear()` on error wipes all scopes, not just the current function's
**`src/ast/functions.cpp:336-339`**

When codegen fails, `CG.arcScopeStack.clear()` destroys ALL ARC scopes including any enclosing contexts. The `ArcScopeGuard` destructor will then try to pop a scope from an empty stack.

**Fix:** Only pop scopes down to the function's entry depth (the guard's `originDepth`), not `clear()` the entire stack.

---

### 14. `stripOwnershipQualifier` lambda is a no-op (dead code)
**`src/ast/ast_types.cpp:145-161`**

The lambda defines `tryConsume` internally but never calls it. No ownership qualifier is ever actually stripped from type strings, causing downstream type resolution failures for types with ownership annotations.

**Fix:** Call `tryConsume` for each ownership keyword inside the lambda body.

---

### 15. Dangling raw pointer `PrototypeView` in `MethodDefinition`
**`src/ast.h:1447, 1482`**

`PrototypeView` is a non-owning `PrototypeAST*` with no lifetime contract. If the owning `unique_ptr` is moved or destroyed, `getPrototype()` returns a dangling pointer.

**Fix:** Use `std::shared_ptr` or store a non-owning `std::reference_wrapper` with clear documentation.

---

### 16. Iterator invalidation in escape analysis
**`src/optimizer/escape_analysis.cpp:411-419`**

The loop erases instructions via `call->eraseFromParent()` while iterating a vector of raw `CallBase*` pointers. Remaining pointers in the vector may reference freed memory.

**Fix:** Collect instructions to erase into a separate list, then erase after iteration.

---

### 17. Operator precedence map polluted by `operator[]` auto-insertion
**`src/parser/parser_core.cpp:176-181`**

`GetTokPrecedence` uses `BinopPrecedence[Op]` which auto-inserts a 0 entry for any unregistered operator. This silently grows the map and could mask later lookups.

**Fix:** Use `BinopPrecedence.find(Op)` instead.

---

### 18. Unterminated string literal silently accepted
**`src/lexer.cpp:634-689`**

When a string literal hits `EOF` without a closing `"`, no error is reported. The lexer silently produces a garbage token.

**Fix:** Emit an error diagnostic and return `tok_error` when the string is unterminated.

---

### 19. `NumberExprAST::codegen_with_target` uses `zextOrTrunc` for signed values
**`src/ast/expr_literals.cpp:82-87`**

`isIntN(bitWidth)` checks unsigned fitness, and `zextOrTrunc` zero-extends. For negative integer literals targeting a signed type, this produces wrong constant values (e.g. `-1` as `i32` becomes `0x00000000FFFFFFFF` as `i64` instead of `0xFFFFFFFFFFFFFFFF`).

**Fix:** Use `isSignedIntN` and `sextOrTrunc` for signed target types.

---

### 20. Codegen invoked during parsing phase (no rollback possible)
**`src/parser/parser_decls.cpp:406, 543, 591`**

`ParseTypeIdentifier` calls `codegen()` directly during parsing. If a later parse error occurs, the partially-emitted LLVM IR cannot be rolled back, leaving the module in a corrupt state.

**Fix:** Defer all codegen to a separate phase after parsing completes.

---

## Medium

### 21. `autorelease` without a pool silently leaks
**`src/runtime/arc.cpp:608-611`**

When `AutoreleasePools` is empty, `hybrid_autorelease` returns the object without adding it to any pool — the retain count is never decremented.

**Fix:** At minimum emit a runtime warning. Ideally, trap or create an implicit pool.

---

### 22. `gInteractiveMode` is not `thread_local`
**`src/toplevel.cpp:17`**

Plain `static bool` in a codebase where session state is `thread_local`. Data race if the compiler runs on multiple threads.

**Fix:** Make it `thread_local`.

---

### 23. `popGenericTypeBindingScope` unconditionally pops diagnostic stack
**`src/ast/ast_context.cpp:743-748`**

The push is conditional (only when `frameLabel` is non-empty), but the pop always pops. Mismatched push/pop corrupts the diagnostic binding stack.

**Fix:** Guard the pop with the same condition as the push.

---

### 24. `getMangledName` returns reference to reused `thread_local` static
**`src/ast/functions.cpp:36-39`**

Returns `const std::string&` to a `thread_local` buffer that is overwritten on every call. Any caller caching the reference gets a dangling/stale reference.

**Fix:** Return by value, or document that the reference is only valid until the next call.

---

### 25. `makeMethodSignatureKey` separator off-by-one when skipping first param
**`src/ast/ast_overloads.cpp:460-461`**

Checks `i != 0` instead of `i != start`, prepending a stray comma when `start == 1`. Produces keys like `methodName(,int)` instead of `methodName(int)`, breaking overload resolution.

**Fix:** Change `if (i != 0)` to `if (i != start)`.

---

### 26. Negative array dimensions accepted, wrapping to huge `uint64_t`
**`src/ast/expr_literals.cpp:1696-1697`**

`static_cast<uint64_t>(dim)` where `dim` is `int64_t`. A negative dimension wraps to an enormous value, causing OOM or an infinite fill loop.

**Fix:** Validate that dimensions are non-negative before the cast.

---

### 27. `SwitchExprAST` assumes `i8 == bool` and `pointer == string`
**`src/ast/expr_ternary_switch.cpp:82-96`**

Type inference from the first case arm maps `i8` → `"bool"` (could be `byte`/`sbyte`) and any pointer → `"string"` (could be class/interface pointers).

**Fix:** Propagate the actual source-level type name from the AST rather than guessing from the LLVM type.

---

### 28. Double `finalizePayloadValue` call leaks first allocation
**`src/ast/expr_calls.cpp:1030-1036, 1061-1065`**

When `initializerIsPayloadCall` is true, `finalizePayloadValue` can be called twice on the same value — each call heap-allocates and copies, so the first allocation is leaked.

**Fix:** Track whether the value has already been finalized and skip the second call.

---

### 29. `enterUnsafeContext`/`exitUnsafeContext` not exception-safe
**`src/toplevel.cpp:259-285`**

If a `Handle*` function throws between `enterUnsafeContext()` and `exitUnsafeContext()`, the counter is permanently incremented. Should use a RAII guard.

---

### 30. `castToType` result used without null check (multiple locations)
**`src/ast/ast_types.cpp:1427-1429`, `src/ast/expr_literals.cpp:2051`**

`castToType(...)` can return `nullptr`, but the result is passed directly to `CreateStore` / `CreateCall`, crashing on null.

**Fix:** Check for null and return an error.

---

### 31. Static `wrapperCache` holds stale `llvm::Function*` across module rebuilds
**`src/ast/ast_types.cpp:662`**

Function-local `static std::map<std::string, llvm::Function*> wrapperCache` caches LLVM function pointers that become dangling when the module is reset (e.g. in REPL sessions).

**Fix:** Clear the cache when the module is rebuilt, or tie it to the module's lifetime.

---

### 32. `makeNumericDefault` silently mishandles `BOOLEAN` constant values
**`src/ast/ast_overloads.cpp:232-241`**

The `BOOLEAN` case falls through the switch to `break` without setting `info.kind` or `info.numberValue`, producing a `DefaultArgInfo` with kind `Number` but uninitialized numeric content.

**Fix:** Handle the `BOOLEAN` case explicitly — set `info.kind = Bool` and `info.boolValue`.

---

### 33. Missing constructor generic-binding application
**`src/ast/aggregates.cpp:969`**

`getTypeFromString(Param.DeclaredType.typeName)` uses the raw type name without first applying `applyActiveTypeBindings()`. For generic constructors, this resolves to the wrong LLVM type.

**Fix:** Apply active type bindings before resolving the type.

---

### 34. Unanchored `ExitBB` leaked on early return in switch codegen
**`src/ast/aggregates.cpp:2260-2331`**

`ExitBB` is created without a parent function and only inserted later. Any error-path `return nullptr` between creation and insertion leaks the basic block.

**Fix:** Create `ExitBB` with the parent function, or ensure cleanup on error paths.

---

### 35. Redundant `Operand->codegen()` call in `++`/`--` causes double evaluation
**`src/ast/expr_ops.cpp:3715-3728`**

`codegen_ptr()` is called for the address, then `codegen()` is called again just to get the type. Side-effectful operands are evaluated twice.

**Fix:** Use `Builder->CreateLoad(...)` with the type from the alloca, or get the type from `codegen_ptr()`.

---

### 36. `hybrid_strlen` truncates `size_t` to `int`
**`src/runtime_support.cpp:172-174`**

`static_cast<int>(hybrid_string_size(str))` truncates strings longer than `INT_MAX`.

**Fix:** Return `size_t` or `int64_t`.

---

### 37. Missing `return` after `LogError` for comma-separated variable declarations
**`src/parser/parser_statements.cpp:57-58`**

After diagnosing the error, execution falls through and produces a confusing second error message.

**Fix:** Add `return nullptr;` after the `LogError` call.

---

### 38. `for` loop with multiplicative step and wrong direction causes infinite loop
**`src/ast/stmts.cpp:1553-1558`**

For descending `for` with `step * 2`, the fallback condition `SGE(VarVal, 1)` is always true for positive values, creating an infinite loop when the multiplicative step goes in the wrong direction.

**Fix:** Check that the step direction matches the loop direction, or compute the correct bound.

---

### 39. `__hybrid_string_from_char32` accepts invalid Unicode codepoints
**`src/runtime_support.cpp:466-497`**

Surrogate codepoints (0xD800-0xDFFF) and codepoints above 0x10FFFF are not rejected, producing invalid UTF-8.

**Fix:** Validate the codepoint range before encoding.

---

### 40. `parameterIsRef` accessed without bounds check
**`src/ast/ast_runtime.cpp:922`, `694`**

`memberInfo.parameterIsRef[idx]` is indexed without verifying `idx < parameterIsRef.size()`. Compare with line 909 which correctly guards this access.

**Fix:** Add a bounds check: `idx < memberInfo.parameterIsRef.size() ? memberInfo.parameterIsRef[idx] : false`.

---
