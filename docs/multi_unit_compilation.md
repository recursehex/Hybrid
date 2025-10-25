# Multi-Unit Compilation

Hybrid's front-end routes all parser, lexer, and code generation state through
`CompilerSession`. You can feed multiple source files to a single driver
invocation, and the session will track shared types while clearing per-unit
scratch state.

```
hybrid module_a.hy module_b.hy -o my_program
```

The CLI mirrors `clang` semantics:

- List as many `.hy` files as you want; they are parsed in the order specified.
- `-o <path>` selects the output. If the path ends in `.ll`, `.bc`, or is `-`,
  the driver emits LLVM IR. Otherwise it produces a native binary (default
  `a.out`).
- `--emit-llvm` forces IR emission regardless of the extension. Combine it with
  `-o -` to stream to stdout.

## Manual Harness Verification

You can still drive the pipeline manually from C++ for experimentation:

```cpp
#include "compiler_session.h"
#include "parser.h"
#include "toplevel.h"

static void runSource(const std::string &source) {
  CompilerSession session;
  session.resetAll();
  pushCompilerSession(session);

  currentLexer().setInputBuffer(source);
  InitializeModule();

  getNextToken();
  MainLoop();

  popCompilerSession();
}

int main() {
  runSource("struct Foo { int x; }\n");
  runSource("Foo value;\n"); // emits an error: Foo is undefined in the second unit
}
```

The second run reports an undefined type, proving that per-unit state is reset.

## Automated Regression

The test suite scans `test/multi_unit/` for subdirectories. Every directory is
treated as a single compilation command: all `.hy` files inside are passed to
`hybrid` together in lexical order. Directories ending in `_fail` (or containing
an `EXPECT_FAIL` file) are expected to fail; all others must succeed. Run just
those tests with:

```
./run_tests.sh multi_unit
```

This approach keeps multi-file scenarios under continuous coverage without the
need for manifest files.
