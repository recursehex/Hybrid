#!/bin/bash

set -euo pipefail

if [ "$(uname -s)" != "Linux" ]; then
    echo "Error: scripts/run_arc_valgrind.sh currently supports Linux only." >&2
    exit 1
fi

if ! command -v valgrind >/dev/null 2>&1; then
    echo "Error: valgrind not found in PATH." >&2
    exit 1
fi

BUILD_DIR="${HYBRID_VALGRIND_BUILD_DIR:-build-valgrind}"

if [ $# -gt 0 ]; then
    TEST_PATTERNS=("$@")
else
    TEST_PATTERNS=("arc/memory" "arc/arc_off" "arc/debug" "errors/arc")
fi

LLVM_DIR=""
if command -v llvm-config >/dev/null 2>&1; then
    LLVM_PREFIX=$(llvm-config --prefix)
    LLVM_DIR="$LLVM_PREFIX/lib/cmake/llvm"
else
    echo "Error: llvm-config not found in PATH." >&2
    exit 1
fi

CLANG_BIN="${CC:-}"
CLANGXX_BIN="${CXX:-}"
if [ -z "$CLANG_BIN" ]; then
    CLANG_BIN=$(command -v clang || true)
fi
if [ -z "$CLANGXX_BIN" ]; then
    CLANGXX_BIN=$(command -v clang++ || true)
fi

if [ -z "$CLANG_BIN" ] || [ -z "$CLANGXX_BIN" ]; then
    echo "Error: clang/clang++ not found in PATH." >&2
    exit 1
fi

echo "Configuring valgrind build in ${BUILD_DIR}..."
cmake -B "$BUILD_DIR" \
    -DCMAKE_BUILD_TYPE=Debug \
    -DBUILD_TESTS=ON \
    -DLLVM_DIR="$LLVM_DIR" \
    -DCMAKE_C_COMPILER="$CLANG_BIN" \
    -DCMAKE_CXX_COMPILER="$CLANGXX_BIN" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

cmake --build "$BUILD_DIR" -j"$(nproc)"

if [ ! -x "$BUILD_DIR/hybrid" ]; then
    echo "Error: missing executable '$BUILD_DIR/hybrid' after build." >&2
    exit 1
fi

for pattern in "${TEST_PATTERNS[@]}"; do
    echo
    echo "Running valgrind ARC tests: ${pattern}"
    HYBRID_EXEC="./${BUILD_DIR}/hybrid" \
    HYBRID_TEST_VALGRIND=1 \
    ./run_tests.sh --valgrind "$pattern"
done
