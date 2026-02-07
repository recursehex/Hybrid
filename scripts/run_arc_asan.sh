#!/bin/bash

set -euo pipefail

BUILD_DIR="${HYBRID_ASAN_BUILD_DIR:-build-asan}"

if [ $# -gt 0 ]; then
    TEST_PATTERNS=("$@")
else
    TEST_PATTERNS=("arc/memory" "arc/arc_off" "arc/debug" "errors/arc")
fi

LLVM_DIR=""
if command -v llvm-config >/dev/null 2>&1; then
    LLVM_PREFIX=$(llvm-config --prefix)
    LLVM_DIR="$LLVM_PREFIX/lib/cmake/llvm"
elif [ -d "/opt/homebrew/opt/llvm/lib/cmake/llvm" ]; then
    LLVM_DIR="/opt/homebrew/opt/llvm/lib/cmake/llvm"
else
    echo "Error: LLVM not found (expected llvm-config or /opt/homebrew/opt/llvm)." >&2
    exit 1
fi

CLANG_BIN=""
CLANGXX_BIN=""
if [ -d "/opt/homebrew/opt/llvm/bin" ]; then
    CLANG_BIN="/opt/homebrew/opt/llvm/bin/clang"
    CLANGXX_BIN="/opt/homebrew/opt/llvm/bin/clang++"
elif command -v clang >/dev/null 2>&1; then
    CLANG_BIN=$(command -v clang)
    CLANGXX_BIN=$(command -v clang++)
fi

if [ "$(uname -s)" = "Darwin" ] && [ -z "${SDKROOT:-}" ]; then
    SDKROOT=$(xcrun --show-sdk-path 2>/dev/null || true)
fi

echo "Configuring ASan build in ${BUILD_DIR}..."
CMAKE_ARGS=(
    -B "$BUILD_DIR"
    -DCMAKE_BUILD_TYPE=Debug
    -DHYBRID_ENABLE_ASAN=ON
    -DBUILD_TESTS=ON
    -DLLVM_DIR="$LLVM_DIR"
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
)

if [ -n "$CLANG_BIN" ]; then
    CMAKE_ARGS+=(-DCMAKE_C_COMPILER="$CLANG_BIN")
fi
if [ -n "$CLANGXX_BIN" ]; then
    CMAKE_ARGS+=(-DCMAKE_CXX_COMPILER="$CLANGXX_BIN")
fi
if [ -n "${SDKROOT:-}" ]; then
    CMAKE_ARGS+=(-DCMAKE_OSX_SYSROOT="$SDKROOT")
fi

cmake "${CMAKE_ARGS[@]}"
cmake --build "$BUILD_DIR" -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu)"

if [ ! -x "$BUILD_DIR/hybrid" ]; then
    echo "Error: missing executable '$BUILD_DIR/hybrid' after build." >&2
    exit 1
fi

export ASAN_OPTIONS="${ASAN_OPTIONS:-abort_on_error=1:detect_leaks=0:symbolize=1}"

for pattern in "${TEST_PATTERNS[@]}"; do
    echo
    echo "Running ASan ARC tests: ${pattern}"
    HYBRID_EXEC="./${BUILD_DIR}/hybrid" \
    HYBRID_TEST_SANITIZER=address \
    ./run_tests.sh --asan "$pattern"
done
