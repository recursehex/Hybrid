#!/bin/bash

# Hybrid Compiler Build Script
# Provides convenient build commands for CMake

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
BUILD_TYPE="Release"
BUILD_DIR="build"
CLEAN=0
RUN_TESTS=0
VERBOSE=0

# Help function
show_help() {
    echo "Hybrid Compiler Build Script"
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -d, --debug       Build in Debug mode (default: Release)"
    echo "  -c, --clean       Clean build directory before building"
    echo "  -t, --test        Run tests after building"
    echo "  -v, --verbose     Enable verbose build output"
    echo "  -h, --help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                # Build in Release mode"
    echo "  $0 -d             # Build in Debug mode"
    echo "  $0 -c -t          # Clean build and run tests"
    echo "  $0 -d -v          # Debug build with verbose output"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        -c|--clean)
            CLEAN=1
            shift
            ;;
        -t|--test)
            RUN_TESTS=1
            shift
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            show_help
            exit 1
            ;;
    esac
done

echo -e "${BLUE}Hybrid Compiler Build System${NC}"
echo "=============================="
echo "Build Type: $BUILD_TYPE"
echo "Build Directory: $BUILD_DIR"
echo ""

# Find LLVM
LLVM_DIR=""
if command -v llvm-config &> /dev/null; then
    LLVM_PREFIX=$(llvm-config --prefix)
    LLVM_DIR="$LLVM_PREFIX/lib/cmake/llvm"
elif [ -d "/opt/homebrew/opt/llvm" ]; then
    LLVM_DIR="/opt/homebrew/opt/llvm/lib/cmake/llvm"
else
    echo -e "${RED}Error: LLVM not found!${NC}"
    echo "Please install LLVM first:"
    echo "  macOS: brew install llvm"
    echo "  Linux: apt-get install llvm-dev"
    exit 1
fi

echo "Found LLVM at: $LLVM_DIR"
echo ""

# Clean if requested
if [ $CLEAN -eq 1 ]; then
    echo -e "${YELLOW}Cleaning build directory...${NC}"
    rm -rf "$BUILD_DIR"
    echo ""
fi

# Configure
echo -e "${YELLOW}Configuring CMake...${NC}"
cmake -B "$BUILD_DIR" \
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
    -DLLVM_DIR="$LLVM_DIR" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

echo ""

# Build
echo -e "${YELLOW}Building...${NC}"
if [ $VERBOSE -eq 1 ]; then
    cmake --build "$BUILD_DIR" --config "$BUILD_TYPE" --verbose -j$(nproc 2>/dev/null || sysctl -n hw.ncpu)
else
    cmake --build "$BUILD_DIR" --config "$BUILD_TYPE" -j$(nproc 2>/dev/null || sysctl -n hw.ncpu)
fi

echo ""
echo -e "${GREEN}Build completed successfully!${NC}"
echo "Executable: $BUILD_DIR/hybrid"

# Run tests if requested
if [ $RUN_TESTS -eq 1 ]; then
    echo ""
    echo -e "${YELLOW}Running tests...${NC}"
    ./run_tests.sh
fi