CXX = clang++
# Try to find llvm-config in PATH first, otherwise use Homebrew location
LLVM_CONFIG := $(shell command -v llvm-config 2> /dev/null || echo /opt/homebrew/opt/llvm/bin/llvm-config)
CXXFLAGS = -g -O3 $(shell $(LLVM_CONFIG) --cxxflags)
LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags --system-libs --libs core)
TARGET = hybrid
SOURCES = src/driver.cpp src/lexer.cpp src/parser.cpp src/ast.cpp src/toplevel.cpp

$(TARGET): $(SOURCES)
	$(CXX) $(CXXFLAGS) $(SOURCES) $(LDFLAGS) -o $(TARGET)

clean:
	rm -f $(TARGET)

.PHONY: clean