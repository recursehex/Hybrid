# Define source files
set(HYBRID_SOURCES
    driver.cpp
    lexer.cpp
    parser.cpp
    ast.cpp
    toplevel.cpp
    compiler_session.cpp
    runtime_support.cpp
    memory/ref_count.cpp
    runtime/arc.cpp
    runtime/weak_table.cpp
    analysis/lifetime.cpp
    analysis/cycles.cpp
    analysis/semantics.cpp
    analysis/smart_pointers.cpp
    optimizer/arc_optimizer.cpp
    optimizer/escape_analysis.cpp
)

# Define header files
set(HYBRID_HEADERS
    lexer.h
    parser.h
    ast.h
    toplevel.h
    compiler_session.h
    codegen_context.h
)

# Create the executable
add_executable(hybrid ${HYBRID_SOURCES} ${HYBRID_HEADERS})

# Build reusable runtime archives for test execution.
add_library(hybrid_runtime_test STATIC
    runtime_support.cpp
    runtime/arc.cpp
    runtime/weak_table.cpp
    memory/ref_count.cpp
)

add_library(hybrid_test_stub STATIC
    ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/test_runtime_stub.c
)

target_include_directories(hybrid_runtime_test PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/include)
target_include_directories(hybrid_test_stub PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/include)

target_compile_features(hybrid_runtime_test PRIVATE cxx_std_17)

set_target_properties(hybrid_runtime_test PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)
set_target_properties(hybrid_test_stub PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)

target_compile_definitions(hybrid PRIVATE
    HYBRID_RUNTIME_SUPPORT_SOURCE="${CMAKE_CURRENT_SOURCE_DIR}/runtime_support.cpp"
    HYBRID_ARC_RUNTIME_SOURCE="${CMAKE_CURRENT_SOURCE_DIR}/runtime/arc.cpp"
    HYBRID_WEAK_TABLE_SOURCE="${CMAKE_CURRENT_SOURCE_DIR}/runtime/weak_table.cpp"
    HYBRID_REFCOUNT_SOURCE="${CMAKE_CURRENT_SOURCE_DIR}/memory/ref_count.cpp"
    HYBRID_RUNTIME_INCLUDE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/../runtime/include"
    HYBRID_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}")

# Link LLVM libraries
target_link_libraries(hybrid PRIVATE ${llvm_libs} c++)

# Set compile options
target_compile_options(hybrid PRIVATE
    $<$<CONFIG:Debug>:-g -O0>
    $<$<CONFIG:Release>:-O3>
    $<$<CONFIG:RelWithDebInfo>:-g -O2>
)

# Set warning flags
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
    target_compile_options(hybrid PRIVATE
        -Wall
        -Wextra
        -Wpedantic
        -Wno-unused-parameter
        -Wno-unused-variable
    )
elseif(MSVC)
    target_compile_options(hybrid PRIVATE
        /W4
        /wd4100  # unreferenced formal parameter
        /wd4101  # unreferenced local variable
        /wd4996  # deprecated functions
    )
    # Set Windows subsystem to console
    set_target_properties(hybrid PROPERTIES
        LINK_FLAGS "/SUBSYSTEM:CONSOLE"
    )
endif()

# Add current directory to include path
target_include_directories(hybrid PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../runtime/include)

# Set executable output directory
set_target_properties(hybrid PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)

# Generate debug symbols for macOS
if(APPLE AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    set_target_properties(hybrid PROPERTIES
        XCODE_ATTRIBUTE_DEBUG_INFORMATION_FORMAT "dwarf-with-dsym"
        XCODE_ATTRIBUTE_DWARF_DSYM_FOLDER_PATH "${CMAKE_BINARY_DIR}"
    )
endif()
