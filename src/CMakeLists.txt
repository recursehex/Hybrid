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
    analysis/lifetime.cpp
    analysis/cycles.cpp
    analysis/semantics.cpp
    analysis/smart_pointers.cpp
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

# Link LLVM libraries
target_link_libraries(hybrid PRIVATE ${llvm_libs})

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
target_include_directories(hybrid PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

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
