cmake_minimum_required(VERSION 3.20)
project(Hybrid VERSION 1.0.0 LANGUAGES C CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Set default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Export compile commands for better IDE integration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set LLVM definitions and include directories
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

# Get LLVM components
llvm_map_components_to_libnames(llvm_libs core support)

# Add source directory
add_subdirectory(src)

# Enable testing
enable_testing()

# Option to build tests
option(BUILD_TESTS "Build test suite" ON)

if(BUILD_TESTS)
    # Add test target
    add_custom_target(run-tests
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run_tests.sh
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Running Hybrid test suite"
        VERBATIM
    )
endif()

# Installation rules
install(TARGETS hybrid
    RUNTIME DESTINATION bin
)

# CPack configuration for packaging
set(CPACK_PACKAGE_NAME "Hybrid")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Hybrid Programming Language Compiler")
set(CPACK_PACKAGE_VENDOR "Hybrid Project")
include(CPack)