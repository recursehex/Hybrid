@echo off
setlocal enabledelayedexpansion

:: Build script for Hybrid on Windows
:: Usage: build.bat [options]
::   -d, --debug    Build in debug mode (default: release)
::   -c, --clean    Clean build directory before building
::   -t, --test     Run tests after building
::   -h, --help     Display this help message

set BUILD_TYPE=Release
set CLEAN_BUILD=0
set RUN_TESTS=0

:: Parse command line arguments
:parse_args
if "%1"=="" goto end_parse
if "%1"=="-d" goto set_debug
if "%1"=="--debug" goto set_debug
if "%1"=="-c" goto set_clean
if "%1"=="--clean" goto set_clean
if "%1"=="-t" goto set_test
if "%1"=="--test" goto set_test
if "%1"=="-h" goto show_help
if "%1"=="--help" goto show_help
goto invalid_arg

:set_debug
set BUILD_TYPE=Debug
shift
goto parse_args

:set_clean
set CLEAN_BUILD=1
shift
goto parse_args

:set_test
set RUN_TESTS=1
shift
goto parse_args

:show_help
echo Build script for Hybrid on Windows
echo Usage: build.bat [options]
echo   -d, --debug    Build in debug mode (default: release)
echo   -c, --clean    Clean build directory before building
echo   -t, --test     Run tests after building
echo   -h, --help     Display this help message
exit /b 0

:invalid_arg
echo Invalid argument: %1
echo Use -h or --help for usage information
exit /b 1

:end_parse

:: Set build preset based on build type
if "%BUILD_TYPE%"=="Debug" (
    set PRESET=debug
) else (
    set PRESET=release
)

echo Building Hybrid in %BUILD_TYPE% mode...

:: Clean if requested
if %CLEAN_BUILD%==1 (
    echo Cleaning build directory...
    if exist build rd /s /q build
)

:: Check for Ninja
where ninja >nul 2>nul
if errorlevel 1 (
    echo Warning: Ninja not found. Using default generator.
    echo For better performance, install Ninja: https://ninja-build.org/
)

:: Configure with CMake
echo Configuring with CMake preset: %PRESET%
cmake --preset=%PRESET%
if errorlevel 1 (
    echo CMake configuration failed!
    exit /b 1
)

:: Build
echo Building...
cmake --build --preset=%PRESET%
if errorlevel 1 (
    echo Build failed!
    exit /b 1
)

echo Build successful!

:: Run tests if requested
if %RUN_TESTS%==1 (
    echo Running tests...
    call run_tests.bat
    if errorlevel 1 (
        echo Some tests failed!
        exit /b 1
    )
)

echo Done!
exit /b 0