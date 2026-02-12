@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: Hybrid Compiler Build Script for Windows
:: Mirrors the behaviour of build.sh

:: ANSI color markers (kept minimal for portability)
set BLUE=[94m
set GREEN=[92m
set YELLOW=[93m
set RED=[91m
set NC=[0m

:: Defaults
set BUILD_TYPE=Release
set BUILD_DIR=build
set CLEAN_BUILD=0
set RUN_TESTS=0
set VERBOSE_BUILD=0

:: ---------------------------------------------------------------------------
:: Helpers
:: ---------------------------------------------------------------------------
:show_help
echo Hybrid Compiler Build Script
echo Usage: build.bat [OPTIONS]
echo.
echo Options:
echo   -d, --debug       Build in Debug mode ^(default: Release^)
echo   -c, --clean       Remove the build directory before configuring
echo   -t, --test        Run the test suite after building
echo   -v, --verbose     Enable verbose build output
echo   -h, --help        Show this help message
echo.
echo Examples:
echo   build.bat              ^# Build in Release mode
echo   build.bat -d           ^# Build in Debug mode
echo   build.bat -c -t        ^# Clean build and run tests
echo   build.bat -d -v        ^# Debug build with verbose output
exit /b 0

:: ---------------------------------------------------------------------------
:: Argument parsing
:: ---------------------------------------------------------------------------
:parse_args
if "%~1"=="" goto args_done
if /i "%~1"=="-d" (
    set BUILD_TYPE=Debug
) else if /i "%~1"=="--debug" (
    set BUILD_TYPE=Debug
) else if /i "%~1"=="-c" (
    set CLEAN_BUILD=1
) else if /i "%~1"=="--clean" (
    set CLEAN_BUILD=1
) else if /i "%~1"=="-t" (
    set RUN_TESTS=1
) else if /i "%~1"=="--test" (
    set RUN_TESTS=1
) else if /i "%~1"=="-v" (
    set VERBOSE_BUILD=1
) else if /i "%~1"=="--verbose" (
    set VERBOSE_BUILD=1
) else if /i "%~1"=="-h" (
    call :show_help
    exit /b 0
) else if /i "%~1"=="--help" (
    call :show_help
    exit /b 0
) else (
    echo %RED%Unknown option: %~1%NC%
    echo Use -h or --help for usage information.
    exit /b 1
)
shift
goto parse_args
:args_done

echo %BLUE%Hybrid Compiler Build System%NC%
echo ==============================
echo Build Type: %BUILD_TYPE%
echo Build Directory: %BUILD_DIR%
echo.

:: ---------------------------------------------------------------------------
:: Locate LLVM
:: ---------------------------------------------------------------------------
set "PRESET_LLVM_DIR=%LLVM_DIR%"
set "LLVM_DIR="
where llvm-config >nul 2>nul
if %errorlevel%==0 (
    for /f "usebackq delims=" %%i in (`llvm-config --prefix 2^>nul`) do (
        if not defined LLVM_DIR set "LLVM_DIR=%%~fi\lib\cmake\llvm"
    )
)
if not defined LLVM_DIR (
    if exist "%ProgramFiles%\LLVM\lib\cmake\llvm" (
        set "LLVM_DIR=%ProgramFiles%\LLVM\lib\cmake\llvm"
    ) else if defined ProgramFiles(x86) if exist "%ProgramFiles(x86)%\LLVM\lib\cmake\llvm" (
        set "LLVM_DIR=%ProgramFiles(x86)%\LLVM\lib\cmake\llvm"
    )
)
if not defined LLVM_DIR if defined PRESET_LLVM_DIR (
    set "LLVM_DIR=%PRESET_LLVM_DIR%"
)

if not defined LLVM_DIR (
    echo %RED%Error: LLVM not found!%NC%
    echo Install LLVM and ensure llvm-config or LLVM_DIR is available.
    echo Suggested installation: https://releases.llvm.org/download.html
    exit /b 1
)

echo Found LLVM at: %LLVM_DIR%

:: ---------------------------------------------------------------------------
:: Locate clang/clang++
:: ---------------------------------------------------------------------------
set "CLANG_BIN="
set "CLANGXX_BIN="
if exist "%ProgramFiles%\LLVM\bin\clang.exe" (
    set "CLANG_BIN=%ProgramFiles%\LLVM\bin\clang.exe"
    set "CLANGXX_BIN=%ProgramFiles%\LLVM\bin\clang++.exe"
) else (
    where clang >nul 2>nul
    if %errorlevel%==0 (
        for /f "usebackq delims=" %%i in (`where clang 2^>nul`) do (
            if not defined CLANG_BIN set "CLANG_BIN=%%~fi"
        )
        for /f "usebackq delims=" %%i in (`where clang++ 2^>nul`) do (
            if not defined CLANGXX_BIN set "CLANGXX_BIN=%%~fi"
        )
    )
)
if defined CLANG_BIN (
    echo Using C compiler: %CLANG_BIN%
)
if defined CLANGXX_BIN (
    echo Using C++ compiler: %CLANGXX_BIN%
)
echo.

:: ---------------------------------------------------------------------------
:: Clean
:: ---------------------------------------------------------------------------
if %CLEAN_BUILD%==1 (
    echo %YELLOW%Cleaning build directory...%NC%
    if exist "%BUILD_DIR%" rd /s /q "%BUILD_DIR%"
    echo.
)

:: ---------------------------------------------------------------------------
:: Configure
:: ---------------------------------------------------------------------------
set "GENERATOR_ARG="
where ninja >nul 2>nul
if %errorlevel%==0 (
    set "GENERATOR_ARG=-G Ninja"
)

echo %YELLOW%Configuring CMake...%NC%
set "CONFIGURE_CMD=cmake -B \"%BUILD_DIR%\" %GENERATOR_ARG% -DCMAKE_BUILD_TYPE=%BUILD_TYPE% -DLLVM_DIR=\"%LLVM_DIR%\" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
if defined CLANG_BIN (
    set "CONFIGURE_CMD=%CONFIGURE_CMD% -DCMAKE_C_COMPILER=\"%CLANG_BIN%\""
)
if defined CLANGXX_BIN (
    set "CONFIGURE_CMD=%CONFIGURE_CMD% -DCMAKE_CXX_COMPILER=\"%CLANGXX_BIN%\""
)

call %CONFIGURE_CMD%
if errorlevel 1 (
    echo %RED%CMake configuration failed!%NC%
    exit /b 1
)
echo.

:: ---------------------------------------------------------------------------
:: Build
:: ---------------------------------------------------------------------------
set "JOBS=%NUMBER_OF_PROCESSORS%"
if not defined JOBS set JOBS=1

echo %YELLOW%Building...%NC%
if defined GENERATOR_ARG (
    if %VERBOSE_BUILD%==1 (
        cmake --build "%BUILD_DIR%" --parallel %JOBS% --verbose
    ) else (
        cmake --build "%BUILD_DIR%" --parallel %JOBS%
    )
) else (
    if %VERBOSE_BUILD%==1 (
        cmake --build "%BUILD_DIR%" --config %BUILD_TYPE% --parallel %JOBS% --verbose
    ) else (
        cmake --build "%BUILD_DIR%" --config %BUILD_TYPE% --parallel %JOBS%
    )
)
if errorlevel 1 (
    echo %RED%Build failed!%NC%
    exit /b 1
)

echo.
echo %GREEN%Build completed successfully!%NC%
if exist "%BUILD_DIR%\hybrid.exe" (
    echo Executable: %BUILD_DIR%\hybrid.exe
) else (
    echo Executable: %BUILD_DIR%\hybrid
)

:: ---------------------------------------------------------------------------
:: Run tests
:: ---------------------------------------------------------------------------
if %RUN_TESTS%==1 (
    echo.
    echo %YELLOW%Running tests...%NC%
    call run_tests.bat
    if errorlevel 1 (
        echo %RED%Tests failed!%NC%
        exit /b 1
    )
)

echo.
echo %GREEN%Done!%NC%
exit /b 0
