@echo off
setlocal enabledelayedexpansion

:: Hybrid Compiler Test Runner for Windows
:: Automatically discovers and runs all test files in the test/ directory

:: Colors are limited in Windows CMD, but we can use basic formatting
set RED=[91m
set GREEN=[92m
set YELLOW=[93m
set BLUE=[94m
set NC=[0m

:: Counters
set /a TOTAL_TESTS=0
set /a PASSED_TESTS=0
set /a FAILED_TESTS=0

:: Initialize command line option variables
set VERBOSE_MODE=0
set FAILURES_ONLY=0
set TEST_PATTERN=
set SINGLE_TESTS_ENABLED=1
set FILTER_MODE=all
set FILTER_PATH=
set FILTER_MATCH=

echo %BLUE%Hybrid Compiler Test Suite%NC%
echo ===============================

:: Check for hybrid executable - try multiple locations
set HYBRID_EXEC=
if exist "build\hybrid.exe" (
    set HYBRID_EXEC=build\hybrid.exe
) else if exist "build\Release\hybrid.exe" (
    set HYBRID_EXEC=build\Release\hybrid.exe
) else if exist "build\Debug\hybrid.exe" (
    set HYBRID_EXEC=build\Debug\hybrid.exe
) else if exist "cmake-build-release\hybrid.exe" (
    set HYBRID_EXEC=cmake-build-release\hybrid.exe
) else if exist "cmake-build-debug\hybrid.exe" (
    set HYBRID_EXEC=cmake-build-debug\hybrid.exe
) else (
    echo %RED%Error: hybrid executable not found.%NC%
    echo %RED%Build the project first using:%NC%
    echo %RED%  cmake -B build ^&^& cmake --build build%NC%
    echo %RED%Or use the build script:%NC%
    echo %RED%  build.bat%NC%
    exit /b 1
)

echo Using executable: %HYBRID_EXEC%

:: Create runtime library for test execution if clang is available
set RUNTIME_LIB=
where clang >nul 2>&1
if %errorlevel% equ 0 (
    set RUNTIME_LIB=%TEMP%\hybrid_runtime_%RANDOM%.o

    :: Create runtime C source
    echo #include ^<stdio.h^> > %TEMP%\hybrid_runtime.c
    echo #include ^<stdlib.h^> >> %TEMP%\hybrid_runtime.c
    echo. >> %TEMP%\hybrid_runtime.c
    echo void print^(int x^) { >> %TEMP%\hybrid_runtime.c
    echo     printf^("%%d\n", x^); >> %TEMP%\hybrid_runtime.c
    echo } >> %TEMP%\hybrid_runtime.c

    :: Compile runtime library
    clang -c %TEMP%\hybrid_runtime.c -o !RUNTIME_LIB! 2>nul
    if errorlevel 1 set RUNTIME_LIB=
)

:: Count multi-unit test manifests
set /a MULTI_UNIT_TEST_COUNT=0
if exist "test\multi_unit" (
    for /d %%m in ("test\multi_unit\*") do (
        set has_multi=0
        set "multi_dir=%%~fm"
        if exist "!multi_dir!" (
            for %%f in ("!multi_dir!\*.hy") do (
                if exist "%%~ff" set has_multi=1
            )
        )
        if !has_multi! equ 1 set /a MULTI_UNIT_TEST_COUNT+=1
    )
)

:: Count tests by category
echo Test categories:
for /d %%d in (test\*) do (
    set "category=%%~nd"
    if /i "!category!"=="multi_unit" (
        set count=%MULTI_UNIT_TEST_COUNT%
    ) else (
        set /a count=0
        for /r "%%d" %%f in (*.hy) do (
            set /a count+=1
        )
    )
    if !count! gtr 0 (
        echo   - !category!: !count! tests
    )
)
echo.

:: Parse command line arguments
:parse_args
if "%~1"=="" goto end_parse
if /i "%~1"=="-v" (
    set VERBOSE_MODE=1
    shift
    goto parse_args
)
if /i "%~1"=="--verbose" (
    set VERBOSE_MODE=1
    shift
    goto parse_args
)
if /i "%~1"=="-f" (
    set FAILURES_ONLY=1
    shift
    goto parse_args
)
if /i "%~1"=="--failures-only" (
    set FAILURES_ONLY=1
    shift
    goto parse_args
)
if /i "%~1"=="-h" goto show_help
if /i "%~1"=="--help" goto show_help
set TEST_PATTERN=%~1
shift
goto parse_args

:show_help
echo Usage: %0 [OPTIONS] [TEST_PATTERN]
echo.
echo Options:
echo   -v, --verbose       Show detailed output for each test
echo   -f, --failures-only Only show failing tests
echo   -h, --help          Show this help message
echo.
echo Test Pattern:
echo   Can be a category name, file path, or pattern to match
echo.
echo Examples:
echo   %0                  # Run all tests
echo   %0 -v               # Run all tests with verbose output
echo   %0 structs          # Run all tests in structs category
echo   %0 test_bool        # Run tests matching 'test_bool'
exit /b 0

:end_parse

:: Filter tests if pattern provided
if not "%TEST_PATTERN%"=="" (
    if exist "test\%TEST_PATTERN%\." (
        if /i "%TEST_PATTERN%"=="multi_unit" (
            set SINGLE_TESTS_ENABLED=0
            set FILTER_MODE=none
            echo Running multi-unit manifest tests
        ) else (
            set FILTER_MODE=category
            set FILTER_PATH=test\%TEST_PATTERN%
            echo Running all tests in category: %TEST_PATTERN%
        )
    ) else (
        if exist "%TEST_PATTERN%" (
            set FILTER_MODE=single_path
            set FILTER_PATH=%TEST_PATTERN%
            echo Running specific test file: %TEST_PATTERN%
        ) else (
            if exist "test\%TEST_PATTERN%" (
                set FILTER_MODE=single_path_root
                set FILTER_PATH=test\%TEST_PATTERN%
                echo Running specific test file: test\%TEST_PATTERN%
            ) else (
                set FILTER_MODE=pattern
                set FILTER_MATCH=%TEST_PATTERN%
                echo Running tests matching pattern: %TEST_PATTERN%
            )
        )
    )
    if %VERBOSE_MODE%==1 echo ^(verbose mode enabled^)
    echo.
)

:: Prepare list of single-file tests to run
set "TEST_LIST_FILE=%TEMP%\hybrid_tests_%RANDOM%.lst"
if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1
set /a SINGLE_TEST_COUNT=0

if %SINGLE_TESTS_ENABLED%==1 (
    if /i "%FILTER_MODE%"=="category" (
        for /r "%FILTER_PATH%" %%f in (*.hy) do (
            set "file=%%~ff"
            set "no_multi=!file:\multi_unit\=!"
            if "!no_multi!"=="!file!" (
                >>"%TEST_LIST_FILE%" echo(!file!
                set /a SINGLE_TEST_COUNT+=1
            )
        )
    ) else (
        if /i "%FILTER_MODE%"=="single_path" (
            set "normalized=!FILTER_PATH:/=\!"
            for %%s in ("!normalized!") do (
                if exist "%%~fs" (
                    >>"%TEST_LIST_FILE%" echo(%%~fs
                    set /a SINGLE_TEST_COUNT+=1
                )
            )
        ) else (
            if /i "%FILTER_MODE%"=="single_path_root" (
                set "normalized=!FILTER_PATH:/=\!"
                for %%s in ("!normalized!") do (
                    if exist "%%~fs" (
                        >>"%TEST_LIST_FILE%" echo(%%~fs
                        set /a SINGLE_TEST_COUNT+=1
                    )
                )
            ) else (
                if /i "%FILTER_MODE%"=="pattern" (
                    set "pattern=!FILTER_MATCH!"
                    for /r "test" %%f in (*.hy) do (
                        set "file=%%~ff"
                        set "no_multi=!file:\multi_unit\=!"
                        if "!no_multi!"=="!file!" (
                            set include=0
                            echo %%~nf | findstr /C:"!pattern!" >nul
                            if !errorlevel! equ 0 set include=1
                            if !include! equ 0 (
                                echo !file! | findstr /C:"!pattern!" >nul
                                if !errorlevel! equ 0 set include=1
                            )
                            if !include! equ 1 (
                                >>"%TEST_LIST_FILE%" echo(!file!
                                set /a SINGLE_TEST_COUNT+=1
                            )
                        )
                    )
                ) else (
                    if /i "%FILTER_MODE%"=="none" (
                        rem Multi-unit only, no single-file tests collected
                    ) else (
                        for /r "test" %%f in (*.hy) do (
                            set "file=%%~ff"
                            set "no_multi=!file:\multi_unit\=!"
                            if "!no_multi!"=="!file!" (
                                >>"%TEST_LIST_FILE%" echo(!file!
                                set /a SINGLE_TEST_COUNT+=1
                            )
                        )
                    )
                )
            )
        )
    )
)

if %SINGLE_TESTS_ENABLED%==1 if /i "%FILTER_MODE%"=="pattern" if %SINGLE_TEST_COUNT% equ 0 (
    if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1
    echo %RED%No test files found matching pattern: %TEST_PATTERN%%NC%
    exit /b 1
)

set /a TOTAL_DISCOVERED_TESTS=SINGLE_TEST_COUNT+MULTI_UNIT_TEST_COUNT
echo Found %TOTAL_DISCOVERED_TESTS% total tests to run
echo.

:: Run tests
if %VERBOSE_MODE%==1 (
    echo Running tests in verbose mode...
    echo.
)

:: Run single-file tests from list
if %SINGLE_TEST_COUNT% gtr 0 (
    for /f "usebackq delims=" %%f in ("%TEST_LIST_FILE%") do (
        call :run_test "%%f" "%%~nf"
    )
)

:: Run multi-unit manifest tests
call :run_multi_unit_tests

:: Cleanup collected test list
if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1

:: Cleanup runtime library
if not "%RUNTIME_LIB%"=="" (
    if exist "%RUNTIME_LIB%" del "%RUNTIME_LIB%" >nul 2>&1
    if exist "%TEMP%\hybrid_runtime.c" del "%TEMP%\hybrid_runtime.c" >nul 2>&1
)

:: Print summary
echo ===============================
echo %BLUE%Test Summary%NC%
echo Total tests:  %TOTAL_TESTS%
echo Passed:       %GREEN%%PASSED_TESTS%%NC%
echo Failed:       %RED%%FAILED_TESTS%%NC%

if %FAILURES_ONLY% equ 1 if %FAILED_TESTS% equ 0 (
    echo %GREEN%(No failures shown - all tests passed)%NC%
)

if %FAILED_TESTS% gtr 0 (
    echo %RED%Some tests failed!%NC%
    echo.
    echo To debug failing tests, run:
    echo   %0 -v [test_pattern]
    exit /b 1
) else (
    echo %GREEN%All tests passed!%NC%
    exit /b 0
)

:: Run multi-unit compilation tests
:run_multi_unit_tests
if %MULTI_UNIT_TEST_COUNT% leq 0 goto :eof

echo.
echo %BLUE%Multi-unit Compilation Tests%NC%
echo -------------------------------

for /d %%d in ("test\multi_unit\*") do (
    set "multi_dir=%%~fd"
    if exist "!multi_dir!" (
        set "multi_name=%%~nd"
        set "multi_expect=pass"
        if exist "!multi_dir!\EXPECT_FAIL" set "multi_expect=fail"
        if /i "!multi_name:~-5!"=="_fail" set "multi_expect=fail"

        set "multi_files="
        set /a multi_file_count=0
        for %%f in ("!multi_dir!\*.hy") do (
            if exist "%%~ff" (
                set "multi_files=!multi_files! "%%~ff""
                set /a multi_file_count+=1
            )
        )

        if !multi_file_count! equ 0 (
            if %FAILURES_ONLY% equ 0 (
                echo %YELLOW%Skipping !multi_name! ^(no .hy files^)%NC%
            )
        ) else (
            set "multi_temp=%TEMP%\hybrid_multi_%RANDOM%"
            set "multi_output=!multi_temp!.out"
            set "multi_stdout=!multi_temp!.log"
            cmd /c ""%HYBRID_EXEC%"!multi_files! -o "!multi_output!"" > "!multi_stdout!" 2>&1
            set multi_status=!errorlevel!

            set /a TOTAL_TESTS+=1
            set passed=0
            if /i "!multi_expect!"=="pass" (
                if !multi_status! equ 0 set passed=1
            ) else (
                if not !multi_status! equ 0 set passed=1
            )

            if !passed! equ 1 (
                set /a PASSED_TESTS+=1
                if %FAILURES_ONLY% equ 0 (
                    echo %GREEN%✓ PASSED: !multi_name!%NC%
                )
            ) else (
                set /a FAILED_TESTS+=1
                echo %RED%✗ FAILED: !multi_name!%NC%
                if /i "!multi_expect!"=="pass" (
                    echo %RED%  Expected success but command failed with status !multi_status!%NC%
                ) else (
                    echo %RED%  Expected failure but command succeeded%NC%
                )
                echo --- Output ---
                type "!multi_stdout!"
                echo --------------
            )

            if exist "!multi_output!" del "!multi_output!" >nul 2>&1
            if exist "!multi_stdout!" del "!multi_stdout!" >nul 2>&1
        )
    )
)
goto :eof

:: Function to run a single test
:run_test
setlocal
set test_file=%~1
set test_name=%~2
set test_passed=0
set has_errors=0
set runtime_exit_code=0

:: Run the test and capture output
%HYBRID_EXEC% < "%test_file%" > "%TEMP%\hybrid_test_output.txt" 2>&1
set exit_code=%errorlevel%

:: Read output
set output=
for /f "delims=" %%i in (%TEMP%\hybrid_test_output.txt) do set output=!output!%%i

:: Check for error patterns in output
findstr /i "Error:" "%TEMP%\hybrid_test_output.txt" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

findstr /i "Failed to generate" "%TEMP%\hybrid_test_output.txt" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

findstr /i "Unknown function" "%TEMP%\hybrid_test_output.txt" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

findstr /i "Unknown variable" "%TEMP%\hybrid_test_output.txt" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

findstr /i "invalid binary operator" "%TEMP%\hybrid_test_output.txt" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

findstr /C:"Expected" "%TEMP%\hybrid_test_output.txt" | findstr /C:"after" >nul 2>&1
if !errorlevel! equ 0 set has_errors=1

:: If compilation succeeded and test is not expected to fail, compile and run with clang
echo !test_name! | findstr /i "fail error" >nul
set is_fail_test=!errorlevel!

if !has_errors! equ 0 if !exit_code! equ 0 if !is_fail_test! neq 0 (
    if not "%RUNTIME_LIB%"=="" if exist "%RUNTIME_LIB%" (
        where clang >nul 2>&1
        if !errorlevel! equ 0 (
            :: Extract final LLVM module from output
            set temp_ir=%TEMP%\hybrid_test_%RANDOM%.ll
            set temp_bin=%TEMP%\hybrid_bin_%RANDOM%.exe

            :: Find line with "=== Final Generated" and extract everything after
            findstr /n "^" "%TEMP%\hybrid_test_output.txt" | findstr /c:"=== Final Generated" > "%TEMP%\module_line.txt"
            set /p module_line_num=<"%TEMP%\module_line.txt"
            set module_line_num=!module_line_num::= !
            for /f "tokens=1" %%a in ("!module_line_num!") do set module_start=%%a
            set /a module_start+=1

            :: Extract clean IR (skip REPL prompts and Generated messages)
            (for /f "skip=!module_start! delims=" %%i in (%TEMP%\hybrid_test_output.txt) do (
                set line=%%i
                echo !line! | findstr /b /c:"ready>" /c:"Parsed" /c:"Generated" >nul
                if !errorlevel! neq 0 (
                    echo !line!
                )
            )) > "!temp_ir!"

            :: Compile IR to binary with clang
            clang "!temp_ir!" "%RUNTIME_LIB%" -o "!temp_bin!" >nul 2>&1
            if !errorlevel! equ 0 (
                :: Execute the binary
                "!temp_bin!" >nul 2>&1
                set runtime_exit_code=!errorlevel!

                :: Check for SIGABRT equivalent on Windows (typically STATUS_ACCESS_VIOLATION = 3221225477 or -1073741819)
                :: On Windows, abort() typically returns 3
                if !runtime_exit_code! equ 3 set has_errors=1
                if !runtime_exit_code! equ -1073741819 set has_errors=1
            )

            :: Clean up temp files
            if exist "!temp_ir!" del "!temp_ir!" >nul 2>&1
            if exist "!temp_bin!" del "!temp_bin!" >nul 2>&1
            if exist "%TEMP%\module_line.txt" del "%TEMP%\module_line.txt" >nul 2>&1
        )
    )
)

:: Determine if test passed or failed
echo !test_name! | findstr /i "fail error" >nul
if !errorlevel! equ 0 (
    :: Test expected to fail
    if !has_errors! equ 1 (
        set test_passed=1
    ) else if !exit_code! neq 0 (
        set test_passed=1
    )
) else (
    :: Normal test should not have errors
    if !has_errors! equ 0 if !exit_code! equ 0 (
        set test_passed=1
    )
)

:: Update counters (must be done before endlocal)
set /a TOTAL_TESTS+=1
if !test_passed! equ 1 (
    set /a PASSED_TESTS+=1
) else (
    set /a FAILED_TESTS+=1
)

:: Only show output if not in failures-only mode, or if test failed
if %FAILURES_ONLY% equ 0 (
    set show_output=1
) else if !test_passed! equ 0 (
    set show_output=1
) else (
    set show_output=0
)

if !show_output! equ 1 (
    echo %YELLOW%Running test: !test_name!%NC%
    echo ----------------------------------------

    echo !test_name! | findstr /i "fail error" >nul
    if !errorlevel! equ 0 (
        :: Test expected to fail
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name! ^(correctly failed as expected^)%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name! ^(should have failed but didn't^)%NC%
        )
    ) else (
        :: Normal test
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name!%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name!%NC%
            if !exit_code! neq 0 (
                echo %RED%  Compilation exit code: !exit_code!%NC%
            )
            if !runtime_exit_code! neq 0 (
                echo %RED%  Runtime exit code: !runtime_exit_code! ^(possible assert failure or abort^)%NC%
            )
            if !has_errors! equ 1 (
                echo %RED%  Errors found in output:%NC%
                findstr /i "Error: Failed Unknown invalid Expected" "%TEMP%\hybrid_test_output.txt" 2>nul | findstr /n "^" | findstr "^[1-3]:"
            )
        )
    )
    echo.
)

:: Cleanup temp output file
if exist "%TEMP%\hybrid_test_output.txt" del "%TEMP%\hybrid_test_output.txt" >nul 2>&1

endlocal & set /a TOTAL_TESTS=%TOTAL_TESTS% & set /a PASSED_TESTS=%PASSED_TESTS% & set /a FAILED_TESTS=%FAILED_TESTS%
goto :eof
