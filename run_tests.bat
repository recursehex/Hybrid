@echo off
setlocal EnableExtensions EnableDelayedExpansion

:: Hybrid Compiler Test Runner for Windows
:: Mirrors the behavior of run_tests.sh for single-file and multi-unit suites

rem Colors are limited in Windows CMD, but keep the same markers for parity
set RED=[91m
set GREEN=[92m
set YELLOW=[93m
set BLUE=[94m
set NC=[0m

rem Counters
set /a TOTAL_TESTS=0
set /a PASSED_TESTS=0
set /a FAILED_TESTS=0

rem Timing for runtime-enabled tests
set /a TIMED_DURATION=0
set /a TIMED_TESTS=0

rem Command-line flags
set VERBOSE_MODE=0
set FAILURES_ONLY=1
set TEST_PATTERN=
set RUN_MULTI_UNIT_TESTS=1
set MULTI_UNIT_FILTER=
set SINGLE_TESTS_ENABLED=1
set EXTRA_COMPILER_ARGS=
set FORCE_VERBOSE_FROM_ENV=0
set VERBOSE_FROM_ARGS=0
set ARC_RUNTIME_REQUIRED=0

rem Progress tracking for failures-only mode
set PROGRESS_BAR_WIDTH=40
set /a PROGRESS_COUNT=0
set /a PROGRESS_TOTAL=0
set PROGRESS_VISIBLE=0

echo %BLUE%Hybrid Compiler Test Suite%NC%
echo ===============================

call :locate_hybrid_executable
if errorlevel 1 (
    endlocal & exit /b 1
)

call :configure_env_flags
call :prepare_runtime_library

call :recount_multi_unit ""
set "BASE_MULTI_UNIT_COUNT=%MULTI_UNIT_TEST_COUNT%"

echo Test categories:
for /d %%d in (test\*) do (
    set "category=%%~nd"
    if /i "!category!"=="multi_unit" (
        set count=%BASE_MULTI_UNIT_COUNT%
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

rem -----------------------------
rem Parse command line arguments
rem -----------------------------
:parse_args
if "%~1"=="" goto end_parse
if /i "%~1"=="-v" (
    set VERBOSE_MODE=1
    set VERBOSE_FROM_ARGS=1
    shift
    goto parse_args
)
if /i "%~1"=="--verbose" (
    set VERBOSE_MODE=1
    set VERBOSE_FROM_ARGS=1
    shift
    goto parse_args
)
if /i "%~1"=="-f" (
    set FAILURES_ONLY=0
    shift
    goto parse_args
)
if /i "%~1"=="--full" (
    set FAILURES_ONLY=0
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

set "TEST_PATTERN=%~1"
shift
goto parse_args

:show_help
echo Usage: %0 [OPTIONS] [TEST_PATTERN]
echo.
echo Options:
echo   -v, --verbose       Show detailed output for each test
echo   -f, --full          Show full output for each test
echo       --failures-only Show only failing tests ^(default^)
echo   -h, --help          Show this help message
echo.
echo Test Pattern:
echo   Can be a category name, file path, or pattern to match
echo.
echo Examples:
echo   %0                  ^# Run all tests ^(compact mode^)
echo   %0 -v               ^# Run all tests with verbose output
echo   %0 -f               ^# Run all tests with full output
echo   %0 structs          ^# Run all tests in structs category
echo   %0 test_bool        ^# Run tests matching 'test_bool'
echo   %0 test\types\bool.hy  ^# Run specific test file
exit /b 0

:end_parse

rem Normalize multi-unit filters like multi_unit/suite_name
if defined TEST_PATTERN (
    set "pattern_normalized=%TEST_PATTERN:/=\%"
    if /i "!pattern_normalized:~0,11!"=="multi_unit\" (
        set "MULTI_UNIT_FILTER=!pattern_normalized:~11!"
        set "TEST_PATTERN=multi_unit"
    )
)

if %FORCE_VERBOSE_FROM_ENV%==1 if %VERBOSE_FROM_ARGS%==0 set VERBOSE_MODE=1

call :recount_multi_unit "%MULTI_UNIT_FILTER%"

rem -----------------------------
rem Collect test list
rem -----------------------------
set "TEST_LIST_FILE=%TEMP%\hybrid_tests_%RANDOM%.lst"
if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1
set /a SINGLE_TEST_COUNT=0

if defined TEST_PATTERN (
    set RUN_MULTI_UNIT_TESTS=0
    if exist "test\%TEST_PATTERN%\." (
        if /i "%TEST_PATTERN%"=="multi_unit" (
            set SINGLE_TESTS_ENABLED=0
            set RUN_MULTI_UNIT_TESTS=1
            if defined MULTI_UNIT_FILTER (
                echo Running multi-unit manifest tests ^(filtered to '!MULTI_UNIT_FILTER!'^)
            ) else (
                echo Running multi-unit manifest tests
            )
        ) else (
            for /r "test\%TEST_PATTERN%" %%f in (*.hy) do (
                set "file=%%~ff"
                set "no_multi=!file:\multi_unit\=!"
                if "!no_multi!"=="!file!" (
                    >>"%TEST_LIST_FILE%" echo(!file!
                    set /a SINGLE_TEST_COUNT+=1
                )
            )
            echo Running all tests in category: %TEST_PATTERN%
        )
    ) else if exist "%TEST_PATTERN%" (
        set "normalized=%TEST_PATTERN:/=\%"
        if exist "!normalized!" (
            >>"%TEST_LIST_FILE%" echo(!normalized!
            set /a SINGLE_TEST_COUNT+=1
            echo Running specific test file: %TEST_PATTERN%
        )
    ) else if exist "test\%TEST_PATTERN%" (
        set "normalized=test\%TEST_PATTERN%"
        set "normalized=!normalized:/=\!"
        if exist "!normalized!" (
            >>"%TEST_LIST_FILE%" echo(!normalized!
            set /a SINGLE_TEST_COUNT+=1
            echo Running specific test file: test\%TEST_PATTERN%
        )
    ) else (
        echo Running tests matching pattern: %TEST_PATTERN%
        for /r "test" %%f in (*%TEST_PATTERN%*.hy) do (
            set "file=%%~ff"
            set "no_multi=!file:\multi_unit\=!"
            if "!no_multi!"=="!file!" (
                >>"%TEST_LIST_FILE%" echo(!file!
                set /a SINGLE_TEST_COUNT+=1
            )
        )
        if !SINGLE_TEST_COUNT! equ 0 (
            echo %RED%No test files found matching pattern: %TEST_PATTERN%%NC%
            goto finish_with_error
        )
    )
    if %VERBOSE_MODE%==1 echo ^(verbose mode enabled^)
    echo.
) else (
    if %SINGLE_TESTS_ENABLED%==1 (
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

set /a EFFECTIVE_MULTI_UNIT_COUNT=MULTI_UNIT_TEST_COUNT
if not "%RUN_MULTI_UNIT_TESTS%"=="1" set /a EFFECTIVE_MULTI_UNIT_COUNT=0
set /a TOTAL_DISCOVERED_TESTS=SINGLE_TEST_COUNT+EFFECTIVE_MULTI_UNIT_COUNT
echo Found %TOTAL_DISCOVERED_TESTS% total tests to run
echo.

set /a PROGRESS_TOTAL=TOTAL_DISCOVERED_TESTS
set /a PROGRESS_COUNT=0

rem -----------------------------
rem Execute tests
rem -----------------------------
if %VERBOSE_MODE%==1 (
    echo Running tests in verbose mode...
    echo.
)

if %SINGLE_TEST_COUNT% gtr 0 (
    for /f "usebackq delims=" %%f in ("%TEST_LIST_FILE%") do (
        call :run_test "%%f"
    )
)

if "%RUN_MULTI_UNIT_TESTS%"=="1" call :run_multi_unit_tests

call :ensure_progress_newline

rem Cleanup collected test list early
if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1

rem -----------------------------
rem Summary
rem -----------------------------
echo ===============================
echo %BLUE%Test Summary%NC%
echo Total tests:  %TOTAL_TESTS%
echo Passed:       %GREEN%%PASSED_TESTS%%NC%
echo Failed:       %RED%%FAILED_TESTS%%NC%

if %FAILURES_ONLY% equ 1 if %FAILED_TESTS% equ 0 (
    echo %GREEN%All tests passed!%NC%
)

if %TIMED_TESTS% gtr 0 (
    for /f %%v in ('powershell -NoProfile -Command "[Math]::Round(%TIMED_DURATION% / %TIMED_TESTS%, 2)"') do set "AVG_TIME=%%v"
) else (
    set "AVG_TIME=n/a"
)

echo.
echo Timing ^(runtime tests only^):
if %TIMED_TESTS% gtr 0 (
    echo   Counted tests: %TIMED_TESTS%
    echo   Total elapsed: %TIMED_DURATION%s
    echo   Avg per test: %AVG_TIME%s
) else (
    echo   No runtime tests were timed.
)

if %FAILED_TESTS% gtr 0 (
    echo %RED%Some tests failed!%NC%
    echo.
    echo To debug failing tests, run:
    echo   %0 -v [test_pattern]
    set EXIT_CODE=1
) else (
    echo %GREEN%All tests passed!%NC%
    set EXIT_CODE=0
)

goto finish

:finish_with_error
set EXIT_CODE=1
goto finish

:finish
if exist "%TEST_LIST_FILE%" del "%TEST_LIST_FILE%" >nul 2>&1
if defined RUNTIME_LIB if exist "%RUNTIME_LIB%" del "%RUNTIME_LIB%" >nul 2>&1

endlocal & exit /b %EXIT_CODE%

rem ==========================================================
rem Helper: locate executable
rem ==========================================================
:locate_hybrid_executable
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
) else if exist "hybrid.exe" (
    set HYBRID_EXEC=hybrid.exe
)

if not defined HYBRID_EXEC (
    echo %RED%Error: hybrid executable not found.%NC%
    echo %RED%Build the project first using:%NC%
    echo %RED%  cmake -B build ^&^& cmake --build build%NC%
    echo %RED%Or use the build script:%NC%
    echo %RED%  build.bat%NC%
    exit /b 1
)

echo Using executable: %HYBRID_EXEC%
exit /b 0

rem ==========================================================
rem Helper: check environment flags
rem ==========================================================
:configure_env_flags
call :is_truthy HYBRID_SHOW_GENERIC_METRICS
if %ENV_TRUE%==1 (
    set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --diagnostics generics"
    set FORCE_VERBOSE_FROM_ENV=1
)

call :is_truthy HYBRID_ARC_DEBUG
if %ENV_TRUE%==1 (
    set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-debug"
    set ARC_RUNTIME_REQUIRED=1
    set FORCE_VERBOSE_FROM_ENV=1
)

call :is_truthy HYBRID_ARC_TRACE_RUNTIME
if %ENV_TRUE%==1 (
    set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-trace-retains"
    set ARC_RUNTIME_REQUIRED=1
)

call :is_truthy HYBRID_ARC_LEAK_DETECT
if %ENV_TRUE%==1 (
    set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-leak-detect"
    set ARC_RUNTIME_REQUIRED=1
)

call :is_truthy HYBRID_ARC_VERIFY_RUNTIME
if %ENV_TRUE%==1 (
    set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-verify-runtime"
    set ARC_RUNTIME_REQUIRED=1
)
exit /b 0

rem ==========================================================
rem Helper: truthy env check
rem ==========================================================
:is_truthy
setlocal EnableDelayedExpansion
set "val=!%~1!"
if not defined val (
    endlocal & set ENV_TRUE=0 & exit /b 0
)
set "val=!val: =!"
set "val=!val:	=!"
set "val=!val:\"=!"
set "val=!val:'=!"
if "!val!"=="" (
    endlocal & set ENV_TRUE=1 & exit /b 0
)
set "lower=!val!"
for %%A in (!lower!) do set "lower=%%A"
if /i "!lower!"=="0" (
    endlocal & set ENV_TRUE=0 & exit /b 0
)
if /i "!lower!"=="false" (
    endlocal & set ENV_TRUE=0 & exit /b 0
)
if /i "!lower!"=="off" (
    endlocal & set ENV_TRUE=0 & exit /b 0
)
endlocal & set ENV_TRUE=1 & exit /b 0

rem ==========================================================
rem Helper: prepare runtime library
rem ==========================================================
:prepare_runtime_library
set RUNTIME_LIB=
where clang >nul 2>&1
if %errorlevel% neq 0 exit /b 0

set "RUNTIME_BASE=%TEMP%\hybrid_runtime_%RANDOM%"
set "RUNTIME_LIB=%RUNTIME_BASE%.obj"
clang -c "runtime\test_runtime_stub.c" -o "%RUNTIME_LIB%" 2>nul
if errorlevel 1 (
    set RUNTIME_LIB=
)
exit /b 0

rem ==========================================================
rem Helper: recount multi-unit suites
rem ==========================================================
:recount_multi_unit
setlocal EnableDelayedExpansion
set "filter=%~1"
set /a count=0
if exist "test\multi_unit" (
    for /d %%m in ("test\multi_unit\*") do (
        set "name=%%~nm"
        set process_suite=1
        if defined filter (
            set process_suite=0
            if /i "!name!"=="!filter!" set process_suite=1
        )
        if !process_suite! equ 1 (
            set has_multi=0
            for %%f in ("%%~fm\*.hy") do (
                if exist "%%~ff" set has_multi=1
            )
            if !has_multi! equ 1 set /a count+=1
        )
    )
)
endlocal & set MULTI_UNIT_TEST_COUNT=%count% & exit /b 0

rem ==========================================================
rem Helper: progress display
rem ==========================================================
:ensure_progress_newline
if %PROGRESS_VISIBLE%==1 (
    echo.
    set PROGRESS_VISIBLE=0
)
exit /b 0

:update_progress_display
if not %FAILURES_ONLY%==1 exit /b 0
if %PROGRESS_TOTAL% leq 0 exit /b 0
if %PROGRESS_COUNT% gtr %PROGRESS_TOTAL% set /a PROGRESS_COUNT=PROGRESS_TOTAL

set /a completed=PROGRESS_COUNT*PROGRESS_BAR_WIDTH/PROGRESS_TOTAL
set /a remaining=PROGRESS_BAR_WIDTH-completed
set "hashes="
for /l %%i in (1,1,%completed%) do set "hashes=!hashes!#"
set "spaces="
for /l %%i in (1,1,%remaining%) do set "spaces=!spaces!."
set /p "=Progress: [!hashes!!spaces!] %PROGRESS_COUNT%/%PROGRESS_TOTAL%" <nul
set PROGRESS_VISIBLE=1
exit /b 0

:mark_test_completed
set /a TOTAL_TESTS+=1
if %FAILURES_ONLY%==1 (
    set /a PROGRESS_COUNT+=1
    call :update_progress_display
)
exit /b 0

rem ==========================================================
rem Helper: epoch seconds
rem ==========================================================
:get_epoch
for /f %%t in ('powershell -NoProfile -Command "[int][double](Get-Date -UFormat %%s)"') do set "%~1=%%t"
exit /b 0

rem ==========================================================
rem Execute a single test file
rem ==========================================================
:run_test
set "test_file=%~1"
set "test_name=%~n1"
set has_errors=0
set runtime_exit_code=0
set test_passed=0
set "run_opts="
set counts_for_timing=1
set "runtime_output_file="
set "output_file=%TEMP%\hybrid_test_output_%RANDOM%.txt"
set "runtime_log=%TEMP%\hybrid_runtime_%RANDOM%.txt"

echo !test_name! | findstr /i "fail error" >nul
if !errorlevel! equ 0 set counts_for_timing=0

rem Parse RUN_OPTS
for /f "usebackq delims=" %%l in (`findstr /b /c:"// RUN_OPTS:" "%test_file%"`) do (
    set "line=%%l"
    set "line=!line:// RUN_OPTS:=!"
    for /f "tokens=* delims= " %%o in ("!line!") do set "line=%%o"
    if defined line set "run_opts=!run_opts! !line!"
)

call :get_epoch test_wall_start

cmd /c ""%HYBRID_EXEC%"%EXTRA_COMPILER_ARGS% !run_opts! < "%test_file%" > "!output_file!" 2>&1"
set exit_code=%errorlevel%

rem Track known diagnostic patterns
findstr /i "Error" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "Failed to generate" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "Unknown function" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "Unknown variable" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "invalid binary operator" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "Binary operator" "!output_file!" >nul 2>&1 && set has_errors=1
findstr /i "Expected" "!output_file!" | findstr /i "after" >nul 2>&1 && set has_errors=1

rem EXPECT_OUTPUT checks
for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_OUTPUT:" "%test_file%"`) do (
    set "expect_line=%%l"
    set "expect_line=!expect_line:// EXPECT_OUTPUT:=!"
    for /f "tokens=* delims= " %%o in ("!expect_line!") do set "expect_line=%%o"
    if defined expect_line (
        findstr /C:"!expect_line!" "!output_file!" >nul 2>&1
        if !errorlevel! neq 0 (
            set has_errors=1
            echo [test-expectation] missing: !expect_line!>>"!output_file!"
        )
    )
)

rem EXPECT_RUNTIME capture
set /a expected_runtime_count=0
for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_RUNTIME:" "%test_file%"`) do (
    set "rt_line=%%l"
    set "rt_line=!rt_line:// EXPECT_RUNTIME:=!"
    for /f "tokens=* delims= " %%o in ("!rt_line!") do set "rt_line=%%o"
    if defined rt_line (
        set /a expected_runtime_count+=1
        set "expected_runtime_!expected_runtime_count!=!rt_line!"
    )
)

set should_check_runtime=0
echo !test_name! | findstr /i "fail error" >nul
if !errorlevel! equ 0 (
    set should_check_runtime=1
) else (
    if !has_errors! equ 0 if !exit_code! equ 0 set should_check_runtime=1
)

if !should_check_runtime! equ 1 if defined RUNTIME_LIB (
    where clang++ >nul 2>&1
    if !errorlevel! equ 0 (
        set module_start=
        for /f "tokens=1 delims=:" %%n in ('findstr /n "=== Final Generated" "!output_file!"') do set module_start=%%n
        if defined module_start (
            set /a module_start+=1
            set "temp_ir=%TEMP%\hybrid_ir_%RANDOM%.ll"
            set "temp_bin=%TEMP%\hybrid_bin_%RANDOM%.exe"
            powershell -NoProfile -Command "(Get-Content -Path '!output_file!'^| Select-Object -Skip !module_start! ^| Where-Object {$_ -notmatch '^(ready^>|Parsed|Generated|\\[generics|\\[arc-trace\\]|\\[arc-escape\\])'}) ^| Set-Content -Path '!temp_ir!'" >nul 2>&1
            findstr /C:"define i32 @main" "!temp_ir!" >nul 2>&1
            if !errorlevel! equ 0 (
                set needs_runtime=%ARC_RUNTIME_REQUIRED%
                echo !run_opts! | findstr /i "--arc-debug --arc-leak-detect --arc-verify-runtime --arc-trace-retains" >nul 2>&1
                if !errorlevel! equ 0 set needs_runtime=1
                findstr "__hybrid_shared_control __hybrid_smart_" "!temp_ir!" >nul 2>&1 && set needs_runtime=1
                findstr "hybrid_release hybrid_retain hybrid_autorelease hybrid_alloc_array hybrid_alloc_object" "!temp_ir!" >nul 2>&1 && set needs_runtime=1

                if !needs_runtime! equ 1 (
                    clang++ "!temp_ir!" "%RUNTIME_LIB%" -o "!temp_bin!" >nul 2>&1
                ) else (
                    clang++ "!temp_ir!" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "!temp_bin!" >nul 2>&1
                )
                if !errorlevel! equ 0 (
                    cmd /c ""!temp_bin!"" > "!runtime_log!" 2>&1
                    set runtime_exit_code=!errorlevel!
                ) else (
                    set has_errors=1
                )
            )
            if exist "!temp_ir!" del "!temp_ir!" >nul 2>&1
            if exist "!temp_bin!" del "!temp_bin!" >nul 2>&1
        )
    )
)

if %expected_runtime_count% gtr 0 (
    if not exist "!runtime_log!" (
        set has_errors=1
        echo [runtime-expectation] missing runtime output>>"!output_file!"
    ) else (
        for /l %%e in (1,1,!expected_runtime_count!) do (
            set "needle=!expected_runtime_%%e!"
            if defined needle (
                findstr /C:"!needle!" "!runtime_log!" >nul 2>&1
                if !errorlevel! neq 0 (
                    set has_errors=1
                    echo [runtime-expectation] missing: !needle!>>"!output_file!"
                )
            )
        )
    )
)

set is_fail_test=0
echo !test_name! | findstr /i "fail error" >nul
if !errorlevel! equ 0 set is_fail_test=1

if !is_fail_test! equ 1 (
    if !has_errors! equ 1 (
        set test_passed=1
    ) else if !exit_code! neq 0 (
        set test_passed=1
    ) else if !runtime_exit_code! neq 0 (
        set test_passed=1
    )
) else (
    if !has_errors! equ 0 if !exit_code! equ 0 if !runtime_exit_code! equ 0 (
        set test_passed=1
    )
)

if %FAILURES_ONLY% equ 0 (
    set show_output=1
) else if !test_passed! equ 0 (
    set show_output=1
) else (
    set show_output=0
)

if %VERBOSE_MODE% equ 1 if !show_output! equ 1 (
    call :ensure_progress_newline
    echo %BLUE%=== Test: !test_name! ===%NC%
    echo File: !test_file!
    echo Content:
    findstr /n "^" "!test_file!"
    echo.
    echo Output:
    type "!output_file!"
    echo.
    if defined runtime_log (
        echo Runtime output:
        type "!runtime_log!"
        echo.
    )
    if !is_fail_test! equ 1 (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name! ^(correctly failed as expected^)%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name! ^(should have failed but didn't^)%NC%
        )
    ) else (
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
                findstr /i "Error Failed Unknown invalid Binary Expected" "!output_file!" 2>nul | findstr /n "^" | findstr "^[1-3]:"
            )
        )
    )
    echo.
) else if !show_output! equ 1 (
    call :ensure_progress_newline
    echo %YELLOW%Running test: !test_name!%NC%
    echo ----------------------------------------
    if !is_fail_test! equ 1 (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name! ^(correctly failed as expected^)%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name! ^(should have failed but didn't^)%NC%
        )
    ) else (
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
                findstr /i "Error Failed Unknown invalid Binary Expected" "!output_file!" 2>nul | findstr /n "^" | findstr "^[1-3]:"
            )
        )
    )
    echo.
)

if !test_passed! equ 1 (
    set /a PASSED_TESTS+=1
) else (
    set /a FAILED_TESTS+=1
)

call :mark_test_completed

call :get_epoch test_wall_end
set /a test_elapsed=test_wall_end-test_wall_start
if !counts_for_timing! equ 1 (
    set /a TIMED_DURATION+=test_elapsed
    set /a TIMED_TESTS+=1
)

if exist "!output_file!" del "!output_file!" >nul 2>&1
if exist "!runtime_log!" del "!runtime_log!" >nul 2>&1
exit /b 0

rem ==========================================================
rem Execute multi-unit suites
rem ==========================================================
:run_multi_unit_tests
if %MULTI_UNIT_TEST_COUNT% leq 0 goto :eof

call :ensure_progress_newline
echo.
echo %BLUE%Multi-unit Compilation Tests%NC%
echo -------------------------------

for /d %%d in ("test\multi_unit\*") do (
    set "multi_dir=%%~fd"
    set "multi_name=%%~nd"
    if defined MULTI_UNIT_FILTER (
        if /i "!multi_name!" neq "!MULTI_UNIT_FILTER!" (
            goto skip_suite
        )
    )
    set "multi_expect=pass"
    if exist "%%~fd\EXPECT_FAIL" set "multi_expect=fail"
    if /i "!multi_name:~-5!"=="_fail" set "multi_expect=fail"

    set /a counts_for_timing=0
    if /i "!multi_expect!"=="pass" set /a counts_for_timing=1

    if !counts_for_timing! equ 1 call :get_epoch suite_start

    set "cmd_files="
    set /a multi_file_count=0
    for /f "delims=" %%f in ('dir /b /on "%%~fd\*.hy"') do (
        set "cmd_files=!cmd_files! "%%~fd\%%f""
        set /a multi_file_count+=1
    )

    if !multi_file_count! equ 0 (
        if %FAILURES_ONLY% equ 0 (
            echo %YELLOW%Skipping !multi_name! ^(no .hy files^)%NC%
        )
        goto skip_suite
    )

    set "multi_temp=%TEMP%\hybrid_multi_%RANDOM%"
    set "multi_output=!multi_temp!.out"
    set "multi_stdout=!multi_temp!.log"
    set "multi_runtime=!multi_temp!.run"

    cmd /c ""%HYBRID_EXEC%"%EXTRA_COMPILER_ARGS%!cmd_files! -o "!multi_output!"" > "!multi_stdout!" 2>&1
    set multi_status=!errorlevel!

    set runtime_status=0
    if /i "!multi_expect!"=="pass" (
        if !multi_status! equ 0 (
            if exist "!multi_output!" (
                cmd /c ""!multi_output!"" > "!multi_runtime!" 2>&1
                set runtime_status=!errorlevel!
            ) else (
                set runtime_status=1
            )
        )
    )

    set passed=0
    if /i "!multi_expect!"=="pass" (
        if !multi_status! equ 0 if !runtime_status! equ 0 set passed=1
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
        call :ensure_progress_newline
        echo %RED%✗ FAILED: !multi_name!%NC%
        if /i "!multi_expect!"=="pass" (
            if !multi_status! neq 0 (
                echo %RED%  Expected success but compilation failed with status !multi_status!%NC%
            ) else if !runtime_status! neq 0 (
                echo %RED%  Runtime exited with status !runtime_status!%NC%
            )
        ) else (
            echo %RED%  Expected failure but command succeeded%NC%
        )
        echo --- Compiler Output ---
        type "!multi_stdout!"
        if /i "!multi_expect!"=="pass" if exist "!multi_runtime!" (
            echo --- Runtime Output ---
            type "!multi_runtime!"
        )
        echo --------------
    )

    call :mark_test_completed

    if !counts_for_timing! equ 1 (
        call :get_epoch suite_end
        set /a TIMED_DURATION+=suite_end-suite_start
        set /a TIMED_TESTS+=1
    )

    if exist "!multi_output!" del "!multi_output!" >nul 2>&1
    if exist "!multi_stdout!" del "!multi_stdout!" >nul 2>&1
    if exist "!multi_runtime!" del "!multi_runtime!" >nul 2>&1

    :skip_suite
)
goto :eof
