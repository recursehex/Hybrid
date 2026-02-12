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
set JOBS=1
set TEST_PATTERN=
set RUN_MULTI_UNIT_TESTS=1
set MULTI_UNIT_FILTER=
set SINGLE_TESTS_ENABLED=1
set EXTRA_COMPILER_ARGS=
set FORCE_VERBOSE_FROM_ENV=0
set VERBOSE_FROM_ARGS=0
set ARC_RUNTIME_REQUIRED=0
set ARC_ENABLED_OVERRIDE=
set PREBUILT_RUNTIME_STUB_LIB=
set PREBUILT_ARC_RUNTIME_LIB=
set RUNTIME_LIB_IS_TEMP=0

rem Progress tracking for failures-only mode
set PROGRESS_BAR_WIDTH=40
set /a PROGRESS_COUNT=0
set /a PROGRESS_TOTAL=0
set PROGRESS_VISIBLE=0

set WORKER_MODE=0
if /i "%~1"=="--worker-run-test" (
    set WORKER_MODE=1
    set "WORKER_TEST_FILE=%~2"
    set "RESULT_FILE=%~3"
    set "WORKER_ARC_OVERRIDE=%~4"
    goto worker_bootstrap
)

echo %BLUE%Hybrid Compiler Test Suite%NC%
echo ===============================

call :locate_hybrid_executable
if errorlevel 1 (
    endlocal & exit /b 1
)
call :resolve_temp_dir
if errorlevel 1 (
    endlocal & exit /b 1
)

for %%i in ("%HYBRID_EXEC%") do set "HYBRID_BIN_DIR=%%~dpi"
if exist "%HYBRID_BIN_DIR%libhybrid_test_stub.a" set "PREBUILT_RUNTIME_STUB_LIB=%HYBRID_BIN_DIR%libhybrid_test_stub.a"
if exist "%HYBRID_BIN_DIR%libhybrid_runtime_test.a" set "PREBUILT_ARC_RUNTIME_LIB=%HYBRID_BIN_DIR%libhybrid_runtime_test.a"
if not defined PREBUILT_RUNTIME_STUB_LIB if exist "%HYBRID_BIN_DIR%hybrid_test_stub.lib" set "PREBUILT_RUNTIME_STUB_LIB=%HYBRID_BIN_DIR%hybrid_test_stub.lib"
if not defined PREBUILT_ARC_RUNTIME_LIB if exist "%HYBRID_BIN_DIR%hybrid_runtime_test.lib" set "PREBUILT_ARC_RUNTIME_LIB=%HYBRID_BIN_DIR%hybrid_runtime_test.lib"

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
if /i "%~1"=="-j" (
    if "%~2"=="" (
        echo %RED%Error: -j requires a positive integer%NC%
        exit /b 1
    )
    echo %~2 | findstr /r "^[1-9][0-9]*$" >nul 2>&1
    if errorlevel 1 (
        echo %RED%Error: -j requires a positive integer%NC%
        exit /b 1
    )
    set "JOBS=%~2"
    shift
    shift
    goto parse_args
)
echo %~1 | findstr /r /c:"^--jobs=[1-9][0-9]*$" >nul 2>&1
if not errorlevel 1 (
    for /f "tokens=2 delims==" %%v in ("%~1") do set "JOBS=%%v"
    shift
    goto parse_args
)
if /i "%~1"=="-a" (
    if "%~2"=="" (
        echo %RED%Error: -a requires a value ^(on/off^)%NC%
        exit /b 1
    )
    set "ARC_ENABLED_OVERRIDE="
    if /i "%~2"=="on" set ARC_ENABLED_OVERRIDE=true
    if /i "%~2"=="true" set ARC_ENABLED_OVERRIDE=true
    if /i "%~2"=="1" set ARC_ENABLED_OVERRIDE=true
    if /i "%~2"=="off" set ARC_ENABLED_OVERRIDE=false
    if /i "%~2"=="false" set ARC_ENABLED_OVERRIDE=false
    if /i "%~2"=="0" set ARC_ENABLED_OVERRIDE=false
    if not defined ARC_ENABLED_OVERRIDE (
        echo %RED%Error: invalid value for -a ^(use on/off^)%NC%
        exit /b 1
    )
    shift
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
echo   -j, --jobs N        Run up to N single-file tests in parallel
echo   -a on^|off          Toggle ARC lowering ^(--arc-enabled^) for this run
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
echo   %0 -j 8             ^# Run tests with 8 workers
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
if defined ARC_ENABLED_OVERRIDE set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-enabled=%ARC_ENABLED_OVERRIDE%"
if %JOBS% gtr 1 (
    if %VERBOSE_MODE%==1 (
        echo %YELLOW%Note: parallel mode currently supports failures-only compact output; using serial mode.%NC%
        set JOBS=1
    ) else if %FAILURES_ONLY%==0 (
        echo %YELLOW%Note: parallel mode currently supports failures-only compact output; using serial mode.%NC%
        set JOBS=1
    )
)

call :recount_multi_unit "%MULTI_UNIT_FILTER%"

rem -----------------------------
rem Collect test list
rem -----------------------------
set "TEST_LIST_FILE=%HYBRID_TEST_TMP_DIR%\hybrid_tests_%RANDOM%.lst"
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
    if %JOBS% gtr 1 (
        call :run_single_tests_parallel
    ) else (
        for /f "usebackq delims=" %%f in ("%TEST_LIST_FILE%") do (
            call :run_test "%%f"
        )
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
if "%RUNTIME_LIB_IS_TEMP%"=="1" if defined RUNTIME_LIB if exist "%RUNTIME_LIB%" del "%RUNTIME_LIB%" >nul 2>&1

endlocal & exit /b %EXIT_CODE%

rem ==========================================================
rem Worker bootstrap for parallel single-file execution
rem ==========================================================
:worker_bootstrap
if not defined WORKER_TEST_FILE (
    set EXIT_CODE=1
    goto finish
)
call :locate_hybrid_executable
if errorlevel 1 (
    set EXIT_CODE=1
    goto finish
)
call :resolve_temp_dir
if errorlevel 1 (
    set EXIT_CODE=1
    goto finish
)
for %%i in ("%HYBRID_EXEC%") do set "HYBRID_BIN_DIR=%%~dpi"
if exist "%HYBRID_BIN_DIR%libhybrid_test_stub.a" set "PREBUILT_RUNTIME_STUB_LIB=%HYBRID_BIN_DIR%libhybrid_test_stub.a"
if exist "%HYBRID_BIN_DIR%libhybrid_runtime_test.a" set "PREBUILT_ARC_RUNTIME_LIB=%HYBRID_BIN_DIR%libhybrid_runtime_test.a"
if not defined PREBUILT_RUNTIME_STUB_LIB if exist "%HYBRID_BIN_DIR%hybrid_test_stub.lib" set "PREBUILT_RUNTIME_STUB_LIB=%HYBRID_BIN_DIR%hybrid_test_stub.lib"
if not defined PREBUILT_ARC_RUNTIME_LIB if exist "%HYBRID_BIN_DIR%hybrid_runtime_test.lib" set "PREBUILT_ARC_RUNTIME_LIB=%HYBRID_BIN_DIR%hybrid_runtime_test.lib"
call :configure_env_flags
if defined WORKER_ARC_OVERRIDE set "ARC_ENABLED_OVERRIDE=%WORKER_ARC_OVERRIDE%"
if defined ARC_ENABLED_OVERRIDE set "EXTRA_COMPILER_ARGS=%EXTRA_COMPILER_ARGS% --arc-enabled=%ARC_ENABLED_OVERRIDE%"
call :prepare_runtime_library
call :run_test "%WORKER_TEST_FILE%"
set EXIT_CODE=0
goto finish

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
    for /r "build" %%f in (hybrid.exe) do (
        if not defined HYBRID_EXEC set "HYBRID_EXEC=%%f"
    )
)

if not defined HYBRID_EXEC (
    echo %RED%Error: hybrid executable not found.%NC%
    echo %RED%Build the project first using:%NC%
    echo %RED%  cmake -B build ^&^& cmake --build build%NC%
    echo %RED%Or use the build script:%NC%
    echo %RED%  build.bat%NC%
    exit /b 1
)

set "HYBRID_EXEC=%HYBRID_EXEC:"=%"
for %%i in ("%HYBRID_EXEC%") do set "HYBRID_EXEC=%%~fi"
if not exist "%HYBRID_EXEC%" (
    set "HYBRID_EXEC="
    for /r "build" %%f in (hybrid.exe) do (
        if not defined HYBRID_EXEC if exist "%%f" set "HYBRID_EXEC=%%~ff"
    )
    if not defined HYBRID_EXEC (
        echo %RED%Error: hybrid executable path is invalid and no fallback was found.%NC%
        exit /b 1
    )
)

if not "%WORKER_MODE%"=="1" echo Using executable: %HYBRID_EXEC%
exit /b 0

rem ==========================================================
rem Helper: resolve temp directory
rem ==========================================================
:resolve_temp_dir
set "HYBRID_TEST_TMP_DIR="
set "HYBRID_TEST_TMP_DIR=%CD%\build\tmp"
if not exist "%HYBRID_TEST_TMP_DIR%\." md "%HYBRID_TEST_TMP_DIR%" >nul 2>&1
if not exist "%HYBRID_TEST_TMP_DIR%\." if defined RUNNER_TEMP (
    set "HYBRID_TEST_TMP_DIR=%RUNNER_TEMP%\hybrid_tmp"
    if not exist "%HYBRID_TEST_TMP_DIR%\." md "%HYBRID_TEST_TMP_DIR%" >nul 2>&1
)
if not exist "%HYBRID_TEST_TMP_DIR%\." if defined TEMP (
    set "HYBRID_TEST_TMP_DIR=%TEMP%\hybrid_tmp"
    if not exist "%HYBRID_TEST_TMP_DIR%\." md "%HYBRID_TEST_TMP_DIR%" >nul 2>&1
)
if not exist "%HYBRID_TEST_TMP_DIR%\." if defined TMP (
    set "HYBRID_TEST_TMP_DIR=%TMP%\hybrid_tmp"
    if not exist "%HYBRID_TEST_TMP_DIR%\." md "%HYBRID_TEST_TMP_DIR%" >nul 2>&1
)
if not exist "%HYBRID_TEST_TMP_DIR%\." (
    echo %RED%Error: unable to access temp directory: %HYBRID_TEST_TMP_DIR%%NC%
    exit /b 1
)
for %%i in ("%HYBRID_TEST_TMP_DIR%") do set "HYBRID_TEST_TMP_DIR=%%~fi"
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
set RUNTIME_LIB_IS_TEMP=0
if defined PREBUILT_RUNTIME_STUB_LIB if exist "%PREBUILT_RUNTIME_STUB_LIB%" (
    set "RUNTIME_LIB=%PREBUILT_RUNTIME_STUB_LIB%"
    exit /b 0
)
where clang >nul 2>&1
if %errorlevel% neq 0 exit /b 0

set "RUNTIME_BASE=%HYBRID_TEST_TMP_DIR%\hybrid_runtime_%RANDOM%"
set "RUNTIME_LIB=%RUNTIME_BASE%.obj"
clang -c "runtime\test_runtime_stub.c" -o "%RUNTIME_LIB%" 2>nul
if errorlevel 1 (
    set RUNTIME_LIB=
) else (
    set RUNTIME_LIB_IS_TEMP=1
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
rem Parallel single-file helpers
rem ==========================================================
:run_single_tests_parallel
set "parallel_dir=%HYBRID_TEST_TMP_DIR%\hybrid_parallel_%RANDOM%"
if exist "!parallel_dir!" rd /s /q "!parallel_dir!" >nul 2>&1
md "!parallel_dir!" >nul 2>&1

set /a parallel_seq=0
set /a batch_count=0
set "batch_manifest=!parallel_dir!\batch.lst"
if exist "!batch_manifest!" del "!batch_manifest!" >nul 2>&1

for /f "usebackq delims=" %%f in ("%TEST_LIST_FILE%") do (
    if not "%%f"=="" (
        set /a parallel_seq+=1
        set "parallel_result_file=!parallel_dir!\result.!parallel_seq!.txt"
        set "parallel_output_file=!parallel_dir!\output.!parallel_seq!.txt"
        >>"!batch_manifest!" echo(%%f^|!parallel_result_file!^|!parallel_output_file!
        start "" /b cmd /c ""%~f0" --worker-run-test "%%f" "!parallel_result_file!" "%ARC_ENABLED_OVERRIDE%" ^> "!parallel_output_file!" 2^>^&1"
        set /a batch_count+=1

        if !batch_count! geq %JOBS% (
            call :process_parallel_batch "!batch_manifest!"
            if exist "!batch_manifest!" del "!batch_manifest!" >nul 2>&1
            set /a batch_count=0
        )
    )
)

if !batch_count! gtr 0 (
    call :process_parallel_batch "!batch_manifest!"
)

if exist "!parallel_dir!" rd /s /q "!parallel_dir!" >nul 2>&1
exit /b 0

:process_parallel_batch
set "parallel_manifest=%~1"
if not exist "!parallel_manifest!" exit /b 0

set /a wait_seconds=0
:wait_for_parallel_batch
set parallel_all_done=1
for /f "usebackq tokens=1,2,3 delims=|" %%a in ("!parallel_manifest!") do (
    if not exist "%%b" set parallel_all_done=0
)

if "!parallel_all_done!"=="1" goto process_parallel_batch_results
if !wait_seconds! geq 600 goto process_parallel_batch_results
timeout /t 1 /nobreak >nul
set /a wait_seconds+=1
goto wait_for_parallel_batch

:process_parallel_batch_results
for /f "usebackq tokens=1,2,3 delims=|" %%a in ("!parallel_manifest!") do (
    call :process_parallel_result "%%b" "%%c" "%%a"
    if exist "%%b" del "%%b" >nul 2>&1
    if exist "%%c" del "%%c" >nul 2>&1
)
exit /b 0

:process_parallel_result
set "pr_result_file=%~1"
set "pr_output_file=%~2"
set "pr_test_file=%~3"
set "pr_test_name="
set "pr_test_passed=0"
set "pr_counts_for_timing=0"
set "pr_test_elapsed=0"

if exist "!pr_result_file!" (
    for /f "usebackq tokens=1,* delims==" %%k in ("!pr_result_file!") do (
        if /i "%%k"=="test_name" set "pr_test_name=%%l"
        if /i "%%k"=="test_passed" set "pr_test_passed=%%l"
        if /i "%%k"=="counts_for_timing" set "pr_counts_for_timing=%%l"
        if /i "%%k"=="test_elapsed" set "pr_test_elapsed=%%l"
    )
)

set /a TOTAL_TESTS+=1
if "!pr_test_passed!"=="1" (
    set /a PASSED_TESTS+=1
) else (
    set /a FAILED_TESTS+=1
    call :ensure_progress_newline
    if exist "!pr_output_file!" (
        for %%z in ("!pr_output_file!") do if %%~zz gtr 0 (
            type "!pr_output_file!"
        ) else (
            if defined pr_test_name (
                echo %RED%✗ FAILED: !pr_test_name!%NC%
            ) else (
                echo %RED%✗ FAILED: !pr_test_file!%NC%
            )
        )
    ) else (
        if defined pr_test_name (
            echo %RED%✗ FAILED: !pr_test_name!%NC%
        ) else (
            echo %RED%✗ FAILED: !pr_test_file!%NC%
        )
    )
)

if "!pr_counts_for_timing!"=="1" (
    set /a TIMED_DURATION+=pr_test_elapsed
    set /a TIMED_TESTS+=1
)

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
for /f "usebackq delims=" %%p in (`powershell -NoProfile -Command "$root=(Resolve-Path '.').Path; $path=(Resolve-Path '%~1').Path; $rootUri=New-Object System.Uri(($root.TrimEnd('\') + '\')); $fileUri=New-Object System.Uri($path); [System.Uri]::UnescapeDataString($rootUri.MakeRelativeUri($fileUri).ToString())"`) do set "test_file_rel=%%p"
set compile_failed=0
set compiler_expectation_failed=0
set runtime_expectation_failed=0
set runtime_exit_code=0
set runtime_ran=0
set runtime_failed=0
set test_passed=0
set "run_opts="
set counts_for_timing=1
set "runtime_output_file="
set "expected_mode=pass"
set "expected_fail_kind="
set "expected_exit="
set "expect_pass=0"
set machine_mode=0
set "output_file=%HYBRID_TEST_TMP_DIR%\hybrid_test_output_%RANDOM%.txt"
set "runtime_log=%HYBRID_TEST_TMP_DIR%\hybrid_runtime_%RANDOM%.txt"
set "expected_diag_file=%HYBRID_TEST_TMP_DIR%\hybrid_expected_diag_%RANDOM%.txt"
set "actual_diag_file=%HYBRID_TEST_TMP_DIR%\hybrid_actual_diag_%RANDOM%.txt"
set "missing_diag_file=%HYBRID_TEST_TMP_DIR%\hybrid_missing_diag_%RANDOM%.txt"
set "unexpected_diag_file=%HYBRID_TEST_TMP_DIR%\hybrid_unexpected_diag_%RANDOM%.txt"
set "emitted_ir_file="
set "compile_test_input="
set "compile_emit_output="
set run_opts_override_output=0
set may_need_runtime=0
set expected_diag_count=0
set "compile_display="
if exist "!output_file!" del "!output_file!" >nul 2>&1
if exist "!runtime_log!" del "!runtime_log!" >nul 2>&1
if exist "!expected_diag_file!" del "!expected_diag_file!" >nul 2>&1
if exist "!actual_diag_file!" del "!actual_diag_file!" >nul 2>&1
if exist "!missing_diag_file!" del "!missing_diag_file!" >nul 2>&1
if exist "!unexpected_diag_file!" del "!unexpected_diag_file!" >nul 2>&1
if defined RESULT_FILE set machine_mode=1

findstr /b /c:"// EXPECT_PASS" "!test_file!" >nul 2>&1 && set expect_pass=1

for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_FAIL:" "%test_file%"`) do (
    set "line=%%l"
    set "line=!line:// EXPECT_FAIL:=!"
    for /f "tokens=* delims= " %%o in ("!line!") do set "line=%%o"
    if "!line!"=="" (
        set "expected_fail_kind=compile"
    ) else (
        if /i "!line!"=="compile" set "expected_fail_kind=compile"
        if /i "!line!"=="compiler" set "expected_fail_kind=compile"
        if /i "!line!"=="runtime" set "expected_fail_kind=runtime"
        if /i "!line!"=="run" set "expected_fail_kind=runtime"
        if /i "!line!"=="any" set "expected_fail_kind=any"
        if /i "!line!"=="either" set "expected_fail_kind=any"
    )
)

if %expect_pass%==1 (
    set "expected_mode=pass"
) else if defined expected_fail_kind (
    set "expected_mode=fail"
) else (
    echo !test_name! | findstr /i "fail error" >nul
    if !errorlevel! equ 0 (
        set "expected_mode=fail"
        set "expected_fail_kind=compile"
    )
)

if /i "!expected_mode!"=="fail" if not defined expected_fail_kind set "expected_fail_kind=compile"

if /i "!expected_mode!"=="fail" set counts_for_timing=0

rem Parse RUN_OPTS
for /f "usebackq delims=" %%l in (`findstr /b /c:"// RUN_OPTS:" "%test_file%"`) do (
    set "line=%%l"
    set "line=!line:// RUN_OPTS:=!"
    for /f "tokens=* delims= " %%o in ("!line!") do set "line=%%o"
    if defined line set "run_opts=!run_opts! !line!"
)

for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_DIAGNOSTIC:" "%test_file%"`) do (
    set "diag_line=%%l"
    set "diag_line=!diag_line:// EXPECT_DIAGNOSTIC:=!"
    for /f "tokens=* delims= " %%o in ("!diag_line!") do set "diag_line=%%o"
    if defined diag_line >>"!expected_diag_file!" echo(!diag_line!
)

for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_ERROR:" "%test_file%"`) do (
    set "diag_line=%%l"
    set "diag_line=!diag_line:// EXPECT_ERROR:=!"
    for /f "tokens=* delims= " %%o in ("!diag_line!") do set "diag_line=%%o"
    if defined diag_line >>"!expected_diag_file!" echo(!diag_line!
)

for /f %%c in ('powershell -NoProfile -Command "if (Test-Path -LiteralPath '!expected_diag_file!') { (Get-Content -LiteralPath '!expected_diag_file!' | Where-Object { $_.Trim() -ne '' }).Count } else { 0 }"') do set expected_diag_count=%%c

if /i "!expected_mode!"=="pass" (
    set may_need_runtime=1
) else (
    if /i not "!expected_fail_kind!"=="compile" set may_need_runtime=1
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

for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_EXIT:" "%test_file%"`) do (
    set "exit_line=%%l"
    set "exit_line=!exit_line:// EXPECT_EXIT:=!"
    for /f "tokens=* delims= " %%o in ("!exit_line!") do set "exit_line=%%o"
    if defined exit_line set "expected_exit=!exit_line!"
)

if !expected_runtime_count! gtr 0 set may_need_runtime=1
if defined expected_exit set may_need_runtime=1

call :get_epoch test_wall_start

set "RUN_OPTS=!run_opts!"
for /f %%v in ('powershell -NoProfile -Command "$s=$env:RUN_OPTS; if ($s -match '(^|\\s)-o(\\s|$)' -or $s -match '(^|\\s)--emit-llvm(\\s|$)' -or $s -match '(^|\\s)-o=') { 1 } else { 0 }"') do set run_opts_override_output=%%v

if !may_need_runtime! equ 1 if !run_opts_override_output! equ 0 (
    set "emitted_ir_file=%HYBRID_TEST_TMP_DIR%\hybrid_test_%RANDOM%.ll"
    set "compile_test_input=!test_file!"
    if defined test_file_rel (
        set "compile_test_input=!test_file_rel:/=\!"
    )
    set "compile_emit_output=!emitted_ir_file!"
    if /i not "!compile_emit_output:%CD%\=!"=="!compile_emit_output!" (
        set "compile_emit_output=!compile_emit_output:%CD%\=!"
    )
    set "compile_display=!HYBRID_EXEC! !EXTRA_COMPILER_ARGS! !run_opts! !compile_test_input! -o !compile_emit_output!"
    "!HYBRID_EXEC!" !EXTRA_COMPILER_ARGS! !run_opts! "!compile_test_input!" -o "!compile_emit_output!" > "!output_file!" 2>&1
) else (
    set "compile_display=!HYBRID_EXEC! !EXTRA_COMPILER_ARGS! !run_opts! < !test_file!"
    "!HYBRID_EXEC!" !EXTRA_COMPILER_ARGS! !run_opts! < "!test_file!" > "!output_file!" 2>&1
)
set exit_code=%errorlevel%
if not "%exit_code%"=="0" (
    if not exist "!output_file!" (
        > "!output_file!" echo [harness] compiler command failed before output capture
    )
    echo [harness] compile-exit: !exit_code!>>"!output_file!"
    echo [harness] hybrid-exec: !HYBRID_EXEC!>>"!output_file!"
    if defined compile_display echo [harness] compile-cmd: !compile_display!>>"!output_file!"
    echo [harness] test-file: !test_file!>>"!output_file!"
    if defined emitted_ir_file echo [harness] emitted-ir: !emitted_ir_file!>>"!output_file!"
)

rem Track known diagnostic patterns
if not "%exit_code%"=="0" set compile_failed=1
findstr /i "Error" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "Failed to generate" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "Unknown function" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "Unknown variable" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "invalid binary operator" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "Binary operator" "!output_file!" >nul 2>&1 && set compile_failed=1
findstr /i "Expected" "!output_file!" | findstr /i "after" >nul 2>&1 && set compile_failed=1

powershell -NoProfile -Command "$raw = Get-Content -LiteralPath '!output_file!' -Raw; $norm = $raw -replace 'Error at line ',\"`nError at line \" -replace 'Warning at line ',\"`nWarning at line \" -replace 'Error: ',\"`nError: \"; $norm -split \"`r?`n\" | Where-Object { $_ -match '^(Error at line|Warning at line|Error: )' } | Set-Content -LiteralPath '!actual_diag_file!'"

rem EXPECT_OUTPUT checks
for /f "usebackq delims=" %%l in (`findstr /b /c:"// EXPECT_OUTPUT:" "%test_file%"`) do (
    set "expect_line=%%l"
    set "expect_line=!expect_line:// EXPECT_OUTPUT:=!"
    for /f "tokens=* delims= " %%o in ("!expect_line!") do set "expect_line=%%o"
    if defined expect_line (
        findstr /C:"!expect_line!" "!output_file!" >nul 2>&1
        if !errorlevel! neq 0 (
            set compiler_expectation_failed=1
            echo [test-expectation] missing: !expect_line!>>"!output_file!"
        )
    )
)

for /f %%c in ('powershell -NoProfile -Command "if (Test-Path -LiteralPath '!expected_diag_file!') { (Get-Content -LiteralPath '!expected_diag_file!' | Where-Object { $_.Trim() -ne '' }).Count } else { 0 }"') do set expected_diag_count=%%c

if /i "!expected_mode!"=="fail" if /i not "!expected_fail_kind!"=="runtime" (
    if !expected_diag_count! equ 0 (
        set compiler_expectation_failed=1
        echo [test-expectation] missing compile diagnostic expectations for !test_file_rel!>>"!output_file!"
    ) else (
        set "diag_compare_file=%HYBRID_TEST_TMP_DIR%\hybrid_diag_compare_%RANDOM%.txt"
        powershell -NoProfile -Command "$expected = if (Test-Path -LiteralPath '!expected_diag_file!') { Get-Content -LiteralPath '!expected_diag_file!' | Where-Object { $_.Trim() -ne '' } } else { @() }; $actual = if (Test-Path -LiteralPath '!actual_diag_file!') { Get-Content -LiteralPath '!actual_diag_file!' | Where-Object { $_.Trim() -ne '' } } else { @() }; $matched = New-Object bool[] $expected.Count; $missing = New-Object System.Collections.Generic.List[string]; $unexpected = New-Object System.Collections.Generic.List[string]; foreach ($a in $actual) { $ok = $false; for ($i = 0; $i -lt $expected.Count; $i++) { if (-not $matched[$i] -and $a.Contains($expected[$i])) { $matched[$i] = $true; $ok = $true; break } } if (-not $ok) { $unexpected.Add($a) } }; for ($i = 0; $i -lt $expected.Count; $i++) { if (-not $matched[$i]) { $missing.Add($expected[$i]) } }; $missing | Set-Content -LiteralPath '!missing_diag_file!'; $unexpected | Set-Content -LiteralPath '!unexpected_diag_file!'; if ($missing.Count -gt 0 -or $unexpected.Count -gt 0) { '1' } else { '0' }" > "!diag_compare_file!"
        set strict_diag_mismatch=0
        for /f "usebackq delims=" %%m in ("!diag_compare_file!") do set strict_diag_mismatch=%%m
        del "!diag_compare_file!" >nul 2>&1
        if "!strict_diag_mismatch!"=="1" (
            set compiler_expectation_failed=1
            if exist "!missing_diag_file!" (
                for /f "usebackq delims=" %%x in ("!missing_diag_file!") do (
                    if not "%%x"=="" echo [diagnostic-expectation] missing: %%x>>"!output_file!"
                )
            )
            if exist "!unexpected_diag_file!" (
                for /f "usebackq delims=" %%x in ("!unexpected_diag_file!") do (
                    if not "%%x"=="" echo [diagnostic-expectation] unexpected: %%x>>"!output_file!"
                )
            )
        )
    )
)

set should_check_runtime=0
if !compile_failed! equ 0 if !exit_code! equ 0 (
    if /i "!expected_mode!"=="pass" (
        set should_check_runtime=1
    ) else (
        if /i not "!expected_fail_kind!"=="compile" set should_check_runtime=1
        if !expected_runtime_count! gtr 0 set should_check_runtime=1
        if defined expected_exit set should_check_runtime=1
    )
)

if !should_check_runtime! equ 1 if defined RUNTIME_LIB (
    where clang++ >nul 2>&1
    if !errorlevel! equ 0 (
        set "runtime_ir_file=!emitted_ir_file!"
        set runtime_ir_temp=0
        if not defined runtime_ir_file set "runtime_ir_file="
        if defined runtime_ir_file if not exist "!runtime_ir_file!" set "runtime_ir_file="

        if not defined runtime_ir_file (
            set module_start=
            for /f "tokens=1 delims=:" %%n in ('findstr /n "=== Final Generated" "!output_file!"') do set module_start=%%n
            if defined module_start (
                set /a module_start+=1
                set "runtime_ir_file=%HYBRID_TEST_TMP_DIR%\hybrid_ir_%RANDOM%.ll"
                set runtime_ir_temp=1
                powershell -NoProfile -Command "(Get-Content -Path '!output_file!'^| Select-Object -Skip !module_start! ^| Where-Object {$_ -notmatch '^(ready^>|Parsed|Generated|\\[generics|\\[arc-trace\\]|\\[arc-escape\\])'}) ^| Set-Content -Path '!runtime_ir_file!'" >nul 2>&1
            )
        )

        if defined runtime_ir_file if exist "!runtime_ir_file!" (
            set "temp_bin=%HYBRID_TEST_TMP_DIR%\hybrid_bin_%RANDOM%.exe"
            findstr /C:"define i32 @main" "!runtime_ir_file!" >nul 2>&1
            if !errorlevel! equ 0 (
                set needs_runtime=%ARC_RUNTIME_REQUIRED%
                set "RUN_OPTS=!run_opts!"
                for /f %%v in ('powershell -NoProfile -Command "$s=$env:RUN_OPTS; if ($s -match '(^|\\s)--arc-debug(\\s|$)' -or $s -match '(^|\\s)--arc-leak-detect(\\s|$)' -or $s -match '(^|\\s)--arc-verify-runtime(\\s|$)' -or $s -match '(^|\\s)--arc-trace-retains(\\s|$)') { 1 } else { 0 }"') do if "%%v"=="1" set needs_runtime=1
                findstr "__hybrid_shared_control __hybrid_smart_" "!runtime_ir_file!" >nul 2>&1 && set needs_runtime=1
                findstr "hybrid_release hybrid_retain hybrid_autorelease hybrid_alloc_array hybrid_alloc_object" "!runtime_ir_file!" >nul 2>&1 && set needs_runtime=1

                if !needs_runtime! equ 1 (
                    if defined PREBUILT_ARC_RUNTIME_LIB if exist "!PREBUILT_ARC_RUNTIME_LIB!" (
                        clang++ "!runtime_ir_file!" "!PREBUILT_ARC_RUNTIME_LIB!" -o "!temp_bin!" >nul 2>&1
                        if !errorlevel! neq 0 (
                            rem Retry with source files when prebuilt runtime archive is incompatible.
                            clang++ "!runtime_ir_file!" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "!temp_bin!" >nul 2>&1
                        )
                    ) else (
                        clang++ "!runtime_ir_file!" src/runtime_support.cpp src/runtime/arc.cpp src/runtime/weak_table.cpp src/memory/ref_count.cpp -Isrc -Iruntime/include -std=c++17 -o "!temp_bin!" >nul 2>&1
                    )
                ) else (
                    clang++ "!runtime_ir_file!" "%RUNTIME_LIB%" -o "!temp_bin!" >nul 2>&1
                    if !errorlevel! neq 0 if defined PREBUILT_RUNTIME_STUB_LIB if /i "%RUNTIME_LIB%"=="%PREBUILT_RUNTIME_STUB_LIB%" (
                        rem Retry with a freshly built stub when prebuilt runtime archive is incompatible.
                        set "fallback_stub_obj=%HYBRID_TEST_TMP_DIR%\hybrid_runtime_stub_%RANDOM%.obj"
                        clang -c "runtime\test_runtime_stub.c" -o "!fallback_stub_obj!" >nul 2>&1
                        if !errorlevel! equ 0 (
                            clang++ "!runtime_ir_file!" "!fallback_stub_obj!" -o "!temp_bin!" >nul 2>&1
                        )
                        if exist "!fallback_stub_obj!" del "!fallback_stub_obj!" >nul 2>&1
                    )
                )

                if !errorlevel! equ 0 (
                    "!temp_bin!" > "!runtime_log!" 2>&1
                    set runtime_exit_code=!errorlevel!
                    set runtime_ran=1
                    if not "!runtime_exit_code!"=="0" set runtime_failed=1
                ) else (
                    set runtime_failed=1
                )
            )
            if exist "!temp_bin!" del "!temp_bin!" >nul 2>&1
        )

        if !runtime_ir_temp! equ 1 if defined runtime_ir_file if exist "!runtime_ir_file!" del "!runtime_ir_file!" >nul 2>&1
    )
)

if %expected_runtime_count% gtr 0 (
    if !runtime_ran! equ 0 (
        set runtime_expectation_failed=1
        echo [runtime-expectation] runtime did not run>>"!output_file!"
    ) else if not exist "!runtime_log!" (
        set runtime_expectation_failed=1
        echo [runtime-expectation] missing runtime output>>"!output_file!"
    ) else (
        for /l %%e in (1,1,!expected_runtime_count!) do (
            set "needle=!expected_runtime_%%e!"
            if defined needle (
                findstr /C:"!needle!" "!runtime_log!" >nul 2>&1
                if !errorlevel! neq 0 (
                    set runtime_expectation_failed=1
                    echo [runtime-expectation] missing: !needle!>>"!output_file!"
                )
            )
        )
    )
)

if defined expected_exit (
    if !runtime_ran! equ 0 (
        set runtime_expectation_failed=1
        echo [runtime-expectation] missing runtime exit>>"!output_file!"
    ) else (
        set "expected_exit_lower=!expected_exit!"
        for %%A in (!expected_exit_lower!) do set "expected_exit_lower=%%A"
        set exit_expectation_failed=0
        if /i "!expected_exit_lower!"=="nonzero" (
            if "!runtime_exit_code!"=="0" set exit_expectation_failed=1
        ) else if /i "!expected_exit_lower!"=="zero" (
            if not "!runtime_exit_code!"=="0" set exit_expectation_failed=1
        ) else if /i "!expected_exit_lower!"=="abort" (
            if not "!runtime_exit_code!"=="134" set exit_expectation_failed=1
        ) else if /i "!expected_exit_lower!"=="sigabrt" (
            if not "!runtime_exit_code!"=="134" set exit_expectation_failed=1
        ) else (
            if not "!runtime_exit_code!"=="!expected_exit!" set exit_expectation_failed=1
        )
        if !exit_expectation_failed! neq 0 (
            set runtime_expectation_failed=1
            echo [runtime-expectation] expected exit !expected_exit!, got !runtime_exit_code!>>"!output_file!"
        )
    )
)

if /i "!expected_mode!"=="fail" (
    if /i "!expected_fail_kind!"=="compile" (
        if !compile_failed! equ 1 if !compiler_expectation_failed! equ 0 if !runtime_expectation_failed! equ 0 set test_passed=1
    ) else if /i "!expected_fail_kind!"=="runtime" (
        if !compile_failed! equ 0 if !compiler_expectation_failed! equ 0 if !runtime_failed! equ 1 if !runtime_expectation_failed! equ 0 set test_passed=1
    ) else if /i "!expected_fail_kind!"=="any" (
        if !compiler_expectation_failed! equ 0 if !runtime_expectation_failed! equ 0 (
            if !compile_failed! equ 1 set test_passed=1
            if !runtime_failed! equ 1 set test_passed=1
        )
    ) else (
        if !compile_failed! equ 1 if !compiler_expectation_failed! equ 0 if !runtime_expectation_failed! equ 0 set test_passed=1
    )
) else (
    if !compile_failed! equ 0 if !compiler_expectation_failed! equ 0 if !runtime_failed! equ 0 if !runtime_expectation_failed! equ 0 set test_passed=1
)

set "expected_label=expected to pass"
if /i "!expected_mode!"=="fail" (
    if /i "!expected_fail_kind!"=="any" (
        set "expected_label=expected failure"
    ) else (
        set "expected_label=expected !expected_fail_kind! failure"
    )
)

if %FAILURES_ONLY% equ 0 (
    set show_output=1
) else if !test_passed! equ 0 (
    set show_output=1
) else (
    set show_output=0
)
if !machine_mode! equ 1 if !test_passed! equ 1 set show_output=0

if %VERBOSE_MODE% equ 1 if !show_output! equ 1 (
    call :ensure_progress_newline
    echo %BLUE%=== Test: !test_name! ===%NC%
    echo File: !test_file!
    echo Content:
    findstr /n /r /c:".*" "!test_file!"
    echo.
    echo Output:
    type "!output_file!"
    echo.
    if exist "!runtime_log!" (
        echo Runtime output:
        type "!runtime_log!"
        echo.
    )
    if /i "!expected_mode!"=="fail" (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name! ^(!expected_label!^)%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name! ^(!expected_label!^)%NC%
        )
    ) else (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name!%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name!%NC%
        )
    )
    if !test_passed! equ 0 (
        if not "!exit_code!"=="0" (
            echo %RED%  Compilation exit code: !exit_code!%NC%
        ) else if !compile_failed! equ 1 (
            echo %RED%  Compiler diagnostics detected%NC%
        )
        if !runtime_ran! equ 1 (
            if not "!runtime_exit_code!"=="0" (
                echo %RED%  Runtime exit code: !runtime_exit_code!%NC%
            )
        ) else if /i "!expected_mode!"=="fail" if /i "!expected_fail_kind!"=="runtime" (
            echo %RED%  Runtime did not run%NC%
        )
        if !compiler_expectation_failed! neq 0 if !runtime_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        ) else if !compiler_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        ) else if !runtime_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        )
        if !compile_failed! neq 0 (
            echo %RED%  Compiler output:%NC%
            powershell -NoProfile -Command "$lines = Select-String -Path '!output_file!' -Pattern 'Error','Warning' | Select-Object -First 5; if ($lines) { $lines | ForEach-Object { $_.Line } } else { Get-Content -Path '!output_file!' -TotalCount 5 }"
        ) else if !compiler_expectation_failed! neq 0 (
            echo %RED%  Compiler output:%NC%
            powershell -NoProfile -Command "$lines = Select-String -Path '!output_file!' -Pattern 'Error','Warning' | Select-Object -First 5; if ($lines) { $lines | ForEach-Object { $_.Line } } else { Get-Content -Path '!output_file!' -TotalCount 5 }"
        )
        if !runtime_expectation_failed! neq 0 (
            if exist "!runtime_log!" (
                echo %RED%  Runtime output:%NC%
                powershell -NoProfile -Command "Get-Content -Path '!runtime_log!' -TotalCount 5"
            )
        ) else if !runtime_failed! neq 0 (
            if exist "!runtime_log!" (
                echo %RED%  Runtime output:%NC%
                powershell -NoProfile -Command "Get-Content -Path '!runtime_log!' -TotalCount 5"
            )
        )
    )
    echo.
) else if !show_output! equ 1 (
    call :ensure_progress_newline
    echo %YELLOW%Running test: !test_name!%NC%
    echo ----------------------------------------
    if /i "!expected_mode!"=="fail" (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name! ^(!expected_label!^)%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name! ^(!expected_label!^)%NC%
        )
    ) else (
        if !test_passed! equ 1 (
            echo %GREEN%✓ PASSED: !test_name!%NC%
        ) else (
            echo %RED%✗ FAILED: !test_name!%NC%
        )
    )
    if !test_passed! equ 0 (
        if not "!exit_code!"=="0" (
            echo %RED%  Compilation exit code: !exit_code!%NC%
        ) else if !compile_failed! equ 1 (
            echo %RED%  Compiler diagnostics detected%NC%
        )
        if !runtime_ran! equ 1 (
            if not "!runtime_exit_code!"=="0" (
                echo %RED%  Runtime exit code: !runtime_exit_code!%NC%
            )
        ) else if /i "!expected_mode!"=="fail" if /i "!expected_fail_kind!"=="runtime" (
            echo %RED%  Runtime did not run%NC%
        )
        if !compiler_expectation_failed! neq 0 if !runtime_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        ) else if !compiler_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        ) else if !runtime_expectation_failed! neq 0 (
            echo %RED%  Expectation mismatch detected%NC%
        )
        if !compile_failed! neq 0 (
            echo %RED%  Compiler output:%NC%
            powershell -NoProfile -Command "$lines = Select-String -Path '!output_file!' -Pattern 'Error','Warning' | Select-Object -First 5; if ($lines) { $lines | ForEach-Object { $_.Line } } else { Get-Content -Path '!output_file!' -TotalCount 5 }"
        ) else if !compiler_expectation_failed! neq 0 (
            echo %RED%  Compiler output:%NC%
            powershell -NoProfile -Command "$lines = Select-String -Path '!output_file!' -Pattern 'Error','Warning' | Select-Object -First 5; if ($lines) { $lines | ForEach-Object { $_.Line } } else { Get-Content -Path '!output_file!' -TotalCount 5 }"
        )
        if !runtime_expectation_failed! neq 0 (
            if exist "!runtime_log!" (
                echo %RED%  Runtime output:%NC%
                powershell -NoProfile -Command "Get-Content -Path '!runtime_log!' -TotalCount 5"
            )
        ) else if !runtime_failed! neq 0 (
            if exist "!runtime_log!" (
                echo %RED%  Runtime output:%NC%
                powershell -NoProfile -Command "Get-Content -Path '!runtime_log!' -TotalCount 5"
            )
        )
    )
    echo.
)

if !machine_mode! equ 0 (
    if !test_passed! equ 1 (
        set /a PASSED_TESTS+=1
    ) else (
        set /a FAILED_TESTS+=1
    )
)

if !machine_mode! equ 0 call :mark_test_completed

call :get_epoch test_wall_end
set /a test_elapsed=test_wall_end-test_wall_start
if !counts_for_timing! equ 1 (
    set /a TIMED_DURATION+=test_elapsed
    set /a TIMED_TESTS+=1
)
if !machine_mode! equ 1 if defined RESULT_FILE (
    > "!RESULT_FILE!" (
        echo test_file=!test_file!
        echo test_name=!test_name!
        echo test_passed=!test_passed!
        echo counts_for_timing=!counts_for_timing!
        echo test_elapsed=!test_elapsed!
    )
)

if exist "!output_file!" del "!output_file!" >nul 2>&1
if exist "!runtime_log!" del "!runtime_log!" >nul 2>&1
if defined emitted_ir_file if exist "!emitted_ir_file!" del "!emitted_ir_file!" >nul 2>&1
if exist "!expected_diag_file!" del "!expected_diag_file!" >nul 2>&1
if exist "!actual_diag_file!" del "!actual_diag_file!" >nul 2>&1
if exist "!missing_diag_file!" del "!missing_diag_file!" >nul 2>&1
if exist "!unexpected_diag_file!" del "!unexpected_diag_file!" >nul 2>&1
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
    if /i "!multi_name:~-6!"=="_error" set "multi_expect=fail"

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

    set "multi_temp=%HYBRID_TEST_TMP_DIR%\hybrid_multi_%RANDOM%"
    set "multi_output=!multi_temp!.out"
    set "multi_stdout=!multi_temp!.log"
    set "multi_runtime=!multi_temp!.run"

    "!HYBRID_EXEC!" !EXTRA_COMPILER_ARGS! !cmd_files! -o "!multi_output!" > "!multi_stdout!" 2>&1
    set multi_status=!errorlevel!

    set runtime_status=0
    if /i "!multi_expect!"=="pass" (
        if !multi_status! equ 0 (
            if exist "!multi_output!" (
                "!multi_output!" > "!multi_runtime!" 2>&1
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
