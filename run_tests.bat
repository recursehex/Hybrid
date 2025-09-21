@echo off
setlocal enabledelayedexpansion

:: Test runner for Hybrid on Windows
:: Usage: run_tests.bat [options] [test_pattern]
::   -v, --verbose  Show detailed output for each test
::   -h, --help     Display this help message
::   test_pattern   Run only tests matching this pattern (optional)

set VERBOSE=0
set TEST_PATTERN=

:: Parse arguments
:parse_args
if "%1"=="" goto end_parse
if "%1"=="-v" goto set_verbose
if "%1"=="--verbose" goto set_verbose
if "%1"=="-h" goto show_help
if "%1"=="--help" goto show_help
set TEST_PATTERN=%1
shift
goto parse_args

:set_verbose
set VERBOSE=1
shift
goto parse_args

:show_help
echo Test runner for Hybrid
echo Usage: run_tests.bat [options] [test_pattern]
echo   -v, --verbose  Show detailed output for each test
echo   -h, --help     Display this help message
echo   test_pattern   Run only tests matching this pattern (optional)
exit /b 0

:end_parse

:: Check if hybrid executable exists
set HYBRID_EXE=build\release\hybrid.exe
if not exist %HYBRID_EXE% (
    set HYBRID_EXE=build\debug\hybrid.exe
    if not exist !HYBRID_EXE! (
        echo Error: Hybrid executable not found. Please build first.
        exit /b 1
    )
)

echo Using hybrid executable: %HYBRID_EXE%

:: Initialize counters
set /a PASSED=0
set /a FAILED=0
set /a TOTAL=0

:: Run tests
echo Running Hybrid test suite...
echo.

:: Find and run test files
for %%f in (test\*.hy) do (
    set TEST_FILE=%%~nxf
    set SHOULD_RUN=1

    :: Check if test matches pattern
    if not "%TEST_PATTERN%"=="" (
        echo !TEST_FILE! | findstr /i "%TEST_PATTERN%" >nul
        if errorlevel 1 set SHOULD_RUN=0
    )

    if !SHOULD_RUN!==1 (
        set /a TOTAL+=1

        :: Check if this is expected to fail
        echo !TEST_FILE! | findstr /i "fail" >nul
        if errorlevel 1 (
            set EXPECT_FAIL=0
        ) else (
            set EXPECT_FAIL=1
        )

        :: Run the test
        if %VERBOSE%==1 (
            echo Testing: !TEST_FILE!
            %HYBRID_EXE% < test\!TEST_FILE! 2>&1
            set TEST_RESULT=!errorlevel!
        ) else (
            %HYBRID_EXE% < test\!TEST_FILE! >nul 2>&1
            set TEST_RESULT=!errorlevel!
        )

        :: Check result
        if !EXPECT_FAIL!==1 (
            if !TEST_RESULT!==0 (
                echo [FAIL] !TEST_FILE! - Expected to fail but passed
                set /a FAILED+=1
            ) else (
                echo [PASS] !TEST_FILE! - Failed as expected
                set /a PASSED+=1
            )
        ) else (
            if !TEST_RESULT!==0 (
                echo [PASS] !TEST_FILE!
                set /a PASSED+=1
            ) else (
                echo [FAIL] !TEST_FILE! - Unexpected failure
                set /a FAILED+=1
                if %VERBOSE%==1 (
                    echo Retrying with output:
                    %HYBRID_EXE% < test\!TEST_FILE! 2>&1
                )
            )
        )
    )
)

:: Print summary
echo.
echo =============================
echo Test Results:
echo   Passed: %PASSED%/%TOTAL%
echo   Failed: %FAILED%/%TOTAL%
echo =============================

if %FAILED% GTR 0 (
    exit /b 1
) else (
    echo All tests passed!
    exit /b 0
)