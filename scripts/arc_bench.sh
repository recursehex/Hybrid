#!/bin/bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

RUNS=5
WARN_THRESHOLD=8
FAIL_THRESHOLD=15
DO_BUILD=1
OUT_DIR="${ARC_BENCH_OUT_DIR:-build/arc-bench}"
BENCH_FILTERS=()
COLLECT_PASS_TIMING=0

usage() {
    cat <<'EOF'
Usage: ./scripts/arc_bench.sh [OPTIONS] [BENCH_FILTER...]

Runs ARC benchmark workloads under ARC-on and ARC-off modes and writes
comparison artifacts into build/arc-bench/.

Options:
  --runs N                 Number of repeated runs per benchmark/mode (default: 5)
  --warn-threshold PCT     Warn when ARC-on delta exceeds this percent (default: 8)
  --fail-threshold PCT     Fail when ARC-on delta exceeds this percent (default: 15)
  --fail-threshold off     Disable fail threshold
  --pass-timing            Capture compiler [pass-timing] stages into artifacts
  --no-build               Skip ./build.sh before benchmark runs
  -h, --help               Show this help text

Examples:
  ./scripts/arc_bench.sh
  ./scripts/arc_bench.sh --pass-timing
  ./scripts/arc_bench.sh --runs 7 --warn-threshold 6
  ./scripts/arc_bench.sh object_churn
EOF
}

is_positive_int() {
    [[ "$1" =~ ^[0-9]+$ ]] && [ "$1" -gt 0 ]
}

is_number() {
    [[ "$1" =~ ^[0-9]+([.][0-9]+)?$ ]]
}

compare_gt() {
  awk -v lhs="$1" -v rhs="$2" 'BEGIN { exit !(lhs > rhs) }'
}

extract_stage_seconds() {
    local stage="$1"
    sed -n "s/.*\\[pass-timing\\] stage=${stage} seconds=\\([0-9.][0-9.]*\\).*/\\1/p" | tail -1
}

median_values() {
    if [ $# -eq 0 ]; then
        echo "0"
        return
    fi

    local sorted=()
    while IFS= read -r value; do
        sorted+=("$value")
    done < <(printf "%s\n" "$@" | sort -n)
    local count=${#sorted[@]}
    local middle=$((count / 2))

    if [ $((count % 2)) -eq 1 ]; then
        printf "%s" "${sorted[$middle]}"
    else
        awk -v a="${sorted[$((middle - 1))]}" -v b="${sorted[$middle]}" \
            'BEGIN { printf "%.6f", (a + b) / 2.0 }'
    fi
}

json_escape() {
    printf "%s" "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --runs)
            if [ $# -lt 2 ] || ! is_positive_int "$2"; then
                echo "Error: --runs requires a positive integer" >&2
                exit 1
            fi
            RUNS="$2"
            shift 2
            ;;
        --runs=*)
            value="${1#--runs=}"
            if ! is_positive_int "$value"; then
                echo "Error: --runs requires a positive integer" >&2
                exit 1
            fi
            RUNS="$value"
            shift
            ;;
        --warn-threshold)
            if [ $# -lt 2 ] || ! is_number "$2"; then
                echo "Error: --warn-threshold requires a numeric value" >&2
                exit 1
            fi
            WARN_THRESHOLD="$2"
            shift 2
            ;;
        --warn-threshold=*)
            value="${1#--warn-threshold=}"
            if ! is_number "$value"; then
                echo "Error: --warn-threshold requires a numeric value" >&2
                exit 1
            fi
            WARN_THRESHOLD="$value"
            shift
            ;;
        --fail-threshold)
            if [ $# -lt 2 ]; then
                echo "Error: --fail-threshold requires a value or 'off'" >&2
                exit 1
            fi
            value=$(printf "%s" "$2" | tr '[:upper:]' '[:lower:]')
            if [ "$value" = "off" ]; then
                FAIL_THRESHOLD=""
            elif is_number "$2"; then
                FAIL_THRESHOLD="$2"
            else
                echo "Error: --fail-threshold requires a numeric value or 'off'" >&2
                exit 1
            fi
            shift 2
            ;;
        --fail-threshold=*)
            value="${1#--fail-threshold=}"
            lower=$(printf "%s" "$value" | tr '[:upper:]' '[:lower:]')
            if [ "$lower" = "off" ]; then
                FAIL_THRESHOLD=""
            elif is_number "$value"; then
                FAIL_THRESHOLD="$value"
            else
                echo "Error: --fail-threshold requires a numeric value or 'off'" >&2
                exit 1
            fi
            shift
            ;;
        --no-build)
            DO_BUILD=0
            shift
            ;;
        --pass-timing)
            COLLECT_PASS_TIMING=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            BENCH_FILTERS+=("$1")
            shift
            ;;
    esac
done

if [ $DO_BUILD -eq 1 ]; then
    echo "Building compiler for benchmarks..."
    ./build.sh
fi

HYBRID_EXEC_PATH="${HYBRID_EXEC:-./build/hybrid}"
if [ ! -x "$HYBRID_EXEC_PATH" ]; then
    echo "Error: compiler executable not found or not executable: $HYBRID_EXEC_PATH" >&2
    echo "Hint: run ./build.sh or set HYBRID_EXEC to a built compiler path." >&2
    exit 1
fi

BENCH_ROOT="test/bench/arc"
if [ ! -d "$BENCH_ROOT" ]; then
    echo "Error: benchmark directory '$BENCH_ROOT' not found" >&2
    exit 1
fi

ALL_BENCHES=()
while IFS= read -r bench_file; do
    ALL_BENCHES+=("$bench_file")
done < <(find "$BENCH_ROOT" -maxdepth 1 -type f -name "*.hy" | sort)
if [ ${#ALL_BENCHES[@]} -eq 0 ]; then
    echo "Error: no benchmark files found in '$BENCH_ROOT'" >&2
    exit 1
fi

BENCHES=()
if [ ${#BENCH_FILTERS[@]} -eq 0 ]; then
    BENCHES=("${ALL_BENCHES[@]}")
else
    for bench in "${ALL_BENCHES[@]}"; do
        name=$(basename "$bench" .hy)
        matched=0
        for filter in "${BENCH_FILTERS[@]}"; do
            if [[ "$name" == *"$filter"* ]] || [[ "$bench" == *"$filter"* ]]; then
                matched=1
                break
            fi
        done
        if [ $matched -eq 1 ]; then
            BENCHES+=("$bench")
        fi
    done
fi

if [ ${#BENCHES[@]} -eq 0 ]; then
    echo "Error: no benchmarks matched filters: ${BENCH_FILTERS[*]}" >&2
    exit 1
fi

mkdir -p "$OUT_DIR"
timestamp=$(date -u +"%Y%m%dT%H%M%SZ")
raw_csv="${OUT_DIR}/arc_bench_raw_${timestamp}.csv"
summary_csv="${OUT_DIR}/arc_bench_summary_${timestamp}.csv"
json_file="${OUT_DIR}/arc_bench_${timestamp}.json"
pass_csv="${OUT_DIR}/arc_bench_pass_timing_${timestamp}.csv"

echo "benchmark,mode,run,elapsed_seconds" > "$raw_csv"
echo "benchmark,median_arc_on_seconds,median_arc_off_seconds,delta_percent,status,compile_arc_on_total_seconds,compile_arc_off_total_seconds,compile_delta_percent" > "$summary_csv"
if [ $COLLECT_PASS_TIMING -eq 1 ]; then
    echo "benchmark,mode,stage,seconds" > "$pass_csv"
fi

BENCH_NAME_LIST=()
BENCH_PATH_LIST=()
MEDIAN_ON_LIST=()
MEDIAN_OFF_LIST=()
DELTA_LIST=()
STATUS_LIST=()
COMPILE_ON_TOTAL_LIST=()
COMPILE_OFF_TOTAL_LIST=()
COMPILE_DELTA_LIST=()

any_fail=0
warnings=()

run_and_measure() {
    local mode="$1"
    local bench_path="$2"
    local run_index="$3"

    local output=""
    local status=0
    set +e
    output=$(./run_tests.sh -a "$mode" "$bench_path" 2>&1)
    status=$?
    set -e

    if [ $status -ne 0 ]; then
        echo "$output"
        echo "Error: benchmark run failed (mode=$mode, benchmark=$bench_path, run=$run_index)" >&2
        exit $status
    fi

    local elapsed=""
    elapsed=$(printf "%s\n" "$output" | sed -n 's/.*Total elapsed: \([0-9.][0-9.]*\)s.*/\1/p' | tail -1)
    if [ -z "$elapsed" ]; then
        echo "$output"
        echo "Error: unable to parse elapsed runtime from run_tests output" >&2
        exit 1
    fi

    printf "%s" "$elapsed"
}

collect_pass_timing() {
    local mode="$1"
    local bench_path="$2"
    local bench_name="$3"
    local arc_enabled_value="true"
    if [ "$mode" = "off" ]; then
        arc_enabled_value="false"
    fi

    local temp_ir
    temp_ir=$(mktemp /tmp/hybrid_arc_bench_pass.XXXXXX)
    mv "$temp_ir" "${temp_ir}.ll"
    temp_ir="${temp_ir}.ll"

    local output=""
    local status=0
    set +e
    output=$(HYBRID_PASS_TIMING=1 "$HYBRID_EXEC_PATH" --arc-enabled="$arc_enabled_value" --emit-llvm -o "$temp_ir" "$bench_path" 2>&1)
    status=$?
    set -e
    rm -f "$temp_ir"

    if [ $status -ne 0 ]; then
        echo "$output"
        echo "Error: pass-timing compile failed (mode=$mode, benchmark=$bench_path)" >&2
        exit $status
    fi

    while IFS= read -r stage_line; do
        [ -z "$stage_line" ] && continue
        stage_name=${stage_line%%,*}
        stage_seconds=${stage_line#*,}
        echo "${bench_name},${mode},${stage_name},${stage_seconds}" >> "$pass_csv"
    done < <(
        printf "%s\n" "$output" \
            | sed -n 's/.*\[pass-timing\] stage=\([^ ]*\) seconds=\([0-9.][0-9.]*\).*/\1,\2/p'
    )

    local total_seconds=""
    total_seconds=$(printf "%s\n" "$output" | extract_stage_seconds "total")
    if [ -z "$total_seconds" ]; then
        total_seconds="0.000000"
    fi
    printf "%s" "$total_seconds"
}

echo "Running ARC benchmarks (${#BENCHES[@]} workloads, ${RUNS} runs/mode)..."

for bench in "${BENCHES[@]}"; do
    bench_name=$(basename "$bench" .hy)
    echo
    echo "Benchmark: $bench_name"

    on_times=()
    off_times=()
    compile_on_total=""
    compile_off_total=""

    if [ $COLLECT_PASS_TIMING -eq 1 ]; then
        compile_on_total=$(collect_pass_timing "on" "$bench" "$bench_name")
        compile_off_total=$(collect_pass_timing "off" "$bench" "$bench_name")
    fi

    for run_index in $(seq 1 "$RUNS"); do
        on_elapsed=$(run_and_measure "on" "$bench" "$run_index")
        on_times+=("$on_elapsed")
        echo "${bench_name},on,${run_index},${on_elapsed}" >> "$raw_csv"
        printf "  arc=on  run %d/%d: %ss\n" "$run_index" "$RUNS" "$on_elapsed"
    done

    for run_index in $(seq 1 "$RUNS"); do
        off_elapsed=$(run_and_measure "off" "$bench" "$run_index")
        off_times+=("$off_elapsed")
        echo "${bench_name},off,${run_index},${off_elapsed}" >> "$raw_csv"
        printf "  arc=off run %d/%d: %ss\n" "$run_index" "$RUNS" "$off_elapsed"
    done

    on_median=$(median_values "${on_times[@]}")
    off_median=$(median_values "${off_times[@]}")
    delta_pct=$(awk -v on="$on_median" -v off="$off_median" \
        'BEGIN { if (off == 0) { printf "0.00" } else { printf "%.2f", ((on - off) / off) * 100.0 } }')

    status="ok"
    if compare_gt "$delta_pct" "$WARN_THRESHOLD"; then
        status="warn"
    fi
    if [ -n "$FAIL_THRESHOLD" ] && compare_gt "$delta_pct" "$FAIL_THRESHOLD"; then
        status="fail"
        any_fail=1
    fi

    if [ "$status" = "warn" ] || [ "$status" = "fail" ]; then
        warnings+=("${bench_name}: ARC-on delta ${delta_pct}%")
    fi

    compile_delta=""
    if [ $COLLECT_PASS_TIMING -eq 1 ]; then
        compile_delta=$(awk -v on="$compile_on_total" -v off="$compile_off_total" \
            'BEGIN { if (off == 0) { printf "0.00" } else { printf "%.2f", ((on - off) / off) * 100.0 } }')
    fi

    BENCH_NAME_LIST+=("$bench_name")
    BENCH_PATH_LIST+=("$bench")
    MEDIAN_ON_LIST+=("$on_median")
    MEDIAN_OFF_LIST+=("$off_median")
    DELTA_LIST+=("$delta_pct")
    STATUS_LIST+=("$status")
    COMPILE_ON_TOTAL_LIST+=("$compile_on_total")
    COMPILE_OFF_TOTAL_LIST+=("$compile_off_total")
    COMPILE_DELTA_LIST+=("$compile_delta")

    echo "${bench_name},${on_median},${off_median},${delta_pct},${status},${compile_on_total},${compile_off_total},${compile_delta}" >> "$summary_csv"
done

host_os=$(uname -s)
host_arch=$(uname -m)
llvm_version=$(llvm-config --version 2>/dev/null || echo "unknown")
build_type=$(cmake -N -L build 2>/dev/null | sed -n 's/^CMAKE_BUILD_TYPE:STRING=//p' | head -1)
if [ -z "$build_type" ]; then
    build_type="unknown"
fi
git_rev=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")

{
    echo "{"
    echo "  \"timestamp_utc\": \"$(json_escape "$timestamp")\","
    echo "  \"runs_per_mode\": ${RUNS},"
    echo "  \"thresholds\": {"
    echo "    \"warn_percent\": ${WARN_THRESHOLD},"
    if [ -n "$FAIL_THRESHOLD" ]; then
        echo "    \"fail_percent\": ${FAIL_THRESHOLD}"
    else
        echo "    \"fail_percent\": null"
    fi
    echo "  },"
    echo "  \"environment\": {"
    echo "    \"host_os\": \"$(json_escape "$host_os")\","
    echo "    \"host_arch\": \"$(json_escape "$host_arch")\","
    echo "    \"llvm_version\": \"$(json_escape "$llvm_version")\","
    echo "    \"build_type\": \"$(json_escape "$build_type")\","
    echo "    \"git_revision\": \"$(json_escape "$git_rev")\""
    echo "  },"
    echo "  \"benchmarks\": ["
    first=1
    for ((i=0; i<${#BENCH_NAME_LIST[@]}; i++)); do
        bench_name="${BENCH_NAME_LIST[$i]}"
        if [ $first -eq 0 ]; then
            echo ","
        fi
        first=0
        echo "    {"
        echo "      \"name\": \"$(json_escape "$bench_name")\","
        echo "      \"path\": \"$(json_escape "${BENCH_PATH_LIST[$i]}")\","
        echo "      \"median_arc_on_seconds\": ${MEDIAN_ON_LIST[$i]},"
        echo "      \"median_arc_off_seconds\": ${MEDIAN_OFF_LIST[$i]},"
        echo "      \"delta_percent\": ${DELTA_LIST[$i]},"
        if [ -n "${COMPILE_ON_TOTAL_LIST[$i]}" ]; then
            echo "      \"compile_arc_on_total_seconds\": ${COMPILE_ON_TOTAL_LIST[$i]},"
            echo "      \"compile_arc_off_total_seconds\": ${COMPILE_OFF_TOTAL_LIST[$i]},"
            echo "      \"compile_delta_percent\": ${COMPILE_DELTA_LIST[$i]},"
        else
            echo "      \"compile_arc_on_total_seconds\": null,"
            echo "      \"compile_arc_off_total_seconds\": null,"
            echo "      \"compile_delta_percent\": null,"
        fi
        echo "      \"status\": \"$(json_escape "${STATUS_LIST[$i]}")\""
        echo -n "    }"
    done
    echo
    echo "  ],"
    echo "  \"artifacts\": {"
    echo "    \"raw_csv\": \"$(json_escape "$raw_csv")\","
    echo "    \"summary_csv\": \"$(json_escape "$summary_csv")\","
    if [ $COLLECT_PASS_TIMING -eq 1 ]; then
        echo "    \"pass_timing_csv\": \"$(json_escape "$pass_csv")\""
    else
        echo "    \"pass_timing_csv\": null"
    fi
    echo "  }"
    echo "}"
} > "$json_file"

echo
echo "ARC benchmark summary:"
printf "%-24s %12s %12s %10s %8s\n" "Benchmark" "ARC-on(s)" "ARC-off(s)" "Delta(%)" "Status"
for ((i=0; i<${#BENCH_NAME_LIST[@]}; i++)); do
    printf "%-24s %12.4f %12.4f %10.2f %8s\n" \
        "${BENCH_NAME_LIST[$i]}" \
        "${MEDIAN_ON_LIST[$i]}" \
        "${MEDIAN_OFF_LIST[$i]}" \
        "${DELTA_LIST[$i]}" \
        "${STATUS_LIST[$i]}"
done

echo
echo "Artifacts:"
echo "  Raw runs:    $raw_csv"
echo "  Summary CSV: $summary_csv"
echo "  Summary JSON:$json_file"
if [ $COLLECT_PASS_TIMING -eq 1 ]; then
    echo "  Pass timing: $pass_csv"
fi

if [ ${#warnings[@]} -gt 0 ]; then
    echo
    echo "Threshold notices:"
    for warning in "${warnings[@]}"; do
        echo "  - $warning"
    done
fi

if [ $any_fail -eq 1 ]; then
    echo
    echo "ARC benchmark regression exceeded fail threshold." >&2
    exit 2
fi
