#!/usr/bin/env bash
# scripts/bench.sh
# ================
# Benchmark runner for HVM4.
# Clones/caches the bench repo, builds the C binary, and runs
# benchmarks across multiple thread counts in a table format.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CACHE_DIR="$ROOT_DIR/.cache/bench"
BENCH_DIR="$CACHE_DIR/bench"
MAIN="$ROOT_DIR/clang/main"
BENCH_REPO="https://github.com/HigherOrderCO/bench.git"
TIMEOUT=${TIMEOUT:-10}
THREADS=(1 2 4 8 12)

# Help
# ----

# Prints usage and exits
show_help() {
  echo "Usage: scripts/bench.sh [--interpreted | --compiled]"
  echo ""
  echo "  --interpreted  Run benchmarks via the C interpreter"
  echo "  --compiled     Run benchmarks via AOT compilation (--as-c)"
  echo ""
  echo "The bench repo is cloned/cached at .cache/bench/."
  exit 0
}

# Args
# ----

# Parse mode from flags
MODE=""
for arg in "$@"; do
  case "$arg" in
    --interpreted ) MODE="interpreted" ;;
    --compiled    ) MODE="compiled" ;;
    --help | -h   ) show_help ;;
    * )
      echo "error: unknown flag '$arg'" >&2
      show_help
      ;;
  esac
done

if [ -z "$MODE" ]; then
  show_help
fi

# Timeout
# -------

# Runs a command with a timeout; stores output in the named variable
run_with_timeout() {
  local out_var="$1" t="$2" marker cap_file pid status
  shift 2
  marker="$(mktemp "${TMPDIR:-/tmp}/hvm-timeout.XXXXXX")" || return 1
  cap_file="$(mktemp "${TMPDIR:-/tmp}/hvm-capture.XXXXXX")" || {
    rm -f "$marker"
    return 1
  }
  rm -f "$marker"
  "$@" >"$cap_file" 2>&1 & pid=$!
  ( sleep "$t"; kill -0 "$pid" 2>/dev/null || exit 0; : > "$marker"; kill -KILL "$pid" 2>/dev/null || true ) &
  wait "$pid" 2>/dev/null; status=$?
  [ -f "$marker" ] && status=124
  printf -v "$out_var" '%s' "$(cat "$cap_file")"
  rm -f "$cap_file" "$marker"
  return $status
}

# Cache
# -----

# Clones or updates the bench repo
sync_bench_repo() {
  if [ -d "$CACHE_DIR/.git" ]; then
    echo "Updating bench repo..."
    (cd "$CACHE_DIR" && git pull --quiet)
  else
    echo "Cloning bench repo..."
    mkdir -p "$(dirname "$CACHE_DIR")"
    git clone --quiet "$BENCH_REPO" "$CACHE_DIR"
  fi
}

# Build
# -----

# Compiles the C binary
build_main() {
  if [ ! -f "$MAIN.c" ]; then
    echo "error: expected C entrypoint at $MAIN.c" >&2
    exit 1
  fi
  echo "Building clang/main..."
  (cd "$ROOT_DIR/clang" && clang -O2 -o main main.c)
}

# Bench
# -----

# Runs all benchmarks and prints a results table
run_benchmarks() {
  # Collect bench files: .cache/bench/bench/*/main.hvm
  shopt -s nullglob
  local bench_files=("$BENCH_DIR"/*/main.hvm)
  shopt -u nullglob

  if [ ${#bench_files[@]} -eq 0 ]; then
    echo "error: no benchmarks found under $BENCH_DIR/*/main.hvm" >&2
    exit 1
  fi

  IFS=$'\n' bench_files=($(printf '%s\n' "${bench_files[@]}" | sort))
  unset IFS

  # Compute column width from bench names
  local name_w=4
  for file in "${bench_files[@]}"; do
    local name
    name="$(basename "$(dirname "$file")")"
    local len=${#name}
    if [ $len -gt $name_w ]; then
      name_w=$len
    fi
  done
  name_w=$((name_w + 2))

  # Print header
  printf "%-*s" "$name_w" "bench"
  for t in "${THREADS[@]}"; do
    printf "%8s" "T$t"
  done
  echo

  # Build mode-specific flags
  local mode_flags=()
  if [ "$MODE" = "compiled" ]; then
    mode_flags+=("--as-c")
  fi

  # Run each benchmark across thread counts
  for file in "${bench_files[@]}"; do
    local name
    name="$(basename "$(dirname "$file")")"
    printf "%-*s" "$name_w" "$name"

    for t in "${THREADS[@]}"; do
      local extra_args=()
      case "$name" in
        gen_*      ) extra_args+=("-C1") ;;
        collapse_* ) extra_args+=("-C") ;;
      esac

      run_with_timeout out "$TIMEOUT" \
        "$MAIN" "$file" -s -S "-T$t" \
        "${mode_flags[@]+"${mode_flags[@]}"}" \
        "${extra_args[@]+"${extra_args[@]}"}"
      local status=$?

      local val
      if [ $status -eq 124 ]; then
        val="timeout"
      elif [ $status -ne 0 ]; then
        val="error"
      else
        local perf_line
        perf_line=$(printf '%s\n' "$out" | awk -F': ' '/^- Perf:/{print $2; exit}')
        if [ -z "$perf_line" ]; then
          val="n/a"
        else
          val=${perf_line%% *}
        fi
      fi
      printf "%8s" "$val"
    done
    echo
  done
}

# Main
# ----

sync_bench_repo
build_main
run_benchmarks
