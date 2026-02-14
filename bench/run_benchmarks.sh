#!/usr/bin/env bash
set -u

MAIN=${MAIN:-./clang/main}
TIMEOUT=${TIMEOUT:-10}
threads=(1 2 4 8 12)

run_with_timeout() {
  local out_var="$1" t="$2" marker cap_file pid status
  shift 2
  marker="$(mktemp "${TMPDIR:-/tmp}/hvm4-timeout.XXXXXX")" || return 1
  cap_file="$(mktemp "${TMPDIR:-/tmp}/hvm4-capture.XXXXXX")" || {
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

if [ ! -x "$MAIN" ]; then
  echo "error: $MAIN not found or not executable" >&2
  exit 1
fi

shopt -s nullglob
bench_files=(bench/*.hvm4 bench/par/*.hvm4)
shopt -u nullglob

if [ ${#bench_files[@]} -eq 0 ]; then
  echo "error: no benchmarks found under bench/" >&2
  exit 1
fi

IFS=$'\n' bench_files=($(printf '%s\n' "${bench_files[@]}" | sort))
unset IFS

name_w=4
for file in "${bench_files[@]}"; do
  name_len=${#file}
  if [ $name_len -gt $name_w ]; then
    name_w=$name_len
  fi
done
name_w=$((name_w + 2))

printf "%-*s" "$name_w" "test"
for t in "${threads[@]}"; do
  printf "%8s" "T$t"
done
echo

for file in "${bench_files[@]}"; do
  printf "%-*s" "$name_w" "$file"
  for t in "${threads[@]}"; do
    extra_args=()
    case "$(basename "$file")" in
      gen_*.hvm4 )
        extra_args+=("-C1")
        ;;
      collapse_*.hvm4 )
        extra_args+=("-C")
        ;;
    esac
    run_with_timeout out "$TIMEOUT" "$MAIN" "$file" -s -S -T"$t" "${extra_args[@]+"${extra_args[@]}"}"
    status=$?
    if [ $status -eq 124 ]; then
      val="timeout"
    elif [ $status -ne 0 ]; then
      val="error"
    else
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
