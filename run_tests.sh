#!/usr/bin/env bash
# Run all root .hvm4 test files in 4 modes and generate a markdown report.
#
# Modes:
#   0. SNF 1T:  Normalize (strong normal form), single-thread     (-s)
#   1. SNF MT:  Normalize (strong normal form), multi-thread       (-s -T10)
#   2. CNF 1T:  Collapse  (collapsed normal form), single-thread   (-s -C)
#   3. CNF MT:  Collapse  (collapsed normal form), multi-thread    (-s -C -T10)
#
# Output: test_results.md

set -uo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
BIN="$DIR/clang/main"
OUT="$DIR/test_results.md"
TMPDIR_RUN="$(mktemp -d)"

cleanup() { rm -rf "$TMPDIR_RUN"; }
trap cleanup EXIT

# Build
if [ ! -f "$BIN.c" ]; then
  echo "error: expected C entrypoint at $BIN.c" >&2
  exit 1
fi
echo "Building HVM4..."
(cd "$DIR/clang" && clang -O2 -o main main.c) || { echo "Build failed"; exit 1; }

# Collect root .hvm4 files (excluding strings.hvm4 and rand_list.hvm4)
shopt -s nullglob
files=()
for f in "$DIR"/*.hvm4; do
  name="$(basename "$f")"
  case "$name" in
    strings.hvm4|rand_list.hvm4) continue ;;
    *) files+=("$f") ;;
  esac
done
shopt -u nullglob

if [ ${#files[@]} -eq 0 ]; then
  echo "No .hvm4 files found in $DIR"
  exit 1
fi

# Sort files by name
IFS=$'\n' files=($(sort <<<"${files[*]}")); unset IFS

# Strip ANSI codes, stats lines, and collapse annotations (#N suffix)
strip_output() {
  sed 's/\x1b\[[0-9;]*m//g' \
    | grep -v '^- Itrs' \
    | grep -v '^- Time' \
    | grep -v '^- Perf' \
    | grep -v '^- Heap' \
    | sed 's/ #[0-9]*$//' \
    | sed 's/ *$//'
}

# ── Main loop ─────────────────────────────────────────────────

mode_flags=("-s" "-s -T10" "-s -C" "-s -C -T10")
mode_names=("SNF 1T" "SNF MT" "CNF 1T" "CNF MT")

echo "Running tests..."

for f in "${files[@]}"; do
  name="$(basename "${f%.hvm4}")"
  base_dir="$TMPDIR_RUN/$name"
  mkdir -p "$base_dir"

  for k in 0 1 2 3; do
    $BIN "$f" ${mode_flags[$k]} 2>&1 | strip_output > "$base_dir/$k" || true
    sort < "$base_dir/$k" > "$base_dir/${k}_sorted"
  done

  # Check all modes match (sorted, since collapse order can vary)
  ref="$(cat "$base_dir/0_sorted")"
  match=1
  for k in 1 2 3; do
    if [[ "$(cat "$base_dir/${k}_sorted")" != "$ref" ]]; then
      match=0; break
    fi
  done
  echo "$match" > "$base_dir/match"

  if [[ $match -eq 1 ]]; then
    echo "  $name: PASS"
  else
    echo "  $name: MISMATCH"
  fi
done

# ── Generate markdown report ──────────────────────────────────

{
  echo "# HVM4 Test Results"
  echo ""
  echo "Generated: $(date '+%Y-%m-%d %H:%M:%S')"
  echo ""

  # Mode descriptions
  echo "## Modes"
  echo ""
  echo "Each program is executed in four modes to verify consistent results:"
  echo ""
  echo "| Mode | Flags | Description |"
  echo "|------|-------|-------------|"
  echo "| **SNF 1T** | \`-s\` | Strong Normal Form, single-threaded. Fully reduces the term, keeping SUPs and DUPs in the output. |"
  echo "| **SNF MT** | \`-s -T10\` | Strong Normal Form, multi-threaded (10 workers). Same reduction as SNF 1T but parallelized. |"
  echo "| **CNF 1T** | \`-s -C\` | Collapsed Normal Form, single-threaded. Eliminates SUPs/DUPs and enumerates all branches as pure lambda terms. |"
  echo "| **CNF MT** | \`-s -C -T10\` | Collapsed Normal Form, multi-threaded (10 workers). Same collapse as CNF 1T but parallelized. |"
  echo ""
  echo "A test **passes** when all four modes produce the same output (after sorting, since collapse branch order is non-deterministic)."
  echo ""

  # Results grid
  echo "## Results"
  echo ""

  # Header row
  header="| Program |"
  separator="|---------|"
  for k in 0 1 2 3; do
    header+=" ${mode_names[$k]} |"
    separator+="--------|"
  done
  header+=" Match |"
  separator+="-------|"
  echo "$header"
  echo "$separator"

  for f in "${files[@]}"; do
    name="$(basename "${f%.hvm4}")"
    base_dir="$TMPDIR_RUN/$name"
    match="$(cat "$base_dir/match")"

    row="| \`$name\` |"
    ref="$(cat "$base_dir/0_sorted")"
    for k in 0 1 2 3; do
      out="$(cat "$base_dir/${k}_sorted")"
      if [[ "$out" == "$ref" ]]; then
        row+=" :white_check_mark: |"
      else
        row+=" :x: |"
      fi
    done
    if [[ "$match" == "1" ]]; then
      row+=" :white_check_mark: |"
    else
      row+=" :x: |"
    fi
    echo "$row"
  done

  echo ""

  # Detailed output per file
  echo "## Output Details"
  echo ""

  for f in "${files[@]}"; do
    name="$(basename "${f%.hvm4}")"
    base_dir="$TMPDIR_RUN/$name"
    match="$(cat "$base_dir/match")"

    echo "### \`$name\`"
    echo ""

    if [[ "$match" == "1" ]]; then
      echo "All 4 modes produced identical output:"
      echo ""
      echo "\`\`\`"
      cat "$base_dir/0"
      echo "\`\`\`"
    else
      echo "**Outputs differ across modes.**"
      echo ""
      for k in 0 1 2 3; do
        echo "**${mode_names[$k]}** (\`${mode_flags[$k]}\`):"
        echo "\`\`\`"
        cat "$base_dir/$k"
        echo "\`\`\`"
        echo ""
      done
    fi

    echo ""
    echo "---"
    echo ""
  done

} > "$OUT"

echo ""
echo "Report written to: $OUT"
