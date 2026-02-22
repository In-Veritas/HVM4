#!/usr/bin/env bash
# AOT test runner wrapper
# -----------------------
# Runs the regular suite through standalone AOT execution (`--as-c`).

set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
FLAGS="--as-c"
if [ -n "${HVM_TEST_FLAGS:-}" ]; then
  FLAGS="$FLAGS ${HVM_TEST_FLAGS}"
fi

TIMEOUT="${HVM_TEST_TIMEOUT_SECS:-20}"
HVM_TEST_FLAGS="$FLAGS" \
HVM_TEST_TIMEOUT_SECS="$TIMEOUT" \
HVM_TEST_AS_C_ONLY=1 \
"$DIR/_all_.sh" "$@"
