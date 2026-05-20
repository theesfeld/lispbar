#!/bin/sh
# tests/run-tests.sh -- run every test file under tests/ in a fresh SBCL.
#
# Used by `make test' and by CI.  Each test file is responsible for
# loading the :lispbar system, doing its assertions, and exiting with
# a non-zero code on failure.

set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if ! command -v sbcl >/dev/null 2>&1; then
    printf '%s: sbcl not on PATH\n' "$0" >&2
    exit 2
fi

cd "$ROOT"

fail=0
for f in tests/test-*.lisp; do
    [ -e "$f" ] || continue
    printf '==> %s\n' "$f"
    if ! sbcl --non-interactive --load "$f" 2>&1; then
        fail=1
    fi
    echo
done

if [ "$fail" != 0 ]; then
    echo "tests: FAILED" >&2
    exit 1
fi
echo "tests: PASS"
