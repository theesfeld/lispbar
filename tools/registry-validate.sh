#!/bin/sh
# tools/registry-validate.sh -- verify the registry manifest is consistent
# with the files it references.  Exits non-zero on the first problem.
#
# Used by CI on every PR; also runnable locally as a pre-submit check.

set -eu

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MANIFEST="$ROOT/registry/manifest.lisp"

ok()  { printf '  \033[32m✓\033[0m %s\n' "$1"; }
err() { printf '  \033[31m✗\033[0m %s\n' "$1" >&2; }

[ -f "$MANIFEST" ] || { err "manifest not found: $MANIFEST"; exit 1; }

cd "$ROOT/registry"

# Parse the manifest once into a flat /tmp file of "name file sha"
# triples that subsequent steps can re-read.
PARSED=$(mktemp)
trap 'rm -f "$PARSED"' EXIT

awk '
  function flush() {
      if (name != "" && file != "" && sha != "")
          print name " " file " " sha
      name = ""; file = ""; sha = ""
  }
  /^\(register/ {
      flush()
      line = $0
      sub(/^\(register[[:space:]]+/, "", line)
      split(line, a, /[[:space:]]+/)
      name = a[1]
      next
  }
  /:file[[:space:]]+"/ {
      match($0, /"[^"]*"/)
      file = substr($0, RSTART+1, RLENGTH-2)
  }
  /:sha256[[:space:]]+"/ {
      match($0, /"[^"]*"/)
      sha = substr($0, RSTART+1, RLENGTH-2)
  }
  END { flush() }
' "$MANIFEST" > "$PARSED"

if [ ! -s "$PARSED" ]; then
    err "no (register …) entries parsed from manifest.lisp"
    exit 1
fi

# Step 1: every file under registry/{modules,themes}/ must have a
# matching entry in the manifest.
echo "==> manifest.lisp consistency"
fail=0
manifest_files=$(awk '{print $2}' "$PARSED")
for kind in modules themes; do
    for f in "$kind"/*.lisp; do
        [ -e "$f" ] || continue
        if echo "$manifest_files" | grep -qx "$f"; then
            ok "manifest references $f"
        else
            err "$f is on disk but not in manifest.lisp"
            fail=1
        fi
    done
done
for f in $manifest_files; do
    [ -f "$f" ] || { err "manifest declares $f which doesn't exist"; fail=1; }
done
[ "$fail" = 0 ] || exit 1

# Step 2: SHA-256 of every file must match what the manifest claims.
echo
echo "==> SHA-256 verification"
fail=0
while read -r name file expected; do
    actual=$(sha256sum "$file" | cut -d' ' -f1)
    if [ "$actual" = "$expected" ]; then
        ok "$name ($file)"
    else
        err "$name: manifest=$expected  actual=$actual"
        fail=1
    fi
done < "$PARSED"
[ "$fail" = 0 ] || exit 1

# Step 3: every file must byte-compile against a minimal stub of the
# :lispbar package.  Catches typos and macro misuse without needing a
# full build.
echo
echo "==> byte-compile check (SBCL)"
if ! command -v sbcl >/dev/null 2>&1; then
    err "sbcl not on PATH; skipping byte-compile checks"
    exit 0
fi

cd "$ROOT"
fail=0
for f in registry/modules/*.lisp registry/themes/*.lisp; do
    [ -e "$f" ] || continue
    output=$(sbcl --non-interactive --no-userinit --no-sysinit \
        --eval '(load "tools/registry-stub.lisp")' \
        --eval "(handler-case (load \"$f\")
                   (error (c)
                     (format t \"COMPILE-FAIL: ~A~%\" c)
                     (sb-ext:exit :code 1)))" \
        2>&1) || {
        err "$f failed to load:"
        printf '%s\n' "$output" | sed 's/^/      /' >&2
        fail=1
        continue
    }
    ok "$f"
done
[ "$fail" = 0 ] || exit 1

echo
echo "Registry validation passed."
