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
  function strval(s,    m) {
      match(s, /"[^"]*"/)
      return substr(s, RSTART+1, RLENGTH-2)
  }
  function flush() {
      if (name != "" && file != "" && sha != "")
          print name "|" file "|" sha "|" summary "|" version \
                "|" license "|" added "|" updated
      name = ""; file = ""; sha = ""; summary = ""; version = ""
      license = ""; added = ""; updated = ""
  }
  /^\(register/ {
      flush()
      line = $0
      sub(/^\(register[[:space:]]+/, "", line)
      split(line, a, /[[:space:]]+/)
      name = a[1]
      next
  }
  /:file[[:space:]]+"/    { file    = strval($0) }
  /:sha256[[:space:]]+"/  { sha     = strval($0) }
  /:summary[[:space:]]+"/ { summary = strval($0) }
  /:version[[:space:]]+"/ { version = strval($0) }
  /:license[[:space:]]+"/ { license = strval($0) }
  /:added[[:space:]]+"/   { added   = strval($0) }
  /:updated[[:space:]]+"/ { updated = strval($0) }
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
manifest_files=$(awk -F'|' '{print $2}' "$PARSED")
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
while IFS='|' read -r name file expected summary version license added updated; do
    actual=$(sha256sum "$file" | cut -d' ' -f1)
    if [ "$actual" = "$expected" ]; then
        ok "$name ($file)"
    else
        err "$name: manifest=$expected  actual=$actual"
        fail=1
    fi
done < "$PARSED"
[ "$fail" = 0 ] || exit 1

# Step 2b: every required metadata field must be present and well-formed.
echo
echo "==> metadata schema"
fail=0
date_re='^[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]$'
ver_re='^[0-9]+\.[0-9]+\.[0-9]+([-+][0-9A-Za-z.-]+)?$'
while IFS='|' read -r name file sha summary version license added updated; do
    issues=""
    [ -n "$summary" ] || issues="$issues :summary"
    [ -n "$version" ] || issues="$issues :version"
    [ -n "$license" ] || issues="$issues :license"
    [ -n "$added"   ] || issues="$issues :added"
    [ -n "$updated" ] || issues="$issues :updated"
    if [ -n "$version" ] && ! echo "$version" | grep -Eq "$ver_re"; then
        issues="$issues :version-not-semver($version)"
    fi
    if [ -n "$added" ] && ! echo "$added" | grep -Eq "$date_re"; then
        issues="$issues :added-not-iso($added)"
    fi
    if [ -n "$updated" ] && ! echo "$updated" | grep -Eq "$date_re"; then
        issues="$issues :updated-not-iso($updated)"
    fi
    if [ -n "$added" ] && [ -n "$updated" ] && \
       [ "$(printf '%s\n%s\n' "$added" "$updated" | sort | head -1)" != "$added" ]; then
        issues="$issues :added-after-updated"
    fi
    if [ -z "$issues" ]; then
        ok "$name"
    else
        err "$name: missing/invalid$issues"
        fail=1
    fi
done < "$PARSED"
[ "$fail" = 0 ] || exit 1

# Step 2c: if a file's SHA-256 differs from master's, the entry's
# :version and :updated must also have changed.  Skips silently if
# we're not in a git work tree or master is unreachable (e.g. a fresh
# clone with no remote).
echo
echo "==> version bump check (vs origin/master)"
if ! git -C "$ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    ok "skipped (not a git work tree)"
else
    base="$(git -C "$ROOT" merge-base HEAD origin/master 2>/dev/null || true)"
    if [ -z "$base" ]; then
        ok "skipped (no origin/master to diff against)"
    else
        fail=0
        while IFS='|' read -r name file sha summary version license added updated; do
            master_entry=$(git -C "$ROOT" show "$base:registry/manifest.lisp" 2>/dev/null | \
                awk -v n="$name" '
                  function strval(s) {
                      match(s, /"[^"]*"/); return substr(s, RSTART+1, RLENGTH-2)
                  }
                  /^\(register/ { in_block = ($0 ~ "register[[:space:]]+" n "([[:space:]]|$)") }
                  in_block && /:sha256[[:space:]]+"/  { ms = strval($0) }
                  in_block && /:version[[:space:]]+"/ { mv = strval($0) }
                  in_block && /:updated[[:space:]]+"/ { mu = strval($0) }
                  END { print ms "|" mv "|" mu }')
            ms=$(echo "$master_entry" | cut -d'|' -f1)
            mv=$(echo "$master_entry" | cut -d'|' -f2)
            mu=$(echo "$master_entry" | cut -d'|' -f3)
            if [ -z "$ms" ]; then
                ok "$name (new entry)"
                continue
            fi
            if [ "$ms" = "$sha" ]; then
                ok "$name (unchanged)"
                continue
            fi
            # Content changed: enforce version and updated bumps.
            if [ "$mv" = "$version" ]; then
                err "$name: file changed but :version still $version"
                fail=1
            elif [ "$mu" = "$updated" ]; then
                err "$name: file changed but :updated still $updated"
                fail=1
            else
                ok "$name (bumped $mv → $version, $mu → $updated)"
            fi
        done < "$PARSED"
        [ "$fail" = 0 ] || exit 1
    fi
fi

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
