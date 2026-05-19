# Contributing to the Lispbar registry

Thanks for wanting to share something with other lispbar users.
This guide describes the registry's review standards and the
mechanics of getting a PR merged.

## Before you start

* The registry is **curated**.  Maintainers will reject submissions
  that don't fit, that duplicate existing entries, or that don't
  meet the quality bar below.  None of this is personal — open an
  issue first if you want to gauge interest in a larger module.
* Modules should be useful to **multiple users**.  A "show my
  desk lamp colour" module that hardcodes one device API is
  probably better as a personal extension under your own
  `~/.config/lispbar/modules/`.
* Themes should reflect a recognisable, **named** colour scheme
  (Tokyo Night, Solarized, Gruvbox, …) so users know what to
  expect.

## Submission checklist

For a module:

- [ ] One file at `registry/modules/<name>.lisp`.
- [ ] Starts with `(in-package :lispbar)`.
- [ ] One `(defmodule :<name> …)` form (no auxiliary modules).
- [ ] Update function returns the documented shape
      (`NIL` / string / `(:text … :face …)` / `(:fragments …)`).
- [ ] No `*` user-config defvars without `(defvar ... "docstring")`.
- [ ] Optional but encouraged: `:tooltip` and `:on-click`.
- [ ] No network calls without an explicit `*foo-url*` defvar the
      user can override or NIL-out.
- [ ] No writes outside `$XDG_STATE_HOME/lispbar/` or
      `$XDG_CACHE_HOME/lispbar/` (use the exported helpers
      `lispbar-state-directory` / `lispbar-cache-directory`).
- [ ] Errors in user-supplied callbacks are swallowed (so a bad
      handler doesn't crash the bar).

For a theme:

- [ ] One file at `registry/themes/<name>.lisp`.
- [ ] Starts with `(in-package :lispbar)`.
- [ ] One `(define-theme :<name> …)` form with **all seven**
      semantic faces (`:bg :normal :accent :ok :warn :urgent :muted`).
- [ ] RGBA values in 0.0-1.0 range (not 0-255).
- [ ] One blank line and a comment at the top citing the source
      palette (the original project, a colour-scheme URL, etc.).

For both:

- [ ] One new `(register …)` entry in `manifest.lisp`.  Required and
      optional fields below.
- [ ] Updated entry in `registry/README.md`'s module / theme table.

### Manifest fields

Required on every entry:

| Field         | Example                                               |
|---------------|-------------------------------------------------------|
| `:kind`       | `:module` or `:theme`                                 |
| `:file`       | `"modules/foo.lisp"` or `"themes/foo.lisp"`           |
| `:sha256`     | the file's SHA-256 (`sha256sum file`)                 |
| `:summary`    | one-line description shown in `registry list`         |
| `:version`    | semver string, e.g. `"1.0.0"`                         |
| `:license`    | SPDX identifier, e.g. `"GPL-3.0-or-later"`            |
| `:added`      | ISO date the item was first submitted (`"2026-05-19"`)|
| `:updated`    | ISO date of the most recent change                    |

Optional:

| Field         | Example                                               |
|---------------|-------------------------------------------------------|
| `:description`| multi-line full description (paragraph or two)        |
| `:author`     | GitHub handle or display name                         |
| `:homepage`   | upstream URL for the colour scheme / API / etc.       |
| `:tags`       | `'("system" "weather")` — for filtering in `list`     |
| `:requires`   | `'("curl" "jq")` — external programs the module needs |

Notes:

* `:tags` and `:requires` are **quoted** lists (`'("a" "b")`).  The
  manifest is read with `eval`, so an unquoted list reads as a
  function call.
* For brand-new submissions, set `:added` and `:updated` to the same
  date.  On follow-up PRs touching the file, bump both `:version`
  and `:updated`.  The validator enforces this: if the entry's
  `:sha256` differs from master's, `:version` and `:updated` must
  also differ.
* `:description` may span several lines; the client renders it
  verbatim under `registry info`.

## Style

* Two-space comment lead-in (`;;;` for headers, `;;` for prose,
  `;` for inline code annotations) — same as the built-ins.
* 80-column soft limit on docstrings; hard limit 100 cols.
* Don't shadow anything from `cl:` or `:lispbar`.

## Validation

CI runs `tools/registry-validate.sh` on every PR.  It:

1. Recomputes SHA-256 for every file in the registry.
2. Compares against the value in `manifest.lisp`.
3. Byte-compiles each `.lisp` file in isolation against an
   image with the `:lispbar` package mocked.
4. Rejects the PR if any check fails.

You can run the same checks locally:

```sh
sh tools/registry-validate.sh
```

## Licence

Submissions are merged under the project's GPL-3.0-or-later
licence.  By submitting you agree to that licensing.  Don't
submit code you didn't write or aren't otherwise free to license
that way.

## Etiquette

* If your module duplicates an existing one but does something
  meaningfully different, name it accordingly
  (`:weather-metno` vs `:weather` etc.).
* If you find a bug in an existing module, fix it in the same PR
  rather than adding a parallel version.
* If a module needs follow-up changes after merge, please come
  back and PR them — abandoning is fine, but maintainers might
  remove unmaintained items if real users hit problems with
  them.
