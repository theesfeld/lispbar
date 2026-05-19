# Registry — sharing modules and themes

The **registry** is a curated, community-driven library of modules
and themes that ships in this repository under `registry/`.  Lispbar
ships with a `registry` subcommand that fetches items from it on
demand.  Nothing in the registry is loaded automatically — you opt
in, one item at a time.

## Quick tour

```sh
lispbar registry list                     # everything available
lispbar registry list --kind theme        # just themes
lispbar registry browse                   # interactive fzf picker
lispbar registry info :weather            # details for one item
lispbar registry install :weather         # download + verify + place
lispbar registry install :tokyo-night     # themes work the same
lispbar registry remove  :weather         # delete it again
lispbar registry update                   # refresh all installed
```

The leading `:` on the name is optional — `weather` and `:weather`
both work.

## Interactive browser

`lispbar registry browse` opens an [fzf](https://github.com/junegunn/fzf)
TUI with:

- The full inventory in the left pane (one item per line, with a
  `*` marker for items already installed).
- A live preview on the right showing the output of
  `lispbar registry info` for the highlighted entry.
- **Enter** installs (or re-downloads) the highlighted item.
- **Esc** cancels.

If you don't have `fzf` installed, the command prints a hint and
exits — fall back to `list` + `install` in that case.

There's also a built-in **`:registry`** module: drop it into a
placement list and you get a single-glyph button on the bar that
spawns a terminal running `lispbar registry browse` on left-click.
The seeded config already places it on the right.  Tunables:

| Variable             | Default                                      |
|----------------------|----------------------------------------------|
| `*registry-label*`   | `"📦"` — set to `NIL` to hide the module     |
| `*registry-terminal*`| Shell expression resolving the terminal cmd  |
| `*registry-on-click*`| Override the click action entirely           |

## Where things land

Registry items use the regular XDG extension paths.  A successful
install lays things out exactly as if you had hand-written the file:

| Kind   | Destination                                                |
|--------|------------------------------------------------------------|
| module | `$XDG_CONFIG_HOME/lispbar/modules/<name>.lisp`             |
| theme  | `$XDG_CONFIG_HOME/lispbar/themes/<name>.lisp`              |

Bookkeeping (so `remove` and `update` know what's there) lives in
`$XDG_STATE_HOME/lispbar/registry-installed.lisp`.  The cached copy
of the manifest is in `$XDG_CACHE_HOME/lispbar/registry-manifest.lisp`
and refreshes every hour by default.

After installing a module you still have to **enable** it in
`config.lisp`:

```lisp
(placement :left   '(:workspaces))
(placement :center '(:clock))
(placement :right  '(:weather :network :clock))   ; <-- here
```

Themes are activated similarly:

```lisp
(theme :tokyo-night)
```

## Trust model

Everything downloaded is verified against a SHA-256 stored in the
registry's `manifest.lisp`:

1. CI on this repo runs `tools/registry-validate.sh` on every PR.
   That script recomputes hashes, byte-compiles each file against a
   stub `:lispbar` package, and rejects the PR if anything is off.
2. The client (`lispbar registry install`) re-checks the SHA on
   download and deletes the file if it doesn't match.
3. The file lands in `$XDG_CONFIG_HOME` like any user extension; it
   isn't loaded with any special privilege.

That said: registry items are **Common Lisp code**.  They run inside
your `lispbar` process and can call anything the binary can call
(read your files, run programs, talk to the network).  Treat
`registry install` like installing any other piece of software from
GitHub — read what you're pulling in if it matters to you.

## Pointing at a fork or a mirror

The base URL is a defvar:

```lisp
;; In ~/.config/lispbar/config.lisp
(in-package :lispbar)
(setf *registry-base-url*
      "https://raw.githubusercontent.com/myfork/lispbar/master/registry")
;; or a local mirror for development:
(setf *registry-base-url* "file:///srv/lispbar-registry")
```

Anything `curl` accepts as a URL is fine.  `manifest.lisp`,
`modules/*.lisp` and `themes/*.lisp` must all live under that prefix.

## Submitting your own

See `registry/CONTRIBUTING.md` for the full checklist.  In short:

1. Drop a single `.lisp` file under `registry/modules/` or
   `registry/themes/`.
2. Add a `(register …)` entry to `registry/manifest.lisp`, including
   the SHA-256 of your file (`sha256sum registry/modules/foo.lisp`).
3. Update the table in `registry/README.md`.
4. Open a PR.

CI runs the same `tools/registry-validate.sh` you can run locally:

```sh
sh tools/registry-validate.sh
```

If it's green and a maintainer merges, **every user gets the new
item on their next `lispbar registry list/install/update`** — no
lispbar release required.  The client fetches the live manifest
from `master`.

## See also

- `registry/README.md` — index of what's currently published.
- `registry/CONTRIBUTING.md` — review standards and PR mechanics.
- `docs/modules.md`, `docs/themes.md` — how to write a module / theme
  from scratch (registry items follow the same conventions).
