# CLAUDE.md - Lispbar project notes for Claude

## What this project is

Lispbar is a **native Wayland status bar** written in **Common Lisp**.
The deliverable is the single executable `lispbar`, produced by
`make build` (which calls SBCL's `sb-ext:save-lisp-and-die`).

There is no Emacs in the picture. The earlier Elisp prototype lives
only in the git history (commits prior to the SBCL pivot).

## Layout

```
/
├── Makefile               make build / install / clean / test
├── build.lisp             sb-ext:save-lisp-and-die driver
├── lispbar.asd            ASDF system definition
├── protocols/             vendored Wayland protocol XML
│   └── wlr-layer-shell-unstable-v1.xml
├── cshim/                 ~300-line C glue (the only C in the project)
│   ├── wlbar.c            wl_display + layer-surface + wl_shm
│   ├── wlbar.h            ~15-symbol FFI surface for SBCL
│   └── Makefile
├── src/                   the whole bar in Common Lisp
│   ├── package.lisp       :lispbar package + exports
│   ├── log.lisp           stderr structured logging
│   ├── xdg.lisp           XDG Base Directory helpers
│   ├── theme.lisp         theme registry + define-theme + built-in themes
│   ├── config.lisp        XDG-aware config loader + extension discovery
│   ├── module.lisp        defmodule macro + module registry + faces
│   ├── modules/           built-in modules
│   ├── output/
│   │   ├── stdout.lisp    plain-text + JSON drivers
│   │   └── wayland.lisp   wlr-layer-shell + cairo/pango renderer
│   └── main.lisp          entry, arg parsing, signal handlers
├── examples/              sample config + sample user module / theme
└── systemd/lispbar.service  user unit
```

## Working rules

- **Lisp first.** The C shim does exactly what it has to (generated
  wayland-scanner marshalling, shm buffers, event dispatch). Every
  user-visible feature belongs in `src/`.
- **No Emacs anywhere.** Never propose Emacs-package idioms,
  `defcustom`, `eieio`, `defmodule` based on `make-instance` of an
  EIEIO class, etc.  This is Common Lisp - `defclass`, plists,
  `defmacro`.
- **XDG-compliant.** All user paths come from `src/xdg.lisp`.
  Never hard-code `~/.config/...` or `/etc/...` outside of that
  file.
- **Extensions are first-class.** Users drop `*.lisp` files under
  `$XDG_CONFIG_HOME/lispbar/{modules,themes}/` and they're picked
  up at boot via `load-extensions`.  The same `defmodule` and
  `define-theme` macros work in built-in source and in user files.

## Conventions

- Public symbols are exported from the `:lispbar` package; user
  extensions live in that package via `(in-package :lispbar)`.
- Faces: `:bg :normal :accent :ok :warn :urgent :muted`.
- Module return values: either a string, `(:text X :face F)`, or
  `(:fragments ((TEXT1 FACE1) (TEXT2 FACE2) ...))`.
- Themes are property lists of face -> `(R G B A)` doubles 0.0-1.0.

## Build & run

```sh
make build                  # produces ./lispbar
./lispbar --print-paths     # show XDG resolution
./lispbar --show-extensions # show loaded user files
./lispbar --once            # one frame to stdout (for scripts)
./lispbar                   # streams forever, picks driver from config
sudo make install           # /usr/local/{bin,lib,share}/lispbar/...
```

## When in doubt

Read `README.md` first - it's the source of truth for users; if it
disagrees with code, the README is wrong and we should fix it.
