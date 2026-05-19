# Architecture

This document is for people hacking on Lispbar itself.  End users
should read [configuration.md](configuration.md) and
[modules.md](modules.md) instead.

## Source layout

```
.
├── Makefile             build / install / clean
├── build.lisp           sb-ext:save-lisp-and-die driver
├── lispbar.asd          ASDF system definition
├── protocols/
│   └── wlr-layer-shell-unstable-v1.xml
├── cshim/               C glue (compiled to two shared libraries)
│   ├── wlbar.{c,h}      Wayland: wl_display + layer surfaces + wl_shm + pointer
│   ├── wltray.{c,h}     D-Bus: SNI host (Watcher + per-item refresh + invoke)
│   └── Makefile         builds libwlbar.so + libwltray.so
├── src/                 everything else, in Common Lisp
│   ├── package.lisp     :lispbar package + public exports
│   ├── log.lisp         structured stderr logger
│   ├── xdg.lisp         XDG Base Directory helpers
│   ├── theme.lisp       theme registry + define-theme + built-ins
│   ├── config.lisp      DSL loader + extension discovery + seeding
│   ├── module.lisp      module class, defmodule macro, click dispatch
│   ├── modules/         11 built-in modules
│   ├── output/
│   │   ├── stdout.lisp  text + JSON drivers; fragment helpers
│   │   └── wayland.lisp wlr-layer-shell driver: render, hit-test, tooltips
│   └── main.lisp        entry, arg parsing, signal handlers
├── examples/            seeded user config + sample user module/theme
├── init/                {systemd, runit, openrc} integration recipes
└── docs/                you are here
```

## Layered design

```
        +-----------------------------------+
        |          src/main.lisp            |  arg parsing
        +-----------------------------------+
        |  output/wayland.lisp              |  layer-shell driver
        |  output/stdout.lisp               |  text/json driver
        +-----------------------------------+
        |  module.lisp + modules/*          |  module class + builtins
        +-----------------------------------+
        |  theme.lisp                       |  face → colour registry
        |  config.lisp                      |  DSL + XDG load + seeding
        |  xdg.lisp                         |  Base Dir helpers
        +-----------------------------------+
        |  CFFI to libwlbar / libwltray     |  thin Lisp wrappers
        +-----------------------------------+
        |  cshim/wlbar.c                    |  Wayland marshalling, shm,
        |  cshim/wltray.c                   |  D-Bus SNI host
        +-----------------------------------+
        |  libwayland-client, libdbus,      |  system libraries
        |  libcairo, libpango               |
        +-----------------------------------+
```

The C layer exists **only** because libwayland and libdbus require
generated marshalling code that's much cleaner in C than via raw
CFFI.  Everything user-visible — modules, themes, config, click
dispatch, rendering decisions — lives in Lisp.

## C shim API (libwlbar)

```c
/* Lifecycle */
int   wlbar_init(int height, int position,
                 int margin_top, int margin_right,
                 int margin_bottom, int margin_left);
void  wlbar_shutdown(void);

/* Per-output bar surface */
int   wlbar_output_count(void);
int   wlbar_output_width(int i);
int   wlbar_output_height(int i);
uint32_t *wlbar_output_pixels(int i);
int   wlbar_output_stride(int i);
void  wlbar_output_commit(int i);
const char *wlbar_output_name(int i);

/* Per-output tooltip surface (created lazily) */
int   wlbar_tooltip_show(int output, int anchor_x, int w, int h);
uint32_t *wlbar_tooltip_pixels(int output);
int   wlbar_tooltip_stride(int output);
void  wlbar_tooltip_commit(int output);
void  wlbar_tooltip_hide(int output);

/* Event loop */
int   wlbar_poll(int timeout_ms);
int   wlbar_closed(void);
int   wlbar_fd(void);

/* Pointer events */
struct wlbar_pointer_event {
    int output_idx;
    double x, y;
    int button;
    int pressed;
};
int   wlbar_poll_pointer_event(struct wlbar_pointer_event *out);
int   wlbar_pointer_hover(int *output_idx, double *x, double *y);
```

## C shim API (libwltray)

```c
int   wltray_init(void);
void  wltray_shutdown(void);
int   wltray_poll(int timeout_ms);
int   wltray_fd(void);

struct wltray_item {
    const char *id, *title, *status, *icon_name, *tooltip;
    int has_pixmap, pixmap_w, pixmap_h;
    const uint32_t *pixmap;
    const char *icon_path;        /* resolved via icon-theme search */
};

int   wltray_item_count(void);
int   wltray_item_get(int i, struct wltray_item *out);
unsigned wltray_revision(void);
void  wltray_invoke(int i, int button, int x, int y);
```

The pattern: C owns the OS-protocol state machines, exposes a
flat snapshot every render tick.  Lisp consumes the snapshot,
makes UI decisions, paints, and calls back into C for actions.

## Render loop

```
run-wayland
└─ loop while running:
   ├─ wlbar-poll(tick-ms)            -- pump compositor events
   ├─ drain-pointer-events           -- dispatch any queued clicks
   ├─ render-frame                   -- repaint every output
   │  └─ render-output(i):
   │     ├─ reset-bboxes(i)
   │     ├─ reset-subfragments(i)
   │     ├─ paint-background(cr, w, h)
   │     ├─ draw-section :left   …   -- records bboxes + subframes
   │     ├─ draw-section :center …
   │     ├─ draw-section :right  …
   │     ├─ wlbar-output-commit(i)
   │     └─ update-tooltip-overlay(i)  -- separate surface
```

Bookkeeping data:

| Hash table                  | Key → Value                                          |
| --------------------------- | ---------------------------------------------------- |
| `*module-bboxes*`           | output-idx → list of (MODULE X-START X-END)         |
| `*subfragment-handlers*`    | output-idx → list of (X-START X-END HANDLER DATA)   |
| `*tray-fragment-bboxes*`    | output-idx → list of (TRAY-FRAG X-START X-END)      |
| `*tooltip-state*`           | output-idx → (MODULE TEXT) most recently painted    |
| `*icon-cache*`              | path → cairo image surface                          |

## Click dispatch order

```
click on (output, x, button)
   │
   ├─ subfragment-at-x(output, x)         hit
   │  └─ run-subfragment-action(handler, data, module, button, output)
   │
   ├─ module-at-x(output, x)              hit on a module
   │  └─ dispatch-module-click(module, button, output, x)
   │     └─ module-action(module, button)
   │        └─ run-module-action(action, module, button, output)
   │
   └─ else: drop                          no module under click
```

## Fragment shapes the renderer recognises

```
("plain text" :face-keyword)
   plain colored text

(:gap PIXELS-OR-NIL)
   horizontal whitespace.  NIL = *wayland-module-gap*.

(:clickable :text "X" :face :accent :on-click HANDLER :data DATA)
   like text, but renderer records bbox in *subfragment-handlers*

(:tray-placeholder LIST-OF-TRAY-FRAGMENTS)
   special-cased by the renderer; iterates list and blits icons /
   text per tray item, recording per-item bboxes in
   *tray-fragment-bboxes*
```

Stdout/JSON drivers collapse `:gap` to a single space and
treat `:clickable` as plain text (no click info in text output).

## Theme dispatch

`(theme :name)` calls `apply-theme :name`, which:

1. Looks up `*themes*[name]` (defaults to `:default` if missing).
2. Sets `*theme-palette*` to that palette.
3. Subsequent `(theme-color :accent)` calls return the chosen RGBA.

Built-in themes are registered in `theme.lisp`; user themes drop
into `~/.config/lispbar/themes/` and are loaded by
`load-extensions`.

## Extension load order

`load-extensions` (in `config.lisp`) walks:

```
$XDG_CONFIG_HOME/lispbar/{modules,themes}/*.lisp        ; user
$XDG_CONFIG_DIRS/.../lispbar/{modules,themes}/*.lisp    ; system config
$XDG_DATA_HOME/lispbar/{modules,themes}/*.lisp          ; user data
$XDG_DATA_DIRS/.../lispbar/{modules,themes}/*.lisp      ; system data
```

Each file is `load`ed with `*package*` bound to `:lispbar`.
Errors are caught and logged at `:warn`; the next file still
loads.

## Build pipeline

```
make build
├─ make -C cshim
│   ├─ wayland-scanner private-code/client-header < .xml > .c/.h
│   ├─ cc wlbar.c   + generated  -> libwlbar.so
│   └─ cc wltray.c               -> libwltray.so
└─ sbcl --load build.lisp
    ├─ load ~/quicklisp/setup.lisp
    ├─ (ql:quickload :cffi)                       ; FASLs baked into image
    ├─ (asdf:load-system :lispbar)                ; src/*.lisp compiled
    └─ (sb-ext:save-lisp-and-die
          :executable t :compression t
          :save-runtime-options t
          :toplevel #'main-fn)
```

`save-lisp-and-die` produces a single ELF (~12 MB) containing the
SBCL runtime + compiled image + loaded foreign libs as dlopen
specs.  At runtime CFFI does the actual `dlopen` of
`libwlbar.so`, `libwltray.so`, `libcairo.so.2`, optionally
`libpango-1.0.so.0` / `libpangocairo-1.0.so.0`.

## Adding a new module

The shortest path: see [modules.md §Writing your own module](modules.md#writing-your-own-module).
If your module is universally useful and you want it built in:

1. Add `src/modules/foo.lisp` with `(defmodule :foo …)`.
2. Add `(:file "foo")` to the `:modules` entry of `lispbar.asd`.
3. Add a row to the built-in modules table in `docs/modules.md`.
4. Add the per-module options block to the seeded
   `examples/config.lisp`.

## Adding a new theme

1. `src/theme.lisp`: add `(define-theme :foo …)` near the
   bottom alongside the existing ones.
2. List it in the "Built-in" enumeration of `docs/themes.md`.
3. Mention it in the `(theme …)` comment in
   `examples/config.lisp`.

## Adding a new compositor backend

For the workspaces module, see [compositors.md §Adding a
compositor backend](compositors.md#adding-a-compositor-backend).

For any other compositor-specific functionality (window title,
fullscreen indicator, …), the pattern is the same: add a fetcher
that talks to the compositor's IPC, gate it behind an env-var
or probe-based detector, fall back to NIL when the compositor
isn't present.

## Coding conventions

- **Lisp first.**  Add behaviour in `src/`, not C, unless the
  feature *requires* a generated protocol marshaller or a
  performance-critical inner loop.
- **XDG everywhere.**  Never hard-code `~/.config/...` or
  `/etc/...` outside `src/xdg.lisp`.
- **Public symbols are exported from `:lispbar`.**  User
  extension files live in that package; if a symbol isn't
  exported they can't use it without `lispbar::` qualification.
- **Faces, not colours.**  Modules return `:face :urgent`, not
  hex strings.
- **Module return shapes are documented.**  If you invent a
  new fragment kind, teach `stdout.lisp` (text/JSON drivers)
  and `wayland.lisp` (renderer) about it in the same commit.
- **Errors in user code don't crash the bar.**  Anything
  invoking user-provided closures (`module-output`,
  `run-module-action`, `resolve-tooltip`) must `handler-case`
  and log at `:warn`.

## Tests

There's no formal test harness yet.  The integration path is to
spin up `sway -B headless`, drive it with `swaymsg`, render the
bar against it, screenshot via `grim`, and visually compare.
Tooling for that is in the test scripts inside `/tmp/` during
development sessions.

## Performance notes

* Cold start: ~110 ms on a modern x86-64.  Dominated by SBCL
  image load.
* Render tick: < 1 ms typical, dominated by Pango layout.  Cairo
  image creation is the next chunk.  Per-module update functions
  run at most once per their `:interval`, so e.g. CPU/memory only
  re-read /proc files every 5 / 10 seconds even though the bar
  repaints every second.
* Memory: ~30 MB RSS steady state with the default module set.
* D-Bus traffic: zero on idle; only when SNI signals fire.

## File-by-file what-changes-where

| Want to change…                       | File                              |
| ------------------------------------- | --------------------------------- |
| The default config users see          | `examples/config.lisp`            |
| Built-in module behaviour             | `src/modules/NAME.lisp`           |
| Built-in theme palette                | `src/theme.lisp`                  |
| Click dispatch logic                  | `src/module.lisp`                 |
| Wayland rendering / fragment kinds    | `src/output/wayland.lisp`         |
| Stdout / JSON drivers                 | `src/output/stdout.lisp`          |
| Wayland protocol plumbing             | `cshim/wlbar.c`                   |
| SNI tray plumbing                     | `cshim/wltray.c`                  |
| XDG path resolution                   | `src/xdg.lisp`                    |
| First-run seeding                     | `src/config.lisp`                 |
| CLI flags                             | `src/main.lisp`                   |
| Init system recipes                   | `init/{systemd,runit,openrc}/`    |
| Build pipeline                        | `Makefile`, `build.lisp`          |
