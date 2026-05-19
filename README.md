# lispbar (native)

A standalone Wayland status bar implemented in Common Lisp.

This is what the compositor spawns. It is to lispbar what waybar is
to its config file — a real binary with a configuration file at
`~/.config/lispbar/config.lisp`, not an Emacs plugin.

## Build

```sh
make build              # produces ./lispbar (12 MB, self-contained)
sudo make install       # see "Install" below
```

Requirements:

| Required                       | Purpose                                 |
| ------------------------------ | --------------------------------------- |
| SBCL ≥ 2.0 + Quicklisp (CFFI)  | builds the binary                       |
| libwayland-client              | talks to the compositor                 |
| wayland-protocols + scanner    | code-generates the layer-shell glue     |
| libcairo                       | pixel rendering                         |
| libpangocairo, libpango (opt.) | proper text shaping / font fallback     |

The binary is built once on your machine and runs anywhere that has
those shared libraries available — there's no Lisp dependency on the
target host because SBCL's runtime is embedded.

## Install

`make install` honours the standard `PREFIX` (default `/usr/local`)
and `DESTDIR` (default empty) variables.  Init-system integration
is auto-detected; override with `INIT=`:

```sh
sudo make install                       # auto-detect (systemd | runit | openrc)
sudo make install INIT=systemd
sudo make install INIT=runit
sudo make install INIT=openrc           # no init wiring; uses compositor exec
sudo make install PREFIX=/usr           # distribution-style prefix
make install PREFIX=$HOME/.local        # user-local prefix
```

What gets installed:

| File                                                                | Source                          |
| ------------------------------------------------------------------- | ------------------------------- |
| `$PREFIX/bin/lispbar`                                               | the binary                      |
| `$PREFIX/lib/lispbar/libwlbar.so`                                   | Wayland C shim                  |
| `$PREFIX/share/lispbar/examples/config.lisp`                        | starter config                  |
| `$PREFIX/share/lispbar/examples/modules/loadavg.lisp`               | example user module             |
| `$PREFIX/share/lispbar/examples/themes/dracula.lisp`                | example user theme              |
| `$PREFIX/share/lispbar/init/systemd/lispbar.service`                | systemd user unit (reference)   |
| `$PREFIX/share/lispbar/init/runit/lispbar/run`                      | runit service (reference)       |
| `$PREFIX/share/lispbar/init/openrc/README.md`                       | OpenRC integration notes        |
| `$PREFIX/share/doc/lispbar/{README.md,LICENSE}`                     | docs                            |
| `$PREFIX/lib/systemd/user/lispbar.service`                          | only when `INIT=systemd`        |

`make uninstall` removes every file `make install` placed.  User
config under `$XDG_CONFIG_HOME/lispbar/` is **not** touched.

### Init-system integration

Depending on your init system, choose one of:

| Init     | How Lispbar starts                                                              |
| -------- | ------------------------------------------------------------------------------- |
| systemd  | `systemctl --user enable --now lispbar`                                         |
| runit    | `ln -s $PREFIX/share/lispbar/init/runit/lispbar ~/runit/service/`               |
| OpenRC   | `exec lispbar` in `~/.config/sway/config` (or `exec-once = lispbar` for Hyprland) |
| anything | `exec lispbar` in your compositor config always works                           |

OpenRC users: see `$PREFIX/share/lispbar/init/openrc/README.md`
for the rationale.  TL;DR — OpenRC is a system-level supervisor;
graphical user-session bars are started by the compositor on every
init system, OpenRC included.

## Use

```sh
./lispbar                       # streams forever, picks driver from config
./lispbar --once                # one frame to stdout (handy for scripts)
./lispbar --output json         # waybar-compatible JSON per tick
./lispbar --init                # seed ~/.config/lispbar/ explicitly
./lispbar --list-modules        # registry inventory
./lispbar --list-themes         # theme registry
./lispbar --print-paths         # XDG path resolution
./lispbar --show-extensions     # which user files got loaded
./lispbar --help
```

### First run

On the first launch, if no config exists anywhere in the XDG search
path, Lispbar creates `$XDG_CONFIG_HOME/lispbar/`, drops a starter
`config.lisp` (the same one in `examples/config.lisp`), and creates
empty `modules/` and `themes/` directories so you know where to put
your own extensions.  The bar then boots from that fresh config -
no manual `cp` required.

To turn this off (e.g. on a server / staged image where you'll ship
the config separately), pass `--no-seed` or set
`LISPBAR_NO_SEED=1` in the environment.  To re-seed an existing
config explicitly, `lispbar --init --force`.

### Configuration

`$XDG_CONFIG_HOME/lispbar/config.lisp` (defaulting to
`~/.config/lispbar/config.lisp`) is a Lisp file evaluated by the
binary on startup.  Every form is one of:

```lisp
(placement :left   (:workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery :clock))

(position      :top)            ; :top | :bottom
(height        28)              ; bar height in pixels
(margin        8 12 0 12)       ; CSS-style: top right bottom left
(padding       12)              ; inside the bar, left + right edges
(gap           12)              ; horizontal space between modules
(corner-radius 12)              ; rounded background; 0 = sharp
(font          "Sans Bold 11")  ; Pango font description
(theme         :nordish)        ; or any registered theme
(tick          1.0)             ; refresh interval in seconds
(output        :wayland)        ; :wayland :stdout :json
(log-level     :info)           ; :debug :info :warn :error
```

Forms are evaluated top-to-bottom; later forms override earlier ones.
The config file may also contain ordinary Lisp forms - `defmodule`,
`define-theme`, `setf` - so you can keep your whole setup in one
place if you prefer.

### XDG search path

Lispbar is fully XDG-compliant.  The binary searches, in order:

| What         | Path                                                                  |
| ------------ | --------------------------------------------------------------------- |
| Config file  | `$XDG_CONFIG_HOME/lispbar/config.lisp`                                |
| Config dirs  | every `$XDG_CONFIG_DIRS/.../lispbar/config.lisp`                      |
| User modules | `$XDG_CONFIG_HOME/lispbar/modules/*.lisp`                             |
| User themes  | `$XDG_CONFIG_HOME/lispbar/themes/*.lisp`                              |
| System dirs  | every `$XDG_CONFIG_DIRS/.../lispbar/{modules,themes}/*.lisp`          |
| Data dirs    | `$XDG_DATA_HOME/lispbar/{modules,themes}/*.lisp`, then `$XDG_DATA_DIRS` |
| State        | `$XDG_STATE_HOME/lispbar/` (for module bookkeeping)                   |
| Cache        | `$XDG_CACHE_HOME/lispbar/` (for module caches)                        |

Inspect the live resolution with `lispbar --print-paths` and
`lispbar --show-extensions`.

## Writing your own module

Drop a file under `$XDG_CONFIG_HOME/lispbar/modules/`:

```lisp
;; ~/.config/lispbar/modules/loadavg.lisp
(in-package :lispbar)

(defmodule :loadavg
  (:doc "5- and 15-minute load average."
   :position :right :priority 58 :interval 5.0)
  (when (probe-file "/proc/loadavg")
    (with-open-file (s "/proc/loadavg" :direction :input)
      (let* ((line (read-line s nil ""))
             (parts (uiop:split-string line :separator '(#\Space))))
        (when (>= (length parts) 3)
          (list :text (format nil "LOAD ~a ~a"
                              (second parts) (third parts))
                :face :muted))))))
```

Then reference `:loadavg` in `config.lisp`:

```lisp
(placement :right (:loadavg :cpu :memory :battery :clock))
```

Restart lispbar; the new module appears.  No rebuild needed - user
extensions are loaded as source `.lisp` files at startup.

### Module API

`defmodule NAME (&key doc position priority interval) BODY...`
registers a factory.  Every tick the body is evaluated; its value
becomes the module's display content.  The return value is one of:

| Return                                  | Meaning                                |
| --------------------------------------- | -------------------------------------- |
| `NIL` or `""`                           | Module hidden this tick.               |
| `"a string"`                            | One fragment, `:normal` face.          |
| `(:text "X" :face :urgent)`             | One fragment, custom face.             |
| `(:fragments (("X" :muted) ("2" :accent)))` | Multiple fragments with per-fragment faces. |

Available faces: `:normal :accent :ok :warn :urgent :muted`
(`:bg` is the bar background).

Helpers exported for module authors:

| Symbol                       | What                                                    |
| ---------------------------- | ------------------------------------------------------- |
| `run-capture PROGRAM &rest`  | Run a process, return stdout string on exit 0, NIL else |
| `logmsg LEVEL FMT &rest`     | Structured stderr log                                   |
| `lispbar-state-directory`    | `$XDG_STATE_HOME/lispbar/`, created on first use         |
| `lispbar-cache-directory`    | `$XDG_CACHE_HOME/lispbar/`, created on first use         |

## Writing your own theme

Drop a file under `$XDG_CONFIG_HOME/lispbar/themes/`:

```lisp
;; ~/.config/lispbar/themes/dracula.lisp
(in-package :lispbar)

(define-theme :dracula
  :bg     '(0.157 0.165 0.212 1.0)
  :normal '(0.945 0.945 0.945 1.0)
  :accent '(0.741 0.576 0.976 1.0)
  :ok     '(0.314 0.980 0.482 1.0)
  :warn   '(0.945 0.980 0.549 1.0)
  :urgent '(1.000 0.333 0.333 1.0)
  :muted  '(0.380 0.420 0.490 1.0))
```

Reference it from `config.lisp`:

```lisp
(theme :dracula)
```

Colours are `(R G B A)` doubles in 0.0-1.0.  Every face listed in
the `Module API` section is recognised; omitted faces fall back to
the active theme's `:normal`.

Built-in themes you can copy as a starting point:
`:default`, `:minimal`, `:nordish`, `:gruvboxish`, `:catppuccinish`,
`:doomish`.

## Built-in modules

| Name           | Source                       |
| -------------- | ---------------------------- |
| `:clock`       | local time                   |
| `:workspaces`  | `swaymsg` / `hyprctl` IPC    |
| `:media`       | `playerctl` (any MPRIS2)     |
| `:cpu`         | `/proc/loadavg`              |
| `:memory`      | `/proc/meminfo`              |
| `:battery`     | `/sys/class/power_supply/`   |
| `:audio`       | `wpctl` → `pactl` → `amixer` |
| `:bluetooth`   | `bluetoothctl`               |
| `:brightness`  | `brightnessctl` → sysfs      |

`lispbar --list-modules` prints the live inventory (built-ins plus
anything discovered under XDG).

## Output drivers

| Output     | What it does                                       |
| ---------- | -------------------------------------------------- |
| `:wayland` | Native `wlr-layer-shell` client, one surface per monitor, cairo + Pango rendering. |
| `:stdout`  | Plain text, one bar line per tick.                 |
| `:json`    | Waybar-compatible JSON object per tick (drop-in custom-module driver). |

The JSON driver is what made early development bootable: until the
Wayland path landed, lispbar plugged into waybar via:

```jsonc
// ~/.config/waybar/config
"custom/lispbar": {
  "exec": "lispbar --output json",
  "return-type": "json"
}
```

The `:wayland` driver makes that intermediate step unnecessary.

## How the Wayland driver works

* `cshim/wlbar.c` is a ~300-line C glue layer.  It hides the parts
  that *must* be C — the generated wlr-layer-shell marshalling
  tables, binding registry globals, allocating wl_shm ARGB32 buffers,
  pumping `wl_display_dispatch` — behind a flat ~15-symbol API.
* `src/output/wayland.lisp` FFIs into `libwlbar.so` and into a tight
  set of cairo + pango functions.  It iterates `wlbar_output_count()`
  each tick, paints each output's buffer, commits.
* Everything user-facing — modules, config, theming, layout — stays
  in Common Lisp.

That separation is why the binary stays "Lispy" without re-implementing
the Wayland wire format by hand.

## Layout

```
.
├── lispbar.asd                ASDF system (depends on cffi)
├── build.lisp                 sb-ext:save-lisp-and-die driver
├── Makefile                   make build / install / test
├── systemd/lispbar.service    user unit
├── protocols/
│   └── wlr-layer-shell-unstable-v1.xml
├── cshim/
│   ├── wlbar.c                C glue (uses generated marshalling)
│   ├── wlbar.h                ~15-symbol FFI surface
│   └── Makefile               builds libwlbar.so
├── src/
│   ├── package.lisp           Common Lisp package
│   ├── log.lisp               stderr logger
│   ├── xdg.lisp               XDG Base Directory helpers
│   ├── theme.lisp             theme registry + define-theme + built-ins
│   ├── config.lisp            config DSL loader + extension discovery
│   ├── module.lisp            defmodule macro + registry + faces
│   ├── modules/               clock, workspaces, media, cpu, memory,
│   │                          battery, audio, bluetooth, brightness
│   ├── output/
│   │   ├── stdout.lisp        text + JSON drivers
│   │   └── wayland.lisp       layer-shell driver
│   └── main.lisp              entry, arg parsing, signal handlers
└── examples/
    ├── config.lisp
    ├── modules/loadavg.lisp
    └── themes/dracula.lisp
```

## License

GPL-3.0-or-later.
