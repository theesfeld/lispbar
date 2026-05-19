# lispbar (native)

A standalone Wayland status bar implemented in Common Lisp.

This is what the compositor spawns. It is to lispbar what waybar is
to its config file — a real binary with a configuration file at
`~/.config/lispbar/config.lisp`, not an Emacs plugin.

## Build

```sh
make build              # produces ./lispbar (12 MB, self-contained)
sudo make install       # /usr/local/{bin,lib,share}/lispbar/...
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

## Use

```sh
./lispbar                      # streams forever, picks driver from config
./lispbar --once               # one frame to stdout (handy for scripts)
./lispbar --output json        # waybar-compatible JSON per tick
./lispbar --list-modules       # registry inventory
./lispbar --help
```

Add `lispbar` to your compositor startup (Sway `exec`, Hyprland
`exec-once`, or `systemctl --user enable --now lispbar.service`).
The bar is anchored to the top edge of every output via
`wlr-layer-shell` — no window-rule snippets, no struts: it Just Works.

### Configuration

`~/.config/lispbar/config.lisp` is a Lisp file evaluated by the
binary on startup.  Every form is one of:

```lisp
(placement :left   (:workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery :clock))

(theme    :nordish)             ; or :gruvboxish :catppuccinish :doomish :minimal
(font     "Sans Bold 11")       ; Pango font description
(height   28)                   ; bar height in pixels
(tick     1.0)                  ; refresh interval in seconds
(output   :wayland)             ; :wayland :stdout :json
(log-level :info)               ; :debug :info :warn :error
```

Forms are evaluated top-to-bottom; later forms override earlier ones.

## Modules

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

Add your own:

```lisp
(defmodule :weather (:doc "Open-Meteo current conditions"
                     :position :right :priority 35 :interval 600.0)
  (run-capture "curl" "-s" "wttr.in/?format=3"))
```

Drop the file in `src/modules/`, add it to `lispbar.asd`, rebuild.

### Module faces

A module's update function returns one of:

```lisp
"plain text"                              ; → :normal face
(:text "BAT -5%" :face :urgent)           ; one fragment, urgent
(:fragments (("[" :muted)                 ; many fragments, each
             ("2" :accent)                ; with its own face
             ("]" :muted)))
```

Available faces: `:normal :accent :ok :warn :urgent :muted` (plus
`:bg` for the bar background).  Every shipped theme defines the full
palette, so themes work uniformly across modules.

## Themes

Built-in: `:adaptive` (default), `:minimal`, `:nordish`,
`:gruvboxish`, `:catppuccinish`, `:doomish`.

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
native/
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
│   ├── config.lisp            config DSL loader
│   ├── module.lisp            defmodule macro + registry + faces
│   ├── modules/               clock, workspaces, media, cpu, memory,
│   │                          battery, audio, bluetooth, brightness
│   ├── output/
│   │   ├── stdout.lisp        text + JSON drivers
│   │   └── wayland.lisp       layer-shell driver
│   └── main.lisp              entry, arg parsing, signal handlers
└── examples/config.lisp
```

## License

GPL-3.0-or-later.
