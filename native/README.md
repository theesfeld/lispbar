# lispbar (native)

A standalone status-bar binary written in Common Lisp.

The Elisp implementation that lives one directory up is an Emacs minor
mode and requires Emacs as a runtime. This native build is what you
spawn from your compositor config — like waybar — except every line
of it is Lisp and its config is `~/.config/lispbar/config.lisp`,
itself an s-expression.

## Build

```sh
make build          # produces ./lispbar (10 MB, self-contained)
make install        # to /usr/local/bin/lispbar
```

Requires SBCL ≥ 2.0. `sb-ext:save-lisp-and-die` produces the binary;
the SBCL runtime is embedded, no separate Lisp install needed on the
target machine.

## Use

```sh
# One-shot render, plain text
./lispbar --once

# One-shot, JSON (drop-in waybar custom-module driver)
./lispbar --once --output json

# Streaming (re-renders every tick)
./lispbar

# List the available modules
./lispbar --list-modules
```

### Config

`~/.config/lispbar/config.lisp` — see `examples/config.lisp`:

```lisp
(placement :left   (:clock))
(placement :center ())
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery))
(tick     1.0)
(output   :stdout)        ;; :stdout | :json | :wayland (planned)
(log-level :info)
```

Forms are evaluated top-to-bottom; later forms override earlier ones.

## Modules

| Name         | Source                     |
| ------------ | -------------------------- |
| `:clock`     | local time                 |
| `:cpu`       | `/proc/loadavg`            |
| `:memory`    | `/proc/meminfo`            |
| `:battery`   | `/sys/class/power_supply/` |
| `:audio`     | `wpctl` → `pactl`          |
| `:bluetooth` | `bluetoothctl`             |
| `:brightness`| `brightnessctl` → sysfs    |

Add your own with the `defmodule` macro:

```lisp
(defmodule :weather (:doc "OpenWeatherMap"
                     :position :right :priority 40 :interval 600.0)
  (lispbar:run-capture "curl" "-s" "wttr.in/?format=3"))
```

Drop the file into the `src/modules/` directory and rebuild.

## Output drivers

| Output     | What it does                                       | Status   |
| ---------- | -------------------------------------------------- | -------- |
| `:stdout`  | One bar line per tick to stdout                    | shipping |
| `:json`    | One waybar-compatible JSON object per tick         | shipping |
| `:wayland` | Native `wlr-layer-shell` client (cairo)            | shipping |

The JSON driver lets the binary serve as a custom module for
**waybar**, **eww**, or **yambar** today:

```jsonc
// ~/.config/waybar/config
"custom/lispbar": {
  "exec": "lispbar --output json",
  "return-type": "json",
  "interval": "once"
}
```

## How the Wayland driver works

* `cshim/wlbar.c` is a small (~200 line) C glue that handles the parts
  that *must* be C: the generated wlr-layer-shell marshalling tables,
  binding the registry globals, allocating a `wl_shm` ARGB32 buffer,
  and pumping `wl_display_dispatch`.  It exposes a flat ~10-symbol
  API to the rest of the program.
* `src/output/wayland.lisp` FFIs into `libwlbar.so` plus `libcairo.so.2`
  (just 10 functions worth of cairo), gets the raw pixel pointer, and
  paints the bar with cairo on every tick.
* Everything user-facing — modules, config, theming, layout — stays in
  Common Lisp.

That separation is why the binary stays "Lispy" without re-implementing
the Wayland protocol wire format by hand.

## Layout

```
native/
├── lispbar.asd                ASDF system (depends on cffi)
├── build.lisp                 sb-ext:save-lisp-and-die driver
├── Makefile                   make build / install / test
├── protocols/
│   └── wlr-layer-shell-unstable-v1.xml
├── cshim/
│   ├── wlbar.c                C glue (uses generated marshalling)
│   ├── wlbar.h                10-symbol FFI surface
│   └── Makefile               builds libwlbar.so
├── src/
│   ├── package.lisp           Common Lisp package
│   ├── log.lisp               stderr logger
│   ├── config.lisp            config DSL loader
│   ├── module.lisp            defmodule macro + registry
│   ├── modules/               clock, cpu, memory, battery, audio,
│   │                          bluetooth, brightness
│   ├── output/
│   │   ├── stdout.lisp        text + JSON drivers
│   │   └── wayland.lisp       layer-shell driver (libwlbar + cairo)
│   └── main.lisp              entry, arg parsing, signal handlers
└── examples/config.lisp
```

## License

GPL-3.0-or-later.
