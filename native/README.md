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
| `:wayland` | Native layer-shell client (cairo + pango via FFI)  | TODO     |

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

## Roadmap to a true standalone bar

`output: :wayland` needs three pieces, none of which exists in CL yet:

1. **FFI to `libwayland-client`.** Generate CFFI bindings against
   `wayland-client.h`; auto-grovel the protocol with `wayland-scanner`
   driven from CL.
2. **`wlr-layer-shell-unstable-v1`.** Codegen from
   `/usr/share/wayland-protocols/.../wlr-layer-shell-unstable-v1.xml`.
   Anchor a surface to the top edge with an exclusive zone equal to
   the bar height.
3. **Cairo + Pango rendering.** Draw text to a wl_shm buffer; damage
   the surface on each module update.

When those land, `(output :wayland)` makes the binary the bar.

## Layout

```
native/
├── lispbar.asd           ASDF system
├── build.lisp            sb-ext:save-lisp-and-die driver
├── Makefile              make build / install / test
├── src/
│   ├── package.lisp      Common Lisp package
│   ├── log.lisp          stderr logger
│   ├── config.lisp       config DSL loader
│   ├── module.lisp       defmodule macro + registry
│   ├── modules/          clock, cpu, memory, battery, audio, bluetooth, brightness
│   ├── output/
│   │   ├── stdout.lisp   text + JSON drivers
│   │   └── wayland-stub.lisp     placeholder until FFI lands
│   └── main.lisp         entry, arg parsing, signal handlers
└── examples/config.lisp
```

## License

GPL-3.0-or-later.
