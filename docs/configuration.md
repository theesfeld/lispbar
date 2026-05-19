# Configuration reference

The full configuration lives at
`$XDG_CONFIG_HOME/lispbar/config.lisp` (defaulting to
`~/.config/lispbar/config.lisp`).  Every form is evaluated in the
`:lispbar` Common Lisp package, top to bottom; later forms override
earlier ones.

```sh
lispbar --print-paths        # see which paths are searched
lispbar --show-extensions    # see which user files actually loaded
```

The file is split into eight sections matching the seeded default;
this document lists every keyword, every defvar, and every
shipped default.

## Section 1 — Modules and placement

Three lists decide which modules appear and where:

```lisp
(placement :left   (:launcher :workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :network :audio :bluetooth
                    :brightness :battery :tray :clock))
```

Within each section modules are ordered by **priority descending**
(higher priority lands further left).  The list itself is just a
membership set; reordering it doesn't change visual order.

Run `lispbar --list-modules` for the live inventory (built-ins plus
anything you drop into `~/.config/lispbar/modules/`).

## Section 2 — Geometry

| Keyword           | Type            | Default | Notes                                                  |
| ----------------- | --------------- | ------- | ------------------------------------------------------ |
| `(position …)`    | `:top` / `:bottom` | `:top` | Edge the bar anchors to                              |
| `(height N)`      | integer pixels  | 28      | Inner height; margins live outside this              |
| `(margin …)`      | 1–4 ints        | 0       | CSS-style: `top right bottom left` (see below)       |
| `(padding N)`     | integer pixels  | 12      | Inside the bar, before first/after last module       |
| `(gap N)`         | integer pixels  | 12      | Horizontal space between adjacent modules            |
| `(corner-radius N)` | integer pixels | 0       | Round the bar background; 0 = sharp                  |

`margin` syntax:

```lisp
(margin 8)               ; 8 px on every side
(margin 8 16)            ; vertical / horizontal
(margin 8 16 0)          ; top / horizontal / bottom
(margin 8 16 4 16)       ; top / right / bottom / left
```

Non-zero margin gives a "floating" bar; pair with a non-zero
`corner-radius` for the rounded-pill look.

## Section 3 — Typography

```lisp
(font "Sans 11")
```

Anything Pango can parse:

```
"Monospace 11"             "Sans 11"
"Sans Bold 11"             "FiraCode Nerd Font 11"
"Inter 11"                 "JetBrains Mono Medium 10"
```

Without Pango (libpango not installed), the binary falls back to
cairo's "toy text" API and uses the first word as the family,
ignoring weight/size keywords.

## Section 4 — Colour theme

```lisp
(theme :catppuccinish)
```

Built-in theme names:

```
:default     :minimal     :nordish     :gruvboxish
:catppuccinish    :doomish
```

User themes live in `~/.config/lispbar/themes/*.lisp` and are
auto-loaded.  See [themes.md](themes.md).

## Section 5 — Runtime behaviour

| Keyword       | Type      | Default    | Notes                                |
| ------------- | --------- | ---------- | ------------------------------------ |
| `(tick S)`    | seconds   | 1.0        | Main loop wake interval              |
| `(output K)`  | keyword   | `:stdout`  | `:wayland` / `:stdout` / `:json`     |
| `(log-level L)`| keyword | `:info`    | `:debug` `:info` `:warn` `:error`    |

Output backends:

| Backend     | What it does                                                         |
| ----------- | -------------------------------------------------------------------- |
| `:wayland`  | Native wlr-layer-shell client; the real bar.  Default after seeding. |
| `:stdout`   | One line per tick to stdout (testing, piping).                       |
| `:json`     | One waybar-compatible JSON object per tick (custom-module driver).   |

## Section 6 — Per-module options

Each module exposes ordinary defvars; set them with `setf` anywhere
in `config.lisp`.  The full list:

### `:audio`

```lisp
(setf *audio-on-click*         "pavucontrol || pwvucontrol")
(setf *audio-on-middle-click*  "pactl set-sink-mute @DEFAULT_SINK@ toggle")
(setf *audio-scroll-step*      5)
```

### `:battery`

No tunables.  Faces are picked from charge level/state:

* `>= 25 %` charging  → `:ok`
* `<  10 %`           → `:urgent`
* `<  25 %`           → `:warn`
* otherwise           → `:normal`

### `:bluetooth`

```lisp
(setf *bluetooth-on-click*         "blueman-manager || blueberry")
(setf *bluetooth-on-middle-click*  "bluetoothctl power toggle")
```

### `:brightness`

```lisp
(setf *brightness-step*       5)     ; percentage points per scroll tick
(setf *brightness-on-click*   nil)   ; optional left-click command
```

### `:clock`

```lisp
(setf *clock-format* :hh-mm-ss)      ; or :hh-mm  :iso8601  <function>
```

A function value can do anything `format-time-string`-ish:

```lisp
(setf *clock-format*
      (lambda ()
        (multiple-value-bind (s m h day mo yr dow) (get-decoded-time)
          (declare (ignore s))
          (format nil "~a ~d/~d ~2,'0d:~2,'0d"
                  (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                  mo day h m))))
```

### `:launcher`

```lisp
(setf *launcher-command*         "wofi --show drun || fuzzel || rofi -show drun")
(setf *launcher-on-right-click*  "wlogout || swaynag -m 'logout?'")
(setf *launcher-label*           " ")
```

### `:media`

```lisp
(setf *media-format*       :artist-title)   ; :title-only  :short
(setf *media-max-length*   60)
```

### `:network`

```lisp
(setf *network-format-wifi*       "{ssid} {signal}%")
(setf *network-format-ethernet*   "ETH")
(setf *network-format-down*       "OFF")
(setf *network-on-click*          "nm-connection-editor || iwgtk")
(setf *network-on-middle-click*   "nmcli radio wifi")
```

WiFi placeholders: `{ssid} {signal} {bars} {device}`.

### `:tray`

```lisp
(setf *tray-icon-size*               16)
(setf *tray-show-text-when-no-icon*  t)
```

### `:workspaces`

```lisp
(setf *workspaces-scope*       :current-output)   ; :all  :focused
(setf *workspaces-brackets*    '("[" . "]"))
(setf *workspaces-separator*   " ")
(setf *workspaces-empty-text*  nil)               ; or "—"
```

Scope semantics:

| Value             | What appears                                              |
| ----------------- | --------------------------------------------------------- |
| `:current-output` | Workspaces belonging to the monitor with focus.  Default. |
| `:all`            | Every workspace across every monitor.                     |
| `:focused`        | Only the workspace that currently has focus.              |

## Section 7 — Hover tooltips

Tooltips render on a **separate floating wlr-layer-shell surface**
anchored just below the bar.  They can extend beyond the bar
height; you don't have to make the bar tall to accommodate them.

```lisp
(setf *wayland-tooltip-bg*         '(0.10 0.12 0.18 0.92))  ; RGBA 0.0-1.0
(setf *wayland-tooltip-padding-x*  10.0d0)
(setf *wayland-tooltip-padding-y*  6.0d0)
(setf *wayland-tooltip-corner*     8.0d0)
```

Modules opt in by declaring `:tooltip` (see [modules.md](modules.md#tooltips)).

## Section 8 — Click handler reference

`defmodule :on-click` is a plist mapping button keywords to handlers:

```
:left   :right   :middle   :side   :extra
:scroll-up   :scroll-down
```

Handler shapes:

| Form              | What it does                                      |
| ----------------- | ------------------------------------------------- |
| `NIL`             | no-op                                             |
| `"string"`        | `sh -c "string"`                                  |
| `("argv" ...)`    | run argv directly via `uiop:launch-program`       |
| `SYMBOL`          | resolved via `symbol-function`; called with `(MODULE BUTTON OUTPUT-IDX)` |
| `#'function`      | same arity as above                               |

Per-fragment click handlers — see [modules.md](modules.md#per-fragment-click-routing).

## CLI override flags

| Flag            | Effect                                              |
| --------------- | --------------------------------------------------- |
| `-c / --config FILE` | Use FILE instead of the XDG-discovered config |
| `-o / --output K`    | Override `(output …)` for one invocation       |
| `--once`             | Render one frame and exit                       |
| `--init [--force]`   | Seed user config explicitly                     |
| `--no-seed`          | Skip first-run seeding                          |
| `--list-modules`     | Inventory and exit                              |
| `--list-themes`      | Theme inventory and exit                        |
| `--show-extensions`  | What user .lisp files got loaded                |
| `--print-paths`      | XDG path resolution                             |
| `-v / --verbose`     | `:debug` log level                              |

## Environment variables

| Variable               | Effect                                                  |
| ---------------------- | ------------------------------------------------------- |
| `XDG_CONFIG_HOME`      | Where `lispbar/config.lisp` is read from               |
| `XDG_CONFIG_DIRS`      | Fallback config dirs (system-wide configs)              |
| `XDG_DATA_HOME` / `_DIRS` | Extra module/theme search paths                     |
| `XDG_STATE_HOME`       | Where modules can write persistent state                |
| `XDG_CACHE_HOME`       | Tray icon cache (also `lispbar/icons/` under it)        |
| `LISPBAR_NO_SEED`      | Set to anything non-empty to skip first-run seeding     |
| `WAYLAND_DISPLAY`      | Standard Wayland socket name (used by libwayland)       |

## Reload after changes

Lispbar reads config only at startup.  Restart it to apply edits:

```sh
systemctl --user restart lispbar              # systemd
sv restart lispbar                            # runit
pkill -USR2 lispbar; sleep 0.2; exec lispbar  # any compositor
```

(`SIGUSR2`-based reload isn't wired yet; just restart for now.)
