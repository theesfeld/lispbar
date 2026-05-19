# Modules

Two parts:

1. **[Built-in module reference](#built-in-modules)** — every shipped module, its data source, and its default click bindings.
2. **[Writing your own module](#writing-your-own-module)** — the whole API.

---

## Built-in modules

`lispbar --list-modules` prints the live inventory.  Defaults below.

| Name          | Data source                       | Default position | Priority | Interval |
| ------------- | --------------------------------- | ---------------- | -------- | -------- |
| `:audio`      | `wpctl` → `pactl` → `amixer`      | right            | 65       | 2.0 s    |
| `:battery`    | `/sys/class/power_supply/`        | right            | 70       | 30.0 s   |
| `:bluetooth`  | `bluetoothctl`                    | right            | 50       | 10.0 s   |
| `:brightness` | `brightnessctl` → sysfs           | right            | 45       | 10.0 s   |
| `:clock`      | local time                        | right            | 90       | 1.0 s    |
| `:cpu`        | `/proc/loadavg`                   | right            | 60       | 5.0 s    |
| `:launcher`   | (clickable button)                | left             | 99       | 60.0 s   |
| `:media`      | `playerctl` (any MPRIS2)          | center           | 40       | 3.0 s    |
| `:memory`     | `/proc/meminfo`                   | right            | 55       | 10.0 s   |
| `:network`    | `nmcli` → `/proc/net/wireless` → `ip` | right        | 75       | 5.0 s    |
| `:registry`   | (clickable button → fzf picker)   | right            | 5        | 3600.0 s |
| `:tray`       | StatusNotifierItem (D-Bus)        | right            | 30       | 1.0 s    |
| `:workspaces` | `swaymsg` / `hyprctl` / `niri msg`| left             | 80       | 0.5 s    |

### Per-module behaviour

#### `:audio`

* Output: `VOL 65%` or `MUTE` when muted.
* Face: `:muted` when muted, `:warn` if volume > 100 %, else `:normal`.
* Left-click: open mixer GUI.  Override `*audio-on-click*`.
* Middle-click: toggle mute.  Override `*audio-on-middle-click*`.
* Scroll up/down: ±5 % volume.  Override `*audio-scroll-step*`.
* Tooltip: full volume / mute state with click-hint.

#### `:battery`

* Output: `BAT +85%` (charging), `BAT -22%` (low + discharging), `BAT =100%` (full), `BAT ?` (unknown).
* Face: `:ok` charging, `:urgent` < 10 %, `:warn` < 25 %, `:muted` full, `:normal` otherwise.
* No click bindings shipped.

#### `:bluetooth`

* Output: `BT on/2` (powered, 2 devices), `BT on` (powered, 0 devices), `BT off`.
* Face: `:ok` with connections, `:muted` off, `:normal` on.
* Left-click: blueman-manager / blueberry.
* Middle-click: toggle adapter power.
* Tooltip: human-readable state.

#### `:brightness`

* Output: `BRT 75%`.
* Scroll: ±5 % via `brightnessctl`.
* Left-click: nothing by default (`*brightness-on-click*` is NIL).

#### `:clock`

* Output: time-of-day, format set by `*clock-format*`.
* Face: `:accent`.
* Tooltip: `Mon 2026-05-19`.

#### `:cpu`

* Output: `CPU 0.42` (1-minute load).
* Face: `:warn` when load exceeds `*cpu-high-threshold*` (defaults to number of logical CPUs), else `:normal`.

#### `:launcher`

* Output: configurable label (default `≡`).
* Face: `:accent`.
* Left-click: spawn launcher (`*launcher-command*`).
* Right-click: logout (`*launcher-on-right-click*`).

#### `:media`

* Output: `▶ Artist - Title` while playing, `❚❚ Artist - Title` while paused.
* Face: `:accent`.
* Truncated to `*media-max-length*` chars.

#### `:memory`

* Output: `MEM 42%`.
* Face: `:warn` ≥ 85 % used, else `:normal`.

#### `:network`

* Output: `MyWiFi 65%` (WiFi), `ETH` (ethernet), `OFF` (down), or VPN/bridge connection name.
* Face: `:ok` strong signal, `:warn` medium, `:urgent` weak.
* Left-click: NM/iwd GUI.
* Middle-click: `nmcli radio wifi` (default), override to toggle/etc.
* Tooltip: SSID + signal + device.

#### `:registry`

* Output: a single glyph (default `📦`).  Set `*registry-label*` to
  override or `NIL` to hide.
* Left-click: spawns a terminal running `lispbar registry browse`
  (the fzf-driven picker — see [registry.md](registry.md)).
* The terminal is resolved by `*registry-terminal*` — a
  space-separated preference list (default: `$TERMINAL footclient
  foot alacritty kitty wezterm gnome-terminal konsole xterm`).
  The first entry found on `PATH` wins.  `footclient` comes
  before `foot` so users running a foot server get the snappier
  client variant.
* For full control, set `*registry-on-click*` to your own shell
  command — that takes precedence over the built-in terminal-spawn.

#### `:tray`

* Output: one icon per registered SNI item (inline pixmap > icon-theme PNG > text fallback).
* Click: left = Activate, middle = SecondaryActivate, right = ContextMenu (all routed via D-Bus to the right item).
* Updates on D-Bus signals (`NewTitle`, `NewIcon`, `NewToolTip`, ...).

#### `:workspaces`

* Output: list of workspaces, focused one bracketed in `:accent`.
* Scope filter via `*workspaces-scope*` (`:current-output` default).
* Left-click on a workspace number → switch to it (per-number routing).
* Scroll: prev / next workspace.
* Supports Sway, Hyprland, niri (auto-detected).

---

## Writing your own module

Drop a file into `~/.config/lispbar/modules/`.  It's loaded on
startup with `*package*` already set to `:lispbar`.

### Minimal template

```lisp
;; ~/.config/lispbar/modules/loadavg.lisp
(in-package :lispbar)

(defmodule :loadavg
  (:doc "5- and 15-minute load average."
   :position :right :priority 58 :interval 5.0)
  (when (probe-file "/proc/loadavg")
    (with-open-file (s "/proc/loadavg")
      (let* ((line (read-line s nil ""))
             (parts (uiop:split-string line :separator '(#\Space))))
        (when (>= (length parts) 3)
          (list :text (format nil "LOAD ~a ~a"
                              (second parts) (third parts))
                :face :muted))))))
```

Add `:loadavg` to one of your placement lists and restart.

### The `defmodule` macro

```
(defmodule NAME (&key doc position priority interval on-click tooltip)
  BODY ...)
```

| Keyword     | Type              | Default     | Notes                                              |
| ----------- | ----------------- | ----------- | -------------------------------------------------- |
| `:doc`      | string            | `""`        | Shown by `lispbar --list-modules`.                |
| `:position` | `:left`/`:center`/`:right` | `:right` | Default section for `(placement …)`.        |
| `:priority` | integer 0-100     | 50          | Higher = further left within a section.            |
| `:interval` | seconds           | 5.0         | How often `BODY` re-runs.  Cached between ticks.   |
| `:on-click` | plist (see below) | nil         | Button → action mapping.                           |
| `:tooltip`  | string / fn / symbol | nil      | Floating tooltip on hover.                         |

`BODY` is the update function body; it runs at most once per
`interval` seconds and must return one of:

| Return                                  | Meaning                                       |
| --------------------------------------- | --------------------------------------------- |
| `NIL` or `""`                           | Module is hidden this tick.                  |
| `"a string"`                            | One fragment, `:normal` face.                |
| `(:text "X" :face :urgent)`             | One fragment, custom face.                   |
| `(:fragments ((TEXT FACE) ...))`        | Multiple fragments, each with its own face.  |

### Faces

The renderer recognises seven semantic faces — themes map them to
concrete colours:

| Face       | Use for                                                     |
| ---------- | ----------------------------------------------------------- |
| `:bg`      | bar background (never use in a module return)              |
| `:normal`  | the default                                                 |
| `:accent`  | emphasis (clock, focused workspace, …)                      |
| `:ok`      | positive states (charging, connected, …)                    |
| `:warn`    | low / cautionary states                                     |
| `:urgent`  | critical states                                             |
| `:muted`   | dim, secondary text (separators, "off", …)                  |

### Click handlers

`:on-click` is a plist mapping button keywords to handler values.

```lisp
(defmodule :weather
  (:doc "Open-Meteo"
   :position :right :priority 35 :interval 600.0
   :on-click ((:left        "xdg-open https://wttr.in")          ; shell
              (:right       my-weather-fn)                       ; symbol
              (:middle      (list "notify-send" "Hi"))           ; argv
              (:scroll-up   #'my-handler-fn)))                   ; #'function
  ...)
```

Button keys: `:left :right :middle :side :extra :scroll-up
:scroll-down`.

Handler shapes:

| Form              | Effect                                                 |
| ----------------- | ------------------------------------------------------ |
| `NIL`             | no-op                                                  |
| `"string"`        | run via `sh -c`                                         |
| `("argv" …)`      | run via `uiop:launch-program`                          |
| `SYMBOL`          | resolved via `symbol-function`; called with `(MODULE BUTTON OUTPUT-IDX)` |
| `FUNCTION`        | same arity                                              |

### Per-fragment click routing

A module returning `:fragments` can mark individual fragments as
clickable.  This is what makes individual workspace numbers
independently clickable.

```lisp
(in-package :lispbar)

(defun outputs-click (output-name button m i)
  (declare (ignore button m i))
  (uiop:launch-program (list "swaymsg" "focus" "output" output-name)))

(defmodule :outputs
  (:doc "Click an output name to focus it"
   :position :right :priority 20 :interval 30.0)
  (let* ((j (run-capture "swaymsg" "-t" "get_outputs" "-r")))
    (when j
      (let ((names (loop for o in (split-json-objects j)
                         collect (json-string-value o "name"))))
        (list :fragments
              (loop for first = t then nil
                    for n in names
                    unless first
                      collect (list "  " :muted)
                    collect (list :clickable
                                  :text n
                                  :face :accent
                                  :on-click 'outputs-click
                                  :data n)))))))
```

The renderer paints `:clickable` like text but records its painted
bbox.  A click in that bbox calls the handler with `(DATA BUTTON
MODULE OUTPUT-IDX)`.  If no sub-fragment matches the click, dispatch
falls back to the module-level `:on-click`.

### Tooltips

Declare `:tooltip` with any of:

```lisp
:tooltip "static text"                     ; literal
:tooltip my-tooltip-fn                     ; symbol of a 0-arg fn
:tooltip (lambda () (format-time "%c"))    ; inline lambda
```

The tooltip is rendered on a **separate floating layer-shell
surface** just below the module (or above for bottom bars).  It
can extend beyond the bar height; you don't need a tall bar.

Visual tuning is global (see [configuration.md §7](configuration.md#section-7--hover-tooltips)).

### Module class slots

For users who want to reach below the macro:

```lisp
(defclass module ()
  ((name        :reader  module-name)
   (doc         :reader  module-doc)
   (update-fn   :reader  module-update-fn)
   (position    :accessor module-position)
   (priority    :accessor module-priority)
   (interval    :accessor module-interval)
   (on-click    :accessor module-on-click)
   (tooltip     :accessor module-tooltip)
   ;; internal:
   (state       :accessor module-state)
   (last-output :accessor module-last-output)
   (last-run    :accessor module-last-run)))
```

Modify a slot on a constructed instance to override declared
defaults at runtime; `lispbar` re-reads them every render.

### Helpers exported for module authors

All in the `:lispbar` package:

| Symbol                       | Use                                                  |
| ---------------------------- | ---------------------------------------------------- |
| `run-capture PROGRAM &rest`  | Run an external program, return stdout on exit 0.   |
| `executable-find-check NAME` | `T` if NAME is on `$PATH`.                          |
| `logmsg LEVEL FMT &rest`     | Write to stderr at LEVEL = `:debug` `:info` `:warn` `:error`. |
| `lispbar-state-directory`    | `$XDG_STATE_HOME/lispbar/` (auto-created).          |
| `lispbar-cache-directory`    | `$XDG_CACHE_HOME/lispbar/` (auto-created).          |
| `module-fragments MODULES`   | Internal renderer helper (rarely needed).           |
| `split-json-objects TEXT`    | Slice a JSON array string into per-object substrings. |
| `json-string-value / json-number-value / json-bool-value` | Lightweight JSON value extraction (whitespace-tolerant). |

### Worked examples

#### Mail count

```lisp
(in-package :lispbar)

(defmodule :mail
  (:doc "Unread Maildir count"
   :position :right :priority 42 :interval 30.0
   :on-click ((:left "notmuch-emacs --search='tag:unread'")))
  (let ((out (run-capture "notmuch" "count" "tag:unread")))
    (when out
      (let ((n (parse-integer (string-trim '(#\Space #\Newline) out)
                              :junk-allowed t)))
        (when (and n (plusp n))
          (list :text (format nil "MAIL ~d" n)
                :face (if (>= n 5) :warn :accent)))))))
```

#### CPU temperature

```lisp
(in-package :lispbar)

(defun cpu-temp-c ()
  (let ((out (run-capture "sensors" "-uA" "coretemp-isa-0000")))
    (when out
      (let ((m (search "temp1_input:" out)))
        (when m
          (parse-integer (subseq out (+ m 12))
                         :junk-allowed t))))))

(defmodule :cputemp
  (:doc "CPU package temperature in °C"
   :position :right :priority 56 :interval 5.0)
  (let ((c (cpu-temp-c)))
    (when c
      (list :text (format nil "~d°C" c)
            :face (cond ((>= c 80) :urgent)
                        ((>= c 65) :warn)
                        (t         :normal))))))
```

#### Counter that increments on click (state in a closure)

```lisp
(in-package :lispbar)

(let ((counter 0))
  (defun counter-bump (_m _b _i) (declare (ignore _m _b _i))
    (incf counter))
  (defmodule :counter
    (:doc "Click to increment"
     :position :center :priority 1 :interval 0.5
     :on-click ((:left counter-bump)))
    (format nil "~d" counter)))
```

### Where files load from

Lispbar walks the XDG search path on startup and `(load …)`s every
`*.lisp` file found.  Order tried, first wins (well — files always
*all* load; placement just controls visibility, not which wins):

```
$XDG_CONFIG_HOME/lispbar/modules/*.lisp     (user, highest priority)
$XDG_CONFIG_DIRS/.../lispbar/modules/*.lisp (system config)
$XDG_DATA_HOME/lispbar/modules/*.lisp       (per-user data)
$XDG_DATA_DIRS/.../lispbar/modules/*.lisp   (system data)
```

A file that errors during load is logged at `:warn` and skipped;
the bar continues with whatever else loaded.
