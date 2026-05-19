;;;; ~/.config/lispbar/config.lisp
;;;;
;;;; This file is evaluated top-to-bottom on every start; later
;;;; forms override earlier ones.  Edit it freely and restart
;;;; lispbar to apply changes.
;;;;
;;;; Every option below shows its built-in default in a comment;
;;;; the value being set is the opinionated "looks good" pick.
;;;; Delete a form to fall back to the built-in default.
;;;;
;;;; Want a fresh copy of this file?      lispbar --init --force
;;;; Want to see what got loaded?         lispbar --show-extensions
;;;; Want to see where things resolve?    lispbar --print-paths

;;; ==========================================================
;;; 1.  Modules and placement
;;; ==========================================================

;; Each placement list is the modules to render in that section, in
;; the order they appear.  Run `lispbar --list-modules' for the live
;; inventory (built-ins plus anything you drop into
;; ~/.config/lispbar/modules/).
;;
;; Built-in module names:
;;
;;   :workspaces  - active workspaces (Sway / Hyprland / niri)
;;   :media       - now-playing (playerctl / any MPRIS2 player)
;;   :clock       - local time of day
;;   :cpu         - 1-minute CPU load average
;;   :memory      - used-RAM percentage
;;   :battery     - battery percent + charge state
;;   :audio       - default-sink volume + mute state
;;   :bluetooth   - adapter state and connection count
;;   :brightness  - backlight percentage
;;
;; Built-in default:
;;   :left   ()
;;   :center ()
;;   :right  (:clock)

(placement :left   (:workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery :clock))

;;; ==========================================================
;;; 2.  Geometry
;;; ==========================================================

;; Where the bar lives on every monitor.
;;
;;   :top | :bottom              Built-in default: :top
(position :top)

;; Bar height in pixels (its interior; margins live outside it).
;;
;;   integer                     Built-in default: 28
(height 32)

;; Outer margin between the bar and the screen edge.  Non-zero
;; margin gives a "floating" look; pair with corner-radius for the
;; rounded pill.  Accepts CSS-style:
;;
;;   (margin 8)                ; 8 px every side
;;   (margin 8 16)             ; vertical / horizontal
;;   (margin 8 16 0)           ; top / horizontal / bottom
;;   (margin 8 16 4 16)        ; top / right / bottom / left
;;
;;   Built-in default: 0   (flush against the screen edge)
(margin 8 12 0 12)

;; Horizontal padding inside the bar before the first / after the
;; last module.
;;
;;   integer (pixels)            Built-in default: 12
(padding 18)

;; Horizontal gap between adjacent modules.
;;
;;   integer (pixels)            Built-in default: 12
(gap 18)

;; Corner radius of the bar background.  0 = sharp corners.  Pair
;; with a non-zero margin (above) for the floating rounded look.
;;
;;   integer (pixels)            Built-in default: 0
(corner-radius 12)

;;; ==========================================================
;;; 3.  Typography
;;; ==========================================================

;; Pango font description.  Anything Pango can parse, e.g.:
;;
;;   "Monospace 11"
;;   "Sans 11"
;;   "Sans Bold 11"
;;   "FiraCode Nerd Font 11"
;;   "Inter 11"
;;   "JetBrains Mono Medium 10"
;;
;;   Built-in default: "Monospace 11"
(font "Sans 11")

;;; ==========================================================
;;; 4.  Colour theme
;;; ==========================================================

;; Built-in:  :default :minimal :nordish :gruvboxish :catppuccinish :doomish
;;
;; Drop your own (define-theme :my-theme ...) into
;; ~/.config/lispbar/themes/ and the name becomes selectable here.
;;
;;   keyword                     Built-in default: :default
(theme :catppuccinish)

;;; ==========================================================
;;; 5.  Runtime behaviour
;;; ==========================================================

;; Main-loop wake interval (seconds).  Modules have their own
;; per-module refresh interval; this is the outer poll cycle.
;;
;;   number (seconds)            Built-in default: 1.0
(tick 1.0)

;; Output backend.
;;
;;   :wayland     native wlr-layer-shell bar  (the real thing)
;;   :stdout      one bar line per tick to stdout (testing, piping)
;;   :json        waybar-compatible JSON per tick (custom modules)
;;
;;   Built-in default: :stdout
(output :wayland)

;; Logging verbosity.
;;
;;   :debug :info :warn :error   Built-in default: :info
(log-level :info)

;;; ==========================================================
;;; 6.  Per-module options
;;; ==========================================================
;;;
;;; Modules expose plain Lisp variables; tune them with `setf'.
;;; Unset variables fall back to the documented built-in default.
;;; Listed alphabetically.

;; ----- :audio -----
;;
;; (none currently exposed; behaviour is fully derived from `wpctl' /
;; `pactl' / `amixer' output.)

;; ----- :battery -----
;;
;; (none currently exposed; faces are picked automatically based on
;; charge level and charging state.)

;; ----- :bluetooth -----
;;
;; (none currently exposed; reads bluetoothctl directly.)

;; ----- :brightness -----
;;
;; (none currently exposed; tries brightnessctl then sysfs.)

;; ----- :clock -----
;;
;; Time-of-day format.
;;
;;   :hh-mm-ss     24-hour with seconds       "14:32:07"
;;   :hh-mm        24-hour without seconds    "14:32"
;;   :iso8601      full ISO 8601              "2025-05-19 14:32:07"
;;   <function>    your own zero-arg function returning a string
;;
;;   Built-in default: :hh-mm-ss
(setf *clock-format* :hh-mm-ss)

;; ----- :cpu, :memory -----
;;
;; (none currently exposed; thresholds are baked in.)

;; ----- :media -----
;;
;; Format of the now-playing string.
;;
;;   :artist-title  "Artist - Title"
;;   :title-only    just the title
;;   :short         title when present, else artist
;;
;;   Built-in default: :artist-title
(setf *media-format* :artist-title)
;; Maximum characters before the now-playing text is truncated.
;;
;;   integer                     Built-in default: 60
(setf *media-max-length* 60)

;; ----- :workspaces -----
;;
;; Which workspaces to show.  Same policy is applied uniformly to
;; Sway, Hyprland and niri (they all expose workspace + output info
;; in their IPC).
;;
;;   :current-output  workspaces on the focused monitor only
;;   :all             every workspace across every monitor (note:
;;                    on niri this may show duplicate numbers, since
;;                    each output has its own 1, 2, 3 numbering)
;;   :focused         only the workspace that currently has focus
;;
;;   Built-in default: :current-output
(setf *workspaces-scope* :current-output)
;; Brackets wrapping the focused workspace.
;;
;;   cons of OPEN/CLOSE strings   Built-in default: ("[" . "]")
(setf *workspaces-brackets* '("[" . "]"))
;; Separator between workspaces.
;;
;;   string                       Built-in default: " "
(setf *workspaces-separator* " ")
;; What to show when the filter leaves zero workspaces (e.g. early
;; in session).  NIL keeps the module silent.
;;
;;   string | NIL                 Built-in default: NIL
(setf *workspaces-empty-text* nil)
