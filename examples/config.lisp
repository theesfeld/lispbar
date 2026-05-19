;;;; ~/.config/lispbar/config.lisp
;;;;
;;;; Evaluated top-to-bottom; later forms override earlier ones.
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

;; Run `lispbar --list-modules' for the live inventory (built-ins
;; plus anything you drop into ~/.config/lispbar/modules/).
;;
;; Built-in module names:
;;
;;   :launcher    - click-to-spawn launcher button
;;   :workspaces  - active workspaces (Sway / Hyprland / niri)
;;   :media       - now-playing (playerctl / MPRIS2)
;;   :clock       - local time of day
;;   :cpu         - 1-minute CPU load average
;;   :memory      - used-RAM percentage
;;   :battery     - battery percent + charge state
;;   :audio       - default-sink volume + mute
;;   :network     - WiFi SSID + signal, Ethernet, VPN
;;   :bluetooth   - adapter state and connection count
;;   :brightness  - backlight percentage
;;   :tray        - StatusNotifierItem system tray (D-Bus)

(placement :left   (:launcher :workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :network :audio :bluetooth :brightness
                    :battery :tray :clock))

;;; ==========================================================
;;; 2.  Geometry
;;; ==========================================================

;;   :top | :bottom              Built-in default: :top
(position :top)

;;   integer (pixels)            Built-in default: 28
(height 32)

;; Outer margin between the bar and the screen edge.  Non-zero
;; margin gives the floating look; pair with corner-radius.
;;   (margin 8)                ; 8 px every side
;;   (margin 8 16)             ; vertical / horizontal
;;   (margin 8 16 0)           ; top / horizontal / bottom
;;   (margin 8 16 4 16)        ; top / right / bottom / left
;;
;;   Built-in default: 0
(margin 8 12 0 12)

;;   integer (pixels)            Built-in default: 12
(padding 18)

;;   integer (pixels)            Built-in default: 12
(gap 18)

;;   integer (pixels)            Built-in default: 0
(corner-radius 12)

;;; ==========================================================
;;; 3.  Typography
;;; ==========================================================

;; Pango font description.  Examples:  "Monospace 11"  "Sans 11"
;; "Sans Bold 11"  "FiraCode Nerd Font 11"  "Inter 11"
;;
;;   Built-in default: "Monospace 11"
(font "Sans 11")

;;; ==========================================================
;;; 4.  Colour theme
;;; ==========================================================

;; Built-in:  :default :minimal :nordish :gruvboxish :catppuccinish :doomish
;; Drop your own (define-theme :my-theme ...) into
;; ~/.config/lispbar/themes/ for new names.
;;
;;   keyword                     Built-in default: :default
(theme :catppuccinish)

;;; ==========================================================
;;; 5.  Runtime behaviour
;;; ==========================================================

;;   number (seconds)            Built-in default: 1.0
(tick 1.0)

;;   :wayland :stdout :json      Built-in default: :stdout
(output :wayland)

;;   :debug :info :warn :error   Built-in default: :info
(log-level :info)

;;; ==========================================================
;;; 6.  Per-module options
;;; ==========================================================
;;;
;;; Module options are ordinary Lisp variables; tune them with
;;; `setf'.  Listed alphabetically.

;; ----- :audio -----
;; Left-click and middle-click actions (shell commands; NIL to disable).
;;
;;   string | NIL                Built-in defaults:
;;                                 "pavucontrol || pwvucontrol"
;;                                 "pactl set-sink-mute @DEFAULT_SINK@ toggle"
(setf *audio-on-click*        "pavucontrol || pwvucontrol")
(setf *audio-on-middle-click* "pactl set-sink-mute @DEFAULT_SINK@ toggle")

;; ----- :battery -----
;; No tunables exposed; faces are picked from charge level/state.

;; ----- :bluetooth -----
;;
;;   string | NIL                Built-in defaults:
(setf *bluetooth-on-click*        "blueman-manager || blueberry")
(setf *bluetooth-on-middle-click* "bluetoothctl power toggle")

;; ----- :brightness -----
;; No tunables exposed; tries brightnessctl then sysfs.

;; ----- :clock -----
;; Time format.
;;   :hh-mm-ss     "14:32:07"
;;   :hh-mm        "14:32"
;;   :iso8601      "2025-05-19 14:32:07"
;;   <function>    your own zero-arg function returning a string
;;
;;   Built-in default: :hh-mm-ss
(setf *clock-format* :hh-mm-ss)

;; ----- :cpu, :memory -----
;; No tunables exposed.

;; ----- :launcher -----
;; Command for left/right click.
;;
;;   string | NIL                Built-in defaults:
;;                                 "wofi --show drun || fuzzel || rofi -show drun"
;;                                 "wlogout || swaynag -m 'logout?'"
(setf *launcher-command*         "wofi --show drun || fuzzel || rofi -show drun")
(setf *launcher-on-right-click*  "wlogout || swaynag -m 'logout?'")
;; Visible label (a single glyph or short string).
;;
;;   string                      Built-in default: " "
(setf *launcher-label* " ")

;; ----- :media -----
;;
;;   :artist-title | :title-only | :short  Built-in default: :artist-title
(setf *media-format* :artist-title)
;;   integer                     Built-in default: 60
(setf *media-max-length* 60)

;; ----- :network -----
;; Format string for WiFi.  Placeholders:
;;   {ssid} {signal} {bars} {device}
;;
;;   string                      Built-in default: "{ssid} {signal}%"
(setf *network-format-wifi*     "{ssid} {signal}%")
;;   string                      Built-in default: "ETH"
(setf *network-format-ethernet* "ETH")
;;   string                      Built-in default: "OFF"
(setf *network-format-down*     "OFF")
;; Left-click action (NetworkManager / iwd GUIs by default).
;;
;;   string | NIL                Built-in default:
;;                                 "nm-connection-editor || iwgtk"
(setf *network-on-click* "nm-connection-editor || iwgtk")

;; ----- :workspaces -----
;; Scope of the list.
;;   :current-output   workspaces on the focused monitor only
;;   :all              every workspace across every monitor
;;   :focused          only the workspace that has focus
;;
;;   Built-in default: :current-output
(setf *workspaces-scope*     :current-output)
;;   cons of OPEN/CLOSE strings  Built-in default: ("[" . "]")
(setf *workspaces-brackets*  '("[" . "]"))
;;   string                      Built-in default: " "
(setf *workspaces-separator* " ")
;; Text shown if the filter leaves zero entries; NIL = stay silent.
;;
;;   string | NIL                Built-in default: NIL
(setf *workspaces-empty-text* nil)

;; ----- :tray -----
;; The tray module hosts a StatusNotifierItem watcher on the session
;; D-Bus and shows every registered tray app.  Icons are rendered
;; from each item's inline pixmap (cairo blit); apps that publish
;; only an icon name (no pixmap) fall back to text if the flag below
;; is on.
;;
;; Icon size (pixels).  Will be scaled down from the source pixmap.
;;
;;   integer                     Built-in default: 16
(setf *tray-icon-size* 18)
;;
;; Fall back to the item Id as text when no pixmap is provided.
;;
;;   T | NIL                     Built-in default: T
(setf *tray-show-text-when-no-icon* t)

;;; ==========================================================
;;; 7.  Hover tooltips
;;; ==========================================================
;;;
;;; Every module may declare `:tooltip' (a string or a thunk
;;; returning a string) that's drawn as an overlay above the
;;; module's pixel range while the pointer hovers over it.  The
;;; built-in modules already expose useful tooltips - hover the
;;; clock to see the date, the battery to see time-remaining, etc.
;;;
;;; Visual tuning of the tooltip overlay:

;;   (R G B A) doubles 0.0-1.0   Built-in default: (0 0 0 0.85)
(setf *wayland-tooltip-bg* '(0.10 0.12 0.18 0.92))
;;   integer (pixels)            Built-in default: 8.0d0
(setf *wayland-tooltip-padding-x* 10.0d0)
;;   integer (pixels)            Built-in default: 4.0d0
(setf *wayland-tooltip-padding-y* 6.0d0)
;;   integer (pixels)            Built-in default: 6.0d0
(setf *wayland-tooltip-corner* 8.0d0)
