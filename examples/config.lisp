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
;;   :registry    - click-to-open community module/theme browser
;;                  (needs fzf installed; spawns a terminal)

(placement :left   (:launcher :workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :network :audio :bluetooth :brightness
                    :battery :tray :registry :clock))

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
;; Click bindings: left = open GUI mixer, middle = toggle mute,
;; scroll = adjust volume.
;;
;;   string | NIL                Built-in defaults:
;;                                 "pavucontrol || pwvucontrol"
;;                                 "pactl set-sink-mute @DEFAULT_SINK@ toggle"
(setf *audio-on-click*        "pavucontrol || pwvucontrol")
(setf *audio-on-middle-click* "pactl set-sink-mute @DEFAULT_SINK@ toggle")
;;   integer (percentage points)  Built-in default: 5
(setf *audio-scroll-step* 5)

;; ----- :battery -----
;; No tunables exposed; faces are picked from charge level/state.

;; ----- :bluetooth -----
;;
;;   string | NIL                Built-in defaults:
(setf *bluetooth-on-click*        "blueman-manager || blueberry")
(setf *bluetooth-on-middle-click* "bluetoothctl power toggle")

;; ----- :brightness -----
;; Scroll-wheel changes brightness via brightnessctl.  Step is
;; configurable.  Left-click runs an optional shell command.
;;
;;   integer (percentage points)   Built-in default: 5
(setf *brightness-step* 5)
;;
;;   string | NIL                  Built-in default: NIL
(setf *brightness-on-click* nil)

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

;; ----- :registry -----
;; The label glyph, the terminal used to host `lispbar registry browse',
;; and an optional override for the on-click shell command.
;;
;;   string | NIL                Built-in default: "📦"
(setf *registry-label*    "📦")
;;   shell expression           Built-in default tries $TERMINAL, foot,
;;                              alacritty, kitty, wezterm, gnome-terminal,
;;                              konsole, xterm in that order.
(setf *registry-terminal* "$TERMINAL || foot || alacritty || kitty || wezterm || gnome-terminal || konsole || xterm")
;;   string | NIL                Built-in default: NIL (use the terminal-spawn)
;;                              Set this to drive the picker yourself.
(setf *registry-on-click* nil)

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
;; Left-click opens the NM/iwd GUI; middle-click runs a configurable
;; command (default: print wifi radio state).
;;
;; Format string for WiFi.  Placeholders:
;;   {ssid} {signal} {bars} {device}
;;
;;   string                      Built-in default: "{ssid} {signal}%"
(setf *network-format-wifi*     "{ssid} {signal}%")
;;   string                      Built-in default: "ETH"
(setf *network-format-ethernet* "ETH")
;;   string                      Built-in default: "OFF"
(setf *network-format-down*     "OFF")
;;   string | NIL                Built-in default:
;;                                 "nm-connection-editor || iwgtk"
(setf *network-on-click*        "nm-connection-editor || iwgtk")
;;   string | NIL                Built-in default: "nmcli radio wifi"
(setf *network-on-middle-click* "nmcli radio wifi")

;; ----- :workspaces -----
;; Each workspace number is independently clickable (left-click
;; switches to that workspace).  Scroll-wheel on the workspaces
;; module moves to previous/next.  Sway, Hyprland and niri are all
;; handled via their native CLI tools.
;;
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
;;; Every module may declare `:tooltip' (a string, a function of
;;; no args, or a symbol naming such a function) which is drawn
;;; as a floating overlay just below the module while the pointer
;;; hovers over it.  The tooltip is a separate wlr-layer-shell
;;; surface anchored to the bar's edge, so it can extend BEYOND
;;; the bar's vertical bounds - it doesn't have to fit inside the
;;; bar height.
;;;
;;; The built-in modules already expose useful tooltips: hover
;;; the clock for the full date, the audio module for verbose
;;; volume/mute state, the network module to see the device
;;; details, etc.
;;;
;;; Visual tuning:

;;   (R G B A) doubles 0.0-1.0   Built-in default: (0 0 0 0.85)
(setf *wayland-tooltip-bg* '(0.10 0.12 0.18 0.92))
;;   integer (pixels)            Built-in default: 8.0d0
(setf *wayland-tooltip-padding-x* 10.0d0)
;;   integer (pixels)            Built-in default: 4.0d0
(setf *wayland-tooltip-padding-y* 6.0d0)
;;   integer (pixels)            Built-in default: 6.0d0
(setf *wayland-tooltip-corner* 8.0d0)

;;; ==========================================================
;;; 8.  Click handler reference for custom modules
;;; ==========================================================
;;;
;;; `defmodule' takes an `:on-click' plist mapping button keywords
;;; to handlers:
;;;
;;;   :left   :right   :middle   :scroll-up   :scroll-down
;;;   :side   :extra
;;;
;;; A handler may be:
;;;
;;;   STRING          shell command, run via `sh -c'
;;;   LIST            argv list, run directly via uiop:launch-program
;;;   SYMBOL          named function, called with (MODULE BUTTON OUTPUT-IDX)
;;;
;;; Example - simple shell commands:
;;;
;;;   (in-package :lispbar)
;;;
;;;   (defmodule :weather
;;;     (:doc "Hourly forecast"
;;;      :position :right :priority 35 :interval 600.0
;;;      :on-click ((:left  "xdg-open https://wttr.in")
;;;                 (:right "notify-send 'Weather' \"$(curl -s wttr.in/?format=4)\"")))
;;;     (run-capture "curl" "-s" "wttr.in/?format=3"))
;;;
;;; Example - named functions with full info:
;;;
;;;   (defun my-click (module button output-idx)
;;;     (format *error-output* "clicked ~a (~a)~%"
;;;             (module-name module) button))
;;;
;;;   (defmodule :foo (:on-click ((:left  my-click)
;;;                               (:right my-click)))
;;;     ...)
;;;
;;; ---- Per-sub-fragment click handlers ----
;;;
;;; If your module returns a list-of-fragments output, individual
;;; fragments can carry their own click handlers.  The :workspaces
;;; module uses this so each workspace number is independently
;;; clickable.
;;;
;;; Fragment shapes accepted by the renderer:
;;;
;;;   (TEXT FACE)
;;;     plain text in FACE
;;;
;;;   (:gap PIXELS)
;;;     horizontal whitespace between modules
;;;
;;;   (:clickable :text TEXT :face FACE :on-click HANDLER :data DATA)
;;;     painted like text but the renderer records its bbox; clicks
;;;     anywhere in the bbox invoke HANDLER with (DATA BUTTON MODULE
;;;     OUTPUT-IDX).  HANDLER is the same shape as above (string,
;;;     argv list, symbol, function).
;;;
;;; Custom-module example with per-fragment handlers:
;;;
;;;   (defun pin-screen (data button m i)
;;;     (declare (ignore button m i))
;;;     (uiop:launch-program (list "swaymsg" "output" data "power" "off")))
;;;
;;;   (defmodule :outputs (:doc "Click an output name to power it off"
;;;                        :position :right :priority 20 :interval 30.0)
;;;     (let* ((j (run-capture "swaymsg" "-t" "get_outputs" "-r"))
;;;            (names (and j (loop for o in (split-json-objects j)
;;;                                collect (json-string-value o "name")))))
;;;       (and names (list :fragments
;;;                        (loop for n in names
;;;                              collect (list :clickable :text n
;;;                                            :face :accent
;;;                                            :on-click 'pin-screen
;;;                                            :data n)
;;;                              collect (list "  " :muted))))))
;;;
;;; ---- Hover tooltips ----
;;;
;;; `defmodule' also takes `:tooltip', a string / function-of-no-args /
;;; symbol naming such a function.  Whatever it returns is rendered
;;; as a floating tooltip below the module while the pointer hovers.
;;; See section 7 for visual tuning.
