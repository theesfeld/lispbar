;;;; Lispbar configuration.
;;;;
;;;; Place this file at $XDG_CONFIG_HOME/lispbar/config.lisp - the
;;;; binary reads it on startup.  Forms are evaluated top-to-bottom;
;;;; later forms override earlier ones.

;; Which modules appear and where (any registered module name is fair
;; game; see `lispbar --list-modules' for the current inventory).
(placement :left   (:workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery :clock))

;; ----- geometry -----

;; Where the bar lives on every monitor.   :top | :bottom
(position :top)

;; Bar height in pixels.
(height 28)

;; Outer margin between the bar and the screen edge.  Accepts either
;; a single number (all sides) or up to four values (CSS-style):
;;
;;   (margin 8)               ; 8 px on every side
;;   (margin 8 16)            ; vertical / horizontal
;;   (margin 8 16 0)          ; top / horizontal / bottom
;;   (margin 8 16 4 16)       ; top / right / bottom / left
;;
;; Non-zero margin gives you a "floating" bar.  Combine with
;; corner-radius for the rounded look.
(margin 0)

;; Horizontal padding inside the bar before the first / after the
;; last module (pixels).
(padding 12)

;; Horizontal gap between adjacent modules (pixels).
(gap 12)

;; Corner radius for the bar background (pixels).
;; 0 = sharp.  Pair with a non-zero margin for a "floating pill" look.
(corner-radius 0)

;; ----- text -----

;; Pango font description for the high-quality text path.
;; Examples:  "Monospace 11"     "FiraCode Nerd Font 11"     "Sans Bold 12"
(font "Sans Bold 11")

;; ----- colour -----

;; Visual theme: any registered theme name.  Built-in: :default,
;; :minimal, :nordish, :gruvboxish, :catppuccinish, :doomish.  Drop a
;; (define-theme :my-theme ...) file into $XDG_CONFIG_HOME/lispbar/themes/
;; to add your own.
(theme :nordish)

;; ----- runtime -----

;; Module refresh interval (seconds).
(tick 1.0)

;; Output target:
;;   :wayland   - native wlr-layer-shell bar  (the real thing)
;;   :stdout    - human-readable line per tick  (debugging / piping)
;;   :json      - waybar-style JSON per tick    (custom-module driver)
(output :wayland)

;; :debug :info :warn :error
(log-level :info)
