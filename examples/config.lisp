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

;; Where the bar lives on every monitor.   :top | :bottom
(position :top)

;; Bar height in pixels.
(height 28)

;; Pango font description for the high-quality text path.
;; Examples:  "Monospace 11"     "FiraCode Nerd Font 11"     "Sans Bold 12"
(font "Sans Bold 11")

;; Visual theme: any registered theme name.  Built-in: :default,
;; :minimal, :nordish, :gruvboxish, :catppuccinish, :doomish.  Drop a
;; (define-theme :my-theme ...) file into $XDG_CONFIG_HOME/lispbar/themes/
;; to add your own.
(theme :nordish)

;; Module refresh interval (seconds).
(tick 1.0)

;; Output target:
;;   :wayland   - native wlr-layer-shell bar  (the real thing)
;;   :stdout    - human-readable line per tick  (debugging / piping)
;;   :json      - waybar-style JSON per tick    (custom-module driver)
(output :wayland)

;; :debug :info :warn :error
(log-level :info)
