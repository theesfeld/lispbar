;;;; Lispbar configuration.
;;;;
;;;; Place this file at ~/.config/lispbar/config.lisp - the binary
;;;; reads it on startup.  Forms are evaluated top-to-bottom; later
;;;; forms override earlier ones.

(placement :left   (:workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :bluetooth :brightness :battery :clock))

;; Refresh interval for the main loop (seconds).
(tick 1.0)

;; Output target:
;;   :stdout    - human-readable line per tick    (great for testing / piping)
;;   :json      - waybar-style JSON per tick      (custom-module driver)
;;   :wayland   - native layer-shell bar          (when implemented)
(output :stdout)

;; :debug :info :warn :error
(log-level :info)
