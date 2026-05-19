;;;; Example user theme: Dracula.
;;;;
;;;; Copy to $XDG_CONFIG_HOME/lispbar/themes/dracula.lisp, then
;;;; reference it from your config:
;;;;
;;;;   (theme :dracula)

(in-package :lispbar)

(define-theme :dracula
  :bg     '(0.157 0.165 0.212 1.0)
  :normal '(0.945 0.945 0.945 1.0)
  :accent '(0.741 0.576 0.976 1.0)
  :ok     '(0.314 0.980 0.482 1.0)
  :warn   '(0.945 0.980 0.549 1.0)
  :urgent '(1.000 0.333 0.333 1.0)
  :muted  '(0.380 0.420 0.490 1.0))
