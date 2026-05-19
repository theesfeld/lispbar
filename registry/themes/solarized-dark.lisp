;;;; solarized-dark.lisp - Ethan Schoonover's Solarized (dark).

(in-package :lispbar)

(define-theme :solarized-dark
  :bg     '(0.000 0.169 0.212 1.0)   ; base03  #002b36
  :normal '(0.514 0.580 0.588 1.0)   ; base0   #839496
  :accent '(0.149 0.545 0.824 1.0)   ; blue    #268bd2
  :ok     '(0.522 0.600 0.000 1.0)   ; green   #859900
  :warn   '(0.710 0.537 0.000 1.0)   ; yellow  #b58900
  :urgent '(0.863 0.196 0.184 1.0)   ; red     #dc322f
  :muted  '(0.345 0.431 0.459 1.0))  ; base01  #586e75
