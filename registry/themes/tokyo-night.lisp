;;;; tokyo-night.lisp - Tokyo Night palette for Lispbar.
;;;;
;;;; Faithful interpretation of folke/tokyonight.nvim's "storm"
;;;; variant.  Pair with `(font "JetBrains Mono 11")' for the full
;;;; effect.

(in-package :lispbar)

(define-theme :tokyo-night
  :bg     '(0.094 0.106 0.149 1.0)   ; #181a25
  :normal '(0.776 0.808 0.929 1.0)   ; #c6cfed
  :accent '(0.471 0.620 0.949 1.0)   ; #789ef2
  :ok     '(0.620 0.808 0.580 1.0)   ; #9ece95
  :warn   '(0.918 0.745 0.439 1.0)   ; #ebbe70
  :urgent '(0.961 0.482 0.604 1.0)   ; #f57b9a
  :muted  '(0.337 0.371 0.475 1.0))  ; #565e79
