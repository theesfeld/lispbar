;;;; Example user module: 5- and 15-minute load average.
;;;;
;;;; Copy this file to one of:
;;;;
;;;;   $XDG_CONFIG_HOME/lispbar/modules/loadavg.lisp   (per-user)
;;;;   $XDG_DATA_HOME/lispbar/modules/loadavg.lisp
;;;;   /etc/xdg/lispbar/modules/loadavg.lisp           (system-wide)
;;;;
;;;; then add :loadavg to one of your (placement ...) lists.

(in-package :lispbar)

(defmodule :loadavg
  (:doc "5- and 15-minute load average."
   :position :right :priority 58 :interval 5.0)
  (when (probe-file "/proc/loadavg")
    (with-open-file (s "/proc/loadavg" :direction :input)
      (let* ((line (read-line s nil ""))
             (parts (uiop:split-string line :separator '(#\Space))))
        (when (>= (length parts) 3)
          (list :text (format nil "LOAD ~a ~a"
                              (second parts) (third parts))
                :face :muted))))))
