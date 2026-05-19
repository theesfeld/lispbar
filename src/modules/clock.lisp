;;;; clock.lisp  --  Time-of-day module.

(in-package #:lispbar)

(defvar *clock-format* :hh-mm-ss
  "One of :hh-mm-ss :hh-mm :iso8601 or a function of zero args.")

(defun clock-now ()
  (multiple-value-bind (s m h day mo yr) (get-decoded-time)
    (case *clock-format*
      (:hh-mm-ss (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))
      (:hh-mm    (format nil "~2,'0d:~2,'0d" h m))
      (:iso8601  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                         yr mo day h m s))
      (t (if (functionp *clock-format*) (funcall *clock-format*)
             (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))))))

(defun clock-tooltip ()
  (multiple-value-bind (s m h day mo yr dow) (get-decoded-time)
    (declare (ignore s m h))
    (format nil "~a ~4,'0d-~2,'0d-~2,'0d"
            (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            yr mo day)))

(defmodule :clock (:doc "Local time of day."
                   :position :right :priority 90 :interval 1.0
                   :tooltip clock-tooltip)
  (list :text (clock-now) :face :accent))
