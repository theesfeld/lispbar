;;;; memory.lisp  --  Used-RAM percentage from /proc/meminfo.

(in-package #:lispbar)

(defun read-meminfo-value (key)
  "Return the integer value (in kB) of KEY in /proc/meminfo, or NIL."
  (when (probe-file "/proc/meminfo")
    (with-open-file (s "/proc/meminfo" :direction :input)
      (loop for line = (read-line s nil nil)
            while line
            when (and (>= (length line) (length key))
                      (string= line key :end1 (length key)))
              return (let ((digits (remove-if-not #'digit-char-p line)))
                       (and (plusp (length digits))
                            (parse-integer digits)))))))

(defmodule :memory (:doc "Used-RAM percentage."
                    :position :right :priority 55 :interval 10.0)
  (let ((total (read-meminfo-value "MemTotal:"))
        (avail (read-meminfo-value "MemAvailable:")))
    (when (and total avail (plusp total))
      (format nil "MEM ~d%"
              (round (* 100 (/ (- total avail) total)))))))
