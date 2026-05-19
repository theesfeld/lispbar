;;;; cpu.lisp  --  1-minute load average from /proc/loadavg.

(in-package #:lispbar)

(defun read-loadavg ()
  "Return the 1-minute load average as a float, or NIL."
  (when (probe-file "/proc/loadavg")
    (with-open-file (s "/proc/loadavg" :direction :input)
      (let* ((line (read-line s nil nil))
             (token (and line (subseq line 0 (position #\Space line)))))
        (and token (with-input-from-string (s2 token)
                     (let ((*read-default-float-format* 'double-float))
                       (read s2))))))))

(defmodule :cpu (:doc "1-minute CPU load average."
                 :position :right :priority 60 :interval 5.0)
  (let ((v (read-loadavg)))
    (and v (format nil "CPU ~,2f" v))))
