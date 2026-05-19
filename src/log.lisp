;;;; log.lisp  --  Tiny structured logger that writes to stderr.

(in-package #:lispbar)

(defvar *log-level* :info
  "One of :debug :info :warn :error.")

(defvar *log-levels* '((:debug . 0) (:info . 1) (:warn . 2) (:error . 3)))

(defun log-enabled-p (level)
  (>= (cdr (assoc level *log-levels*))
      (cdr (assoc *log-level* *log-levels*))))

(defun logmsg (level fmt &rest args)
  "Emit a single-line log record at LEVEL using FMT and ARGS."
  (when (log-enabled-p level)
    (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time))
      (declare (ignore d mo y))
      (format *error-output* "~2,'0d:~2,'0d:~2,'0d ~5a " h m s level))
    (apply #'format *error-output* fmt args)
    (terpri *error-output*)
    (force-output *error-output*)))
