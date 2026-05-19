;;;; config.lisp  --  Loader and DSL for `~/.config/lispbar/config.lisp'.

(in-package #:lispbar)

(defvar *config*
  '(:left ()
    :center ()
    :right (:clock)
    :theme :default
    :output :stdout
    :tick 1.0)
  "Active configuration plist, set by `load-config'.")

(defvar *config-path-candidates*
  (list
   ;; XDG_CONFIG_HOME/lispbar/config.lisp
   (lambda ()
     (let ((xdg (or (uiop:getenv "XDG_CONFIG_HOME")
                    (uiop:native-namestring (merge-pathnames ".config/"
                                                              (user-homedir-pathname))))))
       (merge-pathnames "lispbar/config.lisp" xdg)))
   ;; ~/.lispbar.lisp
   (lambda ()
     (merge-pathnames ".lispbar.lisp" (user-homedir-pathname))))
  "Functions that produce candidate config paths in order of preference.")

(defun default-config-path ()
  "Return the first candidate config path that exists, or the first
candidate if none exists (for diagnostic messages)."
  (let ((paths (mapcar #'funcall *config-path-candidates*)))
    (or (find-if #'probe-file paths) (first paths))))

;;; ---- The DSL ----
;;;
;;; A user config is a sequence of top-level forms.  Each form is one of:
;;;
;;;   (placement :left   (:clock :workspace))
;;;   (placement :center (:window-title))
;;;   (placement :right  (:cpu :memory :audio :battery))
;;;   (theme :nordish)
;;;   (output :stdout)        ; or :wayland (when implemented) or :json
;;;   (tick 1.0)              ; main-loop refresh interval in seconds
;;;
;;; Forms are interpreted by `evaluate-config-form' below.  Unknown
;;; forms are logged but do not abort the load - configs degrade
;;; gracefully when older binaries see newer keywords.

(defun evaluate-config-form (form)
  "Apply a single top-level config FORM to `*config*'."
  (unless (and (consp form) (symbolp (first form)))
    (logmsg :warn "ignoring malformed config form: ~s" form)
    (return-from evaluate-config-form nil))
  (let ((head (intern (string (first form)) :keyword))
        (args (rest form)))
    (case head
      (:placement
       (destructuring-bind (where modules) args
         (setf (getf *config* (intern (string where) :keyword)) modules)))
      (:theme    (setf (getf *config* :theme)  (first args)))
      (:output   (setf (getf *config* :output) (first args)))
      (:tick     (setf (getf *config* :tick)   (first args)))
      (:log-level
       (setf *log-level* (first args)))
      (otherwise
       (logmsg :warn "ignoring unknown config form: (~a ...)" head)))))

(defun load-config (&optional path)
  "Load PATH (or the first existing default) into `*config*'.
A missing or unreadable config file is non-fatal - the binary then
runs with the built-in defaults."
  (let ((file (or path (default-config-path))))
    (cond
      ((null file)
       (logmsg :info "no config file specified, using defaults"))
      ((not (probe-file file))
       (logmsg :info "config file ~a not found, using defaults" file))
      (t
       (logmsg :info "loading config from ~a" file)
       (with-open-file (s file :direction :input)
         (let ((*package* (find-package :lispbar)))
           (loop for form = (read s nil :eof)
                 until (eq form :eof)
                 do (evaluate-config-form form))))))
    *config*))
