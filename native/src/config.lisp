;;;; config.lisp  --  Loader, DSL, and XDG-aware extension discovery.
;;;;
;;;; A user config lives at $XDG_CONFIG_HOME/lispbar/config.lisp.
;;;; Before evaluating it, the binary walks the XDG search path and
;;;; loads every *.lisp file under
;;;;
;;;;     $XDG_CONFIG_HOME/lispbar/modules/
;;;;     $XDG_CONFIG_HOME/lispbar/themes/
;;;;     $XDG_CONFIG_DIRS .../lispbar/modules/
;;;;     $XDG_CONFIG_DIRS .../lispbar/themes/
;;;;     $XDG_DATA_HOME/lispbar/modules/
;;;;     $XDG_DATA_HOME/lispbar/themes/
;;;;     $XDG_DATA_DIRS .../lispbar/modules/
;;;;     $XDG_DATA_DIRS .../lispbar/themes/
;;;;
;;;; Each file is evaluated with *package* set to :lispbar so users
;;;; just call (defmodule ...) or (define-theme ...) directly.
;;;; After all extensions load, the user config can reference any of
;;;; their names in (placement ...) and (theme ...).

(in-package #:lispbar)

(defvar *config*
  '(:left ()
    :center ()
    :right (:clock)
    :theme :default
    :output :stdout
    :tick 1.0)
  "Active configuration plist, set by `load-config'.")

;;; ---- The DSL ----
;;;
;;; A user config is a sequence of top-level forms.  Each form is one
;;; of the recognised shapes below; unrecognised forms are logged and
;;; skipped, so older binaries seeing newer keywords degrade
;;; gracefully.
;;;
;;;   (placement :left   (:workspaces))
;;;   (placement :center (:media))
;;;   (placement :right  (:cpu :memory :audio :battery :clock))
;;;
;;;   (theme     :nordish)
;;;   (font      "Sans Bold 11")          ; Pango description
;;;   (height    28)                      ; pixels
;;;   (tick      1.0)                     ; seconds
;;;   (output    :wayland)                ; :wayland | :stdout | :json
;;;   (log-level :info)
;;;
;;; In addition to the declarative DSL above, the file may contain
;;; ordinary Lisp forms: e.g. (defmodule ...) / (define-theme ...) /
;;; (setf *workspaces-separator* " | ").  This is what lets a user
;;; keep their entire setup in one file if they prefer.

(defun evaluate-config-form (form)
  "Apply a single top-level config FORM to `*config*'.
Forms that don't look like a known DSL keyword are evaluated as
ordinary Lisp - this is what allows inline (defmodule ...) calls
inside config.lisp itself."
  (cond
    ((not (and (consp form) (symbolp (first form))))
     (logmsg :warn "ignoring malformed config form: ~s" form))
    (t
     (let ((head (intern (string (first form)) :keyword))
           (args (rest form)))
       (case head
         (:placement
          (destructuring-bind (where modules) args
            (setf (getf *config* (intern (string where) :keyword)) modules)))
         (:theme     (setf (getf *config* :theme)  (first args)))
         (:output    (setf (getf *config* :output) (first args)))
         (:tick      (setf (getf *config* :tick)   (first args)))
         (:height    (setf (getf *config* :height) (first args)))
         (:font      (setf (getf *config* :font)   (first args)))
         (:log-level (setf *log-level* (first args)))
         (otherwise
          ;; Not a DSL keyword - treat as ordinary Lisp (defmodule,
          ;; define-theme, setf, etc.).
          (eval form)))))))

;;; ---- Extension discovery ----

(defvar *loaded-extension-files* nil
  "List of files that `load-extensions' has loaded this session,
in load order.  Useful for diagnostics and `--show-extensions'.")

(defun load-extensions ()
  "Discover and load every user/system extension file.
See the file header for the directories searched.  Failures load
the next file (one broken module never stops the bar from booting)."
  (setf *loaded-extension-files* nil)
  (dolist (subdir '("modules" "themes"))
    (dolist (dir (lispbar-extension-directories subdir))
      (dolist (f (lisp-files-in-directory dir))
        (handler-case
            (let ((*package* (find-package :lispbar)))
              (logmsg :debug "loading extension ~a" f)
              (load f :verbose nil :print nil)
              (push f *loaded-extension-files*))
          (error (c)
            (logmsg :warn "extension ~a failed to load: ~a" f c))))))
  (setf *loaded-extension-files* (nreverse *loaded-extension-files*)))

;;; ---- Config loading ----

(defun load-config (&optional path)
  "Load extensions, then read PATH (or the first XDG-discovered
config) into `*config*'.  A missing config file is non-fatal - the
binary then runs with the built-in defaults plus whatever modules /
themes are present in $XDG_*_HOME/lispbar/."
  (load-extensions)
  (let ((file (or path (lispbar-config-file))))
    (cond
      ((null file)
       (logmsg :info "no config file found (expected at ~a); using defaults"
               (lispbar-default-config-path)))
      ((not (probe-file file))
       (logmsg :info "config file ~a not found; using defaults" file))
      (t
       (logmsg :info "loading config from ~a" file)
       (with-open-file (s file :direction :input)
         (let ((*package* (find-package :lispbar)))
           (loop for form = (read s nil :eof)
                 until (eq form :eof)
                 do (evaluate-config-form form))))))
    *config*))
