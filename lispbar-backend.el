;;; lispbar-backend.el --- Display-server backend protocol -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: frames, wayland, x11, exwm
;; URL: https://github.com/theesfeld/lispbar

;;; Commentary:

;; This file defines a pluggable backend protocol that abstracts every
;; display-server- or window-manager-specific operation Lispbar needs:
;; monitor detection, workspace info, frame docking hints, hot-plug
;; signalling, and window queries.
;;
;; Backends are EIEIO objects.  Each operation is a `cl-defgeneric'
;; whose first argument dispatches on the backend instance, so any
;; user can write a new backend purely in Emacs Lisp without modifying
;; Lispbar itself:
;;
;;   (defclass my-backend (lispbar-backend) ())
;;   (cl-defmethod lispbar-backend-detect-monitors ((_b my-backend)) ...)
;;   (lispbar-backend-register (make-instance 'my-backend) :priority 80)
;;
;; The backend with the highest priority whose `lispbar-backend-available-p'
;; method returns non-nil is selected automatically by
;; `lispbar-backend-autoselect'.  Selection can be overridden via the
;; `lispbar-backend' customisation option.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defgroup lispbar-backend nil
  "Display-server backends for Lispbar."
  :group 'lispbar
  :prefix "lispbar-backend-")

(defcustom lispbar-backend-choice 'auto
  "Which backend `lispbar-backend-autoselect' should pick.
The symbol `auto' picks the highest-priority available backend.
A non-nil symbol selects the registered backend with that name.
A `lispbar-backend' instance is used directly."
  :type '(choice (const :tag "Autodetect" auto)
                 (symbol :tag "Named backend")
                 (sexp :tag "Backend instance"))
  :group 'lispbar-backend)

;;; Base class

(defclass lispbar-backend ()
  ((name      :initarg :name      :initform 'generic :type symbol
              :documentation "Symbolic name of the backend.")
   (priority  :initarg :priority  :initform 0 :type integer
              :documentation "Selection priority; higher wins.")
   (features  :initarg :features  :initform nil :type list
              :documentation "Capability symbols this backend provides."))
  "Abstract base class for Lispbar display-server backends.")

(defvar lispbar-backend--registry nil
  "Alist of (NAME . BACKEND-INSTANCE) for every registered backend.")

(defvar lispbar-backend--current nil
  "The currently active `lispbar-backend' instance, or nil.")

;;; Registry

(defun lispbar-backend-register (backend)
  "Register BACKEND, replacing any previous backend with the same name.
BACKEND must be a `lispbar-backend' instance.  Returns BACKEND."
  (cl-check-type backend lispbar-backend)
  (let* ((name (oref backend name))
         (cell (assq name lispbar-backend--registry)))
    (if cell
        (setcdr cell backend)
      (push (cons name backend) lispbar-backend--registry)))
  backend)

(defun lispbar-backend-unregister (name)
  "Remove the backend named NAME from the registry."
  (setq lispbar-backend--registry
        (assq-delete-all name lispbar-backend--registry)))

(defun lispbar-backend-get (name)
  "Return the registered backend named NAME, or nil."
  (cdr (assq name lispbar-backend--registry)))

(defun lispbar-backend-list ()
  "Return the list of registered backend names."
  (mapcar #'car lispbar-backend--registry))

(defun lispbar-backend-current ()
  "Return the currently active backend, selecting one on demand."
  (or lispbar-backend--current
      (setq lispbar-backend--current (lispbar-backend-autoselect))))

(defun lispbar-backend-supports-p (feature &optional backend)
  "Return non-nil if BACKEND (current by default) advertises FEATURE."
  (let ((b (or backend (lispbar-backend-current))))
    (and b (memq feature (oref b features)))))

;;; Selection

(defun lispbar-backend-autoselect ()
  "Return the best available registered backend.
The backend with the highest priority whose
`lispbar-backend-available-p' is non-nil is selected.  If
`lispbar-backend' is bound to a symbol other than `auto', that
backend is used unconditionally."
  (cond
   ((and (not (eq lispbar-backend-choice 'auto))
         (symbolp lispbar-backend-choice))
    (or (lispbar-backend-get lispbar-backend-choice)
        (error "Lispbar backend `%s' is not registered" lispbar-backend-choice)))
   ((cl-typep lispbar-backend-choice 'lispbar-backend)
    lispbar-backend-choice)
   (t
    (let ((candidates
           (sort (copy-sequence lispbar-backend--registry)
                 (lambda (a b)
                   (> (oref (cdr a) priority)
                      (oref (cdr b) priority))))))
      (cl-loop for (_ . backend) in candidates
               when (lispbar-backend-available-p backend)
               return backend)))))

(defun lispbar-backend-set (name-or-instance)
  "Force the active backend to NAME-OR-INSTANCE.
A nil argument re-enables autoselection."
  (setq lispbar-backend--current
        (cond ((null name-or-instance) nil)
              ((cl-typep name-or-instance 'lispbar-backend) name-or-instance)
              ((symbolp name-or-instance) (lispbar-backend-get name-or-instance))
              (t (error "Invalid backend argument: %S" name-or-instance)))))

;;; Generic operations

(cl-defgeneric lispbar-backend-available-p (backend)
  "Return non-nil if BACKEND can run in the current environment.")

(cl-defmethod lispbar-backend-available-p ((_ lispbar-backend)) t)

(cl-defgeneric lispbar-backend-init (backend)
  "Perform one-time setup for BACKEND.")

(cl-defmethod lispbar-backend-init ((_ lispbar-backend)) nil)

(cl-defgeneric lispbar-backend-cleanup (backend)
  "Release resources owned by BACKEND.")

(cl-defmethod lispbar-backend-cleanup ((_ lispbar-backend)) nil)

(cl-defgeneric lispbar-backend-detect-monitors (backend)
  "Return a list of monitor plists for BACKEND.
Each plist must contain at least :id :name :x :y :width :height
:primary :source.  Implementations may add :display-name :edid
:properties or any other key consumers wish to read.")

(cl-defmethod lispbar-backend-detect-monitors ((_ lispbar-backend))
  (lispbar-backend--monitors-from-attributes))

(cl-defgeneric lispbar-backend-configure-frame (backend frame geometry)
  "Apply backend-specific window properties to FRAME.
GEOMETRY is the plist returned by `lispbar--calculate-frame-geometry'.
Implementations may set struts, dock hints, layer-shell anchors, etc.
Should be safe to call repeatedly.")

(cl-defmethod lispbar-backend-configure-frame ((_ lispbar-backend) _frame _geometry)
  nil)

(cl-defgeneric lispbar-backend-register-hotplug (backend callback)
  "Arrange for CALLBACK to be invoked when the monitor layout changes.
CALLBACK is a function of no arguments.  Returns a token suitable
for `lispbar-backend-unregister-hotplug'.")

(cl-defmethod lispbar-backend-register-hotplug ((_ lispbar-backend) _cb) nil)

(cl-defgeneric lispbar-backend-unregister-hotplug (backend token)
  "Undo a previous `lispbar-backend-register-hotplug' call.")

(cl-defmethod lispbar-backend-unregister-hotplug ((_ lispbar-backend) _t) nil)

(cl-defgeneric lispbar-backend-current-workspace (backend)
  "Return the active workspace index (0-based) or nil if unknown.")

(cl-defmethod lispbar-backend-current-workspace ((_ lispbar-backend)) nil)

(cl-defgeneric lispbar-backend-workspace-names (backend)
  "Return a list of workspace name strings, or nil if unknown.")

(cl-defmethod lispbar-backend-workspace-names ((_ lispbar-backend)) nil)

(cl-defgeneric lispbar-backend-workspace-count (backend)
  "Return the number of workspaces, or nil if unknown."
  (length (lispbar-backend-workspace-names backend)))

(cl-defgeneric lispbar-backend-switch-workspace (backend index)
  "Activate workspace INDEX (0-based).  Return non-nil on success.")

(cl-defmethod lispbar-backend-switch-workspace ((_ lispbar-backend) _i) nil)

(cl-defgeneric lispbar-backend-register-workspace-hook (backend callback)
  "Arrange for CALLBACK to run on workspace changes.
Returns an opaque token for unregistration.")

(cl-defmethod lispbar-backend-register-workspace-hook ((_ lispbar-backend) _cb) nil)

(cl-defgeneric lispbar-backend-unregister-workspace-hook (backend token)
  "Undo a previous `lispbar-backend-register-workspace-hook'.")

(cl-defmethod lispbar-backend-unregister-workspace-hook ((_ lispbar-backend) _t) nil)

(cl-defgeneric lispbar-backend-window-title (backend)
  "Return the title of the focused window, or nil.")

(cl-defmethod lispbar-backend-window-title ((_ lispbar-backend))
  (when-let* ((frame (selected-frame)))
    (frame-parameter frame 'name)))

(cl-defgeneric lispbar-backend-window-class (backend)
  "Return the class (X11 WM_CLASS or Wayland app-id) of the focused window.")

(cl-defmethod lispbar-backend-window-class ((_ lispbar-backend)) nil)

(cl-defgeneric lispbar-backend-describe (backend)
  "Return a human-readable description of BACKEND for status output.")

(cl-defmethod lispbar-backend-describe ((b lispbar-backend))
  (format "%s (priority %d, features %S)"
          (oref b name) (oref b priority) (oref b features)))

;;; Helpers shared by backends

(defun lispbar-backend--monitors-from-attributes ()
  "Build monitor plists from `display-monitor-attributes-list'.
Works on every graphical Emacs build (X, GTK, NS, PGTK)."
  (let ((monitors (and (display-graphic-p)
                       (ignore-errors (display-monitor-attributes-list))))
        (counter 0)
        (result nil))
    (dolist (m monitors)
      (let* ((geom (cdr (assq 'geometry m)))
             (name (or (cdr (assq 'name m))
                       (format "monitor-%d" counter)))
             (x (nth 0 geom))
             (y (nth 1 geom))
             (w (nth 2 geom))
             (h (nth 3 geom)))
        (push (list :id (intern name)
                    :name name
                    :display-name name
                    :x x :y y :width w :height h
                    :primary (or (cdr (assq 'primary m)) (zerop counter))
                    :connected t
                    :edid nil
                    :properties m
                    :source 'display-monitor-attributes
                    :timestamp (current-time))
              result))
      (setq counter (1+ counter)))
    (or (nreverse result)
        (and (display-graphic-p)
             (list (list :id 'fallback :name "fallback"
                         :display-name "Fallback Monitor"
                         :x 0 :y 0
                         :width (display-pixel-width)
                         :height (display-pixel-height)
                         :primary t :connected t :edid nil
                         :properties nil :source 'fallback
                         :timestamp (current-time)))))))

(defvar json-object-type)
(defvar json-array-type)
(defvar json-key-type)
(defvar json-false)
(defvar json-null)

(defun lispbar-backend--call-process-json (program &rest args)
  "Invoke PROGRAM with ARGS, parsing stdout as JSON.
Returns the decoded structure on success, nil on failure."
  (when (executable-find program)
    (with-temp-buffer
      (let ((status (apply #'call-process program nil t nil args)))
        (when (eq status 0)
          (goto-char (point-min))
          (condition-case _err
              (if (fboundp 'json-parse-buffer)
                  (json-parse-buffer :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)
                (require 'json)
                (let ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol)
                      (json-false nil)
                      (json-null nil))
                  (funcall (intern "json-read"))))
            (error nil)))))))

;;; Generic backend instance (used as last-resort fallback)

(defvar lispbar-backend-generic
  (make-instance 'lispbar-backend
                 :name 'generic
                 :priority 1
                 :features '(monitor-detect))
  "Last-resort backend; usable on any graphical Emacs build.")

(lispbar-backend-register lispbar-backend-generic)

(provide 'lispbar-backend)
;;; lispbar-backend.el ends here
