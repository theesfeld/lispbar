;;;; module.lisp  --  Module data structure, registry and `defmodule' DSL.

(in-package #:lispbar)

(defclass module ()
  ((name      :initarg :name      :reader module-name)
   (doc       :initarg :doc       :reader module-doc       :initform "")
   (update-fn :initarg :update-fn :reader module-update-fn)
   (position  :initarg :position  :accessor module-position :initform :right)
   (priority  :initarg :priority  :accessor module-priority :initform 50)
   (interval  :initarg :interval  :accessor module-interval :initform 5.0)
   ;; Per-button click actions.  Either a single value (applied to
   ;; left-click) or a plist  ((:left ACTION) (:right ACTION) (:middle ACTION)).
   ;; ACTION can be:
   ;;   - a string                shell command, run with `sh -c'
   ;;   - a list of strings       argv, run directly
   ;;   - a function              called with (MODULE BUTTON OUTPUT-INDEX)
   ;;   - NIL                     do nothing
   (on-click  :initarg :on-click  :accessor module-on-click :initform nil)
   ;; Tooltip can be a string (static), a function of no args
   ;; returning a string, or NIL (no tooltip).
   (tooltip   :initarg :tooltip   :accessor module-tooltip  :initform nil)
   (state     :initform nil       :accessor module-state)
   (last-output :initform nil     :accessor module-last-output)
   (last-run  :initform 0         :accessor module-last-run))
  (:documentation "Runtime representation of a Lispbar module."))

(defvar *module-factories* (make-hash-table :test 'eq)
  "NAME (a keyword) -> closure that returns a fresh `module' instance.")

(defun register-module (name &key doc factory)
  "Register module NAME with optional DOC and FACTORY (a thunk).
Replaces any previous registration with the same NAME."
  (check-type name keyword)
  (check-type factory function)
  (setf (gethash name *module-factories*)
        (list :doc doc :factory factory))
  name)

(defun module-names ()
  "Return a sorted list of registered module names."
  (sort (loop for k being the hash-keys of *module-factories* collect k)
        #'string<
        :key #'symbol-name))

(defun find-module-factory (name)
  "Return the registration plist for NAME, or NIL."
  (gethash name *module-factories*))

(defun make-module (name)
  "Instantiate a fresh module from the factory registered as NAME."
  (let ((entry (gethash name *module-factories*)))
    (unless entry
      (error "No Lispbar module factory registered for ~S" name))
    (funcall (getf entry :factory))))

(defmacro defmodule (name (&key doc (position :right) (priority 50)
                                  (interval 5.0) on-click tooltip)
                     &body body)
  ;; Treat `on-click' as data: it's a plist of keyword -> action,
  ;; where each action is either a string (shell command), a symbol
  ;; naming a function, or a list of strings (argv).  We quote it
  ;; into the factory body so the keyword list doesn't get
  ;; mis-evaluated as a function call.
  (declare (ignorable on-click))
  "Declare a module NAME (a keyword).
BODY is the update function body and must return a string (or NIL).
The module is registered eagerly under NAME and a fresh instance is
created on each `make-module' call.

`:on-click' attaches a default action triggered when the user clicks
the module's bbox in the Wayland output.  Same shape as the
`on-click' slot on the module class - see (find-class 'module).

Example:

  (defmodule :clock (:doc \"24h clock.\" :position :right :interval 1.0
                     :on-click \"gnome-calendar\")
    (multiple-value-bind (s m h) (get-decoded-time)
      (format nil \"~2,'0d:~2,'0d:~2,'0d\" h m s)))"
  (let ((fn-name (intern (concatenate 'string "MODULE-UPDATE-"
                                      (symbol-name name)))))
    `(progn
       (defun ,fn-name () ,@body)
       (register-module ,name
                        :doc ,(or doc "")
                        :factory (lambda ()
                                   (make-instance 'module
                                                  :name ,name
                                                  :doc ,(or doc "")
                                                  :update-fn #',fn-name
                                                  :position ,position
                                                  :priority ,priority
                                                  :interval ,interval
                                                  :on-click ',on-click
                                                  :tooltip  ',tooltip)))
       ,name)))

(defun module-output (module)
  "Run MODULE's update function (respecting its interval) and return
its latest output.  An update function may return either a string
or a plist of the form (:text STRING :face FACE) where FACE is one
of :normal :accent :ok :warn :urgent :muted."
  (let* ((now  (get-internal-real-time))
         (per-sec internal-time-units-per-second)
         (elapsed (/ (- now (module-last-run module)) per-sec)))
    (if (or (null (module-last-output module))
            (>= elapsed (module-interval module)))
        (handler-case
            (let ((value (funcall (module-update-fn module))))
              (setf (module-last-output module) value
                    (module-last-run module) now)
              value)
          (error (c)
            (logmsg :warn "module ~a failed: ~a" (module-name module) c)
            (module-last-output module)))
        (module-last-output module))))

(defun module-output-text (value)
  "Return the display string for a module output VALUE, or NIL."
  (cond ((null value)    nil)
        ((stringp value) (and (plusp (length value)) value))
        ((consp value)
         (cond ((getf value :fragments)
                (format nil "~{~a~}"
                        (mapcar #'first (getf value :fragments))))
               (t (let ((s (getf value :text)))
                    (and s (plusp (length s)) s)))))))

(defun module-output-face (value)
  "Return the face keyword for VALUE, defaulting to :normal."
  (cond ((consp value) (or (getf value :face) :normal))
        (t :normal)))

(defun module-output-fragments (value)
  "Return a list of (TEXT FACE) pairs that represent VALUE.
A simple string yields one fragment with :normal face; a (:text X
:face Y) plist yields one fragment with face Y; a (:fragments
((T1 F1) (T2 F2) ...)) plist passes its list through verbatim."
  (cond ((null value) nil)
        ((stringp value) (list (list value :normal)))
        ((consp value)
         (or (getf value :fragments)
             (let ((s (getf value :text)))
               (and s (plusp (length s))
                    (list (list s (or (getf value :face) :normal)))))))))

(defun format-module (module)
  "Return MODULE's current display string, or NIL if it has no output."
  (module-output-text (module-output module)))

(defun resolve-tooltip (module)
  "Return MODULE's tooltip text right now, or NIL.
A tooltip may be a string, a function of no args, or a symbol naming
a function.  Handler errors are swallowed; tooltip just goes blank."
  (let ((spec (module-tooltip module)))
    (handler-case
        (cond
          ((null spec) nil)
          ((stringp spec) spec)
          ((functionp spec) (funcall spec))
          ((and (symbolp spec) (fboundp spec))
           (funcall (symbol-function spec)))
          (t nil))
      (error () nil))))

;;; ---- Click dispatch ----

(defvar *click-x* 0
  "Surface-x of the click currently being dispatched.  Bound around
calls to module on-click handlers so they can pinpoint sub-fragments
inside a wider module (e.g. the :tray pulls the specific tray item
under the cursor from this).")

(defvar *click-output* 0
  "Output index of the click currently being dispatched.")

(defun run-capture (program &rest args)
  "Run PROGRAM with ARGS, returning its stdout string on success, NIL on
non-zero exit or missing binary.  Exposed for user modules."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (cons program args)
                            :output :string :error-output nil
                            :ignore-error-status t)
        (declare (ignore err))
        (and (eql code 0) out))
    (error () nil)))

(defun executable-find-check (name)
  "Return T when NAME is on $PATH (a quick shell-out check).
Avoids relying on Lisp implementations to expose PATH lookup."
  (and (run-capture "sh" "-c"
                    (format nil "command -v ~a >/dev/null 2>&1 && echo y" name))
       t))

(defun button->key (button)
  "Map a button number to a keyword.
The C shim emits both Linux input-event-codes.h BTN_* values (272+)
for mouse buttons and X11-style 4/5 for scroll wheel directions, so
we handle both here."
  (case button
    (272 :left)            ; BTN_LEFT
    (273 :right)           ; BTN_RIGHT
    (274 :middle)          ; BTN_MIDDLE
    (275 :side)            ; BTN_SIDE
    (276 :extra)           ; BTN_EXTRA
    (4   :scroll-up)       ; wl_pointer.axis vertical < 0
    (5   :scroll-down)     ; wl_pointer.axis vertical > 0
    (t   :left)))

(defun module-action (module button)
  "Return the action attached to MODULE for the given mouse BUTTON.
BUTTON is a keyword (:left :right :middle ...).  Looks first at the
plist form ((:left ACTION) (:right ACTION) ...) then at the single
value form (which is treated as the :left action)."
  (let ((spec (module-on-click module)))
    (cond
      ((null spec) nil)
      ((and (listp spec)
            (every (lambda (e) (and (consp e) (keywordp (car e)))) spec))
       (cadr (assoc button spec)))
      ((eq button :left) spec)
      (t nil))))

(defun run-module-action (action module button output-index)
  "Execute ACTION.  Recognised shapes:

  NIL                   no-op
  STRING                shell command, run with `sh -c'
  LIST OF STRINGS       argv, run directly
  SYMBOL                resolved via SYMBOL-FUNCTION, called with
                        (MODULE BUTTON OUTPUT-INDEX)
  FUNCTION              called with (MODULE BUTTON OUTPUT-INDEX)"
  (handler-case
      (cond
        ((null action))
        ((stringp action)
         (uiop:launch-program (list "sh" "-c" action)))
        ((and (listp action) (every #'stringp action))
         (uiop:launch-program action))
        ((functionp action)
         (funcall action module button output-index))
        ((and (symbolp action) (fboundp action))
         (funcall (symbol-function action) module button output-index))
        (t
         (logmsg :warn "module ~a has unrecognised on-click action: ~s"
                 (module-name module) action)))
    (error (e)
      (logmsg :warn "module ~a click handler failed: ~a"
              (module-name module) e))))

(defvar *subfragment-handlers* (make-hash-table :test 'eql)
  "OUTPUT-IDX -> list of (X-START X-END HANDLER DATA).  Populated
by the Wayland renderer when it paints a `:clickable' fragment.
Hit-tested before the module-level on-click fires.")

(defun reset-subfragments (output-idx)
  (setf (gethash output-idx *subfragment-handlers*) nil))

(defun record-subfragment (output-idx x-start x-end handler data)
  (push (list x-start x-end handler data)
        (gethash output-idx *subfragment-handlers*)))

(defun subfragment-at-x (output-idx x)
  "Return (HANDLER DATA) for the sub-fragment containing X on
OUTPUT-IDX, or NIL."
  (loop for (s e handler data) in (gethash output-idx *subfragment-handlers*)
        when (and (>= x s) (<= x e))
        return (list handler data)))

(defun dispatch-module-click (module button output-index &optional (x 0))
  "Invoke an action for BUTTON.  Tries sub-fragment handlers first
(e.g. an individual workspace number, an individual tray item),
then falls back to the module-level `:on-click'."
  (let ((*click-x* x)
        (*click-output* output-index))
    (let ((sub (subfragment-at-x output-index x)))
      (cond
        (sub
         (destructuring-bind (handler data) sub
           (logmsg :debug "sub-click ~a on ~a -> ~s (data ~s)"
                   button (module-name module) handler data)
           (run-subfragment-action handler data module button output-index)))
        (t
         (let ((action (module-action module button)))
           (when action
             (logmsg :debug "click ~a on ~a (output ~d, x ~a)"
                     button (module-name module) output-index x)
             (run-module-action action module button output-index))))))))

(defun run-subfragment-action (handler data module button output-idx)
  "Invoke a sub-fragment HANDLER.  Recognised shapes:

  NIL                no-op
  STRING             shell command, run with `sh -c'
  LIST OF STRINGS    argv, run directly (DATA appended)
  SYMBOL or FUNCTION called with (DATA BUTTON MODULE OUTPUT-IDX)"
  (handler-case
      (cond
        ((null handler))
        ((stringp handler) (uiop:launch-program (list "sh" "-c" handler)))
        ((and (listp handler) (every #'stringp handler))
         (uiop:launch-program handler))
        ((functionp handler)
         (funcall handler data button module output-idx))
        ((and (symbolp handler) (fboundp handler))
         (funcall (symbol-function handler) data button module output-idx)))
    (error (e)
      (logmsg :warn "sub-fragment handler failed: ~a" e))))
