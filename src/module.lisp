;;;; module.lisp  --  Module data structure, registry and `defmodule' DSL.

(in-package #:lispbar)

(defclass module ()
  ((name      :initarg :name      :reader module-name)
   (doc       :initarg :doc       :reader module-doc       :initform "")
   (update-fn :initarg :update-fn :reader module-update-fn)
   (position  :initarg :position  :accessor module-position :initform :right)
   (priority  :initarg :priority  :accessor module-priority :initform 50)
   (interval  :initarg :interval  :accessor module-interval :initform 5.0)
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

(defmacro defmodule (name (&key doc (position :right) (priority 50) (interval 5.0))
                     &body body)
  "Declare a module NAME (a keyword).
BODY is the update function body and must return a string (or NIL).
The module is registered eagerly under NAME and a fresh instance is
created on each `make-module' call.

Example:

  (defmodule :clock (:doc \"24h clock.\" :position :right :interval 1.0)
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
                                                  :interval ,interval)))
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
