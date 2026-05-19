;;;; tools/registry-stub.lisp -- minimal :lispbar package mock used
;;;; by registry-validate.sh to byte-compile contributed modules
;;;; and themes without depending on the full binary.
;;;;
;;;; Stubs just enough that a well-formed (defmodule …) or
;;;; (define-theme …) loads without error.  Doesn't actually run
;;;; the modules; this is purely a syntax / undefined-symbol check.

(require :asdf)  ; brings UIOP

(defpackage #:lispbar
  (:use #:common-lisp)
  (:export #:defmodule #:define-theme
           #:run-capture #:executable-find-check #:logmsg
           #:lispbar-state-directory #:lispbar-cache-directory
           ;; module return values that user code constructs
           #:workspace #:workspace-name #:workspace-output #:workspace-focused))

(in-package #:lispbar)

(defmacro defmodule (name (&rest opts) &body body)
  (declare (ignore name opts body))
  '(values))

(defun define-theme (name &rest plist)
  (declare (ignore name plist))
  nil)

(defun run-capture            (&rest _) (declare (ignore _)) nil)
(defun executable-find-check  (_)       (declare (ignore _)) nil)
(defun logmsg                 (&rest _) (declare (ignore _)) nil)
(defun lispbar-state-directory ()       "/tmp/")
(defun lispbar-cache-directory ()       "/tmp/")
