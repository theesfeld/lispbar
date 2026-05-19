;;;; package.lisp  --  Common Lisp packages used by the native Lispbar binary.

(defpackage #:lispbar
  (:use #:common-lisp)
  (:export
   ;; Entry point
   #:main
   ;; Config DSL
   #:bar #:module-list #:placement #:theme #:output
   #:load-config
   ;; Module registry
   #:defmodule #:make-module #:register-module
   #:module-names #:find-module-factory
   #:module #:module-name #:module-update-fn
   #:module-position #:module-priority #:module-interval
   ;; Module API used by built-in modules
   #:module-output #:format-module
   ;; Output drivers
   #:run-output
   #:*current-output*
   ;; Logging
   #:logmsg))
