;;;; package.lisp  --  Public Common Lisp package for the Lispbar
;;;; binary.  User extension files (those dropped into
;;;; $XDG_CONFIG_HOME/lispbar/modules and themes) live in this
;;;; package, so every symbol they want to call is exported here.

(defpackage #:lispbar
  (:use #:common-lisp)
  (:export
   ;; Entry point
   #:main

   ;; XDG paths
   #:xdg-config-home #:xdg-data-home
   #:xdg-state-home  #:xdg-cache-home
   #:xdg-config-dirs #:xdg-data-dirs
   #:xdg-config-path #:xdg-data-path
   #:lispbar-config-file
   #:lispbar-extension-directories
   #:lispbar-state-directory
   #:lispbar-cache-directory

   ;; Config DSL
   #:placement #:theme #:output #:font #:height #:tick #:log-level
   #:load-config #:load-extensions
   #:*config* #:*loaded-extension-files*

   ;; Module registry
   #:defmodule
   #:register-module #:unregister-module
   #:make-module #:find-module-factory
   #:module-names
   #:module #:module-name #:module-doc
   #:module-update-fn
   #:module-position #:module-priority #:module-interval
   #:module-output #:module-output-text #:module-output-face
   #:module-output-fragments
   #:format-module

   ;; Theme registry
   #:define-theme #:apply-theme #:theme-color
   #:theme-names #:*theme-name* #:*theme-palette*

   ;; Helpers user modules typically want
   #:run-capture
   #:logmsg

   ;; Output drivers
   #:run-output #:*current-output*))
