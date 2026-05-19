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
   #:position
   #:load-config #:load-extensions
   #:seed-user-config #:*default-config-source* #:*seed-disabled*
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

   ;; Per-module tunables (set via `setf' from config.lisp)
   #:*clock-format*
   #:*media-format* #:*media-max-length*
   #:*workspaces-scope* #:*workspaces-brackets*
   #:*workspaces-separator* #:*workspaces-empty-text*
   #:workspace #:workspace-name #:workspace-output #:workspace-focused
   #:*network-format-wifi* #:*network-format-ethernet*
   #:*network-format-down* #:*network-show-bars* #:*network-bar-glyphs*
   #:*network-on-click*
   #:*audio-on-click* #:*audio-on-middle-click*
   #:*bluetooth-on-click* #:*bluetooth-on-middle-click*
   #:*launcher-label* #:*launcher-command* #:*launcher-on-right-click*

   ;; Click dispatch (for users writing their own modules)
   #:module-on-click #:dispatch-module-click #:run-module-action
   #:button->key #:*click-x* #:*click-output*
   #:record-subfragment #:reset-subfragments
   #:run-subfragment-action #:subfragment-at-x

   ;; Tooltips
   #:module-tooltip #:resolve-tooltip
   #:*wayland-tooltip-bg* #:*wayland-tooltip-padding-x*
   #:*wayland-tooltip-padding-y* #:*wayland-tooltip-corner*

   ;; Tray
   #:*tray-icon-size* #:*tray-show-text-when-no-icon* #:*tray-poll-interval*

   ;; Output drivers
   #:run-output #:*current-output*))
