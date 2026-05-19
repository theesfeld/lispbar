;;; lispbar-tray.el --- StatusNotifierItem (SNI) tray module -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tray, frames

;;; Commentary:

;; A pure-Elisp system tray host built on the FreeDesktop
;; StatusNotifierItem D-Bus protocol.  This is the protocol modern
;; tray applets (NetworkManager, Steam, Discord, KeePassXC, ...) use
;; on both X11 and Wayland.
;;
;; Because Lispbar renders text, icons aren't displayed; instead each
;; item is shown as its short title or tooltip.  Click an item (or use
;; M-x lispbar-tray-activate) to send `Activate' to the underlying
;; application.
;;
;; Implementation notes:
;;
;;   * We register on the well-known name org.kde.StatusNotifierWatcher
;;     and answer RegisterStatusNotifierItem from clients.
;;   * Existing items already on the bus are discovered by walking
;;     dbus-list-names for "org.kde.StatusNotifierItem-*" services and
;;     for any name listed in the Watcher's RegisteredStatusNotifierItems
;;     property when another tray is running.

;;; Code:

(require 'cl-lib)
(require 'dbus)
(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-tray nil
  "StatusNotifierItem tray module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-tray-")

(defcustom lispbar-tray-separator " "
  "String inserted between tray entries."
  :type 'string
  :group 'lispbar-tray)

(defcustom lispbar-tray-format "[%s]"
  "`format'-style string fed each item's short label."
  :type 'string
  :group 'lispbar-tray)

(defcustom lispbar-tray-update-interval 5.0
  "Seconds between tray polls (also acts as a safety re-scan)."
  :type 'number
  :group 'lispbar-tray)

(defcustom lispbar-tray-register-as-watcher t
  "Register Lispbar as the org.kde.StatusNotifierWatcher.
Disable if another tray (e.g. waybar) is already providing one."
  :type 'boolean
  :group 'lispbar-tray)

(defvar lispbar-tray--items nil
  "List of currently known tray bus-names.")

(defvar lispbar-tray--watcher-token nil
  "Token returned by `dbus-register-method' for the Watcher service.")

;;; Item discovery -------------------------------------------------

(defun lispbar-tray--scan-bus ()
  "Discover SNI items by scanning the session bus."
  (ignore-errors
    (cl-remove-if-not
     (lambda (n)
       (or (string-prefix-p "org.kde.StatusNotifierItem-" n)
           (string-prefix-p "org.freedesktop.StatusNotifierItem-" n)))
     (dbus-list-names :session))))

(defun lispbar-tray--label-for (bus)
  "Return a short text label for tray BUS, or nil."
  (or (ignore-errors
        (dbus-get-property :session bus
                           "/StatusNotifierItem"
                           "org.kde.StatusNotifierItem" "Title"))
      (ignore-errors
        (let ((tt (dbus-get-property :session bus
                                     "/StatusNotifierItem"
                                     "org.kde.StatusNotifierItem"
                                     "ToolTip")))
          ;; ToolTip is a struct (s,a(iiay),s,s); we want field 2 (title).
          (and (listp tt) (nth 2 tt))))
      (ignore-errors
        (dbus-get-property :session bus
                           "/StatusNotifierItem"
                           "org.kde.StatusNotifierItem" "Id"))))

;;; Watcher service ------------------------------------------------

(defun lispbar-tray--register-watcher ()
  "Register Lispbar as org.kde.StatusNotifierWatcher, if available."
  (when (and lispbar-tray-register-as-watcher
             (not lispbar-tray--watcher-token)
             (ignore-errors
               (dbus-register-service :session "org.kde.StatusNotifierWatcher"
                                      :do-not-queue)))
    (setq lispbar-tray--watcher-token
          (dbus-register-method
           :session "org.kde.StatusNotifierWatcher"
           "/StatusNotifierWatcher"
           "org.kde.StatusNotifierWatcher"
           "RegisterStatusNotifierItem"
           (lambda (service)
             (cl-pushnew service lispbar-tray--items :test #'string=)
             :ignore)))))

(defun lispbar-tray--unregister-watcher ()
  "Release the Watcher service if we own it."
  (when lispbar-tray--watcher-token
    (ignore-errors
      (dbus-unregister-object lispbar-tray--watcher-token))
    (setq lispbar-tray--watcher-token nil)
    (ignore-errors
      (dbus-unregister-service :session "org.kde.StatusNotifierWatcher"))))

;;; Module API -----------------------------------------------------

(defun lispbar-tray-refresh ()
  "Refresh `lispbar-tray--items' from D-Bus."
  (setq lispbar-tray--items
        (cl-delete-duplicates
         (append (lispbar-tray--scan-bus) lispbar-tray--items)
         :test #'string=))
  ;; Drop names that vanished:
  (let ((live (dbus-list-names :session)))
    (setq lispbar-tray--items
          (cl-remove-if-not (lambda (n) (member n live))
                            lispbar-tray--items))))

(defun lispbar-tray-update ()
  "Render the tray module string."
  (lispbar-tray--register-watcher)
  (lispbar-tray-refresh)
  (let ((labels (delq nil (mapcar #'lispbar-tray--label-for
                                   lispbar-tray--items))))
    (when labels
      (mapconcat
       (lambda (lbl)
         (propertize (format lispbar-tray-format lbl)
                     'face 'lispbar-tray-face))
       labels
       lispbar-tray-separator))))

;;;###autoload
(defun lispbar-tray-activate (bus)
  "Send `Activate' to tray service BUS."
  (interactive
   (list (completing-read "Tray item: " lispbar-tray--items nil t)))
  (ignore-errors
    (dbus-call-method :session bus "/StatusNotifierItem"
                      "org.kde.StatusNotifierItem" "Activate" 0 0)))

;;;###autoload
(defun lispbar-tray-context-menu (bus)
  "Open BUS's context menu (best-effort, app-dependent)."
  (interactive
   (list (completing-read "Tray item: " lispbar-tray--items nil t)))
  (ignore-errors
    (dbus-call-method :session bus "/StatusNotifierItem"
                      "org.kde.StatusNotifierItem" "ContextMenu" 0 0)))

(lispbar-defmodule tray
  "FreeDesktop StatusNotifierItem (SNI) text tray."
  :update-fn #'lispbar-tray-update
  :update-interval lispbar-tray-update-interval
  :position 'right :priority 30)

(provide 'lispbar-tray)
;;; lispbar-tray.el ends here
