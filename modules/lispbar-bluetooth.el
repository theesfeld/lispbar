;;; lispbar-bluetooth.el --- Bluetooth module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, frames

;;; Commentary:

;; Reports the Bluetooth adapter state and number of connected
;; devices.  Uses `bluetoothctl' (part of BlueZ) which works on every
;; Linux desktop, X11 or Wayland.
;;
;; Click the module (or call M-x lispbar-bluetooth-toggle) to power
;; the adapter on/off.

;;; Code:

(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-bluetooth nil
  "Bluetooth module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-bluetooth-")

(defcustom lispbar-bluetooth-format "BT %s"
  "`format'-style string fed a short status string.
Examples passed in: \"on\", \"off\", \"on/2\"."
  :type 'string
  :group 'lispbar-bluetooth)

(defcustom lispbar-bluetooth-update-interval 10.0
  "Seconds between Bluetooth polls."
  :type 'number
  :group 'lispbar-bluetooth)

(defun lispbar-bluetooth--powered-p ()
  "Return non-nil when the default Bluetooth adapter is powered."
  (when (executable-find "bluetoothctl")
    (with-temp-buffer
      (when (eq 0 (call-process "bluetoothctl" nil t nil "show"))
        (goto-char (point-min))
        (and (re-search-forward "Powered: yes" nil t) t)))))

(defun lispbar-bluetooth--connected-count ()
  "Return how many Bluetooth devices are currently connected."
  (or (when (executable-find "bluetoothctl")
        (with-temp-buffer
          (when (eq 0 (call-process "bluetoothctl" nil t nil "devices" "Connected"))
            (goto-char (point-min))
            (count-matches "^Device " (point-min) (point-max)))))
      0))

(defun lispbar-bluetooth-update ()
  "Render the Bluetooth module string, or nil if bluetoothctl is missing."
  (when (executable-find "bluetoothctl")
    (let* ((powered (lispbar-bluetooth--powered-p))
           (count (and powered (lispbar-bluetooth--connected-count)))
           (status (cond ((not powered) "off")
                         ((and count (> count 0)) (format "on/%d" count))
                         (t "on")))
           (face (cond ((not powered) 'lispbar-inactive-face)
                       ((and count (> count 0)) 'lispbar-bluetooth-connected-face)
                       (t 'lispbar-bluetooth-face))))
      (propertize (format lispbar-bluetooth-format status) 'face face))))

;;;###autoload
(defun lispbar-bluetooth-toggle ()
  "Toggle the default Bluetooth adapter power."
  (interactive)
  (when (executable-find "bluetoothctl")
    (call-process "bluetoothctl" nil nil nil
                  "power" (if (lispbar-bluetooth--powered-p) "off" "on"))
    (when (fboundp 'lispbar-modules-update)
      (lispbar-modules-update 'bluetooth))))

(lispbar-defmodule bluetooth
  "Bluetooth adapter state via bluetoothctl."
  :update-fn #'lispbar-bluetooth-update
  :update-interval lispbar-bluetooth-update-interval
  :position 'right :priority 50)

(provide 'lispbar-bluetooth)
;;; lispbar-bluetooth.el ends here
