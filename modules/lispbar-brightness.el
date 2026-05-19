;;; lispbar-brightness.el --- Display brightness module -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, frames

;;; Commentary:

;; Reports backlight brightness as a percentage.  Tries, in order:
;;
;;   1. brightnessctl (preferred - no privileges required)
;;   2. /sys/class/backlight/*/brightness + max_brightness
;;
;; Works under both X11 and Wayland.

;;; Code:

(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-brightness nil
  "Brightness module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-brightness-")

(defcustom lispbar-brightness-format "BRT %d%%"
  "`format'-style string fed the integer brightness percentage."
  :type 'string
  :group 'lispbar-brightness)

(defcustom lispbar-brightness-update-interval 10.0
  "Seconds between brightness refreshes."
  :type 'number
  :group 'lispbar-brightness)

(defun lispbar-brightness--via-brightnessctl ()
  "Return brightness percent via brightnessctl, or nil."
  (when (executable-find "brightnessctl")
    (with-temp-buffer
      (when (eq 0 (call-process "brightnessctl" nil t nil "-m" "info"))
        (goto-char (point-min))
        (when (re-search-forward "\\([0-9]+\\)%" nil t)
          (string-to-number (match-string 1)))))))

(defun lispbar-brightness--via-sysfs ()
  "Return brightness percent from /sys/class/backlight, or nil."
  (let ((dirs (and (file-directory-p "/sys/class/backlight")
                   (directory-files "/sys/class/backlight" t
                                    "\\`[^.]" t))))
    (catch 'done
      (dolist (d dirs)
        (let ((cur (expand-file-name "brightness" d))
              (max (expand-file-name "max_brightness" d)))
          (when (and (file-readable-p cur) (file-readable-p max))
            (let ((c (with-temp-buffer (insert-file-contents cur)
                                       (string-to-number (buffer-string))))
                  (m (with-temp-buffer (insert-file-contents max)
                                       (string-to-number (buffer-string)))))
              (when (> m 0)
                (throw 'done (round (* 100.0 (/ (float c) m))))))))))))

(defun lispbar-brightness-update ()
  "Return the rendered brightness string, or nil if not detected."
  (when-let* ((pct (or (lispbar-brightness--via-brightnessctl)
                       (lispbar-brightness--via-sysfs))))
    (propertize (format lispbar-brightness-format pct)
                'face 'lispbar-brightness-face)))

(lispbar-defmodule brightness
  "Backlight brightness percent."
  :update-fn #'lispbar-brightness-update
  :update-interval lispbar-brightness-update-interval
  :position 'right :priority 45)

(provide 'lispbar-brightness)
;;; lispbar-brightness.el ends here
