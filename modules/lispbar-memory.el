;;; lispbar-memory.el --- Memory module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, frames

;;; Commentary:

;; Reports used/total RAM from /proc/meminfo.  Linux only; silently
;; produces nil elsewhere.

;;; Code:

(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-memory nil
  "Memory module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-memory-")

(defcustom lispbar-memory-format "MEM %d%%"
  "`format'-style string fed the integer used-percentage."
  :type 'string
  :group 'lispbar-memory)

(defcustom lispbar-memory-high-threshold 85
  "Percent above which the high-memory face is used."
  :type 'integer
  :group 'lispbar-memory)

(defcustom lispbar-memory-update-interval 10.0
  "Seconds between memory module refreshes."
  :type 'number
  :group 'lispbar-memory)

(defun lispbar-memory--read ()
  "Return a plist (:total KB :available KB) from /proc/meminfo, or nil."
  (when (file-readable-p "/proc/meminfo")
    (with-temp-buffer
      (insert-file-contents "/proc/meminfo")
      (let (total avail)
        (goto-char (point-min))
        (when (re-search-forward "^MemTotal:[ \t]+\\([0-9]+\\)" nil t)
          (setq total (string-to-number (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^MemAvailable:[ \t]+\\([0-9]+\\)" nil t)
          (setq avail (string-to-number (match-string 1))))
        (and total avail (list :total total :available avail))))))

(defun lispbar-memory-update ()
  "Return the rendered memory module string, or nil if unavailable."
  (when-let* ((m (lispbar-memory--read))
              (total (plist-get m :total))
              (avail (plist-get m :available))
              (used-pct (round (* 100.0 (/ (float (- total avail)) total)))))
    (propertize (format lispbar-memory-format used-pct)
                'face (if (>= used-pct lispbar-memory-high-threshold)
                          'lispbar-memory-high-face
                        'lispbar-memory-face))))

(lispbar-defmodule memory
  "Used-RAM percentage from /proc/meminfo."
  :update-fn #'lispbar-memory-update
  :update-interval lispbar-memory-update-interval
  :position 'right :priority 55)

(provide 'lispbar-memory)
;;; lispbar-memory.el ends here
