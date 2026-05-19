;;; lispbar-cpu.el --- CPU module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, frames

;;; Commentary:

;; Shows CPU load average from /proc/loadavg on Linux.  Falls back to
;; `load-average' on other systems.  Use it via:
;;
;;   (require 'lispbar-cpu)
;;   (add-to-list 'lispbar-default-modules 'cpu)

;;; Code:

(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-cpu nil
  "CPU module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-cpu-")

(defcustom lispbar-cpu-format "CPU %.2f"
  "`format'-style string fed the 1-minute load average."
  :type 'string
  :group 'lispbar-cpu)

(defcustom lispbar-cpu-high-threshold nil
  "Load above which the high-load face is used.
Defaults to the number of logical CPUs."
  :type '(choice (const :tag "Auto (= number of CPUs)" nil)
                 (number :tag "Manual threshold"))
  :group 'lispbar-cpu)

(defcustom lispbar-cpu-update-interval 5.0
  "Seconds between CPU module refreshes."
  :type 'number
  :group 'lispbar-cpu)

(defun lispbar-cpu--num-cpus ()
  "Return the number of logical CPUs, or 1 if it cannot be determined."
  (or (and (file-readable-p "/proc/cpuinfo")
           (with-temp-buffer
             (insert-file-contents "/proc/cpuinfo")
             (let ((n 0))
               (while (re-search-forward "^processor[ \t]*:" nil t)
                 (cl-incf n))
               (and (> n 0) n))))
      (and (fboundp 'num-processors) (num-processors))
      1))

(defun lispbar-cpu--read-load ()
  "Return the 1-minute load average as a float, or nil."
  (cond
   ((file-readable-p "/proc/loadavg")
    (with-temp-buffer
      (insert-file-contents "/proc/loadavg")
      (string-to-number (car (split-string (buffer-string))))))
   ((fboundp 'load-average)
    (/ (car (load-average)) 100.0))))

(defun lispbar-cpu-update ()
  "Return the rendered CPU module string, or nil if unavailable."
  (when-let* ((load (lispbar-cpu--read-load)))
    (let* ((threshold (or lispbar-cpu-high-threshold
                          (lispbar-cpu--num-cpus)))
           (face (if (>= load threshold)
                     'lispbar-cpu-high-face
                   'lispbar-cpu-face)))
      (propertize (format lispbar-cpu-format load) 'face face))))

(lispbar-defmodule cpu
  "1-minute CPU load average from /proc/loadavg."
  :update-fn #'lispbar-cpu-update
  :update-interval lispbar-cpu-update-interval
  :position 'right :priority 60)

(provide 'lispbar-cpu)
;;; lispbar-cpu.el ends here
