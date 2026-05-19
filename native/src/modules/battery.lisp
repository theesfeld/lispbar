;;;; battery.lisp  --  Battery percentage and state from /sys/class/power_supply.

(in-package #:lispbar)

(defun battery-paths ()
  "Return a list of /sys/class/power_supply/BAT* directory pathnames."
  (when (uiop:directory-exists-p "/sys/class/power_supply/")
    (sort (remove-if-not
           (lambda (p)
             (search "BAT" (car (last (pathname-directory p)))))
           (uiop:subdirectories "/sys/class/power_supply/"))
          #'string<
          :key (lambda (p) (namestring p)))))

(defun read-sysfs-line (path)
  "Read the first line of PATH and strip whitespace, or NIL."
  (when (probe-file path)
    (with-open-file (s path :direction :input)
      (let ((line (read-line s nil nil)))
        (and line (string-trim '(#\Space #\Tab) line))))))

(defun battery-info (dir)
  "Return (PERCENT STATUS) for the battery under DIR, or NIL."
  (let ((cap    (read-sysfs-line (merge-pathnames "capacity" dir)))
        (status (read-sysfs-line (merge-pathnames "status"   dir))))
    (when (and cap status (every #'digit-char-p cap))
      (list (parse-integer cap) status))))

(defmodule :battery (:doc "Battery percent and charge state."
                     :position :right :priority 70 :interval 30.0)
  (let ((bats (battery-paths)))
    (when bats
      (let ((info (battery-info (first bats))))
        (when info
          (destructuring-bind (pct status) info
            (let ((tag (cond ((string= status "Charging")    "+")
                             ((string= status "Discharging") "-")
                             ((string= status "Full")        "=")
                             (t "?"))))
              (format nil "BAT ~a~d%" tag pct))))))))
