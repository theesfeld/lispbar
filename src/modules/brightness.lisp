;;;; brightness.lisp  --  Backlight percent via brightnessctl or sysfs.

(in-package #:lispbar)

(defun brightness-via-brightnessctl ()
  (let ((out (run-capture "brightnessctl" "-m" "info")))
    (when out
      ;; Format: device,class,current,percent,max
      (let ((parts (uiop:split-string out :separator '(#\,))))
        (when (>= (length parts) 4)
          (let* ((p (nth 3 parts))
                 (n (parse-integer p :junk-allowed t)))
            n))))))

(defun brightness-via-sysfs ()
  (let ((dir "/sys/class/backlight/"))
    (when (uiop:directory-exists-p dir)
      (let* ((bls (uiop:subdirectories dir))
             (bl  (first bls)))
        (when bl
          (let ((cur (read-sysfs-line (merge-pathnames "brightness" bl)))
                (max (read-sysfs-line (merge-pathnames "max_brightness" bl))))
            (when (and cur max)
              (let ((c (parse-integer cur :junk-allowed t))
                    (m (parse-integer max :junk-allowed t)))
                (when (and c m (plusp m))
                  (round (* 100 (/ c m))))))))))))

(defvar *brightness-step* 5
  "Brightness change per scroll tick (percentage points).")

(defvar *brightness-on-click* nil
  "Optional shell command for left-click of the brightness module.
NIL means no action (most users prefer scroll-wheel control).")

(defun brightness-scroll (_d button &rest _)
  (declare (ignore _ _d))
  (when (executable-find-check "brightnessctl")
    (let ((arg (case button
                 (:scroll-up   (format nil "+~d%" *brightness-step*))
                 (:scroll-down (format nil "~d%-" *brightness-step*)))))
      (when arg
        (handler-case
            (uiop:launch-program (list "brightnessctl" "set" arg))
          (error (e) (logmsg :warn "brightness scroll failed: ~a" e)))))))

(defun brightness-left-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *brightness-on-click* (plusp (length *brightness-on-click*)))
    (uiop:launch-program (list "sh" "-c" *brightness-on-click*))))

(defmodule :brightness
  (:doc "Backlight percentage.  Scroll to change."
   :position :right :priority 45 :interval 10.0
   :on-click ((:left        brightness-left-click)
              (:scroll-up   brightness-scroll)
              (:scroll-down brightness-scroll)))
  (let ((pct (or (brightness-via-brightnessctl)
                 (brightness-via-sysfs))))
    (and pct (format nil "BRT ~d%" pct))))
