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

(defmodule :brightness (:doc "Backlight percentage."
                        :position :right :priority 45 :interval 10.0)
  (let ((pct (or (brightness-via-brightnessctl)
                 (brightness-via-sysfs))))
    (and pct (format nil "BRT ~d%" pct))))
