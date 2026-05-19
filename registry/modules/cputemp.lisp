;;;; cputemp.lisp - CPU package temperature via lm_sensors.

(in-package :lispbar)

(defvar *cputemp-chip* "coretemp-isa-0000"
  "lm_sensors chip name.  Try `sensors' to see what your CPU
exports.  Common values: `coretemp-isa-0000' (Intel),
`k10temp-pci-00c3' (AMD), `cpu_thermal-virtual-0' (ARM).")

(defvar *cputemp-feature* "Package id 0"
  "Feature within the chip.  `sensors -u CHIP' lists them.")

(defun cputemp-c ()
  "Return the integer °C for the configured chip/feature, or NIL."
  (let ((out (run-capture "sensors" "-uA" *cputemp-chip*)))
    (when out
      (let ((tail (search *cputemp-feature* out)))
        (when tail
          (let ((m (search "_input:" out :start2 tail)))
            (when m
              (parse-integer (subseq out (+ m 7)) :junk-allowed t))))))))

(defmodule :cputemp
  (:doc "CPU package temperature in °C"
   :position :right :priority 56 :interval 5.0
   :tooltip
   (lambda ()
     (let ((out (run-capture "sensors" *cputemp-chip*)))
       (and out (string-trim '(#\Space #\Newline) out)))))
  (let ((c (cputemp-c)))
    (when c
      (list :text (format nil "~d°C" c)
            :face (cond ((>= c 85) :urgent)
                        ((>= c 70) :warn)
                        (t         :normal))))))
