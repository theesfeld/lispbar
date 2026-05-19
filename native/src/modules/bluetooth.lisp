;;;; bluetooth.lisp  --  Adapter + connected-device count via bluetoothctl.

(in-package #:lispbar)

(defun bt-powered-p ()
  (let ((out (run-capture "bluetoothctl" "show")))
    (and out (search "Powered: yes" out) t)))

(defun bt-connected-count ()
  (let ((out (run-capture "bluetoothctl" "devices" "Connected")))
    (if out
        (count-if (lambda (line) (search "Device " line :end2 (min 7 (length line))))
                  (uiop:split-string out :separator '(#\Newline)))
        0)))

(defmodule :bluetooth (:doc "Adapter state and connection count."
                       :position :right :priority 50 :interval 10.0)
  (cond ((not (uiop:getenv "PATH")) nil)
        ((not (bt-powered-p)) "BT off")
        (t (let ((n (bt-connected-count)))
             (if (plusp n) (format nil "BT on/~d" n) "BT on")))))
