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

(defvar *bluetooth-on-click* "blueman-manager || blueberry"
  "Shell command for left-click on the bluetooth module.  NIL to disable.")
(defvar *bluetooth-on-middle-click* "bluetoothctl power toggle"
  "Shell command for middle-click - toggle adapter power.")

(defun bluetooth-left-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *bluetooth-on-click* (plusp (length *bluetooth-on-click*)))
    (uiop:launch-program (list "sh" "-c" *bluetooth-on-click*))))

(defun bluetooth-middle-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *bluetooth-on-middle-click*
             (plusp (length *bluetooth-on-middle-click*)))
    (uiop:launch-program (list "sh" "-c" *bluetooth-on-middle-click*))))

(defun bluetooth-tooltip ()
  (cond
    ((not (executable-find-check "bluetoothctl")) "bluetoothctl not installed")
    ((not (bt-powered-p)) "Bluetooth off - middle-click to power on")
    (t (let ((n (bt-connected-count)))
         (cond
           ((zerop n) "Bluetooth on - no devices connected")
           (t (format nil "Bluetooth on - ~d device~:p connected" n)))))))

(defun executable-find-check (name)
  (and (run-capture "sh" "-c" (format nil "command -v ~a >/dev/null 2>&1 && echo y" name)) t))

(defmodule :bluetooth (:doc "Adapter state and connection count."
                       :position :right :priority 50 :interval 10.0
                       :on-click ((:left   bluetooth-left-click)
                                  (:middle bluetooth-middle-click))
                       :tooltip  bluetooth-tooltip)
  (cond ((not (bt-powered-p))
         (list :text "BT off" :face :muted))
        (t
         (let ((n (bt-connected-count)))
           (if (plusp n)
               (list :text (format nil "BT on/~d" n) :face :ok)
               (list :text "BT on" :face :normal))))))
