;;;; audio.lisp  --  Volume / mute via wpctl > pactl > amixer cascade.

(in-package #:lispbar)

(defun run-capture (program &rest args)
  "Run PROGRAM with ARGS, returning its stdout string on success, NIL on
non-zero exit or missing binary."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program (cons program args)
                            :output :string :error-output nil
                            :ignore-error-status t)
        (declare (ignore err))
        (and (eql code 0) out))
    (error () nil)))

(defun audio-via-wpctl ()
  (let ((out (run-capture "wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")))
    (when out
      (let* ((tokens (uiop:split-string out :separator '(#\Space #\Newline)))
             (num    (find-if (lambda (s) (find #\. s)) tokens))
             (volume (and num (round (* 100 (parse-float num))))))
        (when volume
          (list :volume volume
                :muted  (search "MUTED" out)))))))

(defun parse-float (s)
  "Parse S as a float (e.g. \"0.55\")."
  (with-input-from-string (in s)
    (let ((*read-default-float-format* 'double-float))
      (coerce (read in) 'double-float))))

(defun audio-via-pactl ()
  (let ((vol-out (run-capture "pactl" "get-sink-volume" "@DEFAULT_SINK@"))
        (mute-out (run-capture "pactl" "get-sink-mute" "@DEFAULT_SINK@")))
    (when vol-out
      (let* ((pct (loop for c across vol-out
                        with digits = nil
                        when (or (digit-char-p c) (char= c #\%))
                          do (push c digits)
                        finally (return (coerce (nreverse digits) 'string))))
             (volume (when (find #\% pct)
                       (parse-integer pct :junk-allowed t))))
        (when volume
          (list :volume volume
                :muted (and mute-out (search "yes" mute-out))))))))

(defvar *audio-on-click* "pavucontrol || pwvucontrol"
  "Shell command for left-click on the audio module.  NIL to disable.")
(defvar *audio-on-middle-click* "pactl set-sink-mute @DEFAULT_SINK@ toggle"
  "Shell command for middle-click - toggle mute by default.")

(defun audio-left-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *audio-on-click* (plusp (length *audio-on-click*)))
    (uiop:launch-program (list "sh" "-c" *audio-on-click*))))

(defun audio-middle-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *audio-on-middle-click* (plusp (length *audio-on-middle-click*)))
    (uiop:launch-program (list "sh" "-c" *audio-on-middle-click*))))

(defmodule :audio (:doc "Volume + mute state of default sink."
                   :position :right :priority 65 :interval 2.0
                   :on-click ((:left   audio-left-click)
                              (:middle audio-middle-click)))
  (let ((s (or (audio-via-wpctl) (audio-via-pactl))))
    (when s
      (if (getf s :muted)
          (list :text "MUTE" :face :muted)
          (let* ((vol (getf s :volume))
                 (face (cond ((> vol 100) :warn)
                             (t           :normal))))
            (list :text (format nil "VOL ~d%" vol) :face face))))))
