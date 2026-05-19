;;;; pomodoro.lisp - Click-driven 25/5 minute pomodoro timer.
;;;;
;;;; Hidden when idle.  Left-click starts a 25-minute work block;
;;;; another left-click toggles pause; right-click resets.  Auto
;;;; rolls into a 5-minute break, then back to work.

(in-package :lispbar)

(defvar *pomodoro-work-minutes*  25)
(defvar *pomodoro-break-minutes* 5)
(defvar *pomodoro-notify-cmd*    "notify-send"
  "Command run when a phase ends.  NIL to disable notifications.")

(defstruct pomo
  (phase :idle)                          ; :idle :work :break :paused
  (started-at nil)                       ; universal time
  (remaining 0)                          ; seconds left in current phase
  (was-phase :work))                     ; what phase to resume on unpause

(defvar *pomo* (make-pomo))

(defun pomo-tick ()
  "Recompute remaining time.  Called every render."
  (case (pomo-phase *pomo*)
    ((:work :break)
     (let* ((now      (get-universal-time))
            (elapsed  (- now (pomo-started-at *pomo*)))
            (full     (* 60 (if (eq (pomo-phase *pomo*) :work)
                                *pomodoro-work-minutes*
                                *pomodoro-break-minutes*)))
            (left     (- full elapsed)))
       (setf (pomo-remaining *pomo*) left)
       (when (<= left 0)
         (pomo-phase-ended))))))

(defun pomo-notify (msg)
  (when (and *pomodoro-notify-cmd* (plusp (length *pomodoro-notify-cmd*)))
    (ignore-errors (uiop:launch-program (list *pomodoro-notify-cmd*
                                                "Pomodoro" msg)))))

(defun pomo-phase-ended ()
  (case (pomo-phase *pomo*)
    (:work  (pomo-notify "Work block done - 5 min break")
            (setf (pomo-phase *pomo*) :break
                  (pomo-started-at *pomo*) (get-universal-time)))
    (:break (pomo-notify "Break over - back to work")
            (setf (pomo-phase *pomo*) :work
                  (pomo-started-at *pomo*) (get-universal-time)))))

(defun pomodoro-left-click (_m _b _i) (declare (ignore _m _b _i))
  (case (pomo-phase *pomo*)
    (:idle   (setf (pomo-phase *pomo*) :work
                    (pomo-started-at *pomo*) (get-universal-time)
                    (pomo-was-phase *pomo*) :work))
    (:paused (setf (pomo-phase *pomo*) (pomo-was-phase *pomo*)))
    (t       (setf (pomo-was-phase *pomo*) (pomo-phase *pomo*)
                    (pomo-phase *pomo*) :paused))))

(defun pomodoro-right-click (_m _b _i) (declare (ignore _m _b _i))
  (setf *pomo* (make-pomo)))

(defun pomodoro-fmt (secs)
  (let* ((s (max 0 (round secs)))
         (m (floor s 60))
         (r (mod   s 60)))
    (format nil "~d:~2,'0d" m r)))

(defmodule :pomodoro
  (:doc "25/5 pomodoro timer; click to start, right-click to reset"
   :position :center :priority 95 :interval 1.0
   :on-click ((:left   pomodoro-left-click)
              (:right  pomodoro-right-click))
   :tooltip
   (lambda ()
     (case (pomo-phase *pomo*)
       (:idle   "Click to start a 25-minute work block")
       (:work   "Working - click to pause, right-click to reset")
       (:break  "On break - click to pause, right-click to reset")
       (:paused "Paused - click to resume, right-click to reset"))))
  (pomo-tick)
  (case (pomo-phase *pomo*)
    (:idle   nil)
    (:work   (list :text (format nil " ~a" (pomodoro-fmt (pomo-remaining *pomo*)))
                   :face :accent))
    (:break  (list :text (format nil " ~a" (pomodoro-fmt (pomo-remaining *pomo*)))
                   :face :ok))
    (:paused (list :text (format nil "⏸ ~a" (pomodoro-fmt (pomo-remaining *pomo*)))
                   :face :muted))))
