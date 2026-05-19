;;;; launcher.lisp  --  Click-to-spawn launcher button.
;;;;
;;;; A tiny module whose only job is to be clicked.  Its :on-click
;;;; action shells out to whatever launcher you actually want
;;;; (wofi, fuzzel, rofi, anything that can be exec'd).  The label
;;;; is just a hint that something will happen.

(in-package #:lispbar)

(defvar *launcher-label* " "
  "Text shown for the :launcher module.  Defaults to a Unicode
hamburger; override with whatever icon font your bar can render.")

(defvar *launcher-command* "wofi --show drun || fuzzel || rofi -show drun"
  "Shell command run on left-click.  The default tries the popular
launchers in succession; override with the single command you use:

  (setf *launcher-command* \"fuzzel\")
  (setf *launcher-command* \"rofi -show drun -theme catppuccin\")")

(defvar *launcher-on-right-click* "wlogout || swaynag -m 'logout?'"
  "Optional shell command run on right-click.  Set to NIL to
disable.")

(defun launcher-left-click  (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *launcher-command* (plusp (length *launcher-command*)))
    (uiop:launch-program (list "sh" "-c" *launcher-command*))))

(defun launcher-right-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *launcher-on-right-click*
             (plusp (length *launcher-on-right-click*)))
    (uiop:launch-program (list "sh" "-c" *launcher-on-right-click*))))

(defmodule :launcher
  (:doc "Click to open your launcher (wofi / fuzzel / rofi)."
   :position :left :priority 99 :interval 60.0
   :on-click ((:left  launcher-left-click)
              (:right launcher-right-click)))
  (list :text *launcher-label* :face :accent))
