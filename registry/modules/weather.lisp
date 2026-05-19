;;;; weather.lisp - OpenWeatherMap-style "current weather" via wttr.in.
;;;;
;;;; Drop into ~/.config/lispbar/modules/ or install via:
;;;;   lispbar registry install :weather

(in-package :lispbar)

(defvar *weather-location* nil
  "Location query for wttr.in.  NIL = auto-detect from IP.  Examples:
'Paris', 'Berlin', '~Eiffel+Tower', 'Munich,Germany', 'KSFO'.")

(defvar *weather-format* "%c%t"
  "wttr.in format string.  See https://wttr.in for the codes.")

(defun weather-update ()
  (let* ((loc (or *weather-location* ""))
         (url (format nil "wttr.in/~a?format=~a" loc *weather-format*))
         (out (run-capture "curl" "-fsSL" "--max-time" "5" url)))
    (when out
      (string-trim '(#\Space #\Tab #\Newline #\Return) out))))

(defmodule :weather
  (:doc "Current weather from wttr.in"
   :position :right :priority 35 :interval 600.0
   :on-click ((:left "xdg-open https://wttr.in"))
   :tooltip
   (lambda ()
     (let ((full (run-capture "curl" "-fsSL" "--max-time" "5"
                                "wttr.in/?format=4")))
       (and full (string-trim '(#\Space #\Tab #\Newline #\Return) full)))))
  (weather-update))
