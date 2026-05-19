;;;; workspaces.lisp  --  Active workspace list, driven by the compositor.
;;;;
;;;; Auto-detects which compositor is running and shells out to the
;;;; matching IPC tool:
;;;;
;;;;   Sway        $SWAYSOCK set         -> swaymsg -t get_workspaces -r
;;;;   Hyprland    $HYPRLAND_INSTANCE_*  -> hyprctl -j workspaces  +
;;;;                                        hyprctl -j activeworkspace
;;;;
;;;; Returns a string like  "1 [2] 3 4"  where the bracketed entry is
;;;; the focused workspace.  Customise the brackets / separator via
;;;; the variables below.

(in-package #:lispbar)

(defvar *workspaces-brackets* '("[" . "]")
  "Cons of (OPEN . CLOSE) wrapping the focused workspace.")

(defvar *workspaces-separator* " "
  "String inserted between workspace names.")

;;; ---- Tiny JSON token reader (good enough for workspace records) ----
;;;
;;; We only need a handful of fields per workspace.  Rather than pull
;;; in cl-json, scan the JSON text for the specific tokens we want.
;;; This is robust against most reasonable formatting but does NOT
;;; understand backslash-escaped quotes inside names; compositors
;;; don't emit those for workspace names in practice.

(defun skip-ws (text pos)
  "Return the index of the first non-whitespace char at or after POS."
  (loop while (and (< pos (length text))
                   (member (char text pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun find-value-start (text key)
  "Return the index of the value following \"KEY\": in TEXT, or NIL.
Tolerates arbitrary whitespace around the colon."
  (let* ((needle (format nil "\"~a\"" key))
         (start  (search needle text)))
    (when start
      (let ((p (skip-ws text (+ start (length needle)))))
        (when (and (< p (length text)) (char= (char text p) #\:))
          (skip-ws text (1+ p)))))))

(defun json-string-value (text key)
  "Return the string value of KEY in TEXT, or NIL."
  (let ((p (find-value-start text key)))
    (when (and p (< p (length text)) (char= (char text p) #\"))
      (let ((end (position #\" text :start (1+ p))))
        (and end (subseq text (1+ p) end))))))

(defun json-number-value (text key)
  "Return the integer value of KEY in TEXT, or NIL."
  (let ((p (find-value-start text key)))
    (when p (parse-integer text :start p :junk-allowed t))))

(defun json-bool-value (text key)
  "Return T when KEY's value is the literal `true' in TEXT."
  (let ((p (find-value-start text key)))
    (and p (<= (+ p 4) (length text))
         (string= text "true" :start1 p :end1 (+ p 4)))))

(defun split-json-objects (text)
  "Slice TEXT (a JSON array of objects) into a list of object strings.
Splits on top-level brace boundaries; ignores braces inside strings."
  (let ((result nil) (depth 0) (start nil) (in-string nil))
    (loop for i from 0 below (length text)
          for c = (char text i) do
            (cond
              ((and (char= c #\") (or (zerop i) (char/= (char text (1- i)) #\\)))
               (setf in-string (not in-string)))
              (in-string)
              ((char= c #\{)
               (when (zerop depth) (setf start i))
               (incf depth))
              ((char= c #\})
               (decf depth)
               (when (zerop depth)
                 (push (subseq text start (1+ i)) result)
                 (setf start nil)))))
    (nreverse result)))

;;; ---- Compositor adapters ----

(defun workspaces-sway ()
  "Return (NAMES FOCUSED-NAME) for Sway, or NIL on failure."
  (let ((out (run-capture "swaymsg" "-t" "get_workspaces" "-r")))
    (when out
      (let (names focused)
        (dolist (obj (split-json-objects out))
          (let ((name (json-string-value obj "name")))
            (when name
              (push name names)
              (when (json-bool-value obj "focused")
                (setf focused name)))))
        (when names (list (nreverse names) focused))))))

(defun workspaces-hyprland ()
  "Return (NAMES FOCUSED-NAME) for Hyprland, or NIL on failure."
  (let ((all    (run-capture "hyprctl" "-j" "workspaces"))
        (active (run-capture "hyprctl" "-j" "activeworkspace")))
    (when (and all active)
      (let ((focused (json-string-value active "name"))
            (entries (split-json-objects all))
            names ids)
        ;; Sort by id so workspaces appear in numeric order.
        (dolist (e entries)
          (let ((id (json-number-value e "id"))
                (n  (json-string-value e "name")))
            (when (and id n)
              (push (cons id n) ids))))
        (setf names (mapcar #'cdr (sort ids #'< :key #'car)))
        (and names (list names focused))))))

(defun detect-workspaces-source ()
  "Return :sway, :hyprland, or NIL based on env vars."
  (cond
    ((uiop:getenv "SWAYSOCK")                  :sway)
    ((uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE") :hyprland)))

(defun workspaces-fetch ()
  "Return (NAMES FOCUSED) for the active compositor, or NIL."
  (case (detect-workspaces-source)
    (:sway     (workspaces-sway))
    (:hyprland (workspaces-hyprland))))

;;; ---- The module ----

(defun render-workspaces (names focused)
  "Render NAMES as a space-separated string, wrapping FOCUSED in brackets."
  (with-output-to-string (out)
    (loop for first = t then nil
          for n in names
          do (unless first (write-string *workspaces-separator* out))
             (cond
               ((string= n focused)
                (write-string (car *workspaces-brackets*) out)
                (write-string n out)
                (write-string (cdr *workspaces-brackets*) out))
               (t (write-string n out))))))

(defmodule :workspaces (:doc "Active workspace list (Sway / Hyprland)."
                        :position :left :priority 80 :interval 0.5)
  (let ((data (workspaces-fetch)))
    (and data (render-workspaces (first data) (second data)))))
