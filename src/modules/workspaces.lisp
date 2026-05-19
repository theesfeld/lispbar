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

(defun workspaces-niri ()
  "Return (NAMES FOCUSED-NAME) for niri, or NIL on failure.

niri exposes a JSON IPC: `niri msg --json workspaces' returns an
array of workspace objects.  Each object has at least:

  {\"id\": N, \"idx\": N, \"name\": STRING|null,
   \"output\": \"DP-1\", \"is_focused\": true|false, ...}

niri workspaces are per-output (every monitor has its own 1, 2, 3,
...), so showing them all on every bar gives confusing duplicates.
We pick whichever output currently has focus and show only that
output's workspaces.  Names fall back to numeric idx so unnamed
workspaces still appear as `1', `2', ..."
  (let ((out (run-capture "niri" "msg" "--json" "workspaces")))
    (when out
      (let* ((entries (split-json-objects out))
             (focused-output
              (loop for e in entries
                    when (json-bool-value e "is_focused")
                    return (json-string-value e "output")))
             ids focused)
        (dolist (e entries)
          (let* ((output (json-string-value e "output"))
                 (name   (json-string-value e "name"))
                 (idx    (json-number-value e "idx"))
                 (label  (or (and name (plusp (length name)) name)
                             (and idx (format nil "~d" idx))))
                 (focus  (json-bool-value e "is_focused")))
            (when (and label
                       (or (null focused-output)
                           (and output (string= output focused-output))))
              (push (cons (or idx 0) label) ids)
              (when focus (setf focused label)))))
        (let ((names (mapcar #'cdr (sort ids #'< :key #'car))))
          (and names (list names focused)))))))

(defvar *workspaces-source-cache* :unprobed
  "Cached result of `detect-workspaces-source' to avoid spawning IPC
helper processes on every tick.")

(defun detect-workspaces-source ()
  "Return :sway, :hyprland, :niri, or NIL.
Looks at the obvious env vars first, then falls back to probing the
IPC helper itself in case env vars didn't propagate (which happens
when the bar is started by a service manager rather than the
compositor's exec hook).  The result is cached after first call."
  (unless (eq *workspaces-source-cache* :unprobed)
    (return-from detect-workspaces-source *workspaces-source-cache*))
  (let ((source
         (or (and (uiop:getenv "SWAYSOCK")                    :sway)
             (and (uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE") :hyprland)
             (and (uiop:getenv "NIRI_SOCKET")                 :niri)
             ;; Env vars absent - probe the IPC helpers themselves.
             (and (run-capture "swaymsg" "--version")         :sway)
             (and (run-capture "hyprctl" "version")           :hyprland)
             (and (run-capture "niri"    "msg" "version")     :niri))))
    (when source
      (logmsg :info "workspaces backend detected: ~a" source))
    (unless source
      (logmsg :debug "no workspaces backend detected (sway/hyprland/niri); ~
module will stay silent"))
    (setf *workspaces-source-cache* source)
    source))

(defun workspaces-fetch ()
  "Return (NAMES FOCUSED) for the active compositor, or NIL."
  (case (detect-workspaces-source)
    (:sway     (workspaces-sway))
    (:hyprland (workspaces-hyprland))
    (:niri     (workspaces-niri))))

;;; ---- The module ----

(defun workspace-fragments (names focused)
  "Build a list of (TEXT FACE) pairs for NAMES, marking FOCUSED accent."
  (let (frags first)
    (setf first t)
    (dolist (n names)
      (unless first
        (push (list *workspaces-separator* :muted) frags))
      (setf first nil)
      (cond
        ((string= n focused)
         (push (list (car *workspaces-brackets*) :muted)  frags)
         (push (list n :accent)                          frags)
         (push (list (cdr *workspaces-brackets*) :muted)  frags))
        (t (push (list n :normal) frags))))
    (nreverse frags)))

(defmodule :workspaces (:doc "Active workspace list (Sway / Hyprland)."
                        :position :left :priority 80 :interval 0.5)
  (let ((data (workspaces-fetch)))
    (and data (list :fragments (workspace-fragments (first data) (second data))))))
