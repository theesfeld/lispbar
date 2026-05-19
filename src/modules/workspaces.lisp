;;;; workspaces.lisp  --  Active workspace list, driven by the compositor.
;;;;
;;;; Adapters per compositor return a uniform list of workspace
;;;; records; a single, user-configurable filter then decides what
;;;; to display.  The same filter works regardless of which
;;;; compositor is detected, so adding a new compositor is just
;;;; "write an adapter that returns records."

(in-package #:lispbar)

;;; ---- Public tunables ----

(defvar *workspaces-scope* :current-output
  "How to filter the workspace list.

  :current-output - show only the workspaces that belong to the
                    monitor currently in focus.  Sensible default
                    for multi-monitor users; every compositor with
                    per-output workspaces (Sway, Hyprland, niri)
                    is filtered the same way.
  :all            - show every workspace from every output.  May
                    show duplicates on niri (where each output has
                    its own 1, 2, 3 numbering).
  :focused        - show only the workspace that currently has
                    focus, nothing else.")

(defvar *workspaces-brackets* '("[" . "]")
  "Cons of (OPEN . CLOSE) wrapping the focused workspace name.")

(defvar *workspaces-separator* " "
  "Text inserted between adjacent workspace names.")

(defvar *workspaces-empty-text* nil
  "When non-NIL, the string displayed if the filter leaves zero
workspaces (e.g. early in session before any are created).
NIL keeps the module silent in that case.")

;;; ---- Record type returned by every adapter ----

(defstruct workspace
  "One workspace, as reported by whichever compositor is running."
  (name    "")
  (output  nil)
  (focused nil))

;;; ---- JSON helpers (whitespace-tolerant; sufficient for IPC output) ----

(defun skip-ws (text pos)
  (loop while (and (< pos (length text))
                   (member (char text pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun find-value-start (text key)
  (let* ((needle (format nil "\"~a\"" key))
         (start  (search needle text)))
    (when start
      (let ((p (skip-ws text (+ start (length needle)))))
        (when (and (< p (length text)) (char= (char text p) #\:))
          (skip-ws text (1+ p)))))))

(defun json-string-value (text key)
  (let ((p (find-value-start text key)))
    (when (and p (< p (length text)) (char= (char text p) #\"))
      (let ((end (position #\" text :start (1+ p))))
        (and end (subseq text (1+ p) end))))))

(defun json-number-value (text key)
  (let ((p (find-value-start text key)))
    (when p (parse-integer text :start p :junk-allowed t))))

(defun json-bool-value (text key)
  (let ((p (find-value-start text key)))
    (and p (<= (+ p 4) (length text))
         (string= text "true" :start1 p :end1 (+ p 4)))))

(defun split-json-objects (text)
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

;;; ---- Compositor adapters: each returns (LIST OF WORKSPACE) or NIL ----

(defun workspaces-sway ()
  "Return every workspace Sway knows about."
  (let ((out (run-capture "swaymsg" "-t" "get_workspaces" "-r")))
    (when out
      (loop for obj in (split-json-objects out)
            for name = (json-string-value obj "name")
            when name collect
              (make-workspace
                :name    name
                :output  (json-string-value obj "output")
                :focused (json-bool-value obj "focused"))))))

(defun workspaces-hyprland ()
  "Return every workspace Hyprland knows about."
  (let ((all    (run-capture "hyprctl" "-j" "workspaces"))
        (active (run-capture "hyprctl" "-j" "activeworkspace")))
    (when (and all active)
      (let ((focused-name (json-string-value active "name"))
            (records nil))
        (dolist (e (split-json-objects all))
          (let ((n  (json-string-value e "name"))
                (id (json-number-value e "id"))
                (m  (json-string-value e "monitor")))
            (when (and n id)
              (push (cons id
                          (make-workspace
                            :name n
                            :output m
                            :focused (and focused-name (string= n focused-name))))
                    records))))
        (mapcar #'cdr (sort records #'< :key #'car))))))

(defun workspaces-niri ()
  "Return every workspace niri knows about.
Uses `niri msg --json workspaces' (the JSON IPC); returns NIL on
older niri builds that don't support it."
  (let ((out (run-capture "niri" "msg" "--json" "workspaces")))
    (when out
      (let ((records nil))
        (dolist (e (split-json-objects out))
          (let* ((idx (json-number-value e "idx"))
                 (n   (json-string-value e "name"))
                 (label (or (and n (plusp (length n))) n
                            (and idx (format nil "~d" idx)))))
            (when label
              (push (cons (or idx 0)
                          (make-workspace
                            :name    label
                            :output  (json-string-value e "output")
                            :focused (json-bool-value e "is_focused")))
                    records))))
        (mapcar #'cdr (sort records #'< :key #'car))))))

;;; ---- Adapter selection (cached) ----

(defvar *workspaces-source-cache* :unprobed
  "Cached compositor detection result.  Set to :unprobed on first
call so callers re-detect on demand.")

(defun detect-workspaces-source ()
  "Return :sway, :hyprland, :niri, or NIL.  Env vars first, then
fall back to probing the IPC tool itself in case env vars didn't
propagate to this process.  Result is cached for the lifetime of
the process."
  (unless (eq *workspaces-source-cache* :unprobed)
    (return-from detect-workspaces-source *workspaces-source-cache*))
  (let ((source
         (or (and (uiop:getenv "SWAYSOCK")                    :sway)
             (and (uiop:getenv "HYPRLAND_INSTANCE_SIGNATURE") :hyprland)
             (and (uiop:getenv "NIRI_SOCKET")                 :niri)
             (and (run-capture "swaymsg" "--version")         :sway)
             (and (run-capture "hyprctl" "version")           :hyprland)
             (and (run-capture "niri"    "msg" "version")     :niri))))
    (cond (source (logmsg :info "workspaces backend detected: ~a" source))
          (t      (logmsg :debug "no workspaces backend detected; module silent")))
    (setf *workspaces-source-cache* source)
    source))

(defun workspaces-fetch ()
  "Return every workspace the active compositor knows about, as a
list of WORKSPACE records.  Returns NIL when no compositor is
detected or its IPC fails."
  (case (detect-workspaces-source)
    (:sway     (workspaces-sway))
    (:hyprland (workspaces-hyprland))
    (:niri     (workspaces-niri))))

;;; ---- The configurable filter, identical across all compositors ----

(defun apply-workspaces-scope (records)
  "Apply `*workspaces-scope*' to RECORDS, returning the filtered list."
  (case *workspaces-scope*
    (:all records)
    (:focused
     (remove-if-not #'workspace-focused records))
    (:current-output
     (let ((focused-output
            (loop for r in records
                  when (and (workspace-focused r)
                            (workspace-output r))
                  return (workspace-output r))))
       (cond
         ;; No focused output known -> can't filter, show everything.
         ((null focused-output) records)
         (t (remove-if-not
             (lambda (r)
               ;; Keep records whose output matches, or where the
               ;; backend didn't tell us the output (better to show
               ;; than to silently drop).
               (or (null (workspace-output r))
                   (string= (workspace-output r) focused-output)))
             records)))))
    (t records)))

;;; ---- Rendering ----

(defun workspaces-click-handler (data button &rest _)
  "Switch to the clicked workspace.  DATA is a WORKSPACE record."
  (declare (ignore _))
  (when (or (eq button :left) (null button))
    (let ((name (workspace-name data))
          (source (detect-workspaces-source)))
      (handler-case
          (case source
            (:sway (uiop:launch-program
                    (list "swaymsg" "workspace" name)))
            (:hyprland (uiop:launch-program
                        (list "hyprctl" "dispatch" "workspace" name)))
            (:niri (uiop:launch-program
                    (list "niri" "msg" "action"
                          "focus-workspace" name))))
        (error (e)
          (logmsg :warn "workspaces: failed to switch to ~a: ~a" name e))))))

(defun workspaces-scroll-handler (_d button &rest _)
  "Wheel up = previous workspace, wheel down = next.  Routed via the
module-level on-click; handled separately from per-workspace clicks."
  (declare (ignore _ _d))
  (let ((cmd (case (detect-workspaces-source)
               (:sway     (case button
                            (:scroll-up   '("swaymsg" "workspace" "prev"))
                            (:scroll-down '("swaymsg" "workspace" "next"))))
               (:hyprland (case button
                            (:scroll-up   '("hyprctl" "dispatch" "workspace" "e-1"))
                            (:scroll-down '("hyprctl" "dispatch" "workspace" "e+1"))))
               (:niri     (case button
                            (:scroll-up   '("niri" "msg" "action" "focus-workspace-up"))
                            (:scroll-down '("niri" "msg" "action" "focus-workspace-down")))))))
    (when cmd (handler-case (uiop:launch-program cmd)
                (error (e) (logmsg :warn "workspaces: scroll failed: ~a" e))))))

(defun workspace-fragments-from-records (records)
  (let (frags first)
    (setf first t)
    (dolist (r records)
      (unless first
        (push (list *workspaces-separator* :muted) frags))
      (setf first nil)
      (let* ((n         (workspace-name r))
             (focused-p (workspace-focused r))
             (face      (if focused-p :accent :normal)))
        ;; Brackets aren't clickable (they're decoration).
        (when focused-p
          (push (list (car *workspaces-brackets*) :muted) frags))
        ;; The workspace name itself IS clickable; the renderer
        ;; records its bbox and routes clicks to `workspaces-click-handler'.
        (push (list :clickable :text n :face face
                    :on-click 'workspaces-click-handler :data r)
              frags)
        (when focused-p
          (push (list (cdr *workspaces-brackets*) :muted) frags))))
    (nreverse frags)))

(defmodule :workspaces
  (:doc "Active workspaces (Sway / Hyprland / niri).  Scope is
configurable via *workspaces-scope* (see docstring).  Each
workspace number is independently clickable; scroll wheel moves
to previous/next."
   :position :left :priority 80 :interval 0.5
   :on-click ((:scroll-up   workspaces-scroll-handler)
              (:scroll-down workspaces-scroll-handler)))
  (let ((records (workspaces-fetch)))
    (cond
      ((null records) nil)
      (t
       (let ((filtered (apply-workspaces-scope records)))
         (cond
           (filtered
            (list :fragments (workspace-fragments-from-records filtered)))
           (*workspaces-empty-text*
            (list :text *workspaces-empty-text* :face :muted))
           (t nil)))))))
