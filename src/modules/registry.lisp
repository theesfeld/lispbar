;;;; registry.lisp  --  Click-to-open registry browser.
;;;;
;;;; A tiny built-in module whose only job is to be clicked.  The
;;;; left-click spawns a terminal running `lispbar registry browse',
;;;; which is the fzf-driven picker for installing community modules
;;;; and themes.  The label is just a hint that something will
;;;; happen on click.

(in-package #:lispbar)

(defvar *registry-label* "📦"
  "Text shown for the :registry module.  Defaults to the package
emoji.  Override with whatever your bar font can render — e.g.
\"registry\", a Nerd Font glyph, or `nil' to suppress the module
entirely.")

(defvar *registry-terminal*
  "$TERMINAL footclient foot alacritty kitty wezterm gnome-terminal konsole xterm"
  "Space-separated list of terminal commands tried in order; the
first one found on PATH wins.  Each entry may include flags, but
the actual program-to-run is always appended via `-e CMD'.
Override if the auto-detection picks the wrong one:

  (setf *registry-terminal* \"alacritty\")
  (setf *registry-terminal* \"foot -a registry\")

`footclient' is listed before `foot' so users running a foot
server get the snappier startup of the client variant.

NOTE: the value is interpolated directly into a shell command,
so it must be a plain whitespace-separated list of program names
(optionally with their own flags).  Don't put `&&', `||', shell
substitutions other than `$TERMINAL', or anything else you
wouldn't drop into `for t in …; do …; done'.")

(defvar *registry-on-click* nil
  "Optional shell command run on left-click instead of the built-in
terminal-spawn behaviour.  Set this if you'd rather drive the
picker yourself, e.g. via a custom keybind or a different fuzzy
finder.")

(defun registry-spawn-browser ()
  "Run `lispbar registry browse' inside a freshly-spawned terminal."
  (let* ((picker  "lispbar registry browse")
         ;; Resolve the first terminal in the user's preference list
         ;; that's actually on PATH.  The list is expanded by the
         ;; shell (so $TERMINAL works) and we take the first token
         ;; that `command -v' resolves.
         (resolve (format nil "for t in ~a; do \
command -v \"$t\" >/dev/null 2>&1 && echo \"$t\" && exit 0; \
done; exit 1" *registry-terminal*))
         (term    (string-trim '(#\Space #\Newline #\Tab)
                                (or (run-capture "sh" "-c" resolve) ""))))
    (cond
      ((zerop (length term))
       (logmsg :warn "registry module: no terminal found (tried ~a)"
               *registry-terminal*))
      (t
       (uiop:launch-program
        (list "sh" "-c"
              (format nil "~a -e sh -c ~s" term picker)))))))

(defun registry-left-click (_m _b _i)
  (declare (ignore _m _b _i))
  (cond
    ((and *registry-on-click* (plusp (length *registry-on-click*)))
     (uiop:launch-program (list "sh" "-c" *registry-on-click*)))
    (t (registry-spawn-browser))))

(defmodule :registry
  (:doc "Click to browse community modules and themes (fzf picker)."
   :position :right :priority 5 :interval 3600.0
   :tooltip "lispbar registry — click to install modules and themes"
   :on-click ((:left registry-left-click)))
  (when *registry-label*
    (list :text *registry-label* :face :muted)))
