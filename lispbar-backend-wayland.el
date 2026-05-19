;;; lispbar-backend-wayland.el --- Wayland backends for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: frames, wayland, sway, hyprland

;;; Commentary:

;; Three Wayland backends are defined here, all subclasses of the
;; generic `lispbar-backend':
;;
;;   * `lispbar-backend-sway'      - Sway / wlroots via swaymsg
;;   * `lispbar-backend-hyprland'  - Hyprland via hyprctl
;;   * `lispbar-backend-wayland'   - Any other Wayland session (PGTK)
;;
;; On Wayland an Emacs frame cannot directly become a layer-shell
;; surface — that protocol is only exposed via gtk-layer-shell, which
;; PGTK does not link.  Instead, Lispbar creates an undecorated frame
;; in the normal way and these backends provide helper commands that
;; emit compositor configuration snippets which pin that frame to the
;; top/bottom edge and add it to the reserved-space (struts) list.
;;
;; The helpers are exposed interactively:
;;
;;   M-x lispbar-wayland-print-sway-config
;;   M-x lispbar-wayland-print-hyprland-config
;;
;; The user pastes the output into their compositor config.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispbar-backend)

(defgroup lispbar-wayland nil
  "Wayland backend options for Lispbar."
  :group 'lispbar-backend
  :prefix "lispbar-wayland-")

(defcustom lispbar-wayland-frame-name "Lispbar"
  "Frame name prefix matched by compositor window rules.
Lispbar names every frame \"Lispbar-MONITOR\"; compositor rules
should match titles starting with this string."
  :type 'string
  :group 'lispbar-wayland)

(defcustom lispbar-wayland-poll-interval 1.0
  "Seconds between polls for workspace and monitor changes.
Used as a fallback when no IPC subscription is available."
  :type 'number
  :group 'lispbar-wayland)

;;; Detection helpers

(defun lispbar-wayland--wayland-session-p ()
  "Return non-nil when running in a Wayland session."
  (or (eq window-system 'pgtk)
      (and (getenv "WAYLAND_DISPLAY") t)
      (equal (getenv "XDG_SESSION_TYPE") "wayland")))

(defun lispbar-wayland--sway-session-p ()
  "Return non-nil when running under Sway."
  (and (lispbar-wayland--wayland-session-p)
       (or (getenv "SWAYSOCK")
           (member "sway" (split-string (or (getenv "XDG_CURRENT_DESKTOP") "")
                                        ":" t)))
       (executable-find "swaymsg")))

(defun lispbar-wayland--hyprland-session-p ()
  "Return non-nil when running under Hyprland."
  (and (lispbar-wayland--wayland-session-p)
       (getenv "HYPRLAND_INSTANCE_SIGNATURE")
       (executable-find "hyprctl")))

;;; Shared poll-based change watcher

(defclass lispbar-backend--wayland-base (lispbar-backend)
  ((poll-timer        :initform nil)
   (hotplug-callbacks :initform nil)
   (workspace-callbacks :initform nil)
   (last-monitors     :initform nil)
   (last-workspace    :initform nil))
  "Common state for IPC-poll-based Wayland backends.")

(cl-defmethod lispbar-backend-init ((b lispbar-backend--wayland-base))
  (lispbar-backend-cleanup b)
  (oset b poll-timer
        (run-at-time lispbar-wayland-poll-interval
                     lispbar-wayland-poll-interval
                     (lambda () (lispbar-backend--wayland-poll b))))
  t)

(cl-defmethod lispbar-backend-cleanup ((b lispbar-backend--wayland-base))
  (when-let* ((tm (oref b poll-timer)))
    (cancel-timer tm))
  (oset b poll-timer nil)
  (oset b hotplug-callbacks nil)
  (oset b workspace-callbacks nil))

(defun lispbar-backend--wayland-poll (backend)
  "Detect monitor and workspace changes for BACKEND and dispatch callbacks."
  (condition-case err
      (let ((monitors (lispbar-backend-detect-monitors backend))
            (ws (lispbar-backend-current-workspace backend)))
        (unless (equal monitors (oref backend last-monitors))
          (oset backend last-monitors monitors)
          (dolist (cb (oref backend hotplug-callbacks))
            (condition-case e (funcall cb)
              (error (message "lispbar-wayland: hotplug callback error: %s" e)))))
        (unless (equal ws (oref backend last-workspace))
          (oset backend last-workspace ws)
          (dolist (cb (oref backend workspace-callbacks))
            (condition-case e (funcall cb)
              (error (message "lispbar-wayland: workspace callback error: %s" e))))))
    (error (message "lispbar-wayland poll failed: %s" err))))

(cl-defmethod lispbar-backend-register-hotplug
  ((b lispbar-backend--wayland-base) callback)
  (cl-pushnew callback (oref b hotplug-callbacks) :test #'eq)
  callback)

(cl-defmethod lispbar-backend-unregister-hotplug
  ((b lispbar-backend--wayland-base) token)
  (oset b hotplug-callbacks (delq token (oref b hotplug-callbacks))))

(cl-defmethod lispbar-backend-register-workspace-hook
  ((b lispbar-backend--wayland-base) callback)
  (cl-pushnew callback (oref b workspace-callbacks) :test #'eq)
  callback)

(cl-defmethod lispbar-backend-unregister-workspace-hook
  ((b lispbar-backend--wayland-base) token)
  (oset b workspace-callbacks (delq token (oref b workspace-callbacks))))

;;; Sway backend ----------------------------------------------------

(defclass lispbar-backend-sway (lispbar-backend--wayland-base) ()
  "Lispbar backend driving Sway (or any swaymsg-compatible wlroots WM).")

(cl-defmethod lispbar-backend-available-p ((_ lispbar-backend-sway))
  (lispbar-wayland--sway-session-p))

(cl-defmethod lispbar-backend-detect-monitors ((_ lispbar-backend-sway))
  (let ((outputs (lispbar-backend--call-process-json
                  "swaymsg" "-t" "get_outputs" "-r")))
    (if (not outputs)
        (lispbar-backend--monitors-from-attributes)
      (let ((idx 0) result)
        (dolist (o outputs)
          (let* ((name (alist-get 'name o))
                 (rect (alist-get 'rect o))
                 (x (alist-get 'x rect))
                 (y (alist-get 'y rect))
                 (w (alist-get 'width rect))
                 (h (alist-get 'height rect))
                 (active (alist-get 'active o))
                 (primary (alist-get 'primary o)))
            (when (and active name)
              (push (list :id (intern name)
                          :name name
                          :display-name (or (alist-get 'make o) name)
                          :x x :y y :width w :height h
                          :primary (or primary (zerop idx))
                          :connected t
                          :edid nil
                          :properties o
                          :source 'sway
                          :timestamp (current-time))
                    result)
              (cl-incf idx))))
        (nreverse result)))))

(cl-defmethod lispbar-backend-current-workspace ((_ lispbar-backend-sway))
  (let ((workspaces (lispbar-backend--call-process-json
                     "swaymsg" "-t" "get_workspaces" "-r"))
        (idx 0) found)
    (catch 'done
      (dolist (ws workspaces)
        (when (alist-get 'focused ws)
          (setq found idx)
          (throw 'done t))
        (cl-incf idx)))
    found))

(cl-defmethod lispbar-backend-workspace-names ((_ lispbar-backend-sway))
  (let ((workspaces (lispbar-backend--call-process-json
                     "swaymsg" "-t" "get_workspaces" "-r")))
    (mapcar (lambda (ws) (format "%s" (alist-get 'name ws))) workspaces)))

(cl-defmethod lispbar-backend-switch-workspace
  ((_ lispbar-backend-sway) index)
  (let* ((names (lispbar-backend-workspace-names
                 (lispbar-backend-current)))
         (target (nth index names)))
    (when target
      (eq 0 (call-process "swaymsg" nil nil nil "workspace" target)))))

(cl-defmethod lispbar-backend-window-title ((_ lispbar-backend-sway))
  (let ((tree (lispbar-backend--call-process-json
               "swaymsg" "-t" "get_tree" "-r")))
    (lispbar-backend--sway-focused-name tree)))

(cl-defmethod lispbar-backend-window-class ((_ lispbar-backend-sway))
  (let ((tree (lispbar-backend--call-process-json
               "swaymsg" "-t" "get_tree" "-r")))
    (lispbar-backend--sway-focused tree 'app_id)))

(defun lispbar-backend--sway-focused (node prop)
  "Walk NODE recursively returning PROP of the focused leaf, or nil."
  (when (listp node)
    (or (and (alist-get 'focused node) (alist-get prop node))
        (cl-some (lambda (child) (lispbar-backend--sway-focused child prop))
                 (or (alist-get 'nodes node) nil))
        (cl-some (lambda (child) (lispbar-backend--sway-focused child prop))
                 (or (alist-get 'floating_nodes node) nil)))))

(defun lispbar-backend--sway-focused-name (node)
  "Return name of the currently focused window in NODE."
  (lispbar-backend--sway-focused node 'name))

;;;###autoload
(defun lispbar-wayland-print-sway-config ()
  "Print Sway configuration that pins Lispbar to the top of every output.
The snippet is shown in *Lispbar Sway Config*; paste it into your
Sway config file (e.g. ~/.config/sway/config)."
  (interactive)
  (let ((buf (get-buffer-create "*Lispbar Sway Config*"))
        (h (or (and (boundp 'lispbar-height) lispbar-height) 28)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Lispbar - Sway window rules
# Place these lines in your Sway config (e.g. ~/.config/sway/config).
# Restart the WM (Mod+Shift+c) after saving.

for_window [title=\"^%s.*\"] {
    floating enable
    border none
    sticky enable
    focus disable
    move position 0 0
}

# Reserve %d px at the top of every output for Lispbar:
bar { swaybar_command true }   # disable the built-in swaybar if you have one

# Adjust gaps so windows don't overlap the bar:
gaps top %d
"
                      lispbar-wayland-frame-name h h)))
    (pop-to-buffer buf)))

;;; Hyprland backend ------------------------------------------------

(defclass lispbar-backend-hyprland (lispbar-backend--wayland-base) ()
  "Lispbar backend driving Hyprland through hyprctl IPC.")

(cl-defmethod lispbar-backend-available-p ((_ lispbar-backend-hyprland))
  (lispbar-wayland--hyprland-session-p))

(cl-defmethod lispbar-backend-detect-monitors ((_ lispbar-backend-hyprland))
  (let ((mons (lispbar-backend--call-process-json
               "hyprctl" "-j" "monitors")))
    (if (not mons)
        (lispbar-backend--monitors-from-attributes)
      (let ((idx 0) result)
        (dolist (m mons)
          (let ((name (alist-get 'name m)))
            (push (list :id (intern (or name (format "m%d" idx)))
                        :name name
                        :display-name (or (alist-get 'description m) name)
                        :x (alist-get 'x m)
                        :y (alist-get 'y m)
                        :width (alist-get 'width m)
                        :height (alist-get 'height m)
                        :primary (or (alist-get 'focused m) (zerop idx))
                        :connected t
                        :edid nil
                        :properties m
                        :source 'hyprland
                        :timestamp (current-time))
                  result)
            (cl-incf idx)))
        (nreverse result)))))

(cl-defmethod lispbar-backend-current-workspace ((_ lispbar-backend-hyprland))
  (let ((ws (lispbar-backend--call-process-json "hyprctl" "-j" "activeworkspace")))
    (when ws
      (let ((id (alist-get 'id ws)))
        (and (numberp id) (max 0 (1- id)))))))

(cl-defmethod lispbar-backend-workspace-names ((_ lispbar-backend-hyprland))
  (let ((workspaces (lispbar-backend--call-process-json
                     "hyprctl" "-j" "workspaces")))
    (mapcar (lambda (ws) (format "%s" (alist-get 'name ws)))
            (sort (copy-sequence (or workspaces nil))
                  (lambda (a b) (< (alist-get 'id a) (alist-get 'id b)))))))

(cl-defmethod lispbar-backend-switch-workspace
  ((_ lispbar-backend-hyprland) index)
  (eq 0 (call-process "hyprctl" nil nil nil "dispatch"
                      "workspace" (number-to-string (1+ index)))))

(cl-defmethod lispbar-backend-window-title ((_ lispbar-backend-hyprland))
  (let ((w (lispbar-backend--call-process-json "hyprctl" "-j" "activewindow")))
    (alist-get 'title w)))

(cl-defmethod lispbar-backend-window-class ((_ lispbar-backend-hyprland))
  (let ((w (lispbar-backend--call-process-json "hyprctl" "-j" "activewindow")))
    (alist-get 'class w)))

;;;###autoload
(defun lispbar-wayland-print-hyprland-config ()
  "Print Hyprland window rules that pin Lispbar to the top edge.
The snippet is shown in *Lispbar Hyprland Config*; paste it into
your Hyprland config (e.g. ~/.config/hypr/hyprland.conf)."
  (interactive)
  (let ((buf (get-buffer-create "*Lispbar Hyprland Config*"))
        (h (or (and (boundp 'lispbar-height) lispbar-height) 28)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# Lispbar - Hyprland window rules
# Append to ~/.config/hypr/hyprland.conf and reload with `hyprctl reload'.

windowrulev2 = float,title:^(%s.*)$
windowrulev2 = pin,title:^(%s.*)$
windowrulev2 = noborder,title:^(%s.*)$
windowrulev2 = noshadow,title:^(%s.*)$
windowrulev2 = noblur,title:^(%s.*)$
windowrulev2 = nofocus,title:^(%s.*)$
windowrulev2 = move 0 0,title:^(%s.*)$

# Reserve %d px at the top so windows don't overlap Lispbar:
general {
    gaps_out = %d, 0, 0, 0
}
"
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      lispbar-wayland-frame-name
                      h h)))
    (pop-to-buffer buf)))

;;; Plain Wayland backend (no compositor IPC) -----------------------

(defclass lispbar-backend-wayland (lispbar-backend--wayland-base) ()
  "Generic Wayland backend (PGTK build, unknown compositor).
Provides monitor detection via `display-monitor-attributes-list'
and exposes workspace/window queries as nil.")

(cl-defmethod lispbar-backend-available-p ((_ lispbar-backend-wayland))
  (lispbar-wayland--wayland-session-p))

(cl-defmethod lispbar-backend-detect-monitors ((_ lispbar-backend-wayland))
  (lispbar-backend--monitors-from-attributes))

;;; Common: configure-frame is a no-op for Wayland (no _NET_WM_STRUT,
;;; no override-redirect — those are X11-only).  The compositor rules
;;; printed by the helpers above do the equivalent work.

;;; Registration ----------------------------------------------------

(lispbar-backend-register
 (make-instance 'lispbar-backend-sway
                :name 'sway :priority 80
                :features '(monitor-detect workspaces window-info hotplug)))

(lispbar-backend-register
 (make-instance 'lispbar-backend-hyprland
                :name 'hyprland :priority 80
                :features '(monitor-detect workspaces window-info hotplug)))

(lispbar-backend-register
 (make-instance 'lispbar-backend-wayland
                :name 'wayland :priority 40
                :features '(monitor-detect)))

(provide 'lispbar-backend-wayland)
;;; lispbar-backend-wayland.el ends here
