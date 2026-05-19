;;; lispbar-theme.el --- Theme and face system for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes, frames

;;; Commentary:

;; Defines every face used by Lispbar and a small theme system that
;; lets users switch the entire palette with one symbol:
;;
;;   (setq lispbar-current-theme 'nordish)
;;   (lispbar-theme-apply)
;;
;; A theme is just an alist of (FACE . SPEC) pairs.  Users register
;; new themes with `lispbar-define-theme'; the Lispbar faces are
;; updated live and the toolbar redraws.
;;
;; The `adaptive' theme derives colours from the current Emacs theme
;; so Lispbar looks at home in any colour scheme without manual
;; tuning.

;;; Code:

(require 'cl-lib)

(defgroup lispbar-theme nil
  "Theming and faces for Lispbar."
  :group 'lispbar
  :prefix "lispbar-theme-")

;;; Faces -----------------------------------------------------------

(defface lispbar-face
  '((t :inherit mode-line))
  "Base face used by all Lispbar content."
  :group 'lispbar-theme)

(defface lispbar-active-face
  '((t :inherit lispbar-face :weight bold))
  "Face for the active workspace / focused window indicators."
  :group 'lispbar-theme)

(defface lispbar-inactive-face
  '((t :inherit shadow))
  "Face for inactive workspaces and dim items."
  :group 'lispbar-theme)

(defface lispbar-urgent-face
  '((t :inherit error :weight bold))
  "Face for urgent / critical states (battery, errors)."
  :group 'lispbar-theme)

(defface lispbar-warning-face
  '((t :inherit warning))
  "Face for warning states (low battery, mute)."
  :group 'lispbar-theme)

(defface lispbar-ok-face
  '((t :inherit success))
  "Face for good states (charging, connected)."
  :group 'lispbar-theme)

(defface lispbar-accent-face
  '((t :inherit font-lock-keyword-face))
  "Accent face for highlights (current value, important text)."
  :group 'lispbar-theme)

(defface lispbar-muted-face
  '((t :inherit font-lock-comment-face))
  "Muted face for separators and decorations."
  :group 'lispbar-theme)

(defface lispbar-clock-face
  '((t :inherit lispbar-accent-face))
  "Face for the clock module."
  :group 'lispbar-theme)

(defface lispbar-workspace-face
  '((t :inherit lispbar-face))
  "Face for workspace names."
  :group 'lispbar-theme)

(defface lispbar-workspace-active-face
  '((t :inherit lispbar-active-face))
  "Face for the focused workspace."
  :group 'lispbar-theme)

(defface lispbar-battery-face
  '((t :inherit lispbar-face))
  "Face for the battery percentage."
  :group 'lispbar-theme)

(defface lispbar-battery-low-face
  '((t :inherit lispbar-warning-face))
  "Face for low battery state."
  :group 'lispbar-theme)

(defface lispbar-battery-critical-face
  '((t :inherit lispbar-urgent-face))
  "Face for critical battery state."
  :group 'lispbar-theme)

(defface lispbar-battery-charging-face
  '((t :inherit lispbar-ok-face))
  "Face shown while the battery is charging."
  :group 'lispbar-theme)

(defface lispbar-network-face
  '((t :inherit lispbar-face))
  "Face for the network module."
  :group 'lispbar-theme)

(defface lispbar-network-down-face
  '((t :inherit lispbar-urgent-face))
  "Face when network is down."
  :group 'lispbar-theme)

(defface lispbar-audio-face
  '((t :inherit lispbar-face))
  "Face for the audio module."
  :group 'lispbar-theme)

(defface lispbar-audio-muted-face
  '((t :inherit lispbar-muted-face))
  "Face for muted audio."
  :group 'lispbar-theme)

(defface lispbar-bluetooth-face
  '((t :inherit lispbar-face))
  "Face for the Bluetooth module."
  :group 'lispbar-theme)

(defface lispbar-bluetooth-connected-face
  '((t :inherit lispbar-ok-face))
  "Face when at least one Bluetooth device is connected."
  :group 'lispbar-theme)

(defface lispbar-cpu-face
  '((t :inherit lispbar-face))
  "Face for the CPU module."
  :group 'lispbar-theme)

(defface lispbar-cpu-high-face
  '((t :inherit lispbar-warning-face))
  "Face when CPU load is high."
  :group 'lispbar-theme)

(defface lispbar-memory-face
  '((t :inherit lispbar-face))
  "Face for the memory module."
  :group 'lispbar-theme)

(defface lispbar-memory-high-face
  '((t :inherit lispbar-warning-face))
  "Face for high memory pressure."
  :group 'lispbar-theme)

(defface lispbar-brightness-face
  '((t :inherit lispbar-face))
  "Face for the brightness module."
  :group 'lispbar-theme)

(defface lispbar-mpris-face
  '((t :inherit lispbar-accent-face))
  "Face for now-playing media titles."
  :group 'lispbar-theme)

(defface lispbar-tray-face
  '((t :inherit lispbar-face))
  "Face for tray entries."
  :group 'lispbar-theme)

(defface lispbar-separator-face
  '((t :inherit lispbar-muted-face))
  "Face for inter-module separators."
  :group 'lispbar-theme)

(defconst lispbar-theme--all-faces
  '(lispbar-face lispbar-active-face lispbar-inactive-face
    lispbar-urgent-face lispbar-warning-face lispbar-ok-face
    lispbar-accent-face lispbar-muted-face
    lispbar-clock-face
    lispbar-workspace-face lispbar-workspace-active-face
    lispbar-battery-face lispbar-battery-low-face
    lispbar-battery-critical-face lispbar-battery-charging-face
    lispbar-network-face lispbar-network-down-face
    lispbar-audio-face lispbar-audio-muted-face
    lispbar-bluetooth-face lispbar-bluetooth-connected-face
    lispbar-cpu-face lispbar-cpu-high-face
    lispbar-memory-face lispbar-memory-high-face
    lispbar-brightness-face lispbar-mpris-face
    lispbar-tray-face lispbar-separator-face)
  "All faces that may be re-styled by a Lispbar theme.")

;;; Theme registry --------------------------------------------------

(defvar lispbar-theme-registry nil
  "Alist of (NAME . PLIST) for every registered theme.
PLIST keys: :doc, :faces (alist of FACE . SPEC).")

(defcustom lispbar-current-theme 'adaptive
  "Currently active Lispbar theme.
A symbol present in `lispbar-theme-registry'.  Use `adaptive' to
inherit colours from the active Emacs theme."
  :type 'symbol
  :group 'lispbar-theme
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'lispbar-theme-apply)
           (lispbar-theme-apply))))

(defun lispbar-define-theme (name docstring faces)
  "Register a Lispbar theme.
NAME is a symbol; DOCSTRING describes it; FACES is an alist of
\(FACE . SPEC) pairs where SPEC is the same form accepted by
`face-spec-set'."
  (cl-check-type name symbol)
  (cl-check-type faces list)
  (let ((entry (list :doc docstring :faces faces))
        (cell (assq name lispbar-theme-registry)))
    (if cell (setcdr cell entry)
      (push (cons name entry) lispbar-theme-registry)))
  name)

(defun lispbar-theme-names ()
  "Return the list of all registered theme names."
  (mapcar #'car lispbar-theme-registry))

(defun lispbar-theme-apply (&optional name)
  "Apply theme NAME (default: `lispbar-current-theme').
Re-applies the active theme on the live faces and triggers a
toolbar redraw if Lispbar is running."
  (interactive
   (list (intern (completing-read "Lispbar theme: "
                                  (mapcar #'symbol-name (lispbar-theme-names))
                                  nil t nil nil
                                  (symbol-name lispbar-current-theme)))))
  (let* ((theme (or name lispbar-current-theme))
         (entry (cdr (assq theme lispbar-theme-registry))))
    (unless entry
      (user-error "Unknown Lispbar theme `%s'" theme))
    (dolist (pair (plist-get entry :faces))
      ;; Each entry is (FACE SPEC); face-spec-set wants the bare SPEC.
      (face-spec-set (car pair) (cadr pair) 'face-defface-spec))
    (setq lispbar-current-theme theme)
    (when (and (fboundp 'lispbar-refresh)
               (boundp 'lispbar-mode) lispbar-mode)
      (ignore-errors (lispbar-refresh)))
    (message "Lispbar theme: %s" theme)
    theme))

;;; Built-in themes ------------------------------------------------

(lispbar-define-theme
 'adaptive
 "Inherits from the current Emacs theme via `mode-line', `success', etc."
 ;; Empty alist: faces use their :inherit defaults defined above.
 nil)

(lispbar-define-theme
 'minimal
 "Monochrome theme without bold or colour highlights."
 '((lispbar-face                  ((t :inherit mode-line :weight normal)))
   (lispbar-active-face           ((t :inherit mode-line :weight normal :underline t)))
   (lispbar-inactive-face         ((t :inherit shadow)))
   (lispbar-accent-face           ((t :inherit mode-line :weight normal)))
   (lispbar-urgent-face           ((t :inherit mode-line :underline t)))
   (lispbar-warning-face          ((t :inherit shadow)))
   (lispbar-ok-face               ((t :inherit mode-line)))))

(lispbar-define-theme
 'nordish
 "Cool blue-and-snow palette inspired by Nord."
 '((lispbar-face                  ((t :foreground "#d8dee9" :background "#2e3440")))
   (lispbar-active-face           ((t :foreground "#eceff4" :background "#3b4252" :weight bold)))
   (lispbar-inactive-face         ((t :foreground "#4c566a")))
   (lispbar-accent-face           ((t :foreground "#88c0d0")))
   (lispbar-muted-face            ((t :foreground "#616e88")))
   (lispbar-urgent-face           ((t :foreground "#bf616a" :weight bold)))
   (lispbar-warning-face          ((t :foreground "#ebcb8b")))
   (lispbar-ok-face               ((t :foreground "#a3be8c")))
   (lispbar-clock-face            ((t :foreground "#88c0d0")))
   (lispbar-workspace-active-face ((t :foreground "#eceff4" :weight bold)))
   (lispbar-battery-charging-face ((t :foreground "#a3be8c")))
   (lispbar-battery-low-face      ((t :foreground "#ebcb8b")))
   (lispbar-battery-critical-face ((t :foreground "#bf616a" :weight bold)))
   (lispbar-mpris-face            ((t :foreground "#b48ead" :slant italic)))))

(lispbar-define-theme
 'gruvboxish
 "Warm dark palette inspired by Gruvbox."
 '((lispbar-face                  ((t :foreground "#ebdbb2" :background "#282828")))
   (lispbar-active-face           ((t :foreground "#fbf1c7" :background "#3c3836" :weight bold)))
   (lispbar-inactive-face         ((t :foreground "#665c54")))
   (lispbar-accent-face           ((t :foreground "#fabd2f")))
   (lispbar-muted-face            ((t :foreground "#7c6f64")))
   (lispbar-urgent-face           ((t :foreground "#fb4934" :weight bold)))
   (lispbar-warning-face          ((t :foreground "#fe8019")))
   (lispbar-ok-face               ((t :foreground "#b8bb26")))
   (lispbar-clock-face            ((t :foreground "#83a598")))
   (lispbar-mpris-face            ((t :foreground "#d3869b" :slant italic)))))

(lispbar-define-theme
 'catppuccinish
 "Pastel palette inspired by Catppuccin Mocha."
 '((lispbar-face                  ((t :foreground "#cdd6f4" :background "#1e1e2e")))
   (lispbar-active-face           ((t :foreground "#f5e0dc" :background "#313244" :weight bold)))
   (lispbar-inactive-face         ((t :foreground "#6c7086")))
   (lispbar-accent-face           ((t :foreground "#89b4fa")))
   (lispbar-muted-face            ((t :foreground "#7f849c")))
   (lispbar-urgent-face           ((t :foreground "#f38ba8" :weight bold)))
   (lispbar-warning-face          ((t :foreground "#f9e2af")))
   (lispbar-ok-face               ((t :foreground "#a6e3a1")))
   (lispbar-clock-face            ((t :foreground "#cba6f7")))
   (lispbar-mpris-face            ((t :foreground "#f5c2e7" :slant italic)))))

(lispbar-define-theme
 'doomish
 "Dark high-contrast palette inspired by doom-one."
 '((lispbar-face                  ((t :foreground "#bbc2cf" :background "#282c34")))
   (lispbar-active-face           ((t :foreground "#ECEFF4" :background "#3e4451" :weight bold)))
   (lispbar-inactive-face         ((t :foreground "#5B6268")))
   (lispbar-accent-face           ((t :foreground "#51afef")))
   (lispbar-muted-face            ((t :foreground "#5B6268")))
   (lispbar-urgent-face           ((t :foreground "#ff6c6b" :weight bold)))
   (lispbar-warning-face          ((t :foreground "#ECBE7B")))
   (lispbar-ok-face               ((t :foreground "#98be65")))
   (lispbar-clock-face            ((t :foreground "#c678dd")))
   (lispbar-mpris-face            ((t :foreground "#a9a1e1" :slant italic)))))

;;; Live theme tracking --------------------------------------------

(defun lispbar-theme--on-emacs-theme-change (&rest _)
  "Reapply the Lispbar theme when the Emacs theme changes."
  (when (eq lispbar-current-theme 'adaptive)
    (ignore-errors (lispbar-theme-apply 'adaptive))))

(add-hook 'enable-theme-functions  #'lispbar-theme--on-emacs-theme-change)
(add-hook 'disable-theme-functions #'lispbar-theme--on-emacs-theme-change)

;;; Initial application --------------------------------------------

(ignore-errors (lispbar-theme-apply lispbar-current-theme))

(provide 'lispbar-theme)
;;; lispbar-theme.el ends here
