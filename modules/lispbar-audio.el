;;; lispbar-audio.el --- Audio (volume / mute) module -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, frames

;;; Commentary:

;; Reports current volume and mute state of the default audio sink.
;; Detection cascade:
;;
;;   1. wpctl   (PipeWire / WirePlumber)        - preferred
;;   2. pactl   (PulseAudio / pipewire-pulse)
;;   3. amixer  (ALSA, master channel)
;;
;; Works identically on X11 and Wayland.
;; M-x lispbar-audio-toggle-mute  /  -volume-up  /  -volume-down  are
;; provided as convenient interactive helpers.

;;; Code:

(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-audio nil
  "Audio module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-audio-")

(defcustom lispbar-audio-format "VOL %d%%"
  "`format'-style string fed the integer volume percentage."
  :type 'string
  :group 'lispbar-audio)

(defcustom lispbar-audio-muted-format "MUTE"
  "Display string when the sink is muted."
  :type 'string
  :group 'lispbar-audio)

(defcustom lispbar-audio-step 5
  "Percent step used by `lispbar-audio-volume-up' / `-down'."
  :type 'integer
  :group 'lispbar-audio)

(defcustom lispbar-audio-update-interval 2.0
  "Seconds between audio polls when no event hook fires."
  :type 'number
  :group 'lispbar-audio)

;;; Backend detection

(defun lispbar-audio--via-wpctl ()
  "Return a plist (:volume PCT :muted BOOL) via wpctl, or nil."
  (when (executable-find "wpctl")
    (with-temp-buffer
      (when (eq 0 (call-process "wpctl" nil t nil "get-volume" "@DEFAULT_AUDIO_SINK@"))
        (goto-char (point-min))
        (when (re-search-forward "Volume: \\([0-9]+\\.[0-9]+\\)\\(.*\\)" nil t)
          (list :volume (round (* 100 (string-to-number (match-string 1))))
                :muted (and (string-match-p "MUTED" (match-string 2)) t)))))))

(defun lispbar-audio--via-pactl ()
  "Return a plist (:volume PCT :muted BOOL) via pactl, or nil."
  (when (executable-find "pactl")
    (let (volume muted)
      (with-temp-buffer
        (when (eq 0 (call-process "pactl" nil t nil "get-sink-volume" "@DEFAULT_SINK@"))
          (goto-char (point-min))
          (when (re-search-forward "\\([0-9]+\\)%" nil t)
            (setq volume (string-to-number (match-string 1))))))
      (with-temp-buffer
        (when (eq 0 (call-process "pactl" nil t nil "get-sink-mute" "@DEFAULT_SINK@"))
          (goto-char (point-min))
          (setq muted (and (re-search-forward "yes" nil t) t))))
      (and volume (list :volume volume :muted muted)))))

(defun lispbar-audio--via-amixer ()
  "Return a plist (:volume PCT :muted BOOL) via amixer, or nil."
  (when (executable-find "amixer")
    (with-temp-buffer
      (when (eq 0 (call-process "amixer" nil t nil "get" "Master"))
        (goto-char (point-min))
        (let (volume muted)
          (when (re-search-forward "\\[\\([0-9]+\\)%\\]" nil t)
            (setq volume (string-to-number (match-string 1))))
          (goto-char (point-min))
          (when (re-search-forward "\\[\\(on\\|off\\)\\]" nil t)
            (setq muted (string= (match-string 1) "off")))
          (and volume (list :volume volume :muted muted)))))))

(defun lispbar-audio-state ()
  "Return the current audio state plist, or nil."
  (or (lispbar-audio--via-wpctl)
      (lispbar-audio--via-pactl)
      (lispbar-audio--via-amixer)))

(defun lispbar-audio-update ()
  "Render the audio module string."
  (when-let* ((s (lispbar-audio-state)))
    (if (plist-get s :muted)
        (propertize lispbar-audio-muted-format
                    'face 'lispbar-audio-muted-face)
      (propertize (format lispbar-audio-format (plist-get s :volume))
                  'face 'lispbar-audio-face))))

;;; Interactive helpers

(defun lispbar-audio--run (&rest args)
  "Invoke whichever backend tool is available with ARGS."
  (let* ((tool (cond ((executable-find "wpctl")  'wpctl)
                     ((executable-find "pactl")  'pactl)
                     ((executable-find "amixer") 'amixer)))
         (cmd (cl-case tool
                (wpctl  (cons "wpctl" args))
                (pactl  (cons "pactl" args))
                (amixer (cons "amixer" args)))))
    (when cmd (apply #'call-process (car cmd) nil nil nil (cdr cmd)))))

;;;###autoload
(defun lispbar-audio-toggle-mute ()
  "Toggle mute on the default audio sink."
  (interactive)
  (cond
   ((executable-find "wpctl")
    (lispbar-audio--run "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"))
   ((executable-find "pactl")
    (lispbar-audio--run "set-sink-mute" "@DEFAULT_SINK@" "toggle"))
   ((executable-find "amixer")
    (lispbar-audio--run "set" "Master" "toggle")))
  (when (fboundp 'lispbar-modules-update) (lispbar-modules-update 'audio)))

;;;###autoload
(defun lispbar-audio-volume-up (&optional step)
  "Raise the master volume by STEP percent (default `lispbar-audio-step')."
  (interactive)
  (let ((step (number-to-string (or step lispbar-audio-step))))
    (cond
     ((executable-find "wpctl")
      (lispbar-audio--run "set-volume" "@DEFAULT_AUDIO_SINK@" (concat step "%+")))
     ((executable-find "pactl")
      (lispbar-audio--run "set-sink-volume" "@DEFAULT_SINK@" (concat "+" step "%")))
     ((executable-find "amixer")
      (lispbar-audio--run "set" "Master" (concat step "%+")))))
  (when (fboundp 'lispbar-modules-update) (lispbar-modules-update 'audio)))

;;;###autoload
(defun lispbar-audio-volume-down (&optional step)
  "Lower the master volume by STEP percent (default `lispbar-audio-step')."
  (interactive)
  (let ((step (number-to-string (or step lispbar-audio-step))))
    (cond
     ((executable-find "wpctl")
      (lispbar-audio--run "set-volume" "@DEFAULT_AUDIO_SINK@" (concat step "%-")))
     ((executable-find "pactl")
      (lispbar-audio--run "set-sink-volume" "@DEFAULT_SINK@" (concat "-" step "%")))
     ((executable-find "amixer")
      (lispbar-audio--run "set" "Master" (concat step "%-")))))
  (when (fboundp 'lispbar-modules-update) (lispbar-modules-update 'audio)))

(lispbar-defmodule audio
  "Volume and mute state of the default audio sink."
  :update-fn #'lispbar-audio-update
  :update-interval lispbar-audio-update-interval
  :position 'right :priority 65)

(provide 'lispbar-audio)
;;; lispbar-audio.el ends here
