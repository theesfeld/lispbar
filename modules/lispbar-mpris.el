;;; lispbar-mpris.el --- MPRIS2 now-playing module -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, frames

;;; Commentary:

;; Pure-Elisp now-playing indicator that talks to MPRIS2-compatible
;; media players (Spotify, mpv, VLC, Firefox, Rhythmbox, ...) over
;; the session D-Bus.  Works on X11 and Wayland alike.
;;
;; Interactive helpers: M-x lispbar-mpris-play-pause / -next /
;; -previous toggle and skip the focused player.

;;; Code:

(require 'dbus)
(require 'lispbar-modules)
(require 'lispbar-theme)

(defgroup lispbar-mpris nil
  "MPRIS2 media player module for Lispbar."
  :group 'lispbar
  :prefix "lispbar-mpris-")

(defcustom lispbar-mpris-format "%s - %s"
  "`format'-style string fed two strings: ARTIST and TITLE."
  :type 'string
  :group 'lispbar-mpris)

(defcustom lispbar-mpris-max-length 50
  "Maximum characters shown before the now-playing text is truncated."
  :type 'integer
  :group 'lispbar-mpris)

(defcustom lispbar-mpris-update-interval 3.0
  "Seconds between MPRIS polls."
  :type 'number
  :group 'lispbar-mpris)

(defcustom lispbar-mpris-preferred-player nil
  "If non-nil, prefer the player whose bus name matches this regex.
For example \"spotify\" pins Lispbar to Spotify even when several
players are running."
  :type '(choice (const :tag "Any active player" nil)
                 (regexp :tag "Bus-name regex"))
  :group 'lispbar-mpris)

(defun lispbar-mpris--players ()
  "Return the list of MPRIS2 bus names currently on the session bus."
  (ignore-errors
    (cl-remove-if-not
     (lambda (n) (string-prefix-p "org.mpris.MediaPlayer2." n))
     (dbus-list-names :session))))

(defun lispbar-mpris--get-prop (bus iface prop)
  "Read PROP on IFACE from BUS, returning nil on any error."
  (ignore-errors
    (dbus-get-property :session bus
                       "/org/mpris/MediaPlayer2"
                       iface prop)))

(defun lispbar-mpris--pick-player ()
  "Return the bus name of the player to display, or nil."
  (let* ((players (lispbar-mpris--players))
         (preferred (and lispbar-mpris-preferred-player
                         (cl-find-if
                          (lambda (n)
                            (string-match-p lispbar-mpris-preferred-player n))
                          players))))
    (or preferred
        (cl-find-if
         (lambda (n)
           (let ((status (lispbar-mpris--get-prop
                          n "org.mpris.MediaPlayer2.Player" "PlaybackStatus")))
             (member status '("Playing" "Paused"))))
         players)
        (car players))))

(defun lispbar-mpris--truncate (s)
  "Truncate S to `lispbar-mpris-max-length' with an ellipsis."
  (if (> (length s) lispbar-mpris-max-length)
      (concat (substring s 0 (max 0 (- lispbar-mpris-max-length 1))) "…")
    s))

(defun lispbar-mpris--metadata-value (metadata key)
  "Extract KEY from a D-Bus METADATA a{sv} structure."
  (let ((entry (assoc key metadata)))
    (when entry
      (let ((v (cadr entry)))
        (cond ((stringp v) v)
              ((and (listp v) (stringp (car v))) (car v))
              (v (format "%s" v)))))))

(defun lispbar-mpris-update ()
  "Render the MPRIS module string, or nil when nothing is playing."
  (when-let* ((bus (lispbar-mpris--pick-player))
              (md (lispbar-mpris--get-prop
                   bus "org.mpris.MediaPlayer2.Player" "Metadata")))
    (let* ((title (or (lispbar-mpris--metadata-value md "xesam:title") ""))
           (artists (or (lispbar-mpris--metadata-value md "xesam:artist") ""))
           (text (cond ((and (string-empty-p title) (string-empty-p artists)) nil)
                       ((string-empty-p artists) title)
                       (t (format lispbar-mpris-format artists title)))))
      (when text
        (propertize (lispbar-mpris--truncate text)
                    'face 'lispbar-mpris-face)))))

;;; Interactive helpers

(defun lispbar-mpris--call (method)
  "Invoke METHOD on the chosen MPRIS player, if any."
  (when-let* ((bus (lispbar-mpris--pick-player)))
    (ignore-errors
      (dbus-call-method :session bus
                        "/org/mpris/MediaPlayer2"
                        "org.mpris.MediaPlayer2.Player" method))
    (when (fboundp 'lispbar-modules-update)
      (lispbar-modules-update 'mpris))))

;;;###autoload
(defun lispbar-mpris-play-pause () "Toggle playback."     (interactive) (lispbar-mpris--call "PlayPause"))
;;;###autoload
(defun lispbar-mpris-next ()       "Skip to next track."   (interactive) (lispbar-mpris--call "Next"))
;;;###autoload
(defun lispbar-mpris-previous ()   "Skip to previous track."(interactive) (lispbar-mpris--call "Previous"))

(lispbar-defmodule mpris
  "Now-playing indicator for any MPRIS2 media player."
  :update-fn #'lispbar-mpris-update
  :update-interval lispbar-mpris-update-interval
  :position 'center :priority 40)

(provide 'lispbar-mpris)
;;; lispbar-mpris.el ends here
