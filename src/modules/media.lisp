;;;; media.lisp  --  Now-playing indicator via playerctl.
;;;;
;;;; `playerctl' is the lingua franca for MPRIS2 on Linux: it speaks
;;;; D-Bus on our behalf, so we don't have to.  Apt: `playerctl'.
;;;; Without it the module silently produces NIL.

(in-package #:lispbar)

(defvar *media-format* :artist-title
  "One of :artist-title, :title-only, :short.")

(defvar *media-max-length* 60
  "Maximum characters before the now-playing text is truncated.")

(defun playerctl (&rest args)
  "Invoke playerctl with ARGS; return the trimmed stdout, or NIL."
  (let ((out (apply #'run-capture "playerctl" args)))
    (and out (string-trim '(#\Space #\Tab #\Newline #\Return) out))))

(defun media-truncate (s)
  (if (> (length s) *media-max-length*)
      (concatenate 'string (subseq s 0 (max 0 (1- *media-max-length*))) "…")
      s))

(defmodule :media (:doc "Now-playing artist/title from any MPRIS player."
                   :position :center :priority 40 :interval 3.0)
  (let ((status (playerctl "status")))
    (when (and status (or (string= status "Playing") (string= status "Paused")))
      (let* ((artist (playerctl "metadata" "artist"))
             (title  (playerctl "metadata" "title"))
             (text   (case *media-format*
                       (:title-only   (and title (not (zerop (length title))) title))
                       (:short        (or title artist))
                       (t (cond
                            ((and artist title
                                  (plusp (length artist))
                                  (plusp (length title)))
                             (format nil "~a - ~a" artist title))
                            (title artist)
                            (t title))))))
        (when (and text (plusp (length text)))
          (let ((prefix (if (string= status "Paused") "❚❚ " "▶ ")))
            (media-truncate (concatenate 'string prefix text))))))))
