;;;; diskspace.lisp - Free space on a chosen mount point.

(in-package :lispbar)

(defvar *diskspace-mount* "/"
  "Mount point to monitor.  Common: `/' `/home' `/data'.")

(defvar *diskspace-warn-percent*   85
  "Used-percent threshold for :warn face.")
(defvar *diskspace-urgent-percent* 95
  "Used-percent threshold for :urgent face.")

(defun diskspace-used-percent ()
  "Return (USED-PCT AVAIL-HUMAN) for *diskspace-mount*, or NIL."
  (let ((out (run-capture "df" "-h" "--output=pcent,avail" *diskspace-mount*)))
    (when out
      (let* ((lines (uiop:split-string out :separator '(#\Newline)))
             (data  (second lines)))
        (when (and data (plusp (length (string-trim '(#\Space) data))))
          (let* ((tokens (uiop:split-string (string-trim '(#\Space) data)
                                              :separator '(#\Space #\Tab)))
                 (pct (parse-integer (string-right-trim "%" (first tokens))
                                      :junk-allowed t)))
            (and pct (list pct (second tokens)))))))))

(defmodule :diskspace
  (:doc "Free space on a chosen mount point"
   :position :right :priority 53 :interval 60.0
   :on-click ((:left "xdg-open ~"))
   :tooltip
   (lambda ()
     (let ((info (diskspace-used-percent)))
       (and info (format nil "~a free on ~a (~d% used)"
                          (second info) *diskspace-mount* (first info))))))
  (let ((info (diskspace-used-percent)))
    (when info
      (destructuring-bind (pct avail) info
        (list :text (format nil "DISK ~a" avail)
              :face (cond ((>= pct *diskspace-urgent-percent*) :urgent)
                          ((>= pct *diskspace-warn-percent*)   :warn)
                          (t                                    :normal)))))))
