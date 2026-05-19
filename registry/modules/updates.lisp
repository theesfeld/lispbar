;;;; updates.lisp - Available package-manager updates counter.
;;;;
;;;; Detects which package manager you have and counts pending
;;;; updates without applying them.  Refreshes every 30 minutes by
;;;; default - bump *updates-interval* if you want more frequent
;;;; checks.  Left-click runs the configured upgrade command.

(in-package :lispbar)

(defvar *updates-interval* 1800.0
  "Seconds between checks.  Default: 30 minutes.")

(defvar *updates-upgrade-command* nil
  "Optional shell command for left-click; runs an interactive
upgrade in a terminal.  Examples:
    \"kitty -e sudo emerge -uDNav @world\"
    \"foot -e sudo pacman -Syu\"
    \"alacritty -e sudo apt upgrade\"")

(defun updates-count ()
  "Return how many updates are pending, or NIL on detection failure."
  (cond
    ;; Gentoo
    ((probe-file "/var/db/pkg/")
     (let ((out (run-capture "sh" "-c"
                              "emerge -puDN --color=n @world 2>/dev/null | grep -c '^\\[ebuild'")))
       (and out (parse-integer (string-trim '(#\Newline) out) :junk-allowed t))))
    ;; Arch
    ((probe-file "/var/lib/pacman/")
     (let ((out (run-capture "sh" "-c"
                              "checkupdates 2>/dev/null | wc -l")))
       (and out (parse-integer (string-trim '(#\Newline) out) :junk-allowed t))))
    ;; Debian / Ubuntu
    ((probe-file "/etc/apt/")
     (let ((out (run-capture "sh" "-c"
                              "apt list --upgradable 2>/dev/null | sed 1d | wc -l")))
       (and out (parse-integer (string-trim '(#\Newline) out) :junk-allowed t))))
    ;; Fedora
    ((probe-file "/etc/dnf/")
     (let ((out (run-capture "sh" "-c"
                              "dnf -q check-update 2>/dev/null | grep -cE '^[a-zA-Z]'")))
       (and out (parse-integer (string-trim '(#\Newline) out) :junk-allowed t))))))

(defun updates-click (_m _b _i) (declare (ignore _m _b _i))
  (when (and *updates-upgrade-command*
             (plusp (length *updates-upgrade-command*)))
    (uiop:launch-program (list "sh" "-c" *updates-upgrade-command*))))

(defmodule :updates
  (:doc "Pending OS package updates"
   :position :right :priority 38 :interval 1800.0
   :on-click ((:left updates-click))
   :tooltip "Pending package-manager updates")
  (let ((n (updates-count)))
    (cond
      ((null n) nil)
      ((zerop n) nil)                    ; quiet when nothing pending
      (t (list :text (format nil "UPD ~d" n)
               :face (cond ((>= n 50) :warn)
                           ((>= n 10) :accent)
                           (t          :normal)))))))
