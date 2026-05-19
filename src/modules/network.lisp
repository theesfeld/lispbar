;;;; network.lisp  --  Active connection summary: WiFi SSID + signal
;;;; bars, plain Ethernet, or "OFF" when nothing's up.
;;;;
;;;; Source cascade (first one that works):
;;;;
;;;;   1. nmcli              (NetworkManager - most desktop installs)
;;;;   2. /proc/net/wireless (raw kernel wireless stats)
;;;;   3. ip / fallback      (just "ETH" if a default route exists)
;;;;
;;;; All public tunables are exposed as defvars so users can `setf'
;;;; them from config.lisp.

(in-package #:lispbar)

(defvar *network-format-wifi* "{ssid} {signal}%"
  "Format string for WiFi.  Recognised placeholders:
{ssid} {signal} {bars} {device}.")

(defvar *network-format-ethernet* "ETH"
  "Display string when the active connection is wired.")

(defvar *network-format-down* "OFF"
  "Display string when no connection is up.")

(defvar *network-show-bars* nil
  "When non-NIL, use a Unicode bar glyph for the signal (`{bars}'
expands to it; useful as a substitute for the raw percent).")

(defvar *network-bar-glyphs* "▁▂▃▄▅▆▇█"
  "Eight-step Unicode bar set used when *network-show-bars* is t.")

;;; ---- nmcli ----

(defun network--nmcli-active-connection ()
  "Return the (TYPE NAME DEVICE) of the first active NetworkManager
connection (in NM's own preference order), or NIL."
  (let ((out (run-capture "nmcli" "-t" "-f" "NAME,TYPE,DEVICE" "c" "show" "--active")))
    (when out
      (loop for line in (uiop:split-string out :separator '(#\Newline))
            for parts = (uiop:split-string line :separator '(#\:))
            when (and (>= (length parts) 3)
                      (plusp (length (first parts))))
            return (list (second parts) (first parts) (third parts))))))

(defun network--nmcli-wifi-signal (device)
  "Return (SSID SIGNAL%) for the WiFi DEVICE NetworkManager reports
as active, or NIL.  DEVICE may be NIL, in which case the first IN-USE
line wins."
  (let ((out (run-capture "nmcli" "-t" "-f" "IN-USE,SSID,SIGNAL,DEVICE"
                          "d" "wifi" "list" "--rescan" "no")))
    (when out
      (loop for line in (uiop:split-string out :separator '(#\Newline))
            for parts = (uiop:split-string line :separator '(#\:))
            when (and (>= (length parts) 4)
                      (string= (first parts) "*")
                      (or (null device)
                          (string= device (fourth parts))))
            return (list (second parts)
                         (parse-integer (third parts) :junk-allowed t))))))

(defun network-via-nmcli ()
  (let ((conn (network--nmcli-active-connection)))
    (when conn
      (destructuring-bind (type name device) conn
        (cond
          ((or (string= type "802-11-wireless")
               (string= type "wifi"))
           (let* ((sig (network--nmcli-wifi-signal device))
                  (ssid   (or (and sig (first sig)) name))
                  (signal (or (and sig (second sig)) 0))
                  (face   (cond ((>= signal 60) :ok)
                                ((>= signal 30) :warn)
                                (t              :urgent))))
             (list :text (network--format-wifi ssid signal device)
                   :face face)))
          ((or (string= type "802-3-ethernet")
               (string= type "ethernet"))
           (list :text *network-format-ethernet* :face :ok))
          (t
           ;; VPN, bridge, etc.  Show its connection name.
           (list :text name :face :accent)))))))

;;; ---- /proc/net/wireless (kernel-level fallback) ----

(defun network--read-proc-wireless ()
  "Return (IFACE QUALITY-PCT) for the first wireless interface
showing data in /proc/net/wireless, or NIL."
  (when (probe-file "/proc/net/wireless")
    (with-open-file (s "/proc/net/wireless" :direction :input)
      ;; First two lines are headers.
      (read-line s nil nil) (read-line s nil nil)
      (loop for line = (read-line s nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab) line)
            for parts = (uiop:split-string trimmed
                                            :separator '(#\Space #\Tab))
            when (and (>= (length parts) 3)
                      (find #\: (first parts)))
            return (let* ((iface (string-right-trim ":" (first parts)))
                          (link  (parse-integer (third parts)
                                                :junk-allowed t)))
                     (when link
                       (list iface (max 0 (min 100 (* 100 (/ link 70.0))))))))) ))

(defun network-via-proc ()
  (let ((data (network--read-proc-wireless)))
    (when data
      (let* ((iface (first data))
             (pct   (round (second data)))
             (face  (cond ((>= pct 60) :ok)
                          ((>= pct 30) :warn)
                          (t           :urgent))))
        (list :text (network--format-wifi iface pct iface)
              :face face)))))

;;; ---- default route (last resort) ----

(defun network-via-ip ()
  "Return ETHERNET text if there's a default route, NIL otherwise."
  (let ((out (run-capture "ip" "route" "show" "default")))
    (when (and out (search "default" out))
      (list :text *network-format-ethernet* :face :ok))))

;;; ---- formatting ----

(defun network--bar-glyph (percent)
  "Map PERCENT [0,100] to a single glyph from *network-bar-glyphs*."
  (let* ((n (length *network-bar-glyphs*))
         (idx (max 0 (min (1- n) (floor (* (/ percent 100.0) n))))))
    (string (char *network-bar-glyphs* idx))))

(defun network--format-wifi (ssid signal device)
  "Expand the templated *network-format-wifi*."
  (let ((s *network-format-wifi*))
    (setf s (substitute-substring s "{ssid}"   (or ssid "")))
    (setf s (substitute-substring s "{signal}" (format nil "~d" (or signal 0))))
    (setf s (substitute-substring s "{bars}"   (network--bar-glyph (or signal 0))))
    (setf s (substitute-substring s "{device}" (or device "")))
    s))

(defun substitute-substring (haystack needle replacement)
  "Return HAYSTACK with every NEEDLE replaced by REPLACEMENT."
  (with-output-to-string (out)
    (loop with start = 0
          for hit = (search needle haystack :start2 start)
          do (cond
               (hit (write-string haystack out :start start :end hit)
                    (write-string replacement out)
                    (setf start (+ hit (length needle))))
               (t   (write-string haystack out :start start)
                    (loop-finish))))))

;;; ---- The module ----

(defvar *network-on-click* "nm-connection-editor || iwgtk"
  "Shell command for left-click on the network module.  Defaults
to the common NetworkManager / iwd GUIs.  NIL to disable.")

(defun network-left-click (_m _b _i)
  (declare (ignore _m _b _i))
  (when (and *network-on-click* (plusp (length *network-on-click*)))
    (uiop:launch-program (list "sh" "-c" *network-on-click*))))

(defun network-tooltip ()
  "Detailed network state for the hover tooltip."
  (let ((conn (network--nmcli-active-connection)))
    (cond
      ((null conn) "No active connection - click to open network manager")
      (t (destructuring-bind (type name device) conn
           (let ((sig (and (or (string= type "wifi")
                                (string= type "802-11-wireless"))
                            (network--nmcli-wifi-signal device))))
             (cond
               (sig (format nil "WiFi: ~a   ~d% on ~a" (first sig) (second sig) device))
               (t   (format nil "~a (~a)" name device)))))))))

(defmodule :network
  (:doc "Active network connection (WiFi SSID + signal, Ethernet, VPN)."
   :position :right :priority 75 :interval 5.0
   :on-click ((:left network-left-click))
   :tooltip  network-tooltip)
  (or (network-via-nmcli)
      (network-via-proc)
      (network-via-ip)
      (list :text *network-format-down* :face :muted)))
