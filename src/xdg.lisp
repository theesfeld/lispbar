;;;; xdg.lisp  --  XDG Base Directory Specification helpers.
;;;;
;;;; All Lispbar paths are derived through these accessors so the
;;;; binary is fully XDG-compliant: user files live under
;;;; $XDG_CONFIG_HOME / $XDG_DATA_HOME, system files are searched
;;;; under $XDG_CONFIG_DIRS / $XDG_DATA_DIRS, state under
;;;; $XDG_STATE_HOME, cache under $XDG_CACHE_HOME.

(in-package #:lispbar)

(defun home-pathname ()
  (uiop:ensure-directory-pathname (user-homedir-pathname)))

(defun xdg--env-or-default (var fallback-subdir)
  "Return the directory pathname denoted by VAR, or HOME/FALLBACK-SUBDIR."
  (let ((v (uiop:getenv var)))
    (if (and v (plusp (length v)))
        (uiop:ensure-directory-pathname v)
        (uiop:ensure-directory-pathname
         (merge-pathnames fallback-subdir (home-pathname))))))

(defun xdg-config-home () (xdg--env-or-default "XDG_CONFIG_HOME" ".config/"))
(defun xdg-data-home   () (xdg--env-or-default "XDG_DATA_HOME"   ".local/share/"))
(defun xdg-state-home  () (xdg--env-or-default "XDG_STATE_HOME"  ".local/state/"))
(defun xdg-cache-home  () (xdg--env-or-default "XDG_CACHE_HOME"  ".cache/"))

(defun xdg--split-dirs (var fallback)
  "Return the list of directory pathnames in VAR, defaulting to FALLBACK
when unset.  Empty entries (allowed by the spec) are skipped."
  (let ((value (or (uiop:getenv var) fallback)))
    (loop for s in (uiop:split-string value :separator '(#\:))
          when (plusp (length s))
          collect (uiop:ensure-directory-pathname s))))

(defun xdg-config-dirs ()
  (xdg--split-dirs "XDG_CONFIG_DIRS" "/etc/xdg"))

(defun xdg-data-dirs ()
  (xdg--split-dirs "XDG_DATA_DIRS" "/usr/local/share:/usr/share"))

(defun xdg-config-path ()
  "All directories to search for config files, user first."
  (cons (xdg-config-home) (xdg-config-dirs)))

(defun xdg-data-path ()
  "All directories to search for data files, user first."
  (cons (xdg-data-home) (xdg-data-dirs)))

;;; ---- Lispbar-specific path helpers ----

(defun lispbar-config-file ()
  "Return the first lispbar/config.lisp found in the XDG config path,
or NIL if no config file exists anywhere."
  (loop for dir in (xdg-config-path)
        for path = (merge-pathnames "lispbar/config.lisp" dir)
        when (probe-file path) return path))

(defun lispbar-extension-directories (subdir)
  "Return every existing lispbar/SUBDIR directory across the XDG search
path.  Both config and data trees are searched, in order:

    $XDG_CONFIG_HOME/lispbar/SUBDIR/    (highest priority, per-user)
    $XDG_CONFIG_DIRS .../lispbar/SUBDIR/
    $XDG_DATA_HOME/lispbar/SUBDIR/
    $XDG_DATA_DIRS .../lispbar/SUBDIR/   (lowest priority, system-wide)

This is suitable for things like modules/, themes/, and palettes/."
  (let ((result nil))
    (dolist (root (append (xdg-config-path) (xdg-data-path)))
      (let ((d (merge-pathnames (format nil "lispbar/~a/" subdir) root)))
        (when (uiop:directory-exists-p d)
          (push d result))))
    (nreverse result)))

(defun lisp-files-in-directory (dir)
  "Return the list of *.lisp file pathnames directly under DIR (no recursion),
sorted alphabetically so loading is deterministic."
  (when (uiop:directory-exists-p dir)
    (sort (directory (merge-pathnames "*.lisp" dir))
          #'string<
          :key #'namestring)))

(defun lispbar-state-directory ()
  "Return $XDG_STATE_HOME/lispbar/, creating it on first use."
  (let ((d (merge-pathnames "lispbar/" (xdg-state-home))))
    (ensure-directories-exist d)
    d))

(defun lispbar-cache-directory ()
  "Return $XDG_CACHE_HOME/lispbar/, creating it on first use."
  (let ((d (merge-pathnames "lispbar/" (xdg-cache-home))))
    (ensure-directories-exist d)
    d))

(defun lispbar-default-config-path ()
  "Return $XDG_CONFIG_HOME/lispbar/config.lisp (whether it exists or not).
Used in diagnostics so the user knows where to put their config."
  (merge-pathnames "lispbar/config.lisp" (xdg-config-home)))
