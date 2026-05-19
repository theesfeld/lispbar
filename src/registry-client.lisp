;;;; registry-client.lisp -- `lispbar registry …' subcommand.
;;;;
;;;; Fetches the manifest from the upstream registry, lets users
;;;; browse / install / remove / update modules and themes.
;;;; Everything happens through `curl' so we don't drag in an HTTP
;;;; client library; every Linux distro ships curl.
;;;;
;;;; Install layout:
;;;;
;;;;   $XDG_CONFIG_HOME/lispbar/modules/<name>.lisp   downloaded module
;;;;   $XDG_CONFIG_HOME/lispbar/themes/<name>.lisp    downloaded theme
;;;;   $XDG_STATE_HOME/lispbar/registry-installed.lisp
;;;;                                                  bookkeeping
;;;;
;;;; The state file is itself a Lisp source the binary loads on
;;;; demand; one (installed …) form per installed item.

(in-package #:lispbar)

(defvar *registry-base-url*
  "https://raw.githubusercontent.com/theesfeld/lispbar/master/registry"
  "Where the registry lives.  Override to point at a fork or a
local mirror.  Must serve `manifest.lisp', `modules/*.lisp', and
`themes/*.lisp' under that prefix.")

(defvar *registry-cache-ttl* 3600
  "Seconds before the cached manifest is considered stale.  Refreshed
on demand by any of list / info / install.")

;;; ---- Manifest in-memory model ----

(defvar *registry-items* (make-hash-table :test #'eq)
  "NAME -> plist with the manifest's per-item fields:
   :kind :file :sha256 :summary :description :version :license
   :homepage :tags :added :updated :author :requires.")

(defvar *registry-loaded* nil
  "Universal time of the last manifest load, or NIL if not loaded.")

;;; The registry's (register …) form is parsed in a tiny dedicated
;;; package so the manifest doesn't collide with the real :lispbar
;;; symbols.

(defpackage #:lispbar-registry
  (:use #:common-lisp)
  (:export #:register))

(in-package #:lispbar-registry)

(defvar *items* (make-hash-table :test #'eq))

(defun register (name &rest plist)
  (setf (gethash name *items*) plist)
  name)

(in-package #:lispbar)

(defun registry-cache-path ()
  (merge-pathnames "registry-manifest.lisp" (lispbar-cache-directory)))

(defun registry-state-path ()
  (merge-pathnames "registry-installed.lisp" (lispbar-state-directory)))

(defun registry-fetch-to (url dest)
  "Download URL to DEST.  Returns DEST on success, NIL on failure."
  (let* ((tmp (uiop:tmpize-pathname dest)))
    (handler-case
        (let ((status (uiop:run-program
                        (list "curl" "-fsSL"
                              "--max-time" "10"
                              "--output" (namestring tmp)
                              url)
                        :ignore-error-status t
                        :output nil :error-output nil)))
          (declare (ignore status))
          (cond
            ((probe-file tmp)
             (ensure-directories-exist dest)
             (rename-file tmp dest)
             dest)
            (t nil)))
      (error (c)
        (logmsg :warn "registry: curl failed for ~a: ~a" url c)
        nil))))

(defun registry-refresh (&key force)
  "Download manifest.lisp to the cache if it's missing or stale."
  (let* ((path (registry-cache-path))
         (age  (if (probe-file path)
                   (- (get-universal-time)
                      (file-write-date path))
                   most-positive-fixnum)))
    (when (or force (>= age *registry-cache-ttl*))
      (let ((url (format nil "~a/manifest.lisp" *registry-base-url*)))
        (logmsg :info "registry: fetching ~a" url)
        (registry-fetch-to url path)))
    path))

(defun registry-load ()
  "Read the cached manifest into `*registry-items*'."
  (let ((path (registry-refresh)))
    (clrhash lispbar-registry::*items*)
    (cond
      ((or (null path) (not (probe-file path)))
       (logmsg :warn "registry: no cached manifest available"))
      (t
       (handler-case
           (with-open-file (s path :direction :input)
             (let ((*package* (find-package :lispbar-registry)))
               (loop for form = (read s nil :eof)
                     until (eq form :eof)
                     do (eval form))))
         (error (c)
           (logmsg :warn "registry: failed to parse manifest: ~a" c)))))
    ;; Mirror lispbar-registry::*items* into our hash table; users
    ;; don't need to know the parsing-package detail.
    (clrhash *registry-items*)
    (maphash (lambda (k v) (setf (gethash k *registry-items*) v))
             lispbar-registry::*items*)
    (setf *registry-loaded* (get-universal-time))))

(defun registry-items-sorted (&optional kind-filter)
  (let (items)
    (maphash (lambda (k v)
               (when (or (null kind-filter)
                         (eq (getf v :kind) kind-filter))
                 (push (cons k v) items)))
             *registry-items*)
    (sort items #'string< :key (lambda (x) (symbol-name (car x))))))

;;; ---- State (what's installed) ----

(defvar *registry-installed* (make-hash-table :test #'eq)
  "NAME -> plist (:kind :file :sha256 :installed-at).")

(defun registry-state-load ()
  (clrhash *registry-installed*)
  (let ((path (registry-state-path)))
    (when (probe-file path)
      (handler-case
          (with-open-file (s path :direction :input)
            (let ((*package* (find-package :lispbar)))
              (loop for form = (read s nil :eof)
                    until (eq form :eof)
                    do (when (and (consp form) (eq (car form) 'installed))
                         (destructuring-bind (name &rest plist) (cdr form)
                           (setf (gethash name *registry-installed*)
                                 plist))))))
        (error (c)
          (logmsg :warn "registry: failed to parse state: ~a" c))))))

(defun registry-state-save ()
  (let ((path (registry-state-path)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format s ";;;; ~a~%~%" path)
      (format s ";; Generated by `lispbar registry'.  Don't edit by hand;~%")
      (format s ";; the bookkeeping here is rewritten on every install/remove.~%~%")
      (let (entries)
        (maphash (lambda (k v) (push (cons k v) entries))
                 *registry-installed*)
        (dolist (e (sort entries #'string< :key (lambda (x) (symbol-name (car x)))))
          (format s "(installed ~s ~{ ~s~})~%" (car e) (cdr e)))))))

;;; ---- Install / remove ----

(defun sha256-of-file (path)
  "Compute SHA-256 of PATH via the system's `sha256sum'."
  (let ((out (run-capture "sha256sum" (namestring path))))
    (and out (first (uiop:split-string
                      (string-trim '(#\Space #\Newline) out)
                      :separator '(#\Space))))))

(defun registry-display-name (name)
  "Return a user-facing string like \":weather\" for the keyword NAME."
  (format nil ":~(~a~)" (symbol-name name)))

(defun registry-install (name)
  "Download item NAME, verify, place under XDG_CONFIG_HOME."
  (registry-load)
  (registry-state-load)
  (let ((entry (gethash name *registry-items*))
        (disp  (registry-display-name name)))
    (cond
      ((null entry)
       (format *error-output* "lispbar: no such registry item: ~a~%" disp)
       1)
      (t
       (let* ((kind   (getf entry :kind))
              (file   (getf entry :file))
              (sha    (getf entry :sha256))
              (subdir (case kind
                        (:module "modules/")
                        (:theme  "themes/")
                        (t (return-from registry-install
                             (progn
                               (format *error-output*
                                       "lispbar: unknown :kind ~s for ~a~%"
                                       kind disp) 1)))))
              (dest   (merge-pathnames
                        (format nil "~a~(~a~).lisp" subdir
                                (symbol-name name))
                        (merge-pathnames "lispbar/"
                                          (xdg-config-home))))
              (url    (format nil "~a/~a" *registry-base-url* file)))
         (format t "Downloading ~a from ~a~%" disp url)
         (let ((got (registry-fetch-to url dest)))
           (cond
             ((null got)
              (format *error-output* "lispbar: download failed for ~a~%" disp)
              1)
             (t
              (let ((actual (sha256-of-file dest)))
                (cond
                  ((null actual)
                   (format *error-output*
                           "lispbar: sha256sum not available; cannot verify~%")
                   1)
                  ((not (string= actual sha))
                   (delete-file dest)
                   (format *error-output*
                           "lispbar: checksum mismatch for ~a~%  expected ~a~%  got      ~a~%"
                           disp sha actual)
                   1)
                  (t
                   (setf (gethash name *registry-installed*)
                         (list :kind kind :file (namestring dest)
                               :sha256 sha
                               :installed-at (get-universal-time)))
                   (registry-state-save)
                   (format t "Installed ~a to ~a~%" disp dest)
                   (case kind
                     (:module (format t "  add ~a to a placement list to enable.~%" disp))
                     (:theme  (format t "  set (theme ~a) in your config to apply.~%" disp)))
                   0)))))))))))

(defun registry-remove (name)
  "Delete an installed item's file and forget it in the state."
  (registry-state-load)
  (let ((info (gethash name *registry-installed*))
        (disp (registry-display-name name)))
    (cond
      ((null info)
       (format *error-output* "lispbar: ~a is not installed~%" disp)
       1)
      (t
       (let ((file (getf info :file)))
         (when (and file (probe-file file))
           (delete-file file)))
       (remhash name *registry-installed*)
       (registry-state-save)
       (format t "Removed ~a~%" disp)
       0))))

(defun registry-update-installed ()
  "Re-download every installed item, replacing local copies whose
sha differs from the manifest's."
  (registry-load)
  (registry-state-load)
  (let ((touched 0) (ok 0) (bad 0))
    (maphash
     (lambda (name info)
       (incf touched)
       (let* ((entry  (gethash name *registry-items*))
              (latest (and entry (getf entry :sha256)))
              (disp   (registry-display-name name)))
         (cond
           ((null entry)
            (format t "  ~a is no longer in the registry; leaving file alone.~%" disp))
           ((string= latest (getf info :sha256))
            (incf ok)
            (format t "  ~a unchanged.~%" disp))
           (t
            (case (registry-install name)
              (0 (incf ok))
              (otherwise (incf bad)))))))
     *registry-installed*)
    (format t "~%~d touched, ~d ok, ~d failed.~%" touched ok bad)
    (if (zerop bad) 0 1)))

;;; ---- Interactive browser (fzf) ----

(defvar *registry-browse-tool* "fzf"
  "External fuzzy-finder used by `lispbar registry browse'.  fzf is
the default; set to NIL to disable interactive browse entirely.")

(defun registry-browse-lines ()
  "Build the line-per-item input that gets piped into fzf."
  (with-output-to-string (s)
    (dolist (cell (registry-items-sorted))
      (let* ((name (car cell))
             (v (cdr cell))
             (installed-p (gethash name *registry-installed*)))
        (format s "~a ~14a ~8a ~a~%"
                (if installed-p "*" "·")
                (registry-display-name name)
                (string-downcase (symbol-name (getf v :kind)))
                (or (getf v :summary) (getf v :doc) ""))))))

(defun registry-browse ()
  "Open an interactive fzf picker over the registry.  Enter installs
the highlighted item (re-downloading if it's already installed)."
  (cond
    ((or (null *registry-browse-tool*)
         (not (executable-find-check *registry-browse-tool*)))
     (format *error-output*
             "lispbar: `registry browse' needs ~a on PATH.~%~
              Install fzf (https://github.com/junegunn/fzf) or use~%~
              `lispbar registry list' / `install' instead.~%"
             (or *registry-browse-tool* "fzf"))
     1)
    (t
     (registry-load)
     (cond
       ((zerop (hash-table-count *registry-items*))
        (format *error-output*
                "lispbar: registry is empty or unreachable.  Check ~a~%"
                *registry-base-url*)
        1)
       (t
        (registry-state-load)
        ;; Pipe candidates through real files instead of Lisp streams.
        ;; fzf needs to talk to /dev/tty for its TUI; with `:input <stream>'
        ;; + `:output :string' SBCL inserts pipes around fzf's stdio and
        ;; the TUI never paints.  File-backed stdin/stdout sidesteps it.
        (let* ((cache  (lispbar-cache-directory))
               (in-f   (merge-pathnames "browse-input.txt"  cache))
               (out-f  (merge-pathnames "browse-output.txt" cache))
               (input  (registry-browse-lines)))
          (ensure-directories-exist in-f)
          (with-open-file (s in-f :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string input s))
          (when (probe-file out-f) (delete-file out-f))
          (handler-case
              (uiop:run-program
               (list *registry-browse-tool*
                     "--prompt=registry> "
                     "--header"
                     "Enter installs/refreshes  Esc cancels  * = installed"
                     "--preview" "lispbar registry info {2}"
                     "--preview-window=right:55%:wrap"
                     "--height=80%"
                     "--reverse"
                     "--no-mouse")
               :input  in-f
               :output out-f
               :error-output :interactive
               :ignore-error-status t)
            (error (c)
              (logmsg :warn "registry browse: ~a" c)))
          (let ((selection (when (probe-file out-f)
                             (uiop:read-file-string out-f))))
            (ignore-errors (delete-file in-f))
            (ignore-errors (delete-file out-f))
            (cond
              ((or (null selection)
                   (zerop (length (string-trim '(#\Space #\Newline) selection))))
               0)
              (t
               (let* ((line  (first (uiop:split-string
                                      selection :separator '(#\Newline))))
                      (toks  (remove-if (lambda (x) (zerop (length x)))
                                        (uiop:split-string
                                          line :separator '(#\Space #\Tab))))
                      (name  (and (>= (length toks) 2)
                                  (cli-name->keyword (second toks)))))
                 (cond
                   ((null name)
                    (format *error-output* "lispbar: couldn't parse selection: ~s~%"
                            line)
                    1)
                   (t (registry-install name)))))))))))))

;;; ---- CLI ----

(defun registry-print-list (&optional kind-filter)
  (registry-load)
  (cond
    ((zerop (hash-table-count *registry-items*))
     (format *error-output*
             "lispbar: registry is empty or unreachable.  Check ~a~%"
             *registry-base-url*)
     1)
    (t
     (registry-state-load)
     (dolist (cell (registry-items-sorted kind-filter))
       (let* ((name (car cell))
              (v (cdr cell))
              (installed-p (gethash name *registry-installed*))
              (mark (if installed-p "*" " ")))
         (format t "~a ~12a ~10a ~a~%"
                 mark
                 (string-downcase (symbol-name (getf v :kind)))
                 (string-downcase (symbol-name name))
                 (or (getf v :summary) (getf v :doc) ""))))
     (format t "~%* = installed.  Use `lispbar registry install NAME' / `remove NAME'.~%")
     0)))

(defun registry-print-info (name)
  (registry-load)
  (registry-state-load)
  (let ((v (gethash name *registry-items*))
        (disp (registry-display-name name)))
    (cond
      ((null v)
       (format *error-output* "lispbar: no such registry item: ~a~%" disp)
       1)
      (t
       (let ((installed (gethash name *registry-installed*))
             (description (getf v :description)))
         (format t "Name        : ~a~%" disp)
         (format t "Kind        : ~(~a~)~%" (getf v :kind))
         (format t "Summary     : ~a~%"
                 (or (getf v :summary) (getf v :doc) ""))
         (format t "Version     : ~a~%" (or (getf v :version) "(unspecified)"))
         (format t "License     : ~a~%" (or (getf v :license) "(unspecified)"))
         (format t "Author      : ~a~%" (or (getf v :author) "(unknown)"))
         (when (getf v :homepage)
           (format t "Homepage    : ~a~%" (getf v :homepage)))
         (format t "Tags        : ~{~a~^, ~}~%"
                 (or (getf v :tags) '("(none)")))
         (format t "Added       : ~a~%" (or (getf v :added) "(unknown)"))
         (format t "Updated     : ~a~%" (or (getf v :updated) "(unknown)"))
         (format t "Requires    : ~{~a~^, ~}~%"
                 (or (getf v :requires) '("(nothing external)")))
         (format t "File        : ~a~%" (getf v :file))
         (format t "URL         : ~a/~a~%" *registry-base-url* (getf v :file))
         (format t "SHA-256     : ~a~%" (getf v :sha256))
         (format t "Installed   : ~a~%"
                 (cond (installed
                        (format nil "yes — ~a" (getf installed :file)))
                       (t "no")))
         (when description
           (format t "~%Description :~%")
           (dolist (line (uiop:split-string description :separator '(#\Newline)))
             (format t "  ~a~%" line)))
         0)))))

(defun parse-registry-args (argv)
  "Pop registry subcommand + flags from the front of ARGV.  Returns
a plist of decisions."
  (let (opts)
    (loop while argv
          for a = (pop argv) do
            (cond
              ((or (string= a "--kind") (string= a "-k"))
               (setf (getf opts :kind)
                     (intern (string-upcase (pop argv)) :keyword)))
              ((or (string= a "--force") (string= a "-f"))
               (setf (getf opts :force) t))
              (t (push a (getf opts :positional)))))
    (setf (getf opts :positional)
          (nreverse (getf opts :positional)))
    opts))

(defun cli-name->keyword (s)
  "Turn a CLI argument like \":weather\" or \"weather\" into the
keyword `:WEATHER'.  Strips a leading colon if present."
  (let ((trimmed (if (and (plusp (length s)) (char= (char s 0) #\:))
                     (subseq s 1)
                     s)))
    (intern (string-upcase trimmed) :keyword)))

(defun do-registry (rest-argv &key config)
  "Entry point invoked from `main' when the first arg is `registry'."
  ;; Load the user config first so `*registry-base-url*' / cache TTL
  ;; can be overridden via `(setf …)' from config.lisp.  Errors here
  ;; are non-fatal — registry commands should still work without a
  ;; valid config.
  (handler-case (load-config config)
    (error (c) (logmsg :warn "registry: skipping config load: ~a" c)))
  (let* ((opts (parse-registry-args rest-argv))
         (pos  (getf opts :positional))
         (sub  (first pos))
         (item (second pos)))
    (cond
      ((null sub) (registry-help) 0)
      ((string= sub "list")
       (registry-print-list (getf opts :kind)))
      ((or (string= sub "browse") (string= sub "select"))
       (registry-browse))
      ((string= sub "info")
       (cond (item (registry-print-info
                      (cli-name->keyword item)))
             (t (format *error-output* "lispbar: registry info NAME~%") 1)))
      ((string= sub "install")
       (cond (item (registry-install
                      (cli-name->keyword item)))
             (t (format *error-output* "lispbar: registry install NAME~%") 1)))
      ((or (string= sub "remove") (string= sub "uninstall"))
       (cond (item (registry-remove
                      (cli-name->keyword item)))
             (t (format *error-output* "lispbar: registry remove NAME~%") 1)))
      ((string= sub "update")
       (registry-update-installed))
      ((or (string= sub "help") (string= sub "-h") (string= sub "--help"))
       (registry-help) 0)
      (t
       (format *error-output* "lispbar: unknown registry subcommand: ~a~%" sub)
       (registry-help)
       1))))

(defun registry-help ()
  (write-string
   "usage: lispbar registry SUBCOMMAND [OPTIONS]

Subcommands:
  list [--kind module|theme]   Browse the registry inventory.
  browse                       Interactive fzf picker (preview + install).
  info NAME                    Show details for one item.
  install NAME                 Download and verify into XDG_CONFIG_HOME.
  remove  NAME                 Delete an installed item.
  update                       Refresh installed items if upstream changed.

NAME is a keyword form like :weather or :tokyo-night (the leading
colon is optional on the command line).

Examples:
  lispbar registry list
  lispbar registry list --kind theme
  lispbar registry info :weather
  lispbar registry install :weather
  lispbar registry remove :weather
"))
