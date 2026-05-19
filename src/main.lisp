;;;; main.lisp  --  Entry point for the standalone `lispbar' binary.

(in-package #:lispbar)

(defvar *cli-help* "usage: lispbar [OPTIONS]

  -c, --config PATH      Path to a config file (default: XDG-discovered)
  -o, --output KIND      Override output: stdout | json | wayland
      --once             Render one frame and exit
      --init             Seed $XDG_CONFIG_HOME/lispbar/ with the default
                         config + empty modules/ and themes/ dirs, then exit.
                         Use --init --force to overwrite an existing config.
      --force            Used with --init to replace an existing config
      --no-seed          Skip the automatic first-run seeding
                         (equivalent to setting $LISPBAR_NO_SEED)
      --list-modules     Print every registered module and exit
      --list-themes      Print every registered theme and exit
      --show-extensions  Print which XDG dirs/files were loaded and exit
      --print-paths      Print XDG path discovery and exit
  -v, --verbose          Enable debug logging
  -h, --help             Show this help and exit

Subcommands:
  registry SUBCMD [...]  Browse and install from the official module /
                         theme registry.  `lispbar registry help' for
                         usage.
")

(defun parse-args (argv)
  "Return a plist of parsed CLI options.  Unknown flags are ignored."
  (let ((opts '()))
    (loop with rest = argv
          while rest
          for a = (pop rest) do
            (cond
              ((or (string= a "-h") (string= a "--help"))
               (setf (getf opts :help) t))
              ((or (string= a "-v") (string= a "--verbose"))
               (setf (getf opts :verbose) t))
              ((or (string= a "-c") (string= a "--config"))
               (setf (getf opts :config) (pop rest)))
              ((or (string= a "-o") (string= a "--output"))
               (setf (getf opts :output)
                     (intern (string-upcase (pop rest)) :keyword)))
              ((string= a "--once")            (setf (getf opts :once) t))
              ((string= a "--init")            (setf (getf opts :init) t))
              ((string= a "--force")           (setf (getf opts :force) t))
              ((string= a "--no-seed")         (setf (getf opts :no-seed) t))
              ((string= a "--list-modules")    (setf (getf opts :list-modules) t))
              ((string= a "--list-themes")     (setf (getf opts :list-themes) t))
              ((string= a "--show-extensions") (setf (getf opts :show-extensions) t))
              ((string= a "--print-paths")     (setf (getf opts :print-paths) t))
              (t
               (format *error-output* "lispbar: unknown option ~a~%" a))))
    opts))

(defun install-signal-handlers ()
  "Make SIGINT / SIGTERM flip `*running*' for graceful exit."
  #+sbcl
  (flet ((bye (&rest _) (declare (ignore _)) (setf *running* nil)))
    (sb-sys:enable-interrupt sb-unix:sigint  #'bye)
    (sb-sys:enable-interrupt sb-unix:sigterm #'bye)))

(defun do-list-modules ()
  (load-extensions)
  (format t "Registered modules:~%")
  (dolist (name (module-names))
    (let ((doc (getf (find-module-factory name) :doc)))
      (format t "  ~16a ~a~%" (string-downcase (symbol-name name)) doc))))

(defun do-list-themes ()
  (load-extensions)
  (format t "Registered themes:~%")
  (dolist (name (theme-names))
    (format t "  ~a~%" (string-downcase (symbol-name name)))))

(defun do-print-paths ()
  (format t "XDG paths (resolved):~%")
  (format t "  XDG_CONFIG_HOME : ~a~%" (xdg-config-home))
  (format t "  XDG_DATA_HOME   : ~a~%" (xdg-data-home))
  (format t "  XDG_STATE_HOME  : ~a~%" (xdg-state-home))
  (format t "  XDG_CACHE_HOME  : ~a~%" (xdg-cache-home))
  (format t "  XDG_CONFIG_DIRS : ~{~a~^, ~}~%" (xdg-config-dirs))
  (format t "  XDG_DATA_DIRS   : ~{~a~^, ~}~%" (xdg-data-dirs))
  (format t "Lispbar paths:~%")
  (format t "  config file     : ~a~%"
          (or (lispbar-config-file)
              (format nil "(not found; would be ~a)" (lispbar-default-config-path))))
  (format t "  modules dirs    : ~{~%    ~a~}~%"
          (or (lispbar-extension-directories "modules") '("(none)")))
  (format t "  themes  dirs    : ~{~%    ~a~}~%"
          (or (lispbar-extension-directories "themes") '("(none)"))))

(defun do-show-extensions ()
  (load-extensions)
  (format t "Loaded extension files:~%")
  (if *loaded-extension-files*
      (dolist (f *loaded-extension-files*) (format t "  ~a~%" f))
      (format t "  (none)~%"))
  (format t "~%Registered modules:~%")
  (dolist (name (module-names))
    (format t "  ~a~%" (string-downcase (symbol-name name))))
  (format t "~%Registered themes:~%")
  (dolist (name (theme-names))
    (format t "  ~a~%" (string-downcase (symbol-name name)))))

(defun do-init (&key force)
  "Explicit `lispbar --init': seed user config + XDG dirs."
  (let ((file (seed-user-config :force force)))
    (cond
      (file
       (format t "Created ~a~%" file)
       (let ((root (merge-pathnames "lispbar/" (xdg-config-home))))
         (format t "User extension dirs:~%")
         (format t "  ~a~%" (merge-pathnames "modules/" root))
         (format t "  ~a~%" (merge-pathnames "themes/" root)))
       0)
      ((not force)
       (format *error-output* "lispbar: config already exists at ~a~%"
               (or (lispbar-config-file)
                   (merge-pathnames "lispbar/config.lisp" (xdg-config-home))))
       (format *error-output* "  use --init --force to overwrite~%")
       1)
      (t
       (format *error-output* "lispbar: --init failed (see warnings above)~%")
       1))))

(defun split-subcommand (argv name)
  "Walk ARGV.  If NAME appears as the first non-flag positional, return
   (VALUES T GLOBAL-FLAGS SUB-ARGS) where GLOBAL-FLAGS are the args
   before NAME and SUB-ARGS are the args after.  Returns (VALUES NIL …)
   if NAME is not the first positional, so global flags can appear on
   either side: `lispbar -v -c CFG registry install :weather' works."
  (loop with rest = argv
        with before = nil
        while rest
        for a = (pop rest) do
          (cond
            ;; Flags that consume the next arg as their value.
            ((or (string= a "-c") (string= a "--config")
                 (string= a "-o") (string= a "--output"))
             (push a before)
             (when rest (push (pop rest) before)))
            ;; Standalone flags — skip and keep looking.
            ((and (>= (length a) 1) (char= (char a 0) #\-))
             (push a before))
            ;; First positional: is it our subcommand?
            ((string= a name)
             (return (values t (nreverse before) rest)))
            ;; Some other positional first — not us.
            (t (return (values nil nil nil))))
        finally (return (values nil nil nil))))

(defun main (&optional (argv (uiop:command-line-arguments)))
  "Native Lispbar entry point.  Returns an integer exit code."
  ;; `lispbar registry …' branches early; it has its own arg parser,
  ;; but it shares the global flags so `-c CFG' and `-v' still apply.
  (multiple-value-bind (found global-flags sub-args)
      (split-subcommand argv "registry")
    (when found
      (let ((opts (parse-args global-flags)))
        (when (getf opts :verbose) (setf *log-level* :debug))
        (return-from main (do-registry sub-args
                                       :config (getf opts :config))))))
  (let ((opts (parse-args argv)))
    (when (getf opts :verbose) (setf *log-level* :debug))
    (when (getf opts :no-seed) (setf *seed-disabled* t))
    (cond
      ((getf opts :help)             (write-string *cli-help*) (return-from main 0))
      ((getf opts :init)             (return-from main (do-init :force (getf opts :force))))
      ((getf opts :print-paths)      (do-print-paths)      (return-from main 0))
      ((getf opts :show-extensions)  (do-show-extensions)  (return-from main 0))
      ((getf opts :list-modules)     (do-list-modules)     (return-from main 0))
      ((getf opts :list-themes)      (do-list-themes)      (return-from main 0)))
    (install-signal-handlers)
    (load-config (getf opts :config))
    (when (getf opts :output)
      (setf (getf *config* :output) (getf opts :output)))
    (cond
      ((getf opts :once)
       (let ((instances (build-instances *config*)))
         (write-line (if (eq (getf *config* :output) :json)
                         (render-json-line instances)
                         (render-text-line instances)))
         (force-output)))
      (t (run-output *config*))))
  0)
