;;;; main.lisp  --  Entry point for the standalone `lispbar' binary.

(in-package #:lispbar)

(defvar *cli-help* "usage: lispbar [OPTIONS]

  -c, --config PATH    Path to a config file (default: ~/.config/lispbar/config.lisp)
  -o, --output KIND    Override output: stdout | json | wayland
      --once           Render one frame and exit (handy for scripts)
      --list-modules   Print every registered module and exit
      --list-themes    Print every registered theme and exit
  -v, --verbose        Enable debug logging
  -h, --help           Show this help and exit
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
              ((string= a "--once")           (setf (getf opts :once) t))
              ((string= a "--list-modules")   (setf (getf opts :list-modules) t))
              ((string= a "--list-themes")    (setf (getf opts :list-themes) t))
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
  (format t "Registered modules:~%")
  (dolist (name (module-names))
    (let ((doc (getf (find-module-factory name) :doc)))
      (format t "  ~16a ~a~%" (string-downcase (symbol-name name)) doc))))

(defun do-list-themes ()
  (format t "Themes are not yet wired into the native build.~%")
  (format t "Faces will be configured in config.lisp once the Wayland~%")
  (format t "renderer lands; see README.md for the roadmap.~%"))

(defun main (&optional (argv (uiop:command-line-arguments)))
  "Native Lispbar entry point.  Returns an integer exit code."
  (let ((opts (parse-args argv)))
    (when (getf opts :verbose) (setf *log-level* :debug))
    (cond
      ((getf opts :help)         (write-string *cli-help*) (return-from main 0))
      ((getf opts :list-modules) (do-list-modules) (return-from main 0))
      ((getf opts :list-themes)  (do-list-themes)  (return-from main 0)))
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
