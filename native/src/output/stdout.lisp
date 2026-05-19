;;;; stdout.lisp  --  Plain-text and JSON line-oriented output.
;;;
;;; Two protocols are supported on the same code path:
;;;
;;;   :stdout   one full bar line per tick:   "LEFT | CENTER | RIGHT"
;;;   :json     one waybar-style JSON object per tick (per module list)
;;;
;;; The JSON variant lets the binary serve as a custom module for
;;; waybar, eww, yambar, or any tool that consumes line-delimited JSON
;;; on a process's stdout - useful immediately, before the native
;;; Wayland renderer lands.

(in-package #:lispbar)

(defvar *running* t
  "Set to NIL by the signal handler to exit the main loop cleanly.")

(defun collect-modules-for (key instances)
  "Return the subset of INSTANCES whose position equals KEY, sorted by
priority descending."
  (let ((subset (remove-if-not (lambda (m) (eq (module-position m) key)) instances)))
    (sort subset #'> :key #'module-priority)))

(defun render-section (modules)
  "Render a list of MODULES into a single space-separated string."
  (let ((parts (loop for m in modules
                     for s = (format-module m)
                     when s collect s)))
    (format nil "~{~a~^ ~}" parts)))

(defun render-text-line (instances)
  "Compose the LEFT | CENTER | RIGHT line."
  (format nil "~a | ~a | ~a"
          (render-section (collect-modules-for :left   instances))
          (render-section (collect-modules-for :center instances))
          (render-section (collect-modules-for :right  instances))))

;;; Tiny JSON emitter - enough for the waybar custom-module schema.

(defun json-escape (s)
  (with-output-to-string (out)
    (loop for c across (or s "")
          do (case c
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return  (write-string "\\r" out))
               (#\Tab     (write-string "\\t" out))
               (otherwise
                (if (and (>= (char-code c) #x20) (<= (char-code c) #x7e))
                    (write-char c out)
                    (format out "\\u~4,'0x" (char-code c))))))))

(defun render-json-line (instances)
  (let ((text (render-text-line instances)))
    (format nil "{\"text\":\"~a\",\"class\":\"lispbar\",\"tooltip\":\"~a\"}"
            (json-escape text)
            (json-escape (format nil "modules: ~{~a~^, ~}"
                                  (mapcar (lambda (m) (string-downcase
                                                        (symbol-name (module-name m))))
                                          instances))))))

;;; Main output loop.

(defun build-instances (config)
  "Return a list of `module' instances based on the config plist."
  (let ((all (append (getf config :left)
                     (getf config :center)
                     (getf config :right)))
        (placements `((:left   . ,(getf config :left))
                      (:center . ,(getf config :center))
                      (:right  . ,(getf config :right)))))
    (loop for name in all
          for instance = (handler-case (make-module name)
                           (error (c)
                             (logmsg :warn "skipping module ~s: ~a" name c)
                             nil))
          when instance do
            (loop for (pos . names) in placements
                  when (member name names) do
                    (setf (module-position instance) pos))
            and collect instance)))

(defun run-stdout (config &key (json nil))
  "Block printing one bar line per tick to standard output."
  (let* ((instances (build-instances config))
         (tick      (or (getf config :tick) 1.0)))
    (logmsg :info "stdout driver started (~d modules, tick=~as, json=~a)"
            (length instances) tick json)
    (force-output)
    (loop while *running* do
          (let ((line (if json
                          (render-json-line instances)
                          (render-text-line instances))))
            (write-line line)
            (force-output))
          (sleep tick))))

(defvar *current-output* :stdout)

(defun run-output (config)
  "Dispatch on (getf config :output)."
  (let ((kind (or (getf config :output) :stdout)))
    (setf *current-output* kind)
    (case kind
      (:stdout  (run-stdout config))
      (:json    (run-stdout config :json t))
      (:wayland (run-wayland config))
      (t (error "Unknown output target: ~s" kind)))))
