;;;; build.lisp  --  Compile the lispbar ASDF system and save a native binary.
;;;;
;;;; Run with:   make build         (preferred)
;;;;       or:   sbcl --non-interactive --load build.lisp
;;;; Result:     ./lispbar  (executable, self-contained except for libwlbar.so)

;;; Pull in Quicklisp so CFFI is available.  Path matches the default
;;; install layout produced by `quicklisp-quickstart:install'.
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))

(require :asdf)

;; Make sure ASDF can find our system in the current directory.
(pushnew (truename (make-pathname :directory (pathname-directory
                                              (or *load-truename*
                                                  *compile-file-truename*
                                                  *default-pathname-defaults*))))
         asdf:*central-registry*
         :test #'equal)

;; Quickload CFFI so its FASLs are baked into the image.
(if (find-package :ql)
    (funcall (intern "QUICKLOAD" :ql) :cffi :silent t)
    (asdf:load-system :cffi))

(asdf:load-system :lispbar)

(format t "~&Lispbar built. Saving binary to ./lispbar~%")
(force-output)

#+sbcl
(sb-ext:save-lisp-and-die
 "lispbar"
 :toplevel (lambda ()
             (handler-case
                 (uiop:quit (or (lispbar:main) 0))
               (sb-sys:interactive-interrupt () (uiop:quit 130))
               (error (c)
                 (format *error-output* "lispbar: fatal: ~a~%" c)
                 (uiop:quit 1))))
 :executable t
 :purify t
 :compression t
 :save-runtime-options t)

#-sbcl
(error "Only SBCL is currently supported as a build host.")
