;;;; build.lisp  --  Compile the lispbar ASDF system and save a native binary.
;;;;
;;;; Run with:   sbcl --script build.lisp
;;;; Result:     ./lispbar  (executable, self-contained)

(require :asdf)

;; Make sure ASDF can find the system in the current directory.
(pushnew (truename (make-pathname :directory (pathname-directory
                                              (or *load-truename*
                                                  *compile-file-truename*
                                                  *default-pathname-defaults*))))
         asdf:*central-registry*
         :test #'equal)

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
 ;; Stop the SBCL runtime from intercepting --help, --version, etc -
 ;; we want every argument to reach our `main' entry point.
 :save-runtime-options t)

#-sbcl
(error "Only SBCL is currently supported as a build host.")
