;;;; tests/test-registry-browse.lisp
;;;;
;;;; Unit tests for the registry-browse selection parser and the
;;;; helpers around it.  The parser is the fragile part: tokenising
;;;; fzf's stdout into a keyword name.  Coupled to the output format
;;;; of `registry-browse-lines'; if you change that, change these.
;;;;
;;;; Run with:   sh tests/run-tests.sh
;;;;
;;;; The harness is intentionally minimal — no FiveAM / Parachute /
;;;; etc., just a counter and `assert*' macro.  Lispbar deliberately
;;;; ships zero external Lisp deps beyond CFFI.

(require :asdf)
;; Pick up Quicklisp if installed at the conventional path so `:cffi'
;; (lispbar's only Lisp dep) resolves automatically.  Same fallback as
;; build.lisp.
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
;; Make sure ASDF finds the lispbar.asd next to this test, not whatever
;; the user happens to have on their source registry.
(pushnew (uiop:pathname-directory-pathname
          (or *load-truename* *compile-file-truename*
              (uiop:getcwd)))
         asdf:*central-registry*
         :test #'equal)
;; Resolve to the project root regardless of where SBCL was launched.
(let ((here (truename
             (uiop:pathname-directory-pathname
              (or *load-truename* (uiop:getcwd))))))
  (pushnew (uiop:pathname-parent-directory-pathname here)
           asdf:*central-registry*
           :test #'equal))
(asdf:load-system :lispbar)

(in-package #:lispbar)

(defvar *test-pass* 0)
(defvar *test-fail* 0)

(defmacro check (label form expected)
  "Eval FORM, compare with EXPECTED under EQUAL, tally pass/fail."
  `(let ((actual (handler-case ,form (error (c) c))))
     (cond
       ((equal actual ,expected)
        (incf *test-pass*)
        (format t "  ok   ~a~%" ,label))
       (t
        (incf *test-fail*)
        (format t "  FAIL ~a~%       got: ~s~%       want: ~s~%"
                ,label actual ,expected)))))

(format t "~%cli-name->keyword~%")
(check "bare-name"          (cli-name->keyword "weather")        :weather)
(check "colon-prefix"       (cli-name->keyword ":weather")       :weather)
(check "uppercase-input"    (cli-name->keyword "WEATHER")        :weather)
(check "mixed-case"         (cli-name->keyword ":Tokyo-Night")   :tokyo-night)
(check "single-char"        (cli-name->keyword "a")              :a)

(format t "~%registry-display-name~%")
(check "weather"            (registry-display-name :weather)      ":weather")
(check "tokyo-night"        (registry-display-name :tokyo-night)  ":tokyo-night")

(format t "~%parse-browse-selection — happy path~%")
(check "installed-marker"
       (parse-browse-selection "* :weather       module   Current weather")
       :weather)
(check "uninstalled-marker"
       (parse-browse-selection "· :tokyo-night   theme    Tokyo Night palette")
       :tokyo-night)
(check "trailing-newline"
       (parse-browse-selection
        "· :pomodoro      module   Click-driven 25/5 pomodoro timer
")
       :pomodoro)
(check "tab-separated"
       (parse-browse-selection
        (format nil "·~c:diskspace~cmodule~cFree space" #\Tab #\Tab #\Tab))
       :diskspace)

(format t "~%parse-browse-selection — multi-line input (first line wins)~%")
(check "multi-line"
       (parse-browse-selection
        "· :weather       module   first
· :cputemp       module   second")
       :weather)

(format t "~%parse-browse-selection — empty / malformed input~%")
(check "nil"                (parse-browse-selection nil)             nil)
(check "empty-string"       (parse-browse-selection "")              nil)
(check "blanks-only"        (parse-browse-selection "   ")           nil)
(check "newlines-only"      (parse-browse-selection (format nil "~%~%~%"))
                                                                     nil)
(check "single-token"       (parse-browse-selection "·")             nil)
(check "non-string"         (parse-browse-selection 42)              nil)

(format t "~%parse-browse-selection — keyword normalisation~%")
(check "uppercase-name"
       (parse-browse-selection "* WEATHER       module")
       :weather)
(check "no-colon-prefix"
       (parse-browse-selection "* weather       module")
       :weather)

(format t "~%~%Result: ~d passed, ~d failed~%" *test-pass* *test-fail*)
(unless (zerop *test-fail*)
  (sb-ext:exit :code 1))
