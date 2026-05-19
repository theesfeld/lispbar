;;;; lispbar.asd  -- ASDF system definition for the native Lispbar binary
;;;;
;;;; Build with:  sbcl --non-interactive --load build.lisp
;;;; Result:      ./lispbar  (standalone native executable)

(asdf:defsystem #:lispbar
  :description "Native status bar implemented in Common Lisp."
  :author      "Lispbar contributors"
  :license     "GPL-3.0-or-later"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:cffi)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "log")
                 (:file "xdg")
                 (:file "theme")
                 (:file "config")
                 (:file "module")
                 (:module "modules"
                  :components ((:file "clock")
                               (:file "cpu")
                               (:file "memory")
                               (:file "battery")
                               (:file "audio")
                               (:file "bluetooth")
                               (:file "brightness")
                               (:file "workspaces")
                               (:file "media")
                               (:file "network")
                               (:file "launcher")
                               (:file "tray")))
                 (:module "output"
                  :components ((:file "stdout")
                               (:file "wayland")))
                 (:file "main")))))
