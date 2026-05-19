;;;; theme.lisp  --  Theme registry and built-in palettes.
;;;;
;;;; A theme is a property list mapping face keywords to (R G B A)
;;;; tuples in the 0.0-1.0 range.  The keywords below have stable
;;;; semantics every module relies on:
;;;;
;;;;   :bg      - bar background colour
;;;;   :normal  - default text colour (used when a module doesn't
;;;;              specify a face)
;;;;   :accent  - emphasis (clock, focused workspace, ...)
;;;;   :ok      - positive states (charging, connected, ...)
;;;;   :warn    - low / cautionary states
;;;;   :urgent  - critical states (battery <10 %, errors)
;;;;   :muted   - secondary text (separators, dim items)
;;;;
;;;; Users define their own themes by dropping a file under
;;;; $XDG_CONFIG_HOME/lispbar/themes/ that calls `define-theme'.

(in-package #:lispbar)

(defvar *themes* (make-hash-table :test #'eq)
  "NAME (keyword) -> palette PLIST.")

(defvar *theme-name* :default
  "Currently active theme name (for diagnostics).")

(defvar *theme-palette* nil
  "Currently active palette (a plist).")

(defun define-theme (name &rest palette)
  "Register theme NAME (a keyword) with the given face palette.
PALETTE is a property list of FACE -> (R G B A); see the file header
for the documented face keywords.  Re-registration replaces the
previous definition.  Returns NAME.

Example:

  (define-theme :catppuccin-latte
    :bg     '(0.937 0.937 0.953 1.0)
    :normal '(0.298 0.310 0.422 1.0)
    :accent '(0.380 0.502 0.886 1.0)
    :ok     '(0.157 0.659 0.376 1.0)
    :warn   '(0.870 0.580 0.255 1.0)
    :urgent '(0.831 0.247 0.314 1.0)
    :muted  '(0.604 0.624 0.741 1.0))"
  (check-type name keyword)
  (setf (gethash name *themes*) palette)
  name)

(defun theme-names ()
  "Return all currently registered theme names, sorted."
  (sort (loop for k being the hash-keys of *themes* collect k)
        #'string<
        :key #'symbol-name))

(defun apply-theme (name)
  "Make NAME the active theme.  Falls back to :default when NAME is
unknown or NIL; falls back to a hard-coded palette if even :default
is missing (e.g. during early bootstrap)."
  (let* ((wanted (or name :default))
         (palette (or (gethash wanted *themes*)
                      (gethash :default *themes*))))
    (cond (palette
           (setf *theme-name*    (if (gethash wanted *themes*) wanted :default)
                 *theme-palette* palette))
          (t
           (setf *theme-name*    :builtin-fallback
                 *theme-palette* '(:bg     (0.110 0.118 0.137 1.0)
                                   :normal (0.880 0.880 0.880 1.0)
                                   :accent (0.580 0.760 0.960 1.0)
                                   :ok     (0.500 0.870 0.480 1.0)
                                   :warn   (0.960 0.760 0.300 1.0)
                                   :urgent (0.960 0.420 0.420 1.0)
                                   :muted  (0.560 0.580 0.620 1.0)))))
    *theme-name*))

(defun theme-color (face)
  "Return the (R G B A) colour for FACE in the active theme."
  (or (getf *theme-palette* face)
      (getf *theme-palette* :normal)
      '(0.88 0.88 0.88 1.0)))

;;; ---- Built-in themes ----

(define-theme :default
  :bg     '(0.110 0.118 0.137 1.0)
  :normal '(0.880 0.880 0.880 1.0)
  :accent '(0.580 0.760 0.960 1.0)
  :ok     '(0.500 0.870 0.480 1.0)
  :warn   '(0.960 0.760 0.300 1.0)
  :urgent '(0.960 0.420 0.420 1.0)
  :muted  '(0.560 0.580 0.620 1.0))

(define-theme :minimal
  :bg     '(0.0 0.0 0.0 1.0)
  :normal '(1.0 1.0 1.0 1.0)
  :accent '(1.0 1.0 1.0 1.0)
  :ok     '(1.0 1.0 1.0 1.0)
  :warn   '(1.0 1.0 1.0 1.0)
  :urgent '(1.0 1.0 1.0 1.0)
  :muted  '(0.7 0.7 0.7 1.0))

(define-theme :nordish
  :bg     '(0.180 0.204 0.251 1.0)
  :normal '(0.847 0.871 0.914 1.0)
  :accent '(0.533 0.753 0.816 1.0)
  :ok     '(0.639 0.745 0.549 1.0)
  :warn   '(0.922 0.796 0.545 1.0)
  :urgent '(0.749 0.380 0.416 1.0)
  :muted  '(0.380 0.420 0.490 1.0))

(define-theme :gruvboxish
  :bg     '(0.157 0.157 0.157 1.0)
  :normal '(0.922 0.859 0.698 1.0)
  :accent '(0.980 0.741 0.184 1.0)
  :ok     '(0.722 0.733 0.149 1.0)
  :warn   '(0.996 0.502 0.098 1.0)
  :urgent '(0.984 0.286 0.204 1.0)
  :muted  '(0.486 0.435 0.392 1.0))

(define-theme :catppuccinish
  :bg     '(0.118 0.118 0.184 1.0)
  :normal '(0.804 0.839 0.957 1.0)
  :accent '(0.537 0.706 0.980 1.0)
  :ok     '(0.651 0.890 0.631 1.0)
  :warn   '(0.976 0.886 0.686 1.0)
  :urgent '(0.953 0.545 0.659 1.0)
  :muted  '(0.424 0.439 0.525 1.0))

(define-theme :doomish
  :bg     '(0.157 0.173 0.204 1.0)
  :normal '(0.733 0.761 0.812 1.0)
  :accent '(0.318 0.686 0.937 1.0)
  :ok     '(0.596 0.745 0.396 1.0)
  :warn   '(0.925 0.745 0.482 1.0)
  :urgent '(1.000 0.424 0.420 1.0)
  :muted  '(0.357 0.384 0.408 1.0))

;; Apply the default at load time so callers can ask for theme-color
;; immediately, even before a user config has selected a theme.
(apply-theme :default)
