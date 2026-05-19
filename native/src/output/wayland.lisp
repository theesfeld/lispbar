;;;; wayland.lisp  --  Layer-shell output driver (replaces the stub).
;;;
;;; FFI hits two libraries:
;;;   libwlbar.so     - the C shim in ../cshim/ that owns the
;;;                     wl_display, wl_surface, layer surface, and
;;;                     wl_shm buffer.  Provides a flat ARGB32 buffer
;;;                     SBCL writes into with cairo.
;;;   libcairo.so.2   - drawing.

(in-package #:lispbar)

;;; ---------- CFFI / library loading ----------

(cffi:define-foreign-library libwlbar
  ;; Search in this order:
  ;;   1. /usr/local/lib/lispbar  (default install prefix)
  ;;   2. /usr/lib/lispbar        (distribution install prefix)
  ;;   3. ./cshim/libwlbar.so     (in-tree development)
  ;;   4. ./libwlbar.so           (next to the binary)
  ;;   5. plain `libwlbar.so'     (let ld.so resolve via LD_LIBRARY_PATH)
  (:unix (:or "/usr/local/lib/lispbar/libwlbar.so"
              "/usr/lib/lispbar/libwlbar.so"
              "./cshim/libwlbar.so"
              "./libwlbar.so"
              "libwlbar.so"))
  (t (:default "libwlbar")))

(cffi:define-foreign-library libcairo
  (:unix (:or "libcairo.so.2" "libcairo.so"))
  (t (:default "libcairo")))

(defvar *wlbar-loaded* nil)
(defvar *cairo-loaded* nil)

(defun ensure-wayland-libs ()
  "Load libwlbar.so and libcairo.  Returns T on success, NIL on failure."
  (handler-case
      (progn
        (unless *wlbar-loaded*
          (cffi:use-foreign-library libwlbar) (setf *wlbar-loaded* t))
        (unless *cairo-loaded*
          (cffi:use-foreign-library libcairo) (setf *cairo-loaded* t))
        t)
    (error (c)
      (logmsg :error "cannot load Wayland support: ~a" c)
      nil)))

;;; ---------- wlbar (C shim) FFI ----------

(cffi:defcfun ("wlbar_init"           wlbar-init)         :int (height :int))
(cffi:defcfun ("wlbar_shutdown"       wlbar-shutdown)     :void)
(cffi:defcfun ("wlbar_poll"           wlbar-poll)         :int (timeout :int))
(cffi:defcfun ("wlbar_closed"         wlbar-closed)       :int)

;; Per-output API (added when multi-monitor support landed).
(cffi:defcfun ("wlbar_output_count"   wlbar-output-count)   :int)
(cffi:defcfun ("wlbar_output_width"   wlbar-output-width)   :int (i :int))
(cffi:defcfun ("wlbar_output_height"  wlbar-output-height)  :int (i :int))
(cffi:defcfun ("wlbar_output_pixels"  wlbar-output-pixels)  :pointer (i :int))
(cffi:defcfun ("wlbar_output_stride"  wlbar-output-stride)  :int (i :int))
(cffi:defcfun ("wlbar_output_commit"  wlbar-output-commit)  :void (i :int))
(cffi:defcfun ("wlbar_output_name"    wlbar-output-name)    :string (i :int))

;;; ---------- cairo FFI ----------

(defconstant +cairo-format-argb32+ 0)

(cffi:defcfun ("cairo_image_surface_create_for_data" cairo-isc4d) :pointer
  (data :pointer) (format :int) (width :int) (height :int) (stride :int))
(cffi:defcfun ("cairo_create"             cairo-create)             :pointer (surf :pointer))
(cffi:defcfun ("cairo_destroy"            cairo-destroy)            :void    (cr :pointer))
(cffi:defcfun ("cairo_surface_destroy"    cairo-surface-destroy)    :void    (s :pointer))
(cffi:defcfun ("cairo_set_source_rgba"    cairo-set-source-rgba)    :void
  (cr :pointer) (r :double) (g :double) (b :double) (a :double))
(cffi:defcfun ("cairo_paint"              cairo-paint)              :void (cr :pointer))
(cffi:defcfun ("cairo_rectangle"          cairo-rectangle)          :void
  (cr :pointer) (x :double) (y :double) (w :double) (h :double))
(cffi:defcfun ("cairo_fill"               cairo-fill)               :void (cr :pointer))
(cffi:defcfun ("cairo_move_to"            cairo-move-to)            :void
  (cr :pointer) (x :double) (y :double))
(cffi:defcfun ("cairo_show_text"          cairo-show-text)          :void
  (cr :pointer) (utf8 :string))
(cffi:defcfun ("cairo_select_font_face"   cairo-select-font-face)   :void
  (cr :pointer) (family :string) (slant :int) (weight :int))
(cffi:defcfun ("cairo_set_font_size"      cairo-set-font-size)      :void
  (cr :pointer) (size :double))

;;; cairo_text_extents returns a struct by reference.
(cffi:defcstruct cairo-text-extents
  (x-bearing :double) (y-bearing :double)
  (width    :double) (height    :double)
  (x-advance :double) (y-advance :double))

(cffi:defcfun ("cairo_text_extents" cairo-text-extents) :void
  (cr :pointer) (utf8 :string) (extents :pointer))

(defun cairo-text-width (cr text)
  (cffi:with-foreign-object (ext '(:struct cairo-text-extents))
    (cairo-text-extents cr text ext)
    (cffi:foreign-slot-value ext '(:struct cairo-text-extents) 'x-advance)))

;;; ---------- Theme palette ----------
;;;
;;; A theme is a plist mapping face keywords to (R G B A) doubles.
;;; Module update functions return either a string (rendered with
;;; :normal) or (:text STR :face FACE).  The faces are:
;;;
;;;   :bg :normal :accent :ok :warn :urgent :muted

(defvar *wayland-theme* nil
  "Plist of face -> colour for the active theme.")
(defvar *wayland-font-family* "monospace")
(defvar *wayland-font-size*   13.0d0)

(defun theme-color (face)
  "Return the (R G B A) colour for FACE in the active theme."
  (or (getf *wayland-theme* face)
      (getf *wayland-theme* :normal)
      '(0.88 0.88 0.88 1.0)))

(defun apply-theme (theme)
  "Install a face palette for THEME (a keyword)."
  (setf *wayland-theme*
        (case theme
          (:nordish
           '(:bg     (0.180 0.204 0.251 1.0)
             :normal (0.847 0.871 0.914 1.0)
             :accent (0.533 0.753 0.816 1.0)
             :ok     (0.639 0.745 0.549 1.0)
             :warn   (0.922 0.796 0.545 1.0)
             :urgent (0.749 0.380 0.416 1.0)
             :muted  (0.380 0.420 0.490 1.0)))
          (:gruvboxish
           '(:bg     (0.157 0.157 0.157 1.0)
             :normal (0.922 0.859 0.698 1.0)
             :accent (0.980 0.741 0.184 1.0)
             :ok     (0.722 0.733 0.149 1.0)
             :warn   (0.996 0.502 0.098 1.0)
             :urgent (0.984 0.286 0.204 1.0)
             :muted  (0.486 0.435 0.392 1.0)))
          (:catppuccinish
           '(:bg     (0.118 0.118 0.184 1.0)
             :normal (0.804 0.839 0.957 1.0)
             :accent (0.537 0.706 0.980 1.0)
             :ok     (0.651 0.890 0.631 1.0)
             :warn   (0.976 0.886 0.686 1.0)
             :urgent (0.953 0.545 0.659 1.0)
             :muted  (0.424 0.439 0.525 1.0)))
          (:doomish
           '(:bg     (0.157 0.173 0.204 1.0)
             :normal (0.733 0.761 0.812 1.0)
             :accent (0.318 0.686 0.937 1.0)
             :ok     (0.596 0.745 0.396 1.0)
             :warn   (0.925 0.745 0.482 1.0)
             :urgent (1.000 0.424 0.420 1.0)
             :muted  (0.357 0.384 0.408 1.0)))
          (:minimal
           '(:bg     (0.0 0.0 0.0 1.0)
             :normal (1.0 1.0 1.0 1.0)
             :accent (1.0 1.0 1.0 1.0)
             :ok     (1.0 1.0 1.0 1.0)
             :warn   (1.0 1.0 1.0 1.0)
             :urgent (1.0 1.0 1.0 1.0)
             :muted  (0.7 0.7 0.7 1.0)))
          (t
           '(:bg     (0.110 0.118 0.137 1.0)
             :normal (0.880 0.880 0.880 1.0)
             :accent (0.580 0.760 0.960 1.0)
             :ok     (0.500 0.870 0.480 1.0)
             :warn   (0.960 0.760 0.300 1.0)
             :urgent (0.960 0.420 0.420 1.0)
             :muted  (0.560 0.580 0.620 1.0))))))

;;; ---------- Frame rendering ----------

(defun rgba->doubles (c) (mapcar (lambda (v) (coerce v 'double-float)) c))

(defun fragment-list-width (cr fragments)
  "Total pixel width of FRAGMENTS, painted back-to-back."
  (loop for (text _face) in fragments
        sum (cairo-text-width cr text)))

(defun draw-fragments (cr fragments x baseline)
  "Paint FRAGMENTS back-to-back starting at X / BASELINE.
Each fragment carries its own face; spacing is up to the caller (the
module-fragments helper already inserts space fragments)."
  (let ((pen (coerce x 'double-float)))
    (dolist (f fragments)
      (let ((text (first f))
            (face (second f)))
        (apply #'cairo-set-source-rgba cr
               (rgba->doubles (theme-color face)))
        (cairo-move-to cr pen baseline)
        (cairo-show-text cr text)
        (incf pen (cairo-text-width cr text))))
    pen))

(defun render-output (i instances)
  "Paint output index I.  Returns NIL when the output is unmapped."
  (let* ((w      (wlbar-output-width i))
         (h      (wlbar-output-height i))
         (data   (wlbar-output-pixels i))
         (stride (wlbar-output-stride i)))
    (when (or (zerop w) (zerop h) (cffi:null-pointer-p data))
      (return-from render-output nil))
    (let* ((surf (cairo-isc4d data +cairo-format-argb32+ w h stride))
           (cr   (cairo-create surf)))
      (unwind-protect
           (progn
             (apply #'cairo-set-source-rgba cr
                    (rgba->doubles (theme-color :bg)))
             (cairo-paint cr)
             (cairo-select-font-face cr *wayland-font-family* 0 0)
             (cairo-set-font-size cr *wayland-font-size*)
             (let* ((left   (module-fragments
                             (collect-modules-for :left   instances)))
                    (center (module-fragments
                             (collect-modules-for :center instances)))
                    (right  (module-fragments
                             (collect-modules-for :right  instances)))
                    (pad 12.0d0)
                    (baseline (+ (/ h 2.0) (/ *wayland-font-size* 3.0d0))))
               (when left
                 (draw-fragments cr left pad baseline))
               (when center
                 (let ((cw (fragment-list-width cr center)))
                   (draw-fragments cr center (/ (- w cw) 2.0d0) baseline)))
               (when right
                 (let ((rw (fragment-list-width cr right)))
                   (draw-fragments cr right (- w rw pad) baseline)))))
        (cairo-destroy cr)
        (cairo-surface-destroy surf)))
    (wlbar-output-commit i)
    t))

(defun render-frame (instances)
  "Paint every output's surface."
  (loop for i from 0 below (wlbar-output-count)
        do (render-output i instances)))

;;; ---------- Main loop ----------

(defun run-wayland (config)
  "Layer-shell main loop.  Returns when the surface is closed."
  (unless (ensure-wayland-libs)
    (logmsg :error "Falling back to :stdout driver.")
    (run-stdout config)
    (return-from run-wayland))

  (apply-theme (getf config :theme))

  (let* ((height (or (getf config :height) 28))
         (rc     (wlbar-init height)))
    (when (< rc 0)
      (logmsg :error "wlbar_init failed; falling back to :stdout")
      (run-stdout config)
      (return-from run-wayland)))

  (unwind-protect
       (let ((instances (build-instances config))
             (tick      (or (getf config :tick) 1.0)))
         (logmsg :info "wayland driver up: ~d output(s), ~d modules, tick=~as"
                 (wlbar-output-count) (length instances) tick)
         (loop for i from 0 below (wlbar-output-count) do
               (logmsg :info "  output ~d: ~ax~a ~a"
                       i (wlbar-output-width i) (wlbar-output-height i)
                       (or (wlbar-output-name i) "?")))
         (render-frame instances)
         (loop while (and *running* (zerop (wlbar-closed))) do
               (wlbar-poll (round (* tick 1000)))
               (render-frame instances)))
    (wlbar-shutdown)))
