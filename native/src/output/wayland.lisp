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
  (:unix (:or "libwlbar.so" "./libwlbar.so" "../cshim/libwlbar.so"))
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

(cffi:defcfun ("wlbar_init"      wlbar-init)     :int (height :int))
(cffi:defcfun ("wlbar_shutdown"  wlbar-shutdown) :void)
(cffi:defcfun ("wlbar_width"     wlbar-width)    :int)
(cffi:defcfun ("wlbar_height"    wlbar-height)   :int)
(cffi:defcfun ("wlbar_pixels"    wlbar-pixels)   :pointer)
(cffi:defcfun ("wlbar_stride"    wlbar-stride)   :int)
(cffi:defcfun ("wlbar_commit"    wlbar-commit)   :void)
(cffi:defcfun ("wlbar_poll"      wlbar-poll)     :int (timeout :int))
(cffi:defcfun ("wlbar_closed"    wlbar-closed)   :int)

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

(defvar *wayland-bg* '(0.18 0.20 0.25 1.0)
  "Bar background colour as a (R G B A) list of doubles 0.0-1.0.")
(defvar *wayland-fg* '(0.85 0.87 0.91 1.0)
  "Foreground colour for module text.")
(defvar *wayland-font-family* "monospace")
(defvar *wayland-font-size*   13.0d0)

(defun apply-theme (theme)
  "Update the foreground/background palette from THEME (a keyword)."
  (case theme
    (:nordish      (setf *wayland-bg* '(0.180 0.204 0.251 1.0)
                         *wayland-fg* '(0.847 0.871 0.914 1.0)))
    (:gruvboxish   (setf *wayland-bg* '(0.157 0.157 0.157 1.0)
                         *wayland-fg* '(0.922 0.859 0.698 1.0)))
    (:catppuccinish(setf *wayland-bg* '(0.118 0.118 0.184 1.0)
                         *wayland-fg* '(0.804 0.839 0.957 1.0)))
    (:doomish      (setf *wayland-bg* '(0.157 0.173 0.204 1.0)
                         *wayland-fg* '(0.733 0.761 0.812 1.0)))
    (:minimal      (setf *wayland-bg* '(0.0 0.0 0.0 1.0)
                         *wayland-fg* '(1.0 1.0 1.0 1.0)))
    (t             (setf *wayland-bg* '(0.110 0.118 0.137 1.0)
                         *wayland-fg* '(0.880 0.880 0.880 1.0)))))

;;; ---------- Frame rendering ----------

(defun rgba->doubles (c) (mapcar (lambda (v) (coerce v 'double-float)) c))

(defun render-frame (instances)
  "Paint a single frame: clear, then draw left | center | right text."
  (let* ((w (wlbar-width))
         (h (wlbar-height))
         (data (wlbar-pixels))
         (stride (wlbar-stride)))
    (when (or (zerop w) (zerop h) (cffi:null-pointer-p data))
      (return-from render-frame))
    (let* ((surf (cairo-isc4d data +cairo-format-argb32+ w h stride))
           (cr   (cairo-create surf)))
      (unwind-protect
           (progn
             ;; Background
             (apply #'cairo-set-source-rgba cr (rgba->doubles *wayland-bg*))
             (cairo-paint cr)
             ;; Font
             (cairo-select-font-face cr *wayland-font-family* 0 0)
             (cairo-set-font-size cr *wayland-font-size*)
             ;; Foreground
             (apply #'cairo-set-source-rgba cr (rgba->doubles *wayland-fg*))

             (let* ((left   (render-section (collect-modules-for :left   instances)))
                    (center (render-section (collect-modules-for :center instances)))
                    (right  (render-section (collect-modules-for :right  instances)))
                    (pad 12.0d0)
                    (baseline (+ (/ h 2.0) (/ *wayland-font-size* 3.0d0))))
               (unless (zerop (length left))
                 (cairo-move-to cr pad baseline)
                 (cairo-show-text cr left))
               (unless (zerop (length center))
                 (let ((cw (cairo-text-width cr center)))
                   (cairo-move-to cr (/ (- w cw) 2.0d0) baseline)
                   (cairo-show-text cr center)))
               (unless (zerop (length right))
                 (let ((rw (cairo-text-width cr right)))
                   (cairo-move-to cr (- w rw pad) baseline)
                   (cairo-show-text cr right)))))
        (cairo-destroy cr)
        (cairo-surface-destroy surf)))
    (wlbar-commit)))

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
    (when (/= rc 0)
      (logmsg :error "wlbar_init failed; falling back to :stdout")
      (run-stdout config)
      (return-from run-wayland)))

  (unwind-protect
       (let ((instances (build-instances config))
             (tick      (or (getf config :tick) 1.0)))
         (logmsg :info "wayland driver up: ~dx~d, ~d modules, tick=~as"
                 (wlbar-width) (wlbar-height) (length instances) tick)
         (render-frame instances)
         (loop while (and *running* (zerop (wlbar-closed))) do
               (wlbar-poll (round (* tick 1000)))
               (render-frame instances)))
    (wlbar-shutdown)))
