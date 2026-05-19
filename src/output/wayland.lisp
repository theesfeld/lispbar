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

(cffi:define-foreign-library libpangocairo
  (:unix (:or "libpangocairo-1.0.so.0" "libpangocairo-1.0.so"))
  (t (:default "libpangocairo-1.0")))

(cffi:define-foreign-library libpango
  (:unix (:or "libpango-1.0.so.0" "libpango-1.0.so"))
  (t (:default "libpango-1.0")))

(defvar *wlbar-loaded*    nil)
(defvar *cairo-loaded*    nil)
(defvar *pango-loaded*    nil)
(defvar *have-pango*      nil
  "Set when libpangocairo loaded; gates the high-quality text path.")

(defun ensure-wayland-libs ()
  "Load libwlbar.so, libcairo, and (if available) libpango/pangocairo.
Returns T on success.  Pango is optional; without it we fall back to
cairo_show_text which has no font fallback or shaping."
  (handler-case
      (progn
        (unless *wlbar-loaded*
          (cffi:use-foreign-library libwlbar) (setf *wlbar-loaded* t))
        (unless *cairo-loaded*
          (cffi:use-foreign-library libcairo) (setf *cairo-loaded* t))
        (unless *pango-loaded*
          (handler-case
              (progn (cffi:use-foreign-library libpango)
                     (cffi:use-foreign-library libpangocairo)
                     (setf *pango-loaded* t *have-pango* t))
            (error (c)
              (logmsg :warn "pango unavailable, using cairo toy text: ~a" c)
              (setf *pango-loaded* t *have-pango* nil))))
        t)
    (error (c)
      (logmsg :error "cannot load Wayland support: ~a" c)
      nil)))

;;; ---------- wlbar (C shim) FFI ----------

(cffi:defcfun ("wlbar_init"           wlbar-init)         :int
  (height :int) (position :int)
  (margin-top :int) (margin-right :int)
  (margin-bottom :int) (margin-left :int))
(cffi:defcfun ("wlbar_shutdown"       wlbar-shutdown)     :void)

(defconstant +wlbar-position-top+    0)
(defconstant +wlbar-position-bottom+ 1)

(defun position->c (sym)
  "Translate a config :top / :bottom keyword to the C-side enum."
  (case sym
    ((:top nil)  +wlbar-position-top+)
    ((:bottom)   +wlbar-position-bottom+)
    (t           +wlbar-position-top+)))
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
(cffi:defcfun ("cairo_set_operator"       cairo-set-operator)       :void
  (cr :pointer) (op :int))
(cffi:defcfun ("cairo_new_path"           cairo-new-path)           :void
  (cr :pointer))
(cffi:defcfun ("cairo_close_path"         cairo-close-path)         :void
  (cr :pointer))
(cffi:defcfun ("cairo_arc"                cairo-arc)                :void
  (cr :pointer) (xc :double) (yc :double) (r :double)
  (a1 :double) (a2 :double))

(defconstant +cairo-operator-clear+  0)
(defconstant +cairo-operator-source+ 1)
(defconstant +cairo-operator-over+   2)

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

;;; ---------- Pango FFI (optional path) ----------

(defvar *pango-scale* 1024
  "PANGO_SCALE - pango sizes are in 1024ths of a pixel.")

(cffi:defcfun ("pango_cairo_create_layout"  pango-cairo-create-layout)  :pointer
  (cr :pointer))
(cffi:defcfun ("pango_cairo_show_layout"    pango-cairo-show-layout)    :void
  (cr :pointer) (layout :pointer))
(cffi:defcfun ("pango_font_description_from_string" pango-fd-from-string) :pointer
  (str :string))
(cffi:defcfun ("pango_font_description_free" pango-fd-free) :void
  (fd :pointer))
(cffi:defcfun ("pango_layout_set_font_description" pango-layout-set-fd) :void
  (layout :pointer) (fd :pointer))
(cffi:defcfun ("pango_layout_set_text"      pango-layout-set-text)      :void
  (layout :pointer) (text :string) (len :int))
(cffi:defcfun ("pango_layout_get_pixel_size" pango-layout-get-pixel-size) :void
  (layout :pointer) (w :pointer) (h :pointer))
(cffi:defcfun ("g_object_unref"             g-object-unref)             :void
  (obj :pointer))

(defun pango-text-width (layout text)
  "Set TEXT on LAYOUT and return its width in pixels."
  (pango-layout-set-text layout text -1)
  (cffi:with-foreign-objects ((w :int) (h :int))
    (pango-layout-get-pixel-size layout w h)
    (cffi:mem-ref w :int)))

;;; ---------- Rendering tunables ----------
;;;
;;; Themes (face palettes) now live in theme.lisp.  This file just
;;; consumes `theme-color' and the font tunables.

(defvar *wayland-font-family* "monospace")
(defvar *wayland-font-size*   13.0d0)
(defvar *wayland-font-spec*   "Monospace 11"
  "Pango font description string used by the high-quality text path.")

(defvar *wayland-padding*       12.0d0
  "Horizontal padding (pixels) inside the bar before the first
fragment on the left and after the last fragment on the right.")
(defvar *wayland-module-gap*    12.0d0
  "Horizontal space (pixels) inserted between adjacent modules.")
(defvar *wayland-corner-radius* 0.0d0
  "Corner radius of the bar background (pixels).  0 = sharp corners.")

(defun normalise-margin (value)
  "Return (top right bottom left) for a config-supplied margin VALUE.

Accepts any of these shapes (mirroring CSS):

  nil             -> 0 0 0 0
  4               -> 4 4 4 4
  (4 8)           -> 4 8 4 8                  ; vert horiz
  (4 8 12)        -> 4 8 12 8                 ; top horiz bottom
  (4 8 12 16)     -> 4 8 12 16                ; top right bottom left

Also tolerates the QUOTE form that a user might write by mistake
inside config.lisp - `'(4 8 0 8)' reads as (QUOTE (4 8 0 8))."
  (let ((v value))
    (when (and (consp v) (eq (car v) 'quote))
      (setf v (cadr v)))
    (cond
      ((null v)        (values 0 0 0 0))
      ((numberp v)     (values v v v v))
      ((and (listp v) (= 2 (length v)))
       (destructuring-bind (a b) v (values a b a b)))
      ((and (listp v) (= 3 (length v)))
       (destructuring-bind (t* h b) v (values t* h b h)))
      ((and (listp v) (>= (length v) 4))
       (destructuring-bind (t* r b l &rest _) v
         (declare (ignore _))
         (values t* r b l)))
      (t (values 0 0 0 0)))))

;;; ---------- Frame rendering ----------

(defun rgba->doubles (c) (mapcar (lambda (v) (coerce v 'double-float)) c))

(defun rounded-rect-path (cr x y w h r)
  "Append a rounded-rectangle path (X Y W H, corner radius R) to CR."
  (let* ((x  (coerce x 'double-float))
         (y  (coerce y 'double-float))
         (w  (coerce w 'double-float))
         (h  (coerce h 'double-float))
         (r  (min (coerce r 'double-float) (/ w 2) (/ h 2)))
         (p  (coerce pi 'double-float)))
    (cairo-new-path cr)
    ;; Top-left corner
    (cairo-arc cr (+ x r)       (+ y r)       r p              (* 1.5d0 p))
    ;; Top-right corner
    (cairo-arc cr (+ x w (- r)) (+ y r)       r (* 1.5d0 p)    (* 2.0d0 p))
    ;; Bottom-right corner
    (cairo-arc cr (+ x w (- r)) (+ y h (- r)) r 0.0d0          (* 0.5d0 p))
    ;; Bottom-left corner
    (cairo-arc cr (+ x r)       (+ y h (- r)) r (* 0.5d0 p)    p)
    (cairo-close-path cr)))

(defun paint-background (cr w h)
  "Clear surface to transparent then paint the bar background, honouring
*WAYLAND-CORNER-RADIUS*."
  (cairo-set-operator cr +cairo-operator-source+)
  (cairo-set-source-rgba cr 0.0d0 0.0d0 0.0d0 0.0d0)
  (cairo-paint cr)
  (cairo-set-operator cr +cairo-operator-over+)
  (apply #'cairo-set-source-rgba cr (rgba->doubles (theme-color :bg)))
  (cond
    ((plusp *wayland-corner-radius*)
     (rounded-rect-path cr 0 0 w h *wayland-corner-radius*)
     (cairo-fill cr))
    (t
     (cairo-paint cr))))

;; Cairo (toy) path -----------------------------------------------

(defun cairo-fragment-list-width (cr fragments)
  (loop for f in fragments
        if (fragment-gap-p f) sum *wayland-module-gap*
        else                  sum (cairo-text-width cr (first f))))

(defun cairo-draw-fragments (cr fragments x baseline)
  (let ((pen (coerce x 'double-float)))
    (dolist (f fragments)
      (cond
        ((fragment-gap-p f)
         (incf pen *wayland-module-gap*))
        (t
         (let ((text (first f)) (face (second f)))
           (apply #'cairo-set-source-rgba cr
                  (rgba->doubles (theme-color face)))
           (cairo-move-to cr pen baseline)
           (cairo-show-text cr text)
           (incf pen (cairo-text-width cr text))))))
    pen))

;; Pango path -----------------------------------------------------

(defun pango-fragment-list-width (cr fragments)
  (let ((layout (pango-cairo-create-layout cr))
        (fd     (pango-fd-from-string *wayland-font-spec*)))
    (unwind-protect
         (progn (pango-layout-set-fd layout fd)
                (loop for f in fragments
                      if (fragment-gap-p f) sum *wayland-module-gap*
                      else                  sum (pango-text-width layout (first f))))
      (pango-fd-free fd)
      (g-object-unref layout))))

(defun pango-draw-fragments (cr fragments x y-top)
  (let ((layout (pango-cairo-create-layout cr))
        (fd     (pango-fd-from-string *wayland-font-spec*))
        (pen    (coerce x 'double-float)))
    (unwind-protect
         (progn (pango-layout-set-fd layout fd)
                (dolist (f fragments)
                  (cond
                    ((fragment-gap-p f)
                     (incf pen *wayland-module-gap*))
                    (t
                     (let ((text (first f)) (face (second f)))
                       (apply #'cairo-set-source-rgba cr
                              (rgba->doubles (theme-color face)))
                       (cairo-move-to cr pen y-top)
                       (pango-layout-set-text layout text -1)
                       (pango-cairo-show-layout cr layout)
                       (incf pen (pango-text-width layout text)))))))
      (pango-fd-free fd)
      (g-object-unref layout))
    pen))

;; Dispatch -------------------------------------------------------

(defun fragment-list-width (cr fragments)
  (if *have-pango*
      (pango-fragment-list-width cr fragments)
      (cairo-fragment-list-width cr fragments)))

(defun draw-fragments (cr fragments x baseline-or-top)
  "Paint FRAGMENTS at horizontal position X.
BASELINE-OR-TOP is the cairo baseline when *have-pango* is NIL, and
the layout top-edge otherwise (pango positions glyphs from the top)."
  (if *have-pango*
      (pango-draw-fragments cr fragments x baseline-or-top)
      (cairo-draw-fragments cr fragments x baseline-or-top)))

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
             (paint-background cr w h)
             (unless *have-pango*
               (cairo-select-font-face cr *wayland-font-family* 0 0)
               (cairo-set-font-size cr *wayland-font-size*))
             (let* ((left   (module-fragments
                             (collect-modules-for :left   instances)))
                    (center (module-fragments
                             (collect-modules-for :center instances)))
                    (right  (module-fragments
                             (collect-modules-for :right  instances)))
                    (pad *wayland-padding*)
                    ;; Pango: top-left coords; cairo: baseline.
                    (ypos (if *have-pango*
                              (/ (- h (* *wayland-font-size* 1.4)) 2.0)
                              (+ (/ h 2.0) (/ *wayland-font-size* 3.0d0)))))
               (when left
                 (draw-fragments cr left pad ypos))
               (when center
                 (let ((cw (fragment-list-width cr center)))
                   (draw-fragments cr center (/ (- w cw) 2.0d0) ypos)))
               (when right
                 (let ((rw (fragment-list-width cr right)))
                   (draw-fragments cr right (- w rw pad) ypos)))))
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
  (when (getf config :font)
    (setf *wayland-font-spec* (getf config :font)))
  (when (getf config :padding)
    (setf *wayland-padding* (coerce (getf config :padding) 'double-float)))
  (when (getf config :gap)
    (setf *wayland-module-gap* (coerce (getf config :gap) 'double-float)))
  (when (getf config :corner-radius)
    (setf *wayland-corner-radius*
          (coerce (getf config :corner-radius) 'double-float)))

  (multiple-value-bind (mt mr mb ml) (normalise-margin (getf config :margin))
    (let* ((height   (or (getf config :height) 28))
           (position (position->c (getf config :position)))
           (rc       (wlbar-init height position mt mr mb ml)))
      (when (< rc 0)
        (logmsg :error "wlbar_init failed; falling back to :stdout")
        (run-stdout config)
        (return-from run-wayland))))

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
