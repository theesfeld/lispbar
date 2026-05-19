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

;; Pointer event drained from the C-side queue.
(cffi:defcstruct wlbar-pointer-event
  (output-idx :int)
  (x          :double)
  (y          :double)
  (button     :int)
  (pressed    :int))
(cffi:defcfun ("wlbar_poll_pointer_event" wlbar-poll-pointer-event) :int
  (out :pointer))
(cffi:defcfun ("wlbar_pointer_hover"      wlbar-pointer-hover)      :int
  (output :pointer) (x :pointer) (y :pointer))

;; Tooltip secondary-surface API
(cffi:defcfun ("wlbar_tooltip_show"     wlbar-tooltip-show)     :int
  (output :int) (anchor-x :int) (w :int) (h :int))
(cffi:defcfun ("wlbar_tooltip_pixels"   wlbar-tooltip-pixels)   :pointer
  (output :int))
(cffi:defcfun ("wlbar_tooltip_stride"   wlbar-tooltip-stride)   :int
  (output :int))
(cffi:defcfun ("wlbar_tooltip_width"    wlbar-tooltip-width)    :int
  (output :int))
(cffi:defcfun ("wlbar_tooltip_height"   wlbar-tooltip-height)   :int
  (output :int))
(cffi:defcfun ("wlbar_tooltip_commit"   wlbar-tooltip-commit)   :void
  (output :int))
(cffi:defcfun ("wlbar_tooltip_hide"     wlbar-tooltip-hide)     :void
  (output :int))

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
(cffi:defcfun ("cairo_save"               cairo-save)               :void
  (cr :pointer))
(cffi:defcfun ("cairo_restore"            cairo-restore)            :void
  (cr :pointer))
(cffi:defcfun ("cairo_translate"          cairo-translate)          :void
  (cr :pointer) (tx :double) (ty :double))
(cffi:defcfun ("cairo_scale"              cairo-scale)              :void
  (cr :pointer) (sx :double) (sy :double))
(cffi:defcfun ("cairo_set_source_surface" cairo-set-source-surface) :void
  (cr :pointer) (surf :pointer) (x :double) (y :double))
(cffi:defcfun ("cairo_image_surface_create_from_png"
               cairo-image-surface-create-from-png) :pointer
  (filename :string))
(cffi:defcfun ("cairo_surface_status"     cairo-surface-status)     :int
  (surf :pointer))
(cffi:defcfun ("cairo_image_surface_get_width"  cairo-image-surface-get-width)  :int
  (surf :pointer))
(cffi:defcfun ("cairo_image_surface_get_height" cairo-image-surface-get-height) :int
  (surf :pointer))

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

(defun fragment-text-of (f)
  "Return the renderable text for any fragment shape."
  (cond ((fragment-gap-p f) " ")
        ((fragment-clickable-p f) (clickable-text f))
        (t (first f))))

(defun fragment-face-of (f)
  (cond ((fragment-gap-p f) :normal)
        ((fragment-clickable-p f) (clickable-face f))
        (t (second f))))

(defun cairo-fragment-list-width (cr fragments)
  (loop for f in fragments
        if (fragment-gap-p f) sum *wayland-module-gap*
        else                  sum (cairo-text-width cr (fragment-text-of f))))

(defun cairo-draw-fragments (cr fragments x baseline &optional output-idx)
  (let ((pen (coerce x 'double-float)))
    (dolist (f fragments)
      (cond
        ((fragment-gap-p f)
         (incf pen *wayland-module-gap*))
        (t
         (let* ((text (fragment-text-of f))
                (face (fragment-face-of f))
                (start pen)
                (text-w (cairo-text-width cr text)))
           (apply #'cairo-set-source-rgba cr
                  (rgba->doubles (theme-color face)))
           (cairo-move-to cr pen baseline)
           (cairo-show-text cr text)
           (incf pen text-w)
           (when (and output-idx (fragment-clickable-p f))
             (record-subfragment output-idx start pen
                                 (clickable-handler f)
                                 (clickable-data f)))))))
    pen))

;; Pango path -----------------------------------------------------

(defun pango-fragment-list-width (cr fragments)
  (let ((layout (pango-cairo-create-layout cr))
        (fd     (pango-fd-from-string *wayland-font-spec*)))
    (unwind-protect
         (progn (pango-layout-set-fd layout fd)
                (loop for f in fragments
                      if (fragment-gap-p f) sum *wayland-module-gap*
                      else                  sum (pango-text-width layout (fragment-text-of f))))
      (pango-fd-free fd)
      (g-object-unref layout))))

(defun pango-draw-fragments (cr fragments x y-top &optional output-idx)
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
                     (let* ((text (fragment-text-of f))
                            (face (fragment-face-of f))
                            (start pen))
                       (apply #'cairo-set-source-rgba cr
                              (rgba->doubles (theme-color face)))
                       (cairo-move-to cr pen y-top)
                       (pango-layout-set-text layout text -1)
                       (pango-cairo-show-layout cr layout)
                       (incf pen (pango-text-width layout text))
                       (when (and output-idx (fragment-clickable-p f))
                         (record-subfragment output-idx start pen
                                              (clickable-handler f)
                                              (clickable-data f))))))))
      (pango-fd-free fd)
      (g-object-unref layout))
    pen))

;; Dispatch -------------------------------------------------------

(defun fragment-list-width (cr fragments)
  (if *have-pango*
      (pango-fragment-list-width cr fragments)
      (cairo-fragment-list-width cr fragments)))

(defun draw-fragments (cr fragments x baseline-or-top &optional output-idx)
  "Paint FRAGMENTS at horizontal position X.
BASELINE-OR-TOP is the cairo baseline when *have-pango* is NIL, and
the layout top-edge otherwise (pango positions glyphs from the top).
When OUTPUT-IDX is supplied, `:clickable' fragments record their
painted bbox in *SUBFRAGMENT-HANDLERS* for click hit-testing."
  (if *have-pango*
      (pango-draw-fragments cr fragments x baseline-or-top output-idx)
      (cairo-draw-fragments cr fragments x baseline-or-top output-idx)))

;;; ---- Tray rendering ----
;;;
;;; The :tray module returns one synthetic fragment of the form
;;;   (:tray-placeholder LIST-OF-TRAY-FRAGMENTS)
;;; Each tray-fragment is either a :pixmap (cairo blit) or a :text
;;; fallback.  We special-case this in the renderer below so the
;;; existing flat-fragment pipeline doesn't need to know about
;;; bitmaps.

(defvar *tray-item-padding* 6.0d0
  "Pixels between adjacent tray icons / labels.")

(defvar *icon-cache* (make-hash-table :test #'equal)
  "Maps absolute PATH -> cairo surface pointer.  Loaded lazily by
`load-icon-file' so subsequent renders don't hit disk.  Surfaces
live for the process lifetime; the LRU concern is small (a couple
of dozen tray icons at most).")

(defun load-icon-file (path)
  "Return a cairo image surface for the PNG/SVG at PATH, or NIL."
  (or (gethash path *icon-cache*)
      (let ((surf (cairo-image-surface-create-from-png path)))
        (cond
          ((or (cffi:null-pointer-p surf)
               (not (zerop (cairo-surface-status surf))))
           (when (and surf (not (cffi:null-pointer-p surf)))
             (cairo-surface-destroy surf))
           (logmsg :debug "tray: failed to load icon ~a" path)
           nil)
          (t (setf (gethash path *icon-cache*) surf)
             surf)))))

(defun blit-cairo-surface (cr src-surf dest-x dest-y dest-size)
  "Blit cairo surface SRC-SURF (any size) at DEST-X DEST-Y scaled
to DEST-SIZE pixels square."
  (let* ((sw (cairo-image-surface-get-width src-surf))
         (sh (cairo-image-surface-get-height src-surf))
         (scale (/ (coerce dest-size 'double-float)
                   (max (coerce sw 'double-float)
                        (coerce sh 'double-float)))))
    (cairo-save cr)
    (cairo-translate cr (coerce dest-x 'double-float)
                         (coerce dest-y 'double-float))
    (cairo-scale cr scale scale)
    (cairo-set-source-surface cr src-surf 0.0d0 0.0d0)
    (cairo-paint cr)
    (cairo-restore cr)))

(defun fragment-tray-p (f)
  "Return non-NIL if F is a synthetic tray placeholder fragment."
  (and (consp f) (eq (first f) :tray-placeholder)))

(defun tray-fragment-pixel-width (cr tray-frags)
  "Width the tray will consume on this output, given current fonts."
  (let ((w 0)
        (first t))
    (dolist (tf tray-frags)
      (unless first (incf w *tray-item-padding*))
      (setf first nil)
      (case (tray-fragment-kind tf)
        ((:pixmap :icon-file) (incf w *tray-icon-size*))
        (t                    (incf w (if *have-pango*
                                          (let ((lay (pango-cairo-create-layout cr))
                                                (fd  (pango-fd-from-string *wayland-font-spec*)))
                                            (pango-layout-set-fd lay fd)
                                            (prog1 (pango-text-width lay (tray-fragment-label tf))
                                              (pango-fd-free fd)
                                              (g-object-unref lay)))
                                          (cairo-text-width cr (tray-fragment-label tf)))))))
    w))

(defun blit-pixmap (cr ptr w h dest-x dest-y dest-size)
  "Blit the ARGB32 buffer at PTR (size W*H) onto CR at (DEST-X DEST-Y),
scaled to fit DEST-SIZE pixels square."
  (let* ((stride (* 4 w))
         (surf   (cairo-isc4d ptr +cairo-format-argb32+ w h stride))
         (scale  (/ (coerce dest-size 'double-float)
                    (max (coerce w 'double-float)
                         (coerce h 'double-float)))))
    (unwind-protect
         (progn
           (cairo-save cr)
           (cairo-translate cr (coerce dest-x 'double-float)
                                (coerce dest-y 'double-float))
           (cairo-scale cr scale scale)
           (cairo-set-source-surface cr surf 0.0d0 0.0d0)
           (cairo-paint cr)
           (cairo-restore cr))
      (cairo-surface-destroy surf))))

(defun draw-tray-fragments (cr tray-frags x baseline output-idx)
  "Paint TRAY-FRAGS starting at X, recording each item's bbox in
*TRAY-FRAGMENT-BBOXES* keyed by OUTPUT-IDX.  Returns the new pen X."
  (let ((pen   (coerce x 'double-float))
        (first t))
    (setf (gethash output-idx *tray-fragment-bboxes*) nil)
    (dolist (tf tray-frags)
      (unless first (incf pen *tray-item-padding*))
      (setf first nil)
      (let ((start pen))
        (case (tray-fragment-kind tf)
          (:pixmap
           (let* ((h (wlbar-output-height output-idx))
                  (dest-size *tray-icon-size*)
                  (dest-y (max 0 (/ (- h dest-size) 2.0d0))))
             (blit-pixmap cr (tray-fragment-pixmap-pointer tf)
                          (tray-fragment-pixmap-w tf)
                          (tray-fragment-pixmap-h tf)
                          pen dest-y dest-size)
             (incf pen dest-size)))
          (:icon-file
           (let* ((h (wlbar-output-height output-idx))
                  (dest-size *tray-icon-size*)
                  (dest-y (max 0 (/ (- h dest-size) 2.0d0)))
                  (surf (load-icon-file (tray-fragment-icon-path tf))))
             (cond
               (surf
                (blit-cairo-surface cr surf pen dest-y dest-size)
                (incf pen dest-size))
               (t
                ;; Icon path resolved but PNG couldn't be loaded
                ;; (perhaps SVG without librsvg).  Fall back to text.
                (apply #'cairo-set-source-rgba cr
                       (rgba->doubles (theme-color :normal)))
                (cairo-move-to cr pen baseline)
                (cairo-show-text cr (tray-fragment-label tf))
                (incf pen (cairo-text-width cr (tray-fragment-label tf)))))))
          (t
           (apply #'cairo-set-source-rgba cr (rgba->doubles (theme-color :normal)))
           (cond
             (*have-pango*
              (let ((lay (pango-cairo-create-layout cr))
                    (fd  (pango-fd-from-string *wayland-font-spec*)))
                (unwind-protect
                     (progn
                       (pango-layout-set-fd lay fd)
                       (pango-layout-set-text lay (tray-fragment-label tf) -1)
                       (cairo-move-to cr pen baseline)
                       (pango-cairo-show-layout cr lay)
                       (incf pen (pango-text-width lay (tray-fragment-label tf))))
                  (pango-fd-free fd)
                  (g-object-unref lay))))
             (t
              (cairo-move-to cr pen baseline)
              (cairo-show-text cr (tray-fragment-label tf))
              (incf pen (cairo-text-width cr (tray-fragment-label tf)))))))
        (push (list tf start pen)
              (gethash output-idx *tray-fragment-bboxes*))))
    (setf (gethash output-idx *tray-fragment-bboxes*)
          (nreverse (gethash output-idx *tray-fragment-bboxes*)))
    pen))

;;; ---- Per-module bounding boxes (for click hit-testing) ----

(defvar *module-bboxes* (make-hash-table :test 'eql)
  "Hash table OUTPUT-IDX -> list of (MODULE X-START X-END).
Re-populated on every render so click hit-testing always reflects
the latest geometry.")

(defun record-bbox (output-idx module x-start x-end)
  (push (list module x-start x-end)
        (gethash output-idx *module-bboxes*)))

(defun reset-bboxes (output-idx)
  (setf (gethash output-idx *module-bboxes*) nil))

(defun module-at-x (output-idx x)
  "Return the MODULE whose bbox on OUTPUT-IDX contains X, or NIL."
  (loop for (m start end) in (gethash output-idx *module-bboxes*)
        when (and (>= x start) (<= x end))
        return m))

;;; ---- Render: lay out modules, painting per-module so bboxes line up ----

(defun module-pixel-width (cr m)
  "Width that module M's fragments will paint to.  Special-cases
the synthetic :tray-placeholder fragment so the tray reserves
real icon pixels rather than text width."
  (let* ((v     (module-output m))
         (frags (module-output-fragments v)))
    (cond
      ((null frags) 0)
      ((and (= 1 (length frags)) (fragment-tray-p (first frags)))
       (tray-fragment-pixel-width cr (second (first frags))))
      (t (fragment-list-width cr frags)))))

(defun module-section-width (cr modules)
  "Sum the painted width of MODULES + inter-module gaps."
  (let ((total 0)
        (first t))
    (dolist (m modules)
      (let ((w (module-pixel-width cr m)))
        (when (plusp w)
          (unless first (incf total *wayland-module-gap*))
          (setf first nil)
          (incf total w))))
    total))

(defun draw-section (cr modules start-x baseline output-idx)
  "Paint MODULES starting at START-X, recording each module's bbox."
  (let ((pen   (coerce start-x 'double-float))
        (first t))
    (dolist (m modules)
      (let* ((v     (module-output m))
             (frags (module-output-fragments v)))
        (when frags
          (unless first (incf pen *wayland-module-gap*))
          (setf first nil)
          (let ((mod-start pen))
            (cond
              ;; Tray special case: pixmap blits + per-item bboxes.
              ((and (= 1 (length frags)) (fragment-tray-p (first frags)))
               (let ((tray-frags (second (first frags))))
                 (setf pen (draw-tray-fragments cr tray-frags pen
                                                  baseline output-idx))))
              (t
               (draw-fragments cr frags pen baseline output-idx)
               (incf pen (fragment-list-width cr frags))))
            (record-bbox output-idx m mod-start pen)))))
    pen))

(defun render-output (i instances)
  "Paint output index I.  Returns NIL when the output is unmapped."
  (let* ((w      (wlbar-output-width i))
         (h      (wlbar-output-height i))
         (data   (wlbar-output-pixels i))
         (stride (wlbar-output-stride i)))
    (when (or (zerop w) (zerop h) (cffi:null-pointer-p data))
      (return-from render-output nil))
    (reset-bboxes i)
    (reset-subfragments i)
    (let* ((surf (cairo-isc4d data +cairo-format-argb32+ w h stride))
           (cr   (cairo-create surf)))
      (unwind-protect
           (progn
             (paint-background cr w h)
             (unless *have-pango*
               (cairo-select-font-face cr *wayland-font-family* 0 0)
               (cairo-set-font-size cr *wayland-font-size*))
             (let* ((left-modules   (collect-modules-for :left   instances))
                    (center-modules (collect-modules-for :center instances))
                    (right-modules  (collect-modules-for :right  instances))
                    (pad *wayland-padding*)
                    (ypos (if *have-pango*
                              (/ (- h (* *wayland-font-size* 1.4)) 2.0)
                              (+ (/ h 2.0) (/ *wayland-font-size* 3.0d0)))))
               (draw-section cr left-modules pad ypos i)
               (let ((cw (module-section-width cr center-modules)))
                 (draw-section cr center-modules
                               (/ (- w cw) 2.0d0) ypos i))
               (let ((rw (module-section-width cr right-modules)))
                 (draw-section cr right-modules
                               (- w rw pad) ypos i))))
        (cairo-destroy cr)
        (cairo-surface-destroy surf)))
    (wlbar-output-commit i)
    ;; Tooltip lives on a separate floating surface; refresh after
    ;; the bar so bboxes for hit-testing are current.
    (update-tooltip-overlay i)
    t))

(defun render-frame (instances)
  "Paint every output's surface."
  (loop for i from 0 below (wlbar-output-count)
        do (render-output i instances)))

;;; ---- Tooltips ----
;;;
;;; When the pointer is currently inside one of our surfaces and is
;;; over a module that exposes a `:tooltip', we draw the tooltip
;;; text as an overlay floating above the hovered module's bbox.
;;; The overlay is painted inside the bar surface itself (we
;;; don't open a separate popup surface), so the bar's vertical
;;; space must accommodate it.  *wayland-tooltip-height* reserves
;;; that space; the rest of the bar shrinks to fit.

(defvar *wayland-tooltip-bg* '(0.0 0.0 0.0 0.85)
  "RGBA fill behind tooltip text.")

(defvar *wayland-tooltip-padding-x* 8.0d0)
(defvar *wayland-tooltip-padding-y* 4.0d0)
(defvar *wayland-tooltip-corner*    6.0d0
  "Tooltip background corner radius (pixels).  0 = sharp.")

(defun current-hover ()
  "Return (OUTPUT X) when the pointer is over one of our surfaces,
or NIL."
  (cffi:with-foreign-objects ((o :int) (x :double) (y :double))
    (when (eql 1 (wlbar-pointer-hover o x y))
      (list (cffi:mem-ref o :int) (cffi:mem-ref x :double)))))

(defun hovered-module (output-idx x)
  "Return the module under the cursor on OUTPUT-IDX at X."
  (module-at-x output-idx x))

(defun measure-text-width (cr text)
  "Return the painted width of TEXT in the current font."
  (if *have-pango*
      (let ((lay (pango-cairo-create-layout cr))
            (fd  (pango-fd-from-string *wayland-font-spec*)))
        (unwind-protect
             (progn (pango-layout-set-fd lay fd)
                    (pango-text-width lay text))
          (pango-fd-free fd)
          (g-object-unref lay)))
      (cairo-text-width cr text)))

(defun paint-tooltip-into (output-idx text bbox-centre)
  "Paint TEXT into OUTPUT-IDX's secondary tooltip surface, centred
horizontally on BBOX-CENTRE (surface-local pixel x of the hovered
module).  Returns T on success."
  (let* ((w (wlbar-output-width output-idx))
         (bar-h (wlbar-output-height output-idx)))
    (declare (ignore bar-h))
    ;; Measure the text using a throwaway 1x1 cairo surface (we don't
    ;; have the tooltip surface yet at the right size).
    (cffi:with-foreign-object (tmp :uint32 1)
      (let* ((measure-surf (cairo-isc4d tmp +cairo-format-argb32+ 1 1 4))
             (measure-cr   (cairo-create measure-surf))
             text-w box-w box-h)
        (unwind-protect
             (progn
               (unless *have-pango*
                 (cairo-select-font-face measure-cr *wayland-font-family* 0 0)
                 (cairo-set-font-size measure-cr *wayland-font-size*))
               (setf text-w (measure-text-width measure-cr text)
                     box-w  (round (+ text-w (* 2 *wayland-tooltip-padding-x*)))
                     box-h  (round (+ *wayland-font-size*
                                       (* 2 *wayland-tooltip-padding-y*)))))
          (cairo-destroy measure-cr)
          (cairo-surface-destroy measure-surf))

        ;; Clamp the tooltip's left edge inside the screen.
        (let* ((anchor-x (max 0
                              (min (- w box-w 4)
                                   (round (- bbox-centre (/ box-w 2.0d0))))))
               (rc (wlbar-tooltip-show output-idx anchor-x box-w box-h)))
          (cond
            ((zerop rc) nil)            ; configure not ready yet
            (t
             (let* ((tw     (wlbar-tooltip-width  output-idx))
                    (th     (wlbar-tooltip-height output-idx))
                    (data   (wlbar-tooltip-pixels output-idx))
                    (stride (wlbar-tooltip-stride output-idx)))
               (when (and (plusp tw) (plusp th) (not (cffi:null-pointer-p data)))
                 (let* ((surf (cairo-isc4d data +cairo-format-argb32+
                                            tw th stride))
                        (cr   (cairo-create surf)))
                   (unwind-protect
                        (progn
                          ;; Clear to transparent
                          (cairo-set-operator cr +cairo-operator-source+)
                          (cairo-set-source-rgba cr 0.0d0 0.0d0 0.0d0 0.0d0)
                          (cairo-paint cr)
                          (cairo-set-operator cr +cairo-operator-over+)
                          ;; Background rounded rect
                          (apply #'cairo-set-source-rgba cr
                                 (rgba->doubles *wayland-tooltip-bg*))
                          (cond
                            ((plusp *wayland-tooltip-corner*)
                             (rounded-rect-path cr 0 0 tw th
                                                 *wayland-tooltip-corner*)
                             (cairo-fill cr))
                            (t
                             (cairo-rectangle cr 0.0d0 0.0d0
                                               (coerce tw 'double-float)
                                               (coerce th 'double-float))
                             (cairo-fill cr)))
                          ;; Text
                          (unless *have-pango*
                            (cairo-select-font-face cr *wayland-font-family* 0 0)
                            (cairo-set-font-size cr *wayland-font-size*))
                          (apply #'cairo-set-source-rgba cr
                                 (rgba->doubles (theme-color :normal)))
                          (let ((tx (coerce *wayland-tooltip-padding-x* 'double-float))
                                (ty (if *have-pango*
                                        (coerce *wayland-tooltip-padding-y* 'double-float)
                                        (+ *wayland-tooltip-padding-y* *wayland-font-size*))))
                            (cond
                              (*have-pango*
                               (let ((lay (pango-cairo-create-layout cr))
                                     (fd  (pango-fd-from-string *wayland-font-spec*)))
                                 (unwind-protect
                                      (progn (pango-layout-set-fd lay fd)
                                             (pango-layout-set-text lay text -1)
                                             (cairo-move-to cr tx ty)
                                             (pango-cairo-show-layout cr lay))
                                   (pango-fd-free fd)
                                   (g-object-unref lay))))
                              (t
                               (cairo-move-to cr tx (coerce ty 'double-float))
                               (cairo-show-text cr text))))
                          t)
                     (cairo-destroy cr)
                     (cairo-surface-destroy surf))
                   (wlbar-tooltip-commit output-idx)
                   t))))))))))

(defvar *tooltip-state* (make-hash-table :test 'eql)
  "OUTPUT-IDX -> (MODULE TEXT) most recently displayed, used to
short-circuit re-painting the same tooltip every tick.")

(defun update-tooltip-overlay (output-idx)
  "Show or hide OUTPUT-IDX's tooltip surface based on current hover."
  (let* ((hover (current-hover))
         (hovered-on (and hover (first hover)))
         (x (and hover (round (second hover))))
         (m (and (eql hovered-on output-idx) (hovered-module output-idx x)))
         (text (and m (resolve-tooltip m))))
    (cond
      ((and m text (plusp (length text)))
       (let* ((bbox (find-if (lambda (b) (eq (first b) m))
                              (gethash output-idx *module-bboxes*)))
              (centre (and bbox (/ (+ (second bbox) (third bbox)) 2.0d0)))
              (prev (gethash output-idx *tooltip-state*)))
         (when bbox
           (unless (and prev (eq (first prev) m) (equal (second prev) text))
             (paint-tooltip-into output-idx text centre)
             (setf (gethash output-idx *tooltip-state*) (list m text))))))
      (t
       (when (gethash output-idx *tooltip-state*)
         (wlbar-tooltip-hide output-idx)
         (remhash output-idx *tooltip-state*))))))

;;; ---- Click event drain ----

(defun drain-pointer-events ()
  "Pull every queued pointer event from the C shim and dispatch."
  (cffi:with-foreign-object (evt '(:struct wlbar-pointer-event))
    (loop while (eql 1 (wlbar-poll-pointer-event evt))
          do (let* ((output-idx (cffi:foreign-slot-value
                                  evt '(:struct wlbar-pointer-event) 'output-idx))
                    (x          (cffi:foreign-slot-value
                                  evt '(:struct wlbar-pointer-event) 'x))
                    (button     (cffi:foreign-slot-value
                                  evt '(:struct wlbar-pointer-event) 'button))
                    (button-key (button->key button))
                    (module     (module-at-x output-idx x)))
               (when module
                 (dispatch-module-click module button-key output-idx x))))))

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
               (drain-pointer-events)
               (render-frame instances)))
    (wlbar-shutdown)))
