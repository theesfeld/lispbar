;;;; tray.lisp  --  StatusNotifierItem host module.
;;;;
;;;; FFIs into libwltray.so, which talks D-Bus on the session bus,
;;;; owns org.kde.StatusNotifierWatcher, and tracks every registered
;;;; tray item.  For each item we render either its inline pixmap
;;;; (cairo blit) or its Id as text; left/middle/right clicks invoke
;;;; Activate / SecondaryActivate / ContextMenu respectively.

(in-package #:lispbar)

;;; ---- libwltray FFI ----

(cffi:define-foreign-library libwltray
  (:unix (:or "/usr/local/lib/lispbar/libwltray.so"
              "/usr/lib/lispbar/libwltray.so"
              "./cshim/libwltray.so"
              "./libwltray.so"
              "libwltray.so"))
  (t (:default "libwltray")))

(defvar *wltray-loaded* nil)
(defvar *wltray-ready*  nil
  "Set to T once wltray_init succeeded.  Set to :failed if init
fails; the tray module then stays silent.")

(cffi:defcstruct wltray-item
  (id         :string)
  (title      :string)
  (status     :string)
  (icon-name  :string)
  (tooltip    :string)
  (has-pixmap :int)
  (pixmap-w   :int)
  (pixmap-h   :int)
  (pixmap     :pointer))

(cffi:defcfun ("wltray_init"        wltray-c-init)        :int)
(cffi:defcfun ("wltray_shutdown"    wltray-c-shutdown)    :void)
(cffi:defcfun ("wltray_fd"          wltray-c-fd)          :int)
(cffi:defcfun ("wltray_poll"        wltray-c-poll)        :int (timeout :int))
(cffi:defcfun ("wltray_item_count"  wltray-c-item-count)  :int)
(cffi:defcfun ("wltray_item_get"    wltray-c-item-get)    :int
  (i :int) (out :pointer))
(cffi:defcfun ("wltray_revision"    wltray-c-revision)    :uint)
(cffi:defcfun ("wltray_invoke"      wltray-c-invoke)      :void
  (i :int) (button :int) (x :int) (y :int))

(defun ensure-wltray-loaded ()
  (unless *wltray-loaded*
    (handler-case
        (progn (cffi:use-foreign-library libwltray)
               (setf *wltray-loaded* t))
      (error (c)
        (logmsg :warn "tray: cannot load libwltray.so: ~a" c)
        (setf *wltray-loaded* :failed))))
  (eq *wltray-loaded* t))

(defun ensure-wltray-ready ()
  (cond
    ((eq *wltray-ready* t) t)
    ((eq *wltray-ready* :failed) nil)
    ((not (ensure-wltray-loaded)) (setf *wltray-ready* :failed) nil)
    (t (let ((rc (wltray-c-init)))
         (cond ((zerop rc)
                (setf *wltray-ready* t)
                (logmsg :info "tray: connected to session bus")
                t)
               (t
                (logmsg :warn "tray: wltray_init failed (rc=~a)" rc)
                (setf *wltray-ready* :failed)
                nil))))))

;;; ---- Public tunables ----

(defvar *tray-icon-size* 16
  "Pixel height to scale tray icons to.  Use the bar height minus
some padding; the actual surface space is whatever the icon
renderer takes.")

(defvar *tray-show-text-when-no-icon* t
  "When the SNI item has no pixmap, fall back to displaying the
item Id as text.  NIL hides such items entirely.")

(defvar *tray-poll-interval* 1.0
  "How often (seconds) the tray module re-renders.  Even when items
don't change, this is the cadence at which we visit
wltray-c-poll for D-Bus traffic.")

;;; ---- Rendering ----
;;;
;;; The bar's flat-fragment renderer paints text, not bitmaps.  We
;;; reuse the same fragment shape for tray items: each item's
;;; fragment carries its TEXT label, but the wayland renderer
;;; recognises a special pixmap-fragment shape and paints the icon
;;; instead.  Two layers:
;;;
;;;   (:pixmap PTR W H)        - cairo-blit this ARGB32 image
;;;   (TEXT FACE)              - normal text fragment
;;;
;;; module-output-fragments is what build-instances eventually
;;; feeds the renderer.

(defstruct tray-fragment
  "Internal: one tray item, ready to render."
  (kind :text)       ; :pixmap | :text
  pixmap-pointer
  pixmap-w
  pixmap-h
  label
  index)             ; original index into the C-side array

(defun tray-collect-items ()
  "Snapshot the C-side item list into TRAY-FRAGMENT records, ready
to render.  Drives the C poll cycle first so metadata is fresh."
  (when (ensure-wltray-ready)
    (wltray-c-poll 0)
    (cffi:with-foreign-object (it '(:struct wltray-item))
      (let ((n (wltray-c-item-count))
            (frags nil))
        (dotimes (i n)
          (when (eql 1 (wltray-c-item-get i it))
            (let* ((has-pix (cffi:foreign-slot-value
                              it '(:struct wltray-item) 'has-pixmap))
                   (id     (cffi:foreign-slot-value
                             it '(:struct wltray-item) 'id))
                   (title  (cffi:foreign-slot-value
                             it '(:struct wltray-item) 'title))
                   (label  (or (and title (plusp (length title)) title)
                               (and id (plusp (length id)) id)
                               "?")))
              (cond
                ((not (zerop has-pix))
                 (push (make-tray-fragment
                         :kind :pixmap
                         :pixmap-pointer (cffi:foreign-slot-value
                                           it '(:struct wltray-item) 'pixmap)
                         :pixmap-w (cffi:foreign-slot-value
                                     it '(:struct wltray-item) 'pixmap-w)
                         :pixmap-h (cffi:foreign-slot-value
                                     it '(:struct wltray-item) 'pixmap-h)
                         :label label
                         :index i)
                       frags))
                (*tray-show-text-when-no-icon*
                 (push (make-tray-fragment :kind :text
                                            :label label
                                            :index i)
                       frags))))))
        (nreverse frags)))))

;;; ---- Click handling ----

(defvar *tray-fragment-bboxes* (make-hash-table :test 'eql)
  "OUTPUT-IDX -> list of (TRAY-FRAGMENT X-START X-END), populated by
the Wayland renderer so click hit-testing can route to individual
tray items rather than the whole module.")

(defun tray-item-at-x (output-idx x)
  "Return the tray-fragment whose painted x-range on OUTPUT-IDX
contains X, or NIL."
  (loop for (f s e) in (gethash output-idx *tray-fragment-bboxes*)
        when (and (>= x s) (<= x e)) return f))

(defun tray-on-click (module button output-idx)
  "Routed from dispatch-module-click.  Translate button -> SNI verb
and invoke on the item under the click x (read from `*click-x*')."
  (declare (ignore module output-idx))
  (when (ensure-wltray-ready)
    (let* ((x (round *click-x*))
           (frag (tray-item-at-x *click-output* x))
           (sni-button (case button
                         (:left   0)
                         (:middle 1)
                         (:right  2)
                         (t       0))))
      (when frag
        (logmsg :debug "tray: invoke ~a on item ~a (~a)"
                button (tray-fragment-index frag)
                (tray-fragment-label frag))
        (wltray-c-invoke (tray-fragment-index frag) sni-button x 0)))))

;;; ---- The module ----
;;;
;;; The module returns a single :text fragment so the bar's existing
;;; layout engine reserves space.  The Wayland renderer special-cases
;;; the :tray module name and walks the live tray-fragment list to
;;; paint icons, recording per-item bboxes for click hit-testing.

(defmodule :tray
  (:doc "StatusNotifierItem (SNI) system tray.  See *tray-icon-size*
and *tray-show-text-when-no-icon* to tune appearance."
   :position :right :priority 30 :interval 1.0
   :on-click ((:left   tray-on-click)
              (:middle tray-on-click)
              (:right  tray-on-click)))
  ;; Return a single placeholder fragment whose displayed width is
  ;; computed from the actual icon set during render.  This is just
  ;; a sentinel; the wayland renderer notices :tray and substitutes.
  (let ((items (tray-collect-items)))
    (cond
      ((null items) nil)
      (t (list :fragments
               (list (list :tray-placeholder items)))))))
