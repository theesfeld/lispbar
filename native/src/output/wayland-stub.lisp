;;;; wayland-stub.lisp  --  Placeholder for the native layer-shell driver.
;;;
;;; The full implementation will:
;;;   1. Open a connection to the compositor via libwayland-client.
;;;   2. Bind wl_compositor, wl_shm, wl_output and zwlr_layer_shell_v1.
;;;   3. For every wl_output, create a layer surface anchored to top/bottom,
;;;      ask for an exclusive zone (the bar height), and render with cairo.
;;;   4. Run the wl_display event loop, dispatching periodic timer ticks
;;;      that refresh modules and damage the surface.
;;;
;;; That requires:
;;;   * libwayland-client FFI bindings  (cffi-grovel against wayland-client.h)
;;;   * generated glue for wlr-layer-shell-unstable-v1.xml (wayland-scanner)
;;;   * cairo + pango FFI (cl-cairo2, cl-pango)
;;;
;;; Until that lands, this stub falls back to the stdout driver and
;;; logs a clear instruction to the user.

(in-package #:lispbar)

(defun run-wayland-stub (config)
  (logmsg :warn
          "wayland output not yet implemented; falling back to :stdout.")
  (logmsg :info
          "When the layer-shell driver is ready, set :output :wayland in")
  (logmsg :info "your config.lisp to render directly with cairo.")
  (run-stdout config))
