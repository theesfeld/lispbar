;;; lispbar-backend-exwm.el --- EXWM/X11 backend for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: frames, exwm, x11

;;; Commentary:

;; Wraps the existing `lispbar-exwm' integration as a `lispbar-backend'
;; subclass.  The class is registered only when EXWM is actually
;; loaded so that Lispbar can run without EXWM installed.
;;
;; On X11 the backend sets _NET_WM_WINDOW_TYPE and _NET_WM_STRUT_PARTIAL
;; window properties so docks reserve screen space correctly.  These
;; properties are the X11 equivalent of the layer-shell anchors used
;; on Wayland and have no effect on a Wayland compositor.

;;; Code:

(require 'cl-lib)
(require 'lispbar-backend)

;; lispbar-exwm is optional; require it lazily so this file remains
;; loadable in plain X11 sessions without EXWM.
(declare-function lispbar-exwm-init             "lispbar-exwm")
(declare-function lispbar-exwm-cleanup          "lispbar-exwm")
(declare-function lispbar-exwm-refresh          "lispbar-exwm")
(declare-function lispbar-exwm-available-p      "lispbar-exwm")
(declare-function lispbar-exwm-current-workspace "lispbar-exwm")
(declare-function lispbar-exwm-workspace-names  "lispbar-exwm")
(declare-function lispbar-exwm-window-title     "lispbar-exwm")
(declare-function lispbar-exwm-window-class     "lispbar-exwm")

(defclass lispbar-backend-exwm (lispbar-backend) ()
  "Backend that drives Lispbar inside an EXWM/X11 session.")

(cl-defmethod lispbar-backend-available-p ((_ lispbar-backend-exwm))
  (and (display-graphic-p)
       (eq window-system 'x)
       (featurep 'exwm)))

(cl-defmethod lispbar-backend-init ((_ lispbar-backend-exwm))
  (require 'lispbar-exwm)
  (lispbar-exwm-init))

(cl-defmethod lispbar-backend-cleanup ((_ lispbar-backend-exwm))
  (when (fboundp 'lispbar-exwm-cleanup)
    (lispbar-exwm-cleanup)))

(cl-defmethod lispbar-backend-detect-monitors ((_ lispbar-backend-exwm))
  ;; Defer to the existing comprehensive X11 detection in lispbar-core.
  (if (fboundp 'lispbar--perform-monitor-detection)
      (lispbar--perform-monitor-detection)
    (lispbar-backend--monitors-from-attributes)))

(cl-defmethod lispbar-backend-configure-frame
  ((_ lispbar-backend-exwm) frame geometry)
  "Set X11 dock window type and strut hints on FRAME."
  (when (and (frame-live-p frame)
             (eq (framep frame) 'x)
             (fboundp 'x-change-window-property))
    (let* ((position (plist-get geometry :position))
           (height (plist-get geometry :height))
           (x (plist-get geometry :x))
           (width (plist-get geometry :width)))
      (with-selected-frame frame
        (if (eq position 'floating)
            (x-change-window-property
             "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NORMAL"
             frame nil 'ATOM 32 t)
          (x-change-window-property
           "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"
           frame nil 'ATOM 32 t))
        (x-change-window-property
         "_NET_WM_DESKTOP" #xFFFFFFFF frame nil 'CARDINAL 32 t)
        (cl-case position
          ((top top-offset)
           (x-change-window-property
            "_NET_WM_STRUT_PARTIAL"
            (vector 0 0 height 0 0 0 0 0 x (+ x width) 0 0)
            frame nil 'CARDINAL 32 t))
          ((bottom bottom-offset)
           (x-change-window-property
            "_NET_WM_STRUT_PARTIAL"
            (vector 0 0 0 height 0 0 0 0 0 0 x (+ x width))
            frame nil 'CARDINAL 32 t)))))))

(cl-defmethod lispbar-backend-register-hotplug
  ((_ lispbar-backend-exwm) callback)
  (when (featurep 'exwm-randr)
    (add-hook 'exwm-randr-screen-change-hook callback))
  callback)

(cl-defmethod lispbar-backend-unregister-hotplug
  ((_ lispbar-backend-exwm) token)
  (when (featurep 'exwm-randr)
    (remove-hook 'exwm-randr-screen-change-hook token)))

(cl-defmethod lispbar-backend-current-workspace ((_ lispbar-backend-exwm))
  (when (fboundp 'lispbar-exwm-current-workspace)
    (lispbar-exwm-current-workspace)))

(cl-defmethod lispbar-backend-workspace-names ((_ lispbar-backend-exwm))
  (when (fboundp 'lispbar-exwm-workspace-names)
    (lispbar-exwm-workspace-names)))

(cl-defmethod lispbar-backend-switch-workspace
  ((_ lispbar-backend-exwm) index)
  (when (fboundp 'exwm-workspace-switch)
    (condition-case _err
        (progn (exwm-workspace-switch index) t)
      (error nil))))

(cl-defmethod lispbar-backend-register-workspace-hook
  ((_ lispbar-backend-exwm) callback)
  (add-hook 'exwm-workspace-switch-hook callback)
  callback)

(cl-defmethod lispbar-backend-unregister-workspace-hook
  ((_ lispbar-backend-exwm) token)
  (remove-hook 'exwm-workspace-switch-hook token))

(cl-defmethod lispbar-backend-window-title ((_ lispbar-backend-exwm))
  (when (fboundp 'lispbar-exwm-window-title)
    (lispbar-exwm-window-title)))

(cl-defmethod lispbar-backend-window-class ((_ lispbar-backend-exwm))
  (when (fboundp 'lispbar-exwm-window-class)
    (lispbar-exwm-window-class)))

(lispbar-backend-register
 (make-instance 'lispbar-backend-exwm
                :name 'exwm :priority 90
                :features '(monitor-detect workspaces window-info
                            hotplug struts)))

(provide 'lispbar-backend-exwm)
;;; lispbar-backend-exwm.el ends here
