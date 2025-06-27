;;; lispbar-core.el --- Core frame management for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (exwm "0.24"))
;; Keywords: frames, exwm
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides the core frame management functionality for Lispbar.
;; It handles creation, positioning, and lifecycle management of the toolbar
;; frames, with support for multi-monitor setups and proper EXWM integration.
;;
;; The core module ensures that frames are properly positioned, undecorated,
;; and sticky across all workspaces. It provides the foundation for the
;; rendering and module systems.

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;; Variables

(defgroup lispbar nil
  "Customization group for Lispbar."
  :group 'exwm
  :prefix "lispbar-")

(defcustom lispbar-position 'top
  "Position of the toolbar on screen."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'lispbar)

(defcustom lispbar-height 28
  "Height of the toolbar in pixels."
  :type 'integer
  :group 'lispbar)

(defcustom lispbar-background-color nil
  "Background color for the toolbar.
If nil, uses the default frame background color."
  :type '(choice (const :tag "Default" nil)
                 (color :tag "Custom color"))
  :group 'lispbar)

(defcustom lispbar-foreground-color nil
  "Foreground color for the toolbar.
If nil, uses the default frame foreground color."
  :type '(choice (const :tag "Default" nil)
                 (color :tag "Custom color"))
  :group 'lispbar)

(defcustom lispbar-margin-left 0
  "Left margin for the toolbar in pixels."
  :type 'integer
  :group 'lispbar)

(defcustom lispbar-margin-right 0
  "Right margin for the toolbar in pixels."
  :type 'integer
  :group 'lispbar)

(defcustom lispbar-debug nil
  "Enable debug logging for Lispbar."
  :type 'boolean
  :group 'lispbar)

(defvar lispbar--frames nil
  "List of active Lispbar frames.")

(defvar lispbar--monitors nil
  "List of detected monitors with their geometry.")

(defvar lispbar--cleanup-functions nil
  "List of cleanup functions to call when disabling Lispbar.")

(defvar lispbar--initialized nil
  "Whether Lispbar has been initialized.")

;;; Logging

(defun lispbar--log (level message &rest args)
  "Log MESSAGE with LEVEL if debugging is enabled.
ARGS are passed to `format' for MESSAGE formatting."
  (when lispbar-debug
    (let ((formatted-msg (apply #'format message args)))
      (message "[lispbar-%s] %s" level formatted-msg))))

(defun lispbar-log (level message &rest args)
  "Public interface for logging MESSAGE with LEVEL.
ARGS are passed to `format' for MESSAGE formatting."
  (lispbar--log level message args))

;;; Monitor Management

(defun lispbar--detect-monitors ()
  "Detect available monitors and their geometry.
Returns a list of plists with monitor information."
  (lispbar--log 'debug "Detecting monitors")
  (condition-case err
      (if (and (boundp 'exwm-randr-workspace-monitor-plist)
               exwm-randr-workspace-monitor-plist)
          (lispbar--parse-randr-monitors)
        (lispbar--fallback-monitor-detection))
    (error
     (lispbar--log 'error "Monitor detection failed: %s" err)
     (list (lispbar--default-monitor-geometry)))))

(defun lispbar--parse-randr-monitors ()
  "Parse monitor information from EXWM RandR."
  (lispbar--log 'debug "Parsing RandR monitor information")
  ;; For now, provide a basic implementation
  ;; This would be expanded with actual RandR parsing
  (list (lispbar--default-monitor-geometry)))

(defun lispbar--fallback-monitor-detection ()
  "Fallback monitor detection when RandR is not available."
  (lispbar--log 'debug "Using fallback monitor detection")
  (list (lispbar--default-monitor-geometry)))

(defun lispbar--default-monitor-geometry ()
  "Return default monitor geometry."
  (list :x 0
        :y 0
        :width (display-pixel-width)
        :height (display-pixel-height)
        :name "primary"))

(defun lispbar--refresh-monitors ()
  "Refresh monitor information and update frames accordingly."
  (lispbar--log 'debug "Refreshing monitor information")
  (setq lispbar--monitors (lispbar--detect-monitors))
  (when lispbar--frames
    (lispbar--update-frame-positions)))

;;; Frame Geometry Calculation

(defun lispbar--calculate-frame-geometry (monitor)
  "Calculate frame geometry for MONITOR.
Returns a plist with :x, :y, :width, and :height."
  (let* ((monitor-x (plist-get monitor :x))
         (monitor-y (plist-get monitor :y))
         (monitor-width (plist-get monitor :width))
         (monitor-height (plist-get monitor :height))
         (frame-width (- monitor-width lispbar-margin-left lispbar-margin-right))
         (frame-x (+ monitor-x lispbar-margin-left))
         (frame-y (if (eq lispbar-position 'top)
                      monitor-y
                    (- (+ monitor-y monitor-height) lispbar-height))))
    (list :x frame-x
          :y frame-y
          :width frame-width
          :height lispbar-height)))

(defun lispbar--validate-geometry (geometry)
  "Validate and sanitize frame GEOMETRY.
Returns corrected geometry or signals an error."
  (let ((x (plist-get geometry :x))
        (y (plist-get geometry :y))
        (width (plist-get geometry :width))
        (height (plist-get geometry :height)))
    (unless (and (integerp x) (integerp y) 
                 (integerp width) (integerp height)
                 (>= width 1) (>= height 1))
      (error "Invalid frame geometry: %S" geometry))
    (list :x (max 0 x)
          :y (max 0 y)
          :width (max 1 width)
          :height (max 1 height))))

;;; Frame Creation and Management

(defun lispbar--create-frame (geometry &optional monitor-name)
  "Create a new Lispbar frame with GEOMETRY.
MONITOR-NAME is used for identification purposes."
  (lispbar--log 'debug "Creating frame for monitor %s with geometry %S" 
                monitor-name geometry)
  (let* ((validated-geometry (lispbar--validate-geometry geometry))
         (frame-params (lispbar--build-frame-parameters validated-geometry))
         (frame (make-frame frame-params)))
    (when frame
      (lispbar--configure-frame frame validated-geometry)
      (lispbar--log 'info "Created frame for monitor %s" monitor-name)
      frame)))

(defun lispbar--build-frame-parameters (geometry)
  "Build frame parameters list from GEOMETRY."
  (let ((params `((name . "Lispbar")
                  (title . "Lispbar")
                  (left . ,(plist-get geometry :x))
                  (top . ,(plist-get geometry :y))
                  (width . ,(plist-get geometry :width))
                  (height . ,(plist-get geometry :height))
                  (min-width . ,(plist-get geometry :width))
                  (min-height . ,(plist-get geometry :height))
                  (border-width . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (tab-bar-lines . 0)
                  (line-spacing . 0)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (undecorated . t)
                  (override-redirect . t)
                  (desktop-dont-save . t)
                  (visibility . t))))
    (when lispbar-background-color
      (push `(background-color . ,lispbar-background-color) params))
    (when lispbar-foreground-color
      (push `(foreground-color . ,lispbar-foreground-color) params))
    params))

(defun lispbar--configure-frame (frame geometry)
  "Configure FRAME after creation with GEOMETRY."
  (with-selected-frame frame
    ;; Set frame to be sticky and always on top
    (when (fboundp 'x-change-window-property)
      ;; Set window type to dock
      (x-change-window-property 
       "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK" 
       frame nil 'ATOM 32 t)
      
      ;; Set window to be sticky (visible on all workspaces)
      (x-change-window-property 
       "_NET_WM_DESKTOP" 0xFFFFFFFF frame nil 'CARDINAL 32 t)
      
      ;; Reserve space for the toolbar
      (when (eq lispbar-position 'top)
        (x-change-window-property 
         "_NET_WM_STRUT_PARTIAL" 
         (vector 0 0 lispbar-height 0 0 0 0 0 
                 (plist-get geometry :x) 
                 (+ (plist-get geometry :x) (plist-get geometry :width))
                 0 0)
         frame nil 'CARDINAL 32 t))
      
      (when (eq lispbar-position 'bottom)
        (x-change-window-property 
         "_NET_WM_STRUT_PARTIAL" 
         (vector 0 0 0 lispbar-height 0 0 0 0 0 0
                 (plist-get geometry :x) 
                 (+ (plist-get geometry :x) (plist-get geometry :width)))
         frame nil 'CARDINAL 32 t)))))

(defun lispbar--create-frames ()
  "Create Lispbar frames for all detected monitors."
  (lispbar--log 'debug "Creating frames for %d monitors" 
                (length lispbar--monitors))
  (setq lispbar--frames nil)
  (dolist (monitor lispbar--monitors)
    (let* ((geometry (lispbar--calculate-frame-geometry monitor))
           (monitor-name (plist-get monitor :name))
           (frame (lispbar--create-frame geometry monitor-name)))
      (when frame
        (push `(:frame ,frame 
                :monitor ,monitor-name 
                :geometry ,geometry) 
              lispbar--frames)))))

(defun lispbar--update-frame-positions ()
  "Update positions of existing frames based on current monitor configuration."
  (lispbar--log 'debug "Updating frame positions")
  (when lispbar--frames
    (dolist (frame-info lispbar--frames)
      (let* ((frame (plist-get frame-info :frame))
             (monitor-name (plist-get frame-info :monitor))
             (monitor (cl-find-if (lambda (m) 
                                    (string= (plist-get m :name) monitor-name))
                                  lispbar--monitors)))
        (when (and frame monitor (frame-live-p frame))
          (let ((new-geometry (lispbar--calculate-frame-geometry monitor)))
            (lispbar--log 'debug "Updating frame position for monitor %s" monitor-name)
            (modify-frame-parameters 
             frame 
             `((left . ,(plist-get new-geometry :x))
               (top . ,(plist-get new-geometry :y))
               (width . ,(plist-get new-geometry :width))
               (height . ,(plist-get new-geometry :height))))
            (setf (plist-get frame-info :geometry) new-geometry)))))))

;;; Frame Cleanup

(defun lispbar--destroy-frame (frame-info)
  "Destroy a single frame described by FRAME-INFO."
  (let ((frame (plist-get frame-info :frame))
        (monitor-name (plist-get frame-info :monitor)))
    (when (and frame (frame-live-p frame))
      (lispbar--log 'debug "Destroying frame for monitor %s" monitor-name)
      (delete-frame frame))))

(defun lispbar--cleanup-frames ()
  "Clean up all Lispbar frames."
  (lispbar--log 'debug "Cleaning up %d frames" (length lispbar--frames))
  (dolist (frame-info lispbar--frames)
    (lispbar--destroy-frame frame-info))
  (setq lispbar--frames nil))

;;; Lifecycle Management

(defun lispbar--add-cleanup-function (fn)
  "Add FN to the list of cleanup functions."
  (push fn lispbar--cleanup-functions))

(defun lispbar--run-cleanup-functions ()
  "Run all registered cleanup functions."
  (lispbar--log 'debug "Running %d cleanup functions" 
                (length lispbar--cleanup-functions))
  (dolist (fn lispbar--cleanup-functions)
    (condition-case err
        (funcall fn)
      (error
       (lispbar--log 'error "Cleanup function failed: %s" err))))
  (setq lispbar--cleanup-functions nil))

;;;###autoload
(defun lispbar-init ()
  "Initialize Lispbar core system.
This function sets up the frame management system and detects monitors."
  (interactive)
  (when lispbar--initialized
    (lispbar--log 'warning "Lispbar already initialized")
    (return-from lispbar-init))
  
  (lispbar--log 'info "Initializing Lispbar core system")
  
  (condition-case err
      (progn
        ;; Detect monitors
        (setq lispbar--monitors (lispbar--detect-monitors))
        (lispbar--log 'info "Detected %d monitors" (length lispbar--monitors))
        
        ;; Create frames
        (lispbar--create-frames)
        (lispbar--log 'info "Created %d frames" (length lispbar--frames))
        
        ;; Add EXWM hooks if available
        (when (featurep 'exwm-randr)
          (add-hook 'exwm-randr-screen-change-hook #'lispbar--refresh-monitors)
          (lispbar--add-cleanup-function 
           (lambda () 
             (remove-hook 'exwm-randr-screen-change-hook #'lispbar--refresh-monitors))))
        
        (setq lispbar--initialized t)
        (lispbar--log 'info "Lispbar core initialization complete"))
    
    (error
     (lispbar--log 'error "Lispbar initialization failed: %s" err)
     (lispbar-cleanup)
     (signal (car err) (cdr err)))))

;;;###autoload
(defun lispbar-cleanup ()
  "Clean up Lispbar and restore the previous state.
This function removes all frames and runs cleanup functions."
  (interactive)
  (lispbar--log 'info "Cleaning up Lispbar")
  
  ;; Clean up frames
  (lispbar--cleanup-frames)
  
  ;; Run cleanup functions
  (lispbar--run-cleanup-functions)
  
  ;; Reset state
  (setq lispbar--monitors nil
        lispbar--initialized nil)
  
  (lispbar--log 'info "Lispbar cleanup complete"))

;;;###autoload
(defun lispbar-refresh ()
  "Refresh Lispbar by updating monitor detection and frame positions.
This function is useful when display configuration changes."
  (interactive)
  (when lispbar--initialized
    (lispbar--log 'info "Refreshing Lispbar")
    (lispbar--refresh-monitors)
    (lispbar--log 'info "Lispbar refresh complete")))

;;; Utility Functions

(defun lispbar-get-frames ()
  "Return list of active Lispbar frames.
Each element is a plist with :frame, :monitor, and :geometry keys."
  lispbar--frames)

(defun lispbar-get-frame-by-monitor (monitor-name)
  "Return frame information for MONITOR-NAME, or nil if not found."
  (cl-find-if (lambda (frame-info)
                (string= (plist-get frame-info :monitor) monitor-name))
              lispbar--frames))

(defun lispbar-frame-live-p (frame-info)
  "Return non-nil if FRAME-INFO represents a live frame."
  (and frame-info
       (plist-get frame-info :frame)
       (frame-live-p (plist-get frame-info :frame))))

(defun lispbar-validate-config ()
  "Validate current Lispbar configuration.
Returns a list of configuration issues, or nil if configuration is valid."
  (let ((issues nil))
    (unless (memq lispbar-position '(top bottom))
      (push "lispbar-position must be 'top or 'bottom" issues))
    (unless (and (integerp lispbar-height) (> lispbar-height 0))
      (push "lispbar-height must be a positive integer" issues))
    (unless (and (integerp lispbar-margin-left) (>= lispbar-margin-left 0))
      (push "lispbar-margin-left must be a non-negative integer" issues))
    (unless (and (integerp lispbar-margin-right) (>= lispbar-margin-right 0))
      (push "lispbar-margin-right must be a non-negative integer" issues))
    (when (and lispbar-background-color 
               (not (color-defined-p lispbar-background-color)))
      (push "lispbar-background-color is not a valid color" issues))
    (when (and lispbar-foreground-color 
               (not (color-defined-p lispbar-foreground-color)))
      (push "lispbar-foreground-color is not a valid color" issues))
    issues))

;;; Safe Function Wrapper

(defun lispbar-safe-call (fn &rest args)
  "Safely call FN with ARGS, log errors and return nil on failure."
  (condition-case err
      (apply fn args)
    (error
     (lispbar--log 'error "Function %s failed: %s" fn err)
     nil)))

(provide 'lispbar-core)
;;; lispbar-core.el ends here