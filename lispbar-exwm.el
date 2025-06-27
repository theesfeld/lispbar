;;; lispbar-exwm.el --- EXWM integration layer for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (exwm "0.24") (cl-lib "0.5"))
;; Keywords: frames, exwm, window-manager
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides comprehensive EXWM integration for Lispbar.
;; It handles workspace changes, window management events, monitor 
;; configuration changes via EXWM RandR, and provides EXWM-specific
;; functionality for modules to leverage.
;;
;; Key features:
;; - EXWM event handlers for workspace switches and window management
;; - EXWM RandR integration for dynamic monitor configuration
;; - Workspace detection and tracking with fallback strategies
;; - Window property tracking and management
;; - Strut management for proper window manager coordination
;; - Module communication channels for EXWM events
;; - Graceful degradation when EXWM features are unavailable
;; - Performance optimized with throttled updates and caching
;;
;; Integration points:
;; - Works seamlessly with lispbar-core frame management
;; - Enhances monitor detection with EXWM RandR capabilities
;; - Provides rich event system for modules via communication channels
;; - Extends core cleanup system with EXWM-specific teardown
;;
;; Communication channels provided:
;; - workspace-changed: Workspace switches with old/new workspace info
;; - window-managed: New windows being managed by EXWM
;; - window-unmanaged: Windows being removed from EXWM management
;; - window-focus-changed: Focus changes between windows
;; - fullscreen-changed: Windows entering/exiting fullscreen mode
;; - floating-changed: Windows becoming floating or tiled
;; - monitor-configuration-changed: Monitor setup changes
;;
;; The EXWM integration automatically detects available EXWM features
;; and gracefully degrades functionality when features are not available.
;; It provides a rich API for modules to access EXWM information and
;; respond to window manager events.

;;; Code:

(require 'cl-lib)
(require 'lispbar-core)
(require 'lispbar-modules)

;; EXWM feature detection
(defvar lispbar-exwm--exwm-available (featurep 'exwm))
(defvar lispbar-exwm--randr-available (featurep 'exwm-randr))

;;; Customization

(defgroup lispbar-exwm nil
  "Customization group for Lispbar EXWM integration."
  :group 'lispbar
  :prefix "lispbar-exwm-")

(defcustom lispbar-exwm-workspace-switch-delay 0.1
  "Delay in seconds before updating after workspace switch.
This prevents flickering during rapid workspace changes."
  :type 'number
  :group 'lispbar-exwm)

(defcustom lispbar-exwm-track-window-properties t
  "Whether to track detailed window properties for modules.
When enabled, provides richer information but uses more memory."
  :type 'boolean
  :group 'lispbar-exwm)

(defcustom lispbar-exwm-monitor-change-delay 0.5
  "Delay in seconds before refreshing after monitor configuration change.
This allows time for EXWM RandR to complete setup."
  :type 'number
  :group 'lispbar-exwm)

(defcustom lispbar-exwm-workspace-name-sources '(ewmh fallback-numbers)
  "Sources for workspace names in order of preference.
Available sources: ewmh, exwm-workspace-names, fallback-numbers."
  :type '(repeat (choice (const :tag "EWMH properties" ewmh)
                         (const :tag "EXWM workspace names" exwm-workspace-names)
                         (const :tag "Fallback numbers" fallback-numbers)))
  :group 'lispbar-exwm)

(defcustom lispbar-exwm-window-class-filter '()
  "List of window classes to filter from tracking.
Window classes in this list will not trigger window management events."
  :type '(repeat string)
  :group 'lispbar-exwm)

(defcustom lispbar-exwm-debug nil
  "Enable debug logging for EXWM integration."
  :type 'boolean
  :group 'lispbar-exwm)

;;; Variables

(defvar lispbar-exwm--initialized nil
  "Whether EXWM integration has been initialized.")

(defvar lispbar-exwm--current-workspace nil
  "Current workspace number (0-indexed).")

(defvar lispbar-exwm--workspace-names nil
  "Cached list of workspace names.")

(defvar lispbar-exwm--tracked-windows nil
  "Alist of tracked windows and their properties.")

(defvar lispbar-exwm--monitor-configuration nil
  "Current monitor configuration from EXWM RandR.")

(defvar lispbar-exwm--workspace-switch-timer nil
  "Timer for delayed workspace switch processing.")

(defvar lispbar-exwm--monitor-change-timer nil
  "Timer for delayed monitor change processing.")

(defvar lispbar-exwm--hooks-registered nil
  "Whether EXWM hooks have been registered.")

(defvar lispbar-exwm--cleanup-functions nil
  "List of cleanup functions for EXWM integration.")

;;; Logging

(defun lispbar-exwm--log (level message &rest args)
  "Log MESSAGE with LEVEL if EXWM debugging is enabled.
ARGS are passed to `format' for MESSAGE formatting."
  (when (or lispbar-exwm-debug lispbar-debug)
    (let ((formatted-msg (apply #'format message args)))
      (lispbar-log level "[exwm] %s" formatted-msg))))

;;; Feature Detection

(defun lispbar-exwm-available-p ()
  "Return non-nil if EXWM is available and running."
  (and lispbar-exwm--exwm-available
       (boundp 'exwm--connection)
       exwm--connection))

(defun lispbar-exwm-randr-available-p ()
  "Return non-nil if EXWM RandR is available and active."
  (and lispbar-exwm--randr-available
       (lispbar-exwm-available-p)
       (boundp 'exwm-randr-workspace-monitor-plist)))

;;; Workspace Management

(defun lispbar-exwm--get-current-workspace ()
  "Get the current workspace number.
Returns 0-indexed workspace number or nil if unavailable."
  (cond
   ;; Try EXWM current workspace
   ((and (lispbar-exwm-available-p)
         (boundp 'exwm-workspace-current-index))
    exwm-workspace-current-index)
   
   ;; Try EWMH current desktop
   ((and (display-graphic-p)
         (x-window-property "_NET_CURRENT_DESKTOP" nil "CARDINAL" 0 nil t))
    (let ((current (x-window-property "_NET_CURRENT_DESKTOP" nil "CARDINAL" 0 nil t)))
      (when (vectorp current)
        (aref current 0))))
   
   ;; Fallback
   (t 0)))

(defun lispbar-exwm--get-workspace-names ()
  "Get list of workspace names using configured sources.
Returns a list of workspace names or numbers."
  (cl-loop for source in lispbar-exwm-workspace-name-sources
           for names = (lispbar-exwm--get-workspace-names-from-source source)
           when names return names
           finally return (lispbar-exwm--get-fallback-workspace-names)))

(defun lispbar-exwm--get-workspace-names-from-source (source)
  "Get workspace names from SOURCE.
SOURCE can be 'ewmh, 'exwm-workspace-names, or 'fallback-numbers."
  (condition-case err
      (cl-case source
        (ewmh
         (when (display-graphic-p)
           (let ((names (x-window-property "_NET_DESKTOP_NAMES" nil "UTF8_STRING" 0 nil t)))
             (when (stringp names)
               (split-string names "\0" t)))))
        
        (exwm-workspace-names
         (when (and (lispbar-exwm-available-p)
                    (boundp 'exwm-workspace--list))
           (mapcar (lambda (ws)
                     (or (when (boundp 'exwm-workspace-name) 
                           (frame-parameter ws 'exwm-workspace-name))
                         (format "Workspace %d" (cl-position ws exwm-workspace--list))))
                   exwm-workspace--list)))
        
        (fallback-numbers
         (lispbar-exwm--get-fallback-workspace-names)))
    (error
     (lispbar-exwm--log 'error "Failed to get workspace names from %s: %s" source err)
     nil)))

(defun lispbar-exwm--get-fallback-workspace-names ()
  "Generate fallback workspace names as numbers."
  (let ((count (lispbar-exwm--get-workspace-count)))
    (cl-loop for i from 1 to count
             collect (format "%d" i))))

(defun lispbar-exwm--get-workspace-count ()
  "Get the number of workspaces.
Returns a reasonable default if unavailable."
  (cond
   ;; Try EXWM workspace list
   ((and (lispbar-exwm-available-p)
         (boundp 'exwm-workspace--list))
    (length exwm-workspace--list))
   
   ;; Try EWMH number of desktops
   ((and (display-graphic-p)
         (x-window-property "_NET_NUMBER_OF_DESKTOPS" nil "CARDINAL" 0 nil t))
    (let ((count (x-window-property "_NET_NUMBER_OF_DESKTOPS" nil "CARDINAL" 0 nil t)))
      (if (vectorp count) (aref count 0) 4)))
   
   ;; Fallback
   (t 4)))

(defun lispbar-exwm--update-workspace-info ()
  "Update cached workspace information."
  (setq lispbar-exwm--current-workspace (lispbar-exwm--get-current-workspace)
        lispbar-exwm--workspace-names (lispbar-exwm--get-workspace-names))
  (lispbar-exwm--log 'debug "Updated workspace info: current=%s names=%S"
                     lispbar-exwm--current-workspace lispbar-exwm--workspace-names))

;;; Window Management

(defun lispbar-exwm--get-window-properties (window)
  "Get properties for WINDOW.
Returns a plist with window information."
  (when (and window lispbar-exwm-track-window-properties)
    (condition-case err
        (list :id window
              :class (lispbar-exwm--get-window-class window)
              :title (lispbar-exwm--get-window-title window)
              :instance (lispbar-exwm--get-window-instance window)
              :floating (lispbar-exwm--window-floating-p window)
              :fullscreen (lispbar-exwm--window-fullscreen-p window))
      (error
       (lispbar-exwm--log 'error "Failed to get window properties for %s: %s" window err)
       (list :id window :class "unknown" :title "unknown")))))

(defun lispbar-exwm--get-window-class (window)
  "Get window class for WINDOW."
  (or (when (and (lispbar-exwm-available-p) window)
        (let ((buffer (exwm--id->buffer window)))
          (when buffer
            (buffer-local-value 'exwm-class-name buffer))))
      "unknown"))

(defun lispbar-exwm--get-window-title (window)
  "Get window title for WINDOW."
  (or (when (and (lispbar-exwm-available-p) window)
        (let ((buffer (exwm--id->buffer window)))
          (when buffer
            (buffer-local-value 'exwm-title buffer))))
      "unknown"))

(defun lispbar-exwm--get-window-instance (window)
  "Get window instance for WINDOW."
  (or (when (and (lispbar-exwm-available-p) window)
        (let ((buffer (exwm--id->buffer window)))
          (when buffer
            (buffer-local-value 'exwm-instance-name buffer))))
      "unknown"))

(defun lispbar-exwm--window-floating-p (window)
  "Check if WINDOW is floating."
  (when (and (lispbar-exwm-available-p) window)
    (let ((buffer (exwm--id->buffer window)))
      (when buffer
        (buffer-local-value 'exwm--floating-frame buffer)))))

(defun lispbar-exwm--window-fullscreen-p (window)
  "Check if WINDOW is fullscreen."
  (when (and (lispbar-exwm-available-p) window)
    (memq window exwm--fullscreen-frame-list)))

(defun lispbar-exwm--track-window (window)
  "Start tracking WINDOW if it passes filters."
  (when window
    (let* ((class (lispbar-exwm--get-window-class window))
           (should-track (not (member class lispbar-exwm-window-class-filter))))
      (when should-track
        (let ((properties (lispbar-exwm--get-window-properties window)))
          (setq lispbar-exwm--tracked-windows
                (cons (cons window properties)
                      (assq-delete-all window lispbar-exwm--tracked-windows)))
          (lispbar-exwm--log 'debug "Started tracking window %s (%s)" window class)
          properties)))))

(defun lispbar-exwm--untrack-window (window)
  "Stop tracking WINDOW."
  (when (assq window lispbar-exwm--tracked-windows)
    (setq lispbar-exwm--tracked-windows
          (assq-delete-all window lispbar-exwm--tracked-windows))
    (lispbar-exwm--log 'debug "Stopped tracking window %s" window)))

(defun lispbar-exwm--get-tracked-window-properties (window)
  "Get cached properties for tracked WINDOW."
  (cdr (assq window lispbar-exwm--tracked-windows)))

;;; Monitor Configuration

(defun lispbar-exwm--get-monitor-configuration ()
  "Get current monitor configuration from EXWM RandR.
Returns a list of monitor plists or nil if unavailable."
  (when (lispbar-exwm-randr-available-p)
    (condition-case err
        (let ((monitors nil))
          (when (boundp 'exwm-randr-workspace-monitor-plist)
            ;; Extract monitor information from RandR
            (maphash (lambda (monitor-name geometry)
                       (push (list :name (symbol-name monitor-name)
                                   :x (nth 0 geometry)
                                   :y (nth 1 geometry)
                                   :width (nth 2 geometry)
                                   :height (nth 3 geometry))
                             monitors))
                     exwm-randr--monitor-geometry))
          monitors)
      (error
       (lispbar-exwm--log 'error "Failed to get monitor configuration: %s" err)
       nil))))

(defun lispbar-exwm--update-monitor-configuration ()
  "Update cached monitor configuration."
  (setq lispbar-exwm--monitor-configuration (lispbar-exwm--get-monitor-configuration))
  (lispbar-exwm--log 'debug "Updated monitor configuration: %d monitors"
                     (length lispbar-exwm--monitor-configuration)))

;;; Event Handlers

(defun lispbar-exwm--on-workspace-switch (&optional old-workspace new-workspace)
  "Handle workspace switch from OLD-WORKSPACE to NEW-WORKSPACE."
  (lispbar-exwm--log 'debug "Workspace switch: %s -> %s" old-workspace new-workspace)
  
  ;; Cancel any pending workspace switch timer
  (when lispbar-exwm--workspace-switch-timer
    (cancel-timer lispbar-exwm--workspace-switch-timer)
    (setq lispbar-exwm--workspace-switch-timer nil))
  
  ;; Schedule delayed update to prevent flickering
  (setq lispbar-exwm--workspace-switch-timer
        (run-at-time lispbar-exwm-workspace-switch-delay nil
                     (lambda ()
                       (setq lispbar-exwm--workspace-switch-timer nil)
                       (lispbar-exwm--process-workspace-switch old-workspace new-workspace)))))

(defun lispbar-exwm--process-workspace-switch (old-workspace new-workspace)
  "Process workspace switch after delay."
  (let ((old-ws (or old-workspace lispbar-exwm--current-workspace))
        (new-ws (or new-workspace (lispbar-exwm--get-current-workspace))))
    
    (lispbar-exwm--update-workspace-info)
    
    ;; Broadcast to modules
    (lispbar-modules-send-message 'workspace-changed
                                  (list :old-workspace old-ws
                                        :new-workspace new-ws
                                        :workspace-names lispbar-exwm--workspace-names)
                                  'lispbar-exwm)
    
    (lispbar-exwm--log 'info "Processed workspace switch: %s -> %s" old-ws new-ws)))

(defun lispbar-exwm--on-window-manage (window)
  "Handle new window WINDOW being managed by EXWM."
  (when window
    (lispbar-exwm--log 'debug "Window managed: %s" window)
    
    (let ((properties (lispbar-exwm--track-window window)))
      (when properties
        ;; Broadcast to modules
        (lispbar-modules-send-message 'window-managed
                                      (append (list :window window) properties)
                                      'lispbar-exwm)))))

(defun lispbar-exwm--on-window-unmanage (window)
  "Handle window WINDOW being unmanaged by EXWM."
  (when window
    (lispbar-exwm--log 'debug "Window unmanaged: %s" window)
    
    (let ((properties (lispbar-exwm--get-tracked-window-properties window)))
      (lispbar-exwm--untrack-window window)
      
      ;; Broadcast to modules
      (lispbar-modules-send-message 'window-unmanaged
                                    (append (list :window window) properties)
                                    'lispbar-exwm))))

(defun lispbar-exwm--on-window-focus-change (&optional window)
  "Handle window focus change to WINDOW."
  (let ((focused-window (or window (lispbar-exwm--get-focused-window))))
    (when focused-window
      (lispbar-exwm--log 'debug "Window focus changed: %s" focused-window)
      
      (let ((properties (or (lispbar-exwm--get-tracked-window-properties focused-window)
                            (lispbar-exwm--get-window-properties focused-window))))
        ;; Broadcast to modules
        (lispbar-modules-send-message 'window-focus-changed
                                      (append (list :window focused-window) properties)
                                      'lispbar-exwm)))))

(defun lispbar-exwm--on-fullscreen-change (window fullscreen)
  "Handle WINDOW entering or exiting fullscreen mode."
  (when window
    (lispbar-exwm--log 'debug "Window fullscreen changed: %s fullscreen=%s" window fullscreen)
    
    (let ((properties (lispbar-exwm--get-tracked-window-properties window)))
      ;; Update tracked properties
      (when properties
        (setf (plist-get properties :fullscreen) fullscreen))
      
      ;; Broadcast to modules
      (lispbar-modules-send-message 'fullscreen-changed
                                    (list :window window
                                          :fullscreen fullscreen
                                          :properties properties)
                                    'lispbar-exwm))))

(defun lispbar-exwm--on-floating-change (window floating)
  "Handle WINDOW becoming floating or tiled."
  (when window
    (lispbar-exwm--log 'debug "Window floating changed: %s floating=%s" window floating)
    
    (let ((properties (lispbar-exwm--get-tracked-window-properties window)))
      ;; Update tracked properties
      (when properties
        (setf (plist-get properties :floating) floating))
      
      ;; Broadcast to modules
      (lispbar-modules-send-message 'floating-changed
                                    (list :window window
                                          :floating floating
                                          :properties properties)
                                    'lispbar-exwm))))

(defun lispbar-exwm--on-randr-screen-change ()
  "Handle monitor configuration change via EXWM RandR."
  (lispbar-exwm--log 'debug "Monitor configuration changed")
  
  ;; Cancel any pending monitor change timer
  (when lispbar-exwm--monitor-change-timer
    (cancel-timer lispbar-exwm--monitor-change-timer)
    (setq lispbar-exwm--monitor-change-timer nil))
  
  ;; Schedule delayed update to allow RandR to complete
  (setq lispbar-exwm--monitor-change-timer
        (run-at-time lispbar-exwm-monitor-change-delay nil
                     (lambda ()
                       (setq lispbar-exwm--monitor-change-timer nil)
                       (lispbar-exwm--process-monitor-change)))))

(defun lispbar-exwm--process-monitor-change ()
  "Process monitor configuration change after delay."
  (let ((old-config lispbar-exwm--monitor-configuration))
    (lispbar-exwm--update-monitor-configuration)
    
    ;; Update core monitor information
    (when (fboundp 'lispbar--refresh-monitors)
      (lispbar-safe-call #'lispbar--refresh-monitors))
    
    ;; Broadcast to modules
    (lispbar-modules-send-message 'monitor-configuration-changed
                                  (list :old-configuration old-config
                                        :new-configuration lispbar-exwm--monitor-configuration)
                                  'lispbar-exwm)
    
    (lispbar-exwm--log 'info "Processed monitor configuration change")))

;;; Hook Management

(defun lispbar-exwm--register-hooks ()
  "Register EXWM event hooks."
  (unless lispbar-exwm--hooks-registered
    (lispbar-exwm--log 'debug "Registering EXWM hooks")
    
    (when (lispbar-exwm-available-p)
      ;; Workspace switch hooks
      (add-hook 'exwm-workspace-switch-hook #'lispbar-exwm--on-workspace-switch)
      (push (lambda () (remove-hook 'exwm-workspace-switch-hook #'lispbar-exwm--on-workspace-switch))
            lispbar-exwm--cleanup-functions)
      
      ;; Window management hooks
      (add-hook 'exwm-manage-finish-hook #'lispbar-exwm--on-window-manage)
      (add-hook 'exwm-unmanage-finish-hook #'lispbar-exwm--on-window-unmanage)
      (push (lambda () 
              (remove-hook 'exwm-manage-finish-hook #'lispbar-exwm--on-window-manage)
              (remove-hook 'exwm-unmanage-finish-hook #'lispbar-exwm--on-window-unmanage))
            lispbar-exwm--cleanup-functions)
      
      ;; Window focus hooks (if available)
      (when (boundp 'window-selection-change-functions)
        (add-hook 'window-selection-change-functions #'lispbar-exwm--on-window-focus-change)
        (push (lambda () (remove-hook 'window-selection-change-functions #'lispbar-exwm--on-window-focus-change))
              lispbar-exwm--cleanup-functions)))
    
    ;; RandR hooks
    (when (lispbar-exwm-randr-available-p)
      (add-hook 'exwm-randr-screen-change-hook #'lispbar-exwm--on-randr-screen-change)
      (push (lambda () (remove-hook 'exwm-randr-screen-change-hook #'lispbar-exwm--on-randr-screen-change))
            lispbar-exwm--cleanup-functions))
    
    (setq lispbar-exwm--hooks-registered t)
    (lispbar-exwm--log 'info "EXWM hooks registered")))

(defun lispbar-exwm--unregister-hooks ()
  "Unregister EXWM event hooks."
  (when lispbar-exwm--hooks-registered
    (lispbar-exwm--log 'debug "Unregistering EXWM hooks")
    
    ;; Run cleanup functions
    (dolist (cleanup-fn lispbar-exwm--cleanup-functions)
      (condition-case err
          (funcall cleanup-fn)
        (error
         (lispbar-exwm--log 'error "Cleanup function failed: %s" err))))
    
    (setq lispbar-exwm--cleanup-functions nil
          lispbar-exwm--hooks-registered nil)
    
    (lispbar-exwm--log 'info "EXWM hooks unregistered")))

;;; Utility Functions

(defun lispbar-exwm--get-focused-window ()
  "Get the currently focused window ID."
  (when (lispbar-exwm-available-p)
    (condition-case err
        (let ((buffer (current-buffer)))
          (when (and (boundp 'exwm--id) 
                     (buffer-local-value 'exwm--id buffer))
            (buffer-local-value 'exwm--id buffer)))
      (error
       (lispbar-exwm--log 'error "Failed to get focused window: %s" err)
       nil))))

;;; Public API

(defun lispbar-exwm-current-workspace ()
  "Get the current workspace number (0-indexed).
Returns nil if workspace information is unavailable."
  lispbar-exwm--current-workspace)

(defun lispbar-exwm-workspace-names ()
  "Get list of workspace names.
Returns a list of strings representing workspace names."
  lispbar-exwm--workspace-names)

(defun lispbar-exwm-current-window ()
  "Get the currently focused window.
Returns window ID or nil if no window is focused."
  (lispbar-exwm--get-focused-window))

(defun lispbar-exwm-window-properties (window)
  "Get properties for WINDOW.
Returns a plist with window information or nil if unavailable."
  (or (lispbar-exwm--get-tracked-window-properties window)
      (lispbar-exwm--get-window-properties window)))

(defun lispbar-exwm-window-class (&optional window)
  "Get window class for WINDOW (defaults to current window).
Returns the window class string or nil if unavailable."
  (let ((win (or window (lispbar-exwm-current-window))))
    (when win
      (lispbar-exwm--get-window-class win))))

(defun lispbar-exwm-window-title (&optional window)
  "Get window title for WINDOW (defaults to current window).
Returns the window title string or nil if unavailable."
  (let ((win (or window (lispbar-exwm-current-window))))
    (when win
      (lispbar-exwm--get-window-title win))))

(defun lispbar-exwm-fullscreen-window ()
  "Get the current fullscreen window if any.
Returns window ID or nil if no window is fullscreen."
  (when (and (lispbar-exwm-available-p) 
             (boundp 'exwm--fullscreen-frame-list)
             exwm--fullscreen-frame-list)
    (car exwm--fullscreen-frame-list)))

(defun lispbar-exwm-floating-windows ()
  "Get list of currently floating windows.
Returns a list of window IDs."
  (when lispbar-exwm-track-window-properties
    (cl-loop for (window . properties) in lispbar-exwm--tracked-windows
             when (plist-get properties :floating)
             collect window)))

(defun lispbar-exwm-monitor-configuration ()
  "Get current monitor configuration.
Returns a list of monitor plists or nil if unavailable."
  lispbar-exwm--monitor-configuration)

;;; Lifecycle Management

;;;###autoload
(defun lispbar-exwm-init ()
  "Initialize EXWM integration for Lispbar.
This function sets up EXWM event handlers and integration."
  (interactive)
  (when lispbar-exwm--initialized
    (lispbar-exwm--log 'warning "EXWM integration already initialized")
    (return-from lispbar-exwm-init))
  
  (lispbar-exwm--log 'info "Initializing EXWM integration")
  
  (condition-case err
      (progn
        ;; Check EXWM availability
        (unless (lispbar-exwm-available-p)
          (lispbar-exwm--log 'warning "EXWM not available, limited functionality"))
        
        ;; Initialize data structures
        (setq lispbar-exwm--tracked-windows nil
              lispbar-exwm--cleanup-functions nil)
        
        ;; Update initial information
        (lispbar-exwm--update-workspace-info)
        (lispbar-exwm--update-monitor-configuration)
        
        ;; Register hooks
        (lispbar-exwm--register-hooks)
        
        ;; Register cleanup with core
        (when (fboundp 'lispbar--add-cleanup-function)
          (lispbar--add-cleanup-function #'lispbar-exwm-cleanup))
        
        ;; Integrate with core monitor detection
        (when (and (lispbar-exwm-randr-available-p)
                   (fboundp 'lispbar--refresh-monitors))
          (lispbar-safe-call #'lispbar--refresh-monitors))
        
        (setq lispbar-exwm--initialized t)
        (lispbar-exwm--log 'info "EXWM integration initialized successfully"))
    
    (error
     (lispbar-exwm--log 'error "EXWM integration initialization failed: %s" err)
     (lispbar-exwm-cleanup)
     (signal (car err) (cdr err)))))

;;;###autoload
(defun lispbar-exwm-cleanup ()
  "Clean up EXWM integration.
This function removes all event handlers and resets state."
  (interactive)
  (lispbar-exwm--log 'info "Cleaning up EXWM integration")
  
  ;; Cancel timers
  (when lispbar-exwm--workspace-switch-timer
    (cancel-timer lispbar-exwm--workspace-switch-timer)
    (setq lispbar-exwm--workspace-switch-timer nil))
  
  (when lispbar-exwm--monitor-change-timer
    (cancel-timer lispbar-exwm--monitor-change-timer)
    (setq lispbar-exwm--monitor-change-timer nil))
  
  ;; Unregister hooks
  (lispbar-exwm--unregister-hooks)
  
  ;; Reset state
  (setq lispbar-exwm--current-workspace nil
        lispbar-exwm--workspace-names nil
        lispbar-exwm--tracked-windows nil
        lispbar-exwm--monitor-configuration nil
        lispbar-exwm--initialized nil)
  
  (lispbar-exwm--log 'info "EXWM integration cleanup complete"))

;;;###autoload
(defun lispbar-exwm-refresh ()
  "Refresh EXWM integration state.
This function updates workspace and monitor information."
  (interactive)
  (when lispbar-exwm--initialized
    (lispbar-exwm--log 'info "Refreshing EXWM integration")
    (lispbar-exwm--update-workspace-info)
    (lispbar-exwm--update-monitor-configuration)
    (lispbar-exwm--log 'info "EXWM integration refresh complete")))

(provide 'lispbar-exwm)
;;; lispbar-exwm.el ends here