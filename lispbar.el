;;; lispbar.el --- A modular status bar for EXWM -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (exwm "0.24") (eieio "1.4") (cl-lib "0.5"))
;; Keywords: frames, exwm, status-bar, toolbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; Lispbar is a modular, extensible status bar designed specifically for EXWM
;; (Emacs X Window Manager). It provides a comprehensive system for displaying
;; system information, workspace details, and custom modules in an elegant
;; toolbar that integrates seamlessly with your EXWM setup.
;;
;; Key Features:
;; - Modular architecture with EIEIO-based module system
;; - Multi-monitor support with proper positioning
;; - Comprehensive EXWM integration with workspace tracking
;; - Left/center/right positioning with priority-based ordering
;; - Efficient rendering engine with change detection and caching
;; - Rich communication system for inter-module coordination
;; - Graceful degradation when EXWM features are unavailable
;; - Extensive customization options and theming support
;;
;; Quick Start:
;;   (require 'lispbar)
;;   (lispbar-mode 1)
;;
;; The main entry point is `lispbar-mode', a global minor mode that
;; initializes all subsystems in the correct order and provides
;; easy enable/disable functionality.
;;
;; Configuration:
;; Lispbar can be extensively customized through the `lispbar' customization
;; group. Key configuration options include position (top/bottom), height,
;; colors, margins, and module settings.
;;
;; Module System:
;; Lispbar uses an EIEIO-based module system that allows easy creation of
;; custom modules. Modules can be positioned left, center, or right, have
;; configurable update intervals, and communicate with each other through
;; a message-passing system.
;;
;; Architecture:
;; Lispbar consists of four main subsystems:
;; - lispbar-core: Frame management and monitor detection
;; - lispbar-render: Rendering engine with layout calculation
;; - lispbar-modules: Module system with lifecycle management
;; - lispbar-exwm: EXWM integration and event handling
;;
;; Each subsystem can be used independently, but the main lispbar.el file
;; provides the coordinated initialization and public API.

;;; Code:

(require 'cl-lib)
(require 'lispbar-core)
(require 'lispbar-render)
(require 'lispbar-modules)
(require 'lispbar-exwm)

;;; Customization

(defgroup lispbar nil
  "Modular status bar for EXWM."
  :group 'exwm
  :prefix "lispbar-"
  :link '(url-link :tag "GitHub" "https://github.com/yourusername/lispbar"))

(defcustom lispbar-auto-start-modules t
  "Whether to automatically start default modules when enabling lispbar-mode.
When non-nil, a basic set of modules will be created and enabled automatically."
  :type 'boolean
  :group 'lispbar)

(defcustom lispbar-default-modules '(time workspace window-title)
  "List of default modules to enable when `lispbar-auto-start-modules' is t.
Each element should be a symbol representing a module type."
  :type '(repeat (choice (const :tag "Current time" time)
                         (const :tag "Workspace indicator" workspace)
                         (const :tag "Window title" window-title)
                         (const :tag "Battery status" battery)
                         (const :tag "System load" system-load)
                         (symbol :tag "Custom module")))
  :group 'lispbar)

(defcustom lispbar-startup-hook nil
  "Hook run after Lispbar has been fully initialized.
This hook is called after all subsystems are initialized and
any default modules have been created."
  :type 'hook
  :group 'lispbar)

(defcustom lispbar-shutdown-hook nil
  "Hook run before Lispbar is shut down.
This hook is called before any cleanup functions are executed."
  :type 'hook
  :group 'lispbar)

(defcustom lispbar-before-module-create-hook nil
  "Hook run before creating each default module.
The hook functions are called with the module type as argument."
  :type 'hook
  :group 'lispbar)

(defcustom lispbar-after-module-create-hook nil
  "Hook run after creating each default module.
The hook functions are called with the module instance as argument."
  :type 'hook
  :group 'lispbar)

(defcustom lispbar-config-validation t
  "Whether to validate configuration before initialization.
When non-nil, configuration issues will be reported before starting."
  :type 'boolean
  :group 'lispbar)

(defcustom lispbar-error-on-invalid-config nil
  "Whether to signal an error when configuration validation fails.
When nil, warnings are logged but initialization continues.
When non-nil, initialization is aborted on configuration errors."
  :type 'boolean
  :group 'lispbar)

;;; Variables

(defvar lispbar--mode-enabled nil
  "Whether lispbar-mode is currently enabled.")

(defvar lispbar--initialization-in-progress nil
  "Whether Lispbar initialization is currently in progress.")

(defvar lispbar--shutdown-in-progress nil
  "Whether Lispbar shutdown is currently in progress.")

(defvar lispbar--created-modules nil
  "List of modules created by Lispbar during auto-start.
These modules will be cleaned up when lispbar-mode is disabled.")

(defvar lispbar--initialization-errors nil
  "List of errors encountered during initialization.")

;;; Default Module Definitions

(defun lispbar--create-time-module ()
  "Create a time display module."
  (make-instance 'lispbar-module
                 :name 'time
                 :update-fn (lambda ()
                              (format-time-string "%H:%M:%S"))
                 :update-interval 1.0
                 :position 'right
                 :priority 90))

(defun lispbar--create-workspace-module ()
  "Create a workspace indicator module."
  (make-instance 'lispbar-module
                 :name 'workspace
                 :update-fn (lambda ()
                              (if (featurep 'lispbar-exwm)
                                  (let ((current (lispbar-exwm-current-workspace))
                                        (names (lispbar-exwm-workspace-names)))
                                    (if (and current names)
                                        (format "[%s]" (nth current names))
                                      (format "[%s]" (or current "?"))))
                                "[?]"))
                 :hooks '(exwm-workspace-switch-hook)
                 :communicates '(workspace-changed)
                 :position 'left
                 :priority 80))

(defun lispbar--create-window-title-module ()
  "Create a window title display module."
  (make-instance 'lispbar-module
                 :name 'window-title
                 :update-fn (lambda ()
                              (if (featurep 'lispbar-exwm)
                                  (or (lispbar-exwm-window-title)
                                      (buffer-name))
                                (buffer-name)))
                 :communicates '(window-focus-changed window-managed)
                 :position 'center
                 :priority 50
                 :cache-timeout 5.0))

(defun lispbar--create-battery-module ()
  "Create a battery status module."
  (make-instance 'lispbar-module
                 :name 'battery
                 :update-fn (lambda ()
                              (when (fboundp 'battery-format)
                                (let ((status (funcall battery-status-function)))
                                  (when status
                                    (battery-format "%p%% %L" status)))))
                 :update-interval 60.0
                 :position 'right
                 :priority 70))

(defun lispbar--create-system-load-module ()
  "Create a system load module."
  (make-instance 'lispbar-module
                 :name 'system-load
                 :update-fn (lambda ()
                              (when (file-exists-p "/proc/loadavg")
                                (with-temp-buffer
                                  (insert-file-contents "/proc/loadavg")
                                  (let ((load (car (split-string (buffer-string)))))
                                    (format "Load: %s" load)))))
                 :update-interval 5.0
                 :position 'right
                 :priority 60))

;;; Module Factory

(defun lispbar--create-default-module (module-type)
  "Create a default module of MODULE-TYPE.
Returns the created module instance or nil if type is unknown."
  (run-hook-with-args 'lispbar-before-module-create-hook module-type)
  
  (let ((module
         (cl-case module-type
           (time (lispbar--create-time-module))
           (workspace (lispbar--create-workspace-module))
           (window-title (lispbar--create-window-title-module))
           (battery (lispbar--create-battery-module))
           (system-load (lispbar--create-system-load-module))
           (t (progn
                (lispbar-log 'warning "Unknown default module type: %s" module-type)
                nil)))))
    
    (when module
      (run-hook-with-args 'lispbar-after-module-create-hook module)
      (push module lispbar--created-modules)
      (lispbar-log 'info "Created default module: %s" module-type))
    
    module))

;;; Configuration Validation

(defun lispbar--validate-configuration ()
  "Validate Lispbar configuration.
Returns a list of configuration issues or nil if valid."
  (let ((issues nil))
    
    ;; Validate core configuration
    (let ((core-issues (lispbar-validate-config)))
      (when core-issues
        (setq issues (append issues (mapcar (lambda (issue)
                                              (format "Core: %s" issue))
                                            core-issues)))))
    
    ;; Validate default modules list
    (unless (listp lispbar-default-modules)
      (push "lispbar-default-modules must be a list" issues))
    
    ;; Validate module types
    (dolist (module-type lispbar-default-modules)
      (unless (symbolp module-type)
        (push (format "Invalid module type (not a symbol): %S" module-type) issues)))
    
    ;; Validate hooks
    (dolist (hook '(lispbar-startup-hook lispbar-shutdown-hook
                    lispbar-before-module-create-hook lispbar-after-module-create-hook))
      (let ((value (symbol-value hook)))
        (unless (or (null value) (functionp value) (listp value))
          (push (format "%s must be nil, a function, or a list of functions" hook) issues))))
    
    issues))

(defun lispbar--handle-configuration-issues (issues)
  "Handle configuration ISSUES according to user preferences."
  (when issues
    (let ((message (format "Lispbar configuration issues found:\n%s"
                           (mapconcat #'identity issues "\n- "))))
      (if lispbar-error-on-invalid-config
          (error "Lispbar configuration validation failed:\n%s" message)
        (lispbar-log 'warning "Configuration issues (continuing anyway):\n%s" message)))))

;;; Initialization and Cleanup

(defun lispbar--initialize-subsystems ()
  "Initialize all Lispbar subsystems in the correct order.
Returns t on success, nil on failure."
  (lispbar-log 'info "Initializing Lispbar subsystems")
  (setq lispbar--initialization-errors nil)
  
  (condition-case err
      (progn
        ;; Initialize core system (frame management)
        (lispbar-log 'debug "Initializing core system")
        (unless (lispbar-safe-call #'lispbar-init)
          (error "Core initialization failed"))
        
        ;; Initialize rendering system
        (lispbar-log 'debug "Initializing render system")
        (unless (lispbar-safe-call #'lispbar-render-init)
          (error "Render system initialization failed"))
        
        ;; Initialize module system
        (lispbar-log 'debug "Initializing module system")
        (unless (lispbar-safe-call #'lispbar-modules-init)
          (error "Module system initialization failed"))
        
        ;; Initialize EXWM integration (optional)
        (when (featurep 'exwm)
          (lispbar-log 'debug "Initializing EXWM integration")
          (condition-case exwm-err
              (lispbar-exwm-init)
            (error
             (lispbar-log 'warning "EXWM integration failed: %s" exwm-err)
             (push exwm-err lispbar--initialization-errors))))
        
        (lispbar-log 'info "Subsystem initialization complete")
        t)
    
    (error
     (lispbar-log 'error "Subsystem initialization failed: %s" err)
     (push err lispbar--initialization-errors)
     nil)))

(defun lispbar--create-default-modules ()
  "Create and register default modules if auto-start is enabled."
  (when lispbar-auto-start-modules
    (lispbar-log 'info "Creating default modules: %S" lispbar-default-modules)
    
    (dolist (module-type lispbar-default-modules)
      (condition-case err
          (let ((module (lispbar--create-default-module module-type)))
            (when module
              (lispbar-modules-register module)
              (lispbar-log 'debug "Registered default module: %s" module-type)))
        (error
         (lispbar-log 'error "Failed to create module %s: %s" module-type err)
         (push err lispbar--initialization-errors))))
    
    (lispbar-log 'info "Default module creation complete")))

(defun lispbar--cleanup-created-modules ()
  "Clean up modules created during auto-start."
  (when lispbar--created-modules
    (lispbar-log 'info "Cleaning up %d created modules" (length lispbar--created-modules))
    
    (dolist (module lispbar--created-modules)
      (condition-case err
          (when (and module (slot-boundp module 'name))
            (let ((name (oref module name)))
              (lispbar-modules-unregister name)
              (lispbar-log 'debug "Unregistered module: %s" name)))
        (error
         (lispbar-log 'error "Failed to unregister module: %s" err))))
    
    (setq lispbar--created-modules nil)
    (lispbar-log 'info "Module cleanup complete")))

(defun lispbar--cleanup-subsystems ()
  "Clean up all Lispbar subsystems in reverse order."
  (lispbar-log 'info "Cleaning up Lispbar subsystems")
  
  ;; Clean up EXWM integration first
  (when (fboundp 'lispbar-exwm-cleanup)
    (lispbar-log 'debug "Cleaning up EXWM integration")
    (lispbar-safe-call #'lispbar-exwm-cleanup))
  
  ;; Clean up module system
  (when (fboundp 'lispbar-modules-cleanup)
    (lispbar-log 'debug "Cleaning up module system")
    (lispbar-safe-call #'lispbar-modules-cleanup))
  
  ;; Clean up rendering system
  (when (fboundp 'lispbar-render-cleanup)
    (lispbar-log 'debug "Cleaning up render system")
    (lispbar-safe-call #'lispbar-render-cleanup))
  
  ;; Clean up core system last
  (when (fboundp 'lispbar-cleanup)
    (lispbar-log 'debug "Cleaning up core system")
    (lispbar-safe-call #'lispbar-cleanup))
  
  (lispbar-log 'info "Subsystem cleanup complete"))

;;; Mode Definition

;;;###autoload
(define-minor-mode lispbar-mode
  "Toggle Lispbar status bar.

Lispbar is a modular status bar designed for EXWM that provides
system information, workspace details, and custom modules in an
elegant toolbar.

When enabled, Lispbar initializes all subsystems (core frame
management, rendering engine, module system, and EXWM integration)
and optionally creates default modules based on configuration.

When disabled, all frames are cleaned up, modules are unregistered,
and resources are freed.

Configuration is managed through the `lispbar' customization group.
Key options include toolbar position, height, colors, and default
modules to enable.

\\{lispbar-mode-map}"
  :global t
  :group 'lispbar
  :lighter " Lispbar"
  :keymap (make-sparse-keymap)
  
  (cond
   ;; Enabling lispbar-mode
   (lispbar-mode
    (if lispbar--mode-enabled
        (lispbar-log 'warning "Lispbar mode already enabled")
      
      (lispbar-log 'info "Enabling Lispbar mode")
      (setq lispbar--initialization-in-progress t)
      
      (condition-case err
          (progn
            ;; Validate configuration if requested
            (when lispbar-config-validation
              (let ((issues (lispbar--validate-configuration)))
                (lispbar--handle-configuration-issues issues)))
            
            ;; Initialize subsystems
            (unless (lispbar--initialize-subsystems)
              (error "Subsystem initialization failed"))
            
            ;; Create default modules
            (lispbar--create-default-modules)
            
            ;; Mark as enabled
            (setq lispbar--mode-enabled t
                  lispbar--initialization-in-progress nil)
            
            ;; Run startup hook
            (run-hooks 'lispbar-startup-hook)
            
            (lispbar-log 'info "Lispbar mode enabled successfully")
            (when lispbar--initialization-errors
              (lispbar-log 'warning "Initialization completed with %d warnings"
                           (length lispbar--initialization-errors))))
        
        (error
         (lispbar-log 'error "Failed to enable Lispbar mode: %s" err)
         (setq lispbar--initialization-in-progress nil)
         
         ;; Attempt cleanup on failure
         (condition-case cleanup-err
             (progn
               (lispbar--cleanup-created-modules)
               (lispbar--cleanup-subsystems))
           (error
            (lispbar-log 'error "Cleanup after failure also failed: %s" cleanup-err)))
         
         ;; Disable the mode
         (setq lispbar-mode nil)
         (signal (car err) (cdr err))))))
   
   ;; Disabling lispbar-mode
   (t
    (if (not lispbar--mode-enabled)
        (lispbar-log 'warning "Lispbar mode already disabled")
      
      (lispbar-log 'info "Disabling Lispbar mode")
      (setq lispbar--shutdown-in-progress t)
      
      (condition-case err
          (progn
            ;; Run shutdown hook
            (run-hooks 'lispbar-shutdown-hook)
            
            ;; Clean up created modules
            (lispbar--cleanup-created-modules)
            
            ;; Clean up subsystems
            (lispbar--cleanup-subsystems)
            
            ;; Mark as disabled
            (setq lispbar--mode-enabled nil
                  lispbar--shutdown-in-progress nil
                  lispbar--initialization-errors nil)
            
            (lispbar-log 'info "Lispbar mode disabled successfully"))
        
        (error
         (lispbar-log 'error "Error during Lispbar shutdown: %s" err)
         (setq lispbar--shutdown-in-progress nil)
         ;; Force disable even on error
         (setq lispbar--mode-enabled nil)))))))

;;; Public API Functions

;;;###autoload
(defun lispbar-restart ()
  "Restart Lispbar by disabling and re-enabling lispbar-mode.
This is useful when configuration has changed or after system updates."
  (interactive)
  (lispbar-log 'info "Restarting Lispbar")
  
  (when lispbar-mode
    (lispbar-mode -1))
  
  ;; Small delay to ensure cleanup is complete
  (run-at-time 0.1 nil
               (lambda ()
                 (lispbar-mode 1)
                 (lispbar-log 'info "Lispbar restart complete"))))

;;;###autoload
(defun lispbar-refresh ()
  "Refresh all Lispbar components.
This updates monitor detection, refreshes all modules, and redraws frames."
  (interactive)
  (if (not lispbar--mode-enabled)
      (user-error "Lispbar is not enabled")
    
    (lispbar-log 'info "Refreshing Lispbar")
    
    ;; Refresh core monitor detection
    (when (fboundp 'lispbar-refresh)
      (lispbar-safe-call #'lispbar-refresh))
    
    ;; Refresh EXWM integration
    (when (fboundp 'lispbar-exwm-refresh)
      (lispbar-safe-call #'lispbar-exwm-refresh))
    
    ;; Update all modules
    (when (fboundp 'lispbar-modules-update-all)
      (lispbar-safe-call #'lispbar-modules-update-all))
    
    (lispbar-log 'info "Lispbar refresh complete")))

;;;###autoload
(defun lispbar-toggle-debug ()
  "Toggle debug logging for Lispbar.
When enabled, detailed debug information is logged to *Messages*."
  (interactive)
  (setq lispbar-debug (not lispbar-debug))
  (message "Lispbar debug logging %s" (if lispbar-debug "enabled" "disabled"))
  (lispbar-log 'info "Debug logging toggled: %s" lispbar-debug))

;;;###autoload
(defun lispbar-show-status ()
  "Show current Lispbar status and configuration.
Displays information about enabled subsystems, modules, and configuration."
  (interactive)
  (if (not lispbar--mode-enabled)
      (message "Lispbar is disabled")
    
    (let ((frames (when (fboundp 'lispbar-get-frames)
                    (lispbar-get-frames)))
          (modules (when (fboundp 'lispbar-modules-list)
                     (lispbar-modules-list)))
          (workspace (when (fboundp 'lispbar-exwm-current-workspace)
                       (lispbar-exwm-current-workspace))))
      
      (message "Lispbar Status:\n  Frames: %d\n  Modules: %d (%S)\n  Workspace: %s\n  Debug: %s"
               (length frames)
               (length modules)
               modules
               (or workspace "N/A")
               (if lispbar-debug "enabled" "disabled")))))

;;;###autoload
(defun lispbar-create-module (name update-fn &rest args)
  "Create and register a custom module.

NAME should be a unique symbol identifying the module.
UPDATE-FN should be a function that returns content to display.
ARGS are additional arguments passed to the module constructor.

Supported ARGS (as keyword arguments):
  :position - 'left, 'center, or 'right (default: 'right)
  :priority - Number 0-100, higher is more important (default: 50)
  :update-interval - Seconds between updates, nil for event-only (default: nil)
  :hooks - List of hooks that trigger updates (default: nil)
  :cache-timeout - Seconds to cache content (default: nil for global default)
  :dependencies - List of module names this module depends on (default: nil)
  :communicates - List of communication channels (default: nil)

Returns the created module instance.

Example:
  (lispbar-create-module 'my-module
                         (lambda () (format \"Time: %s\" (current-time-string)))
                         :position 'center
                         :update-interval 5.0
                         :priority 75)"
  (unless lispbar--mode-enabled
    (user-error "Lispbar must be enabled to create modules"))
  
  (unless (symbolp name)
    (error "Module name must be a symbol: %S" name))
  
  (unless (functionp update-fn)
    (error "Update function must be a function: %S" update-fn))
  
  (let ((module (apply #'make-instance 'lispbar-module
                       :name name
                       :update-fn update-fn
                       args)))
    
    (lispbar-modules-register module)
    (lispbar-log 'info "Created custom module: %s" name)
    module))

;;; Convenience Functions for Common Tasks

;;;###autoload
(defun lispbar-add-text-module (name text &rest args)
  "Add a simple text module displaying TEXT.
NAME should be a unique symbol, TEXT should be a string.
ARGS are passed to `lispbar-create-module'."
  (apply #'lispbar-create-module name (lambda () text) args))

;;;###autoload
(defun lispbar-add-function-module (name function &rest args)
  "Add a module that calls FUNCTION to get display content.
NAME should be a unique symbol, FUNCTION should return a string.
ARGS are passed to `lispbar-create-module'."
  (apply #'lispbar-create-module name function args))

;;;###autoload
(defun lispbar-remove-module (name)
  "Remove module NAME from Lispbar.
Returns t if module was found and removed, nil otherwise."
  (if (not lispbar--mode-enabled)
      (user-error "Lispbar is not enabled")
    
    (if (fboundp 'lispbar-modules-get)
        (if (lispbar-modules-get name)
            (progn
              (lispbar-modules-unregister name)
              (lispbar-log 'info "Removed module: %s" name)
              t)
          (lispbar-log 'warning "Module not found: %s" name)
          nil)
      (error "Module system not available"))))

;;; Keymap Setup

(define-key lispbar-mode-map (kbd "C-c l r") #'lispbar-refresh)
(define-key lispbar-mode-map (kbd "C-c l R") #'lispbar-restart)
(define-key lispbar-mode-map (kbd "C-c l s") #'lispbar-show-status)
(define-key lispbar-mode-map (kbd "C-c l d") #'lispbar-toggle-debug)

;;; Integration Helpers

(defun lispbar--setup-integration ()
  "Set up integration with other Emacs packages and systems.
This function is called during initialization to ensure proper integration."
  ;; EXWM integration is handled by lispbar-exwm.el
  
  ;; Theme integration - update colors when theme changes
  (when (boundp 'after-load-theme-hook)
    (add-hook 'after-load-theme-hook
              (lambda ()
                (when lispbar--mode-enabled
                  (run-at-time 0.1 nil #'lispbar-refresh)))))
  
  ;; Window configuration change integration
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when lispbar--mode-enabled
                ;; Throttled refresh to avoid excessive updates
                (run-at-time 0.2 nil #'lispbar-refresh)))))

;; Set up integration when this file is loaded
(eval-after-load 'lispbar
  '(lispbar--setup-integration))

(provide 'lispbar)
;;; lispbar.el ends here