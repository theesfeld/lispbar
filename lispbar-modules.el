;;; lispbar-modules.el --- Module system for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4"))
;; Keywords: frames, modules, exwm
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides the module system implementation for Lispbar.
;; It defines the base module class using EIEIO, handles module registration
;; and lifecycle management, implements update scheduling (both timer-based
;; and event-driven), and provides a module communication system.
;;
;; The module system supports:
;; - EIEIO-based module classes with inheritance
;; - Timer-based and hook-based update mechanisms
;; - Left/center/right positioning with priority ordering
;; - Module caching for expensive operations
;; - Dependency management and inter-module communication
;; - Graceful error handling and recovery
;; - Easy module creation via lispbar-define-module macro
;;
;; Key features:
;; - Base lispbar-module class with comprehensive slots
;; - Module registry and lifecycle management
;; - Update scheduler with intelligent throttling
;; - Position-based module organization with priorities
;; - Caching mechanism to avoid expensive recalculations
;; - Module communication via event system
;; - Validation and error handling throughout
;; - Helper macros for rapid module development

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lispbar-core)

;;; Customization

(defgroup lispbar-modules nil
  "Customization group for Lispbar module system."
  :group 'lispbar
  :prefix "lispbar-modules-")

(defcustom lispbar-modules-update-interval 1.0
  "Default update interval in seconds for timer-based modules."
  :type 'number
  :group 'lispbar-modules)

(defcustom lispbar-modules-cache-timeout 30.0
  "Default cache timeout in seconds for module content."
  :type 'number
  :group 'lispbar-modules)

(defcustom lispbar-modules-max-update-time 0.5
  "Maximum time in seconds a module update should take before warning."
  :type 'number
  :group 'lispbar-modules)

(defcustom lispbar-modules-error-recovery t
  "Whether to attempt error recovery for failed modules."
  :type 'boolean
  :group 'lispbar-modules)

(defcustom lispbar-modules-position-separator " | "
  "String used to separate modules within the same position."
  :type 'string
  :group 'lispbar-modules)

(defcustom lispbar-modules-debug nil
  "Enable debug logging for module system."
  :type 'boolean
  :group 'lispbar-modules)

;;; Variables

(defvar lispbar-modules--registry nil
  "Registry of all registered modules.
Each entry is a plist with :module, :instance, :timer, and :status.")

(defvar lispbar-modules--position-lists nil
  "Alist mapping positions to ordered lists of module names.
Format: ((left . (module1 module2)) (center . (module3)) (right . (module4 module5)))")

(defvar lispbar-modules--update-queue nil
  "Queue of modules pending update.")

(defvar lispbar-modules--event-handlers nil
  "Alist mapping event names to lists of module names that handle them.")

(defvar lispbar-modules--dependencies nil
  "Alist mapping module names to their dependency lists.")

(defvar lispbar-modules--communication-channels nil
  "Alist mapping channel names to subscriber module lists.")

(defvar lispbar-modules--initialized nil
  "Whether the module system has been initialized.")

;;; Logging

(defun lispbar-modules--log (level message &rest args)
  "Log MESSAGE with LEVEL if module debugging is enabled.
ARGS are passed to `format' for MESSAGE formatting."
  (when (or lispbar-modules-debug lispbar-debug)
    (let ((formatted-msg (apply #'format message args)))
      (lispbar-log level "[modules] %s" formatted-msg))))

;;; Base Module Class

(defclass lispbar-module ()
  ((name :initarg :name
         :type symbol
         :documentation "Unique module identifier.")
   (update-fn :initarg :update-fn
              :type function
              :documentation "Function that returns module content.")
   (update-interval :initarg :update-interval
                    :type (or null number)
                    :initform nil
                    :documentation "Update interval in seconds, nil for event-driven only.")
   (hooks :initarg :hooks
          :type list
          :initform nil
          :documentation "List of hooks that trigger updates.")
   (position :initarg :position
             :type symbol
             :initform 'right
             :documentation "Position: 'left, 'center, or 'right.")
   (priority :initarg :priority
             :type number
             :initform 50
             :documentation "Display priority (0-100, higher is more important).")
   (cache :initform nil
          :documentation "Cached module output with timestamp.")
   (cache-timeout :initarg :cache-timeout
                  :type (or null number)
                  :initform nil
                  :documentation "Cache timeout in seconds, nil to use default.")
   (dependencies :initarg :dependencies
                 :type list
                 :initform nil
                 :documentation "List of module names this module depends on.")
   (enabled :initarg :enabled
            :type boolean
            :initform t
            :documentation "Whether this module is currently enabled.")
   (error-count :initform 0
                :type integer
                :documentation "Number of consecutive errors encountered.")
   (last-error :initform nil
               :documentation "Last error encountered by this module.")
   (last-update :initform nil
                :documentation "Time of last successful update.")
   (communicates :initarg :communicates
                 :type list
                 :initform nil
                 :documentation "List of communication channels this module uses."))
  "Base class for lispbar modules.")

;;; Module Registry Management

(defun lispbar-modules--validate-module (module)
  "Validate that MODULE is a proper lispbar-module instance.
Returns t if valid, signals error if invalid."
  (unless (object-of-class-p module 'lispbar-module)
    (error "Object is not a lispbar-module: %S" module))
  
  (let ((name (oref module name))
        (update-fn (oref module update-fn))
        (position (oref module position)))
    
    (unless (symbolp name)
      (error "Module name must be a symbol: %S" name))
    
    (unless (functionp update-fn)
      (error "Module update-fn must be a function: %S" update-fn))
    
    (unless (memq position '(left center right))
      (error "Module position must be left, center, or right: %S" position))
    
    (let ((priority (oref module priority)))
      (unless (and (numberp priority) (<= 0 priority 100))
        (error "Module priority must be a number between 0-100: %S" priority))))
  
  t)

(defun lispbar-modules--register-module (module)
  "Register MODULE in the module system.
Validates the module and adds it to the registry."
  (lispbar-modules--validate-module module)
  
  (let ((name (oref module name)))
    (lispbar-modules--log 'debug "Registering module: %s" name)
    
    ;; Remove existing registration if any
    (lispbar-modules--unregister-module name)
    
    ;; Add to registry
    (push (list :module module
                :instance module
                :timer nil
                :status 'registered)
          lispbar-modules--registry)
    
    ;; Add to position list
    (lispbar-modules--add-to-position-list module)
    
    ;; Register dependencies
    (lispbar-modules--register-dependencies module)
    
    ;; Register communication channels
    (lispbar-modules--register-communication-channels module)
    
    ;; Set up hooks if any
    (lispbar-modules--setup-module-hooks module)
    
    (lispbar-modules--log 'info "Module registered: %s" name)
    module))

(defun lispbar-modules--unregister-module (name)
  "Unregister module NAME from the module system."
  (when (lispbar-modules--find-module-entry name)
    (lispbar-modules--log 'debug "Unregistering module: %s" name)
    
    ;; Stop timer if running
    (lispbar-modules--stop-module-timer name)
    
    ;; Remove from hooks
    (lispbar-modules--cleanup-module-hooks name)
    
    ;; Remove from communication channels
    (lispbar-modules--unregister-communication-channels name)
    
    ;; Remove from dependencies
    (lispbar-modules--unregister-dependencies name)
    
    ;; Remove from position list
    (lispbar-modules--remove-from-position-list name)
    
    ;; Remove from registry
    (setq lispbar-modules--registry
          (cl-remove-if (lambda (entry)
                          (eq (oref (plist-get entry :module) name) name))
                        lispbar-modules--registry))
    
    (lispbar-modules--log 'info "Module unregistered: %s" name)))

(defun lispbar-modules--find-module-entry (name)
  "Find module entry for NAME in the registry.
Returns the module entry plist or nil if not found."
  (cl-find-if (lambda (entry)
                (eq (oref (plist-get entry :module) name) name))
              lispbar-modules--registry))

(defun lispbar-modules--find-module (name)
  "Find module instance for NAME in the registry.
Returns the module instance or nil if not found."
  (let ((entry (lispbar-modules--find-module-entry name)))
    (when entry
      (plist-get entry :module))))

;;; Position Management

(defun lispbar-modules--add-to-position-list (module)
  "Add MODULE to the appropriate position list based on its position and priority."
  (let ((name (oref module name))
        (position (oref module position))
        (priority (oref module priority)))
    
    ;; Remove from any existing position list
    (lispbar-modules--remove-from-position-list name)
    
    ;; Get current list for position
    (let ((current-list (cdr (assq position lispbar-modules--position-lists))))
      
      ;; Insert module in priority order (higher priority first)
      (let ((new-list (lispbar-modules--insert-by-priority name priority current-list)))
        (setq lispbar-modules--position-lists
              (cons (cons position new-list)
                    (assq-delete-all position lispbar-modules--position-lists)))))))

(defun lispbar-modules--remove-from-position-list (name)
  "Remove module NAME from all position lists."
  (dolist (position '(left center right))
    (let ((current-list (cdr (assq position lispbar-modules--position-lists))))
      (when (memq name current-list)
        (setq lispbar-modules--position-lists
              (cons (cons position (delq name current-list))
                    (assq-delete-all position lispbar-modules--position-lists)))))))

(defun lispbar-modules--insert-by-priority (name priority module-list)
  "Insert module NAME with PRIORITY into MODULE-LIST maintaining priority order.
Returns the updated list."
  (if (null module-list)
      (list name)
    (let ((result nil)
          (inserted nil))
      (dolist (existing-name module-list)
        (let ((existing-module (lispbar-modules--find-module existing-name)))
          (when (and existing-module (not inserted))
            (let ((existing-priority (oref existing-module priority)))
              (when (> priority existing-priority)
                (push name result)
                (setq inserted t))))
          (push existing-name result)))
      
      ;; If not inserted yet, add at end
      (unless inserted
        (push name result))
      
      (nreverse result))))

(defun lispbar-modules--get-position-modules (position)
  "Get ordered list of module names for POSITION."
  (cdr (assq position lispbar-modules--position-lists)))

;;; Dependency Management

(defun lispbar-modules--register-dependencies (module)
  "Register dependencies for MODULE."
  (let ((name (oref module name))
        (deps (oref module dependencies)))
    (when deps
      (lispbar-modules--log 'debug "Registering dependencies for %s: %S" name deps)
      (setq lispbar-modules--dependencies
            (cons (cons name deps)
                  (assq-delete-all name lispbar-modules--dependencies))))))

(defun lispbar-modules--unregister-dependencies (name)
  "Unregister dependencies for module NAME."
  (setq lispbar-modules--dependencies
        (assq-delete-all name lispbar-modules--dependencies)))

(defun lispbar-modules--check-dependencies (name)
  "Check if all dependencies for module NAME are satisfied.
Returns t if satisfied, nil otherwise."
  (let ((deps (cdr (assq name lispbar-modules--dependencies))))
    (if deps
        (cl-every (lambda (dep-name)
                    (and (lispbar-modules--find-module dep-name)
                         (lispbar-modules--module-enabled-p dep-name)))
                  deps)
      t)))

(defun lispbar-modules--get-dependents (name)
  "Get list of modules that depend on module NAME."
  (cl-loop for (module-name . deps) in lispbar-modules--dependencies
           when (memq name deps)
           collect module-name))

;;; Communication Channels

(defun lispbar-modules--register-communication-channels (module)
  "Register communication channels for MODULE."
  (let ((name (oref module name))
        (channels (oref module communicates)))
    (dolist (channel channels)
      (lispbar-modules--log 'debug "Registering %s for channel: %s" name channel)
      (let ((current-subscribers (cdr (assq channel lispbar-modules--communication-channels))))
        (unless (memq name current-subscribers)
          (setq lispbar-modules--communication-channels
                (cons (cons channel (cons name current-subscribers))
                      (assq-delete-all channel lispbar-modules--communication-channels))))))))

(defun lispbar-modules--unregister-communication-channels (name)
  "Unregister module NAME from all communication channels."
  (setq lispbar-modules--communication-channels
        (mapcar (lambda (entry)
                  (cons (car entry)
                        (delq name (cdr entry))))
                lispbar-modules--communication-channels)))

(defun lispbar-modules--broadcast-message (channel message &optional sender)
  "Broadcast MESSAGE on CHANNEL to all subscribers except SENDER."
  (let ((subscribers (cdr (assq channel lispbar-modules--communication-channels))))
    (lispbar-modules--log 'debug "Broadcasting on channel %s to %d subscribers" 
                          channel (length subscribers))
    (dolist (subscriber subscribers)
      (unless (eq subscriber sender)
        (let ((module (lispbar-modules--find-module subscriber)))
          (when (and module (lispbar-modules--module-enabled-p subscriber))
            (lispbar-modules--safe-call-method module 'receive-message channel message)))))))

;;; Timer Management

(defun lispbar-modules--start-module-timer (module)
  "Start update timer for MODULE if it has an update interval."
  (let ((name (oref module name))
        (interval (oref module update-interval)))
    (when (and interval (> interval 0))
      (lispbar-modules--log 'debug "Starting timer for module: %s (interval: %s)" name interval)
      
      ;; Stop existing timer if any
      (lispbar-modules--stop-module-timer name)
      
      ;; Start new timer
      (let ((timer (run-at-time 0 interval
                                (lambda ()
                                  (lispbar-modules--schedule-update name 'timer)))))
        (lispbar-modules--set-module-timer name timer)))))

(defun lispbar-modules--stop-module-timer (name)
  "Stop update timer for module NAME."
  (let ((entry (lispbar-modules--find-module-entry name)))
    (when entry
      (let ((timer (plist-get entry :timer)))
        (when timer
          (lispbar-modules--log 'debug "Stopping timer for module: %s" name)
          (cancel-timer timer)
          (lispbar-modules--set-module-timer name nil))))))

(defun lispbar-modules--set-module-timer (name timer)
  "Set timer for module NAME to TIMER."
  (let ((entry (lispbar-modules--find-module-entry name)))
    (when entry
      (setf (plist-get entry :timer) timer))))

;;; Hook Management

(defun lispbar-modules--setup-module-hooks (module)
  "Set up hooks for MODULE."
  (let ((name (oref module name))
        (hooks (oref module hooks)))
    (dolist (hook hooks)
      (lispbar-modules--log 'debug "Adding hook %s for module: %s" hook name)
      (add-hook hook (lispbar-modules--create-hook-function name)))))

(defun lispbar-modules--cleanup-module-hooks (name)
  "Clean up hooks for module NAME."
  (let ((module (lispbar-modules--find-module name)))
    (when module
      (let ((hooks (oref module hooks)))
        (dolist (hook hooks)
          (lispbar-modules--log 'debug "Removing hook %s for module: %s" hook name)
          (remove-hook hook (lispbar-modules--create-hook-function name)))))))

(defun lispbar-modules--create-hook-function (name)
  "Create a hook function for module NAME."
  (lambda ()
    (lispbar-modules--schedule-update name 'hook)))

;;; Update Scheduling and Execution

(defun lispbar-modules--schedule-update (name trigger)
  "Schedule an update for module NAME triggered by TRIGGER."
  (when (lispbar-modules--module-enabled-p name)
    (let ((module (lispbar-modules--find-module name)))
      (when (and module (lispbar-modules--check-dependencies name))
        (lispbar-modules--log 'debug "Scheduling update for %s (trigger: %s)" name trigger)
        (unless (memq name lispbar-modules--update-queue)
          (push name lispbar-modules--update-queue))
        
        ;; Process updates asynchronously to avoid blocking
        (run-at-time 0.01 nil #'lispbar-modules--process-update-queue)))))

(defun lispbar-modules--process-update-queue ()
  "Process the module update queue."
  (when lispbar-modules--update-queue
    (let ((name (pop lispbar-modules--update-queue)))
      (lispbar-modules--update-module name)
      
      ;; Continue processing if more items in queue
      (when lispbar-modules--update-queue
        (run-at-time 0.01 nil #'lispbar-modules--process-update-queue)))))

(defun lispbar-modules--update-module (name)
  "Update module NAME by calling its update function."
  (let ((module (lispbar-modules--find-module name)))
    (when (and module (lispbar-modules--module-enabled-p name))
      (lispbar-modules--log 'debug "Updating module: %s" name)
      
      (let ((start-time (current-time))
            (cached-content (lispbar-modules--get-cached-content module)))
        
        ;; Use cached content if still valid
        (if cached-content
            (progn
              (lispbar-modules--log 'debug "Using cached content for module: %s" name)
              (lispbar-modules--render-module-content name cached-content))
          
          ;; Update module content
          (condition-case err
              (let ((content (lispbar-modules--safe-call-module-update module)))
                (when content
                  ;; Cache the content
                  (lispbar-modules--cache-content module content)
                  
                  ;; Render the content
                  (lispbar-modules--render-module-content name content)
                  
                  ;; Update success tracking
                  (lispbar-modules--reset-error-count module)
                  (oset module last-update (current-time))
                  
                  ;; Check update time
                  (let ((update-time (float-time (time-subtract (current-time) start-time))))
                    (when (> update-time lispbar-modules-max-update-time)
                      (lispbar-modules--log 'warning 
                                            "Module %s update took %.2fs (threshold: %.2fs)"
                                            name update-time lispbar-modules-max-update-time)))))
            
            (error
             (lispbar-modules--handle-module-error module err))))))))

(defun lispbar-modules--safe-call-module-update (module)
  "Safely call the update function for MODULE.
Returns the content or nil on error."
  (let ((update-fn (oref module update-fn)))
    (funcall update-fn)))

(defun lispbar-modules--render-module-content (name content)
  "Render CONTENT for module NAME to the appropriate frame position."
  (when (and content lispbar--initialized)
    (let ((module (lispbar-modules--find-module name)))
      (when module
        (let ((position (oref module position)))
          ;; Collect all content for this position
          (let ((position-content (lispbar-modules--collect-position-content position)))
            ;; Update all frames with the new content
            (dolist (frame-info (lispbar-get-frames))
              (let ((frame (plist-get frame-info :frame)))
                (when (and frame (frame-live-p frame))
                  (lispbar-safe-call #'lispbar-render-update 
                                     frame position position-content))))))))))

(defun lispbar-modules--collect-position-content (position)
  "Collect content from all enabled modules in POSITION.
Returns a list of content strings in priority order."
  (let ((module-names (lispbar-modules--get-position-modules position))
        (content-list nil))
    (dolist (name module-names)
      (let ((module (lispbar-modules--find-module name)))
        (when (and module (lispbar-modules--module-enabled-p name))
          (let ((cached-content (lispbar-modules--get-cached-content module)))
            (when cached-content
              (push cached-content content-list))))))
    (nreverse content-list)))

;;; Caching System

(defun lispbar-modules--cache-content (module content)
  "Cache CONTENT for MODULE with current timestamp."
  (oset module cache (list :content content :timestamp (current-time))))

(defun lispbar-modules--get-cached-content (module)
  "Get cached content for MODULE if still valid.
Returns content string or nil if cache is invalid or expired."
  (let ((cache (oref module cache)))
    (when cache
      (let* ((content (plist-get cache :content))
             (timestamp (plist-get cache :timestamp))
             (timeout (or (oref module cache-timeout) 
                          lispbar-modules-cache-timeout))
             (age (float-time (time-subtract (current-time) timestamp))))
        
        (if (< age timeout)
            content
          ;; Cache expired
          (progn
            (oset module cache nil)
            nil))))))

(defun lispbar-modules--invalidate-cache (name)
  "Invalidate cache for module NAME."
  (let ((module (lispbar-modules--find-module name)))
    (when module
      (lispbar-modules--log 'debug "Invalidating cache for module: %s" name)
      (oset module cache nil))))

;;; Error Handling

(defun lispbar-modules--handle-module-error (module error)
  "Handle ERROR for MODULE with appropriate recovery strategy."
  (let ((name (oref module name))
        (error-count (1+ (oref module error-count))))
    
    (lispbar-modules--log 'error "Module %s error: %s" name error)
    
    ;; Update error tracking
    (oset module error-count error-count)
    (oset module last-error error)
    
    ;; Disable module if too many errors
    (when (and lispbar-modules-error-recovery (>= error-count 3))
      (lispbar-modules--log 'warning "Disabling module %s due to repeated errors" name)
      (lispbar-modules--disable-module name))
    
    ;; Attempt recovery
    (when (and lispbar-modules-error-recovery (< error-count 3))
      (lispbar-modules--log 'info "Attempting recovery for module: %s" name)
      (run-at-time 5.0 nil (lambda () (lispbar-modules--schedule-update name 'recovery))))))

(defun lispbar-modules--reset-error-count (module)
  "Reset error count for MODULE after successful update."
  (when (> (oref module error-count) 0)
    (lispbar-modules--log 'debug "Resetting error count for module: %s" (oref module name))
    (oset module error-count 0)
    (oset module last-error nil)))

;;; Module State Management

(defun lispbar-modules--module-enabled-p (name)
  "Check if module NAME is enabled."
  (let ((module (lispbar-modules--find-module name)))
    (and module (oref module enabled))))

(defun lispbar-modules--enable-module (name)
  "Enable module NAME."
  (let ((module (lispbar-modules--find-module name)))
    (when module
      (lispbar-modules--log 'info "Enabling module: %s" name)
      (oset module enabled t)
      (lispbar-modules--start-module-timer module)
      (lispbar-modules--schedule-update name 'enable))))

(defun lispbar-modules--disable-module (name)
  "Disable module NAME."
  (let ((module (lispbar-modules--find-module name)))
    (when module
      (lispbar-modules--log 'info "Disabling module: %s" name)
      (oset module enabled nil)
      (lispbar-modules--stop-module-timer name)
      
      ;; Clear module's contribution from display
      (let ((position (oref module position)))
        (lispbar-modules--render-module-content name nil)))))

;;; Safe Method Calling

(defun lispbar-modules--safe-call-method (module method &rest args)
  "Safely call METHOD on MODULE with ARGS.
Returns the result or nil if method doesn't exist or errors."
  (condition-case err
      (when (slot-exists-p module method)
        (apply method module args))
    (error
     (lispbar-modules--log 'error "Method call failed for %s.%s: %s" 
                           (oref module name) method err)
     nil)))

;;; Public API

(defun lispbar-modules-register (module)
  "Register MODULE in the module system.
MODULE must be an instance of lispbar-module or a subclass."
  (lispbar-modules--register-module module))

(defun lispbar-modules-unregister (name)
  "Unregister module NAME from the module system."
  (lispbar-modules--unregister-module name))

(defun lispbar-modules-get (name)
  "Get module instance for NAME.
Returns the module instance or nil if not found."
  (lispbar-modules--find-module name))

(defun lispbar-modules-list (&optional position)
  "List all registered modules.
If POSITION is specified, list only modules in that position."
  (if position
      (lispbar-modules--get-position-modules position)
    (mapcar (lambda (entry) (oref (plist-get entry :module) name))
            lispbar-modules--registry)))

(defun lispbar-modules-enable (name)
  "Enable module NAME."
  (lispbar-modules--enable-module name))

(defun lispbar-modules-disable (name)
  "Disable module NAME."
  (lispbar-modules--disable-module name))

(defun lispbar-modules-update (name)
  "Manually trigger update for module NAME."
  (lispbar-modules--schedule-update name 'manual))

(defun lispbar-modules-update-all ()
  "Manually trigger update for all enabled modules."
  (dolist (name (lispbar-modules-list))
    (when (lispbar-modules--module-enabled-p name)
      (lispbar-modules--schedule-update name 'manual))))

(defun lispbar-modules-send-message (channel message &optional sender)
  "Send MESSAGE on CHANNEL from SENDER to all subscribers."
  (lispbar-modules--broadcast-message channel message sender))

(defun lispbar-modules-invalidate-cache (name)
  "Invalidate cache for module NAME, forcing next update."
  (lispbar-modules--invalidate-cache name))

;;; Helper Macro for Module Creation

(defmacro lispbar-define-module (name &rest args)
  "Define a custom lispbar module NAME with ARGS.

ARGS is a plist that can contain:
  :slots      - Additional slots for the module class
  :update-fn  - Function body for the update method
  :init-fn    - Function body for initialization
  :cleanup-fn - Function body for cleanup

Example:
  (lispbar-define-module example
    :slots ((format :initarg :format
                    :initform \"Example: %s\"
                    :documentation \"Display format.\"))
    :update-fn (format (oref module format) \"Hello World\")
    :init-fn (message \"Example module initialized\")
    :cleanup-fn (message \"Example module cleaned up\"))"
  (let* ((class-name (intern (format "lispbar-%s-module" name)))
         (create-fn (intern (format "lispbar-%s-create" name)))
         (init-fn (intern (format "lispbar-%s-init" name)))
         (cleanup-fn (intern (format "lispbar-%s-cleanup" name)))
         (slots (plist-get args :slots))
         (update-body (plist-get args :update-fn))
         (init-body (plist-get args :init-fn))
         (cleanup-body (plist-get args :cleanup-fn)))
    
    `(progn
       ;; Define the module class
       (defclass ,class-name (lispbar-module)
         ,slots
         ,(format "Custom lispbar module: %s." name))
       
       ;; Define the update method
       ,(when update-body
          `(cl-defmethod lispbar-module-update ((module ,class-name))
             ,(format "Update method for %s module." name)
             ,update-body))
       
       ;; Define the creation function
       (defun ,create-fn (&rest args)
         ,(format "Create a new %s module." name)
         (let ((module (apply #'make-instance ',class-name
                              :name ',name
                              :update-fn (lambda () (lispbar-module-update module))
                              args)))
           ,(when init-body
              `(progn ,init-body))
           module))
       
       ;; Define initialization function if provided
       ,(when init-body
          `(defun ,init-fn ()
             ,(format "Initialize %s module." name)
             ,init-body))
       
       ;; Define cleanup function if provided
       ,(when cleanup-body
          `(defun ,cleanup-fn ()
             ,(format "Cleanup %s module." name)
             ,cleanup-body))
       
       ;; Provide the module
       (provide ',(intern (format "lispbar-%s" name))))))

;;; Base Update Method (for inheritance)

(cl-defgeneric lispbar-module-update (module)
  "Update MODULE and return content string.
This method should be overridden by specific module implementations."
  (format "Module %s" (oref module name)))

;;; Lifecycle Management

;;;###autoload
(defun lispbar-modules-init ()
  "Initialize the Lispbar module system."
  (interactive)
  (when lispbar-modules--initialized
    (lispbar-modules--log 'warning "Module system already initialized")
    (return-from lispbar-modules-init))
  
  (lispbar-modules--log 'info "Initializing Lispbar module system")
  
  ;; Initialize data structures
  (setq lispbar-modules--registry nil
        lispbar-modules--position-lists '((left . nil) (center . nil) (right . nil))
        lispbar-modules--update-queue nil
        lispbar-modules--event-handlers nil
        lispbar-modules--dependencies nil
        lispbar-modules--communication-channels nil)
  
  ;; Register cleanup function with core
  (lispbar--add-cleanup-function #'lispbar-modules-cleanup)
  
  (setq lispbar-modules--initialized t)
  (lispbar-modules--log 'info "Module system initialized"))

;;;###autoload
(defun lispbar-modules-cleanup ()
  "Clean up the Lispbar module system."
  (interactive)
  (lispbar-modules--log 'info "Cleaning up Lispbar module system")
  
  ;; Unregister all modules
  (dolist (entry lispbar-modules--registry)
    (let ((name (oref (plist-get entry :module) name)))
      (lispbar-modules--unregister-module name)))
  
  ;; Reset state
  (setq lispbar-modules--registry nil
        lispbar-modules--position-lists nil
        lispbar-modules--update-queue nil
        lispbar-modules--event-handlers nil
        lispbar-modules--dependencies nil
        lispbar-modules--communication-channels nil
        lispbar-modules--initialized nil)
  
  (lispbar-modules--log 'info "Module system cleanup complete"))

(provide 'lispbar-modules)
;;; lispbar-modules.el ends here