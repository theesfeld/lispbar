;;; lispbar-clock.el --- Clock module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar Development Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4"))
;; Keywords: time, clock, modules, lispbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides a clock display for Lispbar with comprehensive
;; time and date formatting options. It supports customizable format
;; strings using `format-time-string', optional date display, timezone
;; selection, and real-time updates every second.
;;
;; The clock module demonstrates the full capabilities of the Lispbar
;; module system and serves as the first test module to verify the
;; complete architecture works correctly.
;;
;; Features:
;; - Real-time clock display with second precision
;; - Customizable time format using `format-time-string' syntax
;; - Optional date display with separate formatting
;; - Timezone support for different regions
;; - Configurable position (left/center/right) and priority
;; - Intelligent caching to reduce CPU usage
;; - Proper integration with Lispbar module lifecycle
;; - Full customization through Emacs custom system
;;
;; Usage:
;;   (require 'lispbar-clock)
;;   (lispbar-clock-enable)
;;
;; Customization:
;;   M-x customize-group RET lispbar-clock RET

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lispbar-modules)

;;; Customization

(defgroup lispbar-clock nil
  "Customization group for Lispbar clock module."
  :group 'lispbar-modules
  :prefix "lispbar-clock-")

(defcustom lispbar-clock-time-format "%H:%M:%S"
  "Format string for time display.
Uses `format-time-string' syntax. Common formats:
  %H:%M:%S - 24-hour format with seconds (14:30:45)
  %I:%M:%S %p - 12-hour format with seconds (2:30:45 PM)
  %H:%M - 24-hour format without seconds (14:30)
  %I:%M %p - 12-hour format without seconds (2:30 PM)"
  :type 'string
  :group 'lispbar-clock)

(defcustom lispbar-clock-date-format "%Y-%m-%d"
  "Format string for date display.
Uses `format-time-string' syntax. Common formats:
  %Y-%m-%d - ISO format (2025-06-27)
  %A, %B %d, %Y - Full format (Thursday, June 27, 2025)
  %a %b %d - Short format (Thu Jun 27)
  %m/%d/%Y - US format (06/27/2025)
  %d.%m.%Y - European format (27.06.2025)"
  :type 'string
  :group 'lispbar-clock)

(defcustom lispbar-clock-show-date t
  "Whether to display the date along with the time.
When enabled, both date and time are shown with the separator."
  :type 'boolean
  :group 'lispbar-clock)

(defcustom lispbar-clock-date-time-separator " | "
  "Separator string between date and time when both are displayed."
  :type 'string
  :group 'lispbar-clock)

(defcustom lispbar-clock-timezone nil
  "Timezone for clock display.
If nil, uses the system default timezone.
Can be a timezone string like 'America/New_York', 'Europe/London',
'Asia/Tokyo', etc. Use `M-x list-timezones' to see available options."
  :type '(choice (const :tag "System Default" nil)
                 (string :tag "Timezone"))
  :group 'lispbar-clock)

(defcustom lispbar-clock-position 'center
  "Position of the clock on the toolbar.
Can be 'left, 'center, or 'right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'lispbar-clock)

(defcustom lispbar-clock-priority 50
  "Display priority for the clock module.
Higher values are displayed first within the same position.
Range: 0-100, default: 50 (medium priority)."
  :type 'integer
  :group 'lispbar-clock)

(defcustom lispbar-clock-update-interval 1.0
  "Update interval for the clock in seconds.
Default is 1.0 for real-time second updates.
Can be increased to reduce CPU usage if seconds are not displayed."
  :type 'number
  :group 'lispbar-clock)

(defcustom lispbar-clock-cache-timeout 0.9
  "Cache timeout for clock content in seconds.
Should be slightly less than update-interval to ensure fresh display.
Set to 0 to disable caching (not recommended)."
  :type 'number
  :group 'lispbar-clock)

(defcustom lispbar-clock-face 'lispbar-render-default
  "Face to use for clock display.
Can be any valid face name or face specification."
  :type 'face
  :group 'lispbar-clock)

(defcustom lispbar-clock-urgent-face 'lispbar-render-urgent
  "Face to use for urgent time notifications.
Currently unused but available for future features."
  :type 'face
  :group 'lispbar-clock)

;;; Variables

(defvar lispbar-clock--module-instance nil
  "Instance of the clock module.")

(defvar lispbar-clock--enabled nil
  "Whether the clock module is currently enabled.")

;;; Clock Module Class

(defclass lispbar-clock-module (lispbar-module)
  ((time-format :initarg :time-format
                :type string
                :initform "%H:%M:%S"
                :documentation "Format string for time display.")
   (date-format :initarg :date-format
                :type string
                :initform "%Y-%m-%d"
                :documentation "Format string for date display.")
   (show-date :initarg :show-date
              :type boolean
              :initform t
              :documentation "Whether to show date along with time.")
   (separator :initarg :separator
              :type string
              :initform " | "
              :documentation "Separator between date and time.")
   (timezone :initarg :timezone
             :type (or null string)
             :initform nil
             :documentation "Timezone for display, nil for system default.")
   (face :initarg :face
         :type (or symbol list)
         :initform 'lispbar-render-default
         :documentation "Face for clock display."))
  "Clock module class for Lispbar.
Displays current time and optionally date with customizable formatting.")

;;; Helper Functions

(defun lispbar-clock--validate-timezone (timezone)
  "Validate TIMEZONE string.
Returns t if valid, nil otherwise."
  (if (null timezone)
      t
    (condition-case nil
        (progn
          ;; Test if timezone is valid by trying to format time with it
          (let ((process-environment 
                 (cons (concat "TZ=" timezone) process-environment)))
            (format-time-string "%H:%M" (current-time)))
          t)
      (error nil))))

(defun lispbar-clock--format-time-with-timezone (format-string timezone)
  "Format current time using FORMAT-STRING in TIMEZONE.
Returns formatted time string."
  (let ((time (current-time)))
    (if timezone
        (let ((process-environment 
               (cons (concat "TZ=" timezone) process-environment)))
          (format-time-string format-string time))
      (format-time-string format-string time))))

(defun lispbar-clock--get-display-content (module)
  "Get formatted display content for clock MODULE.
Returns propertized string ready for display."
  (let* ((time-format (oref module time-format))
         (date-format (oref module date-format))
         (show-date (oref module show-date))
         (separator (oref module separator))
         (timezone (oref module timezone))
         (face (oref module face))
         (time-str (lispbar-clock--format-time-with-timezone time-format timezone))
         (content-str (if show-date
                          (let ((date-str (lispbar-clock--format-time-with-timezone 
                                         date-format timezone)))
                            (concat date-str separator time-str))
                        time-str)))
    
    ;; Apply face if specified
    (if face
        (propertize content-str 'face face)
      content-str)))

;;; Module Implementation

(cl-defmethod lispbar-module-update ((module lispbar-clock-module))
  "Update method for clock module.
Returns formatted time/date string for display."
  (condition-case err
      (lispbar-clock--get-display-content module)
    (error
     (lispbar-modules--log 'error "Clock module update failed: %s" err)
     (propertize "Clock Error" 'face 'lispbar-render-urgent))))

(cl-defmethod lispbar-clock-configure ((module lispbar-clock-module))
  "Configure clock module with current customization values."
  (oset module time-format lispbar-clock-time-format)
  (oset module date-format lispbar-clock-date-format)
  (oset module show-date lispbar-clock-show-date)
  (oset module separator lispbar-clock-date-time-separator)
  (oset module timezone lispbar-clock-timezone)
  (oset module face lispbar-clock-face)
  (oset module position lispbar-clock-position)
  (oset module priority lispbar-clock-priority)
  (oset module update-interval lispbar-clock-update-interval)
  (oset module cache-timeout lispbar-clock-cache-timeout)
  
  ;; Update position list if position changed
  (lispbar-modules--add-to-position-list module)
  
  ;; Validate timezone if specified
  (when (oref module timezone)
    (unless (lispbar-clock--validate-timezone (oref module timezone))
      (lispbar-modules--log 'warning "Invalid timezone: %s, using system default" 
                            (oref module timezone))
      (oset module timezone nil))))

;;; Module Creation and Management

(defun lispbar-clock--create-module ()
  "Create and configure a new clock module instance.
Returns the configured module instance."
  (let ((module (make-instance 'lispbar-clock-module
                               :name 'clock
                               :update-fn (lambda () 
                                          (lispbar-module-update lispbar-clock--module-instance))
                               :update-interval lispbar-clock-update-interval
                               :position lispbar-clock-position
                               :priority lispbar-clock-priority
                               :cache-timeout lispbar-clock-cache-timeout
                               :enabled t)))
    
    ;; Configure with current customization values
    (lispbar-clock-configure module)
    
    ;; Set the global instance
    (setq lispbar-clock--module-instance module)
    
    (lispbar-modules--log 'info "Clock module created")
    module))

;;;###autoload
(defun lispbar-clock-enable ()
  "Enable the Lispbar clock module.
Creates and registers the clock module if not already enabled."
  (interactive)
  (if lispbar-clock--enabled
      (lispbar-modules--log 'info "Clock module already enabled")
    (progn
      (lispbar-modules--log 'info "Enabling clock module")
      
      ;; Ensure module system is initialized
      (unless lispbar-modules--initialized
        (lispbar-modules-init))
      
      ;; Create and register module
      (let ((module (lispbar-clock--create-module)))
        (lispbar-modules-register module)
        (setq lispbar-clock--enabled t)
        (lispbar-modules--log 'info "Clock module enabled successfully")))))

;;;###autoload
(defun lispbar-clock-disable ()
  "Disable the Lispbar clock module.
Unregisters and cleans up the clock module."
  (interactive)
  (if (not lispbar-clock--enabled)
      (lispbar-modules--log 'info "Clock module not enabled")
    (progn
      (lispbar-modules--log 'info "Disabling clock module")
      
      ;; Unregister module
      (when lispbar-clock--module-instance
        (lispbar-modules-unregister 'clock))
      
      ;; Clean up
      (setq lispbar-clock--module-instance nil
            lispbar-clock--enabled nil)
      
      (lispbar-modules--log 'info "Clock module disabled"))))

;;;###autoload
(defun lispbar-clock-toggle ()
  "Toggle the Lispbar clock module on/off."
  (interactive)
  (if lispbar-clock--enabled
      (lispbar-clock-disable)
    (lispbar-clock-enable)))

;;;###autoload
(defun lispbar-clock-reconfigure ()
  "Reconfigure the clock module with current customization values.
Useful after changing clock customization options."
  (interactive)
  (when (and lispbar-clock--enabled lispbar-clock--module-instance)
    (lispbar-modules--log 'info "Reconfiguring clock module")
    (lispbar-clock-configure lispbar-clock--module-instance)
    (lispbar-modules-invalidate-cache 'clock)
    (lispbar-modules-update 'clock)
    (lispbar-modules--log 'info "Clock module reconfigured")))

;;; Interactive Commands

;;;###autoload
(defun lispbar-clock-set-timezone (timezone)
  "Set the clock timezone to TIMEZONE.
Use tab completion to see available timezones."
  (interactive 
   (list (completing-read "Timezone (empty for system default): "
                          ;; Provide some common timezones for completion
                          '("America/New_York" "America/Chicago" "America/Denver" 
                            "America/Los_Angeles" "Europe/London" "Europe/Paris"
                            "Europe/Berlin" "Asia/Tokyo" "Asia/Shanghai" 
                            "Australia/Sydney")
                          nil nil nil nil "")))
  (setq lispbar-clock-timezone (if (string-empty-p timezone) nil timezone))
  (when lispbar-clock--enabled
    (lispbar-clock-reconfigure))
  (message "Clock timezone set to: %s" (or timezone "system default")))

;;;###autoload
(defun lispbar-clock-set-format (time-format &optional date-format)
  "Set clock TIME-FORMAT and optionally DATE-FORMAT.
TIME-FORMAT uses `format-time-string' syntax.
If DATE-FORMAT is not provided, only time format is changed."
  (interactive 
   (list (read-string "Time format: " lispbar-clock-time-format)
         (when current-prefix-arg
           (read-string "Date format: " lispbar-clock-date-format))))
  (setq lispbar-clock-time-format time-format)
  (when date-format
    (setq lispbar-clock-date-format date-format))
  (when lispbar-clock--enabled
    (lispbar-clock-reconfigure))
  (message "Clock format updated"))

;;;###autoload
(defun lispbar-clock-toggle-date ()
  "Toggle date display on/off for the clock module."
  (interactive)
  (setq lispbar-clock-show-date (not lispbar-clock-show-date))
  (when lispbar-clock--enabled
    (lispbar-clock-reconfigure))
  (message "Clock date display: %s" (if lispbar-clock-show-date "enabled" "disabled")))

;;; Status and Information Functions

(defun lispbar-clock-status ()
  "Display current status of the clock module.
Shows whether enabled, current format, timezone, etc."
  (interactive)
  (if lispbar-clock--enabled
      (let* ((module lispbar-clock--module-instance)
             (time-format (oref module time-format))
             (date-format (oref module date-format))
             (show-date (oref module show-date))
             (timezone (oref module timezone))
             (position (oref module position))
             (priority (oref module priority))
             (sample-output (lispbar-clock--get-display-content module)))
        (message (concat "Clock module: ENABLED\n"
                        "Position: %s (priority %d)\n"
                        "Time format: %s\n"
                        "Date format: %s (shown: %s)\n"
                        "Timezone: %s\n"
                        "Sample output: %s")
                position priority time-format date-format 
                (if show-date "yes" "no")
                (or timezone "system default")
                sample-output))
    (message "Clock module: DISABLED")))

;;; Module Cleanup

(defun lispbar-clock--cleanup ()
  "Clean up clock module resources.
Called during module system shutdown."
  (when lispbar-clock--enabled
    (lispbar-clock-disable)))

;; Register cleanup function
(eval-after-load 'lispbar-modules
  '(lispbar--add-cleanup-function #'lispbar-clock--cleanup))

;;; Provide

(provide 'lispbar-clock)
;;; lispbar-clock.el ends here