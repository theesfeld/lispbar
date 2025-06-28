;;; lispbar-battery.el --- Battery status module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar Development Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4"))
;; Keywords: battery, power, modules, lispbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides battery status monitoring for Lispbar with
;; comprehensive power information display. It supports cross-platform
;; battery detection, customizable warning thresholds, and intelligent
;; icon-based status representation.
;;
;; The battery module detects power supply information across multiple
;; platforms and provides real-time battery status with configurable
;; display formats and warning levels.
;;
;; Features:
;; - Cross-platform battery detection (Linux, macOS, Windows)
;; - Battery percentage with charging/discharging status
;; - Time remaining calculations (charging/discharging)
;; - Configurable low/critical battery thresholds
;; - Icon support for different battery states
;; - Customizable display format templates
;; - Graceful handling of systems without batteries
;; - Intelligent update intervals (30 seconds default)
;; - Warning faces for low/critical battery levels
;; - Proper integration with Lispbar module lifecycle
;;
;; Usage:
;;   (require 'lispbar-battery)
;;   (lispbar-battery-enable)
;;
;; Customization:
;;   M-x customize-group RET lispbar-battery RET

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lispbar-modules)
(require 'battery nil t)

;;; Customization

(defgroup lispbar-battery nil
  "Customization group for Lispbar battery module."
  :group 'lispbar-modules
  :prefix "lispbar-battery-")

(defcustom lispbar-battery-format "{icon} {percentage}% ({status})"
  "Format string for battery display.
Available placeholders:
  {icon}        - Battery status icon
  {percentage}  - Battery charge percentage
  {status}      - Charging status (charging/discharging/full/unknown)
  {time}        - Time remaining (if available)
  {power}       - Power consumption/input (if available)

Example formats:
  \="{icon} {percentage}%\=" - Simple icon and percentage
  \="{icon} {percentage}% ({time})\=" - Include time remaining
  \="Battery: {percentage}% {status}\=" - Verbose format"
  :type 'string
  :group 'lispbar-battery)

(defcustom lispbar-battery-low-threshold 20
  "Battery percentage threshold for low battery warning.
When battery percentage falls below this value, the low battery
face will be used for display."
  :type 'integer
  :group 'lispbar-battery)

(defcustom lispbar-battery-critical-threshold 10
  "Battery percentage threshold for critical battery warning.
When battery percentage falls below this value, the critical battery
face will be used for display."
  :type 'integer
  :group 'lispbar-battery)

(defcustom lispbar-battery-show-percentage t
  "Whether to display battery percentage.
When disabled, only icons and status information are shown."
  :type 'boolean
  :group 'lispbar-battery)

(defcustom lispbar-battery-show-time t
  "Whether to display time remaining when available.
Time information depends on system battery reporting capabilities."
  :type 'boolean
  :group 'lispbar-battery)

(defcustom lispbar-battery-icons-charging
  '((100 . "🔋") (75 . "🔋") (50 . "🔋") (25 . "🔋") (0 . "🔋"))
  "Icons for charging battery at different levels.
List of (percentage . icon) pairs, used when battery is charging."
  :type '(alist :key-type integer :value-type string)
  :group 'lispbar-battery)

(defcustom lispbar-battery-icons-discharging
  '((100 . "🔋") (75 . "🔋") (50 . "🔋") (25 . "🪫") (0 . "🪫"))
  "Icons for discharging battery at different levels.
List of (percentage . icon) pairs, used when battery is discharging."
  :type '(alist :key-type integer :value-type string)
  :group 'lispbar-battery)

(defcustom lispbar-battery-icon-full "🔋"
  "Icon to display when battery is full."
  :type 'string
  :group 'lispbar-battery)

(defcustom lispbar-battery-icon-charging "⚡"
  "Icon to display when battery is charging (alternative mode)."
  :type 'string
  :group 'lispbar-battery)

(defcustom lispbar-battery-icon-unknown "❓"
  "Icon to display when battery status is unknown."
  :type 'string
  :group 'lispbar-battery)

(defcustom lispbar-battery-position 'right
  "Position of the battery module on the toolbar.
Can be \='left, \='center, or \='right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'lispbar-battery)

(defcustom lispbar-battery-priority 60
  "Display priority for the battery module.
Higher values are displayed first within the same position.
Range: 0-100, default: 60 (medium-high priority)."
  :type 'integer
  :group 'lispbar-battery)

(defcustom lispbar-battery-update-interval 30.0
  "Update interval for the battery module in seconds.
Default is 30.0 seconds to balance freshness and system load."
  :type 'number
  :group 'lispbar-battery)

(defcustom lispbar-battery-cache-timeout 25.0
  "Cache timeout for battery content in seconds.
Should be less than update-interval to ensure fresh display."
  :type 'number
  :group 'lispbar-battery)

(defcustom lispbar-battery-face 'lispbar-render-default
  "Face to use for normal battery display."
  :type 'face
  :group 'lispbar-battery)

(defcustom lispbar-battery-low-face 'lispbar-render-warning
  "Face to use for low battery warning."
  :type 'face
  :group 'lispbar-battery)

(defcustom lispbar-battery-critical-face 'lispbar-render-urgent
  "Face to use for critical battery warning."
  :type 'face
  :group 'lispbar-battery)

(defcustom lispbar-battery-charging-face 'lispbar-render-success
  "Face to use when battery is charging."
  :type 'face
  :group 'lispbar-battery)

(defcustom lispbar-battery-enable-notifications t
  "Whether to show system notifications for battery warnings.
When enabled, shows notifications for low and critical battery levels."
  :type 'boolean
  :group 'lispbar-battery)

(defcustom lispbar-battery-platform-override nil
  "Override platform detection for battery information.
Can be \='linux, \='macos, \='windows, or nil for auto-detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "Linux" linux)
                 (const :tag "macOS" macos)
                 (const :tag "Windows" windows))
  :group 'lispbar-battery)

;;; Variables

(defvar lispbar-battery--module-instance nil
  "Instance of the battery module.")

(defvar lispbar-battery--enabled nil
  "Whether the battery module is currently enabled.")

(defvar lispbar-battery--last-percentage nil
  "Last reported battery percentage for notification tracking.")

(defvar lispbar-battery--last-status nil
  "Last reported battery status for change detection.")

(defvar lispbar-battery--platform nil
  "Detected platform for battery information.")

(defvar lispbar-battery--backend nil
  "Selected battery backend function.")

;;; Battery Module Class

(defclass lispbar-battery-module (lispbar-module)
  ((format-string :initarg :format-string
                  :type string
                  :initform "{icon} {percentage}% ({status})"
                  :documentation "Format string for battery display.")
   (low-threshold :initarg :low-threshold
                  :type integer
                  :initform 20
                  :documentation "Low battery warning threshold.")
   (critical-threshold :initarg :critical-threshold
                       :type integer
                       :initform 10
                       :documentation "Critical battery warning threshold.")
   (show-percentage :initarg :show-percentage
                    :type boolean
                    :initform t
                    :documentation "Whether to show battery percentage.")
   (show-time :initarg :show-time
              :type boolean
              :initform t
              :documentation "Whether to show time remaining.")
   (icons-charging :initarg :icons-charging
                   :type list
                   :initform nil
                   :documentation "Icons for charging battery states.")
   (icons-discharging :initarg :icons-discharging
                      :type list
                      :initform nil
                      :documentation "Icons for discharging battery states.")
   (face :initarg :face
         :type (or symbol list)
         :initform 'lispbar-render-default
         :documentation "Default face for battery display.")
   (low-face :initarg :low-face
             :type (or symbol list)
             :initform 'lispbar-render-warning
             :documentation "Face for low battery warning.")
   (critical-face :initarg :critical-face
                  :type (or symbol list)
                  :initform 'lispbar-render-urgent
                  :documentation "Face for critical battery warning.")
   (charging-face :initarg :charging-face
                  :type (or symbol list)
                  :initform 'lispbar-render-success
                  :documentation "Face for charging status.")
   (platform :initform nil
             :documentation "Detected platform for battery access.")
   (backend :initform nil
            :documentation "Selected battery backend function."))
  "Battery module class for Lispbar.
Displays battery status with customizable formatting and warnings.")

;;; Platform Detection

(defun lispbar-battery--detect-platform ()
  "Detect the current platform for battery information access.
Returns \='linux, \='macos, \='windows, or \='unknown."
  (or lispbar-battery-platform-override
      (cond
       ((eq system-type 'gnu/linux) 'linux)
       ((eq system-type 'darwin) 'macos)
       ((eq system-type 'windows-nt) 'windows)
       (t 'unknown))))

(defun lispbar-battery--select-backend (platform)
  "Select appropriate battery backend function for PLATFORM.
Returns a function that retrieves battery information."
  (pcase platform
    ('linux #'lispbar-battery--linux-backend)
    ('macos #'lispbar-battery--macos-backend)
    ('windows #'lispbar-battery--windows-backend)
    (_ #'lispbar-battery--fallback-backend)))

;;; Battery Information Backends

(defun lispbar-battery--linux-backend ()
  "Get battery information on Linux systems.
Returns a plist with battery data or nil if unavailable."
  (condition-case err
      (let ((power-supply-dir "/sys/class/power_supply"))
        (when (file-directory-p power-supply-dir)
          (let ((battery-dirs (lispbar-battery--find-linux-batteries power-supply-dir)))
            (when battery-dirs
              (lispbar-battery--parse-linux-battery (car battery-dirs))))))
    (error
     (lispbar-modules--log 'error "Linux battery backend failed: %s" err)
     nil)))

(defun lispbar-battery--find-linux-batteries (power-supply-dir)
  "Find battery directories in POWER-SUPPLY-DIR.
Returns list of battery directory paths."
  (let ((battery-dirs nil))
    (dolist (entry (directory-files power-supply-dir t "^[^.]"))
      (when (file-directory-p entry)
        (let ((type-file (expand-file-name "type" entry)))
          (when (and (file-readable-p type-file)
                     (string= "Battery" 
                              (string-trim (lispbar-battery--read-file type-file))))
            (push entry battery-dirs)))))
    battery-dirs))

(defun lispbar-battery--parse-linux-battery (battery-dir)
  "Parse battery information from BATTERY-DIR.
Returns a plist with battery data."
  (let* ((capacity-file (expand-file-name "capacity" battery-dir))
         (status-file (expand-file-name "status" battery-dir))
         (voltage-file (expand-file-name "voltage_now" battery-dir))
         (current-file (expand-file-name "current_now" battery-dir))
         (energy-full-file (expand-file-name "energy_full" battery-dir))
         (energy-now-file (expand-file-name "energy_now" battery-dir))
         (power-file (expand-file-name "power_now" battery-dir)))
    
    (let ((percentage (lispbar-battery--read-number capacity-file))
          (status (lispbar-battery--read-file status-file))
          (voltage (lispbar-battery--read-number voltage-file))
          (current (lispbar-battery--read-number current-file))
          (energy-full (lispbar-battery--read-number energy-full-file))
          (energy-now (lispbar-battery--read-number energy-now-file))
          (power (lispbar-battery--read-number power-file)))
      
      (list :percentage percentage
            :status (lispbar-battery--normalize-status status)
            :voltage voltage
            :current current
            :energy-full energy-full
            :energy-now energy-now
            :power power
            :time-remaining (lispbar-battery--calculate-time-remaining 
                           percentage status current energy-full energy-now power)))))

(defun lispbar-battery--macos-backend ()
  "Get battery information on macOS systems.
Returns a plist with battery data or nil if unavailable."
  (condition-case err
      (let ((pmset-output (shell-command-to-string "pmset -g batt")))
        (when (and pmset-output (not (string-empty-p pmset-output)))
          (lispbar-battery--parse-macos-pmset pmset-output)))
    (error
     (lispbar-modules--log 'error "macOS battery backend failed: %s" err)
     nil)))

(defun lispbar-battery--parse-macos-pmset (output)
  "Parse pmset OUTPUT for battery information.
Returns a plist with battery data."
  (when (string-match "\\([0-9]+\\)%.*\\(charging\\|discharging\\|charged\\|AC Power\\)" output)
    (let* ((percentage (string-to-number (match-string 1 output)))
           (status-raw (match-string 2 output))
           (status (lispbar-battery--normalize-macos-status status-raw))
           (time-remaining (lispbar-battery--parse-macos-time output)))
      
      (list :percentage percentage
            :status status
            :time-remaining time-remaining))))

(defun lispbar-battery--normalize-macos-status (status)
  "Normalize macOS battery STATUS string."
  (pcase status
    ("charging" "charging")
    ("discharging" "discharging")
    ("charged" "full")
    ("AC Power" "full")
    (_ "unknown")))

(defun lispbar-battery--parse-macos-time (output)
  "Parse time remaining from macOS pmset OUTPUT.
Returns time in minutes or nil if unavailable."
  (when (string-match "\\([0-9]+\\):\\([0-9]+\\) remaining" output)
    (let ((hours (string-to-number (match-string 1 output)))
          (minutes (string-to-number (match-string 2 output))))
      (+ (* hours 60) minutes))))

(defun lispbar-battery--windows-backend ()
  "Get battery information on Windows systems.
Returns a plist with battery data or nil if unavailable."
  (condition-case err
      (let ((wmic-output (shell-command-to-string 
                         "wmic path Win32_Battery get EstimatedChargeRemaining /value")))
        (when (and wmic-output (not (string-empty-p wmic-output)))
          (lispbar-battery--parse-windows-wmic wmic-output)))
    (error
     (lispbar-modules--log 'error "Windows battery backend failed: %s" err)
     nil)))

(defun lispbar-battery--parse-windows-wmic (output)
  "Parse Windows wmic OUTPUT for battery information.
Returns a plist with battery data."
  (when (string-match "EstimatedChargeRemaining=\\([0-9]+\\)" output)
    (let ((percentage (string-to-number (match-string 1 output))))
      (list :percentage percentage
            :status "unknown"))))

(defun lispbar-battery--fallback-backend ()
  "Fallback battery backend using Emacs built-in battery.el.
Returns a plist with battery data or nil if unavailable."
  (condition-case err
      (when (and (featurep 'battery)
                 (functionp 'battery-status-function)
                 battery-status-function)
        (let ((battery-data (funcall battery-status-function)))
          (when battery-data
            (lispbar-battery--parse-emacs-battery battery-data))))
    (error
     (lispbar-modules--log 'error "Fallback battery backend failed: %s" err)
     nil)))

(defun lispbar-battery--parse-emacs-battery (battery-data)
  "Parse Emacs BATTERY-DATA alist into normalized format.
Returns a plist with battery data."
  (let ((percentage-str (cdr (assoc ?p battery-data)))
        (status-str (cdr (assoc ?B battery-data)))
        (time-str (cdr (assoc ?t battery-data))))
    
    (list :percentage (when percentage-str (string-to-number percentage-str))
          :status (lispbar-battery--normalize-status status-str)
          :time-remaining (when time-str (lispbar-battery--parse-time-string time-str)))))

;;; Helper Functions

(defun lispbar-battery--read-file (file-path)
  "Read content from FILE-PATH and return as string.
Returns nil if file is not readable."
  (when (and file-path (file-readable-p file-path))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file-path)
          (string-trim (buffer-string)))
      (error nil))))

(defun lispbar-battery--read-number (file-path)
  "Read numeric content from FILE-PATH.
Returns number or nil if file is not readable or not numeric."
  (let ((content (lispbar-battery--read-file file-path)))
    (when (and content (string-match-p "^[0-9]+$" content))
      (string-to-number content))))

(defun lispbar-battery--normalize-status (status)
  "Normalize battery STATUS string to standard values.
Returns \='charging, \='discharging, \='full, or \='unknown."
  (when status
    (let ((status-lower (downcase (string-trim status))))
      (cond
       ((member status-lower '("charging" "charge")) "charging")
       ((member status-lower '("discharging" "discharge")) "discharging")
       ((member status-lower '("full" "charged" "complete")) "full")
       ((member status-lower '("not charging" "idle")) "full")
       (t "unknown")))))

(defun lispbar-battery--calculate-time-remaining (percentage status current energy-full energy-now power)
  "Calculate time remaining based on battery parameters.
Returns time in minutes or nil if calculation not possible."
  (when (and percentage status)
    (cond
     ((and (string= status "discharging") energy-now power (> power 0))
      ;; Time to empty based on current energy and power consumption
      (/ (* energy-now 60) (* power 1000000))) ; Convert to minutes
     
     ((and (string= status "charging") energy-full energy-now power (> power 0))
      ;; Time to full based on remaining energy and charging power
      (/ (* (- energy-full energy-now) 60) (* power 1000000)))
     
     ((and (string= status "discharging") current (> current 0))
      ;; Rough estimate based on current draw (assuming linear discharge)
      (* (/ percentage 100.0) 5 60)) ; Assume 5-hour battery life
     
     (t nil))))

(defun lispbar-battery--parse-time-string (time-str)
  "Parse TIME-STR in format 'H:MM' to minutes.
Returns time in minutes or nil if parsing fails."
  (when (and time-str (string-match "\\([0-9]+\\):\\([0-9]+\\)" time-str))
    (let ((hours (string-to-number (match-string 1 time-str)))
          (minutes (string-to-number (match-string 2 time-str))))
      (+ (* hours 60) minutes))))

(defun lispbar-battery--format-time (minutes)
  "Format MINUTES into human-readable time string.
Returns formatted string like '1h 30m' or '45m'."
  (when (and minutes (> minutes 0))
    (let ((hours (/ minutes 60))
          (mins (% minutes 60)))
      (if (> hours 0)
          (format "%dh %02dm" hours mins)
        (format "%dm" mins)))))

(defun lispbar-battery--get-icon (percentage status)
  "Get appropriate icon for battery PERCENTAGE and STATUS.
Returns icon string based on current battery state."
  (let ((icons (if (string= status "charging")
                   lispbar-battery-icons-charging
                 lispbar-battery-icons-discharging)))
    
    (cond
     ((string= status "full") lispbar-battery-icon-full)
     ((string= status "charging") lispbar-battery-icon-charging)
     ((string= status "unknown") lispbar-battery-icon-unknown)
     (t
      ;; Find appropriate icon based on percentage
      (let ((icon-entry (cl-find-if (lambda (entry)
                                     (<= (car entry) percentage))
                                   (sort (copy-sequence icons)
                                         (lambda (a b) (> (car a) (car b)))))))
        (if icon-entry
            (cdr icon-entry)
          lispbar-battery-icon-unknown))))))

(defun lispbar-battery--determine-face (percentage status)
  "Determine appropriate face for battery PERCENTAGE and STATUS.
Returns face symbol based on battery state and thresholds."
  (cond
   ((string= status "charging") 'lispbar-render-success)
   ((string= status "full") 'lispbar-render-default)
   ((and percentage (<= percentage lispbar-battery-critical-threshold)) 
    'lispbar-render-urgent)
   ((and percentage (<= percentage lispbar-battery-low-threshold)) 
    'lispbar-render-warning)
   (t 'lispbar-render-default)))

(defun lispbar-battery--format-display (module battery-info)
  "Format battery display for MODULE using BATTERY-INFO.
Returns formatted and propertized string for display."
  (if (not battery-info)
      (propertize "No Battery" 'face 'lispbar-render-dim)
    
    (let* ((percentage (plist-get battery-info :percentage))
           (status (plist-get battery-info :status))
           (time-remaining (plist-get battery-info :time-remaining))
           (format-string (oref module format-string))
           (icon (lispbar-battery--get-icon percentage status))
           (face (lispbar-battery--determine-face percentage status))
           (time-str (when time-remaining 
                      (lispbar-battery--format-time time-remaining))))
      
      ;; Replace placeholders in format string
      (let ((formatted (replace-regexp-in-string
                       "{icon}" (or icon "")
                       (replace-regexp-in-string
                        "{percentage}" (if percentage (number-to-string percentage) "?")
                        (replace-regexp-in-string
                         "{status}" (or status "unknown")
                         (replace-regexp-in-string
                          "{time}" (or time-str "")
                          (replace-regexp-in-string
                           "{power}" "" ; Power info placeholder for future use
                           format-string)))))))
        
        ;; Apply appropriate face
        (propertize (string-trim formatted) 'face face)))))

(defun lispbar-battery--check-notifications (percentage status)
  "Check if notifications should be sent for PERCENTAGE and STATUS.
Sends system notifications for battery warnings if enabled."
  (when (and lispbar-battery-enable-notifications percentage
             (not (string= status "charging"))
             (not (string= status "full")))
    
    (let ((last-percentage lispbar-battery--last-percentage)
          (last-status lispbar-battery--last-status))
      
      ;; Critical battery notification
      (when (and (<= percentage lispbar-battery-critical-threshold)
                 (or (not last-percentage)
                     (> last-percentage lispbar-battery-critical-threshold)
                     (and (string= last-status "charging")
                          (not (string= status "charging")))))
        (lispbar-battery--send-notification 
         "Critical Battery Warning" 
         (format "Battery level is critically low: %d%%" percentage)
         'critical))
      
      ;; Low battery notification
      (when (and (<= percentage lispbar-battery-low-threshold)
                 (> percentage lispbar-battery-critical-threshold)
                 (or (not last-percentage)
                     (> last-percentage lispbar-battery-low-threshold)
                     (and (string= last-status "charging")
                          (not (string= status "charging")))))
        (lispbar-battery--send-notification 
         "Low Battery Warning" 
         (format "Battery level is low: %d%%" percentage)
         'warning)))
    
    ;; Update tracking variables
    (setq lispbar-battery--last-percentage percentage
          lispbar-battery--last-status status)))

(defun lispbar-battery--send-notification (title message urgency)
  "Send system notification with TITLE, MESSAGE, and URGENCY level.
URGENCY can be \\='critical, \\='warning, or \\='normal."
  (condition-case err
      (cond
       ;; Try notifications.el if available
       ((and (featurep 'notifications) (functionp 'notifications-notify))
        (notifications-notify :title title
                              :body message
                              :urgency urgency
                              :app-name "Lispbar Battery"))
       
       ;; Fallback to message
       (t (message "%s: %s" title message)))
    (error
     (lispbar-modules--log 'error "Notification failed: %s" err))))

;;; Module Implementation

(cl-defmethod lispbar-module-update ((module lispbar-battery-module))
  "Update method for battery module.
Returns formatted battery status string for display."
  (condition-case err
      (let* ((backend (oref module backend))
             (battery-info (when backend (funcall backend))))
        
        (when battery-info
          (let ((percentage (plist-get battery-info :percentage))
                (status (plist-get battery-info :status)))
            
            ;; Check for notifications
            (lispbar-battery--check-notifications percentage status)))
        
        ;; Format and return display string
        (lispbar-battery--format-display module battery-info))
    (error
     (lispbar-modules--log 'error "Battery module update failed: %s" err)
     (propertize "Battery Error" 'face 'lispbar-render-urgent))))

(cl-defmethod lispbar-battery-configure ((module lispbar-battery-module))
  "Configure battery module with current customization values."
  (oset module format-string lispbar-battery-format)
  (oset module low-threshold lispbar-battery-low-threshold)
  (oset module critical-threshold lispbar-battery-critical-threshold)
  (oset module show-percentage lispbar-battery-show-percentage)
  (oset module show-time lispbar-battery-show-time)
  (oset module icons-charging lispbar-battery-icons-charging)
  (oset module icons-discharging lispbar-battery-icons-discharging)
  (oset module face lispbar-battery-face)
  (oset module low-face lispbar-battery-low-face)
  (oset module critical-face lispbar-battery-critical-face)
  (oset module charging-face lispbar-battery-charging-face)
  (oset module position lispbar-battery-position)
  (oset module priority lispbar-battery-priority)
  (oset module update-interval lispbar-battery-update-interval)
  (oset module cache-timeout lispbar-battery-cache-timeout)
  
  ;; Detect platform and select backend
  (let ((platform (lispbar-battery--detect-platform)))
    (oset module platform platform)
    (oset module backend (lispbar-battery--select-backend platform))
    (lispbar-modules--log 'info "Battery module configured for platform: %s" platform))
  
  ;; Update position list if position changed
  (lispbar-modules--add-to-position-list module))

;;; Module Creation and Management

(defun lispbar-battery--create-module ()
  "Create and configure a new battery module instance.
Returns the configured module instance."
  (let ((module (make-instance 'lispbar-battery-module
                               :name 'battery
                               :update-fn (lambda () 
                                          (lispbar-module-update lispbar-battery--module-instance))
                               :update-interval lispbar-battery-update-interval
                               :position lispbar-battery-position
                               :priority lispbar-battery-priority
                               :cache-timeout lispbar-battery-cache-timeout
                               :enabled t)))
    
    ;; Configure with current customization values
    (lispbar-battery-configure module)
    
    ;; Set the global instance
    (setq lispbar-battery--module-instance module)
    
    (lispbar-modules--log 'info "Battery module created")
    module))

;;;###autoload
(defun lispbar-battery-enable ()
  "Enable the Lispbar battery module.
Creates and registers the battery module if not already enabled."
  (interactive)
  (if lispbar-battery--enabled
      (lispbar-modules--log 'info "Battery module already enabled")
    (progn
      (lispbar-modules--log 'info "Enabling battery module")
      
      ;; Ensure module system is initialized
      (unless lispbar-modules--initialized
        (lispbar-modules-init))
      
      ;; Create and register module
      (let ((module (lispbar-battery--create-module)))
        (lispbar-modules-register module)
        (setq lispbar-battery--enabled t)
        (lispbar-modules--log 'info "Battery module enabled successfully")))))

;;;###autoload
(defun lispbar-battery-disable ()
  "Disable the Lispbar battery module.
Unregisters and cleans up the battery module."
  (interactive)
  (if (not lispbar-battery--enabled)
      (lispbar-modules--log 'info "Battery module not enabled")
    (progn
      (lispbar-modules--log 'info "Disabling battery module")
      
      ;; Unregister module
      (when lispbar-battery--module-instance
        (lispbar-modules-unregister 'battery))
      
      ;; Clean up
      (setq lispbar-battery--module-instance nil
            lispbar-battery--enabled nil
            lispbar-battery--last-percentage nil
            lispbar-battery--last-status nil)
      
      (lispbar-modules--log 'info "Battery module disabled"))))

;;;###autoload
(defun lispbar-battery-toggle ()
  "Toggle the Lispbar battery module on/off."
  (interactive)
  (if lispbar-battery--enabled
      (lispbar-battery-disable)
    (lispbar-battery-enable)))

;;;###autoload
(defun lispbar-battery-reconfigure ()
  "Reconfigure the battery module with current customization values.
Useful after changing battery customization options."
  (interactive)
  (when (and lispbar-battery--enabled lispbar-battery--module-instance)
    (lispbar-modules--log 'info "Reconfiguring battery module")
    (lispbar-battery-configure lispbar-battery--module-instance)
    (lispbar-modules-invalidate-cache 'battery)
    (lispbar-modules-update 'battery)
    (lispbar-modules--log 'info "Battery module reconfigured")))

;;; Interactive Commands

;;;###autoload
(defun lispbar-battery-set-format (format-string)
  "Set battery display FORMAT-STRING.
See `lispbar-battery-format' for available placeholders."
  (interactive 
   (list (read-string "Battery format: " lispbar-battery-format)))
  (setq lispbar-battery-format format-string)
  (when lispbar-battery--enabled
    (lispbar-battery-reconfigure))
  (message "Battery format updated"))

;;;###autoload
(defun lispbar-battery-set-thresholds (low critical)
  "Set battery warning thresholds to LOW and CRITICAL percentages."
  (interactive 
   (list (read-number "Low battery threshold (%): " lispbar-battery-low-threshold)
         (read-number "Critical battery threshold (%): " lispbar-battery-critical-threshold)))
  (setq lispbar-battery-low-threshold low
        lispbar-battery-critical-threshold critical)
  (when lispbar-battery--enabled
    (lispbar-battery-reconfigure))
  (message "Battery thresholds updated: low=%d%%, critical=%d%%" low critical))

;;;###autoload
(defun lispbar-battery-toggle-notifications ()
  "Toggle battery notifications on/off."
  (interactive)
  (setq lispbar-battery-enable-notifications (not lispbar-battery-enable-notifications))
  (message "Battery notifications: %s" 
           (if lispbar-battery-enable-notifications "enabled" "disabled")))

;;;###autoload
(defun lispbar-battery-test-notification ()
  "Send a test battery notification."
  (interactive)
  (lispbar-battery--send-notification 
   "Battery Test" 
   "This is a test notification from Lispbar Battery module."
   'normal))

;;; Status and Information Functions

(defun lispbar-battery-status ()
  "Display current status of the battery module.
Shows whether enabled, current battery info, thresholds, etc."
  (interactive)
  (if lispbar-battery--enabled
      (let* ((module lispbar-battery--module-instance)
             (backend (oref module backend))
             (battery-info (when backend (funcall backend)))
             (platform (oref module platform))
             (format-string (oref module format-string))
             (low-threshold (oref module low-threshold))
             (critical-threshold (oref module critical-threshold)))
        
        (if battery-info
            (let ((percentage (plist-get battery-info :percentage))
                  (status (plist-get battery-info :status))
                  (time-remaining (plist-get battery-info :time-remaining)))
              (message (concat "Battery module: ENABLED\n"
                              "Platform: %s\n"
                              "Current status: %s\n"
                              "Percentage: %s%%\n"
                              "Status: %s\n"
                              "Time remaining: %s\n"
                              "Thresholds: low=%d%%, critical=%d%%\n"
                              "Format: %s\n"
                              "Notifications: %s")
                      platform
                      (lispbar-battery--format-display module battery-info)
                      (or percentage "unknown")
                      (or status "unknown")
                      (if time-remaining 
                          (lispbar-battery--format-time time-remaining)
                        "unknown")
                      low-threshold critical-threshold
                      format-string
                      (if lispbar-battery-enable-notifications "enabled" "disabled")))
          (message (concat "Battery module: ENABLED\n"
                          "Platform: %s\n"
                          "Status: No battery information available")
                  platform)))
    (message "Battery module: DISABLED")))

;;; Module Cleanup

(defun lispbar-battery--cleanup ()
  "Clean up battery module resources.
Called during module system shutdown."
  (when lispbar-battery--enabled
    (lispbar-battery-disable)))

;; Register cleanup function
(eval-after-load 'lispbar-modules
  '(lispbar--add-cleanup-function #'lispbar-battery--cleanup))

;;; Provide

(provide 'lispbar-battery)
;;; lispbar-battery.el ends here