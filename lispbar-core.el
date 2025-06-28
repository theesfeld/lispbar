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

(defgroup lispbar-monitor nil
  "Customization group for Lispbar multi-monitor support."
  :group 'lispbar
  :prefix "lispbar-monitor-")

(defgroup lispbar-position nil
  "Customization group for Lispbar advanced positioning."
  :group 'lispbar
  :prefix "lispbar-position-")

(defcustom lispbar-position 'top
  "Position of the toolbar on screen."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Bottom" bottom)
                 (const :tag "Top with offset" top-offset)
                 (const :tag "Bottom with offset" bottom-offset)
                 (const :tag "Floating position" floating))
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

;;; Advanced Position Configuration

(defcustom lispbar-position-offset 0
  "Offset in pixels from screen edge for offset position modes.
Used when position is \\='top-offset or \\='bottom-offset."
  :type 'integer
  :group 'lispbar-position)

(defcustom lispbar-position-floating-x 100
  "X coordinate for floating position mode.
Used when position is \\='floating."
  :type 'integer
  :group 'lispbar-position)

(defcustom lispbar-position-floating-y 100
  "Y coordinate for floating position mode.
Used when position is \\='floating."
  :type 'integer
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-enabled nil
  "Enable auto-hide functionality for the toolbar.
When enabled, toolbar can automatically hide and show based on triggers."
  :type 'boolean
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-trigger 'timer
  "Trigger mechanism for auto-hide functionality.

Values:
- `timer': Hide after specified timeout when not focused
- `mouse': Show when mouse approaches, hide when it leaves
- `focus': Show when Emacs gains focus, hide when it loses focus
- `manual': Only show/hide via interactive commands"
  :type '(choice (const :tag "Timer-based" timer)
                 (const :tag "Mouse proximity" mouse)
                 (const :tag "Window focus" focus)
                 (const :tag "Manual control" manual))
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-timeout 3.0
  "Timeout in seconds before auto-hiding the toolbar.
Used when auto-hide-trigger is \\='timer."
  :type 'number
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-animation t
  "Enable slide animations for auto-hide functionality.
When enabled, toolbar slides in/out smoothly instead of instant show/hide."
  :type 'boolean
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-animation-duration 0.3
  "Duration in seconds for auto-hide slide animations."
  :type 'number
  :group 'lispbar-position)

(defcustom lispbar-auto-hide-reveal-pixels 2
  "Number of pixels to keep visible when toolbar is auto-hidden.
Set to 0 for complete hiding, positive values leave a visible edge."
  :type 'integer
  :group 'lispbar-position)

(defcustom lispbar-strut-enabled t
  "Enable strut reservation to prevent other windows from overlapping.
When disabled, other windows may overlap the toolbar area."
  :type 'boolean
  :group 'lispbar-position)

(defcustom lispbar-strut-conflict-resolution 'adjust
  "How to handle conflicts with existing window manager panels.

Values:
- `adjust': Automatically adjust position to avoid conflicts
- `override': Override existing struts (may cause overlaps)
- `disable': Disable strut reservation when conflicts detected"
  :type '(choice (const :tag "Adjust position" adjust)
                 (const :tag "Override existing" override)
                 (const :tag "Disable struts" disable))
  :group 'lispbar-position)

(defcustom lispbar-edge-snapping-enabled t
  "Enable automatic snapping to screen edges during interactive positioning."
  :type 'boolean
  :group 'lispbar-position)

(defcustom lispbar-edge-snapping-threshold 20
  "Distance in pixels for edge snapping to take effect."
  :type 'integer
  :group 'lispbar-position)

;;; Enhanced Multi-Monitor Customization

(defcustom lispbar-monitor-detection-method 'auto
  "Method to use for monitor detection.

Values:
- `auto': Automatically choose best available method
- `exwm-randr': Use EXWM RandR information (requires EXWM)
- `ewmh': Use EWMH properties via X11
- `xrandr': Execute xrandr command (requires xrandr binary)
- `fallback': Use simple display properties"
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "EXWM RandR" exwm-randr)
                 (const :tag "EWMH Properties" ewmh)
                 (const :tag "xrandr Command" xrandr)
                 (const :tag "Fallback" fallback))
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-cache-timeout 30.0
  "Time in seconds to cache monitor detection results.
Set to 0 to disable caching."
  :type 'number
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-hotplug-delay 1.0
  "Delay in seconds before processing monitor hotplug events.
This allows time for the system to stabilize after monitor changes."
  :type 'number
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-persistence-file
  (expand-file-name "lispbar-monitors.el" user-emacs-directory)
  "File to store persistent monitor configurations.
Set to nil to disable persistence."
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Configuration file"))
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-auto-configure t
  "Whether to automatically apply saved configurations to detected monitors."
  :type 'boolean
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-prefer-primary t
  "Whether to prefer primary monitor for default configuration.
When enabled, the primary monitor gets special treatment for positioning
and module placement."
  :type 'boolean
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-identification-method 'edid-name-resolution
  "Method for creating persistent monitor identifiers.

Values:
- `edid': Use EDID information (most reliable)
- `name': Use monitor name/model
- `name-resolution': Use name and resolution
- `edid-name-resolution': Try EDID, fall back to name+resolution
- `position': Use position coordinates (least reliable)"
  :type '(choice (const :tag "EDID only" edid)
                 (const :tag "Name only" name)
                 (const :tag "Name and resolution" name-resolution)
                 (const :tag "EDID with fallback" edid-name-resolution)
                 (const :tag "Position coordinates" position))
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-default-config
  '(:position top
    :height 28
    :modules-left nil
    :modules-center nil
    :modules-right nil
    :inherit-global t)
  "Default configuration applied to new monitors.

Plist keys:
- `:position' - toolbar position (top/bottom/top-offset/bottom-offset/floating)
- `:height' - toolbar height in pixels
- `:modules-left' - list of left-side modules
- `:modules-center' - list of center modules  
- `:modules-right' - list of right-side modules
- `:inherit-global' - whether to inherit global settings
- `:background-color' - monitor-specific background color
- `:foreground-color' - monitor-specific foreground color
- `:margin-left' - left margin in pixels
- `:margin-right' - right margin in pixels
- `:position-offset' - offset for offset position modes
- `:position-floating-x' - X coordinate for floating position
- `:position-floating-y' - Y coordinate for floating position
- `:auto-hide-enabled' - enable auto-hide for this monitor
- `:auto-hide-trigger' - auto-hide trigger mode
- `:auto-hide-timeout' - auto-hide timeout
- `:strut-enabled' - enable strut reservation"
  :type 'plist
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-frame-migration-strategy 'preserve-content
  "Strategy for handling frames when monitors are removed.

Values:
- `preserve-content': Move frame content to remaining monitors
- `destroy': Simply destroy frames for removed monitors
- `hide': Hide frames but keep them for potential monitor return"
  :type '(choice (const :tag "Preserve content" preserve-content)
                 (const :tag "Destroy frames" destroy)
                 (const :tag "Hide frames" hide))
  :group 'lispbar-monitor)

(defcustom lispbar-monitor-change-hook nil
  "Hook run when monitor configuration changes.
Functions should accept two arguments: OLD-MONITORS NEW-MONITORS."
  :type 'hook
  :group 'lispbar-monitor)

(defvar lispbar--frames nil
  "List of active Lispbar frames.")

(defvar lispbar--monitors nil
  "List of detected monitors with their geometry.")

(defvar lispbar--cleanup-functions nil
  "List of cleanup functions to call when disabling Lispbar.")

(defvar lispbar--initialized nil
  "Whether Lispbar has been initialized.")

;;; Enhanced Multi-Monitor Support Variables

(defvar lispbar--monitor-configurations nil
  "Alist mapping monitor identifiers to their configuration plists.
Each entry is (MONITOR-ID . CONFIG-PLIST) where CONFIG-PLIST contains
monitor-specific settings like position, height, modules, etc.")

(defvar lispbar--monitor-cache nil
  "Cached monitor information with timestamps for performance.
Stores comprehensive monitor data to avoid repeated expensive detection.")

(defvar lispbar--monitor-aliases nil
  "Alist mapping monitor identifiers to user-friendly aliases.
Format: ((MONITOR-ID . ALIAS-NAME) ...)")

(defvar lispbar--monitor-change-callbacks nil
  "List of functions to call when monitor configuration changes.
Each function should accept (OLD-MONITORS NEW-MONITORS) arguments.")

(defvar lispbar--last-monitor-configuration nil
  "Last known monitor configuration for change detection.")

(defvar lispbar--monitor-hotplug-timer nil
  "Timer for delayed monitor hotplug processing.")

(defvar lispbar--primary-monitor-id nil
  "Identifier of the primary monitor, if detected.")

;;; Advanced Position State Variables

(defvar lispbar--auto-hide-states nil
  "Alist mapping frame info to auto-hide state.
Each entry is (FRAME-INFO . STATE-PLIST) where STATE-PLIST contains:
- :visible - current visibility (t/nil)
- :timer - active hide timer
- :animation-timer - active animation timer
- :animation-progress - animation progress (0.0-1.0)
- :target-position - target position during animation")

(defvar lispbar--auto-hide-timers nil
  "List of active auto-hide timers.")

(defvar lispbar--mouse-tracking-timer nil
  "Timer for mouse proximity tracking.")

(defvar lispbar--strut-conflicts nil
  "Detected strut conflicts per monitor.
Alist mapping monitor-id to conflict information.")

(defvar lispbar--position-backups nil
  "Backup of original positions for conflict resolution.
Used to restore positions when conflicts are resolved.")

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

;;; Enhanced Monitor Management

(defun lispbar--detect-monitors ()
  "Detect available monitors with comprehensive properties.
Returns a list of plists with detailed monitor information including
name, EDID, resolution, position, primary status, and identification.
Uses caching to improve performance."
  (lispbar--log 'debug "Detecting monitors using method: %s" lispbar-monitor-detection-method)
  
  ;; Check cache first
  (let ((cached-result (lispbar--get-cached-monitors)))
    (if cached-result
        (progn
          (lispbar--log 'debug "Using cached monitor information")
          cached-result)
      
      ;; Perform fresh detection
      (condition-case err
          (let ((monitors (lispbar--perform-monitor-detection)))
            (lispbar--log 'info "Detected %d monitors" (length monitors))
            
            ;; Cache the results
            (lispbar--cache-monitors monitors)
            
            ;; Identify primary monitor
            (lispbar--identify-primary-monitor monitors)
            
            ;; Add persistent identifiers
            (mapcar #'lispbar--add-monitor-identifier monitors))
        
        (error
         (lispbar--log 'error "Monitor detection failed: %s" err)
         (list (lispbar--create-fallback-monitor)))))

(defun lispbar--perform-monitor-detection ()
  "Perform actual monitor detection based on configured method."
  (pcase lispbar-monitor-detection-method
    ('auto (lispbar--auto-detect-monitors))
    ('exwm-randr (lispbar--detect-via-exwm-randr))
    ('ewmh (lispbar--detect-via-ewmh))
    ('xrandr (lispbar--detect-via-xrandr))
    ('fallback (lispbar--fallback-monitor-detection))
    (_ (lispbar--auto-detect-monitors))))

(defun lispbar--auto-detect-monitors ()
  "Automatically choose the best monitor detection method available."
  (cond
   ;; Try EXWM RandR first (most comprehensive)
   ((and (boundp 'exwm-randr-workspace-monitor-plist)
         exwm-randr-workspace-monitor-plist
         (lispbar--exwm-randr-available-p))
    (lispbar--detect-via-exwm-randr))
   
   ;; Try xrandr command
   ((executable-find "xrandr")
    (lispbar--detect-via-xrandr))
   
   ;; Try EWMH properties
   ((display-graphic-p)
    (lispbar--detect-via-ewmh))
   
   ;; Fall back to basic detection
   (t (lispbar--fallback-monitor-detection))))

(defun lispbar--exwm-randr-available-p ()
  "Check if EXWM RandR is available and functional."
  (and (featurep 'exwm-randr)
       (boundp 'exwm-randr--monitor-geometry)
       exwm-randr--monitor-geometry))

(defun lispbar--detect-via-exwm-randr ()
  "Detect monitors using EXWM RandR with comprehensive properties."
  (lispbar--log 'debug "Detecting monitors via EXWM RandR")
  (let ((monitors '()))
    (when (and (boundp 'exwm-randr--monitor-geometry)
               exwm-randr--monitor-geometry)
      
      ;; Parse each monitor from RandR data
      (maphash (lambda (monitor-name geometry)
                 (let ((monitor-info (lispbar--parse-randr-monitor 
                                    monitor-name geometry)))
                   (when monitor-info
                     (push monitor-info monitors))))
               exwm-randr--monitor-geometry))
    
    (or monitors (list (lispbar--create-fallback-monitor)))))

(defun lispbar--parse-randr-monitor (monitor-name geometry)
  "Parse a single monitor from EXWM RandR data.
MONITOR-NAME is the RandR monitor name, GEOMETRY is (x y width height)."
  (when (and monitor-name geometry (= (length geometry) 4))
    (let* ((name (symbol-name monitor-name))
           (x (nth 0 geometry))
           (y (nth 1 geometry))
           (width (nth 2 geometry))
           (height (nth 3 geometry))
           (edid (lispbar--get-monitor-edid name))
           (properties (lispbar--get-monitor-properties name)))
      
      (list :name name
            :display-name (or (plist-get properties :display-name) name)
            :x x :y y :width width :height height
            :edid edid
            :primary (lispbar--is-monitor-primary name properties)
            :connected t
            :properties properties
            :source 'exwm-randr
            :timestamp (current-time)))))

(defun lispbar--detect-via-xrandr ()
  "Detect monitors using xrandr command."
  (lispbar--log 'debug "Detecting monitors via xrandr command")
  (condition-case err
      (let ((output (shell-command-to-string "xrandr --query")))
        (lispbar--parse-xrandr-output output))
    (error
     (lispbar--log 'error "xrandr detection failed: %s" err)
     (list (lispbar--create-fallback-monitor)))))

(defun lispbar--parse-xrandr-output (output)
  "Parse xrandr output into monitor information list."
  (let ((monitors '())
        (lines (split-string output "\n" t)))
    
    (dolist (line lines)
      (when (string-match "^\\([^ ]+\\) connected \\(primary \\)?\\([0-9]+\\)x\\([0-9]+\\)\\+\\([0-9]+\\)\\+\\([0-9]+\\)" line)
        (let* ((name (match-string 1 line))
               (primary (not (null (match-string 2 line))))
               (width (string-to-number (match-string 3 line)))
               (height (string-to-number (match-string 4 line)))
               (x (string-to-number (match-string 5 line)))
               (y (string-to-number (match-string 6 line)))
               (edid (lispbar--get-monitor-edid name)))
          
          (push (list :name name
                     :display-name name
                     :x x :y y :width width :height height
                     :edid edid
                     :primary primary
                     :connected t
                     :properties nil
                     :source 'xrandr
                     :timestamp (current-time))
                monitors))))
    
    (or monitors (list (lispbar--create-fallback-monitor)))))

(defun lispbar--detect-via-ewmh ()
  "Detect monitors using EWMH properties."
  (lispbar--log 'debug "Detecting monitors via EWMH properties")
  (condition-case err
      (let ((monitors (lispbar--parse-ewmh-workarea)))
        (or monitors (list (lispbar--create-fallback-monitor))))
    (error
     (lispbar--log 'error "EWMH detection failed: %s" err)
     (list (lispbar--create-fallback-monitor)))))

(defun lispbar--parse-ewmh-workarea ()
  "Parse EWMH workarea information into monitor list."
  (when (display-graphic-p)
    (let ((workarea (x-window-property "_NET_WORKAREA" nil "CARDINAL" 0 nil t)))
      (when (and workarea (vectorp workarea) (>= (length workarea) 4))
        (list (list :name "primary"
                   :display-name "Primary Monitor"
                   :x (aref workarea 0)
                   :y (aref workarea 1) 
                   :width (aref workarea 2)
                   :height (aref workarea 3)
                   :edid nil
                   :primary t
                   :connected t
                   :properties nil
                   :source 'ewmh
                   :timestamp (current-time)))))))

(defun lispbar--fallback-monitor-detection ()
  "Fallback monitor detection when advanced methods are not available."
  (lispbar--log 'debug "Using fallback monitor detection")
  (list (lispbar--create-fallback-monitor)))

;;; Monitor Caching System

(defun lispbar--get-cached-monitors ()
  "Get cached monitor information if still valid.
Returns nil if cache is invalid or expired."
  (when (and lispbar--monitor-cache
             (> lispbar-monitor-cache-timeout 0))
    (let* ((cache-time (plist-get lispbar--monitor-cache :timestamp))
           (age (float-time (time-subtract (current-time) cache-time))))
      (if (< age lispbar-monitor-cache-timeout)
          (plist-get lispbar--monitor-cache :monitors)
        (progn
          (lispbar--log 'debug "Monitor cache expired (age: %.1fs)" age)
          nil)))))

(defun lispbar--cache-monitors (monitors)
  "Cache MONITORS information with current timestamp."
  (when (> lispbar-monitor-cache-timeout 0)
    (setq lispbar--monitor-cache
          (list :monitors monitors
                :timestamp (current-time)))
    (lispbar--log 'debug "Cached %d monitors" (length monitors))))

(defun lispbar--invalidate-monitor-cache ()
  "Invalidate the monitor detection cache."
  (setq lispbar--monitor-cache nil)
  (lispbar--log 'debug "Monitor cache invalidated"))

;;; Monitor Identification and Properties

(defun lispbar--add-monitor-identifier (monitor)
  "Add persistent identifier to MONITOR based on configuration.
Returns monitor with :id property added."
  (let ((identifier (lispbar--generate-monitor-identifier monitor)))
    (plist-put monitor :id identifier)))

(defun lispbar--generate-monitor-identifier (monitor)
  "Generate persistent identifier for MONITOR.
Uses method specified by `lispbar-monitor-identification-method'."
  (let ((name (plist-get monitor :name))
        (width (plist-get monitor :width))
        (height (plist-get monitor :height))
        (x (plist-get monitor :x))
        (y (plist-get monitor :y))
        (edid (plist-get monitor :edid)))
    
    (pcase lispbar-monitor-identification-method
      ('edid
       (or edid (format "no-edid-%s" name)))
      
      ('name
       name)
      
      ('name-resolution
       (format "%s-%dx%d" name width height))
      
      ('edid-name-resolution
       (or edid (format "%s-%dx%d" name width height)))
      
      ('position
       (format "pos-%d-%d-%dx%d" x y width height))
      
      (_
       (format "%s-%dx%d" name width height)))))

(defun lispbar--get-monitor-edid (monitor-name)
  "Get EDID information for MONITOR-NAME if available.
Returns EDID string or nil if not available."
  (condition-case err
      (when (and monitor-name (display-graphic-p))
        ;; Try to get EDID via xrandr properties
        (let ((edid-output (shell-command-to-string 
                           (format "xrandr --props | grep -A1 '^%s' | grep EDID" monitor-name))))
          (when (string-match "EDID:\s*\n?\s*\\(.*\\)" edid-output)
            (string-trim (match-string 1 edid-output)))))
    (error
     (lispbar--log 'debug "Could not get EDID for %s: %s" monitor-name err)
     nil)))

(defun lispbar--get-monitor-properties (monitor-name)
  "Get additional properties for MONITOR-NAME.
Returns plist with available properties."
  (condition-case err
      (let ((properties '()))
        ;; Add brightness if available
        (let ((brightness (lispbar--get-monitor-brightness monitor-name)))
          (when brightness
            (setq properties (plist-put properties :brightness brightness))))
        
        ;; Add rotation if available  
        (let ((rotation (lispbar--get-monitor-rotation monitor-name)))
          (when rotation
            (setq properties (plist-put properties :rotation rotation))))
        
        properties)
    (error
     (lispbar--log 'debug "Could not get properties for %s: %s" monitor-name err)
     nil)))

(defun lispbar--get-monitor-brightness (monitor-name)
  "Get brightness level for MONITOR-NAME if available."
  (condition-case err
      (when (executable-find "xrandr")
        (let ((output (shell-command-to-string 
                      (format "xrandr --verbose | grep -A5 '^%s' | grep Brightness" monitor-name))))
          (when (string-match "Brightness:\s*\\([0-9.]+\\)" output)
            (string-to-number (match-string 1 output)))))
    (error nil)))

(defun lispbar--get-monitor-rotation (monitor-name)
  "Get rotation setting for MONITOR-NAME if available."
  (condition-case err
      (when (executable-find "xrandr")
        (let ((output (shell-command-to-string 
                      (format "xrandr --query | grep '^%s'" monitor-name))))
          (cond
           ((string-match "left" output) 'left)
           ((string-match "right" output) 'right)
           ((string-match "inverted" output) 'inverted)
           (t 'normal))))
    (error 'normal)))

(defun lispbar--is-monitor-primary (monitor-name properties)
  "Check if MONITOR-NAME is the primary monitor.
Uses PROPERTIES and system queries to determine primary status."
  (or
   ;; Check if already marked as primary in properties
   (plist-get properties :primary)
   
   ;; Check via EWMH if available
   (lispbar--is-primary-via-ewmh monitor-name)
   
   ;; Check via xrandr if available
   (lispbar--is-primary-via-xrandr monitor-name)
   
   ;; Default: first monitor is primary
   nil))

(defun lispbar--is-primary-via-ewmh (monitor-name)
  "Check if MONITOR-NAME is primary via EWMH properties."
  (when (display-graphic-p)
    (condition-case err
        (let ((primary (x-window-property "_NET_PRIMARY_MONITOR" nil "CARDINAL" 0 nil t)))
          ;; This is a simplified check - real implementation would need
          ;; to correlate the primary monitor index with the monitor list
          (and primary (vectorp primary) (> (aref primary 0) -1)))
      (error nil))))

(defun lispbar--is-primary-via-xrandr (monitor-name)
  "Check if MONITOR-NAME is primary via xrandr."
  (condition-case err
      (when (executable-find "xrandr")
        (let ((output (shell-command-to-string 
                      (format "xrandr --query | grep '^%s.*primary'" monitor-name))))
          (not (string-empty-p output))))
    (error nil)))

(defun lispbar--identify-primary-monitor (monitors)
  "Identify and cache the primary monitor from MONITORS list."
  (let ((primary (cl-find-if (lambda (m) (plist-get m :primary)) monitors)))
    (setq lispbar--primary-monitor-id 
          (if primary 
              (plist-get primary :id)
            ;; Fall back to first monitor
            (plist-get (car monitors) :id)))
    (lispbar--log 'debug "Primary monitor identified: %s" lispbar--primary-monitor-id)))

(defun lispbar--create-fallback-monitor ()
  "Create fallback monitor information when detection fails."
  (list :name "fallback"
        :display-name "Fallback Monitor"
        :x 0 :y 0
        :width (display-pixel-width)
        :height (display-pixel-height)
        :edid nil
        :primary t
        :connected t
        :properties nil
        :source 'fallback
        :timestamp (current-time)))

(defun lispbar--refresh-monitors ()
  "Refresh monitor information and update frames accordingly.
Detects changes and triggers appropriate reconfiguration."
  (lispbar--log 'debug "Refreshing monitor information")
  
  ;; Store old configuration for comparison
  (let ((old-monitors lispbar--monitors))
    
    ;; Invalidate cache and detect fresh
    (lispbar--invalidate-monitor-cache)
    (setq lispbar--monitors (lispbar--detect-monitors))
    
    ;; Check for changes and handle accordingly
    (unless (lispbar--monitors-equal-p old-monitors lispbar--monitors)
      (lispbar--log 'info "Monitor configuration changed")
      
      ;; Store change information
      (setq lispbar--last-monitor-configuration old-monitors)
      
      ;; Handle the change
      (lispbar--handle-monitor-configuration-change old-monitors lispbar--monitors)
      
      ;; Run change callbacks
      (lispbar--run-monitor-change-callbacks old-monitors lispbar--monitors)
      
      ;; Run hook
      (run-hook-with-args 'lispbar-monitor-change-hook old-monitors lispbar--monitors))
    
    ;; Always update frame positions (geometry might have changed)
    (when lispbar--frames
      (lispbar--update-frame-positions))))

;;; Monitor Configuration System

(defun lispbar--get-monitor-config (monitor-id)
  "Get configuration for MONITOR-ID.
Returns merged configuration from monitor-specific, default, and global settings."
  (let* ((monitor-config (cdr (assoc monitor-id lispbar--monitor-configurations)))
         (default-config (copy-sequence lispbar-monitor-default-config))
         (merged-config (lispbar--merge-monitor-configs default-config monitor-config)))
    
    ;; Apply global inheritance if enabled
    (when (plist-get merged-config :inherit-global)
      (setq merged-config (lispbar--apply-global-config-inheritance merged-config)))
    
    merged-config))

(defun lispbar--set-monitor-config (monitor-id config)
  "Set configuration for MONITOR-ID to CONFIG.
CONFIG should be a plist with monitor-specific settings."
  (setq lispbar--monitor-configurations
        (cons (cons monitor-id config)
              (assoc-delete-all monitor-id lispbar--monitor-configurations)))
  
  (lispbar--log 'debug "Updated configuration for monitor: %s" monitor-id)
  
  ;; Save to persistence file if enabled
  (when lispbar-monitor-persistence-file
    (lispbar--save-monitor-configurations)))

(defun lispbar--merge-monitor-configs (base-config override-config)
  "Merge BASE-CONFIG with OVERRIDE-CONFIG.
OVERRIDE-CONFIG values take precedence over BASE-CONFIG."
  (let ((result (copy-sequence base-config)))
    (cl-loop for (key value) on override-config by #'cddr
             do (setq result (plist-put result key value)))
    result))

(defun lispbar--apply-global-config-inheritance (config)
  "Apply global configuration inheritance to CONFIG.
Uses global lispbar settings for unspecified monitor settings."
  (let ((result (copy-sequence config)))
    
    ;; Inherit global position if not specified
    (unless (plist-member result :position)
      (setq result (plist-put result :position lispbar-position)))
    
    ;; Inherit global height if not specified
    (unless (plist-member result :height)
      (setq result (plist-put result :height lispbar-height)))
    
    ;; Inherit global colors if not specified
    (unless (plist-member result :background-color)
      (setq result (plist-put result :background-color lispbar-background-color)))
    
    (unless (plist-member result :foreground-color)
      (setq result (plist-put result :foreground-color lispbar-foreground-color)))
    
    ;; Inherit global margins if not specified
    (unless (plist-member result :margin-left)
      (setq result (plist-put result :margin-left lispbar-margin-left)))
    
    (unless (plist-member result :margin-right)
      (setq result (plist-put result :margin-right lispbar-margin-right)))
    
    ;; Inherit advanced position settings if not specified
    (unless (plist-member result :position-offset)
      (setq result (plist-put result :position-offset lispbar-position-offset)))
    
    (unless (plist-member result :position-floating-x)
      (setq result (plist-put result :position-floating-x lispbar-position-floating-x)))
    
    (unless (plist-member result :position-floating-y)
      (setq result (plist-put result :position-floating-y lispbar-position-floating-y)))
    
    ;; Inherit auto-hide settings if not specified
    (unless (plist-member result :auto-hide-enabled)
      (setq result (plist-put result :auto-hide-enabled lispbar-auto-hide-enabled)))
    
    (unless (plist-member result :auto-hide-trigger)
      (setq result (plist-put result :auto-hide-trigger lispbar-auto-hide-trigger)))
    
    (unless (plist-member result :auto-hide-timeout)
      (setq result (plist-put result :auto-hide-timeout lispbar-auto-hide-timeout)))
    
    (unless (plist-member result :auto-hide-animation)
      (setq result (plist-put result :auto-hide-animation lispbar-auto-hide-animation)))
    
    ;; Inherit strut settings if not specified
    (unless (plist-member result :strut-enabled)
      (setq result (plist-put result :strut-enabled lispbar-strut-enabled)))
    
    result))

(defun lispbar--auto-configure-monitors ()
  "Automatically configure detected monitors.
Applies saved configurations or defaults to new monitors."
  (when lispbar-monitor-auto-configure
    (dolist (monitor lispbar--monitors)
      (let* ((monitor-id (plist-get monitor :id))
             (existing-config (cdr (assoc monitor-id lispbar--monitor-configurations))))
        
        ;; Apply default configuration if no existing config
        (unless existing-config
          (lispbar--log 'debug "Auto-configuring new monitor: %s" monitor-id)
          (lispbar--set-monitor-config monitor-id 
                                       (lispbar--create-default-monitor-config monitor)))))))

(defun lispbar--create-default-monitor-config (monitor)
  "Create default configuration for MONITOR.
Takes into account monitor properties like primary status."
  (let ((config (copy-sequence lispbar-monitor-default-config))
        (is-primary (plist-get monitor :primary)))
    
    ;; Primary monitor gets enhanced default configuration
    (when (and is-primary lispbar-monitor-prefer-primary)
      (setq config (plist-put config :priority 100))
      
      ;; Primary monitor could get different module layout
      (unless (plist-get config :modules-center)
        (setq config (plist-put config :modules-center '(clock)))))
    
    config))

;;; Monitor Configuration Persistence

(defun lispbar--save-monitor-configurations ()
  "Save monitor configurations to persistence file."
  (when lispbar-monitor-persistence-file
    (condition-case err
        (with-temp-file lispbar-monitor-persistence-file
          (insert ";; Lispbar monitor configurations\n")
          (insert ";; Generated automatically - do not edit directly\n\n")
          (insert (format "(setq lispbar--monitor-configurations\n      '%S)\n"
                         lispbar--monitor-configurations))
          (insert (format "\n(setq lispbar--monitor-aliases\n      '%S)\n"
                         lispbar--monitor-aliases)))
      (error
       (lispbar--log 'error "Failed to save monitor configurations: %s" err)))))

(defun lispbar--load-monitor-configurations ()
  "Load monitor configurations from persistence file."
  (when (and lispbar-monitor-persistence-file
             (file-exists-p lispbar-monitor-persistence-file))
    (condition-case err
        (progn
          (load lispbar-monitor-persistence-file t t)
          (lispbar--log 'info "Loaded monitor configurations from %s" 
                        lispbar-monitor-persistence-file))
      (error
       (lispbar--log 'error "Failed to load monitor configurations: %s" err)))))

;;; Monitor Change Handling

(defun lispbar--monitors-equal-p (old-monitors new-monitors)
  "Check if OLD-MONITORS and NEW-MONITORS represent the same configuration."
  (and (= (length old-monitors) (length new-monitors))
       (cl-every (lambda (old-monitor)
                   (cl-find-if (lambda (new-monitor)
                                 (lispbar--monitor-equal-p old-monitor new-monitor))
                               new-monitors))
                 old-monitors)))

(defun lispbar--monitor-equal-p (monitor1 monitor2)
  "Check if MONITOR1 and MONITOR2 represent the same monitor."
  (let ((id1 (plist-get monitor1 :id))
        (id2 (plist-get monitor2 :id))
        (geom1 (list (plist-get monitor1 :x) (plist-get monitor1 :y)
                    (plist-get monitor1 :width) (plist-get monitor1 :height)))
        (geom2 (list (plist-get monitor2 :x) (plist-get monitor2 :y)
                    (plist-get monitor2 :width) (plist-get monitor2 :height))))
    (and (equal id1 id2) (equal geom1 geom2))))

(defun lispbar--handle-monitor-configuration-change (old-monitors new-monitors)
  "Handle monitor configuration change from OLD-MONITORS to NEW-MONITORS."
  (lispbar--log 'info "Handling monitor configuration change")
  
  ;; Identify added and removed monitors
  (let ((added (lispbar--find-added-monitors old-monitors new-monitors))
        (removed (lispbar--find-removed-monitors old-monitors new-monitors)))
    
    ;; Handle removed monitors
    (when removed
      (lispbar--log 'info "Removed monitors: %s" 
                    (mapcar (lambda (m) (plist-get m :id)) removed))
      (lispbar--handle-removed-monitors removed))
    
    ;; Handle added monitors
    (when added
      (lispbar--log 'info "Added monitors: %s" 
                    (mapcar (lambda (m) (plist-get m :id)) added))
      (lispbar--handle-added-monitors added))
    
    ;; Auto-configure new monitors
    (lispbar--auto-configure-monitors)))

(defun lispbar--find-added-monitors (old-monitors new-monitors)
  "Find monitors in NEW-MONITORS that are not in OLD-MONITORS."
  (cl-remove-if (lambda (new-monitor)
                  (cl-find-if (lambda (old-monitor)
                                (equal (plist-get new-monitor :id)
                                      (plist-get old-monitor :id)))
                              old-monitors))
                new-monitors))

(defun lispbar--find-removed-monitors (old-monitors new-monitors)
  "Find monitors in OLD-MONITORS that are not in NEW-MONITORS."
  (cl-remove-if (lambda (old-monitor)
                  (cl-find-if (lambda (new-monitor)
                                (equal (plist-get old-monitor :id)
                                      (plist-get new-monitor :id)))
                              new-monitors))
                old-monitors))

(defun lispbar--handle-removed-monitors (removed-monitors)
  "Handle REMOVED-MONITORS according to migration strategy."
  (dolist (monitor removed-monitors)
    (let ((monitor-id (plist-get monitor :id)))
      
      ;; Find and handle frames for this monitor
      (let ((affected-frames (cl-remove-if-not 
                             (lambda (frame-info)
                               (equal (plist-get frame-info :monitor) monitor-id))
                             lispbar--frames)))
        
        (pcase lispbar-monitor-frame-migration-strategy
          ('preserve-content
           (lispbar--migrate-frame-content affected-frames))
          ('hide
           (lispbar--hide-frames affected-frames))
          ('destroy
           (lispbar--destroy-frames affected-frames)))))))

(defun lispbar--handle-added-monitors (added-monitors)
  "Handle ADDED-MONITORS by creating appropriate frames."
  (dolist (monitor added-monitors)
    (let* ((monitor-id (plist-get monitor :id))
           (config (lispbar--get-monitor-config monitor-id)))
      
      ;; Create frame for new monitor if configured
      (when (plist-get config :enabled)
        (lispbar--create-frame-for-monitor monitor config)))))

(defun lispbar--run-monitor-change-callbacks (old-monitors new-monitors)
  "Run registered monitor change callbacks with OLD-MONITORS and NEW-MONITORS."
  (dolist (callback lispbar--monitor-change-callbacks)
    (condition-case err
        (funcall callback old-monitors new-monitors)
      (error
       (lispbar--log 'error "Monitor change callback failed: %s" err)))))

;;; Enhanced Frame Geometry Calculation

(defun lispbar--calculate-frame-geometry (monitor &optional config)
  "Calculate frame geometry for MONITOR using CONFIG.
CONFIG defaults to the monitor's saved configuration.
Returns a plist with :x, :y, :width, and :height."
  (let* ((monitor-id (plist-get monitor :id))
         (effective-config (or config (lispbar--get-monitor-config monitor-id)))
         (monitor-x (plist-get monitor :x))
         (monitor-y (plist-get monitor :y))
         (monitor-width (plist-get monitor :width))
         (monitor-height (plist-get monitor :height))
         (position (plist-get effective-config :position))
         (height (plist-get effective-config :height))
         (margin-left (or (plist-get effective-config :margin-left) 0))
         (margin-right (or (plist-get effective-config :margin-right) 0))
         (position-offset (or (plist-get effective-config :position-offset) 
                              lispbar-position-offset))
         (floating-x (or (plist-get effective-config :position-floating-x)
                         lispbar-position-floating-x))
         (floating-y (or (plist-get effective-config :position-floating-y)
                         lispbar-position-floating-y))
         (frame-width (- monitor-width margin-left margin-right))
         (frame-x (+ monitor-x margin-left))
         (frame-y (lispbar--calculate-frame-y monitor-y monitor-height height 
                                             position position-offset floating-y)))
    
    ;; Handle floating position special case for X coordinate
    (when (eq position 'floating)
      (setq frame-x (+ monitor-x floating-x)
            frame-width (min frame-width (- monitor-width floating-x margin-right))))
    
    ;; Apply edge snapping if enabled and position is floating
    (when (and (eq position 'floating) lispbar-edge-snapping-enabled)
      (let ((snapped-coords (lispbar--apply-edge-snapping 
                             frame-x frame-y frame-width height monitor)))
        (setq frame-x (car snapped-coords)
              frame-y (cadr snapped-coords))))
    
    (list :x frame-x
          :y frame-y
          :width frame-width
          :height height
          :monitor-id monitor-id
          :config effective-config
          :position position)))

(defun lispbar--calculate-frame-y (monitor-y monitor-height height position offset floating-y)
  "Calculate Y coordinate for frame based on position mode.
MONITOR-Y and MONITOR-HEIGHT define the monitor area.
HEIGHT is the frame height, POSITION is the position mode,
OFFSET is the offset for offset modes, FLOATING-Y is Y for floating mode."
  (cl-case position
    (top monitor-y)
    (bottom (- (+ monitor-y monitor-height) height))
    (top-offset (+ monitor-y offset))
    (bottom-offset (- (+ monitor-y monitor-height) height offset))
    (floating (+ monitor-y floating-y))
    (t monitor-y)))  ; fallback to top

(defun lispbar--apply-edge-snapping (x y width height monitor)
  "Apply edge snapping to coordinates X, Y for frame with WIDTH, HEIGHT.
MONITOR contains the monitor geometry.
Returns list (new-x new-y) with snapped coordinates."
  (let* ((monitor-x (plist-get monitor :x))
         (monitor-y (plist-get monitor :y))
         (monitor-width (plist-get monitor :width))
         (monitor-height (plist-get monitor :height))
         (monitor-right (+ monitor-x monitor-width))
         (monitor-bottom (+ monitor-y monitor-height))
         (threshold lispbar-edge-snapping-threshold)
         (new-x x)
         (new-y y))
    
    ;; Snap to left edge
    (when (< (abs (- x monitor-x)) threshold)
      (setq new-x monitor-x))
    
    ;; Snap to right edge
    (when (< (abs (- (+ x width) monitor-right)) threshold)
      (setq new-x (- monitor-right width)))
    
    ;; Snap to top edge
    (when (< (abs (- y monitor-y)) threshold)
      (setq new-y monitor-y))
    
    ;; Snap to bottom edge
    (when (< (abs (- (+ y height) monitor-bottom)) threshold)
      (setq new-y (- monitor-bottom height)))
    
    (list new-x new-y)))

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

;;; Enhanced Frame Creation and Management

(defun lispbar--create-frame (geometry &optional monitor-name config)
  "Create a new Lispbar frame with GEOMETRY for MONITOR-NAME using CONFIG.
CONFIG contains monitor-specific frame configuration."
  (lispbar--log 'debug "Creating frame for monitor %s with geometry %S" 
                monitor-name geometry)
  (let* ((validated-geometry (lispbar--validate-geometry geometry))
         (frame-params (lispbar--build-frame-parameters validated-geometry config))
         (frame (make-frame frame-params)))
    (when frame
      (lispbar--configure-frame frame validated-geometry config)
      (lispbar--log 'info "Created frame for monitor %s" monitor-name)
      frame)))

(defun lispbar--create-frame-for-monitor (monitor config)
  "Create a frame specifically for MONITOR using CONFIG."
  (let* ((geometry (lispbar--calculate-frame-geometry monitor config))
         (monitor-id (plist-get monitor :id))
         (frame (lispbar--create-frame geometry monitor-id config)))
    (when frame
      (let ((frame-info (list :frame frame
                             :monitor monitor-id
                             :geometry geometry
                             :config config)))
        (push frame-info lispbar--frames)
        
        ;; Initialize auto-hide if enabled
        (lispbar--init-auto-hide-for-frame frame-info)
        
        frame))))

(defun lispbar--build-frame-parameters (geometry &optional config)
  "Build frame parameters list from GEOMETRY and CONFIG."
  (let* ((monitor-id (plist-get geometry :monitor-id))
         (frame-name (if monitor-id 
                        (format "Lispbar-%s" monitor-id)
                      "Lispbar"))
         (params `((name . ,frame-name)
                   (title . ,frame-name)
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
    
    ;; Apply color configuration
    (let ((bg-color (if config 
                       (plist-get config :background-color)
                     lispbar-background-color))
          (fg-color (if config
                       (plist-get config :foreground-color)
                     lispbar-foreground-color)))
      (when bg-color
        (push `(background-color . ,bg-color) params))
      (when fg-color
        (push `(foreground-color . ,fg-color) params)))
    
    params))

(defun lispbar--configure-frame (frame geometry &optional config)
  "Configure FRAME after creation with GEOMETRY and CONFIG."
  (with-selected-frame frame
    ;; Set frame to be sticky and always on top
    (when (fboundp 'x-change-window-property)
      ;; Set window type to dock for docked positions, normal for floating
      (let ((position (plist-get geometry :position)))
        (if (eq position 'floating)
            (x-change-window-property 
             "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NORMAL" 
             frame nil 'ATOM 32 t)
          (x-change-window-property 
           "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK" 
           frame nil 'ATOM 32 t)))
      
      ;; Set window to be sticky (visible on all workspaces)
      (x-change-window-property 
       "_NET_WM_DESKTOP" 0xFFFFFFFF frame nil 'CARDINAL 32 t)
      
      ;; Handle strut reservation for non-floating positions
      (lispbar--configure-frame-struts frame geometry config))))

(defun lispbar--configure-frame-struts (frame geometry config)
  "Configure strut reservation for FRAME with GEOMETRY and CONFIG."
  (let* ((position (plist-get geometry :position))
         (height (plist-get geometry :height))
         (strut-enabled (if config
                           (plist-get config :strut-enabled)
                         lispbar-strut-enabled)))
    
    ;; Only set struts for non-floating positions and if enabled
    (when (and strut-enabled 
               (not (eq position 'floating))
               (fboundp 'x-change-window-property))
      
      ;; Handle strut conflicts based on resolution strategy
      (cl-case lispbar-strut-conflict-resolution
        (adjust
         ;; Try to adjust position to avoid conflicts
         (let ((conflict-detected (lispbar--detect-strut-conflicts geometry)))
           (if conflict-detected
               (progn
                 (lispbar--log 'info "Strut conflict detected, adjusting position")
                 (let ((adjusted-geometry (lispbar--resolve-strut-conflict geometry)))
                   (when adjusted-geometry
                     (lispbar--set-frame-struts frame adjusted-geometry position height))))
             (lispbar--set-frame-struts frame geometry position height))))
        (override
         ;; Set struts regardless of conflicts
         (lispbar--set-frame-struts frame geometry position height))
        (disable
         ;; Only set struts if no conflicts detected
         (unless (lispbar--detect-strut-conflicts geometry)
           (lispbar--set-frame-struts frame geometry position height)))))))

(defun lispbar--set-frame-struts (frame geometry position height)
  "Set strut properties for FRAME based on GEOMETRY, POSITION and HEIGHT."
  (let ((x (plist-get geometry :x))
        (width (plist-get geometry :width)))
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
        frame nil 'CARDINAL 32 t)))))

(defun lispbar--detect-strut-conflicts (geometry)
  "Detect if GEOMETRY would conflict with existing struts.
Returns t if conflicts detected, nil otherwise."
  ;; This is a simplified implementation - could be enhanced to actually
  ;; read existing _NET_WM_STRUT_PARTIAL properties from other windows
  (condition-case nil
      (let ((existing-struts (lispbar--get-existing-struts)))
        (lispbar--geometry-conflicts-with-struts-p geometry existing-struts))
    (error nil)))

(defun lispbar--get-existing-struts ()
  "Get existing strut reservations from window manager.
Returns list of strut data for conflict detection."
  (condition-case nil
      (when (fboundp 'x-window-property)
        ;; Query all windows for _NET_WM_STRUT_PARTIAL properties
        (let ((struts '())
              (all-windows (x-window-property "_NET_CLIENT_LIST" nil "WINDOW" 0 nil t)))
          (when all-windows
            (dolist (window all-windows)
              (let ((strut (x-window-property "_NET_WM_STRUT_PARTIAL" window "CARDINAL" 0 nil)))
                (when strut
                  (push strut struts)))))
          struts))
    (error nil)))

(defun lispbar--geometry-conflicts-with-struts-p (geometry struts)
  "Check if GEOMETRY conflicts with existing STRUTS.
Returns t if conflicts detected."
  (when struts
    (let* ((x (plist-get geometry :x))
           (y (plist-get geometry :y))
           (width (plist-get geometry :width))
           (height (plist-get geometry :height))
           (right (+ x width))
           (bottom (+ y height)))
      (cl-some (lambda (strut)
                 ;; strut format: [left right top bottom left_start left_end 
                 ;;                right_start right_end top_start top_end 
                 ;;                bottom_start bottom_end]
                 (when (and (vectorp strut) (>= (length strut) 12))
                   (let ((s-top (aref strut 2))
                         (s-bottom (aref strut 3))
                         (s-top-start (aref strut 8))
                         (s-top-end (aref strut 9))
                         (s-bottom-start (aref strut 10))
                         (s-bottom-end (aref strut 11)))
                     ;; Check for conflicts with top/bottom struts
                     (or (and (> s-top 0)
                              (<= y s-top)
                              (>= (+ y height) s-top)
                              (not (or (>= x s-top-end) (<= right s-top-start))))
                         (and (> s-bottom 0)
                              (<= (- (display-pixel-height) s-bottom) bottom)
                              (>= (- (display-pixel-height) s-bottom) y)
                              (not (or (>= x s-bottom-end) (<= right s-bottom-start))))))))
               struts))))

(defun lispbar--resolve-strut-conflict (geometry)
  "Attempt to resolve strut conflict by adjusting GEOMETRY.
Returns adjusted geometry or nil if no resolution possible."
  (let* ((existing-struts (lispbar--get-existing-struts))
         (x (plist-get geometry :x))
         (y (plist-get geometry :y))
         (width (plist-get geometry :width))
         (height (plist-get geometry :height))
         (adjusted-y y))
    
    ;; Find the highest conflicting top strut
    (dolist (strut existing-struts)
      (when (and (vectorp strut) (>= (length strut) 12))
        (let ((s-top (aref strut 2))
              (s-top-start (aref strut 8))
              (s-top-end (aref strut 9)))
          ;; If this top strut conflicts with our horizontal position
          (when (and (> s-top 0)
                     (not (or (>= x s-top-end) (<= (+ x width) s-top-start))))
            (setq adjusted-y (max adjusted-y (+ s-top 5)))))))  ; 5px buffer
    
    ;; Return adjusted geometry if we found a better position
    (if (> adjusted-y y)
        (progn
          (lispbar--log 'debug "Adjusted toolbar position to avoid conflicts: y=%d" adjusted-y)
          (plist-put (copy-sequence geometry) :y adjusted-y))
      nil)))

(defun lispbar--create-frames ()
  "Create Lispbar frames for all detected monitors with their configurations."
  (lispbar--log 'debug "Creating frames for %d monitors" 
                (length lispbar--monitors))
  (setq lispbar--frames nil)
  
  ;; Load any persistent configurations first
  (lispbar--load-monitor-configurations)
  
  ;; Auto-configure monitors
  (lispbar--auto-configure-monitors)
  
  (dolist (monitor lispbar--monitors)
    (let* ((monitor-id (plist-get monitor :id))
           (config (lispbar--get-monitor-config monitor-id))
           (geometry (lispbar--calculate-frame-geometry monitor config))
           (frame (lispbar--create-frame geometry monitor-id config)))
      (when frame
        (push (list :frame frame 
                   :monitor monitor-id
                   :geometry geometry
                   :config config) 
              lispbar--frames)))))

(defun lispbar--update-frame-positions ()
  "Update positions of existing frames based on current monitor configuration."
  (lispbar--log 'debug "Updating frame positions")
  (when lispbar--frames
    (dolist (frame-info lispbar--frames)
      (let* ((frame (plist-get frame-info :frame))
             (monitor-id (plist-get frame-info :monitor))
             (monitor (cl-find-if (lambda (m) 
                                    (equal (plist-get m :id) monitor-id))
                                  lispbar--monitors)))
        (when (and frame monitor (frame-live-p frame))
          (let* ((config (lispbar--get-monitor-config monitor-id))
                 (new-geometry (lispbar--calculate-frame-geometry monitor config)))
            (lispbar--log 'debug "Updating frame position for monitor %s" monitor-id)
            (modify-frame-parameters 
             frame 
             `((left . ,(plist-get new-geometry :x))
               (top . ,(plist-get new-geometry :y))
               (width . ,(plist-get new-geometry :width))
               (height . ,(plist-get new-geometry :height))))
            
            ;; Update stored information
            (setf (plist-get frame-info :geometry) new-geometry)
            (setf (plist-get frame-info :config) config)))))))

;;; Auto-Hide System

(defun lispbar--init-auto-hide-for-frame (frame-info)
  "Initialize auto-hide functionality for FRAME-INFO if enabled."
  (let* ((config (plist-get frame-info :config))
         (auto-hide-enabled (or (plist-get config :auto-hide-enabled)
                                lispbar-auto-hide-enabled)))
    (when auto-hide-enabled
      (lispbar--log 'debug "Initializing auto-hide for frame")
      
      ;; Initialize auto-hide state
      (let ((state (list :visible t
                         :timer nil
                         :animation-timer nil
                         :animation-progress 0.0
                         :target-position nil)))
        (setq lispbar--auto-hide-states
              (cons (cons frame-info state)
                    (assoc-delete-all frame-info lispbar--auto-hide-states))))
      
      ;; Set up triggers based on configuration
      (let ((trigger (or (plist-get config :auto-hide-trigger)
                         lispbar-auto-hide-trigger)))
        (lispbar--setup-auto-hide-triggers frame-info trigger)))))

(defun lispbar--setup-auto-hide-triggers (frame-info trigger)
  "Set up auto-hide triggers for FRAME-INFO based on TRIGGER type."
  (cl-case trigger
    (timer
     (lispbar--start-auto-hide-timer frame-info))
    (mouse
     (lispbar--start-mouse-tracking frame-info))
    (focus
     (lispbar--setup-focus-tracking frame-info))
    (manual
     ;; No automatic triggers for manual mode
     nil)))

(defun lispbar--start-auto-hide-timer (frame-info)
  "Start auto-hide timer for FRAME-INFO."
  (let* ((config (plist-get frame-info :config))
         (timeout (or (plist-get config :auto-hide-timeout)
                      lispbar-auto-hide-timeout)))
    (when (> timeout 0)
      (let ((timer (run-with-timer timeout nil
                                   #'lispbar--auto-hide-frame frame-info)))
        (lispbar--set-auto-hide-state frame-info :timer timer)
        (push timer lispbar--auto-hide-timers)))))

(defun lispbar--start-mouse-tracking (frame-info)
  "Start mouse proximity tracking for FRAME-INFO."
  ;; Set up a timer to periodically check mouse position
  (unless lispbar--mouse-tracking-timer
    (setq lispbar--mouse-tracking-timer
          (run-with-timer 0.1 0.1 #'lispbar--check-mouse-proximity))))

(defun lispbar--setup-focus-tracking (frame-info)
  "Set up focus-based auto-hide for FRAME-INFO."
  ;; Add focus change hooks
  (add-hook 'focus-in-hook #'lispbar--on-focus-in)
  (add-hook 'focus-out-hook #'lispbar--on-focus-out))

(defun lispbar--check-mouse-proximity ()
  "Check mouse proximity to auto-hide frames and show/hide accordingly."
  (when (display-graphic-p)
    (condition-case nil
        (let ((mouse-pos (mouse-position)))
          (when mouse-pos
            (dolist (frame-state-pair lispbar--auto-hide-states)
              (let* ((frame-info (car frame-state-pair))
                     (state (cdr frame-state-pair))
                     (frame (plist-get frame-info :frame)))
                (when (and frame (frame-live-p frame))
                  (let ((geometry (plist-get frame-info :geometry))
                        (proximity-threshold 50))  ; pixels
                    (if (lispbar--mouse-near-frame-p mouse-pos geometry proximity-threshold)
                        (lispbar--show-frame frame-info)
                      (lispbar--auto-hide-frame frame-info))))))))
      (error nil))))

(defun lispbar--mouse-near-frame-p (mouse-pos geometry threshold)
  "Check if mouse position is near frame defined by GEOMETRY within THRESHOLD."
  (let* ((mouse-x (cadr mouse-pos))
         (mouse-y (cddr mouse-pos))
         (frame-x (plist-get geometry :x))
         (frame-y (plist-get geometry :y))
         (frame-width (plist-get geometry :width))
         (frame-height (plist-get geometry :height)))
    (and mouse-x mouse-y
         (>= mouse-x (- frame-x threshold))
         (<= mouse-x (+ frame-x frame-width threshold))
         (>= mouse-y (- frame-y threshold))
         (<= mouse-y (+ frame-y frame-height threshold)))))

(defun lispbar--on-focus-in ()
  "Handle Emacs gaining focus for auto-hide frames."
  (dolist (frame-state-pair lispbar--auto-hide-states)
    (let ((frame-info (car frame-state-pair)))
      (lispbar--show-frame frame-info))))

(defun lispbar--on-focus-out ()
  "Handle Emacs losing focus for auto-hide frames."
  (dolist (frame-state-pair lispbar--auto-hide-states)
    (let ((frame-info (car frame-state-pair)))
      (lispbar--auto-hide-frame frame-info))))

(defun lispbar--show-frame (frame-info)
  "Show auto-hide frame FRAME-INFO with animation if enabled."
  (let* ((state (cdr (assoc frame-info lispbar--auto-hide-states)))
         (visible (plist-get state :visible))
         (frame (plist-get frame-info :frame)))
    
    (unless (or visible (not frame) (not (frame-live-p frame)))
      (lispbar--log 'debug "Showing auto-hide frame")
      
      ;; Cancel any hide timer
      (lispbar--cancel-auto-hide-timer frame-info)
      
      ;; Show with animation if enabled
      (let ((config (plist-get frame-info :config)))
        (if (or (plist-get config :auto-hide-animation)
                lispbar-auto-hide-animation)
            (lispbar--animate-frame-show frame-info)
          (progn
            (make-frame-visible frame)
            (lispbar--set-auto-hide-state frame-info :visible t)))))))

(defun lispbar--auto-hide-frame (frame-info)
  "Hide auto-hide frame FRAME-INFO with animation if enabled."
  (let* ((state (cdr (assoc frame-info lispbar--auto-hide-states)))
         (visible (plist-get state :visible))
         (frame (plist-get frame-info :frame)))
    
    (when (and visible frame (frame-live-p frame))
      (lispbar--log 'debug "Hiding auto-hide frame")
      
      ;; Hide with animation if enabled
      (let ((config (plist-get frame-info :config)))
        (if (or (plist-get config :auto-hide-animation)
                lispbar-auto-hide-animation)
            (lispbar--animate-frame-hide frame-info)
          (progn
            (make-frame-invisible frame)
            (lispbar--set-auto-hide-state frame-info :visible nil)))))))

(defun lispbar--animate-frame-show (frame-info)
  "Animate showing of FRAME-INFO."
  (let* ((config (plist-get frame-info :config))
         (duration (or (plist-get config :auto-hide-animation-duration)
                       lispbar-auto-hide-animation-duration))
         (steps 20)
         (step-duration (/ duration steps))
         (frame (plist-get frame-info :frame))
         (geometry (plist-get frame-info :geometry))
         (step 0))
    
    ;; Make frame visible but positioned off-screen initially
    (make-frame-visible frame)
    
    ;; Start animation timer
    (let ((timer (run-with-timer
                  0 step-duration
                  (lambda ()
                    (cl-incf step)
                    (let ((progress (/ (float step) steps)))
                      (if (>= progress 1.0)
                          (progn
                            ;; Animation complete
                            (lispbar--cancel-animation-timer frame-info)
                            (lispbar--set-auto-hide-state frame-info :visible t)
                            (lispbar--position-frame-at-target frame-info))
                        ;; Continue animation
                        (lispbar--update-frame-animation-position frame-info progress)))))))
      (lispbar--set-auto-hide-state frame-info :animation-timer timer))))

(defun lispbar--animate-frame-hide (frame-info)
  "Animate hiding of FRAME-INFO."
  (let* ((config (plist-get frame-info :config))
         (duration (or (plist-get config :auto-hide-animation-duration)
                       lispbar-auto-hide-animation-duration))
         (steps 20)
         (step-duration (/ duration steps))
         (step 0))
    
    ;; Start animation timer
    (let ((timer (run-with-timer
                  0 step-duration
                  (lambda ()
                    (cl-incf step)
                    (let ((progress (/ (float step) steps)))
                      (if (>= progress 1.0)
                          (progn
                            ;; Animation complete
                            (lispbar--cancel-animation-timer frame-info)
                            (make-frame-invisible (plist-get frame-info :frame))
                            (lispbar--set-auto-hide-state frame-info :visible nil))
                        ;; Continue animation
                        (lispbar--update-frame-animation-position frame-info (- 1.0 progress))))))))
      (lispbar--set-auto-hide-state frame-info :animation-timer timer))))

(defun lispbar--update-frame-animation-position (frame-info progress)
  "Update frame position during animation based on PROGRESS (0.0-1.0)."
  ;; This would implement smooth sliding animation
  ;; For now, just update visibility
  (let ((frame (plist-get frame-info :frame)))
    (when (and frame (frame-live-p frame))
      (if (> progress 0.1)
          (make-frame-visible frame)
        (make-frame-invisible frame)))))

(defun lispbar--position-frame-at-target (frame-info)
  "Position frame at its target location after animation."
  (let* ((frame (plist-get frame-info :frame))
         (geometry (plist-get frame-info :geometry)))
    (when (and frame (frame-live-p frame))
      (modify-frame-parameters frame
                               `((left . ,(plist-get geometry :x))
                                 (top . ,(plist-get geometry :y)))))))

(defun lispbar--set-auto-hide-state (frame-info key value)
  "Set auto-hide state KEY to VALUE for FRAME-INFO."
  (let ((state (cdr (assoc frame-info lispbar--auto-hide-states))))
    (when state
      (setq state (plist-put state key value)))))

(defun lispbar--get-auto-hide-state (frame-info key)
  "Get auto-hide state KEY for FRAME-INFO."
  (let ((state (cdr (assoc frame-info lispbar--auto-hide-states))))
    (when state
      (plist-get state key))))

(defun lispbar--cancel-auto-hide-timer (frame-info)
  "Cancel active auto-hide timer for FRAME-INFO."
  (let ((timer (lispbar--get-auto-hide-state frame-info :timer)))
    (when timer
      (cancel-timer timer)
      (lispbar--set-auto-hide-state frame-info :timer nil)
      (setq lispbar--auto-hide-timers (delq timer lispbar--auto-hide-timers)))))

(defun lispbar--cancel-animation-timer (frame-info)
  "Cancel active animation timer for FRAME-INFO."
  (let ((timer (lispbar--get-auto-hide-state frame-info :animation-timer)))
    (when timer
      (cancel-timer timer)
      (lispbar--set-auto-hide-state frame-info :animation-timer nil))))

(defun lispbar--cleanup-auto-hide ()
  "Clean up all auto-hide timers and state."
  (lispbar--log 'debug "Cleaning up auto-hide system")
  
  ;; Cancel all timers
  (dolist (timer lispbar--auto-hide-timers)
    (when timer (cancel-timer timer)))
  (setq lispbar--auto-hide-timers nil)
  
  (when lispbar--mouse-tracking-timer
    (cancel-timer lispbar--mouse-tracking-timer)
    (setq lispbar--mouse-tracking-timer nil))
  
  ;; Cancel all animation timers
  (dolist (frame-state-pair lispbar--auto-hide-states)
    (lispbar--cancel-animation-timer (car frame-state-pair)))
  
  ;; Clear state
  (setq lispbar--auto-hide-states nil)
  
  ;; Remove focus hooks
  (remove-hook 'focus-in-hook #'lispbar--on-focus-in)
  (remove-hook 'focus-out-hook #'lispbar--on-focus-out))

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

;;; Enhanced Hotplug Handling

(defun lispbar--setup-monitor-hotplug-detection ()
  "Set up enhanced monitor hotplug detection."
  (lispbar--log 'debug "Setting up monitor hotplug detection")
  
  ;; Enhanced EXWM RandR hook
  (when (featurep 'exwm-randr)
    (add-hook 'exwm-randr-screen-change-hook #'lispbar--handle-monitor-hotplug)
    (lispbar--add-cleanup-function 
     (lambda () 
       (remove-hook 'exwm-randr-screen-change-hook #'lispbar--handle-monitor-hotplug))))
  
  ;; Additional X11 property change detection if available
  (when (display-graphic-p)
    (lispbar--setup-x11-property-monitoring)))

(defun lispbar--handle-monitor-hotplug ()
  "Handle monitor hotplug events with enhanced delay and validation."
  (lispbar--log 'debug "Monitor hotplug event detected")
  
  ;; Cancel any existing timer
  (when lispbar--monitor-hotplug-timer
    (cancel-timer lispbar--monitor-hotplug-timer))
  
  ;; Set up delayed processing to allow system to stabilize
  (setq lispbar--monitor-hotplug-timer
        (run-at-time lispbar-monitor-hotplug-delay nil
                     (lambda ()
                       (setq lispbar--monitor-hotplug-timer nil)
                       (lispbar--process-monitor-hotplug)))))

(defun lispbar--process-monitor-hotplug ()
  "Process monitor hotplug after stabilization delay."
  (lispbar--log 'info "Processing monitor hotplug event")
  
  (condition-case err
      (progn
        ;; Create backup of current state for potential rollback
        (lispbar--backup-current-configuration)
        
        ;; Refresh monitor detection and handle changes
        (lispbar--refresh-monitors)
        
        ;; Validate the new configuration
        (unless (lispbar--validate-monitor-configuration)
          (lispbar--log 'warning "Monitor configuration validation failed, attempting recovery")
          (lispbar--attempt-configuration-recovery)))
    
    (error
     (lispbar--log 'error "Monitor hotplug processing failed: %s" err)
     (lispbar--attempt-configuration-recovery))))

(defun lispbar--setup-x11-property-monitoring ()
  "Set up X11 property change monitoring for monitor detection."
  ;; This would require more complex X11 event handling
  ;; For now, we rely on EXWM RandR and periodic checking
  (lispbar--log 'debug "X11 property monitoring not yet implemented"))

(defun lispbar--backup-current-configuration ()
  "Create a backup of current configuration for recovery purposes."
  (setq lispbar--configuration-backup
        (list :monitors (copy-sequence lispbar--monitors)
              :frames (copy-sequence lispbar--frames)
              :configurations (copy-alist lispbar--monitor-configurations)
              :timestamp (current-time)))
  (lispbar--log 'debug "Configuration backup created"))

(defun lispbar--attempt-configuration-recovery ()
  "Attempt to recover from configuration errors using backup."
  (when lispbar--configuration-backup
    (lispbar--log 'info "Attempting configuration recovery")
    
    (condition-case err
        (progn
          ;; Restore from backup
          (setq lispbar--monitors (plist-get lispbar--configuration-backup :monitors)
                lispbar--monitor-configurations (plist-get lispbar--configuration-backup :configurations))
          
          ;; Recreate frames if necessary
          (lispbar--cleanup-frames)
          (lispbar--create-frames)
          
          (lispbar--log 'info "Configuration recovery successful"))
      
      (error
       (lispbar--log 'error "Configuration recovery failed: %s" err)
       ;; Fall back to safe default
       (lispbar--reset-to-safe-configuration)))))

(defun lispbar--reset-to-safe-configuration ()
  "Reset to a known-safe configuration."
  (lispbar--log 'warning "Resetting to safe configuration")
  
  ;; Clear all current state
  (lispbar--cleanup-frames)
  (setq lispbar--monitors nil
        lispbar--monitor-configurations nil)
  
  ;; Force fallback detection
  (let ((lispbar-monitor-detection-method 'fallback))
    (setq lispbar--monitors (lispbar--detect-monitors)))
  
  ;; Create basic frames
  (lispbar--create-frames))

(defun lispbar--validate-monitor-configuration ()
  "Validate current monitor configuration for consistency.
Returns t if valid, nil if issues detected."
  (and
   ;; Check that we have at least one monitor
   (> (length lispbar--monitors) 0)
   
   ;; Check that all monitors have required properties
   (cl-every (lambda (monitor)
               (and (plist-get monitor :id)
                    (plist-get monitor :width)
                    (plist-get monitor :height)
                    (numberp (plist-get monitor :x))
                    (numberp (plist-get monitor :y))))
             lispbar--monitors)
   
   ;; Check that frames match monitors
   (cl-every (lambda (frame-info)
               (let ((monitor-id (plist-get frame-info :monitor)))
                 (cl-find-if (lambda (m) (equal (plist-get m :id) monitor-id))
                            lispbar--monitors)))
             lispbar--frames)))

;;; Frame Migration and Management

(defun lispbar--migrate-frame-content (affected-frames)
  "Migrate content from AFFECTED-FRAMES to remaining monitors."
  (lispbar--log 'info "Migrating content from %d affected frames" (length affected-frames))
  
  ;; Find target monitor (prefer primary, fall back to first available)
  (let ((target-monitor (or (cl-find-if (lambda (m) (plist-get m :primary)) lispbar--monitors)
                           (car lispbar--monitors))))
        (target-frame-info (cl-find-if (lambda (fi) 
                                        (equal (plist-get fi :monitor)
                                              (plist-get target-monitor :id)))
                                      lispbar--frames)))
    
    (when (and target-monitor target-frame-info)
      ;; For now, simply ensure the target frame exists
      ;; More sophisticated content migration could be implemented here
      (lispbar--log 'debug "Content migrated to monitor: %s" (plist-get target-monitor :id)))
    
    ;; Clean up the affected frames
    (dolist (frame-info affected-frames)
      (lispbar--destroy-frame frame-info))))

(defun lispbar--hide-frames (affected-frames)
  "Hide AFFECTED-FRAMES without destroying them."
  (lispbar--log 'info "Hiding %d frames" (length affected-frames))
  
  (dolist (frame-info affected-frames)
    (let ((frame (plist-get frame-info :frame)))
      (when (and frame (frame-live-p frame))
        (make-frame-invisible frame))))
  
  ;; Mark frames as hidden but keep them in the list
  (dolist (frame-info affected-frames)
    (setf (plist-get frame-info :hidden) t)))

(defun lispbar--destroy-frames (affected-frames)
  "Destroy AFFECTED-FRAMES completely."
  (lispbar--log 'info "Destroying %d frames" (length affected-frames))
  
  (dolist (frame-info affected-frames)
    (lispbar--destroy-frame frame-info)
    (setq lispbar--frames (delq frame-info lispbar--frames))))

;;; Monitor Aliases and User-Friendly Names

(defun lispbar--set-monitor-alias (monitor-id alias)
  "Set ALIAS for MONITOR-ID."
  (setq lispbar--monitor-aliases
        (cons (cons monitor-id alias)
              (assoc-delete-all monitor-id lispbar--monitor-aliases)))
  (lispbar--log 'debug "Set alias '%s' for monitor: %s" alias monitor-id)
  
  ;; Save to persistence file
  (when lispbar-monitor-persistence-file
    (lispbar--save-monitor-configurations)))

(defun lispbar--get-monitor-alias (monitor-id)
  "Get user-friendly alias for MONITOR-ID."
  (or (cdr (assoc monitor-id lispbar--monitor-aliases))
      monitor-id))

(defun lispbar--get-monitor-display-name (monitor)
  "Get display name for MONITOR (alias or fallback to name)."
  (let ((monitor-id (plist-get monitor :id)))
    (or (lispbar--get-monitor-alias monitor-id)
        (plist-get monitor :display-name)
        (plist-get monitor :name)
        monitor-id)))

;;; Public API Extensions

(defun lispbar-get-monitor-by-id (monitor-id)
  "Get monitor information for MONITOR-ID."
  (cl-find-if (lambda (m) (equal (plist-get m :id) monitor-id))
              lispbar--monitors))

(defun lispbar-get-primary-monitor ()
  "Get the primary monitor information."
  (when lispbar--primary-monitor-id
    (lispbar-get-monitor-by-id lispbar--primary-monitor-id)))

(defun lispbar-list-monitors (&optional include-disconnected)
  "List all detected monitors.
If INCLUDE-DISCONNECTED is non-nil, include disconnected monitors."
  (if include-disconnected
      lispbar--monitors
    (cl-remove-if-not (lambda (m) (plist-get m :connected)) lispbar--monitors)))

(defun lispbar-get-monitor-configuration (monitor-id)
  "Get configuration for MONITOR-ID."
  (lispbar--get-monitor-config monitor-id))

(defun lispbar-set-monitor-configuration (monitor-id config)
  "Set configuration for MONITOR-ID to CONFIG."
  (lispbar--set-monitor-config monitor-id config)
  
  ;; Update the frame if it exists
  (let ((frame-info (lispbar-get-frame-by-monitor monitor-id)))
    (when frame-info
      (let* ((monitor (lispbar-get-monitor-by-id monitor-id))
             (new-geometry (lispbar--calculate-frame-geometry monitor config))
             (frame (plist-get frame-info :frame)))
        
        (when (frame-live-p frame)
          ;; Update frame parameters
          (modify-frame-parameters frame
                                  (lispbar--build-frame-parameters new-geometry config))
          
          ;; Reconfigure frame
          (lispbar--configure-frame frame new-geometry config)
          
          ;; Update stored information
          (setf (plist-get frame-info :geometry) new-geometry)
          (setf (plist-get frame-info :config) config))))))

(defun lispbar-set-monitor-alias (monitor-id alias)
  "Set user-friendly ALIAS for MONITOR-ID."
  (lispbar--set-monitor-alias monitor-id alias))

(defun lispbar-get-monitor-alias (monitor-id)
  "Get user-friendly alias for MONITOR-ID."
  (lispbar--get-monitor-alias monitor-id))

(defun lispbar-add-monitor-change-callback (callback)
  "Add CALLBACK to monitor change notifications.
CALLBACK should accept (OLD-MONITORS NEW-MONITORS) arguments."
  (push callback lispbar--monitor-change-callbacks))

(defun lispbar-remove-monitor-change-callback (callback)
  "Remove CALLBACK from monitor change notifications."
  (setq lispbar--monitor-change-callbacks
        (delq callback lispbar--monitor-change-callbacks)))

(defun lispbar-force-monitor-refresh ()
  "Force immediate monitor refresh without waiting for events."
  (interactive)
  (lispbar--invalidate-monitor-cache)
  (lispbar--refresh-monitors))

(defun lispbar-reset-monitor-configurations ()
  "Reset all monitor configurations to defaults."
  (interactive)
  (when (yes-or-no-p "Reset all monitor configurations to defaults? ")
    (setq lispbar--monitor-configurations nil
          lispbar--monitor-aliases nil)
    
    ;; Save the reset state
    (when lispbar-monitor-persistence-file
      (lispbar--save-monitor-configurations))
    
    ;; Refresh to apply defaults
    (lispbar-force-monitor-refresh)
    
    (lispbar--log 'info "Monitor configurations reset to defaults")))

;;; Enhanced Lifecycle Management

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
  "Initialize enhanced Lispbar core system with multi-monitor support.
This function sets up comprehensive monitor detection, configuration
management, and frame creation with hotplug support."
  (interactive)
  (when lispbar--initialized
    (lispbar--log 'warning "Lispbar already initialized")
    (return-from lispbar-init))
  
  (lispbar--log 'info "Initializing enhanced Lispbar core system")
  
  (condition-case err
      (progn
        ;; Initialize enhanced monitor system
        (lispbar--log 'debug "Initializing monitor management")
        
        ;; Load persistent configurations
        (lispbar--load-monitor-configurations)
        
        ;; Detect monitors with comprehensive properties
        (setq lispbar--monitors (lispbar--detect-monitors))
        (lispbar--log 'info "Detected %d monitors using method: %s" 
                      (length lispbar--monitors) lispbar-monitor-detection-method)
        
        ;; Set up enhanced hotplug detection
        (lispbar--setup-monitor-hotplug-detection)
        
        ;; Create frames with per-monitor configuration
        (lispbar--create-frames)
        (lispbar--log 'info "Created %d frames with enhanced configuration" (length lispbar--frames))
        
        ;; Log detailed monitor information
        (dolist (monitor lispbar--monitors)
          (lispbar--log 'debug "Monitor: %s (%s) %dx%d+%d+%d primary:%s" 
                        (plist-get monitor :id)
                        (lispbar--get-monitor-display-name monitor)
                        (plist-get monitor :width) (plist-get monitor :height)
                        (plist-get monitor :x) (plist-get monitor :y)
                        (plist-get monitor :primary)))
        
        ;; Save current configuration
        (when lispbar-monitor-persistence-file
          (lispbar--save-monitor-configurations))
        
        (setq lispbar--initialized t)
        (lispbar--log 'info "Enhanced Lispbar core initialization complete"))
    
    (error
     (lispbar--log 'error "Lispbar initialization failed: %s" err)
     (lispbar-cleanup)
     (signal (car err) (cdr err)))))

;;;###autoload
(defun lispbar-cleanup ()
  "Clean up enhanced Lispbar system and restore previous state.
This function removes all frames, cleans up monitoring, and runs cleanup functions."
  (interactive)
  (lispbar--log 'info "Cleaning up enhanced Lispbar system")
  
  ;; Cancel any pending timers
  (when lispbar--monitor-hotplug-timer
    (cancel-timer lispbar--monitor-hotplug-timer)
    (setq lispbar--monitor-hotplug-timer nil))
  
  ;; Clean up auto-hide system
  (lispbar--cleanup-auto-hide)
  
  ;; Clean up frames
  (lispbar--cleanup-frames)
  
  ;; Run cleanup functions
  (lispbar--run-cleanup-functions)
  
  ;; Save final configuration state if enabled
  (when lispbar-monitor-persistence-file
    (condition-case err
        (lispbar--save-monitor-configurations)
      (error
       (lispbar--log 'warning "Could not save configurations during cleanup: %s" err))))
  
  ;; Reset enhanced state
  (setq lispbar--monitors nil
        lispbar--monitor-configurations nil
        lispbar--monitor-cache nil
        lispbar--monitor-aliases nil
        lispbar--monitor-change-callbacks nil
        lispbar--last-monitor-configuration nil
        lispbar--primary-monitor-id nil
        lispbar--configuration-backup nil
        lispbar--initialized nil)
  
  (lispbar--log 'info "Enhanced Lispbar cleanup complete")))

;;;###autoload
(defun lispbar-refresh ()
  "Refresh enhanced Lispbar with full monitor reconfiguration.
This function performs comprehensive monitor detection, configuration
application, and frame management updates."
  (interactive)
  (when lispbar--initialized
    (lispbar--log 'info "Refreshing enhanced Lispbar system")
    
    ;; Force cache invalidation for fresh detection
    (lispbar--invalidate-monitor-cache)
    
    ;; Perform full refresh
    (lispbar--refresh-monitors)
    
    ;; Apply any pending configuration changes
    (lispbar--auto-configure-monitors)
    
    (lispbar--log 'info "Enhanced Lispbar refresh complete")))

;;; Utility Functions

(defun lispbar-get-frames ()
  "Return list of active Lispbar frames.
Each element is a plist with :frame, :monitor, and :geometry keys."
  lispbar--frames)

(defun lispbar-get-frame-by-monitor (monitor-id)
  "Return frame information for MONITOR-ID, or nil if not found."
  (cl-find-if (lambda (frame-info)
                (equal (plist-get frame-info :monitor) monitor-id))
              lispbar--frames))

(defun lispbar-frame-live-p (frame-info)
  "Return non-nil if FRAME-INFO represents a live frame."
  (and frame-info
       (plist-get frame-info :frame)
       (frame-live-p (plist-get frame-info :frame))))

(defun lispbar-validate-config ()
  "Validate current enhanced Lispbar configuration.
Returns a list of configuration issues, or nil if configuration is valid.
Includes validation for multi-monitor settings."
  (let ((issues nil))
    
    ;; Validate global settings
    (unless (memq lispbar-position '(top bottom top-offset bottom-offset floating))
      (push "lispbar-position must be 'top, 'bottom, 'top-offset, 'bottom-offset, or 'floating" issues))
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
    
    ;; Validate advanced position settings
    (unless (and (integerp lispbar-position-offset) (>= lispbar-position-offset 0))
      (push "lispbar-position-offset must be a non-negative integer" issues))
    (unless (integerp lispbar-position-floating-x)
      (push "lispbar-position-floating-x must be an integer" issues))
    (unless (integerp lispbar-position-floating-y)
      (push "lispbar-position-floating-y must be an integer" issues))
    (unless (and (numberp lispbar-auto-hide-timeout) (> lispbar-auto-hide-timeout 0))
      (push "lispbar-auto-hide-timeout must be a positive number" issues))
    (unless (and (numberp lispbar-auto-hide-animation-duration) (> lispbar-auto-hide-animation-duration 0))
      (push "lispbar-auto-hide-animation-duration must be a positive number" issues))
    (unless (and (integerp lispbar-auto-hide-reveal-pixels) (>= lispbar-auto-hide-reveal-pixels 0))
      (push "lispbar-auto-hide-reveal-pixels must be a non-negative integer" issues))
    (unless (memq lispbar-auto-hide-trigger '(timer mouse focus manual))
      (push "lispbar-auto-hide-trigger must be 'timer, 'mouse, 'focus, or 'manual" issues))
    (unless (memq lispbar-strut-conflict-resolution '(adjust override disable))
      (push "lispbar-strut-conflict-resolution must be 'adjust, 'override, or 'disable" issues))
    (unless (and (integerp lispbar-edge-snapping-threshold) (> lispbar-edge-snapping-threshold 0))
      (push "lispbar-edge-snapping-threshold must be a positive integer" issues))
    
    ;; Validate multi-monitor settings
    (unless (memq lispbar-monitor-detection-method '(auto exwm-randr ewmh xrandr fallback))
      (push "lispbar-monitor-detection-method must be a valid detection method" issues))
    
    (unless (and (numberp lispbar-monitor-cache-timeout) (>= lispbar-monitor-cache-timeout 0))
      (push "lispbar-monitor-cache-timeout must be a non-negative number" issues))
    
    (unless (and (numberp lispbar-monitor-hotplug-delay) (> lispbar-monitor-hotplug-delay 0))
      (push "lispbar-monitor-hotplug-delay must be a positive number" issues))
    
    (when (and lispbar-monitor-persistence-file
               (not (file-writable-p (file-name-directory lispbar-monitor-persistence-file))))
      (push "lispbar-monitor-persistence-file directory is not writable" issues))
    
    (unless (memq lispbar-monitor-identification-method 
                  '(edid name name-resolution edid-name-resolution position))
      (push "lispbar-monitor-identification-method must be a valid identification method" issues))
    
    (unless (memq lispbar-monitor-frame-migration-strategy
                  '(preserve-content destroy hide))
      (push "lispbar-monitor-frame-migration-strategy must be a valid migration strategy" issues))
    
    ;; Validate monitor configurations
    (dolist (config-entry lispbar--monitor-configurations)
      (let ((monitor-id (car config-entry))
            (config (cdr config-entry)))
        (when config
          (when (and (plist-get config :height)
                     (not (and (integerp (plist-get config :height))
                              (> (plist-get config :height) 0))))
            (push (format "Monitor %s: height must be a positive integer" monitor-id) issues))
          
          (when (and (plist-get config :position)
                     (not (memq (plist-get config :position) '(top bottom top-offset bottom-offset floating))))
            (push (format "Monitor %s: position must be 'top, 'bottom, 'top-offset, 'bottom-offset, or 'floating" monitor-id) issues))
          
          (when (and (plist-get config :background-color)
                     (not (color-defined-p (plist-get config :background-color))))
            (push (format "Monitor %s: background-color is not valid" monitor-id) issues)))))
    
    issues))

;;; Safe Function Wrapper

(defun lispbar-safe-call (fn &rest args)
  "Safely call FN with ARGS, log errors and return nil on failure."
  (condition-case err
      (apply fn args)
    (error
     (lispbar--log 'error "Function %s failed: %s" fn err)
     nil)))

;;; Monitor Information Display (for debugging)

(defun lispbar-describe-monitors ()
  "Display detailed information about detected monitors."
  (interactive)
  (with-output-to-temp-buffer "*Lispbar Monitors*"
    (princ "=== Lispbar Enhanced Monitor Information ===\n\n")
    
    (if lispbar--monitors
        (progn
          (princ (format "Detection method: %s\n" lispbar-monitor-detection-method))
          (princ (format "Total monitors: %d\n" (length lispbar--monitors)))
          (princ (format "Primary monitor: %s\n\n" (or lispbar--primary-monitor-id "None")))
          
          (dolist (monitor lispbar--monitors)
            (let* ((id (plist-get monitor :id))
                   (name (plist-get monitor :name))
                   (display-name (lispbar--get-monitor-display-name monitor))
                   (alias (lispbar--get-monitor-alias id))
                   (config (lispbar--get-monitor-config id))
                   (frame-info (lispbar-get-frame-by-monitor id)))
              
              (princ (format "Monitor: %s\n" id))
              (princ (format "  Name: %s\n" name))
              (princ (format "  Display Name: %s\n" display-name))
              (when (not (equal alias id))
                (princ (format "  Alias: %s\n" alias)))
              (princ (format "  Geometry: %dx%d+%d+%d\n"
                            (plist-get monitor :width) (plist-get monitor :height)
                            (plist-get monitor :x) (plist-get monitor :y)))
              (princ (format "  Primary: %s\n" (if (plist-get monitor :primary) "Yes" "No")))
              (princ (format "  Connected: %s\n" (if (plist-get monitor :connected) "Yes" "No")))
              (princ (format "  Source: %s\n" (plist-get monitor :source)))
              (when (plist-get monitor :edid)
                (princ (format "  EDID: %s\n" (plist-get monitor :edid))))
              
              (princ "  Configuration:\n")
              (princ (format "    Position: %s\n" (plist-get config :position)))
              (princ (format "    Height: %d\n" (plist-get config :height)))
              (princ (format "    Inherit Global: %s\n" (plist-get config :inherit-global)))
              
              (if frame-info
                  (progn
                    (princ "  Frame: Active\n")
                    (let ((geom (plist-get frame-info :geometry)))
                      (princ (format "    Frame Geometry: %dx%d+%d+%d\n"
                                    (plist-get geom :width) (plist-get geom :height)
                                    (plist-get geom :x) (plist-get geom :y)))))
                (princ "  Frame: None\n"))
              
              (princ "\n"))))
      
      (princ "No monitors detected.\n"))
    
    (princ "\n=== Configuration Files ===\n")
    (princ (format "Persistence file: %s\n" 
                  (or lispbar-monitor-persistence-file "Disabled")))
    (when lispbar-monitor-persistence-file
      (princ (format "File exists: %s\n" 
                    (if (file-exists-p lispbar-monitor-persistence-file) "Yes" "No"))))
    
    (princ "\n=== Cache Information ===\n")
    (if lispbar--monitor-cache
        (let ((cache-age (float-time (time-subtract (current-time) 
                                                   (plist-get lispbar--monitor-cache :timestamp)))))
          (princ (format "Cache age: %.1f seconds\n" cache-age))
          (princ (format "Cache timeout: %.1f seconds\n" lispbar-monitor-cache-timeout))
          (princ (format "Cache valid: %s\n" 
                        (if (< cache-age lispbar-monitor-cache-timeout) "Yes" "No"))))
      (princ "No cache data\n"))))

;;; Interactive Position Commands

;;;###autoload
(defun lispbar-set-position (position &optional monitor-id)
  "Set toolbar POSITION for MONITOR-ID (or all monitors if nil).
POSITION can be \\='top, \\='bottom, \\='top-offset, \\='bottom-offset, or \\='floating."
  (interactive 
   (list (intern (completing-read 
                  "Position: " 
                  '("top" "bottom" "top-offset" "bottom-offset" "floating")
                  nil t))
         (when current-prefix-arg
           (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (if monitor-id
      ;; Set position for specific monitor
      (let ((config (lispbar--get-monitor-config monitor-id)))
        (setq config (plist-put config :position position))
        (lispbar-set-monitor-configuration monitor-id config))
    
    ;; Set global position
    (setq lispbar-position position)
    
    ;; Update all monitors that inherit global settings
    (dolist (monitor lispbar--monitors)
      (let* ((monitor-id (plist-get monitor :id))
             (config (lispbar--get-monitor-config monitor-id)))
        (when (plist-get config :inherit-global)
          (lispbar-set-monitor-configuration monitor-id config)))))
  
  (message "Toolbar position set to: %s" position))

;;;###autoload
(defun lispbar-set-position-offset (offset &optional monitor-id)
  "Set position OFFSET in pixels for MONITOR-ID (or all monitors if nil).
Used with \\='top-offset and \\='bottom-offset position modes."
  (interactive 
   (list (read-number "Offset (pixels): " lispbar-position-offset)
         (when current-prefix-arg
           (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (if monitor-id
      ;; Set offset for specific monitor
      (let ((config (lispbar--get-monitor-config monitor-id)))
        (setq config (plist-put config :position-offset offset))
        (lispbar-set-monitor-configuration monitor-id config))
    
    ;; Set global offset
    (setq lispbar-position-offset offset))
  
  (message "Position offset set to: %d pixels" offset))

;;;###autoload
(defun lispbar-set-floating-position (x y &optional monitor-id)
  "Set floating position coordinates X, Y for MONITOR-ID (or all monitors if nil)."
  (interactive 
   (list (read-number "X coordinate: " lispbar-position-floating-x)
         (read-number "Y coordinate: " lispbar-position-floating-y)
         (when current-prefix-arg
           (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (if monitor-id
      ;; Set coordinates for specific monitor
      (let ((config (lispbar--get-monitor-config monitor-id)))
        (setq config (plist-put config :position-floating-x x))
        (setq config (plist-put config :position-floating-y y))
        (lispbar-set-monitor-configuration monitor-id config))
    
    ;; Set global coordinates
    (setq lispbar-position-floating-x x
          lispbar-position-floating-y y))
  
  (message "Floating position set to: (%d, %d)" x y))

;;;###autoload
(defun lispbar-toggle-auto-hide (&optional monitor-id)
  "Toggle auto-hide functionality for MONITOR-ID (or all monitors if nil)."
  (interactive 
   (when current-prefix-arg
     (list (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (if monitor-id
      ;; Toggle for specific monitor
      (let* ((config (lispbar--get-monitor-config monitor-id))
             (current (plist-get config :auto-hide-enabled))
             (new-value (not current)))
        (setq config (plist-put config :auto-hide-enabled new-value))
        (lispbar-set-monitor-configuration monitor-id config)
        (message "Auto-hide %s for monitor %s" 
                 (if new-value "enabled" "disabled") monitor-id))
    
    ;; Toggle globally
    (setq lispbar-auto-hide-enabled (not lispbar-auto-hide-enabled))
    (message "Auto-hide %s globally" 
             (if lispbar-auto-hide-enabled "enabled" "disabled"))))

;;;###autoload
(defun lispbar-show-all-frames ()
  "Show all auto-hidden frames immediately."
  (interactive)
  (dolist (frame-state-pair lispbar--auto-hide-states)
    (let ((frame-info (car frame-state-pair)))
      (lispbar--show-frame frame-info)))
  (message "All frames shown"))

;;;###autoload
(defun lispbar-hide-all-frames ()
  "Hide all frames with auto-hide enabled."
  (interactive)
  (dolist (frame-state-pair lispbar--auto-hide-states)
    (let ((frame-info (car frame-state-pair)))
      (lispbar--auto-hide-frame frame-info)))
  (message "All auto-hide frames hidden"))

;;;###autoload
(defun lispbar-position-status (&optional monitor-id)
  "Display position status for MONITOR-ID (or all monitors if nil)."
  (interactive 
   (when current-prefix-arg
     (list (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (if monitor-id
      ;; Show status for specific monitor
      (let* ((monitor (lispbar-get-monitor-by-id monitor-id))
             (config (lispbar--get-monitor-config monitor-id)))
        (if monitor
            (message (concat "Monitor %s:\n"
                           "Position: %s\n"
                           "Offset: %d\n"
                           "Floating: (%d, %d)\n"
                           "Auto-hide: %s\n"
                           "Strut enabled: %s")
                    monitor-id
                    (plist-get config :position)
                    (or (plist-get config :position-offset) 0)
                    (or (plist-get config :position-floating-x) 0)
                    (or (plist-get config :position-floating-y) 0)
                    (if (plist-get config :auto-hide-enabled) "Yes" "No")
                    (if (plist-get config :strut-enabled) "Yes" "No"))
          (message "Monitor %s not found" monitor-id)))
    
    ;; Show global status
    (message (concat "Global Position Settings:\n"
                   "Position: %s\n"
                   "Offset: %d\n"
                   "Floating: (%d, %d)\n"
                   "Auto-hide: %s\n"
                   "Strut enabled: %s\n"
                   "Active monitors: %d")
            lispbar-position
            lispbar-position-offset
            lispbar-position-floating-x
            lispbar-position-floating-y
            (if lispbar-auto-hide-enabled "Yes" "No")
            (if lispbar-strut-enabled "Yes" "No")
            (length lispbar--monitors))))

;;;###autoload
(defun lispbar-interactive-position-adjust ()
  "Interactively adjust toolbar position using keyboard shortcuts.
Use arrow keys to move, +/- to resize, ESC to finish."
  (interactive)
  (when lispbar--frames
    (let* ((frame-info (car lispbar--frames))  ; Use first frame for demo
           (frame (plist-get frame-info :frame))
           (geometry (copy-sequence (plist-get frame-info :geometry)))
           (adjusting t)
           (step-size 10))
      
      (message "Position adjustment mode: Arrow keys=move, +/-=resize, ESC=finish")
      
      (while adjusting
        (let ((key (read-key)))
          (cl-case key
            ;; Movement
            (up    (setq geometry (plist-put geometry :y 
                                            (- (plist-get geometry :y) step-size))))
            (down  (setq geometry (plist-put geometry :y 
                                            (+ (plist-get geometry :y) step-size))))
            (left  (setq geometry (plist-put geometry :x 
                                            (- (plist-get geometry :x) step-size))))
            (right (setq geometry (plist-put geometry :x 
                                            (+ (plist-get geometry :x) step-size))))
            
            ;; Resizing
            (?+    (setq geometry (plist-put geometry :width 
                                            (+ (plist-get geometry :width) step-size))))
            (?-    (setq geometry (plist-put geometry :width 
                                            (max 50 (- (plist-get geometry :width) step-size)))))
            
            ;; Finish
            (?\e   (setq adjusting nil))
            (t     (message "Unknown key. Use arrow keys, +/-, or ESC")))
          
          (when adjusting
            ;; Apply edge snapping
            (when lispbar-edge-snapping-enabled
              (let* ((monitor (lispbar-get-monitor-by-id (plist-get frame-info :monitor)))
                     (snapped (lispbar--apply-edge-snapping 
                              (plist-get geometry :x) (plist-get geometry :y)
                              (plist-get geometry :width) (plist-get geometry :height)
                              monitor)))
                (setq geometry (plist-put geometry :x (car snapped)))
                (setq geometry (plist-put geometry :y (cadr snapped)))))
            
            ;; Update frame position
            (modify-frame-parameters frame
                                    `((left . ,(plist-get geometry :x))
                                      (top . ,(plist-get geometry :y))
                                      (width . ,(plist-get geometry :width))))
            
            (message "Position: (%d, %d) Size: %dx%d"
                    (plist-get geometry :x) (plist-get geometry :y)
                    (plist-get geometry :width) (plist-get geometry :height)))))
      
      ;; Save final position
      (setf (plist-get frame-info :geometry) geometry)
      (message "Position adjustment complete"))))

;;;###autoload
(defun lispbar-validate-position-configuration (&optional monitor-id)
  "Validate position configuration for MONITOR-ID (or all monitors if nil).
Display detailed validation results and suggestions."
  (interactive 
   (when current-prefix-arg
     (list (completing-read "Monitor ID: " 
                           (mapcar (lambda (m) (plist-get m :id)) 
                                   lispbar--monitors)))))
  
  (let ((issues '())
        (warnings '())
        (suggestions '()))
    
    (if monitor-id
        ;; Validate specific monitor
        (let* ((monitor (lispbar-get-monitor-by-id monitor-id))
               (config (lispbar--get-monitor-config monitor-id)))
          (when monitor
            (lispbar--validate-monitor-position-config monitor config issues warnings suggestions)))
      
      ;; Validate all monitors
      (dolist (monitor lispbar--monitors)
        (let* ((monitor-id (plist-get monitor :id))
               (config (lispbar--get-monitor-config monitor-id)))
          (lispbar--validate-monitor-position-config monitor config issues warnings suggestions))))
    
    ;; Display results
    (let ((message-parts '()))
      (when issues
        (push (format "ISSUES:\n%s" (mapconcat 'identity issues "\n")) message-parts))
      (when warnings
        (push (format "WARNINGS:\n%s" (mapconcat 'identity warnings "\n")) message-parts))
      (when suggestions
        (push (format "SUGGESTIONS:\n%s" (mapconcat 'identity suggestions "\n")) message-parts))
      
      (if (or issues warnings suggestions)
          (message (mapconcat 'identity (reverse message-parts) "\n\n"))
        (message "Position configuration is valid!")))))

(defun lispbar--validate-monitor-position-config (monitor config issues warnings suggestions)
  "Validate position configuration for MONITOR with CONFIG.
Add problems to ISSUES, WARNINGS, and SUGGESTIONS lists."
  (let* ((monitor-id (plist-get monitor :id))
         (position (or (plist-get config :position) lispbar-position))
         (offset (or (plist-get config :position-offset) lispbar-position-offset))
         (floating-x (or (plist-get config :position-floating-x) lispbar-position-floating-x))
         (floating-y (or (plist-get config :position-floating-y) lispbar-position-floating-y))
         (auto-hide-enabled (or (plist-get config :auto-hide-enabled) lispbar-auto-hide-enabled))
         (monitor-width (plist-get monitor :width))
         (monitor-height (plist-get monitor :height)))
    
    ;; Validate position mode
    (unless (memq position '(top bottom top-offset bottom-offset floating))
      (push (format "Monitor %s: Invalid position '%s'" monitor-id position) issues))
    
    ;; Validate offset values
    (when (memq position '(top-offset bottom-offset))
      (when (or (< offset 0) (> offset (/ monitor-height 2)))
        (push (format "Monitor %s: Offset %d may cause positioning issues" monitor-id offset) warnings)))
    
    ;; Validate floating coordinates
    (when (eq position 'floating)
      (when (or (< floating-x 0) (>= floating-x monitor-width))
        (push (format "Monitor %s: Floating X coordinate %d is outside monitor bounds" 
                      monitor-id floating-x) issues))
      (when (or (< floating-y 0) (>= floating-y monitor-height))
        (push (format "Monitor %s: Floating Y coordinate %d is outside monitor bounds" 
                      monitor-id floating-y) issues)))
    
    ;; Check for strut conflicts
    (when (and (not (eq position 'floating))
               (plist-get config :strut-enabled))
      (let* ((geometry (lispbar--calculate-frame-geometry monitor config))
             (conflicts (lispbar--detect-strut-conflicts geometry)))
        (when conflicts
          (push (format "Monitor %s: Strut conflicts detected - consider adjusting position" monitor-id) warnings))))
    
    ;; Auto-hide suggestions
    (when (and auto-hide-enabled (eq position 'floating))
      (push (format "Monitor %s: Auto-hide with floating position may behave unexpectedly" monitor-id) suggestions))))

(provide 'lispbar-core)
;;; lispbar-core.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End: