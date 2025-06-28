;;; lispbar-workspace.el --- EXWM workspace module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar Development Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4") (exwm "0.24"))
;; Keywords: workspace, exwm, modules, lispbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides EXWM workspace display for Lispbar with comprehensive
;; workspace tracking, switching capabilities, and customizable display options.
;; It integrates tightly with EXWM to provide real-time workspace status updates,
;; click-to-switch functionality, window counts, and activity indicators.
;;
;; The workspace module demonstrates advanced integration with the EXWM window
;; manager and showcases the communication channel capabilities of the Lispbar
;; module system for event-driven updates.
;;
;; Features:
;; - Real-time workspace display with current workspace highlighting
;; - Support for both numbered (1|2|3) and named (work|home|games) display modes
;; - Mouse click support for instant workspace switching
;; - Optional window count display per workspace (e.g., "1(3) 2(1) 3")
;; - Activity indicators for workspaces with windows
;; - EXWM event-driven updates via communication channels
;; - Graceful fallback when EXWM is not available
;; - Configurable position (left/center/right) and priority
;; - Intelligent caching and error recovery
;; - Full customization through Emacs custom system
;;
;; Usage:
;;   (require 'lispbar-workspace)
;;   (lispbar-workspace-enable)
;;
;; Customization:
;;   M-x customize-group RET lispbar-workspace RET

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lispbar-modules)
(require 'lispbar-exwm)

;;; Customization

(defgroup lispbar-workspace nil
  "Customization group for Lispbar workspace module."
  :group 'lispbar-modules
  :prefix "lispbar-workspace-")

(defcustom lispbar-workspace-display-mode 'numbered
  "Display mode for workspaces.
'numbered shows workspaces as numbers (1 2 3)
'named shows workspace names when available (work home games)
'both shows both number and name (1:work 2:home 3:games)"
  :type '(choice (const :tag "Numbers only" numbered)
                 (const :tag "Names only" named)
                 (const :tag "Both number and name" both))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-show-window-count t
  "Whether to display window count for each workspace.
When enabled, shows count like '1(3) 2(1) 3' where numbers
in parentheses indicate the number of windows on each workspace."
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-show-activity-indicator t
  "Whether to show activity indicators for workspaces with windows.
When enabled, workspaces with windows are visually distinguished."
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-separator " "
  "Separator string between workspace items."
  :type 'string
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-current-bracket-style 'square
  "Bracket style for highlighting the current workspace.
'square uses [1] 2 3
'round uses (1) 2 3
'angle uses <1> 2 3
'none disables brackets"
  :type '(choice (const :tag "Square brackets [1]" square)
                 (const :tag "Round brackets (1)" round)
                 (const :tag "Angle brackets <1>" angle)
                 (const :tag "No brackets" none))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-enable-switching t
  "Whether to enable workspace switching via mouse clicks.
When enabled, clicking on a workspace number switches to that workspace."
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-position 'left
  "Position of the workspace module on the toolbar.
Can be 'left, 'center, or 'right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-priority 90
  "Display priority for the workspace module.
Higher values are displayed first within the same position.
Range: 0-100, default: 90 (high priority for left side positioning)."
  :type 'integer
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-update-interval nil
  "Update interval for the workspace module in seconds.
If nil, updates are purely event-driven via EXWM communication channels.
Can be set to a number for periodic updates as fallback."
  :type '(choice (const :tag "Event-driven only" nil)
                 (number :tag "Update interval in seconds"))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-cache-timeout 5.0
  "Cache timeout for workspace content in seconds.
Lower values provide more responsive updates but use more CPU."
  :type 'number
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-face 'lispbar-render-default
  "Face to use for normal workspace display.
Can be any valid face name or face specification."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-current-face 'lispbar-render-highlight
  "Face to use for current workspace highlighting.
Can be any valid face name or face specification."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-active-face 'lispbar-render-active
  "Face to use for workspaces with windows (activity indicator).
Can be any valid face name or face specification."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-empty-face 'lispbar-render-dim
  "Face to use for empty workspaces.
Can be any valid face name or face specification."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-fallback-count 4
  "Number of workspaces to show when EXWM is not available.
This provides a reasonable fallback display when workspace
detection fails."
  :type 'integer
  :group 'lispbar-workspace)

;;; Variables

(defvar lispbar-workspace--module-instance nil
  "Instance of the workspace module.")

(defvar lispbar-workspace--enabled nil
  "Whether the workspace module is currently enabled.")

(defvar lispbar-workspace--last-workspace-info nil
  "Cached workspace information for change detection.")

(defvar lispbar-workspace--window-counts nil
  "Cached window counts per workspace.")

;;; Workspace Module Class

(defclass lispbar-workspace-module (lispbar-module)
  ((display-mode :initarg :display-mode
                 :type symbol
                 :initform 'numbered
                 :documentation "Display mode: 'numbered, 'named, or 'both.")
   (show-window-count :initarg :show-window-count
                      :type boolean
                      :initform t
                      :documentation "Whether to show window counts.")
   (show-activity :initarg :show-activity
                  :type boolean
                  :initform t
                  :documentation "Whether to show activity indicators.")
   (separator :initarg :separator
              :type string
              :initform " "
              :documentation "Separator between workspace items.")
   (bracket-style :initarg :bracket-style
                  :type symbol
                  :initform 'square
                  :documentation "Bracket style for current workspace.")
   (enable-switching :initarg :enable-switching
                     :type boolean
                     :initform t
                     :documentation "Whether to enable click-to-switch.")
   (normal-face :initarg :normal-face
                :type (or symbol list)
                :initform 'lispbar-render-default
                :documentation "Face for normal workspaces.")
   (current-face :initarg :current-face
                 :type (or symbol list)
                 :initform 'lispbar-render-highlight
                 :documentation "Face for current workspace.")
   (active-face :initarg :active-face
                :type (or symbol list)
                :initform 'lispbar-render-active
                :documentation "Face for active workspaces.")
   (empty-face :initarg :empty-face
               :type (or symbol list)
               :initform 'lispbar-render-dim
               :documentation "Face for empty workspaces.")
   (fallback-count :initarg :fallback-count
                   :type integer
                   :initform 4
                   :documentation "Fallback workspace count."))
  "Workspace module class for Lispbar.
Displays EXWM workspaces with current highlighting and optional features.")

;;; Helper Functions

(defun lispbar-workspace--get-bracket-chars (style)
  "Get bracket characters for STYLE.
Returns a cons cell (OPEN . CLOSE) with bracket characters."
  (cl-case style
    (square '("[" . "]"))
    (round '("(" . ")"))
    (angle '("<" . ">"))
    (none '("" . ""))
    (t '("[" . "]"))))

(defun lispbar-workspace--get-workspace-info ()
  "Get current workspace information.
Returns a plist with :current, :names, :count, and :window-counts."
  (condition-case err
      (let* ((current (or (lispbar-exwm-current-workspace) 0))
             (names (or (lispbar-exwm-workspace-names) 
                       (lispbar-workspace--generate-fallback-names)))
             (count (length names))
             (window-counts (lispbar-workspace--get-window-counts count)))
        
        (list :current current
              :names names
              :count count
              :window-counts window-counts))
    (error
     (lispbar-modules--log 'error "Failed to get workspace info: %s" err)
     ;; Fallback workspace info
     (let ((fallback-count lispbar-workspace-fallback-count))
       (list :current 0
             :names (lispbar-workspace--generate-fallback-names fallback-count)
             :count fallback-count
             :window-counts (make-vector fallback-count 0))))))

(defun lispbar-workspace--generate-fallback-names (&optional count)
  "Generate fallback workspace names.
COUNT defaults to `lispbar-workspace-fallback-count'."
  (let ((num-workspaces (or count lispbar-workspace-fallback-count)))
    (cl-loop for i from 1 to num-workspaces
             collect (format "%d" i))))

(defun lispbar-workspace--get-window-counts (workspace-count)
  "Get window counts for each workspace.
WORKSPACE-COUNT specifies the number of workspaces.
Returns a vector of window counts."
  (condition-case err
      (if (and (featurep 'exwm) (lispbar-exwm-available-p))
          (lispbar-workspace--get-exwm-window-counts workspace-count)
        (make-vector workspace-count 0))
    (error
     (lispbar-modules--log 'error "Failed to get window counts: %s" err)
     (make-vector workspace-count 0))))

(defun lispbar-workspace--get-exwm-window-counts (workspace-count)
  "Get window counts from EXWM for WORKSPACE-COUNT workspaces.
Returns a vector of window counts."
  (let ((counts (make-vector workspace-count 0)))
    (when (and (boundp 'exwm--id-buffer-alist) exwm--id-buffer-alist)
      (dolist (entry exwm--id-buffer-alist)
        (let ((buffer (cdr entry)))
          (when (buffer-live-p buffer)
            (let ((workspace-id (buffer-local-value 'exwm--frame buffer)))
              (when (and workspace-id (boundp 'exwm-workspace--list))
                (let ((workspace-index (cl-position workspace-id exwm-workspace--list)))
                  (when (and workspace-index 
                             (>= workspace-index 0) 
                             (< workspace-index workspace-count))
                    (cl-incf (aref counts workspace-index))))))))))
    counts))

(defun lispbar-workspace--format-workspace-item (index name current-p window-count module)
  "Format a single workspace item.
INDEX is the 0-based workspace index.
NAME is the workspace name or number.
CURRENT-P indicates if this is the current workspace.
WINDOW-COUNT is the number of windows on this workspace.
MODULE is the workspace module instance."
  (let* ((display-mode (oref module display-mode))
         (show-count (oref module show-window-count))
         (show-activity (oref module show-activity))
         (bracket-style (oref module bracket-style))
         (enable-switching (oref module enable-switching))
         (normal-face (oref module normal-face))
         (current-face (oref module current-face))
         (active-face (oref module active-face))
         (empty-face (oref module empty-face))
         
         ;; Determine display text
         (display-text (cl-case display-mode
                         (numbered (format "%d" (1+ index)))
                         (named name)
                         (both (format "%d:%s" (1+ index) name))
                         (t (format "%d" (1+ index)))))
         
         ;; Add window count if enabled
         (full-text (if show-count
                        (format "%s(%d)" display-text window-count)
                      display-text))
         
         ;; Add brackets for current workspace
         (bracketed-text (if current-p
                             (let ((brackets (lispbar-workspace--get-bracket-chars bracket-style)))
                               (concat (car brackets) full-text (cdr brackets)))
                           full-text))
         
         ;; Choose appropriate face
         (face (cond
                (current-p current-face)
                ((and show-activity (> window-count 0)) active-face)
                ((= window-count 0) empty-face)
                (t normal-face)))
         
         ;; Apply face and mouse support
         (final-text (propertize bracketed-text 'face face)))
    
    ;; Add mouse click support if enabled
    (when enable-switching
      (setq final-text
            (propertize final-text
                        'mouse-face 'highlight
                        'help-echo (format "Switch to workspace %s" display-text)
                        'keymap (let ((map (make-sparse-keymap)))
                                 (define-key map [mouse-1] 
                                   `(lambda (event)
                                      (interactive "e")
                                      (lispbar-workspace--switch-to-workspace ,index)))
                                 map))))
    
    final-text))

(defun lispbar-workspace--switch-to-workspace (index)
  "Switch to workspace at INDEX (0-based)."
  (condition-case err
      (cond
       ;; Use EXWM if available
       ((and (featurep 'exwm) 
             (lispbar-exwm-available-p)
             (fboundp 'exwm-workspace-switch))
        (exwm-workspace-switch index)
        (lispbar-modules--log 'info "Switched to workspace %d via EXWM" (1+ index)))
       
       ;; Try EWMH as fallback
       ((display-graphic-p)
        (x-change-window-property "_NET_CURRENT_DESKTOP" index nil nil nil t t)
        (lispbar-modules--log 'info "Switched to workspace %d via EWMH" (1+ index)))
       
       ;; No switching method available
       (t
        (lispbar-modules--log 'warning "No workspace switching method available")
        (message "Workspace switching not available")))
    (error
     (lispbar-modules--log 'error "Failed to switch to workspace %d: %s" (1+ index) err)
     (message "Failed to switch workspace: %s" err))))

(defun lispbar-workspace--build-display-string (module)
  "Build the complete workspace display string for MODULE.
Returns a propertized string ready for display."
  (let* ((workspace-info (lispbar-workspace--get-workspace-info))
         (current (plist-get workspace-info :current))
         (names (plist-get workspace-info :names))
         (count (plist-get workspace-info :count))
         (window-counts (plist-get workspace-info :window-counts))
         (separator (oref module separator))
         (workspace-items nil))
    
    ;; Build individual workspace items
    (dotimes (i count)
      (let* ((name (nth i names))
             (current-p (= i current))
             (window-count (if (vectorp window-counts) (aref window-counts i) 0)))
        (push (lispbar-workspace--format-workspace-item i name current-p window-count module)
              workspace-items)))
    
    ;; Join items with separator
    (let ((result (mapconcat #'identity (nreverse workspace-items) separator)))
      (when (string-empty-p result)
        (setq result (propertize "No workspaces" 'face (oref module empty-face))))
      result)))

;;; Module Implementation

(cl-defmethod lispbar-module-update ((module lispbar-workspace-module))
  "Update method for workspace module.
Returns formatted workspace string for display."
  (condition-case err
      (lispbar-workspace--build-display-string module)
    (error
     (lispbar-modules--log 'error "Workspace module update failed: %s" err)
     (propertize "Workspace Error" 'face 'lispbar-render-urgent))))

(cl-defmethod lispbar-workspace-configure ((module lispbar-workspace-module))
  "Configure workspace module with current customization values."
  (oset module display-mode lispbar-workspace-display-mode)
  (oset module show-window-count lispbar-workspace-show-window-count)
  (oset module show-activity lispbar-workspace-show-activity-indicator)
  (oset module separator lispbar-workspace-separator)
  (oset module bracket-style lispbar-workspace-current-bracket-style)
  (oset module enable-switching lispbar-workspace-enable-switching)
  (oset module normal-face lispbar-workspace-face)
  (oset module current-face lispbar-workspace-current-face)
  (oset module active-face lispbar-workspace-active-face)
  (oset module empty-face lispbar-workspace-empty-face)
  (oset module fallback-count lispbar-workspace-fallback-count)
  (oset module position lispbar-workspace-position)
  (oset module priority lispbar-workspace-priority)
  (oset module update-interval lispbar-workspace-update-interval)
  (oset module cache-timeout lispbar-workspace-cache-timeout)
  
  ;; Update position list if position changed
  (lispbar-modules--add-to-position-list module))

(cl-defmethod lispbar-workspace-receive-message ((module lispbar-workspace-module) channel message)
  "Handle messages received on communication CHANNEL with MESSAGE."
  (cl-case channel
    (workspace-changed
     (lispbar-modules--log 'debug "Workspace module received workspace change notification")
     ;; Invalidate cache and trigger update
     (lispbar-modules-invalidate-cache (oref module name))
     (lispbar-modules-update (oref module name)))
    
    (window-managed
     (when (oref module show-window-count)
       (lispbar-modules--log 'debug "Workspace module received window managed notification")
       ;; Clear window count cache and update
       (setq lispbar-workspace--window-counts nil)
       (lispbar-modules-invalidate-cache (oref module name))
       (lispbar-modules-update (oref module name))))
    
    (window-unmanaged
     (when (oref module show-window-count)
       (lispbar-modules--log 'debug "Workspace module received window unmanaged notification")
       ;; Clear window count cache and update
       (setq lispbar-workspace--window-counts nil)
       (lispbar-modules-invalidate-cache (oref module name))
       (lispbar-modules-update (oref module name))))
    
    (t
     ;; Ignore other messages
     nil)))

;;; Module Creation and Management

(defun lispbar-workspace--create-module ()
  "Create and configure a new workspace module instance.
Returns the configured module instance."
  (let ((module (make-instance 'lispbar-workspace-module
                               :name 'workspace
                               :update-fn (lambda () 
                                          (lispbar-module-update lispbar-workspace--module-instance))
                               :update-interval lispbar-workspace-update-interval
                               :position lispbar-workspace-position
                               :priority lispbar-workspace-priority
                               :cache-timeout lispbar-workspace-cache-timeout
                               :enabled t
                               :communicates '(workspace-changed window-managed window-unmanaged))))
    
    ;; Configure with current customization values
    (lispbar-workspace-configure module)
    
    ;; Set the global instance
    (setq lispbar-workspace--module-instance module)
    
    (lispbar-modules--log 'info "Workspace module created")
    module))

;;;###autoload
(defun lispbar-workspace-enable ()
  "Enable the Lispbar workspace module.
Creates and registers the workspace module if not already enabled."
  (interactive)
  (if lispbar-workspace--enabled
      (lispbar-modules--log 'info "Workspace module already enabled")
    (progn
      (lispbar-modules--log 'info "Enabling workspace module")
      
      ;; Ensure module system is initialized
      (unless lispbar-modules--initialized
        (lispbar-modules-init))
      
      ;; Ensure EXWM integration is initialized if available
      (when (and (featurep 'exwm) (fboundp 'lispbar-exwm-init))
        (unless lispbar-exwm--initialized
          (lispbar-exwm-init)))
      
      ;; Create and register module
      (let ((module (lispbar-workspace--create-module)))
        (lispbar-modules-register module)
        (setq lispbar-workspace--enabled t)
        (lispbar-modules--log 'info "Workspace module enabled successfully")))))

;;;###autoload
(defun lispbar-workspace-disable ()
  "Disable the Lispbar workspace module.
Unregisters and cleans up the workspace module."
  (interactive)
  (if (not lispbar-workspace--enabled)
      (lispbar-modules--log 'info "Workspace module not enabled")
    (progn
      (lispbar-modules--log 'info "Disabling workspace module")
      
      ;; Unregister module
      (when lispbar-workspace--module-instance
        (lispbar-modules-unregister 'workspace))
      
      ;; Clean up
      (setq lispbar-workspace--module-instance nil
            lispbar-workspace--enabled nil
            lispbar-workspace--last-workspace-info nil
            lispbar-workspace--window-counts nil)
      
      (lispbar-modules--log 'info "Workspace module disabled"))))

;;;###autoload
(defun lispbar-workspace-toggle ()
  "Toggle the Lispbar workspace module on/off."
  (interactive)
  (if lispbar-workspace--enabled
      (lispbar-workspace-disable)
    (lispbar-workspace-enable)))

;;;###autoload
(defun lispbar-workspace-reconfigure ()
  "Reconfigure the workspace module with current customization values.
Useful after changing workspace customization options."
  (interactive)
  (when (and lispbar-workspace--enabled lispbar-workspace--module-instance)
    (lispbar-modules--log 'info "Reconfiguring workspace module")
    (lispbar-workspace-configure lispbar-workspace--module-instance)
    (lispbar-modules-invalidate-cache 'workspace)
    (lispbar-modules-update 'workspace)
    (lispbar-modules--log 'info "Workspace module reconfigured")))

;;; Interactive Commands

;;;###autoload
(defun lispbar-workspace-switch-to (workspace-number)
  "Switch to workspace WORKSPACE-NUMBER (1-based).
Prompts for workspace number if called interactively."
  (interactive "nWorkspace number: ")
  (let ((zero-based-index (1- workspace-number)))
    (lispbar-workspace--switch-to-workspace zero-based-index)))

;;;###autoload
(defun lispbar-workspace-next ()
  "Switch to the next workspace."
  (interactive)
  (let* ((workspace-info (lispbar-workspace--get-workspace-info))
         (current (plist-get workspace-info :current))
         (count (plist-get workspace-info :count))
         (next-workspace (if (>= current (1- count)) 0 (1+ current))))
    (lispbar-workspace--switch-to-workspace next-workspace)))

;;;###autoload
(defun lispbar-workspace-prev ()
  "Switch to the previous workspace."
  (interactive)
  (let* ((workspace-info (lispbar-workspace--get-workspace-info))
         (current (plist-get workspace-info :current))
         (count (plist-get workspace-info :count))
         (prev-workspace (if (<= current 0) (1- count) (1- current))))
    (lispbar-workspace--switch-to-workspace prev-workspace)))

;;;###autoload
(defun lispbar-workspace-set-display-mode (mode)
  "Set workspace display MODE.
MODE can be 'numbered, 'named, or 'both."
  (interactive (list (intern (completing-read "Display mode: " 
                                             '("numbered" "named" "both")
                                             nil t))))
  (setq lispbar-workspace-display-mode mode)
  (when lispbar-workspace--enabled
    (lispbar-workspace-reconfigure))
  (message "Workspace display mode set to: %s" mode))

;;;###autoload
(defun lispbar-workspace-toggle-window-count ()
  "Toggle window count display on/off for workspaces."
  (interactive)
  (setq lispbar-workspace-show-window-count (not lispbar-workspace-show-window-count))
  (when lispbar-workspace--enabled
    (lispbar-workspace-reconfigure))
  (message "Workspace window count display: %s" 
           (if lispbar-workspace-show-window-count "enabled" "disabled")))

;;;###autoload
(defun lispbar-workspace-toggle-activity-indicator ()
  "Toggle activity indicator display on/off for workspaces."
  (interactive)
  (setq lispbar-workspace-show-activity-indicator 
        (not lispbar-workspace-show-activity-indicator))
  (when lispbar-workspace--enabled
    (lispbar-workspace-reconfigure))
  (message "Workspace activity indicator: %s" 
           (if lispbar-workspace-show-activity-indicator "enabled" "disabled")))

;;; Status and Information Functions

(defun lispbar-workspace-status ()
  "Display current status of the workspace module.
Shows whether enabled, current settings, workspace info, etc."
  (interactive)
  (if lispbar-workspace--enabled
      (let* ((module lispbar-workspace--module-instance)
             (workspace-info (lispbar-workspace--get-workspace-info))
             (current (plist-get workspace-info :current))
             (count (plist-get workspace-info :count))
             (display-mode (oref module display-mode))
             (show-count (oref module show-window-count))
             (show-activity (oref module show-activity))
             (position (oref module position))
             (priority (oref module priority))
             (sample-output (lispbar-workspace--build-display-string module)))
        (message (concat "Workspace module: ENABLED\n"
                        "Position: %s (priority %d)\n"
                        "Current workspace: %d of %d\n"
                        "Display mode: %s\n"
                        "Window count: %s\n"
                        "Activity indicator: %s\n"
                        "Click switching: %s\n"
                        "Sample output: %s")
                position priority (1+ current) count display-mode
                (if show-count "enabled" "disabled")
                (if show-activity "enabled" "disabled")
                (if (oref module enable-switching) "enabled" "disabled")
                sample-output))
    (message "Workspace module: DISABLED")))

(defun lispbar-workspace-info ()
  "Display detailed workspace information.
Shows current workspace, available workspaces, window counts, etc."
  (interactive)
  (let* ((workspace-info (lispbar-workspace--get-workspace-info))
         (current (plist-get workspace-info :current))
         (names (plist-get workspace-info :names))
         (count (plist-get workspace-info :count))
         (window-counts (plist-get workspace-info :window-counts)))
    (message (concat "Workspace Information:\n"
                    "Current: %d (%s)\n"
                    "Total workspaces: %d\n"
                    "Workspace details:\n%s")
            (1+ current) (nth current names) count
            (mapconcat (lambda (i)
                        (format "  %d: %s (%d windows)"
                               (1+ i) (nth i names)
                               (if (vectorp window-counts) (aref window-counts i) 0)))
                      (number-sequence 0 (1- count)) "\n"))))

;;; Module Cleanup

(defun lispbar-workspace--cleanup ()
  "Clean up workspace module resources.
Called during module system shutdown."
  (when lispbar-workspace--enabled
    (lispbar-workspace-disable)))

;; Register cleanup function
(eval-after-load 'lispbar-modules
  '(lispbar--add-cleanup-function #'lispbar-workspace--cleanup))

;;; Provide

(provide 'lispbar-workspace)
;;; lispbar-workspace.el ends here