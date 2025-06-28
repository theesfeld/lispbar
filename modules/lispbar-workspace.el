;;; lispbar-workspace.el --- EXWM workspace module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar Development Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4"))
;; Keywords: workspaces, exwm, modules, lispbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides EXWM workspace display and management for Lispbar.
;; It shows current workspaces with configurable highlighting, supports both
;; numbered and named workspace display modes, and provides interactive
;; workspace switching through mouse clicks.
;;
;; Features:
;; - Real-time workspace status with event-driven updates
;; - Current workspace highlighting with customizable bracket styles
;; - Support for numbered (1 2 3) and named (work home games) display modes
;; - Combined display mode (1:work 2:home 3:games)
;; - Optional window count display per workspace
;; - Activity indicators for workspaces with windows
;; - Click-to-switch workspace navigation
;; - Intelligent caching to reduce CPU usage
;; - Graceful fallback when EXWM is not available
;; - Full integration with lispbar-exwm communication channels
;;
;; The module listens to EXWM events through communication channels:
;; - workspace-changed: Updates current workspace display
;; - window-managed: Updates window counts when windows are added
;; - window-unmanaged: Updates window counts when windows are removed
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
Can be:
  \='numbered - Show workspace numbers (1 2 3)
  \='named - Show workspace names (work home games)
  \='both - Show both numbers and names (1:work 2:home)"
  :type '(choice (const :tag "Numbers only" numbered)
                 (const :tag "Names only" named)
                 (const :tag "Both numbers and names" both))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-show-window-count nil
  "Whether to display window count per workspace.
When enabled, shows format like: 1(3) 2(1) 3"
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-show-activity-indicator t
  "Whether to highlight workspaces that have windows.
When enabled, workspaces with windows use different face."
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-current-bracket-style 'square
  "Bracket style for highlighting current workspace.
Can be:
  \='square - [1] 2 3
  \='round - (1) 2 3
  \='angle - <1> 2 3
  \='none - 1 2 3 (uses face only)"
  :type '(choice (const :tag "Square brackets []" square)
                 (const :tag "Round brackets ()" round)
                 (const :tag "Angle brackets <>" angle)
                 (const :tag "No brackets" none))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-separator " "
  "Separator between workspace indicators."
  :type 'string
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-enable-switching t
  "Whether clicking on workspace switches to it.
When enabled, mouse clicks on workspace indicators switch workspaces."
  :type 'boolean
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-position 'left
  "Position of the workspace module on the toolbar.
Can be \='left, \='center, or \='right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-priority 90
  "Display priority for the workspace module.
Higher values are displayed first within the same position.
Range: 0-100, default: 90 (high priority for left position)."
  :type 'integer
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-cache-timeout 0.1
  "Cache timeout for workspace content in seconds.
Set to 0 to disable caching (not recommended)."
  :type 'number
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-face 'lispbar-render-default
  "Face to use for normal workspace indicators."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-current-face 'lispbar-render-highlight
  "Face to use for current workspace indicator."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-active-face 'lispbar-render-active
  "Face to use for workspaces with windows (activity)."
  :type 'face
  :group 'lispbar-workspace)

(defcustom lispbar-workspace-empty-face 'lispbar-render-inactive
  "Face to use for empty workspaces."
  :type 'face
  :group 'lispbar-workspace)

;;; Variables

(defvar lispbar-workspace--module-instance nil
  "Instance of the workspace module.")

(defvar lispbar-workspace--enabled nil
  "Whether the workspace module is currently enabled.")

(defvar lispbar-workspace--window-counts nil
  "Cache of window counts per workspace.
Alist mapping workspace index to window count.")

(defvar lispbar-workspace--last-workspace-data nil
  "Cache of last workspace data for comparison.")

;;; Workspace Module Class

(defclass lispbar-workspace-module (lispbar-module)
  ((display-mode :initarg :display-mode
                 :type symbol
                 :initform 'numbered
                 :documentation "Display mode: numbered, named, or both.")
   (show-window-count :initarg :show-window-count
                      :type boolean
                      :initform nil
                      :documentation "Whether to show window counts.")
   (show-activity :initarg :show-activity
                  :type boolean
                  :initform t
                  :documentation "Whether to show activity indicators.")
   (bracket-style :initarg :bracket-style
                  :type symbol
                  :initform 'square
                  :documentation "Bracket style for current workspace.")
   (separator :initarg :separator
              :type string
              :initform " "
              :documentation "Separator between workspace indicators.")
   (enable-switching :initarg :enable-switching
                     :type boolean
                     :initform t
                     :documentation "Whether mouse clicks switch workspaces.")
   (face :initarg :face
         :type (or symbol list)
         :initform 'lispbar-render-default
         :documentation "Face for normal workspace display.")
   (current-face :initarg :current-face
                 :type (or symbol list)
                 :initform 'lispbar-render-highlight
                 :documentation "Face for current workspace.")
   (active-face :initarg :active-face
                :type (or symbol list)
                :initform 'lispbar-render-active
                :documentation "Face for workspaces with windows.")
   (empty-face :initarg :empty-face
               :type (or symbol list)
               :initform 'lispbar-render-inactive
               :documentation "Face for empty workspaces."))
  "Workspace module class for Lispbar.
Displays EXWM workspace status with interactive switching capabilities.")

;;; Helper Functions

(defun lispbar-workspace--get-brackets (style)
  "Get bracket characters for STYLE.
Returns a cons cell (open . close) or nil for 'none style."
  (cl-case style
    (square '("[" . "]"))
    (round '("(" . ")"))
    (angle '("<" . ">"))
    (none nil)
    (t '("[" . "]"))))

(defun lispbar-workspace--count-windows-in-workspace (workspace-index)
  "Count windows in WORKSPACE-INDEX.
Returns the number of windows in the workspace."
  (if (lispbar-exwm-available-p)
      (condition-case nil
          (let ((count 0))
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                (when (and (eq major-mode 'exwm-mode)
                           (boundp 'exwm--frame)
                           exwm--frame
                           (frame-live-p exwm--frame))
                  (let ((frame-workspace (frame-parameter exwm--frame 'exwm-workspace)))
                    (when (eq frame-workspace workspace-index)
                      (cl-incf count))))))
            count)
        (error 0))
    0))

(defun lispbar-workspace--update-window-counts ()
  "Update cached window counts for all workspaces."
  (let ((workspace-count (length (lispbar-exwm-workspace-names))))
    (setq lispbar-workspace--window-counts
          (cl-loop for i from 0 below workspace-count
                   collect (cons i (lispbar-workspace--count-windows-in-workspace i))))))

(defun lispbar-workspace--get-window-count (workspace-index)
  "Get cached window count for WORKSPACE-INDEX."
  (or (cdr (assq workspace-index lispbar-workspace--window-counts)) 0))

(defun lispbar-workspace--format-workspace-name (index name module)
  "Format workspace name based on INDEX, NAME and MODULE settings.
Returns formatted string for display."
  (let ((display-mode (oref module display-mode)))
    (cl-case display-mode
      (numbered (format "%d" (1+ index)))
      (named (or name (format "%d" (1+ index))))
      (both (format "%d:%s" (1+ index) (or name (format "ws%d" (1+ index)))))
      (t (format "%d" (1+ index))))))

(defun lispbar-workspace--make-clickable (text workspace-index)
  "Make TEXT clickable to switch to WORKSPACE-INDEX."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 
                (lambda () 
                  (interactive)
                  (lispbar-workspace-switch-to workspace-index)))
    (propertize text
                'mouse-face 'highlight
                'help-echo (format "Switch to workspace %d" (1+ workspace-index))
                'keymap map)))

(defun lispbar-workspace--get-workspace-face (index current-index window-count module)
  "Get appropriate face for workspace at INDEX.
CURRENT-INDEX is the current workspace, WINDOW-COUNT is windows in workspace.
MODULE is the workspace module instance."
  (cond
   ((= index current-index) (oref module current-face))
   ((and (oref module show-activity) (> window-count 0)) (oref module active-face))
   ((= window-count 0) (oref module empty-face))
   (t (oref module face))))

(defun lispbar-workspace--format-single-workspace (index name current-index module)
  "Format a single workspace indicator.
INDEX is workspace index, NAME is workspace name, CURRENT-INDEX is current.
MODULE is the workspace module instance."
  (let* ((window-count (lispbar-workspace--get-window-count index))
         (is-current (= index current-index))
         (brackets (when is-current (lispbar-workspace--get-brackets (oref module bracket-style))))
         (face (lispbar-workspace--get-workspace-face index current-index window-count module))
         (base-text (lispbar-workspace--format-workspace-name index name module))
         (count-text (when (oref module show-window-count)
                       (format "(%d)" window-count)))
         (formatted-text (concat (when brackets (car brackets))
                                 base-text
                                 count-text
                                 (when brackets (cdr brackets)))))
    
    ;; Apply face
    (setq formatted-text (propertize formatted-text 'face face))
    
    ;; Make clickable if enabled
    (when (oref module enable-switching)
      (setq formatted-text (lispbar-workspace--make-clickable formatted-text index)))
    
    formatted-text))

(defun lispbar-workspace--get-display-content (module)
  "Get formatted display content for workspace MODULE.
Returns propertized string ready for display."
  (let* ((current-workspace (or (lispbar-exwm-current-workspace) 0))
         (workspace-names (or (lispbar-exwm-workspace-names) '("1" "2" "3" "4")))
         (separator (oref module separator))
         (workspace-strings nil))
    
    ;; Update window counts if needed
    (when (or (oref module show-window-count) (oref module show-activity))
      (lispbar-workspace--update-window-counts))
    
    ;; Format each workspace
    (cl-loop for name in workspace-names
             for index from 0
             do (push (lispbar-workspace--format-single-workspace 
                       index name current-workspace module)
                      workspace-strings))
    
    ;; Join with separator
    (mapconcat #'identity (nreverse workspace-strings) separator)))

;;; Module Implementation

(cl-defmethod lispbar-module-update ((module lispbar-workspace-module))
  "Update method for workspace module.
Returns formatted workspace string for display."
  (condition-case err
      (lispbar-workspace--get-display-content module)
    (error
     (lispbar-modules--log 'error "Workspace module update failed: %s" err)
     (propertize "Workspace Error" 'face 'lispbar-render-urgent))))

(cl-defmethod lispbar-workspace-configure ((module lispbar-workspace-module))
  "Configure workspace module with current customization values."
  (oset module display-mode lispbar-workspace-display-mode)
  (oset module show-window-count lispbar-workspace-show-window-count)
  (oset module show-activity lispbar-workspace-show-activity-indicator)
  (oset module bracket-style lispbar-workspace-current-bracket-style)
  (oset module separator lispbar-workspace-separator)
  (oset module enable-switching lispbar-workspace-enable-switching)
  (oset module face lispbar-workspace-face)
  (oset module current-face lispbar-workspace-current-face)
  (oset module active-face lispbar-workspace-active-face)
  (oset module empty-face lispbar-workspace-empty-face)
  (oset module position lispbar-workspace-position)
  (oset module priority lispbar-workspace-priority)
  (oset module cache-timeout lispbar-workspace-cache-timeout)
  
  ;; Update position list if position changed
  (lispbar-modules--add-to-position-list module))

(cl-defmethod lispbar-workspace-receive-message ((module lispbar-workspace-module) channel _message)
  "Handle messages from communication CHANNEL for MODULE.
_MESSAGE contains event-specific data."
  (cl-case channel
    (workspace-changed
     (lispbar-modules--log 'debug "Workspace module received workspace-changed")
     ;; Invalidate cache and trigger update
     (lispbar-modules-invalidate-cache 'workspace)
     (lispbar-modules-update 'workspace))
    
    ((window-managed window-unmanaged)
     (lispbar-modules--log 'debug "Workspace module received window event: %s" channel)
     ;; Update window counts if we're tracking them
     (when (or (oref module show-window-count) (oref module show-activity))
       (lispbar-workspace--update-window-counts)
       (lispbar-modules-invalidate-cache 'workspace)
       (lispbar-modules-update 'workspace)))))

;;; Module Creation and Management

(defun lispbar-workspace--create-module ()
  "Create and configure a new workspace module instance.
Returns the configured module instance."
  (let ((module (make-instance 'lispbar-workspace-module
                               :name 'workspace
                               :update-fn (lambda () 
                                          (lispbar-module-update lispbar-workspace--module-instance))
                               :update-interval nil  ; Event-driven only
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
      
      ;; Ensure EXWM integration is initialized
      (unless lispbar-exwm--initialized
        (lispbar-exwm-init))
      
      ;; Create and register module
      (let ((module (lispbar-workspace--create-module)))
        (lispbar-modules-register module)
        (setq lispbar-workspace--enabled t)
        
        ;; Initial window count update
        (when (or (oref module show-window-count) (oref module show-activity))
          (lispbar-workspace--update-window-counts))
        
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
            lispbar-workspace--window-counts nil
            lispbar-workspace--last-workspace-data nil)
      
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
(defun lispbar-workspace-switch-to (workspace-index)
  "Switch to WORKSPACE-INDEX (0-indexed).
When called interactively, prompts for workspace number (1-indexed)."
  (interactive
   (list (1- (read-number "Switch to workspace: " 
                          (1+ (or (lispbar-exwm-current-workspace) 0))))))
  (when (lispbar-exwm-available-p)
    (condition-case err
        (progn
          (exwm-workspace-switch workspace-index)
          (message "Switched to workspace %d" (1+ workspace-index)))
      (error
       (message "Failed to switch workspace: %s" err)))))

;;;###autoload
(defun lispbar-workspace-next ()
  "Switch to next workspace, wrapping around if necessary."
  (interactive)
  (let* ((current (or (lispbar-exwm-current-workspace) 0))
         (total (length (lispbar-exwm-workspace-names)))
         (next (mod (1+ current) total)))
    (lispbar-workspace-switch-to next)))

;;;###autoload
(defun lispbar-workspace-prev ()
  "Switch to previous workspace, wrapping around if necessary."
  (interactive)
  (let* ((current (or (lispbar-exwm-current-workspace) 0))
         (total (length (lispbar-exwm-workspace-names)))
         (prev (mod (1- current) total)))
    (lispbar-workspace-switch-to prev)))

;;; Status and Information Functions

;;;###autoload
(defun lispbar-workspace-status ()
  "Display current status of the workspace module.
Shows configuration and current workspace information."
  (interactive)
  (if lispbar-workspace--enabled
      (let* ((module lispbar-workspace--module-instance)
             (current-ws (or (lispbar-exwm-current-workspace) 0))
             (ws-names (lispbar-exwm-workspace-names))
             (display-mode (oref module display-mode))
             (position (oref module position))
             (priority (oref module priority))
             (sample-output (lispbar-workspace--get-display-content module)))
        (message (concat "Workspace module: ENABLED\n"
                        "Position: %s (priority %d)\n"
                        "Display mode: %s\n"
                        "Current workspace: %d (%s)\n"
                        "Total workspaces: %d\n"
                        "Sample output: %s")
                position priority display-mode 
                (1+ current-ws) (nth current-ws ws-names)
                (length ws-names)
                sample-output))
    (message "Workspace module: DISABLED")))

;;;###autoload
(defun lispbar-workspace-info ()
  "Display detailed workspace information including window counts."
  (interactive)
  (if (lispbar-exwm-available-p)
      (let* ((current-ws (or (lispbar-exwm-current-workspace) 0))
             (ws-names (lispbar-exwm-workspace-names))
             (info-lines nil))
        
        ;; Update window counts
        (lispbar-workspace--update-window-counts)
        
        ;; Build info for each workspace
        (cl-loop for name in ws-names
                 for index from 0
                 for window-count = (lispbar-workspace--get-window-count index)
                 do (push (format "Workspace %d%s: %s (%d windows)"
                                  (1+ index)
                                  (if (= index current-ws) " [current]" "")
                                  name
                                  window-count)
                          info-lines))
        
        (message (mapconcat #'identity (nreverse info-lines) "\n")))
    (message "EXWM not available")))

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