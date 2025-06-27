;;; lispbar-render.el --- Rendering engine for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: frames, display, rendering
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides the rendering engine for Lispbar. It handles buffer
;; management, text rendering, layout calculations, and efficient updates for
;; the toolbar display.
;;
;; The rendering engine creates dedicated buffers for each frame and manages
;; left/center/right layout positioning. It supports propertized text, faces,
;; themes, and includes performance optimizations to avoid blocking the UI.
;;
;; Key features:
;; - Buffer management for each frame
;; - Left/center/right layout system with proper spacing
;; - Support for propertized text and icons
;; - Theme-aware rendering through face system
;; - Efficient update mechanism with change detection
;; - Content overflow handling and truncation
;; - Performance optimization with throttled updates

;;; Code:

(require 'cl-lib)
(require 'lispbar-core)

;;; Customization

(defgroup lispbar-render nil
  "Customization group for Lispbar rendering system."
  :group 'lispbar
  :prefix "lispbar-render-")

(defcustom lispbar-render-separator " | "
  "String used to separate modules within each position section."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-padding "  "
  "Padding string added to the edges of the toolbar content."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-module-spacing "   "
  "Spacing between left/center/right sections."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-truncate-length 200
  "Maximum length for individual module content before truncation."
  :type 'integer
  :group 'lispbar-render)

(defcustom lispbar-render-truncate-indicator "..."
  "String to append when content is truncated."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-update-throttle 0.1
  "Minimum time in seconds between frame updates to prevent excessive rendering."
  :type 'number
  :group 'lispbar-render)

(defcustom lispbar-render-async-updates nil
  "Whether to perform frame updates asynchronously to avoid blocking UI."
  :type 'boolean
  :group 'lispbar-render)

;;; Variables

(defvar lispbar-render--frame-buffers nil
  "Alist mapping frames to their display buffers.")

(defvar lispbar-render--frame-content nil
  "Alist mapping frames to their current content for change detection.")

(defvar lispbar-render--update-timers nil
  "Alist mapping frames to their update timers for throttling.")

(defvar lispbar-render--initialized nil
  "Whether the rendering system has been initialized.")

;;; Faces

(defface lispbar-render-default
  '((t :inherit mode-line))
  "Default face for Lispbar content."
  :group 'lispbar-render)

(defface lispbar-render-highlight
  '((t :inherit mode-line-highlight))
  "Face for highlighted content in Lispbar."
  :group 'lispbar-render)

(defface lispbar-render-inactive
  '((t :inherit mode-line-inactive))
  "Face for inactive content in Lispbar."
  :group 'lispbar-render)

(defface lispbar-render-urgent
  '((t :inherit error :weight bold))
  "Face for urgent notifications in Lispbar."
  :group 'lispbar-render)

;;; Buffer Management

(defun lispbar-render--create-buffer (frame)
  "Create and configure a display buffer for FRAME.
Returns the created buffer."
  (let* ((frame-name (frame-parameter frame 'name))
         (monitor-info (cl-find-if (lambda (info)
                                     (eq (plist-get info :frame) frame))
                                   (lispbar-get-frames)))
         (monitor-name (if monitor-info
                           (plist-get monitor-info :monitor)
                         "unknown"))
         (buffer-name (format "*lispbar-%s*" monitor-name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; Configure buffer for optimal toolbar display
      (setq cursor-type nil
            cursor-in-non-selected-windows nil
            mode-line-format nil
            header-line-format nil
            tab-line-format nil
            buffer-read-only t
            truncate-lines t
            word-wrap nil
            show-trailing-whitespace nil
            indicate-empty-lines nil
            indicate-buffer-boundaries nil
            fringe-indicator-alist nil
            left-margin-width 0
            right-margin-width 0)
      
      ;; Disable various minor modes that might interfere
      (when (fboundp 'display-line-numbers-mode)
        (display-line-numbers-mode -1))
      (when (fboundp 'hl-line-mode)
        (hl-line-mode -1))
      (when (fboundp 'whitespace-mode)
        (whitespace-mode -1))
      
      ;; Set appropriate faces
      (face-remap-add-relative 'default 'lispbar-render-default)
      
      ;; Make buffer local to frame
      (set (make-local-variable 'lispbar-render--parent-frame) frame))
    
    (lispbar-log 'debug "Created buffer %s for frame %s" buffer-name frame-name)
    buffer))

(defun lispbar-render--get-frame-buffer (frame)
  "Get or create the display buffer for FRAME."
  (let ((buffer (cdr (assq frame lispbar-render--frame-buffers))))
    (if (and buffer (buffer-live-p buffer))
        buffer
      ;; Create new buffer and register it
      (let ((new-buffer (lispbar-render--create-buffer frame)))
        (setq lispbar-render--frame-buffers
              (cons (cons frame new-buffer)
                    (cl-remove frame lispbar-render--frame-buffers :key #'car)))
        new-buffer))))

(defun lispbar-render--cleanup-buffer (frame)
  "Clean up the display buffer associated with FRAME."
  (let ((buffer (cdr (assq frame lispbar-render--frame-buffers))))
    (when buffer
      (lispbar-log 'debug "Cleaning up buffer for frame %s" frame)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq lispbar-render--frame-buffers
            (cl-remove frame lispbar-render--frame-buffers :key #'car)))))

;;; Content Validation and Normalization

(defun lispbar-render--validate-content (content)
  "Validate that CONTENT is suitable for rendering.
Returns normalized content or signals an error."
  (cond
   ((null content) "")
   ((stringp content) content)
   ((listp content)
    ;; Handle propertized strings and lists of strings
    (if (and (stringp (car-safe content))
             (plist-get (cdr-safe content) 'face))
        ;; Propertized string
        content
      ;; List of content items
      (mapconcat (lambda (item) (lispbar-render--validate-content item))
                 content lispbar-render-separator)))
   ((symbolp content) (symbol-name content))
   (t (format "%s" content))))

(defun lispbar-render--truncate-content (content max-length)
  "Truncate CONTENT to MAX-LENGTH characters if necessary.
Appends truncation indicator if content is truncated."
  (let ((content-str (lispbar-render--validate-content content)))
    (if (<= (length content-str) max-length)
        content-str
      (concat (substring content-str 0 (- max-length (length lispbar-render-truncate-indicator)))
              lispbar-render-truncate-indicator))))

(defun lispbar-render--normalize-content-list (content-list)
  "Normalize a list of content items for rendering.
Returns a single string with appropriate separators."
  (when content-list
    (let ((validated-items (cl-remove-if #'string-empty-p
                                        (mapcar (lambda (item)
                                                  (lispbar-render--truncate-content 
                                                   item lispbar-render-truncate-length))
                                                content-list))))
      (if validated-items
          (mapconcat #'identity validated-items lispbar-render-separator)
        ""))))

;;; Layout Calculation

(defun lispbar-render--calculate-layout (frame left center right)
  "Calculate layout positions for LEFT, CENTER, and RIGHT content in FRAME.
Returns a plist with layout information."
  (let* ((frame-info (cl-find-if (lambda (info)
                                   (eq (plist-get info :frame) frame))
                                 (lispbar-get-frames)))
         (frame-geometry (plist-get frame-info :geometry))
         (frame-width (plist-get frame-geometry :width))
         (spacing-width (length lispbar-render-module-spacing))
         (padding-width (* 2 (length lispbar-render-padding)))
         (available-width (- frame-width padding-width))
         
         ;; Calculate content widths (approximate)
         (left-width (length left))
         (center-width (length center))
         (right-width (length right))
         
         ;; Calculate positions
         (left-pos 0)
         (center-pos (max (+ left-width spacing-width)
                          (- (/ available-width 2) (/ center-width 2))))
         (right-pos (max (+ center-pos center-width spacing-width)
                         (- available-width right-width))))
    
    (list :frame-width frame-width
          :available-width available-width
          :left-pos left-pos
          :center-pos center-pos
          :right-pos right-pos
          :left-width left-width
          :center-width center-width
          :right-width right-width)))

(defun lispbar-render--format-content (left center right layout)
  "Format content using LAYOUT information.
Returns the final formatted string for display."
  (let* ((available-width (plist-get layout :available-width))
         (result (make-string available-width ?\s)))
    
    ;; Insert left content
    (when (and left (> (length left) 0))
      (let ((end-pos (min (length left) available-width)))
        (cl-loop for i from 0 below end-pos
                 do (aset result i (aref left i)))))
    
    ;; Insert center content
    (when (and center (> (length center) 0))
      (let* ((center-pos (plist-get layout :center-pos))
             (end-pos (min (+ center-pos (length center)) available-width)))
        (cl-loop for i from center-pos below end-pos
                 for j from 0
                 do (aset result i (aref center j)))))
    
    ;; Insert right content
    (when (and right (> (length right) 0))
      (let* ((right-pos (plist-get layout :right-pos))
             (end-pos (min (+ right-pos (length right)) available-width)))
        (cl-loop for i from right-pos below end-pos
                 for j from 0
                 do (aset result i (aref right j)))))
    
    ;; Add padding
    (concat lispbar-render-padding result lispbar-render-padding)))

;;; Change Detection

(defun lispbar-render--calculate-content-hash (left center right)
  "Calculate a hash for the content combination to detect changes."
  (sxhash (list left center right)))

(defun lispbar-render--content-changed-p (frame left center right)
  "Check if content has changed for FRAME since last update."
  (let* ((current-hash (lispbar-render--calculate-content-hash left center right))
         (stored-hash (cdr (assq frame lispbar-render--frame-content))))
    (not (eq current-hash stored-hash))))

(defun lispbar-render--update-content-hash (frame left center right)
  "Update the stored content hash for FRAME."
  (let ((new-hash (lispbar-render--calculate-content-hash left center right)))
    (setq lispbar-render--frame-content
          (cons (cons frame new-hash)
                (cl-remove frame lispbar-render--frame-content :key #'car)))))

;;; Core Rendering Functions

(defun lispbar-render--render-frame (frame left center right)
  "Render content to FRAME's buffer.
LEFT, CENTER, and RIGHT are content for respective positions."
  (let ((buffer (lispbar-render--get-frame-buffer frame)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((left-str (lispbar-render--normalize-content-list left))
               (center-str (lispbar-render--normalize-content-list center))
               (right-str (lispbar-render--normalize-content-list right))
               (layout (lispbar-render--calculate-layout frame left-str center-str right-str))
               (formatted-content (lispbar-render--format-content left-str center-str right-str layout))
               (inhibit-read-only t))
          
          ;; Clear buffer and insert new content
          (erase-buffer)
          (insert formatted-content)
          
          ;; Ensure content fills the frame
          (goto-char (point-min))
          (when (eobp)
            (insert " "))
          
          ;; Display buffer in frame
          (let ((window (frame-root-window frame)))
            (when window
              (set-window-buffer window buffer)
              (set-window-point window (point-min))))
          
          (lispbar-log 'debug "Rendered frame with content length: %d" 
                       (length formatted-content)))))))

(defun lispbar-render--update-frame-throttled (frame left center right)
  "Update FRAME with throttling to prevent excessive updates."
  (let ((timer (cdr (assq frame lispbar-render--update-timers))))
    ;; Cancel existing timer if any
    (when timer
      (cancel-timer timer))
    
    ;; Set new timer
    (let ((new-timer 
           (run-at-time lispbar-render-update-throttle nil
                        (lambda ()
                          (when (frame-live-p frame)
                            (lispbar-render--render-frame frame left center right)
                            (lispbar-render--update-content-hash frame left center right))
                          ;; Remove timer from list
                          (setq lispbar-render--update-timers
                                (cl-remove frame lispbar-render--update-timers :key #'car))))))
      (setq lispbar-render--update-timers
            (cons (cons frame new-timer)
                  (cl-remove frame lispbar-render--update-timers :key #'car))))))

;;; Public API

(defun lispbar-render-update (frame position content-list)
  "Update FRAME's POSITION with CONTENT-LIST.
POSITION should be 'left, 'center, or 'right.
CONTENT-LIST is a list of content items to display."
  (unless (frame-live-p frame)
    (error "Frame is not live: %s" frame))
  
  (unless (memq position '(left center right))
    (error "Invalid position: %s (must be left, center, or right)" position))
  
  (lispbar-log 'debug "Updating %s position for frame %s with %d items"
               position frame (length content-list))
  
  ;; Get current content for other positions
  (let* ((frame-info (cl-find-if (lambda (info)
                                   (eq (plist-get info :frame) frame))
                                 (lispbar-get-frames)))
         (current-left (when (eq position 'left) content-list))
         (current-center (when (eq position 'center) content-list))
         (current-right (when (eq position 'right) content-list)))
    
    ;; Update content immediately if changed
    (when (lispbar-render--content-changed-p frame current-left current-center current-right)
      (if lispbar-render-async-updates
          (lispbar-render--update-frame-throttled frame current-left current-center current-right)
        (progn
          (lispbar-render--render-frame frame current-left current-center current-right)
          (lispbar-render--update-content-hash frame current-left current-center current-right))))))

(defun lispbar-render-update-all (frame left center right)
  "Update FRAME with content for all positions.
LEFT, CENTER, and RIGHT are lists of content items."
  (unless (frame-live-p frame)
    (error "Frame is not live: %s" frame))
  
  (lispbar-log 'debug "Updating all positions for frame %s" frame)
  
  ;; Check if content has changed
  (when (lispbar-render--content-changed-p frame left center right)
    (if lispbar-render-async-updates
        (lispbar-render--update-frame-throttled frame left center right)
      (progn
        (lispbar-render--render-frame frame left center right)
        (lispbar-render--update-content-hash frame left center right)))))

(defun lispbar-render-clear (frame &optional position)
  "Clear content for FRAME.
If POSITION is specified, clear only that position.
POSITION can be 'left, 'center, 'right, or nil for all."
  (unless (frame-live-p frame)
    (error "Frame is not live: %s" frame))
  
  (lispbar-log 'debug "Clearing %s for frame %s" 
               (or position "all positions") frame)
  
  (if position
      (lispbar-render-update frame position nil)
    (lispbar-render-update-all frame nil nil nil)))

(defun lispbar-render-refresh (frame)
  "Force refresh of FRAME's display without changing content."
  (unless (frame-live-p frame)
    (error "Frame is not live: %s" frame))
  
  (lispbar-log 'debug "Refreshing frame %s" frame)
  
  ;; Force update by clearing content hash
  (setq lispbar-render--frame-content
        (cl-remove frame lispbar-render--frame-content :key #'car))
  
  ;; Trigger update with empty content to refresh display
  (lispbar-render-update-all frame nil nil nil))

;;; Utility Functions for Modules

(defun lispbar-render-propertize-text (text &rest properties)
  "Apply PROPERTIES to TEXT for enhanced rendering.
This is a convenience function for modules to create propertized text."
  (apply #'propertize text properties))

(defun lispbar-render-format-with-icon (icon text &optional face)
  "Format TEXT with ICON prefix, optionally applying FACE.
Returns propertized text suitable for rendering."
  (let ((formatted (concat icon " " text)))
    (if face
        (propertize formatted 'face face)
      formatted)))

;;; Lifecycle Management

(defun lispbar-render--cleanup-frame-resources (frame)
  "Clean up all resources associated with FRAME."
  (lispbar-log 'debug "Cleaning up render resources for frame %s" frame)
  
  ;; Cancel any pending timers
  (let ((timer (cdr (assq frame lispbar-render--update-timers))))
    (when timer
      (cancel-timer timer)))
  
  ;; Clean up buffer
  (lispbar-render--cleanup-buffer frame)
  
  ;; Remove from tracking lists
  (setq lispbar-render--update-timers
        (cl-remove frame lispbar-render--update-timers :key #'car)
        lispbar-render--frame-content
        (cl-remove frame lispbar-render--frame-content :key #'car)))

;;;###autoload
(defun lispbar-render-init ()
  "Initialize the Lispbar rendering system.
This function sets up the rendering engine and integrates with lispbar-core."
  (interactive)
  (when lispbar-render--initialized
    (lispbar-log 'warning "Lispbar render system already initialized")
    (return-from lispbar-render-init))
  
  (lispbar-log 'info "Initializing Lispbar rendering system")
  
  ;; Register cleanup function with core
  (lispbar--add-cleanup-function #'lispbar-render-cleanup)
  
  ;; Set up frame cleanup hooks
  (add-hook 'delete-frame-functions #'lispbar-render--cleanup-frame-resources)
  
  (setq lispbar-render--initialized t)
  (lispbar-log 'info "Lispbar rendering system initialized"))

;;;###autoload
(defun lispbar-render-cleanup ()
  "Clean up the Lispbar rendering system.
This function removes all buffers and timers."
  (interactive)
  (lispbar-log 'info "Cleaning up Lispbar rendering system")
  
  ;; Cancel all timers
  (dolist (timer-entry lispbar-render--update-timers)
    (cancel-timer (cdr timer-entry)))
  
  ;; Clean up all buffers
  (dolist (buffer-entry lispbar-render--frame-buffers)
    (let ((buffer (cdr buffer-entry)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  
  ;; Remove hooks
  (remove-hook 'delete-frame-functions #'lispbar-render--cleanup-frame-resources)
  
  ;; Reset state
  (setq lispbar-render--frame-buffers nil
        lispbar-render--frame-content nil
        lispbar-render--update-timers nil
        lispbar-render--initialized nil)
  
  (lispbar-log 'info "Lispbar rendering system cleanup complete"))

(provide 'lispbar-render)
;;; lispbar-render.el ends here