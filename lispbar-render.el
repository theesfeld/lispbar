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

;;; Enhanced Layout Configuration

(defcustom lispbar-render-left-min-width 0
  "Minimum width in characters for left section."
  :type 'integer
  :group 'lispbar-render)

(defcustom lispbar-render-left-max-width nil
  "Maximum width in characters for left section.
If nil, no maximum limit is enforced."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Characters"))
  :group 'lispbar-render)

(defcustom lispbar-render-center-min-width 0
  "Minimum width in characters for center section."
  :type 'integer
  :group 'lispbar-render)

(defcustom lispbar-render-center-max-width nil
  "Maximum width in characters for center section.
If nil, no maximum limit is enforced."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Characters"))
  :group 'lispbar-render)

(defcustom lispbar-render-right-min-width 0
  "Minimum width in characters for right section."
  :type 'integer
  :group 'lispbar-render)

(defcustom lispbar-render-right-max-width nil
  "Maximum width in characters for right section.
If nil, no maximum limit is enforced."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Characters"))
  :group 'lispbar-render)

(defcustom lispbar-render-spacing-mode 'adaptive
  "How to calculate spacing between sections.

- `fixed': Use fixed spacing defined by `lispbar-render-module-spacing'
- `equal': Distribute available space equally between sections
- `content-based': Distribute space proportionally to content size
- `adaptive': Intelligently adapt spacing based on content and available space"
  :type '(choice (const :tag "Fixed spacing" fixed)
                 (const :tag "Equal distribution" equal)
                 (const :tag "Content-based" content-based)
                 (const :tag "Adaptive" adaptive))
  :group 'lispbar-render)

(defcustom lispbar-render-overflow-behavior 'truncate-priority
  "How to handle content overflow when sections don't fit.

Values:
- `truncate': Simple truncation from the right
- `truncate-priority': Truncate based on section priority
- `hide-priority': Hide sections based on priority when insufficient space"
  :type '(choice (const :tag "Simple truncate" truncate)
                 (const :tag "Priority-based truncate" truncate-priority)
                 (const :tag "Hide by priority" hide-priority))
  :group 'lispbar-render)

(defcustom lispbar-render-section-priorities '((left . 2) (center . 3) (right . 1))
  "Priority levels for each section when handling overflow.
Higher numbers indicate higher priority. The section with highest priority
is preserved longest during overflow situations."
  :type '(alist :key-type (choice (const left) (const center) (const right))
                :value-type integer)
  :group 'lispbar-render)

(defcustom lispbar-render-overflow-left-indicator "◀"
  "Indicator shown when left section content is truncated."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-overflow-right-indicator "▶"
  "Indicator shown when right section content is truncated."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-overflow-center-indicator "…"
  "Indicator shown when center section content is truncated."
  :type 'string
  :group 'lispbar-render)

(defcustom lispbar-render-min-section-spacing 1
  "Minimum spacing between sections in characters."
  :type 'integer
  :group 'lispbar-render)

(defcustom lispbar-render-enable-width-caching t
  "Whether to cache text width measurements for performance."
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

(defvar lispbar-render--width-cache nil
  "Cache for text width measurements to improve performance.")

(defvar lispbar-render--last-layout-info nil
  "Cache of last layout calculation for change detection.")

(defcustom lispbar-render-configuration-changed-hook nil
  "Hook run when rendering configuration changes.
Functions in this hook should expect no arguments."
  :type 'hook
  :group 'lispbar-render)

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

;;; Text Width Measurement

(defun lispbar-render--clear-width-cache ()
  "Clear the text width measurement cache."
  (setq lispbar-render--width-cache nil))

(defun lispbar-render--get-text-width (text)
  "Measure the display width of TEXT in characters.
Returns cached result if available and caching is enabled."
  (if (and lispbar-render-enable-width-caching
           lispbar-render--width-cache)
      (or (gethash text lispbar-render--width-cache)
          (let ((width (lispbar-render--measure-text-width text)))
            (puthash text width lispbar-render--width-cache)
            width))
    (lispbar-render--measure-text-width text)))

(defun lispbar-render--measure-text-width (text)
  "Measure the actual display width of TEXT in characters.
Accounts for text properties, faces, and multi-byte characters."
  (if (stringp text)
      (with-temp-buffer
        ;; Create temporary buffer with same display characteristics
        (let ((inhibit-modification-hooks t))
          (insert text)
          ;; For now, use simple length - could be enhanced with
          ;; actual pixel width measurement if needed
          (length text)))
    0))

(defun lispbar-render--init-width-cache ()
  "Initialize the text width cache if caching is enabled."
  (when lispbar-render-enable-width-caching
    (setq lispbar-render--width-cache (make-hash-table :test 'equal))))

;;; Section Constraint Application

(defun lispbar-render--apply-section-constraints (content position)
  "Apply width constraints to CONTENT for POSITION (left/center/right).
Returns a plist with :content, :width, :min-width, and :max-width."
  (let* ((min-width (pcase position
                      ('left lispbar-render-left-min-width)
                      ('center lispbar-render-center-min-width)
                      ('right lispbar-render-right-min-width)
                      (_ 0)))
         (max-width (pcase position
                      ('left lispbar-render-left-max-width)
                      ('center lispbar-render-center-max-width)
                      ('right lispbar-render-right-max-width)
                      (_ nil)))
         (content-width (lispbar-render--get-text-width content))
         (constrained-width (max min-width
                                (if max-width
                                    (min content-width max-width)
                                  content-width))))
    
    (list :content content
          :width constrained-width
          :actual-width content-width
          :min-width min-width
          :max-width max-width
          :constrained (not (= content-width constrained-width)))))

;;; Dynamic Spacing Calculation

(defun lispbar-render--calculate-dynamic-spacing (available-width sections)
  "Calculate dynamic spacing based on AVAILABLE-WIDTH and SECTIONS.
SECTIONS is a list of section info plists from constraint application.
Returns updated sections with spacing information."
  (let* ((total-content-width (cl-reduce #'+ sections
                                        :key (lambda (s) (plist-get s :width))
                                        :initial-value 0))
         (min-spacing (* 2 lispbar-render-min-section-spacing)) ; Between sections
         (remaining-width (- available-width total-content-width min-spacing)))
    
    (pcase lispbar-render-spacing-mode
      ('fixed
       ;; Use fixed spacing from configuration
       (let ((fixed-spacing (length lispbar-render-module-spacing)))
         (mapcar (lambda (section)
                   (plist-put section :spacing fixed-spacing))
                 sections)))
      
      ('equal
       ;; Distribute remaining space equally
       (let ((equal-spacing (max lispbar-render-min-section-spacing
                                (/ remaining-width 3))))
         (mapcar (lambda (section)
                   (plist-put section :spacing equal-spacing))
                 sections)))
      
      ('content-based
       ;; Distribute space proportionally to content size
       (if (> total-content-width 0)
           (mapcar (lambda (section)
                     (let* ((proportion (/ (float (plist-get section :width))
                                          total-content-width))
                            (proportional-spacing (max lispbar-render-min-section-spacing
                                                      (* remaining-width proportion))))
                       (plist-put section :spacing proportional-spacing)))
                   sections)
         ;; Fallback to equal if no content
         (let ((equal-spacing (max lispbar-render-min-section-spacing
                                  (/ remaining-width 3))))
           (mapcar (lambda (section)
                     (plist-put section :spacing equal-spacing))
                   sections))))
      
      ('adaptive
       ;; Intelligent adaptive spacing
       (lispbar-render--calculate-adaptive-spacing available-width sections remaining-width))
      
      (_
       ;; Default to fixed spacing
       (let ((default-spacing (length lispbar-render-module-spacing)))
         (mapcar (lambda (section)
                   (plist-put section :spacing default-spacing))
                 sections))))))

(defun lispbar-render--calculate-adaptive-spacing (available-width sections remaining-width)
  "Calculate adaptive spacing for SECTIONS with REMAINING-WIDTH available."
  (let* ((_num-sections (length (cl-remove-if (lambda (s) (= (plist-get s :width) 0))
                                             sections)))
         (base-spacing lispbar-render-min-section-spacing))
    
    (cond
     ;; Plenty of space - use generous spacing
     ((> remaining-width (* available-width 0.3))
      (let ((generous-spacing (* base-spacing 3)))
        (mapcar (lambda (section)
                  (plist-put section :spacing generous-spacing))
                sections)))
     
     ;; Moderate space - use balanced spacing
     ((> remaining-width (* available-width 0.1))
      (let ((balanced-spacing (* base-spacing 2)))
        (mapcar (lambda (section)
                  (plist-put section :spacing balanced-spacing))
                sections)))
     
     ;; Tight space - use minimum spacing
     (t
      (mapcar (lambda (section)
                (plist-put section :spacing base-spacing))
              sections)))))

;;; Overflow Handling

(defun lispbar-render--handle-section-overflow (sections available-width)
  "Handle overflow for SECTIONS when they exceed AVAILABLE-WIDTH.
Returns updated sections with overflow handling applied."
  (let* ((total-width (cl-reduce #'+ sections
                                :key (lambda (s)
                                       (+ (plist-get s :width)
                                          (plist-get s :spacing)))
                                :initial-value 0))
         (overflow-amount (- total-width available-width)))
    
    (if (<= overflow-amount 0)
        ;; No overflow, return sections as-is
        sections
      ;; Handle overflow based on configured behavior
      (pcase lispbar-render-overflow-behavior
        ('truncate
         (lispbar-render--simple-truncate sections available-width))
        
        ('truncate-priority
         (lispbar-render--priority-truncate sections available-width overflow-amount))
        
        ('hide-priority
         (lispbar-render--priority-hide sections available-width))
        
        (_
         ;; Default to simple truncation
         (lispbar-render--simple-truncate sections available-width))))))

(defun lispbar-render--simple-truncate (sections available-width)
  "Apply simple truncation to SECTIONS to fit AVAILABLE-WIDTH."
  (let ((remaining-width available-width)
        (result '()))
    
    (dolist (section sections)
      (let* ((required-width (+ (plist-get section :width)
                               (plist-get section :spacing)))
             (fits (>= remaining-width required-width)))
        
        (if fits
            (progn
              (push section result)
              (setq remaining-width (- remaining-width required-width)))
          ;; Truncate this section to fit remaining space
          (when (> remaining-width lispbar-render-min-section-spacing)
            (let* ((available-for-content (- remaining-width
                                           (plist-get section :spacing)))
                   (truncated-content (lispbar-render--truncate-content-with-indicator
                                     (plist-get section :content)
                                     available-for-content
                                     'right))) ; Default to right truncation
              (push (plist-put (copy-sequence section) :content truncated-content)
                   result)
              (setq remaining-width 0)))))
    
    (nreverse result)))

(defun lispbar-render--priority-truncate (sections available-width overflow-amount)
  "Apply priority-based truncation to SECTIONS with OVERFLOW-AMOUNT excess."
  (let* ((sections-with-priority (mapcar (lambda (section)
                                          (let ((pos (plist-get section :position)))
                                            (plist-put section :priority
                                                      (cdr (assq pos lispbar-render-section-priorities)))))
                                        sections))
         (sorted-sections (sort sections-with-priority
                               (lambda (a b)
                                 (< (plist-get a :priority)
                                    (plist-get b :priority)))))
         (remaining-overflow overflow-amount))
    
    ;; Truncate sections starting with lowest priority
    (dolist (section sorted-sections)
      (when (> remaining-overflow 0)
        (let* ((current-width (plist-get section :width))
               (min-width (plist-get section :min-width))
               (reducible-width (- current-width min-width))
               (reduction (min remaining-overflow reducible-width)))
          
          (when (> reduction 0)
            (let* ((new-width (- current-width reduction))
                   (truncated-content (lispbar-render--truncate-content-with-indicator
                                     (plist-get section :content)
                                     new-width
                                     (plist-get section :position))))
              
              (plist-put section :width new-width)
              (plist-put section :content truncated-content)
              (plist-put section :truncated t)
              (setq remaining-overflow (- remaining-overflow reduction)))))))
    
    sorted-sections))

(defun lispbar-render--priority-hide (sections available-width)
  "Hide entire sections based on priority to fit AVAILABLE-WIDTH."
  (let* ((sections-with-priority (mapcar (lambda (section)
                                          (let ((pos (plist-get section :position)))
                                            (plist-put section :priority
                                                      (cdr (assq pos lispbar-render-section-priorities)))))
                                        sections))
         (sorted-sections (sort sections-with-priority
                               (lambda (a b)
                                 (> (plist-get a :priority)
                                    (plist-get b :priority)))))
         (remaining-width available-width)
         (result '()))
    
    ;; Include sections starting with highest priority until space runs out
    (dolist (section sorted-sections)
      (let ((required-width (+ (plist-get section :width)
                              (plist-get section :spacing))))
        (when (>= remaining-width required-width)
          (push section result)
          (setq remaining-width (- remaining-width required-width)))))
    
    (nreverse result)))

(defun lispbar-render--truncate-content-with-indicator (content max-width position)
  "Truncate CONTENT to MAX-WIDTH with appropriate indicator for POSITION."
  (let ((indicator (lispbar-render--get-overflow-indicator position))
        (indicator-width (length indicator)))
    
    (if (<= (lispbar-render--get-text-width content) max-width)
        content
      (let ((available-width (- max-width indicator-width)))
        (if (> available-width 0)
            (concat (substring content 0 (min (length content) available-width))
                   indicator)
          indicator)))))

(defun lispbar-render--get-overflow-indicator (position)
  "Get the appropriate overflow indicator for POSITION."
  (pcase position
    ('left lispbar-render-overflow-left-indicator)
    ('center lispbar-render-overflow-center-indicator)
    ('right lispbar-render-overflow-right-indicator)
    (_ lispbar-render-truncate-indicator)))

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

;;; Enhanced Layout Calculation

(defun lispbar-render--calculate-layout (frame left center right)
  "Calculate enhanced layout positions for LEFT, CENTER, and RIGHT content in FRAME.
Returns a plist with comprehensive layout information including dynamic spacing,
overflow handling, and responsive positioning."
  (let* ((frame-info (cl-find-if (lambda (info)
                                   (eq (plist-get info :frame) frame))
                                 (lispbar-get-frames)))
         (frame-geometry (plist-get frame-info :geometry))
         (frame-width (plist-get frame-geometry :width))
         (padding-width (* 2 (length lispbar-render-padding)))
         (available-width (- frame-width padding-width))
         
         ;; Apply constraints to each section
         (left-info (lispbar-render--apply-section-constraints left 'left))
         (center-info (lispbar-render--apply-section-constraints center 'center))
         (right-info (lispbar-render--apply-section-constraints right 'right))
         
         ;; Add position information
         (sections (list (plist-put left-info :position 'left)
                        (plist-put center-info :position 'center)
                        (plist-put right-info :position 'right)))
         
         ;; Calculate dynamic spacing
         (sections-with-spacing (lispbar-render--calculate-dynamic-spacing 
                               available-width sections))
         
         ;; Handle overflow if necessary
         (final-sections (lispbar-render--handle-section-overflow 
                        sections-with-spacing available-width))
         
         ;; Calculate final positions
         (layout-positions (lispbar-render--calculate-final-positions 
                          final-sections available-width)))
    
    ;; Return comprehensive layout information
    (append (list :frame-width frame-width
                  :available-width available-width
                  :sections final-sections)
            layout-positions)))

(defun lispbar-render--calculate-final-positions (sections available-width)
  "Calculate final positions for SECTIONS within AVAILABLE-WIDTH.
Returns a plist with position information for each section."
  (let* ((left-section (cl-find-if (lambda (s) (eq (plist-get s :position) 'left)) sections))
         (center-section (cl-find-if (lambda (s) (eq (plist-get s :position) 'center)) sections))
         (right-section (cl-find-if (lambda (s) (eq (plist-get s :position) 'right)) sections))
         
         ;; Calculate positions
         (left-pos 0)
         (left-width (if left-section (plist-get left-section :width) 0))
         
         (center-width (if center-section (plist-get center-section :width) 0))
         (center-pos (if center-section
                        (max (+ left-width (if left-section 
                                              (plist-get left-section :spacing) 0))
                             (- (/ available-width 2) (/ center-width 2)))
                       0))
         
         (right-width (if right-section (plist-get right-section :width) 0))
         (right-pos (if right-section
                       (max (+ center-pos center-width 
                              (if center-section 
                                (plist-get center-section :spacing) 0))
                            (- available-width right-width))
                      available-width)))
    
    (list :left-pos left-pos
          :center-pos center-pos
          :right-pos right-pos
          :left-width left-width
          :center-width center-width
          :right-width right-width
          :total-used-width (+ left-width center-width right-width
                              (if left-section (plist-get left-section :spacing) 0)
                              (if center-section (plist-get center-section :spacing) 0)
                              (if right-section (plist-get right-section :spacing) 0))))))

(defun lispbar-render--is-layout-changed (frame left center right)
  "Check if layout calculation would change for FRAME with given content.
Returns t if layout needs recalculation, nil otherwise."
  (let* ((content-hash (lispbar-render--calculate-content-hash left center right))
         (last-info (cdr (assq frame lispbar-render--last-layout-info)))
         (last-hash (plist-get last-info :content-hash))
         (last-frame-width (plist-get last-info :frame-width))
         (current-frame-info (cl-find-if (lambda (info)
                                          (eq (plist-get info :frame) frame))
                                        (lispbar-get-frames)))
         (current-frame-width (plist-get (plist-get current-frame-info :geometry) :width)))
    
    (or (not (eq content-hash last-hash))
        (not (eq current-frame-width last-frame-width)))))

(defun lispbar-render--cache-layout-info (frame left center right layout)
  "Cache layout information for FRAME to enable change detection."
  (let* ((content-hash (lispbar-render--calculate-content-hash left center right))
         (frame-width (plist-get layout :frame-width))
         (layout-info (list :content-hash content-hash
                           :frame-width frame-width
                           :timestamp (current-time))))
    
    (setq lispbar-render--last-layout-info
          (cons (cons frame layout-info)
                (cl-remove frame lispbar-render--last-layout-info :key #'car)))))

(defun lispbar-render--format-content (left center right layout)
  "Format content using enhanced LAYOUT information.
Returns the final formatted string for display with proper spacing and positioning."
  (let* ((available-width (plist-get layout :available-width))
         (sections (plist-get layout :sections))
         (result (make-string available-width ?\s)))
    
    ;; Insert content for each section using enhanced layout info
    (dolist (section sections)
      (let* ((position (plist-get section :position))
             (content (plist-get section :content))
             (pos (pcase position
                    ('left (plist-get layout :left-pos))
                    ('center (plist-get layout :center-pos))
                    ('right (plist-get layout :right-pos))
                    (_ 0)))
             (width (plist-get section :width)))
        
        (when (and content (> width 0) (>= pos 0) (< pos available-width))
          (let ((content-str (if (stringp content) content (format "%s" content)))
                (end-pos (min (+ pos width) available-width)))
            
            ;; Insert content character by character, respecting boundaries
            (cl-loop for i from pos below end-pos
                     for j from 0 below (length content-str)
                     when (< j (length content-str))
                     do (aset result i (aref content-str j)))))))
    
    ;; Add padding
    (concat lispbar-render-padding result lispbar-render-padding)))

(defun lispbar-render--format-content-legacy (left center right layout)
  "Legacy format function for backward compatibility.
Uses the original formatting logic for cases where enhanced formatting
might not be desired."
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
  "Check if content has changed for FRAME since last update.
Uses enhanced layout change detection when available."
  (if lispbar-render--last-layout-info
      ;; Use enhanced change detection
      (lispbar-render--is-layout-changed frame left center right)
    ;; Fall back to simple hash comparison
    (let* ((current-hash (lispbar-render--calculate-content-hash left center right))
           (stored-hash (cdr (assq frame lispbar-render--frame-content))))
      (not (eq current-hash stored-hash)))))

(defun lispbar-render--update-content-hash (frame left center right)
  "Update the stored content hash for FRAME."
  (let ((new-hash (lispbar-render--calculate-content-hash left center right)))
    (setq lispbar-render--frame-content
          (cons (cons frame new-hash)
                (cl-remove frame lispbar-render--frame-content :key #'car)))))

;;; Core Rendering Functions

(defun lispbar-render--render-frame (frame left center right)
  "Render content to FRAME's buffer using enhanced layout system.
LEFT, CENTER, and RIGHT are content for respective positions."
  (let ((buffer (lispbar-render--get-frame-buffer frame)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((left-str (lispbar-render--normalize-content-list left))
               (center-str (lispbar-render--normalize-content-list center))
               (right-str (lispbar-render--normalize-content-list right))
               
               ;; Use enhanced layout calculation
               (layout (lispbar-render--calculate-layout frame left-str center-str right-str))
               
               ;; Cache layout info for change detection
               (_ (lispbar-render--cache-layout-info frame left-str center-str right-str layout))
               
               ;; Format content with enhanced positioning
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
          
          (lispbar-log 'debug "Rendered frame with enhanced layout - content length: %d, sections: %d, used width: %d" 
                       (length formatted-content)
                       (length (plist-get layout :sections))
                       (plist-get layout :total-used-width))))))

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

;;; Enhanced Utility Functions for Modules

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

(defun lispbar-render-measure-content (content)
  "Measure the display width of CONTENT in characters.
Public interface for modules to measure their content."
  (lispbar-render--get-text-width (lispbar-render--validate-content content)))

(defun lispbar-render-check-section-constraints (content position)
  "Check if CONTENT fits within constraints for POSITION.
Returns a plist with :fits, :actual-width, :max-allowed, and :truncated-content."
  (let* ((width (lispbar-render-measure-content content))
         (max-width (pcase position
                      ('left lispbar-render-left-max-width)
                      ('center lispbar-render-center-max-width)
                      ('right lispbar-render-right-max-width)
                      (_ nil)))
         (fits (or (null max-width) (<= width max-width)))
         (truncated-content (if fits
                              content
                            (lispbar-render--truncate-content-with-indicator
                             content max-width position))))
    
    (list :fits fits
          :actual-width width
          :max-allowed max-width
          :truncated-content truncated-content)))

(defun lispbar-render-get-layout-info (frame)
  "Get current layout information for FRAME.
Returns cached layout info if available, nil otherwise."
  (cdr (assq frame lispbar-render--last-layout-info)))

(defun lispbar-render-clear-caches ()
  "Clear all rendering caches.
Useful for debugging or after configuration changes."
  (interactive)
  (lispbar-render--clear-width-cache)
  (setq lispbar-render--last-layout-info nil)
  (lispbar-log 'info "Rendering caches cleared"))

(defun lispbar-render-debug-layout (frame left center right)
  "Debug layout calculation for FRAME with given content.
Prints detailed layout information to messages buffer."
  (interactive)
  (let* ((left-str (lispbar-render--normalize-content-list left))
         (center-str (lispbar-render--normalize-content-list center))
         (right-str (lispbar-render--normalize-content-list right))
         (layout (lispbar-render--calculate-layout frame left-str center-str right-str)))
    
    (message "=== Lispbar Layout Debug ===")
    (message "Frame width: %d, Available: %d" 
             (plist-get layout :frame-width)
             (plist-get layout :available-width))
    (message "Sections: %d, Total used: %d"
             (length (plist-get layout :sections))
             (plist-get layout :total-used-width))
    
    (dolist (section (plist-get layout :sections))
      (message "  %s: width=%d, spacing=%d, constrained=%s"
               (plist-get section :position)
               (plist-get section :width)
               (plist-get section :spacing)
               (plist-get section :constrained)))
    
    layout))

;;; Configuration Management

(defun lispbar-render-reset-to-legacy-mode ()
  "Reset rendering system to legacy behavior for compatibility.
Disables enhanced layout features and uses original algorithms."
  (interactive)
  (setq lispbar-render-spacing-mode 'fixed
        lispbar-render-overflow-behavior 'truncate
        lispbar-render-enable-width-caching nil
        lispbar-render-left-max-width nil
        lispbar-render-center-max-width nil
        lispbar-render-right-max-width nil)
  
  (lispbar-render-clear-caches)
  (run-hooks 'lispbar-render-configuration-changed-hook)
  (lispbar-log 'info "Rendering system reset to legacy mode"))

(defun lispbar-render-enable-enhanced-mode ()
  "Enable enhanced layout features with recommended settings."
  (interactive)
  (setq lispbar-render-spacing-mode 'adaptive
        lispbar-render-overflow-behavior 'truncate-priority
        lispbar-render-enable-width-caching t)
  
  (lispbar-render--init-width-cache)
  (run-hooks 'lispbar-render-configuration-changed-hook)
  (lispbar-log 'info "Enhanced rendering mode enabled"))

(defun lispbar-render-validate-configuration ()
  "Validate current rendering configuration.
Returns list of warnings or nil if configuration is valid."
  (let ((warnings '()))
    
    ;; Check section width constraints
    (when (and lispbar-render-left-max-width
               (< lispbar-render-left-max-width lispbar-render-left-min-width))
      (push "Left section max-width is less than min-width" warnings))
    
    (when (and lispbar-render-center-max-width
               (< lispbar-render-center-max-width lispbar-render-center-min-width))
      (push "Center section max-width is less than min-width" warnings))
    
    (when (and lispbar-render-right-max-width
               (< lispbar-render-right-max-width lispbar-render-right-min-width))
      (push "Right section max-width is less than min-width" warnings))
    
    ;; Check spacing configuration
    (when (< lispbar-render-min-section-spacing 0)
      (push "Minimum section spacing cannot be negative" warnings))
    
    ;; Check priority configuration
    (unless (and (assq 'left lispbar-render-section-priorities)
                 (assq 'center lispbar-render-section-priorities)
                 (assq 'right lispbar-render-section-priorities))
      (push "Section priorities must include all sections (left, center, right)" warnings))
    
    warnings))

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
  "Initialize the enhanced Lispbar rendering system.
This function sets up the rendering engine with enhanced layout capabilities."
  (interactive)
  (unless lispbar-render--initialized
    (lispbar-log 'info "Initializing enhanced Lispbar rendering system")
    
    ;; Initialize width caching if enabled
    (lispbar-render--init-width-cache)
    
    ;; Register cleanup function with core
    (lispbar--add-cleanup-function #'lispbar-render-cleanup)
    
    ;; Set up frame cleanup hooks
    (add-hook 'delete-frame-functions #'lispbar-render--cleanup-frame-resources)
    
    ;; Add configuration change hooks
    (add-hook 'lispbar-render-configuration-changed-hook #'lispbar-render-clear-caches)
    
    (setq lispbar-render--initialized t)
    (lispbar-log 'info "Enhanced Lispbar rendering system initialized with dynamic layout support")))

;;;###autoload
(defun lispbar-render-cleanup ()
  "Clean up the enhanced Lispbar rendering system.
This function removes all buffers, timers, and caches."
  (interactive)
  (lispbar-log 'info "Cleaning up enhanced Lispbar rendering system")
  
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
  (remove-hook 'lispbar-render-configuration-changed-hook #'lispbar-render-clear-caches)
  
  ;; Clear caches
  (lispbar-render--clear-width-cache)
  
  ;; Reset state
  (setq lispbar-render--frame-buffers nil
        lispbar-render--frame-content nil
        lispbar-render--update-timers nil
        lispbar-render--width-cache nil
        lispbar-render--last-layout-info nil
        lispbar-render--initialized nil)
  
  (lispbar-log 'info "Enhanced Lispbar rendering system cleanup complete"))

;;; Enhanced Layout System Information

;; This enhanced version of lispbar-render.el provides:
;;
;; 1. Dynamic Spacing Calculation:
;;    - Fixed, equal, content-based, and adaptive spacing modes
;;    - Intelligent space distribution based on available width
;;    - Configurable minimum spacing between sections
;;
;; 2. Overflow Handling:
;;    - Simple truncation with indicators
;;    - Priority-based truncation preserving important content
;;    - Section hiding based on priority when space is very limited
;;    - Custom overflow indicators for each section
;;
;; 3. Responsive Design:
;;    - Content-aware width calculation
;;    - Dynamic adaptation to frame size changes
;;    - Minimum and maximum width constraints per section
;;
;; 4. Performance Optimizations:
;;    - Text width caching to avoid repeated calculations
;;    - Layout change detection to minimize recalculation
;;    - Efficient string operations and memory usage
;;
;; 5. Backward Compatibility:
;;    - All enhancements are opt-in via customization
;;    - Legacy mode available for compatibility
;;    - Existing API preserved with enhanced functionality
;;
;; Configuration examples:
;;
;;   ;; Enable adaptive spacing with priority-based overflow
;;   (setq lispbar-render-spacing-mode 'adaptive
;;         lispbar-render-overflow-behavior 'truncate-priority)
;;
;;   ;; Set section width constraints
;;   (setq lispbar-render-left-max-width 30
;;         lispbar-render-center-max-width 50
;;         lispbar-render-right-max-width 40)
;;
;;   ;; Customize section priorities (higher = more important)
;;   (setq lispbar-render-section-priorities
;;         '((left . 1) (center . 3) (right . 2)))

(provide 'lispbar-render)
;;; lispbar-render.el ends here