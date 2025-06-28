;;; test-enhanced-monitors.el --- Test script for enhanced multi-monitor support

;; Copyright (C) 2025 Free Software Foundation, Inc.

;;; Commentary:

;; This script demonstrates and tests the enhanced multi-monitor support
;; in lispbar-core.el. It shows how to use the new functionality for
;; monitor detection, configuration, and management.

;;; Code:

(require 'lispbar-core)

(defun test-monitor-detection ()
  "Test enhanced monitor detection capabilities."
  (message "=== Testing Enhanced Monitor Detection ===")
  
  ;; Test different detection methods
  (dolist (method '(auto fallback))
    (message "Testing detection method: %s" method)
    (let ((lispbar-monitor-detection-method method))
      (let ((monitors (lispbar--detect-monitors)))
        (message "  Detected %d monitors" (length monitors))
        (dolist (monitor monitors)
          (message "    Monitor: %s (%dx%d+%d+%d) Primary:%s"
                   (plist-get monitor :id)
                   (plist-get monitor :width) (plist-get monitor :height)
                   (plist-get monitor :x) (plist-get monitor :y)
                   (plist-get monitor :primary)))))))

(defun test-monitor-configuration ()
  "Test per-monitor configuration system."
  (message "\n=== Testing Monitor Configuration ===")
  
  ;; Detect monitors first
  (setq lispbar--monitors (lispbar--detect-monitors))
  
  (when lispbar--monitors
    (let* ((test-monitor (car lispbar--monitors))
           (monitor-id (plist-get test-monitor :id)))
      
      (message "Testing configuration for monitor: %s" monitor-id)
      
      ;; Test setting custom configuration
      (lispbar-set-monitor-configuration 
       monitor-id
       '(:position bottom
         :height 32
         :background-color "blue"
         :margin-left 10
         :margin-right 10))
      
      ;; Test retrieving configuration
      (let ((config (lispbar-get-monitor-configuration monitor-id)))
        (message "  Position: %s" (plist-get config :position))
        (message "  Height: %d" (plist-get config :height))
        (message "  Background: %s" (plist-get config :background-color))
        (message "  Margins: %d, %d" 
                 (plist-get config :margin-left)
                 (plist-get config :margin-right)))
      
      ;; Test alias system
      (lispbar-set-monitor-alias monitor-id "Main Display")
      (message "  Alias: %s" (lispbar-get-monitor-alias monitor-id)))))

(defun test-monitor-utilities ()
  "Test monitor utility functions."
  (message "\n=== Testing Monitor Utilities ===")
  
  ;; Test monitor listing
  (let ((monitors (lispbar-list-monitors)))
    (message "Connected monitors: %d" (length monitors)))
  
  ;; Test primary monitor detection
  (let ((primary (lispbar-get-primary-monitor)))
    (when primary
      (message "Primary monitor: %s" (plist-get primary :id))))
  
  ;; Test validation
  (let ((issues (lispbar-validate-config)))
    (if issues
        (message "Configuration issues found: %s" issues)
      (message "Configuration validation passed"))))

(defun test-frame-geometry ()
  "Test enhanced frame geometry calculation."
  (message "\n=== Testing Frame Geometry ===")
  
  (setq lispbar--monitors (lispbar--detect-monitors))
  
  (when lispbar--monitors
    (let ((monitor (car lispbar--monitors)))
      (message "Testing geometry for monitor: %s" (plist-get monitor :id))
      
      ;; Test with default configuration
      (let ((geometry1 (lispbar--calculate-frame-geometry monitor)))
        (message "  Default geometry: %dx%d+%d+%d"
                 (plist-get geometry1 :width) (plist-get geometry1 :height)
                 (plist-get geometry1 :x) (plist-get geometry1 :y)))
      
      ;; Test with custom configuration
      (let* ((config '(:position bottom :height 40 :margin-left 20 :margin-right 20))
             (geometry2 (lispbar--calculate-frame-geometry monitor config)))
        (message "  Custom geometry: %dx%d+%d+%d"
                 (plist-get geometry2 :width) (plist-get geometry2 :height)
                 (plist-get geometry2 :x) (plist-get geometry2 :y))))))

(defun run-enhanced-monitor-tests ()
  "Run all enhanced monitor support tests."
  (interactive)
  (message "Starting Enhanced Multi-Monitor Support Tests")
  (message "=============================================")
  
  ;; Enable debug logging for tests
  (setq lispbar-debug t)
  
  (test-monitor-detection)
  (test-monitor-configuration)
  (test-monitor-utilities)
  (test-frame-geometry)
  
  (message "\n=== Test Summary ===")
  (message "Enhanced multi-monitor support tests completed")
  (message "Check *Messages* buffer for detailed output"))

;; Run tests when script is loaded
(when noninteractive
  (run-enhanced-monitor-tests))

(provide 'test-enhanced-monitors)
;;; test-enhanced-monitors.el ends here