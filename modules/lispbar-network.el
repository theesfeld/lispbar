;;; lispbar-network.el --- Network connection module for Lispbar -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lispbar Development Team
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (eieio "1.4"))
;; Keywords: network, connectivity, modules, lispbar
;; URL: https://github.com/yourusername/lispbar

;;; Commentary:

;; This module provides network connection status display for Lispbar with
;; comprehensive cross-platform network detection and monitoring. It supports
;; multiple connection types, WiFi signal strength monitoring, VPN detection,
;; and intelligent interface prioritization.
;;
;; The network module detects and displays information about active network
;; connections, providing users with at-a-glance visibility into their
;; connectivity status. It serves as a comprehensive example of system
;; integration within the Lispbar module framework.
;;
;; Features:
;; - Cross-platform network detection (Linux, macOS, Windows)
;; - Connection type classification (Ethernet, WiFi, VPN, Mobile)
;; - WiFi SSID display and signal strength monitoring
;; - VPN connection detection and status
;; - Interface priority ordering and filtering
;; - Configurable display formats and icons
;; - Intelligent caching to minimize system calls
;; - Graceful error handling and fallback modes
;; - Real-time updates every 10 seconds
;; - Comprehensive customization through Emacs custom system
;;
;; Supported Platforms:
;; - Linux: Uses /sys/class/net, ip command, iwconfig/iw for WiFi
;; - macOS: Uses ifconfig, networksetup for network information
;; - Windows: Uses ipconfig, netsh for network and WiFi data
;;
;; Connection Types Detected:
;; - Ethernet: Wired network connections (🌐)
;; - WiFi: Wireless connections with SSID and signal strength (📶)
;; - VPN: Tunnel interfaces and VPN software (🔒)
;; - Mobile: Cellular and mobile broadband connections (📱)
;;
;; Usage:
;;   (require 'lispbar-network)
;;   (lispbar-network-enable)
;;
;; Customization:
;;   M-x customize-group RET lispbar-network RET

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'lispbar-modules)

;;; Customization

(defgroup lispbar-network nil
  "Customization group for Lispbar network module."
  :group 'lispbar-modules
  :prefix "lispbar-network-")

(defcustom lispbar-network-position 'right
  "Position of the network module on the toolbar.
Can be \\='left, \\='center, or \\='right."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right))
  :group 'lispbar-network)

(defcustom lispbar-network-priority 60
  "Display priority for the network module.
Higher values are displayed first within the same position.
Range: 0-100, default: 60 (higher than clock's 50)."
  :type 'integer
  :group 'lispbar-network)

(defcustom lispbar-network-update-interval 10.0
  "Update interval for network status in seconds.
Default is 10.0 to balance responsiveness with system load."
  :type 'number
  :group 'lispbar-network)

(defcustom lispbar-network-cache-timeout 9.0
  "Cache timeout for network information in seconds.
Should be slightly less than update-interval to ensure fresh data."
  :type 'number
  :group 'lispbar-network)

(defcustom lispbar-network-display-mode 'primary
  "How to display multiple network connections.
\\='primary - Show only the highest priority active connection
\\='multiple - Show multiple active connections separated
\\='all - Show all active connections up to max-interfaces limit"
  :type '(choice (const :tag "Primary Only" primary)
                 (const :tag "Multiple Important" multiple)
                 (const :tag "All Active" all))
  :group 'lispbar-network)

(defcustom lispbar-network-max-interfaces 3
  "Maximum number of interfaces to display in 'all' mode."
  :type 'integer
  :group 'lispbar-network)

(defcustom lispbar-network-show-interface-name t
  "Whether to show interface names (e.g., \\='eth0, \\='wlan0).
When nil, shows friendly names like \\='Ethernet, \\='WiFi."
  :type 'boolean
  :group 'lispbar-network)

(defcustom lispbar-network-show-wifi-ssid t
  "Whether to display WiFi SSID names when connected."
  :type 'boolean
  :group 'lispbar-network)

(defcustom lispbar-network-show-wifi-strength t
  "Whether to display WiFi signal strength indicators."
  :type 'boolean
  :group 'lispbar-network)

(defcustom lispbar-network-show-vpn-name t
  "Whether to display VPN connection names when available."
  :type 'boolean
  :group 'lispbar-network)

(defcustom lispbar-network-hide-ssid nil
  "Hide WiFi SSID for privacy (shows 'WiFi' instead of actual SSID)."
  :type 'boolean
  :group 'lispbar-network)

(defcustom lispbar-network-signal-format 'bars
  "Format for displaying WiFi signal strength.
\\='bars - Use Unicode bar characters (▁▂▃▄▅▆▇█)
\\='percent - Show percentage (75%)
\\='both - Show both bars and percentage"
  :type '(choice (const :tag "Bar Characters" bars)
                 (const :tag "Percentage" percent)
                 (const :tag "Both" both))
  :group 'lispbar-network)

(defcustom lispbar-network-icons
  '((ethernet . "🌐")
    (wifi . "📶")
    (vpn . "🔒")
    (mobile . "📱")
    (unknown . "❓"))
  "Icons for different connection types."
  :type '(alist :key-type symbol :value-type string)
  :group 'lispbar-network)

(defcustom lispbar-network-wifi-signal-chars
  '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")
  "Unicode characters for WiFi signal strength display.
Ordered from weakest to strongest signal."
  :type '(repeat string)
  :group 'lispbar-network)

(defcustom lispbar-network-interface-blacklist
  '("lo" "lo0" "docker0" "br-" "virbr" "veth" "vmnet")
  "List of interface name patterns to ignore.
Supports exact matches and prefix matching (ending with '-')."
  :type '(repeat string)
  :group 'lispbar-network)

(defcustom lispbar-network-interface-filter nil
  "Optional whitelist of interfaces to monitor.
If nil, all non-blacklisted interfaces are monitored.
If set, only interfaces matching these patterns are shown."
  :type '(choice (const :tag "Monitor All" nil)
                 (repeat :tag "Specific Interfaces" string))
  :group 'lispbar-network)

(defcustom lispbar-network-separator " | "
  "Separator string between multiple network connections."
  :type 'string
  :group 'lispbar-network)

(defcustom lispbar-network-face 'lispbar-render-default
  "Face to use for network display."
  :type 'face
  :group 'lispbar-network)

(defcustom lispbar-network-vpn-face 'lispbar-render-important
  "Face to use for VPN connection display."
  :type 'face
  :group 'lispbar-network)

(defcustom lispbar-network-error-face 'lispbar-render-urgent
  "Face to use for network error display."
  :type 'face
  :group 'lispbar-network)

;;; Variables

(defvar lispbar-network--module-instance nil
  "Instance of the network module.")

(defvar lispbar-network--enabled nil
  "Whether the network module is currently enabled.")

(defvar lispbar-network--platform nil
  "Detected platform for network commands.")

(defvar lispbar-network--command-cache nil
  "Cache for command availability checks.")

;;; Platform Detection

(defun lispbar-network--detect-platform ()
  "Detect current platform for network detection.
Returns \\='linux, \\='macos, \\='windows, or \\='unknown."
  (cond
   ((eq system-type 'gnu/linux) 'linux)
   ((eq system-type 'darwin) 'macos)
   ((memq system-type '(windows-nt cygwin)) 'windows)
   (t 'unknown)))

(defun lispbar-network--command-available-p (command)
  "Check if COMMAND is available in PATH.
Results are cached to avoid repeated filesystem lookups."
  (let ((cached (assoc command lispbar-network--command-cache)))
    (if cached
        (cdr cached)
      (let ((available (executable-find command)))
        (push (cons command available) lispbar-network--command-cache)
        available))))

;;; Interface Classification

(defun lispbar-network--classify-interface (interface-name)
  "Classify INTERFACE-NAME into connection type.
Returns \\='ethernet, \\='wifi, \\='vpn, \\='mobile, or \\='unknown."
  (cond
   ;; Loopback interfaces
   ((member interface-name '("lo" "lo0")) 'loopback)
   
   ;; VPN and tunnel interfaces
   ((string-match-p "^\\(tun\\|tap\\|ppp\\|vpn\\|utun\\|wg\\)" interface-name) 'vpn)
   
   ;; WiFi interfaces
   ((string-match-p "^\\(wlan\\|wl\\|wifi\\|ath\\|ra\\|wlp\\|iwm\\|iwn\\)" interface-name) 'wifi)
   
   ;; Mobile/cellular interfaces
   ((string-match-p "^\\(wwan\\|rmnet\\|qmi\\|usb.*mobile\\|cdc.*wdm\\)" interface-name) 'mobile)
   
   ;; Ethernet interfaces
   ((string-match-p "^\\(eth\\|eno\\|enp\\|ens\\|em\\|p[0-9]p\\)" interface-name) 'ethernet)
   
   ;; macOS specific - en0 could be ethernet or WiFi, need additional detection
   ((and (eq lispbar-network--platform 'macos)
         (string-match-p "^en[0-9]+" interface-name)) 'ethernet) ; Default to ethernet, refine later
   
   ;; Virtual/container interfaces (should be filtered out)
   ((string-match-p "^\\(docker\\|br-\\|virbr\\|veth\\|vmnet\\)" interface-name) 'virtual)
   
   ;; Default
   (t 'unknown)))

(defun lispbar-network--interface-blacklisted-p (interface-name)
  "Check if INTERFACE-NAME is blacklisted.
Returns t if the interface should be ignored."
  (cl-some (lambda (pattern)
             (if (string-suffix-p "-" pattern)
                 ;; Prefix matching
                 (string-prefix-p (substring pattern 0 -1) interface-name)
               ;; Exact matching
               (string= pattern interface-name)))
           lispbar-network-interface-blacklist))

(defun lispbar-network--interface-whitelisted-p (interface-name)
  "Check if INTERFACE-NAME is whitelisted.
Returns t if the interface should be included, or if no whitelist is set."
  (if lispbar-network-interface-filter
      (cl-some (lambda (pattern)
                 (if (string-suffix-p "-" pattern)
                     ;; Prefix matching
                     (string-prefix-p (substring pattern 0 -1) interface-name)
                   ;; Exact matching
                   (string= pattern interface-name)))
               lispbar-network-interface-filter)
    t))

;;; Linux Network Detection

(defun lispbar-network--linux-get-interfaces ()
  "Get network interfaces on Linux.
Returns list of interface information plists."
  (let ((interfaces nil))
    (condition-case err
        (progn
          ;; Get interface list from /sys/class/net
          (when (file-directory-p "/sys/class/net")
            (dolist (iface (directory-files "/sys/class/net" nil "^[^.]"))
              (let* ((type (lispbar-network--classify-interface iface))
                     (operstate-file (format "/sys/class/net/%s/operstate" iface))
                     (state (when (file-readable-p operstate-file)
                              (string-trim (with-temp-buffer
                                           (insert-file-contents operstate-file)
                                           (buffer-string)))))
                     (up-p (member state '("up" "unknown"))))
                
                ;; Only include active, non-blacklisted, whitelisted interfaces
                (when (and up-p
                           (not (eq type 'loopback))
                           (not (eq type 'virtual))
                           (not (lispbar-network--interface-blacklisted-p iface))
                           (lispbar-network--interface-whitelisted-p iface))
                  (let ((info (list :name iface
                                    :type type
                                    :state state
                                    :addresses (lispbar-network--linux-get-addresses iface))))
                    
                    ;; Add WiFi-specific information
                    (when (eq type 'wifi)
                      (let ((wifi-info (lispbar-network--linux-get-wifi-info iface)))
                        (when wifi-info
                          (setq info (append info wifi-info)))))
                    
                    (push info interfaces))))))
      (error
       (lispbar-modules--log 'error "Linux interface detection failed: %s" err)))
    
    (nreverse interfaces)))

(defun lispbar-network--linux-get-addresses (interface)
  "Get IP addresses for INTERFACE on Linux."
  (condition-case nil
      (when (lispbar-network--command-available-p "ip")
        (let ((output (shell-command-to-string 
                      (format "ip addr show %s 2>/dev/null" 
                              (shell-quote-argument interface)))))
          (when output
            (let ((addresses nil))
              ;; Extract IPv4 addresses
              (while (string-match "inet \\([0-9.]+\\)" output)
                (push (match-string 1 output) addresses)
                (setq output (substring output (match-end 0))))
              addresses))))
    (error nil)))

(defun lispbar-network--linux-get-wifi-info (interface)
  "Get WiFi information for INTERFACE on Linux.
Returns plist with :ssid and :signal-strength."
  (let ((info nil))
    (condition-case nil
        (progn
          ;; Try iwgetid for SSID
          (when (lispbar-network--command-available-p "iwgetid")
            (let ((ssid-output (shell-command-to-string 
                               (format "iwgetid %s --raw 2>/dev/null" 
                                       (shell-quote-argument interface)))))
              (when (and ssid-output (not (string-empty-p (string-trim ssid-output))))
                (setq info (plist-put info :ssid (string-trim ssid-output))))))
          
          ;; Try to get signal strength from /proc/net/wireless
          (let ((wireless-file "/proc/net/wireless"))
            (when (file-readable-p wireless-file)
              (with-temp-buffer
                (insert-file-contents wireless-file)
                (goto-char (point-min))
                (when (re-search-forward 
                       (format "^\\s-*%s:" (regexp-quote interface)) nil t)
                  (when (re-search-forward "\\([0-9.-]+\\)\\s-+\\([0-9.-]+\\)" 
                                         (line-end-position) t)
                    (let ((signal-dbm (string-to-number (match-string 2))))
                      ;; Convert dBm to percentage (rough approximation)
                      ;; -30 dBm = 100%, -90 dBm = 0%
                      (let ((percentage (max 0 (min 100 (+ 100 (/ (+ signal-dbm 30) 0.6))))))
                        (setq info (plist-put info :signal-strength percentage)))))))))
          
          ;; Fallback: try iwconfig for signal info
          (when (and (not (plist-get info :signal-strength))
                     (lispbar-network--command-available-p "iwconfig"))
            (let ((iwconfig-output (shell-command-to-string 
                                   (format "iwconfig %s 2>/dev/null" 
                                           (shell-quote-argument interface)))))
              (when (string-match "Signal level=\\([0-9-]+\\)" iwconfig-output)
                (let ((signal-dbm (string-to-number (match-string 1 iwconfig-output))))
                  (let ((percentage (max 0 (min 100 (+ 100 (/ (+ signal-dbm 30) 0.6))))))
                    (setq info (plist-put info :signal-strength percentage))))))))
      (error nil))
    
    info))

;;; macOS Network Detection

(defun lispbar-network--macos-get-interfaces ()
  "Get network interfaces on macOS.
Returns list of interface information plists."
  (let ((interfaces nil))
    (condition-case err
        (when (lispbar-network--command-available-p "ifconfig")
          (let ((ifconfig-output (shell-command-to-string "ifconfig -a 2>/dev/null")))
            (dolist (line (split-string ifconfig-output "\n"))
              (when (string-match "^\\([^:[:space:]]+\\):" line)
                (let* ((iface (match-string 1 line))
                       (type (lispbar-network--classify-interface iface)))
                  
                  ;; Refine macOS interface classification
                  (when (and (eq type 'ethernet) 
                             (string-match-p "^en[0-9]+" iface))
                    ;; Check if it's actually WiFi
                    (when (lispbar-network--macos-interface-is-wifi-p iface)
                      (setq type 'wifi)))
                  
                  ;; Check if interface is up and has useful configuration
                  (when (and (string-match "status: active\\|inet " ifconfig-output)
                             (not (eq type 'loopback))
                             (not (eq type 'virtual))
                             (not (lispbar-network--interface-blacklisted-p iface))
                             (lispbar-network--interface-whitelisted-p iface))
                    
                    (let ((info (list :name iface
                                      :type type
                                      :state "up"
                                      :addresses (lispbar-network--macos-get-addresses iface))))
                      
                      ;; Add WiFi-specific information
                      (when (eq type 'wifi)
                        (let ((wifi-info (lispbar-network--macos-get-wifi-info iface)))
                          (when wifi-info
                            (setq info (append info wifi-info)))))
                      
                      (push info interfaces))))))))
      (error
       (lispbar-modules--log 'error "macOS interface detection failed: %s" err)))
    
    (nreverse interfaces)))

(defun lispbar-network--macos-interface-is-wifi-p (interface)
  "Check if INTERFACE is a WiFi interface on macOS."
  (condition-case nil
      (when (lispbar-network--command-available-p "networksetup")
        (let ((output (shell-command-to-string 
                      (format "networksetup -listallhardwareports 2>/dev/null"))))
          (string-match-p (format "Hardware Port: Wi-Fi.*Device: %s" 
                                 (regexp-quote interface)) output)))
    (error nil)))

(defun lispbar-network--macos-get-addresses (interface)
  "Get IP addresses for INTERFACE on macOS."
  (condition-case nil
      (let ((output (shell-command-to-string 
                    (format "ifconfig %s 2>/dev/null" 
                            (shell-quote-argument interface)))))
        (when output
          (let ((addresses nil))
            ;; Extract IPv4 addresses
            (while (string-match "inet \\([0-9.]+\\)" output)
              (push (match-string 1 output) addresses)
              (setq output (substring output (match-end 0))))
            addresses)))
    (error nil)))

(defun lispbar-network--macos-get-wifi-info (interface)
  "Get WiFi information for INTERFACE on macOS.
Returns plist with :ssid and :signal-strength."
  (let ((info nil))
    (condition-case nil
        (progn
          ;; Get SSID using networksetup
          (when (lispbar-network--command-available-p "networksetup")
            (let ((ssid-output (shell-command-to-string 
                               (format "networksetup -getairportnetwork %s 2>/dev/null" 
                                       (shell-quote-argument interface)))))
              (when (string-match "Current Wi-Fi Network: \\(.+\\)" ssid-output)
                (setq info (plist-put info :ssid (string-trim (match-string 1 ssid-output)))))))
          
          ;; Try to get signal strength using airport utility
          (when (file-executable-p "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport")
            (let ((airport-output (shell-command-to-string 
                                  "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I 2>/dev/null")))
              (when (string-match "agrCtlRSSI: \\([0-9-]+\\)" airport-output)
                (let ((signal-dbm (string-to-number (match-string 1 airport-output))))
                  ;; Convert dBm to percentage
                  (let ((percentage (max 0 (min 100 (+ 100 (/ (+ signal-dbm 30) 0.6))))))
                    (setq info (plist-put info :signal-strength percentage))))))))
      (error nil))
    
    info))

;;; Windows Network Detection

(defun lispbar-network--windows-get-interfaces ()
  "Get network interfaces on Windows.
Returns list of interface information plists."
  (let ((interfaces nil))
    (condition-case err
        (when (lispbar-network--command-available-p "ipconfig")
          ;; Use netsh for detailed interface information
          (when (lispbar-network--command-available-p "netsh")
            (let ((netsh-output (shell-command-to-string 
                                "netsh interface show interface 2>nul")))
              (dolist (line (split-string netsh-output "\n"))
                (when (string-match "Enabled.*Connected.*\\([^[:space:]]+\\)$" line)
                  (let* ((iface (match-string 1 line))
                         (type (if (string-match-p "Wi-Fi\\|Wireless" iface) 'wifi 'ethernet)))
                    
                    (when (and (not (lispbar-network--interface-blacklisted-p iface))
                               (lispbar-network--interface-whitelisted-p iface))
                      
                      (let ((info (list :name iface
                                        :type type
                                        :state "up"
                                        :addresses (lispbar-network--windows-get-addresses iface))))
                        
                        ;; Add WiFi-specific information
                        (when (eq type 'wifi)
                          (let ((wifi-info (lispbar-network--windows-get-wifi-info)))
                            (when wifi-info
                              (setq info (append info wifi-info)))))
                        
                        (push info interfaces))))))))
      (error
       (lispbar-modules--log 'error "Windows interface detection failed: %s" err)))
    
    (nreverse interfaces)))

(defun lispbar-network--windows-get-addresses (interface)
  "Get IP addresses for INTERFACE on Windows."
  (condition-case nil
      (let ((output (shell-command-to-string 
                    (format "ipconfig /all 2>nul | findstr /C:\"%s\" /A:20" 
                            interface))))
        (when output
          (let ((addresses nil))
            ;; Extract IPv4 addresses
            (while (string-match "IPv4 Address.*: \\([0-9.]+\\)" output)
              (push (match-string 1 output) addresses)
              (setq output (substring output (match-end 0))))
            addresses)))
    (error nil)))

(defun lispbar-network--windows-get-wifi-info ()
  "Get WiFi information on Windows.
Returns plist with :ssid and :signal-strength."
  (let ((info nil))
    (condition-case nil
        (when (lispbar-network--command-available-p "netsh")
          ;; Get current WiFi connection info
          (let ((wifi-output (shell-command-to-string 
                             "netsh wlan show interfaces 2>nul")))
            (when (string-match "SSID.*: \\(.+\\)" wifi-output)
              (setq info (plist-put info :ssid (string-trim (match-string 1 wifi-output)))))
            
            (when (string-match "Signal.*: \\([0-9]+\\)%" wifi-output)
              (setq info (plist-put info :signal-strength 
                                   (string-to-number (match-string 1 wifi-output)))))))
      (error nil))
    
    info))

;;; Interface Processing and Display

(defun lispbar-network--get-interface-priority (interface-info)
  "Get display priority for INTERFACE-INFO.
Higher numbers have higher priority."
  (let ((type (plist-get interface-info :type)))
    (cond
     ((eq type 'vpn) 100)      ; Highest priority - security important
     ((eq type 'wifi) 80)      ; High priority - user-visible connection
     ((eq type 'ethernet) 60)  ; Medium priority - reliable connection
     ((eq type 'mobile) 40)    ; Lower priority - backup connection
     (t 20))))                 ; Lowest priority - unknown connections

(defun lispbar-network--sort-interfaces (interfaces)
  "Sort INTERFACES by priority (highest first)."
  (sort interfaces (lambda (a b)
                     (> (lispbar-network--get-interface-priority a)
                        (lispbar-network--get-interface-priority b)))))

(defun lispbar-network--format-signal-strength (strength)
  "Format signal STRENGTH according to lispbar-network-signal-format."
  (when (and strength (numberp strength))
    (let ((bars (nth (max 0 (min 7 (/ strength 13))) 
                     lispbar-network-wifi-signal-chars))
          (percent (format "%d%%" (round strength))))
      (pcase lispbar-network-signal-format
        ('bars bars)
        ('percent percent)
        ('both (format "%s %s" bars percent))
        (_ bars)))))

(defun lispbar-network--format-interface (interface-info)
  "Format INTERFACE-INFO for display.
Returns formatted string with icon and connection details."
  (let* ((name (plist-get interface-info :name))
         (type (plist-get interface-info :type))
         (ssid (plist-get interface-info :ssid))
         (signal (plist-get interface-info :signal-strength))
         (icon (or (cdr (assq type lispbar-network-icons)) "❓"))
         (display-name (if lispbar-network-show-interface-name
                           name
                         (pcase type
                           ('ethernet "Ethernet")
                           ('wifi "WiFi")
                           ('vpn "VPN")
                           ('mobile "Mobile")
                           (_ "Network")))))
    
    (let ((formatted icon))
      ;; Add connection name/type
      (setq formatted (concat formatted " " display-name))
      
      ;; Add WiFi-specific information
      (when (eq type 'wifi)
        (when (and lispbar-network-show-wifi-ssid ssid (not lispbar-network-hide-ssid))
          (setq formatted (concat formatted " " ssid)))
        (when (and lispbar-network-show-wifi-strength signal)
          (let ((signal-str (lispbar-network--format-signal-strength signal)))
            (when signal-str
              (setq formatted (concat formatted " " signal-str))))))
      
      ;; Add VPN-specific information
      (when (and (eq type 'vpn) lispbar-network-show-vpn-name)
        ;; VPN name already included in display-name or interface name
        )
      
      formatted)))

(defun lispbar-network--get-display-content (interfaces)
  "Get formatted display content for INTERFACES.
Returns propertized string ready for display."
  (if (null interfaces)
      (propertize "No Network" 'face lispbar-network-error-face)
    
    (let* ((sorted-interfaces (lispbar-network--sort-interfaces interfaces))
           (display-interfaces
            (pcase lispbar-network-display-mode
              ('primary (list (car sorted-interfaces)))
              ('multiple (cl-remove-if (lambda (iface)
                                        (eq (plist-get iface :type) 'ethernet))
                                      (cl-subseq sorted-interfaces 0 
                                                (min 2 (length sorted-interfaces)))))
              ('all (cl-subseq sorted-interfaces 0 
                              (min lispbar-network-max-interfaces 
                                   (length sorted-interfaces))))
              (_ (list (car sorted-interfaces)))))
           (formatted-parts
            (mapcar #'lispbar-network--format-interface display-interfaces))
           (content-str (string-join formatted-parts lispbar-network-separator)))
      
      ;; Apply appropriate face
      (let ((has-vpn (cl-some (lambda (iface) (eq (plist-get iface :type) 'vpn))
                              display-interfaces)))
        (propertize content-str 'face 
                   (if has-vpn lispbar-network-vpn-face lispbar-network-face))))))

;;; Network Module Class

(defclass lispbar-network-module (lispbar-module)
  ((display-mode :initarg :display-mode
                 :type symbol
                 :initform 'primary
                 :documentation "How to display multiple connections.")
   (max-interfaces :initarg :max-interfaces
                   :type integer
                   :initform 3
                   :documentation "Maximum interfaces to show in 'all' mode.")
   (show-interface-name :initarg :show-interface-name
                        :type boolean
                        :initform t
                        :documentation "Whether to show interface names.")
   (show-wifi-ssid :initarg :show-wifi-ssid
                   :type boolean
                   :initform t
                   :documentation "Whether to show WiFi SSID.")
   (show-wifi-strength :initarg :show-wifi-strength
                       :type boolean
                       :initform t
                       :documentation "Whether to show WiFi signal strength.")
   (show-vpn-name :initarg :show-vpn-name
                  :type boolean
                  :initform t
                  :documentation "Whether to show VPN names.")
   (hide-ssid :initarg :hide-ssid
              :type boolean
              :initform nil
              :documentation "Hide WiFi SSID for privacy.")
   (signal-format :initarg :signal-format
                  :type symbol
                  :initform 'bars
                  :documentation "Format for WiFi signal display.")
   (icons :initarg :icons
          :type list
          :initform nil
          :documentation "Connection type icons.")
   (interface-filter :initarg :interface-filter
                     :type (or null list)
                     :initform nil
                     :documentation "Whitelist of interfaces to monitor.")
   (interface-blacklist :initarg :interface-blacklist
                        :type list
                        :initform nil
                        :documentation "Blacklist of interfaces to ignore.")
   (separator :initarg :separator
              :type string
              :initform " | "
              :documentation "Separator between multiple connections.")
   (face :initarg :face
         :type (or symbol list)
         :initform 'lispbar-render-default
         :documentation "Face for network display.")
   (vpn-face :initarg :vpn-face
             :type (or symbol list)
             :initform 'lispbar-render-important
             :documentation "Face for VPN display.")
   (error-face :initarg :error-face
               :type (or symbol list)
               :initform 'lispbar-render-urgent
               :documentation "Face for error display.")
   (platform :initform nil
             :documentation "Detected platform.")
   (last-scan-time :initform nil
                   :documentation "Time of last interface scan.")
   (last-interfaces :initform nil
                    :documentation "Cached interface data."))
  "Network module class for Lispbar.
Displays network connection status with comprehensive cross-platform support.")

;;; Module Implementation

(cl-defmethod lispbar-module-update ((module lispbar-network-module))
  "Update method for network module.
Returns formatted network status string for display."
  (condition-case err
      (let ((interfaces (lispbar-network--scan-interfaces module)))
        (lispbar-network--get-display-content interfaces))
    (error
     (lispbar-modules--log 'error "Network module update failed: %s" err)
     (propertize "Network Error" 'face (oref module error-face)))))

(defun lispbar-network--scan-interfaces (module)
  "Scan for active network interfaces using MODULE configuration.
Returns list of interface information plists."
  (let ((platform (or (oref module platform) 
                      (oset module platform (lispbar-network--detect-platform)))))
    
    (pcase platform
      ('linux (lispbar-network--linux-get-interfaces))
      ('macos (lispbar-network--macos-get-interfaces))
      ('windows (lispbar-network--windows-get-interfaces))
      (_ (progn
           (lispbar-modules--log 'warning "Unsupported platform for network detection: %s" platform)
           nil)))))

(cl-defmethod lispbar-network-configure ((module lispbar-network-module))
  "Configure network module with current customization values."
  (oset module display-mode lispbar-network-display-mode)
  (oset module max-interfaces lispbar-network-max-interfaces)
  (oset module show-interface-name lispbar-network-show-interface-name)
  (oset module show-wifi-ssid lispbar-network-show-wifi-ssid)
  (oset module show-wifi-strength lispbar-network-show-wifi-strength)
  (oset module show-vpn-name lispbar-network-show-vpn-name)
  (oset module hide-ssid lispbar-network-hide-ssid)
  (oset module signal-format lispbar-network-signal-format)
  (oset module icons lispbar-network-icons)
  (oset module interface-filter lispbar-network-interface-filter)
  (oset module interface-blacklist lispbar-network-interface-blacklist)
  (oset module separator lispbar-network-separator)
  (oset module face lispbar-network-face)
  (oset module vpn-face lispbar-network-vpn-face)
  (oset module error-face lispbar-network-error-face)
  (oset module position lispbar-network-position)
  (oset module priority lispbar-network-priority)
  (oset module update-interval lispbar-network-update-interval)
  (oset module cache-timeout lispbar-network-cache-timeout)
  
  ;; Update position list if position changed
  (lispbar-modules--add-to-position-list module)
  
  ;; Detect platform
  (oset module platform (lispbar-network--detect-platform))
  
  ;; Clear command cache on reconfiguration
  (setq lispbar-network--command-cache nil))

;;; Module Creation and Management

(defun lispbar-network--create-module ()
  "Create and configure a new network module instance.
Returns the configured module instance."
  (let ((module (make-instance 'lispbar-network-module
                               :name 'network
                               :update-fn (lambda () 
                                          (lispbar-module-update lispbar-network--module-instance))
                               :update-interval lispbar-network-update-interval
                               :position lispbar-network-position
                               :priority lispbar-network-priority
                               :cache-timeout lispbar-network-cache-timeout
                               :enabled t)))
    
    ;; Configure with current customization values
    (lispbar-network-configure module)
    
    ;; Set the global instance
    (setq lispbar-network--module-instance module)
    
    (lispbar-modules--log 'info "Network module created")
    module))

;;;###autoload
(defun lispbar-network-enable ()
  "Enable the Lispbar network module.
Creates and registers the network module if not already enabled."
  (interactive)
  (if lispbar-network--enabled
      (lispbar-modules--log 'info "Network module already enabled")
    (progn
      (lispbar-modules--log 'info "Enabling network module")
      
      ;; Ensure module system is initialized
      (unless lispbar-modules--initialized
        (lispbar-modules-init))
      
      ;; Create and register module
      (let ((module (lispbar-network--create-module)))
        (lispbar-modules-register module)
        (setq lispbar-network--enabled t)
        (lispbar-modules--log 'info "Network module enabled successfully")))))

;;;###autoload
(defun lispbar-network-disable ()
  "Disable the Lispbar network module.
Unregisters and cleans up the network module."
  (interactive)
  (if (not lispbar-network--enabled)
      (lispbar-modules--log 'info "Network module not enabled")
    (progn
      (lispbar-modules--log 'info "Disabling network module")
      
      ;; Unregister module
      (when lispbar-network--module-instance
        (lispbar-modules-unregister 'network))
      
      ;; Clean up
      (setq lispbar-network--module-instance nil
            lispbar-network--enabled nil
            lispbar-network--command-cache nil)
      
      (lispbar-modules--log 'info "Network module disabled"))))

;;;###autoload
(defun lispbar-network-toggle ()
  "Toggle the Lispbar network module on/off."
  (interactive)
  (if lispbar-network--enabled
      (lispbar-network-disable)
    (lispbar-network-enable)))

;;;###autoload
(defun lispbar-network-reconfigure ()
  "Reconfigure the network module with current customization values.
Useful after changing network customization options."
  (interactive)
  (when (and lispbar-network--enabled lispbar-network--module-instance)
    (lispbar-modules--log 'info "Reconfiguring network module")
    (lispbar-network-configure lispbar-network--module-instance)
    (lispbar-modules-invalidate-cache 'network)
    (lispbar-modules-update 'network)
    (lispbar-modules--log 'info "Network module reconfigured")))

;;; Interactive Commands

;;;###autoload
(defun lispbar-network-set-display-mode (mode)
  "Set the network display MODE.
MODE can be \\='primary, \\='multiple, or \\='all."
  (interactive 
   (list (intern (completing-read "Display mode: "
                                  '("primary" "multiple" "all")
                                  nil t nil nil "primary"))))
  (setq lispbar-network-display-mode mode)
  (when lispbar-network--enabled
    (lispbar-network-reconfigure))
  (message "Network display mode set to: %s" mode))

;;;###autoload
(defun lispbar-network-toggle-wifi-ssid ()
  "Toggle WiFi SSID display on/off for the network module."
  (interactive)
  (setq lispbar-network-show-wifi-ssid (not lispbar-network-show-wifi-ssid))
  (when lispbar-network--enabled
    (lispbar-network-reconfigure))
  (message "Network WiFi SSID display: %s" 
           (if lispbar-network-show-wifi-ssid "enabled" "disabled")))

;;;###autoload
(defun lispbar-network-toggle-wifi-strength ()
  "Toggle WiFi signal strength display on/off for the network module."
  (interactive)
  (setq lispbar-network-show-wifi-strength (not lispbar-network-show-wifi-strength))
  (when lispbar-network--enabled
    (lispbar-network-reconfigure))
  (message "Network WiFi strength display: %s" 
           (if lispbar-network-show-wifi-strength "enabled" "disabled")))

;;;###autoload
(defun lispbar-network-toggle-ssid-privacy ()
  "Toggle WiFi SSID privacy (hide/show actual SSID names)."
  (interactive)
  (setq lispbar-network-hide-ssid (not lispbar-network-hide-ssid))
  (when lispbar-network--enabled
    (lispbar-network-reconfigure))
  (message "Network SSID privacy: %s" 
           (if lispbar-network-hide-ssid "enabled (SSID hidden)" "disabled (SSID shown)")))

;;; Status and Information Functions

(defun lispbar-network-status ()
  "Display current status of the network module.
Shows whether enabled, detected platform, active interfaces, etc."
  (interactive)
  (if lispbar-network--enabled
      (let* ((module lispbar-network--module-instance)
             (platform (oref module platform))
             (interfaces (lispbar-network--scan-interfaces module))
             (display-mode (oref module display-mode))
             (position (oref module position))
             (priority (oref module priority)))
        (message (concat "Network module: ENABLED\n"
                        "Platform: %s\n"
                        "Position: %s (priority %d)\n"
                        "Display mode: %s\n"
                        "Active interfaces: %d\n"
                        "Interface details: %s")
                platform position priority display-mode 
                (length interfaces)
                (if interfaces
                    (string-join (mapcar (lambda (iface)
                                          (format "%s (%s)" 
                                                 (plist-get iface :name)
                                                 (plist-get iface :type)))
                                        interfaces) ", ")
                  "none"))))
    (message "Network module: DISABLED")))

(defun lispbar-network-scan-interfaces ()
  "Scan and display current network interfaces.
Useful for debugging interface detection."
  (interactive)
  (if lispbar-network--enabled
      (let* ((module lispbar-network--module-instance)
             (interfaces (lispbar-network--scan-interfaces module)))
        (if interfaces
            (let ((details (mapcar (lambda (iface)
                                    (format "  %s (%s): %s %s"
                                           (plist-get iface :name)
                                           (plist-get iface :type)
                                           (or (plist-get iface :ssid) "")
                                           (if (plist-get iface :signal-strength)
                                               (format "[%d%%]" (plist-get iface :signal-strength))
                                             "")))
                                  interfaces)))
              (message "Network interfaces found:\n%s" (string-join details "\n")))
          (message "No active network interfaces found")))
    (message "Network module not enabled"))))

;;; Module Cleanup

(defun lispbar-network--cleanup ()
  "Clean up network module resources.
Called during module system shutdown."
  (when lispbar-network--enabled
    (lispbar-network-disable)))

;; Register cleanup function
(eval-after-load 'lispbar-modules
  '(lispbar--add-cleanup-function #'lispbar-network--cleanup))

;;; Provide

(provide 'lispbar-network)
;;; lispbar-network.el ends here