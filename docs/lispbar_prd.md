# Lispbar Project Plan/PRD: A Pure Elisp Toolbar for EXWM

## Executive Summary

Lispbar is a pure Emacs Lisp toolbar package designed specifically for EXWM (Emacs X Window Manager) users. Following GNU Emacs 30.1 coding conventions and best practices, it provides a modular, theme-aware, and highly customizable toolbar system with built-in modules for battery status, network connection, clock/date, and workspace management, plus support for custom user modules.

## 1. Architecture and Design

### Core Architecture

```
lispbar/
├── lispbar.el                 # Main package file with autoloads
├── lispbar-core.el            # Core toolbar framework
├── lispbar-modules.el         # Module system implementation
├── lispbar-exwm.el           # EXWM integration layer
├── lispbar-render.el         # Rendering and display engine
├── lispbar-theme.el          # Theme integration system
├── lispbar-config.el         # Configuration management
├── modules/
│   ├── lispbar-battery.el    # Battery status module
│   ├── lispbar-network.el    # Network connection module
│   ├── lispbar-clock.el      # Clock/date module
│   └── lispbar-workspace.el  # Workspace module
├── test/
│   ├── lispbar-test.el       # ERT tests
│   └── test-helper.el        # Test utilities
└── docs/
    └── README.org            # Documentation
```

### Design Principles

1. **Pure Elisp Implementation**: No external dependencies, leveraging Emacs' built-in capabilities
2. **EXWM-Native Integration**: Direct integration with EXWM APIs and event system
3. **Lazy Loading**: Modules load on-demand with proper autoload cookies
4. **Theme Awareness**: Automatic adaptation to Emacs theme changes
5. **Event-Driven Updates**: Efficient updates using Emacs hooks and timers
6. **Modular Architecture**: Clear separation between core, modules, and rendering

### Display Architecture

```elisp
;; Toolbar display using dedicated frame
(defvar lispbar-frame nil "The dedicated lispbar frame.")

(defun lispbar-create-frame ()
  "Create a dedicated frame for lispbar display."
  (let ((frame (make-frame
                `((name . "lispbar")
                  (minibuffer . nil)
                  (tool-bar-lines . 0)
                  (menu-bar-lines . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (undecorated . t)
                  (sticky . t)))))
    (with-selected-frame frame
      ;; Configure frame for toolbar use
      (set-frame-parameter frame 'exwm-geometry
                          (lispbar--calculate-geometry)))
    frame))
```

### Multi-Monitor Support

```elisp
(defcustom lispbar-monitor-aware t
  "Whether lispbar should create separate instances per monitor."
  :type 'boolean
  :group 'lispbar)

(defvar lispbar-monitor-frames nil
  "Alist mapping monitor names to lispbar frames.")

(defun lispbar-setup-monitors ()
  "Set up lispbar frames for all monitors."
  (when (and lispbar-monitor-aware (featurep 'exwm-randr))
    (dolist (monitor (lispbar--get-monitor-list))
      (lispbar-create-frame-for-monitor monitor))))
```

## 2. Module System Design

### Module Interface

```elisp
(defclass lispbar-module ()
  ((name :initarg :name
         :type symbol
         :documentation "Unique module identifier")
   (update-fn :initarg :update-fn
              :type function
              :documentation "Function that returns module content")
   (update-interval :initarg :update-interval
                    :type (or null number)
                    :initform nil
                    :documentation "Update interval in seconds, nil for event-driven")
   (hooks :initarg :hooks
          :type list
          :initform nil
          :documentation "List of hooks that trigger updates")
   (position :initarg :position
             :type symbol
             :initform 'right
             :documentation "Position: 'left, 'center, or 'right")
   (priority :initarg :priority
             :type number
             :initform 50
             :documentation "Display priority (0-100)")
   (cache :initform nil
          :documentation "Cached module output"))
  "Base class for lispbar modules.")
```

### Built-in Modules

#### Battery Module
```elisp
(defclass lispbar-battery-module (lispbar-module)
  ((format :initarg :format
           :initform "{icon} {percentage}%"
           :documentation "Display format template")
   (low-threshold :initarg :low-threshold
                  :initform 20
                  :documentation "Low battery warning threshold")
   (critical-threshold :initarg :critical-threshold
                       :initform 10
                       :documentation "Critical battery threshold")))

(defmethod lispbar-module-update ((module lispbar-battery-module))
  "Update battery status display."
  (let* ((status (lispbar-battery--get-status))
         (percentage (plist-get status :percentage))
         (charging (plist-get status :charging)))
    (lispbar-format-template
     (oref module format)
     `((icon . ,(lispbar-battery--get-icon percentage charging))
       (percentage . ,percentage)
       (status . ,(if charging "charging" "discharging"))))))
```

#### Network Module
```elisp
(defclass lispbar-network-module (lispbar-module)
  ((show-details :initarg :show-details
                 :initform t
                 :documentation "Show connection details")
   (interfaces :initarg :interfaces
               :initform '("eth0" "wlan0" "tun0")
               :documentation "Network interfaces to monitor")))

(defmethod lispbar-module-update ((module lispbar-network-module))
  "Update network status display."
  (let ((status (lispbar-network--get-active-connection)))
    (pcase (plist-get status :type)
      ('ethernet (format " %s" (plist-get status :interface)))
      ('wifi (format " %s (%s)" 
                     (plist-get status :ssid)
                     (plist-get status :strength)))
      ('vpn (format " VPN"))
      (_ " Offline"))))
```

#### Clock Module
```elisp
(defclass lispbar-clock-module (lispbar-module)
  ((format :initarg :format
           :initform "%H:%M"
           :documentation "Time format string")
   (date-format :initarg :date-format
                :initform "%Y-%m-%d"
                :documentation "Date format string")
   (show-date :initarg :show-date
              :initform nil
              :documentation "Whether to show date")))

(defmethod lispbar-module-update ((module lispbar-clock-module))
  "Update clock display."
  (let ((time-string (format-time-string (oref module format)))
        (date-string (when (oref module show-date)
                      (format-time-string (oref module date-format)))))
    (if date-string
        (format "%s %s" time-string date-string)
      time-string)))
```

#### Workspace Module
```elisp
(defclass lispbar-workspace-module (lispbar-module)
  ((show-names :initarg :show-names
               :initform nil
               :documentation "Show workspace names instead of numbers")
   (current-workspace-face :initarg :current-workspace-face
                           :initform 'lispbar-workspace-current
                           :documentation "Face for current workspace")))

(defmethod lispbar-module-update ((module lispbar-workspace-module))
  "Update workspace display."
  (let ((current-idx exwm-workspace-current-index)
        (workspace-count (exwm-workspace--count)))
    (mapconcat
     (lambda (i)
       (let ((name (if (oref module show-names)
                      (lispbar-workspace--get-name i)
                    (number-to-string i))))
         (if (= i current-idx)
             (propertize name 'face (oref module current-workspace-face))
           name)))
     (number-sequence 0 (1- workspace-count))
     " | ")))
```

### Custom Module Support

```elisp
(defmacro lispbar-define-module (name &rest args)
  "Define a custom lispbar module NAME with ARGS."
  (let ((class-name (intern (format "lispbar-%s-module" name))))
    `(progn
       (defclass ,class-name (lispbar-module)
         ,(plist-get args :slots))
       
       (defmethod lispbar-module-update ((module ,class-name))
         ,(plist-get args :update-fn))
       
       (defun ,(intern (format "lispbar-%s-create" name)) (&rest args)
         ,(format "Create a new %s module." name)
         (apply #'make-instance ',class-name args))
       
       (provide ',(intern (format "lispbar-%s" name))))))

;; Example custom module
(lispbar-define-module git
  :slots ((repository :initarg :repository
                     :initform nil
                     :documentation "Git repository path"))
  :update-fn (let ((repo (or (oref module repository) default-directory)))
              (lispbar-git--get-status repo)))
```

## 3. Configuration System via use-package

### Basic Configuration

```elisp
(use-package lispbar
  :ensure t
  :after exwm
  :hook ((exwm-init . lispbar-mode)
         (exwm-workspace-switch . lispbar-update-workspace)
         (exwm-randr-screen-change . lispbar-refresh-monitors))
  :custom
  ;; Layout configuration
  (lispbar-position 'top)
  (lispbar-height 28)
  (lispbar-monitor-aware t)
  
  ;; Module configuration
  (lispbar-modules-left '(workspace))
  (lispbar-modules-center '(clock))
  (lispbar-modules-right '(network battery))
  
  ;; Theme integration
  (lispbar-theme-integration t)
  (lispbar-transparency 0.95)
  
  :config
  ;; Module-specific configuration
  (setq lispbar-battery-format "{icon} {percentage}%")
  (setq lispbar-battery-low-threshold 20)
  
  (setq lispbar-clock-format "%H:%M")
  (setq lispbar-clock-show-date t)
  
  (setq lispbar-network-show-details t)
  (setq lispbar-network-interfaces '("wlan0" "eth0" "tun0"))
  
  ;; Custom module
  (lispbar-define-module cpu
    :slots ((format :initform "CPU: {usage}%"))
    :update-fn (format "CPU: %d%%" (lispbar-cpu--get-usage)))
  
  (add-to-list 'lispbar-modules-right 'cpu))
```

### Advanced Configuration

```elisp
(use-package lispbar
  :ensure t
  :demand t
  :after (exwm all-the-icons)
  :bind ((\"C-c b\" . lispbar-command-map)
         :map lispbar-command-map
         (\"t\" . lispbar-toggle)
         (\"r\" . lispbar-refresh)
         (\"c\" . lispbar-customize))
  :custom-face
  (lispbar-face ((t :inherit mode-line :height 1.1)))
  (lispbar-workspace-current ((t :inherit mode-line-emphasis :weight bold)))
  :init
  ;; Pre-load configuration
  (setq lispbar-cache-directory
        (expand-file-name "lispbar/" user-emacs-directory))
  :config
  ;; Complex module setup
  (lispbar-configure-module
   'battery
   :format (lambda (data)
            (let ((icon (all-the-icons-material
                        (if (plist-get data :charging)
                            "battery_charging_full"
                          "battery_std"))))
              (format "%s %d%%" icon (plist-get data :percentage))))
   :faces `((low . ((t :foreground ,(face-foreground 'warning))))
           (critical . ((t :foreground ,(face-foreground 'error))))))
  
  ;; Multi-monitor configuration
  (setq lispbar-monitor-configurations
        '(("HDMI-1" . ((position . top) (height . 32)))
          ("eDP-1" . ((position . bottom) (height . 24)))))
  
  ;; Custom keybindings for workspace module
  (define-key lispbar-workspace-map [mouse-1] #'exwm-workspace-switch-to-buffer)
  (define-key lispbar-workspace-map [mouse-3] #'lispbar-workspace-menu))
```

## 4. Complete TODO List

### Phase 1: Core Implementation (Week 1-2)
- [ ] Set up project structure and build system
- [ ] Implement core frame management (lispbar-core.el)
- [ ] Create basic rendering engine (lispbar-render.el)
- [ ] Implement module system base classes
- [ ] Add EXWM integration layer
- [ ] Create theme integration system
- [ ] Implement configuration management
- [ ] Add basic error handling and logging

### Phase 2: Built-in Modules (Week 3-4)
- [ ] Implement battery module with system detection
- [ ] Create network module with interface detection
- [ ] Build clock/date module with formatting options
- [ ] Develop workspace module with EXWM integration
- [ ] Add module update scheduling system
- [ ] Implement module caching mechanism
- [ ] Create module communication system

### Phase 3: Display and Layout (Week 5-6)
- [ ] Implement left/center/right layout system
- [ ] Add multi-monitor support with exwm-randr
- [ ] Create position configuration (top/bottom)
- [ ] Implement dynamic width calculation
- [ ] Add proper strut management for EXWM
- [ ] Create smooth update animations
- [ ] Handle frame cleanup on exit

### Phase 4: Theme Integration (Week 7)
- [ ] Implement theme change detection
- [ ] Create color derivation system
- [ ] Add face definitions following theme
- [ ] Implement icon theme support
- [ ] Create transparency support
- [ ] Add blur/compositing integration
- [ ] Build theme preview system

### Phase 5: User Experience (Week 8)
- [ ] Create interactive customization interface
- [ ] Add mouse interaction support
- [ ] Implement context menus
- [ ] Create module drag-and-drop
- [ ] Add tooltips and help system
- [ ] Build configuration wizard
- [ ] Implement backup/restore

### Phase 6: Testing and Documentation (Week 9-10)
- [ ] Write comprehensive ERT tests
- [ ] Add integration tests with EXWM
- [ ] Create performance benchmarks
- [ ] Write user documentation
- [ ] Create module development guide
- [ ] Add inline documentation
- [ ] Build example configurations

### Phase 7: Optimization and Polish (Week 11-12)
- [ ] Profile and optimize performance
- [ ] Minimize memory usage
- [ ] Implement lazy loading
- [ ] Add byte-compilation optimization
- [ ] Create icon caching system
- [ ] Optimize update algorithms
- [ ] Add native compilation support

## 5. Project Management Instructions for Claude Code

### Sub-agent Architecture

Claude Code should create specialized sub-agents for different aspects of the project:

#### 1. Core Development Agent
**Responsibilities:**
- Implement lispbar-core.el and lispbar-render.el
- Handle frame management and display logic
- Create the module system base infrastructure

**Key Tasks:**
- Set up EXWM frame creation and positioning
- Implement the rendering pipeline
- Create module registration and lifecycle management
- Handle multi-monitor frame coordination

#### 2. Module Development Agent
**Responsibilities:**
- Implement all built-in modules
- Create module base classes and interfaces
- Develop the module update system

**Key Tasks:**
- Build battery status detection across platforms
- Implement network interface monitoring
- Create EXWM workspace integration
- Develop module communication protocols

#### 3. Theme and UI Agent
**Responsibilities:**
- Implement theme integration system
- Create face definitions and color management
- Handle visual polish and animations

**Key Tasks:**
- Build theme change detection system
- Implement dynamic color derivation
- Create smooth visual transitions
- Design default appearance

#### 4. Testing and Quality Agent
**Responsibilities:**
- Write comprehensive test suites
- Ensure code quality and standards compliance
- Performance optimization

**Key Tasks:**
- Create ERT test coverage
- Implement integration tests
- Run package-lint validation
- Profile performance bottlenecks

#### 5. Documentation Agent
**Responsibilities:**
- Write user and developer documentation
- Create example configurations
- Build interactive tutorials

**Key Tasks:**
- Write comprehensive README
- Create Info manual
- Document all public APIs
- Build configuration examples

### Coordination Strategy

```elisp
;; Project coordination structure
(defvar lispbar-dev-milestones
  '((week-1 . core-frame-implementation)
    (week-2 . module-system-base)
    (week-3 . battery-network-modules)
    (week-4 . clock-workspace-modules)
    (week-5 . layout-system)
    (week-6 . multi-monitor-support)
    (week-7 . theme-integration)
    (week-8 . user-interface)
    (week-9 . testing-suite)
    (week-10 . documentation)
    (week-11 . optimization)
    (week-12 . release-preparation)))
```

### Development Workflow

1. **Daily Sync Protocol:**
   - Each agent reports progress on assigned tasks
   - Identify blockers and dependencies
   - Coordinate API changes between agents
   - Update central progress tracking

2. **Code Review Process:**
   - Cross-agent code reviews for interfaces
   - Ensure GNU Elisp coding standards
   - Validate theme integration consistency
   - Check performance implications

3. **Integration Testing:**
   - Weekly integration builds
   - Cross-module functionality testing
   - EXWM compatibility verification
   - Multi-monitor scenario testing

## 6. MCP and Tool Recommendations

### Development Tools

1. **Emacs Development Environment:**
   ```elisp
   (use-package elisp-lint
     :ensure t
     :config
     (setq elisp-lint-ignored-validators '("fill-column")))
   
   (use-package package-lint
     :ensure t)
   
   (use-package flycheck-package
     :ensure t
     :after flycheck
     :config
     (flycheck-package-setup))
   ```

2. **Testing Framework:**
   ```elisp
   (use-package buttercup
     :ensure t)
   
   (use-package undercover
     :ensure t
     :config
     (undercover "*.el" (:exclude "*-test.el")))
   ```

3. **Performance Profiling:**
   ```elisp
   (use-package esup
     :ensure t
     :commands esup)
   
   (use-package explain-pause-mode
     :ensure t
     :config
     (explain-pause-mode 1))
   ```

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        emacs_version: [28.2, 29.1, 30.1]
    steps:
      - uses: actions/checkout@v3
      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}
      - name: Install dependencies
        run: |
          emacs -Q --batch --eval "(package-initialize)" \
                --eval "(package-refresh-contents)" \
                --eval "(package-install 'exwm)"
      - name: Run tests
        run: make test
      - name: Run linter
        run: make lint
```

### Release Management

1. **Version Control:**
   - Use semantic versioning
   - Tag releases with signed commits
   - Maintain CHANGELOG.org

2. **Distribution:**
   - Submit to MELPA
   - Create GitHub releases
   - Package for Guix/Nix

3. **Quality Checklist:**
   - [ ] All tests passing
   - [ ] package-lint clean
   - [ ] byte-compilation clean
   - [ ] Documentation complete
   - [ ] Example configs working
   - [ ] Multi-monitor tested
   - [ ] Theme integration verified

This comprehensive PRD provides a complete roadmap for developing lispbar as a professional-quality Emacs package that integrates seamlessly with EXWM while following all GNU Emacs best practices and conventions.