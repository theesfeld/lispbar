# CLAUDE.md - Lispbar Project Instructions for Claude Opus 4

## Project Overview
Lispbar is a pure Emacs Lisp toolbar package for EXWM. This document contains specific instructions for Claude Opus 4 to efficiently develop this project.

## Critical Development Rules

### Code Standards
1. **GNU Elisp Conventions**: Follow GNU Emacs Lisp coding conventions strictly
   - Use `lispbar-` prefix for all public symbols
   - Use `lispbar--` prefix for internal/private functions
   - Add proper file headers with lexical-binding
   - Include autoload cookies where appropriate

2. **File Structure**:
   ```
   ;;; lispbar-module.el --- Description -*- lexical-binding: t -*-
   
   ;; Copyright (C) 2025 Free Software Foundation, Inc.
   
   ;; Author: Your Name
   ;; Version: 0.1.0
   ;; Package-Requires: ((emacs "27.1") (exwm "0.24"))
   ;; Keywords: frames, exwm
   ;; URL: https://github.com/yourusername/lispbar
   
   ;;; Commentary:
   
   ;; Detailed description here
   
   ;;; Code:
   
   (require 'cl-lib)
   (require 'eieio)
   
   ;; ... code ...
   
   (provide 'lispbar-module)
   ;;; lispbar-module.el ends here
   ```

3. **Documentation**:
   - Every public function needs comprehensive docstring
   - Use active voice in docstrings
   - First line of docstring ≤ 70 chars
   - Include examples in complex functions

### Git Workflow
1. **Commit Strategy**:
   - Small, atomic commits (1 logical change per commit)
   - Commit message format: `component: Brief description (max 50 chars)`
   - Examples:
     - `core: Add frame creation function`
     - `battery: Implement Linux battery detection`
     - `docs: Update installation instructions`

2. **Branch Strategy**:
   - `main` branch for stable code
   - Feature branches: `feature/module-name`
   - Bugfix branches: `fix/issue-description`

### Development Phases

#### Phase 1: Core Setup (Current)
```elisp
;; Priority tasks:
(defvar lispbar-dev-phase-1-tasks
  '((setup-repository . "Create GitHub repo and initial structure")
    (core-frame . "Implement basic frame creation in lispbar-core.el")
    (module-base . "Create module system base class")
    (render-engine . "Build basic rendering pipeline")
    (exwm-hooks . "Add EXWM integration hooks")))
```

#### Module Development Pattern
When creating a module, always follow this pattern:

```elisp
;; 1. Define the module class
(defclass lispbar-MODULENAME-module (lispbar-module)
  ((specific-slot :initarg :specific-slot
                  :initform default-value
                  :type type-spec
                  :documentation "Clear description"))
  "Module documentation.")

;; 2. Implement update method
(cl-defmethod lispbar-module-update ((module lispbar-MODULENAME-module))
  "Update MODULENAME display."
  ;; Return string or nil
  )

;; 3. Create initialization function
(defun lispbar-MODULENAME-init ()
  "Initialize MODULENAME module."
  ;; Setup code
  )

;; 4. Add customization options
(defcustom lispbar-MODULENAME-option default-value
  "Option description."
  :type 'type-spec
  :group 'lispbar-MODULENAME)
```

### Testing Requirements
1. **Every Function** needs corresponding ERT test
2. **Test File Naming**: `test/lispbar-MODULE-test.el`
3. **Test Pattern**:
   ```elisp
   (ert-deftest lispbar-MODULE-test-FUNCTION ()
     "Test FUNCTION in MODULE."
     (should (equal expected (lispbar-MODULE-FUNCTION args))))
   ```

### Performance Guidelines
1. **Rendering**: Never block > 16ms (60fps target)
2. **Updates**: Batch updates in single redraw cycle
3. **Memory**: Cache expensive computations
4. **Timers**: Use single timer for multiple modules

### Module Implementation Order
1. `lispbar-core.el` - Frame and lifecycle management
2. `lispbar-render.el` - Rendering pipeline
3. `lispbar-modules.el` - Module system base
4. `lispbar-clock.el` - Simplest module first
5. `lispbar-workspace.el` - EXWM integration test
6. `lispbar-battery.el` - System info module
7. `lispbar-network.el` - Complex state module

### EXWM Integration Points
```elisp
;; Key hooks to implement:
(defvar lispbar-exwm-hooks
  '((exwm-init-hook . lispbar-init)
    (exwm-exit-hook . lispbar-cleanup)
    (exwm-workspace-switch-hook . lispbar-update-workspace)
    (exwm-randr-screen-change-hook . lispbar-refresh-monitors)
    (exwm-manage-finish-hook . lispbar-update-windows)))
```

### Error Handling Pattern
```elisp
(defun lispbar-safe-call (fn &rest args)
  "Safely call FN with ARGS, log errors."
  (condition-case err
      (apply fn args)
    (error
     (lispbar-log 'error "Function %s failed: %s" fn err)
     nil)))
```

### Common Pitfalls to Avoid
1. **Don't** use `setq` for user options (use `defcustom`)
2. **Don't** modify global state without cleanup
3. **Don't** assume single monitor setup
4. **Don't** hardcode colors (derive from theme)
5. **Don't** block on I/O operations
6. **Don't** create circular dependencies

### Development Commands
```bash
# Run tests
make test

# Lint code
make lint

# Build package
make package

# Clean build artifacts
make clean

# Install locally
make install
```

### Module State Machine
Each module follows this lifecycle:
1. `init` → Module created, not yet active
2. `start` → Module activated, timers started
3. `update` → Content refresh requested
4. `render` → Convert state to display string
5. `stop` → Module deactivated, cleanup
6. `destroy` → Module removed from system

### Configuration Validation
Always validate user configuration:
```elisp
(defun lispbar-validate-config (config)
  "Validate CONFIG, return sanitized version."
  (let ((valid-config (copy-alist config)))
    ;; Validate each option
    (when (plist-get valid-config :height)
      (setf (plist-get valid-config :height)
            (max 16 (min 100 (plist-get valid-config :height)))))
    valid-config))
```

### Platform Detection
```elisp
(defun lispbar-detect-platform ()
  "Detect current platform."
  (cond
   ((eq system-type 'gnu/linux) 'linux)
   ((eq system-type 'darwin) 'macos)
   ((memq system-type '(windows-nt cygwin)) 'windows)
   (t 'unknown)))
```

### Debugging Tools
```elisp
;; Enable debug mode
(setq lispbar-debug t)

;; Trace function calls
(trace-function 'lispbar-render)

;; Profile performance
(profiler-start 'cpu)
;; ... run code ...
(profiler-report)
```

### Release Checklist
Before any release:
- [ ] All tests pass
- [ ] No byte-compile warnings
- [ ] package-lint clean
- [ ] Documentation updated
- [ ] CHANGELOG entry added
- [ ] Version bumped in all files
- [ ] Tagged in git

### Quick Testing Setup
```elisp
;; For rapid development testing
(add-to-list 'load-path "/home/grim/Code/lispbar")
(require 'lispbar)
(setq lispbar-modules-left '(workspace)
      lispbar-modules-center '(clock)
      lispbar-modules-right '(battery network))
(lispbar-mode 1)
```

## Remember
- Frequent small commits > large commits
- Test early, test often
- Profile before optimizing
- Document as you code
- Ask for clarification if needed