# Advanced Position Configuration System for Lispbar

This document describes the advanced position configuration system that extends Lispbar's positioning capabilities far beyond the basic top/bottom options.

## Features Overview

### 1. Enhanced Position Modes
- **top**: Traditional top edge positioning
- **bottom**: Traditional bottom edge positioning  
- **top-offset**: Top positioning with configurable offset from edge
- **bottom-offset**: Bottom positioning with configurable offset from edge
- **floating**: Arbitrary positioning with X,Y coordinates

### 2. Auto-Hide Functionality
- **Timer-based**: Hide after configurable timeout
- **Mouse proximity**: Show when mouse approaches, hide when it leaves
- **Window focus**: Show when Emacs gains focus, hide when it loses focus
- **Manual control**: Show/hide only via interactive commands
- **Smooth animations**: Configurable slide in/out animations

### 3. Smart Strut Management
- **Conflict detection**: Automatically detect conflicts with other panels
- **Configurable resolution**: Choose how to handle conflicts (adjust/override/disable)
- **Per-monitor control**: Enable/disable struts per monitor
- **Floating mode support**: No struts for floating positions

### 4. Edge Snapping
- **Automatic snapping**: Snap to screen edges during interactive positioning
- **Configurable threshold**: Set snap distance in pixels
- **Multi-monitor aware**: Snap to monitor edges in multi-monitor setups

### 5. Multi-Monitor Coordination
- **Per-monitor configuration**: Different positioning per monitor
- **Global inheritance**: Monitors can inherit global settings
- **Position persistence**: Configurations saved across sessions

## Configuration Examples

### Basic Position Configuration

```elisp
;; Set global position to top with 50px offset
(setq lispbar-position 'top-offset
      lispbar-position-offset 50)

;; Set floating position
(setq lispbar-position 'floating
      lispbar-position-floating-x 100
      lispbar-position-floating-y 100)
```

### Auto-Hide Configuration

```elisp
;; Enable auto-hide with timer trigger
(setq lispbar-auto-hide-enabled t
      lispbar-auto-hide-trigger 'timer
      lispbar-auto-hide-timeout 3.0
      lispbar-auto-hide-animation t
      lispbar-auto-hide-animation-duration 0.3)

;; Enable mouse proximity trigger
(setq lispbar-auto-hide-trigger 'mouse)

;; Enable focus-based auto-hide
(setq lispbar-auto-hide-trigger 'focus)
```

### Per-Monitor Configuration

```elisp
;; Configure specific monitor
(lispbar-set-monitor-configuration 
 "monitor-id-here"
 '(:position floating
   :position-floating-x 200
   :position-floating-y 50
   :auto-hide-enabled t
   :auto-hide-trigger mouse
   :strut-enabled nil))

;; Use global settings for most monitors, override specific ones
(setq lispbar-monitor-default-config
      '(:position top-offset
        :position-offset 30
        :auto-hide-enabled nil
        :inherit-global t))
```

### Strut Management

```elisp
;; Disable struts globally (allow window overlap)
(setq lispbar-strut-enabled nil)

;; Handle conflicts by adjusting position
(setq lispbar-strut-conflict-resolution 'adjust)

;; Override existing struts (may cause overlaps)
(setq lispbar-strut-conflict-resolution 'override)
```

### Edge Snapping

```elisp
;; Enable edge snapping with 20px threshold
(setq lispbar-edge-snapping-enabled t
      lispbar-edge-snapping-threshold 20)
```

## Interactive Commands

### Position Control
- `M-x lispbar-set-position` - Set position mode interactively
- `M-x lispbar-set-position-offset` - Set offset for offset modes
- `M-x lispbar-set-floating-position` - Set floating coordinates
- `M-x lispbar-interactive-position-adjust` - Real-time position adjustment

### Auto-Hide Control
- `M-x lispbar-toggle-auto-hide` - Toggle auto-hide functionality
- `M-x lispbar-show-all-frames` - Show all hidden frames immediately
- `M-x lispbar-hide-all-frames` - Hide all auto-hide frames

### Status and Information
- `M-x lispbar-position-status` - Display current position configuration
- `M-x lispbar-describe-monitors` - Detailed monitor information

## Advanced Usage Scenarios

### Scenario 1: Multi-Monitor Setup with Different Positioning

```elisp
;; Primary monitor: traditional top positioning
(lispbar-set-monitor-configuration 
 "primary-monitor-id"
 '(:position top
   :auto-hide-enabled nil
   :strut-enabled t))

;; Secondary monitor: floating with auto-hide
(lispbar-set-monitor-configuration 
 "secondary-monitor-id"
 '(:position floating
   :position-floating-x 50
   :position-floating-y 50
   :auto-hide-enabled t
   :auto-hide-trigger mouse
   :strut-enabled nil))
```

### Scenario 2: Minimal Interference Setup

```elisp
;; Use bottom positioning with auto-hide to minimize interference
(setq lispbar-position 'bottom
      lispbar-auto-hide-enabled t
      lispbar-auto-hide-trigger 'timer
      lispbar-auto-hide-timeout 2.0
      lispbar-strut-enabled nil)  ; Don't reserve space
```

### Scenario 3: Laptop + External Monitor

```elisp
;; Laptop screen: minimal footprint
(lispbar-set-monitor-configuration 
 "laptop-screen-id"
 '(:position bottom-offset
   :position-offset 10
   :auto-hide-enabled t
   :auto-hide-trigger focus))

;; External monitor: traditional positioning
(lispbar-set-monitor-configuration 
 "external-monitor-id"
 '(:position top
   :auto-hide-enabled nil))
```

## Interactive Position Adjustment

The `lispbar-interactive-position-adjust` command provides real-time position adjustment:

1. **Arrow keys**: Move the toolbar
2. **+ key**: Increase width
3. **- key**: Decrease width  
4. **ESC**: Finish adjustment

Edge snapping is automatically applied if enabled.

## Configuration Persistence

All position configurations are automatically saved to the persistence file (default: `~/.emacs.d/lispbar-monitors.el`) and restored on startup.

## Validation and Safety

The system includes comprehensive validation:
- Position modes are validated
- Coordinates are bounds-checked
- Invalid configurations fall back to safe defaults
- Configuration conflicts are detected and resolved

## Performance Considerations

- Auto-hide mouse tracking uses efficient 100ms polling
- Animation timers are automatically cleaned up
- Configuration changes are batched to minimize frame updates
- Edge snapping calculations are optimized for real-time use

## Troubleshooting

### Auto-Hide Not Working
1. Check if `lispbar-auto-hide-enabled` is `t`
2. Verify the trigger mode is set correctly
3. Check for conflicting timers or hooks

### Position Not Applied
1. Verify the position mode is valid
2. Check if monitor configuration overrides global settings
3. Use `M-x lispbar-position-status` to check current configuration

### Strut Conflicts
1. Try setting `lispbar-strut-conflict-resolution` to `'adjust`
2. Disable struts with `(setq lispbar-strut-enabled nil)`
3. Check for other panel software conflicts

## Migration from Basic Positioning

Existing configurations remain fully compatible. The new position modes extend the existing `lispbar-position` variable:

```elisp
;; Old configuration (still works)
(setq lispbar-position 'top)

;; New enhanced configuration
(setq lispbar-position 'top-offset
      lispbar-position-offset 30)
```

All existing monitor configurations automatically inherit the enhanced features through the global inheritance system.