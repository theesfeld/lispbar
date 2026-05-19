# Lispbar documentation

Native Wayland status bar in Common Lisp.

## Read these first

* [Install](install.md) — per-distro build and install steps
* [Configuration](configuration.md) — every config option and variable

## When you want to extend

* [Modules](modules.md) — built-in module reference and authoring guide
* [Themes](themes.md) — built-in palettes and theme authoring guide

## When something breaks

* [Troubleshooting](troubleshooting.md) — common problems and fixes
* [Compositors](compositors.md) — Sway / Hyprland / niri integration notes

## When you want to contribute

* [Architecture](architecture.md) — source layout, render loop,
  click dispatch, extension points

## Quick reference

```
lispbar                       # run forever
lispbar --once                # one frame to stdout
lispbar --init [--force]      # seed user config
lispbar --list-modules        # inventory
lispbar --list-themes
lispbar --print-paths         # XDG resolution
lispbar --show-extensions     # loaded user .lisp files
lispbar --help
```

```
~/.config/lispbar/
├── config.lisp           ; main config
├── modules/*.lisp        ; user modules (auto-loaded)
└── themes/*.lisp         ; user themes  (auto-loaded)

~/.cache/lispbar/icons/   ; SVG→PNG cache for tray icons
~/.local/state/lispbar/   ; for modules that persist state
```
