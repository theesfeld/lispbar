# lispbar

A native Wayland status bar written in Common Lisp.

The deliverable is a single SBCL-compiled binary, ~12 MB, ~110 ms
cold start.  It's a real `wlr-layer-shell` client — same protocol
waybar uses — driven by configuration in Lisp.

```
[1] 2 3                      ▶ Artist — Title             ⚡ 87% 󰘎 60% MEM 4% 5G ▆▆▆░ 12:42
```

## What it gives you

| | |
| -------- | ----------------------------------------------------------- |
| 11 built-in modules | clock, workspaces, media, cpu, memory, battery, audio, network, bluetooth, brightness, tray, launcher |
| 6 themes | adaptive, minimal, nordish, gruvboxish, catppuccinish, doomish |
| 3 output drivers | native Wayland, plain stdout (testing), waybar-style JSON (custom-module driver) |
| Click handling | per-module + **per-fragment** (each workspace number is independently clickable) — left / right / middle / scroll / hover |
| Wayland features | wlr-layer-shell, multi-output (one surface per monitor), floating tooltip surface, optional Pango font shaping |
| Tray | full StatusNotifierItem D-Bus host, with icon-theme resolution and SVG → PNG via librsvg |
| Compositors | Sway, Hyprland, niri (workspaces); any other wlroots compositor for the bar itself |
| Init systems | systemd, runit, OpenRC (compositor `exec` works everywhere) |

## Quick start

```sh
git clone https://github.com/theesfeld/lispbar
cd lispbar
make build
sudo make install               # auto-detects init system
# add `exec lispbar` to ~/.config/sway/config       (Sway)
# or  `exec-once = lispbar`     to ~/.config/hypr/hyprland.conf
# or  systemctl --user enable --now lispbar         (systemd)
```

On first launch lispbar creates `~/.config/lispbar/`
(`config.lisp` + empty `modules/` and `themes/` dirs) so you have
a starting point without any manual `cp`.

```sh
lispbar --help               # CLI flags
lispbar --list-modules       # registry inventory
lispbar --print-paths        # XDG resolution
lispbar --show-extensions    # which user files got loaded
```

## A minimal example config

```lisp
;; ~/.config/lispbar/config.lisp
(placement :left   (:launcher :workspaces))
(placement :center (:media))
(placement :right  (:cpu :memory :audio :network :battery :tray :clock))

(position      :top)
(height        32)
(margin        8 12 0 12)        ; CSS-style: top right bottom left
(corner-radius 12)               ; floating rounded pill
(padding       18)
(gap           18)

(font          "Sans 11")
(theme         :catppuccinish)
(output        :wayland)
```

## Documentation

| Doc                                        | What                                                      |
| ------------------------------------------ | --------------------------------------------------------- |
| [docs/install.md](docs/install.md)         | Per-distro build & install; uninstall; first-run setup    |
| [docs/configuration.md](docs/configuration.md) | Every config keyword, every per-module variable          |
| [docs/modules.md](docs/modules.md)         | Built-in module reference + how to write your own         |
| [docs/themes.md](docs/themes.md)           | Built-in palettes + how to write your own theme           |
| [docs/registry.md](docs/registry.md)       | Install community modules/themes with `lispbar registry`  |
| [docs/compositors.md](docs/compositors.md) | Sway / Hyprland / niri setup notes                       |
| [docs/troubleshooting.md](docs/troubleshooting.md) | Common problems and their fixes                  |
| [docs/architecture.md](docs/architecture.md)| Internals, for contributors                              |

## How custom extension works (the short version)

Drop a `.lisp` file under `~/.config/lispbar/modules/` and it's
loaded at startup.  Same for `~/.config/lispbar/themes/`.  Both
work without rebuilding the binary:

```lisp
;; ~/.config/lispbar/modules/loadavg.lisp
(in-package :lispbar)

(defmodule :loadavg
  (:doc "5- and 15-minute load average"
   :position :right :priority 58 :interval 5.0
   :on-click ((:left "xdg-open https://example.com"))
   :tooltip "hover me")
  (when (probe-file "/proc/loadavg")
    (with-open-file (s "/proc/loadavg")
      (let* ((line (read-line s nil ""))
             (parts (uiop:split-string line :separator '(#\Space))))
        (and (>= (length parts) 3)
             (format nil "LOAD ~a ~a" (second parts) (third parts)))))))
```

Reference it in `config.lisp`:

```lisp
(placement :right (:loadavg ...))
```

Themes work the same way:

```lisp
;; ~/.config/lispbar/themes/dracula.lisp
(in-package :lispbar)

(define-theme :dracula
  :bg     '(0.157 0.165 0.212 1.0)
  :normal '(0.945 0.945 0.945 1.0)
  :accent '(0.741 0.576 0.976 1.0)
  :ok     '(0.314 0.980 0.482 1.0)
  :warn   '(0.945 0.980 0.549 1.0)
  :urgent '(1.000 0.333 0.333 1.0)
  :muted  '(0.380 0.420 0.490 1.0))

;; then: (theme :dracula) in config.lisp
```

See [docs/modules.md](docs/modules.md) and
[docs/themes.md](docs/themes.md) for the full API.

## Community modules and themes

Don't want to write your own?  The repository ships a curated
**registry** of community modules and themes (`registry/`).  Browse
and install with the `registry` subcommand:

```sh
lispbar registry list                  # browse
lispbar registry install :weather      # download + verify
lispbar registry install :tokyo-night
lispbar registry update                # refresh installed items
```

Items land in `~/.config/lispbar/modules/` (or `themes/`) just like
hand-written ones.  Submissions go through a PR with CI-enforced
SHA-256 checks; the moment a PR is merged to `master`, every user's
`lispbar registry` immediately sees it — no new release required.

See [docs/registry.md](docs/registry.md) and
[registry/CONTRIBUTING.md](registry/CONTRIBUTING.md).

## License

GPL-3.0-or-later.  See [LICENSE](LICENSE).
