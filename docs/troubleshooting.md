# Troubleshooting

Common problems and the steps that resolve them, in roughly
decreasing order of frequency.

## The bar doesn't appear at all

### 1. Check the compositor speaks layer-shell

```sh
WAYLAND_DEBUG=1 lispbar 2>&1 | head -20
```

If you see `missing globals (... layer-shell=(nil))`, your
compositor doesn't expose `wlr-layer-shell-unstable-v1`.  GNOME
mainline Wayland is the most common offender — switch to a
wlroots compositor (Sway, Hyprland, niri, river, Wayfire, KDE
Plasma Wayland 5.24+).

### 2. Check it actually started

```sh
ps aux | grep lispbar
journalctl --user -u lispbar -n 50      # systemd
sv status lispbar                       # runit
tail /tmp/lispbar.log                   # if you redirected stderr
```

If it crashed on launch the systemd unit will be in `failed`
state and the logs show why.

### 3. Run it in the foreground to see logs

```sh
LD_LIBRARY_PATH=/usr/local/lib/lispbar lispbar -v
```

Watch stderr for `wlbar_init failed`, `tray: wltray_init failed`,
etc.  Common causes:

| Symptom                                                | Cause                                |
| ------------------------------------------------------ | ------------------------------------ |
| `cannot connect to Wayland display`                    | `$WAYLAND_DISPLAY` not in env       |
| `missing globals (layer-shell=(nil))`                  | Compositor lacks wlr-layer-shell    |
| `no wl_outputs advertised by compositor`               | No monitors detected                 |
| `Unable to load any of the alternatives: libwlbar.so`  | `LD_LIBRARY_PATH` doesn't include the install lib dir |

## The bar appears but workspaces are missing

```sh
lispbar -v 2>&1 | grep workspaces
```

Expected: `workspaces backend detected: SWAY` (or HYPRLAND / NIRI).

If you see `no workspaces backend detected; module silent` then
none of the env vars or IPC probes matched.  Check:

```sh
echo $SWAYSOCK $HYPRLAND_INSTANCE_SIGNATURE $NIRI_SOCKET
swaymsg --version          # if on sway
hyprctl version            # if on hyprland
niri msg version           # if on niri
```

If the IPC tool works on the command line but lispbar doesn't see
it, the most likely cause is the bar inheriting a stripped
environment from its service manager.  systemd users can add:

```ini
# ~/.config/systemd/user/lispbar.service.d/env.conf
[Service]
PassEnvironment=SWAYSOCK HYPRLAND_INSTANCE_SIGNATURE NIRI_SOCKET WAYLAND_DISPLAY
```

…or start the bar from the compositor's `exec`/`exec-once` hook,
which is the supported path.

## Tray is empty

### Step 1: confirm the watcher is up

```sh
gdbus introspect --session --dest org.kde.StatusNotifierWatcher \
                 --object-path /StatusNotifierWatcher
```

Should print an XML tree.  If it says "service not running",
lispbar's tray subsystem failed to claim the name.  Run with
`-v` and look for `tray: wltray_init failed`.

### Step 2: confirm items are registering with us

```sh
gdbus call --session --dest org.kde.StatusNotifierWatcher \
           --object-path /StatusNotifierWatcher \
           --method org.freedesktop.DBus.Properties.Get \
           'org.kde.StatusNotifierWatcher' 'RegisteredStatusNotifierItems'
```

If the returned array is empty, no tray app on your system has
registered.  Try launching `nm-applet`, `keepassxc`, or any other
known SNI app and watch the array grow.

### Step 3: another tray is competing

Common: waybar (or KDE Plasma's panel, or another bar) is also
running and grabbed `org.kde.StatusNotifierWatcher` first.  Stop
the other bar.  At most one Watcher can own the name.

## Icons in the tray are missing

### librsvg-bin isn't installed

The tray ships only inline-pixmap rendering out of the box.  Apps
that publish an `IconName` (most modern apps) need
`rsvg-convert` to rasterise SVG icons:

```sh
# Gentoo
emerge gnome-base/librsvg

# Arch
pacman -S librsvg

# Debian/Ubuntu
apt install librsvg2-bin
```

Restart lispbar.  Icons will populate over the next few seconds
as items publish their `NewIcon` signals.

### Verify the icon cache works

```sh
ls ~/.cache/lispbar/icons/
```

Should contain at least a few `.png` files once you've hovered or
clicked any SNI item.  If the directory is empty after several
minutes, the icon-theme search didn't match anything.

### Force a specific theme

If your icon theme isn't in `/usr/share/icons` and isn't named in
the standard way (with an `index.theme` file), the auto-discovery
won't find it.  Install a mainstream theme:

```sh
# Gentoo
emerge x11-themes/adwaita-icon-theme

# Arch
pacman -S adwaita-icon-theme

# Debian/Ubuntu
apt install adwaita-icon-theme
```

## Tooltips don't appear

### Did `:tooltip` get declared?

Module needs the keyword in its `defmodule`:

```lisp
(defmodule :foo (:tooltip "static text") ...)
;;                 ^^^^^^^^
```

If you wrote `:on-tooltip` or `:hover` or anything else, the
macro silently ignores it.

### Compositor lacks `layer.set_margin`

The floating tooltip uses a secondary layer-shell surface
positioned via `set_margin`.  All recent wlroots compositors
support this; older ones might not.  Update.

## Audio scroll does nothing

Most likely cause: `wpctl` / `pactl` / `amixer` aren't on PATH
in the bar's environment.

```sh
command -v wpctl pactl amixer
```

If at least one is present locally, but the bar still doesn't
respond, the bar's launched environment may not have the same
PATH.  When started from a systemd user service, the unit
inherits a minimal environment; either:

```ini
# ~/.config/systemd/user/lispbar.service.d/path.conf
[Service]
Environment=PATH=%h/.local/bin:/usr/local/bin:/usr/bin:/bin
```

…or start it via the compositor's exec hook (which inherits the
full user session environment).

## Workspace clicks switch the wrong workspace

This usually means your workspace **names** differ from what
Lispbar's IPC call expects.

### Sway

`swaymsg workspace 3` switches to workspace "3" by name.  If your
workspaces have non-numeric names (e.g. "web"), clicking `web`
runs `swaymsg workspace web`, which works if the workspace was
created with that name.

### Hyprland

`hyprctl dispatch workspace NAME` — if NAME is a number string,
Hyprland treats it as the numeric workspace.  Named workspaces
need `name:NAME` prefixing, which we don't currently add.  If you
use named Hyprland workspaces and the click does nothing, file an
issue / patch `workspaces-click-handler`.

### niri

niri workspaces have **both** an `id` (globally unique) and a
`name` (string or null).  We pass the `name` to
`niri msg action focus-workspace`.  Unnamed workspaces have
their numeric index stringified, which niri accepts.

## "first-run: created ~/.config/lispbar/config.lisp" appears every launch

The config file isn't where the bar thinks it should be.  Check:

```sh
lispbar --print-paths
ls -la ~/.config/lispbar/
```

If `XDG_CONFIG_HOME` is set to an unexpected value (some shells
do that), make sure the file lives at:

```
$XDG_CONFIG_HOME/lispbar/config.lisp
```

To suppress seeding regardless: `LISPBAR_NO_SEED=1 lispbar`.

## "the function … is not known to be defined" warnings on build

Make sure you have Quicklisp set up and `cffi` quickloadable:

```sh
sbcl --non-interactive --eval '(ql:quickload :cffi)'
```

If that errors, install Quicklisp first (see [install.md](install.md)).

## Build fails with "wayland-scanner not found"

```sh
# Gentoo
emerge dev-libs/wayland-protocols

# Arch
pacman -S wayland-protocols

# Debian/Ubuntu
apt install wayland-protocols
```

The build script invokes `wayland-scanner` to generate
marshalling code for `wlr-layer-shell`.

## CFFI fails to load libwlbar.so or libwltray.so at runtime

Either the libraries weren't installed, or they're in a
non-standard location.

```sh
ls /usr/local/lib/lispbar/         # default install location
ls /usr/lib/lispbar/               # alternative
```

If they're elsewhere, point the loader at the right directory:

```sh
LD_LIBRARY_PATH=/path/to/lispbar/cshim lispbar
```

For dev builds the project's `Makefile` already exports
`LD_LIBRARY_PATH` so `make run` works without install.

## Getting more help

Run with `-v` and collect the output:

```sh
lispbar -v 2>&1 | tee /tmp/lispbar-trace.txt
```

The trace usually shows exactly which path failed, including the
relevant error message from libwayland / libdbus / glibc.
