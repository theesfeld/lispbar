# Lispbar on OpenRC

OpenRC is a system-level supervisor.  Per-user, per-session bars
like Lispbar don't fit OpenRC's model — they should be started by
your compositor, exactly like waybar.

There are two right answers, pick whichever you prefer:

## Option 1 — let the compositor start it (recommended)

### Sway

Add to `~/.config/sway/config`:

```
exec lispbar
```

`sway reload` (or `Mod+Shift+c`) and the bar appears.

### Hyprland

Add to `~/.config/hypr/hyprland.conf`:

```
exec-once = lispbar
```

`hyprctl reload`.

### River

Add to `~/.config/river/init`:

```
riverctl spawn lispbar
```

### Wayfire / Phosh / labwc / KDE Plasma

These all support either an "autostart" desktop entry or a per-WM
startup hook.  Use whichever the compositor documents.

## Option 2 — use a third-party user supervisor

If you really want supervised restart-on-failure semantics on a
non-systemd box you can layer one of:

* **`s6`** with `s6-rc` (mature, common on Gentoo).  Symlink
  `/usr/local/share/lispbar/init/runit/lispbar/` into your s6 scan
  directory — s6 understands runit-style `run` scripts.
* **`runit`** itself — same path, drop the directory into
  `~/runit/service/`.
* **`dinit`** — write a small service file pointing at
  `/usr/local/bin/lispbar`.

None of these need `make install` to do anything OpenRC-specific.
Lispbar exits cleanly on SIGTERM / SIGINT, so any supervisor that
restarts on failure will Just Work.

## Why no `/etc/init.d/lispbar`?

OpenRC services run as root by default and don't have a session bus
or `$WAYLAND_DISPLAY`.  Starting a graphical client from there
requires fragile workarounds (drop-privs, find-the-user's-DISPLAY).
Letting the compositor start the bar is the same model every other
Wayland status bar uses on OpenRC distros, and it's what `make
install` documents.
