# Compositors

Lispbar is a `wlr-layer-shell-unstable-v1` client.  Any compositor
that implements that protocol is supported.  Per-compositor notes
below.

## Sway

Tested.  Workspaces module detects Sway via `$SWAYSOCK` and uses
`swaymsg` for workspace queries and switches.

### Start the bar

Add to `~/.config/sway/config`:

```
exec lispbar
```

Reload sway (`Mod+Shift+c`).  The bar appears anchored to the top
of every output.

### Workspace clicks

Left-click on a workspace number runs:

```
swaymsg workspace NAME
```

Scroll on the workspaces module runs `swaymsg workspace prev` or
`swaymsg workspace next`.

### Multi-output

Lispbar creates one layer surface per `wl_output`, so the bar
appears independently on every monitor.  Hot-plugging a monitor
in/out works without restart.

## Hyprland

Tested via IPC.  Workspaces module detects Hyprland via
`$HYPRLAND_INSTANCE_SIGNATURE` and uses `hyprctl`.

### Start the bar

Add to `~/.config/hypr/hyprland.conf`:

```
exec-once = lispbar
```

Reload: `hyprctl reload`.

### Workspace clicks

Left-click on a workspace name runs:

```
hyprctl dispatch workspace NAME
```

Scroll moves to the next/prev workspace via:

```
hyprctl dispatch workspace e+1   # next
hyprctl dispatch workspace e-1   # prev
```

## niri

Tested via JSON IPC (`niri msg --json workspaces`).  Detection
priority is `$NIRI_SOCKET` then a probe of `niri msg version`.

### Start the bar

Add to your niri config:

```
spawn-at-startup "lispbar"
```

…or run it from your `kanshi`/`mako`/`startupd` chain like any
other long-running service.

### Multi-monitor caveat

niri has **per-output workspaces** — every monitor has its own
1, 2, 3, ... numbering.  By default Lispbar's workspace module
filters to the focused output's workspaces only.  Set
`*workspaces-scope*` to `:all` if you want everything from every
monitor (note: you'll see duplicate numbers).

### Workspace clicks

Left-click runs:

```
niri msg action focus-workspace NAME
```

Scroll runs:

```
niri msg action focus-workspace-up    # scroll up
niri msg action focus-workspace-down  # scroll down
```

## Other wlroots compositors

Any compositor that implements `wlr-layer-shell-unstable-v1` will
display the bar.  The `:workspaces` module needs the compositor's
own IPC to switch workspaces — currently Sway, Hyprland and niri
are wired up.

If your compositor isn't on the list:

* The bar will still appear.
* Other modules (clock, cpu, memory, audio, network, …) all work.
* `:workspaces` will stay silent (auto-detection logs "no
  workspaces backend detected" at `:debug`).

To add a new backend, see "Adding a compositor backend" below.

## Plain GNOME / Plasma Wayland

Mainline GNOME Wayland does **not** speak `wlr-layer-shell`.  Use
KDE Plasma with Wayland, which does support layer-shell (since
Plasma 5.24), or stick with a wlroots-based compositor.

## Adding a compositor backend

For workspace support on a compositor not yet listed:

1. Add a fetcher to `src/modules/workspaces.lisp`:

```lisp
(defun workspaces-river ()
  "Return a list of WORKSPACE records for river, or NIL."
  ...)
```

It must return `(list-of WORKSPACE)`, where each `WORKSPACE` is a
struct with `:name`, `:output`, `:focused`.

2. Add an env-var or probe to `detect-workspaces-source`:

```lisp
(or ...
    (and (run-capture "riverctl" "list-tags") :river))
```

3. Wire the click handlers in `workspaces-click-handler` and
   `workspaces-scroll-handler`:

```lisp
(case source
  ...
  (:river (uiop:launch-program (list "riverctl" "set-focused-tags"
                                      (workspace-name data)))))
```

That's it.  The scope filter and the bbox-tracking renderer work
across all backends uniformly.

## Compositor-specific tips

### Sway: stop swaybar from also running

If your `~/.config/sway/config` has a `bar { … }` block, Sway will
also start its built-in swaybar.  Two bars stacked is rarely
what you want.  Either:

```
bar { swaybar_command true }    # tell Sway not to spawn one
```

…or delete the `bar { … }` block entirely.  Lispbar reserves its
own exclusive zone via the layer-shell protocol; sway tiles your
windows around it.

### Hyprland: layer rules

If you want lispbar above a fullscreen window:

```
# ~/.config/hypr/hyprland.conf
layerrule = blur, lispbar
layerrule = ignorezero, lispbar
```

The `lispbar` name comes from the `namespace` field we set on the
layer surface; that's stable across versions.

### niri: combined with `niri overview`

niri's overview shows all workspaces.  Combine the bar's
`:workspaces` module with the overview hotkey for a nice
mouse-and-keyboard mix:

```
binds {
    Mod+Tab { toggle-overview; }
}
```

## Environment variables the bar reads

| Var                            | Used for                                          |
| ------------------------------ | ------------------------------------------------- |
| `WAYLAND_DISPLAY`              | libwayland connection                             |
| `SWAYSOCK`                     | Sway IPC discovery                                |
| `HYPRLAND_INSTANCE_SIGNATURE`  | Hyprland IPC discovery                            |
| `NIRI_SOCKET`                  | niri IPC discovery                                |
| `XDG_RUNTIME_DIR`              | session bus, Wayland socket                        |
| `XDG_*`                        | config + data + state + cache paths                |
| `DBUS_SESSION_BUS_ADDRESS`     | tray (SNI host)                                    |
