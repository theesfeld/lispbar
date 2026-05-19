# Lispbar registry

A curated collection of community modules and themes, fetched on
demand by `lispbar registry install`.

## Browse from the command line

```sh
lispbar registry list                  # everything
lispbar registry list --kind module
lispbar registry list --kind theme
lispbar registry info :weather         # show details, requirements
```

## Install one

```sh
lispbar registry install :weather
lispbar registry install :tokyo-night
lispbar registry remove  :weather
lispbar registry update                # refresh installed files
```

Installation writes to your own XDG dirs — modules to
`$XDG_CONFIG_HOME/lispbar/modules/NAME.lisp`, themes to
`$XDG_CONFIG_HOME/lispbar/themes/NAME.lisp` — exactly as if you
had copied the file by hand.  Nothing is auto-enabled; reference
the new name from your `config.lisp`:

```lisp
(placement :right (:weather :cpu :memory :clock))   ; module
(theme :tokyo-night)                                ; theme
```

…then restart lispbar.

## What's in here

See [manifest.lisp](manifest.lisp) for the authoritative list with
SHA-256 checksums.  Currently:

### Modules

| Name        | Version | Summary                                            | Requires | Added      |
| ----------- | ------- | -------------------------------------------------- | -------- | ---------- |
| `:weather`  | 1.0.0   | Current weather from wttr.in                       | `curl`   | 2026-05-19 |
| `:cputemp`  | 1.0.0   | CPU package temperature via lm_sensors             | `sensors`| 2026-05-19 |
| `:diskspace`| 1.0.0   | Free space on a mount point                        | `df`     | 2026-05-19 |
| `:updates`  | 1.0.0   | Pending package updates (Gentoo/Arch/Debian/Fedora)| varies   | 2026-05-19 |
| `:pomodoro` | 1.0.0   | 25/5 timer; click to start/pause                   | —        | 2026-05-19 |

### Themes

| Name              | Version | Source                                | Added      |
| ----------------- | ------- | ------------------------------------- | ---------- |
| `:tokyo-night`    | 1.0.0   | folke/tokyonight.nvim "storm" variant | 2026-05-19 |
| `:solarized-dark` | 1.0.0   | Ethan Schoonover's Solarized          | 2026-05-19 |
| `:rose-pine`      | 1.0.0   | rose-pine.github.io main palette      | 2026-05-19 |

Full per-item metadata (version, license, tags, full description,
SHA-256) lives in [manifest.lisp](manifest.lisp); also reachable with
`lispbar registry info :NAME`.

## Submitting your own

Send a pull request that:

1. Adds one file:
   - `registry/modules/YOUR-NAME.lisp` for a module, or
   - `registry/themes/YOUR-NAME.lisp` for a theme.

2. Appends one `(register …)` entry to
   [manifest.lisp](manifest.lisp) with the file's SHA-256.

3. Updates the table above.

See [CONTRIBUTING.md](CONTRIBUTING.md) for the full checklist.
CI verifies the checksum, byte-compiles the file, and rejects
anything that doesn't load cleanly.

## Trust model

* Every PR is reviewed before merge — this is a **curated**
  registry, not an open package index.
* `lispbar registry install` verifies the SHA-256 from the
  manifest after download; a mismatch is a hard fail.
* The registry never auto-installs anything.  You always type
  `lispbar registry install NAME` explicitly.

If you'd rather not trust the registry at all, the bar works
fine — modules and themes you write yourself drop straight into
`$XDG_CONFIG_HOME/lispbar/{modules,themes}/`.
