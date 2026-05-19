# Installing Lispbar

Lispbar builds from source to a single self-contained binary. The
target machine only needs the runtime shared libraries; SBCL is not
required at runtime (its runtime is embedded in the binary).

## Required at build time

| Package                 | Purpose                                       |
| ----------------------- | --------------------------------------------- |
| `sbcl`                  | Common Lisp compiler                          |
| Quicklisp (provides `cffi`) | FFI bindings                              |
| `wayland-protocols` + `wayland-scanner` | layer-shell glue codegen      |
| `libwayland-client`     | talk to the Wayland compositor                |
| `libdbus-1`             | tray protocol                                 |
| `libcairo`              | rendering                                     |
| `libpangocairo`, `libpango` (optional) | proper text shaping            |
| `librsvg2-bin` (optional, **strongly recommended**) | SVG icon rasterisation |

The optional packages are dlopen'd at runtime; the binary still
works without them (text fallback for tray icons, cairo "toy text"
API for the main bar).

## Per-distro recipes

### Gentoo (OpenRC or systemd)

```sh
emerge dev-lisp/sbcl \
       dev-libs/wayland dev-libs/wayland-protocols \
       sys-apps/dbus \
       x11-libs/cairo x11-libs/pango \
       gnome-base/librsvg
```

Install Quicklisp (one-time):

```sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --non-interactive --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql-util:without-prompting (ql:add-to-init-file))'
rm quicklisp.lisp
```

Build and install:

```sh
git clone https://github.com/theesfeld/lispbar
cd lispbar
make build
sudo make install                # /usr/local/{bin,lib,share}/lispbar
```

### Arch Linux

```sh
sudo pacman -S sbcl quicklisp wayland-protocols dbus cairo pango librsvg
echo '#-quicklisp
(let ((quicklisp-init "/usr/share/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init) (load quicklisp-init)))' \
  >> ~/.sbclrc

git clone https://github.com/theesfeld/lispbar
cd lispbar
make build
sudo make install
```

### Debian / Ubuntu

```sh
sudo apt install sbcl cl-quicklisp wayland-protocols libwayland-dev \
                 libdbus-1-dev libcairo2-dev libpango1.0-dev librsvg2-bin

sbcl --non-interactive --load /usr/share/common-lisp/source/quicklisp/quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql-util:without-prompting (ql:add-to-init-file))'

git clone https://github.com/theesfeld/lispbar
cd lispbar
make build
sudo make install
```

### Fedora

```sh
sudo dnf install sbcl wayland-protocols-devel wayland-devel \
                 dbus-devel cairo-devel pango-devel librsvg2-tools
# Install Quicklisp manually (see Arch instructions; the URL works
# the same).
git clone https://github.com/theesfeld/lispbar
cd lispbar
make build
sudo make install
```

### Other distros

The only hard requirements are SBCL with Quicklisp, libwayland-client,
libdbus-1, and libcairo + their headers.  Adapt accordingly.

## What `make install` lays down

```
/usr/local/
├── bin/lispbar                                          # the binary
├── lib/lispbar/
│   ├── libwlbar.so                                      # layer-shell C glue
│   └── libwltray.so                                     # SNI D-Bus glue
├── lib/systemd/user/lispbar.service                     # when INIT=systemd
└── share/
    ├── doc/lispbar/{README.md, LICENSE}
    └── lispbar/
        ├── examples/
        │   ├── config.lisp                              # starter config
        │   ├── modules/loadavg.lisp                     # example module
        │   └── themes/dracula.lisp                      # example theme
        └── init/
            ├── systemd/lispbar.service
            ├── runit/lispbar/run
            └── openrc/README.md
```

`make install` accepts the standard `PREFIX` (default `/usr/local`)
and `DESTDIR` (for staged installs) variables.  Init-system wiring
is auto-detected; override with `INIT=systemd | runit | openrc | none`.

## Init system

| Init    | How to start lispbar                                                  |
| ------- | --------------------------------------------------------------------- |
| systemd | `systemctl --user enable --now lispbar`                               |
| runit   | `ln -s /usr/local/share/lispbar/init/runit/lispbar ~/runit/service/`  |
| OpenRC  | `exec lispbar` in `~/.config/sway/config` (compositor handles it)     |
| any     | `exec lispbar` in your compositor config always works                 |

See [compositors.md](compositors.md) for the per-compositor exec
hook.

## First run

The very first time `lispbar` starts (or `lispbar --init` is run
explicitly), it creates:

```
~/.config/lispbar/
├── config.lisp           # opinionated default config; edit freely
├── modules/              # drop *.lisp here for custom modules
└── themes/               # drop *.lisp here for custom themes
```

The seeded `config.lisp` is the same one in
`/usr/local/share/lispbar/examples/config.lisp`.  Every option is
documented inline with its built-in default in a comment.

To skip the seeding (server / scripted installs):

```sh
LISPBAR_NO_SEED=1 lispbar    # one-shot opt-out
lispbar --no-seed            # equivalent CLI flag
```

To explicitly re-seed an existing config:

```sh
lispbar --init               # refuses if config already exists
lispbar --init --force       # overwrites
```

## Sanity checks

```sh
lispbar --print-paths        # XDG resolution
lispbar --show-extensions    # which user files got loaded
lispbar --list-modules       # registry inventory
lispbar --list-themes        # theme inventory
lispbar --once               # one frame of stdout output
lispbar --once --output json # one frame of waybar-style JSON
```

If any of those misbehave, see [troubleshooting.md](troubleshooting.md).

## Uninstalling

```sh
sudo make uninstall          # removes everything make install put down
                             # leaves your ~/.config/lispbar/ alone
rm -rf ~/.config/lispbar     # if you also want to wipe your config
rm -rf ~/.cache/lispbar      # icon cache; auto-rebuilds next launch
```
