;;;; manifest.lisp -- canonical index of every registry item.
;;;;
;;;; Generated and validated by `tools/registry-validate.sh' from
;;;; the contents of registry/{modules,themes}/.  Edits to this
;;;; file are required for every submission; the CI workflow
;;;; rejects PRs where a file's actual SHA-256 doesn't match what's
;;;; declared here.
;;;;
;;;; To submit a new module or theme:
;;;;   1. Add registry/modules/NAME.lisp (or themes/NAME.lisp)
;;;;   2. Append a `(register …)' entry below
;;;;   3. PR; CI validates the checksum + byte-compiles the file
;;;;
;;;; See CONTRIBUTING.md for the full process and field reference.

(in-package :lispbar-registry)

(register :weather
  :kind        :module
  :file        "modules/weather.lisp"
  :sha256      "78c993006902122c593a9019c41dfdc0662422c77b76e5c23c556b801228b262"
  :summary     "Current weather from wttr.in"
  :description "Pulls the current temperature and condition from wttr.in
on a configurable interval.  Location autodetects from the requester's
IP; override with `*weather-location*'.  Output is short (e.g. `☀ +21°C')
so it fits in a status bar.  Network failures are silent — the module
just keeps the last reading on screen."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :homepage    "https://wttr.in"
  :tags        '("weather" "system" "network")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld"
  :requires    '("curl"))

(register :cputemp
  :kind        :module
  :file        "modules/cputemp.lisp"
  :sha256      "f9f7b76ba200c65f0df377ed825f7f86d70c8f617c1fc70447a65edd0a919600"
  :summary     "CPU package temperature via lm_sensors"
  :description "Reads the CPU package temperature from `sensors -u' and
formats it as `CPU 54°C'.  Face goes `:warn' above 70 °C and `:urgent'
above 85 °C.  Useful for noticing thermal throttling before it bites."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :tags        '("system" "hardware" "monitoring")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld"
  :requires    '("sensors"))

(register :diskspace
  :kind        :module
  :file        "modules/diskspace.lisp"
  :sha256      "f37672305f79c529109389721b4d1ab9a7700ca95f65db04e97cee39a7bb4f84"
  :summary     "Free space on a chosen mount point"
  :description "Shows free space on a mount point as `/ 42G' (default `/').
Change the watched mount with `*diskspace-mount*'.  Face goes `:warn'
under 20 % free and `:urgent' under 5 %."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :tags        '("system" "storage")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld"
  :requires    '("df"))

(register :updates
  :kind        :module
  :file        "modules/updates.lisp"
  :sha256      "85d9e5a7b88d6a75464388e66c31da71a826af2e5913effa9c869d4a8dcfc8cd"
  :summary     "Pending OS package updates (emerge / pacman / apt / dnf)"
  :description "Counts pending package updates by detecting the host's
package manager.  Supports Gentoo (`eix-diff'), Arch (`checkupdates'),
Debian/Ubuntu (`apt list --upgradable'), and Fedora (`dnf check-update').
Shows nothing when there's nothing to update.  Polls slowly (default
30 min) so it doesn't hammer the network."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :tags        '("system" "packages")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld")

(register :pomodoro
  :kind        :module
  :file        "modules/pomodoro.lisp"
  :sha256      "14b5b5e7b2d47eb67d0bbafc2e680df9d78179ff0f3249c77f2b0cd3dd5a71fa"
  :summary     "Click-driven 25/5 pomodoro timer"
  :description "Standard 25-minute work / 5-minute break pomodoro timer.
Left-click toggles run/pause, middle-click resets, right-click skips to
the next phase.  Face is `:warn' during breaks, `:urgent' in the last
minute of a work block.  State is in-memory; restarts reset it."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :tags        '("productivity" "timer")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld")

(register :tokyo-night
  :kind        :theme
  :file        "themes/tokyo-night.lisp"
  :sha256      "f4427310c572561b41eea8ae7af12c6d73ac7613b9063bcd61162db3a09d983d"
  :summary     "Tokyo Night (storm) palette"
  :description "The popular `Tokyo Night Storm' colour scheme adapted to
lispbar's seven semantic faces.  Cool background, soft blues and purples
for accents, warm coral for `:urgent'."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :homepage    "https://github.com/enkia/tokyo-night-vscode-theme"
  :tags        '("dark" "blue" "purple")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld")

(register :solarized-dark
  :kind        :theme
  :file        "themes/solarized-dark.lisp"
  :sha256      "d5493b126243dc4256c9ab0d97f73ccc22329d03f321e4f3c52d0dcb2c8ace38"
  :summary     "Solarized Dark palette"
  :description "Ethan Schoonover's classic Solarized Dark colour scheme,
mapped to lispbar's faces.  Low-contrast base, accented with cyan, gold,
red, and green from the canonical palette."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :homepage    "https://ethanschoonover.com/solarized/"
  :tags        '("dark" "classic" "low-contrast")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld")

(register :rose-pine
  :kind        :theme
  :file        "themes/rose-pine.lisp"
  :sha256      "d81e4ebe99c57249833d41411f413675673b27a436eb9aef4691c375b3be1494"
  :summary     "Rosé Pine palette"
  :description "Warm, low-saturation Rosé Pine (main variant).  Muted
mauve background with rose, gold, and pine accents — easy on the eyes
during long sessions."
  :version     "1.0.0"
  :license     "GPL-3.0-or-later"
  :homepage    "https://rosepinetheme.com/"
  :tags        '("dark" "warm" "low-contrast")
  :added       "2026-05-19"
  :updated     "2026-05-19"
  :author      "theesfeld")
