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
;;;; See CONTRIBUTING.md for the full process.

(in-package :lispbar-registry)

(register :weather
  :kind     :module
  :file     "modules/weather.lisp"
  :sha256   "78c993006902122c593a9019c41dfdc0662422c77b76e5c23c556b801228b262"
  :doc      "Current weather from wttr.in"
  :author   "theesfeld"
  :requires '("curl"))

(register :cputemp
  :kind     :module
  :file     "modules/cputemp.lisp"
  :sha256   "f9f7b76ba200c65f0df377ed825f7f86d70c8f617c1fc70447a65edd0a919600"
  :doc      "CPU package temperature via lm_sensors"
  :author   "theesfeld"
  :requires '("sensors"))

(register :diskspace
  :kind     :module
  :file     "modules/diskspace.lisp"
  :sha256   "f37672305f79c529109389721b4d1ab9a7700ca95f65db04e97cee39a7bb4f84"
  :doc      "Free space on a chosen mount point"
  :author   "theesfeld"
  :requires '("df"))

(register :updates
  :kind     :module
  :file     "modules/updates.lisp"
  :sha256   "85d9e5a7b88d6a75464388e66c31da71a826af2e5913effa9c869d4a8dcfc8cd"
  :doc      "Pending OS package updates (emerge / pacman / apt / dnf)"
  :author   "theesfeld")

(register :pomodoro
  :kind     :module
  :file     "modules/pomodoro.lisp"
  :sha256   "14b5b5e7b2d47eb67d0bbafc2e680df9d78179ff0f3249c77f2b0cd3dd5a71fa"
  :doc      "Click-driven 25/5 pomodoro timer"
  :author   "theesfeld")

(register :tokyo-night
  :kind     :theme
  :file     "themes/tokyo-night.lisp"
  :sha256   "f4427310c572561b41eea8ae7af12c6d73ac7613b9063bcd61162db3a09d983d"
  :doc      "Tokyo Night (storm) palette"
  :author   "theesfeld")

(register :solarized-dark
  :kind     :theme
  :file     "themes/solarized-dark.lisp"
  :sha256   "d5493b126243dc4256c9ab0d97f73ccc22329d03f321e4f3c52d0dcb2c8ace38"
  :doc      "Solarized Dark palette"
  :author   "theesfeld")

(register :rose-pine
  :kind     :theme
  :file     "themes/rose-pine.lisp"
  :sha256   "d81e4ebe99c57249833d41411f413675673b27a436eb9aef4691c375b3be1494"
  :doc      "Rosé Pine palette"
  :author   "theesfeld")
