<!--
Thanks for the PR.  If you're submitting a registry item, please
keep the relevant section below and delete the others.  See
registry/CONTRIBUTING.md for the full checklist.
-->

## Type of change

- [ ] Bug fix
- [ ] New feature in the main binary
- [ ] **Registry submission**: new module
- [ ] **Registry submission**: new theme
- [ ] Documentation
- [ ] Other (describe):

---

## Description

<!-- One or two paragraphs.  What does this PR do, and why? -->

---

## For registry submissions

- [ ] File added at `registry/modules/<name>.lisp` or
      `registry/themes/<name>.lisp`
- [ ] `(register …)` entry added to `registry/manifest.lisp`
      with correct `:sha256`
- [ ] Tested locally; the module runs without warnings on a real
      Wayland session, or the theme renders correctly with at
      least one built-in module
- [ ] `tools/registry-validate.sh` passes
- [ ] `registry/README.md` table updated
- [ ] (if module) defvar tunables documented with docstrings
- [ ] (if module) errors in user-supplied callbacks are caught
      with `handler-case`
- [ ] License of contribution: GPL-3.0-or-later (project default).
      I have the right to submit this code under that license.

---

## Test plan

<!-- How did you verify this works?  -->
