# Lispbar build / install Makefile.
#
# Tunables (override on the command line as needed):
#
#   PREFIX    installation prefix         (default /usr/local)
#   DESTDIR   staged install root         (default empty)
#   SBCL      Common Lisp compiler        (default sbcl)
#   INIT      init system: systemd | runit | openrc | auto | none
#                                         (default auto-detect)

SBCL    ?= sbcl
PREFIX  ?= /usr/local
DESTDIR ?=
INIT    ?= auto

BIN_DIR     := $(DESTDIR)$(PREFIX)/bin
LIB_DIR     := $(DESTDIR)$(PREFIX)/lib/lispbar
SHARE_DIR   := $(DESTDIR)$(PREFIX)/share/lispbar
DOC_DIR     := $(DESTDIR)$(PREFIX)/share/doc/lispbar
MAN_DIR     := $(DESTDIR)$(PREFIX)/share/man
SYSTEMD_DIR := $(DESTDIR)$(PREFIX)/lib/systemd/user

# Man pages reference $PREFIX in their FILES / SEE ALSO sections so
# the installed copy points at the right paths.  Use sed to substitute
# `@PREFIX@' at install time.
MAN_SUBST := sed -e 's|@PREFIX@|$(PREFIX)|g'

# Auto-detect init system when INIT=auto.
ifeq ($(INIT),auto)
  ifneq ($(wildcard /run/systemd/system),)
    DETECTED_INIT := systemd
  else ifneq ($(wildcard /etc/runit),)
    DETECTED_INIT := runit
  else ifneq ($(wildcard /sbin/openrc-run /usr/sbin/openrc-run),)
    DETECTED_INIT := openrc
  else
    DETECTED_INIT := none
  endif
else
  DETECTED_INIT := $(INIT)
endif

.PHONY: all build shim run test clean man \
        install install-bin install-lib install-share install-init install-man \
        uninstall uninstall-bin uninstall-lib uninstall-share uninstall-init uninstall-man \
        help

all: build

help:
	@echo "Lispbar build targets:"
	@echo "  build      compile the binary (default)"
	@echo "  shim       compile cshim/libwlbar.so only"
	@echo "  run        build and launch from the tree"
	@echo "  test       build and exercise --list-modules and one-shot output"
	@echo "  install    install everything; INIT=$(DETECTED_INIT)"
	@echo "  uninstall  remove everything install added"
	@echo "  man        preview the man pages with groff"
	@echo "  clean      remove build artefacts"
	@echo
	@echo "Tunables:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  DESTDIR=$(DESTDIR)"
	@echo "  SBCL=$(SBCL)"
	@echo "  INIT=$(INIT)  (resolved to: $(DETECTED_INIT))"

# --- Build ----------------------------------------------------------------

shim:
	$(MAKE) -C cshim

build: shim lispbar

lispbar: $(wildcard src/*.lisp src/modules/*.lisp src/output/*.lisp) \
         examples/config.lisp lispbar.asd build.lisp \
         cshim/libwlbar.so cshim/libwltray.so
	@# Drop the cached FASL for src/config.lisp so the `#.' that
	@# reads examples/config.lisp at compile time runs again.
	@find ~/.cache/common-lisp -path '*lispbar/src/config.*' \
	  -delete 2>/dev/null || true
	LD_LIBRARY_PATH=$(CURDIR)/cshim:$$LD_LIBRARY_PATH \
	  $(SBCL) --non-interactive --load build.lisp

run: build
	LD_LIBRARY_PATH=$(CURDIR)/cshim:$$LD_LIBRARY_PATH ./lispbar

test: build
	./lispbar --list-modules
	./lispbar --list-themes
	./lispbar --print-paths
	./lispbar --once --output stdout
	./lispbar --once --output json
	sh tests/run-tests.sh

clean:
	rm -f lispbar
	find . -name '*.fasl' -delete
	$(MAKE) -C cshim clean
	rm -rf ~/.cache/common-lisp/*/$(CURDIR) 2>/dev/null || true

# --- Install --------------------------------------------------------------

install: install-bin install-lib install-share install-init install-man
	@echo
	@echo "Installed Lispbar to $(DESTDIR)$(PREFIX)."
	@echo "Init system: $(DETECTED_INIT)"
	@case "$(DETECTED_INIT)" in \
	  systemd) \
	    echo "  systemctl --user enable --now lispbar" ;; \
	  runit) \
	    echo "  ln -s $(PREFIX)/share/lispbar/init/runit/lispbar ~/runit/service/" ;; \
	  openrc|none) \
	    echo "  Add 'exec lispbar' (Sway) or 'exec-once = lispbar' (Hyprland)" ;; \
	esac
	@echo
	@echo "Sample config + example user module/theme are under"
	@echo "  $(PREFIX)/share/lispbar/examples/"
	@echo "Copy into ~/.config/lispbar/ to customise."

install-bin: build
	install -Dm755 lispbar           $(BIN_DIR)/lispbar

install-lib: build
	install -Dm755 cshim/libwlbar.so  $(LIB_DIR)/libwlbar.so
	install -Dm755 cshim/libwltray.so $(LIB_DIR)/libwltray.so

install-share:
	install -Dm644 examples/config.lisp \
	  $(SHARE_DIR)/examples/config.lisp
	install -Dm644 examples/modules/loadavg.lisp \
	  $(SHARE_DIR)/examples/modules/loadavg.lisp
	install -Dm644 examples/themes/dracula.lisp \
	  $(SHARE_DIR)/examples/themes/dracula.lisp
	install -Dm644 README.md   $(DOC_DIR)/README.md
	install -Dm644 LICENSE     $(DOC_DIR)/LICENSE
	for f in docs/*.md; do \
	  install -Dm644 $$f $(DOC_DIR)/$$(basename $$f); \
	done
	# Drop every init recipe under share/ so users can pick one even if
	# auto-detection chose differently.
	install -Dm644 init/systemd/lispbar.service \
	  $(SHARE_DIR)/init/systemd/lispbar.service
	install -Dm755 init/runit/lispbar/run \
	  $(SHARE_DIR)/init/runit/lispbar/run
	install -Dm644 init/openrc/README.md \
	  $(SHARE_DIR)/init/openrc/README.md

install-init:
ifeq ($(DETECTED_INIT),systemd)
	install -Dm644 init/systemd/lispbar.service \
	  $(SYSTEMD_DIR)/lispbar.service
endif
# runit and openrc are "no-op" at the system level; their recipes
# live under $(SHARE_DIR)/init/ (placed by install-share) and the
# user wires them up explicitly.  See README.md for details.

# Man pages.  @PREFIX@ is substituted so e.g. "see @PREFIX@/share/..."
# resolves to the actual install prefix on the target system.
install-man:
	install -d $(MAN_DIR)/man1 $(MAN_DIR)/man5 $(MAN_DIR)/man7
	$(MAN_SUBST) docs/man/lispbar.1         > $(MAN_DIR)/man1/lispbar.1
	chmod 644 $(MAN_DIR)/man1/lispbar.1
	$(MAN_SUBST) docs/man/lispbar.conf.5    > $(MAN_DIR)/man5/lispbar.conf.5
	chmod 644 $(MAN_DIR)/man5/lispbar.conf.5
	$(MAN_SUBST) docs/man/lispbar-modules.7 > $(MAN_DIR)/man7/lispbar-modules.7
	chmod 644 $(MAN_DIR)/man7/lispbar-modules.7
	$(MAN_SUBST) docs/man/lispbar-themes.7  > $(MAN_DIR)/man7/lispbar-themes.7
	chmod 644 $(MAN_DIR)/man7/lispbar-themes.7
	@# Rebuild the mandb index if we're installing system-wide and
	@# the tool is around.  Best-effort; skip silently otherwise.
	@if [ -z "$(DESTDIR)" ] && command -v mandb >/dev/null 2>&1; then \
	   mandb -q $(PREFIX)/share/man 2>/dev/null || true; \
	fi

# Render man pages with groff for a sanity preview (no install).
man:
	@for p in docs/man/lispbar.1 docs/man/lispbar.conf.5 \
	          docs/man/lispbar-modules.7 docs/man/lispbar-themes.7; do \
	    echo "=== $$p ==="; \
	    $(MAN_SUBST) $$p | groff -Tutf8 -mandoc 2>/dev/null | head -40; \
	    echo ""; \
	done

# --- Uninstall ------------------------------------------------------------

uninstall: uninstall-bin uninstall-lib uninstall-share uninstall-init uninstall-man
	@echo "Uninstalled Lispbar from $(DESTDIR)$(PREFIX)."
	@echo "Your config under \$$XDG_CONFIG_HOME/lispbar/ is untouched."

uninstall-bin:
	rm -f $(BIN_DIR)/lispbar

uninstall-lib:
	rm -f $(LIB_DIR)/libwlbar.so $(LIB_DIR)/libwltray.so
	-rmdir $(LIB_DIR) 2>/dev/null || true

uninstall-share:
	rm -rf $(SHARE_DIR)
	rm -rf $(DOC_DIR)

uninstall-init:
	rm -f $(SYSTEMD_DIR)/lispbar.service

uninstall-man:
	rm -f $(MAN_DIR)/man1/lispbar.1
	rm -f $(MAN_DIR)/man5/lispbar.conf.5
	rm -f $(MAN_DIR)/man7/lispbar-modules.7
	rm -f $(MAN_DIR)/man7/lispbar-themes.7
	@if [ -z "$(DESTDIR)" ] && command -v mandb >/dev/null 2>&1; then \
	   mandb -q $(PREFIX)/share/man 2>/dev/null || true; \
	fi
