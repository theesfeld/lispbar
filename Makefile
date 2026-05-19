SBCL ?= sbcl
PREFIX ?= /usr/local

.PHONY: all build shim install uninstall clean run test

all: build

shim:
	$(MAKE) -C cshim

build: shim lispbar

lispbar: $(wildcard src/*.lisp src/modules/*.lisp src/output/*.lisp) \
         lispbar.asd build.lisp cshim/libwlbar.so
	LD_LIBRARY_PATH=$(CURDIR)/cshim:$$LD_LIBRARY_PATH \
	  $(SBCL) --non-interactive --load build.lisp

install: build
	install -Dm755 lispbar           $(DESTDIR)$(PREFIX)/bin/lispbar
	install -Dm755 cshim/libwlbar.so $(DESTDIR)$(PREFIX)/lib/lispbar/libwlbar.so
	install -Dm644 examples/config.lisp \
	  $(DESTDIR)$(PREFIX)/share/lispbar/example-config.lisp
	install -Dm644 examples/modules/loadavg.lisp \
	  $(DESTDIR)$(PREFIX)/share/lispbar/examples/modules/loadavg.lisp
	install -Dm644 examples/themes/dracula.lisp \
	  $(DESTDIR)$(PREFIX)/share/lispbar/examples/themes/dracula.lisp
	install -Dm644 systemd/lispbar.service \
	  $(DESTDIR)$(PREFIX)/lib/systemd/user/lispbar.service

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/lispbar
	rm -f $(DESTDIR)$(PREFIX)/lib/lispbar/libwlbar.so
	rm -rf $(DESTDIR)$(PREFIX)/share/lispbar
	rm -f $(DESTDIR)$(PREFIX)/lib/systemd/user/lispbar.service

run: build
	LD_LIBRARY_PATH=$(CURDIR)/cshim:$$LD_LIBRARY_PATH ./lispbar

test: build
	./lispbar --list-modules
	./lispbar --once --output stdout
	./lispbar --once --output json

clean:
	rm -f lispbar
	find . -name '*.fasl' -delete
	$(MAKE) -C cshim clean
	rm -rf ~/.cache/common-lisp/*/$(PWD) 2>/dev/null || true
