SBCL ?= sbcl
PREFIX ?= /usr/local

.PHONY: all build install uninstall clean run test

all: build

build: lispbar

lispbar: $(wildcard src/*.lisp src/modules/*.lisp src/output/*.lisp) lispbar.asd build.lisp
	$(SBCL) --non-interactive --no-userinit --no-sysinit --load build.lisp

install: build
	install -Dm755 lispbar $(DESTDIR)$(PREFIX)/bin/lispbar
	install -Dm644 examples/config.lisp \
	  $(DESTDIR)/usr/share/lispbar/example-config.lisp

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/lispbar
	rm -f $(DESTDIR)/usr/share/lispbar/example-config.lisp

run: build
	./lispbar

test: build
	./lispbar --list-modules
	./lispbar --once --output stdout
	./lispbar --once --output json

clean:
	rm -f lispbar
	find . -name '*.fasl' -delete
	rm -rf ~/.cache/common-lisp/*/$(PWD)
