# Makefile for lispbar

EMACS ?= emacs
BATCH = $(EMACS) -Q --batch

# Source files
EL_SOURCES = lispbar.el \
	lispbar-core.el \
	lispbar-modules.el \
	lispbar-exwm.el \
	lispbar-render.el \
	lispbar-theme.el \
	lispbar-config.el \
	$(wildcard modules/*.el)

# Compiled files
ELC_FILES = $(EL_SOURCES:.el=.elc)

# Test files
TEST_FILES = $(wildcard test/*-test.el)

.PHONY: all compile test lint clean package install

all: compile

compile: $(ELC_FILES)

%.elc: %.el
	@echo "Compiling $<..."
	@$(BATCH) -f batch-byte-compile $<

test: compile
	@echo "Running tests..."
	@$(BATCH) -L . -L test \
		-l test/test-helper \
		$(foreach test,$(TEST_FILES),-l $(test)) \
		-f ert-run-tests-batch-and-exit

lint:
	@echo "Running package-lint..."
	@$(BATCH) -L . \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		-f package-lint-batch-and-exit $(EL_SOURCES)

checkdoc:
	@echo "Running checkdoc..."
	@$(BATCH) -L . \
		--eval "(checkdoc-file \"lispbar.el\")" \
		--eval "(checkdoc-file \"lispbar-core.el\")"

clean:
	@echo "Cleaning..."
	@rm -f $(ELC_FILES)
	@rm -f *~ \#*\#
	@rm -f test/*~ test/\#*\#

package: clean
	@echo "Creating package..."
	@mkdir -p dist
	@tar -cf dist/lispbar-$(VERSION).tar --exclude=.git --exclude=dist .

install: compile
	@echo "Installing locally..."
	@mkdir -p ~/.emacs.d/site-lisp/lispbar
	@cp -r *.el *.elc modules ~/.emacs.d/site-lisp/lispbar/

uninstall:
	@echo "Uninstalling..."
	@rm -rf ~/.emacs.d/site-lisp/lispbar

# Development helpers
autoloads:
	@echo "Generating autoloads..."
	@$(BATCH) --eval "(require 'autoload)" \
		--eval "(let ((generated-autoload-file \"$(PWD)/lispbar-autoloads.el\")) \
			  (update-directory-autoloads \"$(PWD)\"))"

check: compile lint checkdoc test
	@echo "All checks passed!"

# CI targets
ci-test: clean compile test

ci-lint: lint checkdoc

# Version management
VERSION = $(shell grep "^;; Version:" lispbar.el | cut -d' ' -f3)

tag-release:
	@echo "Tagging version $(VERSION)..."
	@git tag -a "v$(VERSION)" -m "Release version $(VERSION)"
	@echo "Don't forget to push tags: git push origin v$(VERSION)"