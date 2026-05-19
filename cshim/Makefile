SCANNER ?= wayland-scanner
PKGCONFIG ?= pkg-config

CFLAGS  ?= -O2 -fPIC -Wall -Wextra
LDFLAGS ?=
LIBS    := $(shell $(PKGCONFIG) --cflags --libs wayland-client)

SOURCES := wlbar.c wlr-layer-shell-unstable-v1-protocol.c
HEADERS := wlbar.h wlr-layer-shell-unstable-v1-client-protocol.h

XML := ../protocols/wlr-layer-shell-unstable-v1.xml

.PHONY: all clean

all: libwlbar.so

libwlbar.so: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $@ $(SOURCES) $(LIBS)

wlr-layer-shell-unstable-v1-client-protocol.h: $(XML)
	$(SCANNER) client-header $< $@

wlr-layer-shell-unstable-v1-protocol.c: $(XML)
	$(SCANNER) private-code $< $@

clean:
	rm -f libwlbar.so \
	  wlr-layer-shell-unstable-v1-client-protocol.h \
	  wlr-layer-shell-unstable-v1-protocol.c
