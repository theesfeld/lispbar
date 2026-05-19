SCANNER   ?= wayland-scanner
PKGCONFIG ?= pkg-config

CFLAGS   ?= -O2 -fPIC -Wall -Wextra
LDFLAGS  ?=
WL_LIBS  := $(shell $(PKGCONFIG) --cflags --libs wayland-client)
DBUS_LIBS := $(shell $(PKGCONFIG) --cflags --libs dbus-1)

WLBAR_SRC := wlbar.c wlr-layer-shell-unstable-v1-protocol.c
WLBAR_HDR := wlbar.h wlr-layer-shell-unstable-v1-client-protocol.h

XML := ../protocols/wlr-layer-shell-unstable-v1.xml

.PHONY: all clean

all: libwlbar.so libwltray.so

# Wayland C shim ----------------------------------------------------

libwlbar.so: $(WLBAR_SRC) $(WLBAR_HDR)
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $@ $(WLBAR_SRC) $(WL_LIBS)

wlr-layer-shell-unstable-v1-client-protocol.h: $(XML)
	$(SCANNER) client-header $< $@

wlr-layer-shell-unstable-v1-protocol.c: $(XML)
	$(SCANNER) private-code $< $@

# StatusNotifierItem D-Bus shim ------------------------------------

libwltray.so: wltray.c wltray.h
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $@ wltray.c $(DBUS_LIBS)

clean:
	rm -f libwlbar.so libwltray.so \
	  wlr-layer-shell-unstable-v1-client-protocol.h \
	  wlr-layer-shell-unstable-v1-protocol.c
