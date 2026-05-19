/* wlbar.c - layer-shell client + wl_shm buffer plumbing for Lispbar.
 *
 * This file is the *only* C code in the project.  Everything user-
 * facing (modules, config, theming) is Lisp; this shim exists solely
 * because libwayland-client requires generated marshalling code per
 * protocol XML and that codegen is most cleanly done in C.
 */

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include <wayland-client.h>
#include "wlr-layer-shell-unstable-v1-client-protocol.h"
#include "wlbar.h"

/* The wlr-layer-shell protocol references xdg_popup_interface (used
 * by `get_popup'); we never call that request, but the generated
 * marshalling table still needs the symbol to resolve at link time.
 * Provide a benign stub so we don't pull in the entire xdg-shell
 * protocol just for one unused reference. */
const struct wl_interface xdg_popup_interface = {
    "xdg_popup", 1, 0, NULL, 0, NULL
};

#define BAR_NAMESPACE "lispbar"

/* ---- Global state.  A single bar instance per process is enough. */

static struct wl_display    *g_display   = NULL;
static struct wl_registry   *g_registry  = NULL;
static struct wl_compositor *g_compositor = NULL;
static struct wl_shm        *g_shm       = NULL;
static struct wl_output     *g_output    = NULL;
static struct zwlr_layer_shell_v1   *g_layer_shell   = NULL;
static struct wl_surface            *g_surface       = NULL;
static struct zwlr_layer_surface_v1 *g_layer_surface = NULL;
static struct wl_buffer             *g_buffer        = NULL;

static int       g_width  = 0;
static int       g_height = 0;
static int       g_requested_height = 0;
static int       g_shm_fd = -1;
static uint32_t *g_pixels = NULL;
static size_t    g_pixels_size = 0;
static int       g_configured = 0;
static int       g_closed     = 0;

/* ---- Forward decls ---- */

static int  shm_alloc(int width, int height);
static void shm_free(void);

/* ---- Registry handlers ---- */

static void registry_global(void *_data, struct wl_registry *reg, uint32_t id,
                            const char *iface, uint32_t version) {
    (void)_data;
    if (!strcmp(iface, wl_compositor_interface.name)) {
        g_compositor = wl_registry_bind(reg, id, &wl_compositor_interface, 4);
    } else if (!strcmp(iface, wl_shm_interface.name)) {
        g_shm = wl_registry_bind(reg, id, &wl_shm_interface, 1);
    } else if (!strcmp(iface, wl_output_interface.name) && !g_output) {
        g_output = wl_registry_bind(reg, id, &wl_output_interface, 2);
    } else if (!strcmp(iface, zwlr_layer_shell_v1_interface.name)) {
        uint32_t ver = version > 4 ? 4 : version;
        g_layer_shell = wl_registry_bind(reg, id, &zwlr_layer_shell_v1_interface, ver);
    }
}

static void registry_global_remove(void *_d, struct wl_registry *_r, uint32_t _i) {
    (void)_d; (void)_r; (void)_i;
}

static const struct wl_registry_listener registry_listener = {
    .global         = registry_global,
    .global_remove  = registry_global_remove,
};

/* ---- Layer surface handlers ---- */

static void layer_configure(void *_data,
                            struct zwlr_layer_surface_v1 *ls,
                            uint32_t serial, uint32_t w, uint32_t h) {
    (void)_data;
    zwlr_layer_surface_v1_ack_configure(ls, serial);
    if (w == 0) w = g_width  ? g_width  : 800;
    if (h == 0) h = g_requested_height;

    /* If size changed, reallocate the shm buffer. */
    if ((int)w != g_width || (int)h != g_height) {
        shm_free();
        g_width  = (int)w;
        g_height = (int)h;
        if (shm_alloc(g_width, g_height) < 0) {
            fprintf(stderr, "wlbar: shm_alloc failed: %s\n", strerror(errno));
            g_closed = 1;
            return;
        }
    }
    g_configured = 1;
}

static void layer_closed(void *_data, struct zwlr_layer_surface_v1 *_ls) {
    (void)_data; (void)_ls;
    g_closed = 1;
}

static const struct zwlr_layer_surface_v1_listener layer_listener = {
    .configure = layer_configure,
    .closed    = layer_closed,
};

/* ---- shm helpers ---- */

/* Create an anonymous tmpfile via memfd; portable enough on Linux. */
static int anon_shm_fd(size_t size) {
#ifdef SYS_memfd_create
    int fd = syscall(SYS_memfd_create, "lispbar-shm", 0);
#else
    int fd = -1;
#endif
    if (fd < 0) {
        char name[] = "/tmp/lispbar-shm-XXXXXX";
        fd = mkstemp(name);
        if (fd >= 0) unlink(name);
    }
    if (fd < 0) return -1;
    if (ftruncate(fd, size) < 0) { close(fd); return -1; }
    return fd;
}

static int shm_alloc(int width, int height) {
    int stride = width * 4;
    size_t size = (size_t)stride * (size_t)height;
    int fd = anon_shm_fd(size);
    if (fd < 0) return -1;
    void *map = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (map == MAP_FAILED) { close(fd); return -1; }

    struct wl_shm_pool *pool = wl_shm_create_pool(g_shm, fd, size);
    struct wl_buffer *buf = wl_shm_pool_create_buffer(pool, 0, width, height,
                                                      stride, WL_SHM_FORMAT_ARGB8888);
    wl_shm_pool_destroy(pool);

    g_shm_fd      = fd;
    g_pixels      = (uint32_t *)map;
    g_pixels_size = size;
    g_buffer      = buf;
    /* Pre-clear to transparent. */
    memset(g_pixels, 0, size);
    return 0;
}

static void shm_free(void) {
    if (g_buffer) { wl_buffer_destroy(g_buffer); g_buffer = NULL; }
    if (g_pixels) { munmap(g_pixels, g_pixels_size); g_pixels = NULL; g_pixels_size = 0; }
    if (g_shm_fd >= 0) { close(g_shm_fd); g_shm_fd = -1; }
}

/* ---- Public API ---- */

int wlbar_init(int height) {
    if (height <= 0) height = 28;
    g_requested_height = height;
    g_configured = 0;
    g_closed     = 0;

    g_display = wl_display_connect(NULL);
    if (!g_display) {
        fprintf(stderr, "wlbar: cannot connect to Wayland display\n");
        return -1;
    }
    g_registry = wl_display_get_registry(g_display);
    wl_registry_add_listener(g_registry, &registry_listener, NULL);
    wl_display_roundtrip(g_display);

    if (!g_compositor || !g_shm || !g_layer_shell) {
        fprintf(stderr,
                "wlbar: missing globals (compositor=%p shm=%p layer-shell=%p);"
                " compositor probably lacks wlr-layer-shell\n",
                (void*)g_compositor, (void*)g_shm, (void*)g_layer_shell);
        wlbar_shutdown();
        return -1;
    }

    g_surface = wl_compositor_create_surface(g_compositor);
    g_layer_surface = zwlr_layer_shell_v1_get_layer_surface(
        g_layer_shell, g_surface, g_output,
        ZWLR_LAYER_SHELL_V1_LAYER_TOP, BAR_NAMESPACE);

    /* Anchor top|left|right => span the output's full width. */
    uint32_t anchor =
        ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
        ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
        ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;
    zwlr_layer_surface_v1_set_anchor(g_layer_surface, anchor);
    zwlr_layer_surface_v1_set_size(g_layer_surface, 0, height);
    zwlr_layer_surface_v1_set_exclusive_zone(g_layer_surface, height);
    zwlr_layer_surface_v1_set_keyboard_interactivity(
        g_layer_surface, ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE);
    zwlr_layer_surface_v1_add_listener(g_layer_surface, &layer_listener, NULL);

    wl_surface_commit(g_surface);
    /* Two roundtrips so we receive the configure event and ack it. */
    wl_display_roundtrip(g_display);
    wl_display_roundtrip(g_display);

    if (!g_configured) {
        fprintf(stderr, "wlbar: never received configure event\n");
        wlbar_shutdown();
        return -1;
    }
    return 0;
}

void wlbar_shutdown(void) {
    shm_free();
    if (g_layer_surface) { zwlr_layer_surface_v1_destroy(g_layer_surface); g_layer_surface = NULL; }
    if (g_surface)       { wl_surface_destroy(g_surface);                  g_surface = NULL; }
    if (g_layer_shell)   { zwlr_layer_shell_v1_destroy(g_layer_shell);     g_layer_shell = NULL; }
    if (g_output)        { wl_output_destroy(g_output);                    g_output = NULL; }
    if (g_shm)           { wl_shm_destroy(g_shm);                          g_shm = NULL; }
    if (g_compositor)    { wl_compositor_destroy(g_compositor);            g_compositor = NULL; }
    if (g_registry)      { wl_registry_destroy(g_registry);                g_registry = NULL; }
    if (g_display)       { wl_display_disconnect(g_display);               g_display = NULL; }
    g_width = g_height = 0;
    g_configured = 0;
}

int       wlbar_width   (void) { return g_width;  }
int       wlbar_height  (void) { return g_height; }
uint32_t *wlbar_pixels  (void) { return g_pixels; }
int       wlbar_stride  (void) { return g_width * 4; }
int       wlbar_fd      (void) { return g_display ? wl_display_get_fd(g_display) : -1; }
int       wlbar_closed  (void) { return g_closed; }

void wlbar_commit(void) {
    if (!g_buffer || !g_surface) return;
    wl_surface_attach(g_surface, g_buffer, 0, 0);
    wl_surface_damage_buffer(g_surface, 0, 0, g_width, g_height);
    wl_surface_commit(g_surface);
    wl_display_flush(g_display);
}

int wlbar_poll(int timeout_ms) {
    if (!g_display) return -1;
    /* Flush pending requests, then block on the fd. */
    while (wl_display_prepare_read(g_display) != 0) {
        if (wl_display_dispatch_pending(g_display) < 0) return -1;
    }
    wl_display_flush(g_display);
    struct pollfd pfd = { wl_display_get_fd(g_display), POLLIN, 0 };
    int r = poll(&pfd, 1, timeout_ms);
    if (r < 0) { wl_display_cancel_read(g_display); return -1; }
    if (r == 0) { wl_display_cancel_read(g_display); return 0; }
    if (wl_display_read_events(g_display) < 0) return -1;
    if (wl_display_dispatch_pending(g_display) < 0) return -1;
    return 1;
}
