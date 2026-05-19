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
#include <sys/syscall.h>
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
#define MAX_OUTPUTS   16

struct bar_output {
    struct wl_output             *wl_output;
    uint32_t                      registry_id;
    char                         *name;

    struct wl_surface            *surface;
    struct zwlr_layer_surface_v1 *layer_surface;
    struct wl_buffer             *buffer;

    int        shm_fd;
    uint32_t  *pixels;
    size_t     pixels_size;
    int        width;
    int        height;
    int        configured;
    int        closed;
};

/* ---- Globals ---- */

static struct wl_display    *g_display    = NULL;
static struct wl_registry   *g_registry   = NULL;
static struct wl_compositor *g_compositor = NULL;
static struct wl_shm        *g_shm        = NULL;
static struct zwlr_layer_shell_v1 *g_layer_shell = NULL;

static struct bar_output  g_outputs[MAX_OUTPUTS];
static int                g_output_count     = 0;
static int                g_requested_height = 28;
static int                g_requested_position = WLBAR_POSITION_TOP;
static int                g_margin_top    = 0;
static int                g_margin_right  = 0;
static int                g_margin_bottom = 0;
static int                g_margin_left   = 0;

/* ---- Forward decls ---- */

static int   bar_output_open (struct bar_output *bo);
static void  bar_output_close(struct bar_output *bo);
static int   shm_alloc       (struct bar_output *bo, int width, int height);
static void  shm_free        (struct bar_output *bo);

/* ---- wl_output handlers (name event) ---- */

static void output_geometry(void *_d, struct wl_output *_o,
                            int32_t _x, int32_t _y, int32_t _w, int32_t _h,
                            int32_t _sub, const char *_make, const char *_model,
                            int32_t _trans) {
    (void)_d;(void)_o;(void)_x;(void)_y;(void)_w;(void)_h;(void)_sub;
    (void)_make;(void)_model;(void)_trans;
}
static void output_mode(void *_d, struct wl_output *_o, uint32_t _f,
                        int32_t _w, int32_t _h, int32_t _r) {
    (void)_d;(void)_o;(void)_f;(void)_w;(void)_h;(void)_r;
}
static void output_done(void *_d, struct wl_output *_o) { (void)_d;(void)_o; }
static void output_scale(void *_d, struct wl_output *_o, int32_t _s) {
    (void)_d;(void)_o;(void)_s;
}
static void output_name_evt(void *data, struct wl_output *_o, const char *name) {
    (void)_o;
    struct bar_output *bo = data;
    if (bo->name) free(bo->name);
    bo->name = name ? strdup(name) : NULL;
}
static void output_description(void *_d, struct wl_output *_o, const char *_desc) {
    (void)_d;(void)_o;(void)_desc;
}

static const struct wl_output_listener output_listener = {
    .geometry    = output_geometry,
    .mode        = output_mode,
    .done        = output_done,
    .scale       = output_scale,
    .name        = output_name_evt,
    .description = output_description,
};

/* ---- Layer-surface handlers ---- */

static void layer_configure(void *data, struct zwlr_layer_surface_v1 *ls,
                            uint32_t serial, uint32_t w, uint32_t h) {
    struct bar_output *bo = data;
    zwlr_layer_surface_v1_ack_configure(ls, serial);
    if (w == 0) w = bo->width  ? bo->width  : 800;
    if (h == 0) h = g_requested_height;
    if ((int)w != bo->width || (int)h != bo->height) {
        shm_free(bo);
        bo->width  = (int)w;
        bo->height = (int)h;
        if (shm_alloc(bo, bo->width, bo->height) < 0) {
            fprintf(stderr, "wlbar: shm_alloc failed: %s\n", strerror(errno));
            bo->closed = 1;
            return;
        }
    }
    bo->configured = 1;
}

static void layer_closed(void *data, struct zwlr_layer_surface_v1 *_ls) {
    (void)_ls;
    struct bar_output *bo = data;
    bo->closed = 1;
}

static const struct zwlr_layer_surface_v1_listener layer_listener = {
    .configure = layer_configure,
    .closed    = layer_closed,
};

/* ---- Registry handlers ---- */

static struct bar_output *register_output(struct wl_output *out, uint32_t id) {
    if (g_output_count >= MAX_OUTPUTS) return NULL;
    struct bar_output *bo = &g_outputs[g_output_count++];
    memset(bo, 0, sizeof *bo);
    bo->wl_output   = out;
    bo->registry_id = id;
    bo->shm_fd      = -1;
    wl_output_add_listener(out, &output_listener, bo);
    return bo;
}

static void registry_global(void *_data, struct wl_registry *reg, uint32_t id,
                            const char *iface, uint32_t version) {
    (void)_data;
    if (!strcmp(iface, wl_compositor_interface.name)) {
        g_compositor = wl_registry_bind(reg, id, &wl_compositor_interface, 4);
    } else if (!strcmp(iface, wl_shm_interface.name)) {
        g_shm = wl_registry_bind(reg, id, &wl_shm_interface, 1);
    } else if (!strcmp(iface, wl_output_interface.name)) {
        uint32_t ver = version > 4 ? 4 : version;
        struct wl_output *out = wl_registry_bind(reg, id,
                                                  &wl_output_interface, ver);
        if (out) register_output(out, id);
    } else if (!strcmp(iface, zwlr_layer_shell_v1_interface.name)) {
        uint32_t ver = version > 4 ? 4 : version;
        g_layer_shell = wl_registry_bind(reg, id,
                                          &zwlr_layer_shell_v1_interface, ver);
    }
}

static void registry_global_remove(void *_d, struct wl_registry *_r, uint32_t id) {
    (void)_d;(void)_r;
    for (int i = 0; i < g_output_count; i++) {
        if (g_outputs[i].registry_id == id) {
            bar_output_close(&g_outputs[i]);
            /* Shift remaining entries down. */
            memmove(&g_outputs[i], &g_outputs[i + 1],
                    (g_output_count - i - 1) * sizeof(g_outputs[0]));
            g_output_count--;
            return;
        }
    }
}

static const struct wl_registry_listener registry_listener = {
    .global         = registry_global,
    .global_remove  = registry_global_remove,
};

/* ---- shm helpers ---- */

static int anon_shm_fd(size_t size) {
#ifdef SYS_memfd_create
    int fd = syscall(SYS_memfd_create, "lispbar-shm", 0);
#else
    int fd = -1;
#endif
    if (fd < 0) {
        char tmpl[] = "/tmp/lispbar-shm-XXXXXX";
        fd = mkstemp(tmpl);
        if (fd >= 0) unlink(tmpl);
    }
    if (fd < 0) return -1;
    if (ftruncate(fd, size) < 0) { close(fd); return -1; }
    return fd;
}

static int shm_alloc(struct bar_output *bo, int width, int height) {
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

    bo->shm_fd      = fd;
    bo->pixels      = (uint32_t *)map;
    bo->pixels_size = size;
    bo->buffer      = buf;
    memset(bo->pixels, 0, size);
    return 0;
}

static void shm_free(struct bar_output *bo) {
    if (bo->buffer) { wl_buffer_destroy(bo->buffer); bo->buffer = NULL; }
    if (bo->pixels) { munmap(bo->pixels, bo->pixels_size);
                      bo->pixels = NULL; bo->pixels_size = 0; }
    if (bo->shm_fd >= 0) { close(bo->shm_fd); bo->shm_fd = -1; }
}

/* ---- Per-output lifecycle ---- */

static int bar_output_open(struct bar_output *bo) {
    bo->surface = wl_compositor_create_surface(g_compositor);
    bo->layer_surface = zwlr_layer_shell_v1_get_layer_surface(
        g_layer_shell, bo->surface, bo->wl_output,
        ZWLR_LAYER_SHELL_V1_LAYER_TOP, BAR_NAMESPACE);

    uint32_t anchor =
        ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
        ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT |
        (g_requested_position == WLBAR_POSITION_BOTTOM
         ? ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM
         : ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP);
    zwlr_layer_surface_v1_set_anchor(bo->layer_surface, anchor);
    zwlr_layer_surface_v1_set_size(bo->layer_surface, 0, g_requested_height);
    zwlr_layer_surface_v1_set_margin(bo->layer_surface,
                                     g_margin_top,    g_margin_right,
                                     g_margin_bottom, g_margin_left);
    /* Reserve enough screen real-estate for the bar plus its own
     * outer margin on the anchor side. */
    {
        int outer = (g_requested_position == WLBAR_POSITION_BOTTOM
                     ? g_margin_bottom : g_margin_top);
        zwlr_layer_surface_v1_set_exclusive_zone(bo->layer_surface,
                                                  g_requested_height + outer);
    }
    zwlr_layer_surface_v1_set_keyboard_interactivity(
        bo->layer_surface, ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE);
    zwlr_layer_surface_v1_add_listener(bo->layer_surface, &layer_listener, bo);

    wl_surface_commit(bo->surface);
    return 0;
}

static void bar_output_close(struct bar_output *bo) {
    shm_free(bo);
    if (bo->layer_surface) { zwlr_layer_surface_v1_destroy(bo->layer_surface);
                             bo->layer_surface = NULL; }
    if (bo->surface)       { wl_surface_destroy(bo->surface);
                             bo->surface = NULL; }
    if (bo->wl_output)     { wl_output_destroy(bo->wl_output);
                             bo->wl_output = NULL; }
    if (bo->name)          { free(bo->name); bo->name = NULL; }
    bo->width = bo->height = 0;
    bo->configured = 0;
}

/* ---- Public API ---- */

int wlbar_init(int height, int position,
               int margin_top, int margin_right,
               int margin_bottom, int margin_left) {
    if (height <= 0) height = 28;
    g_requested_height   = height;
    g_requested_position = (position == WLBAR_POSITION_BOTTOM
                             ? WLBAR_POSITION_BOTTOM
                             : WLBAR_POSITION_TOP);
    g_margin_top    = margin_top    < 0 ? 0 : margin_top;
    g_margin_right  = margin_right  < 0 ? 0 : margin_right;
    g_margin_bottom = margin_bottom < 0 ? 0 : margin_bottom;
    g_margin_left   = margin_left   < 0 ? 0 : margin_left;
    g_output_count  = 0;

    g_display = wl_display_connect(NULL);
    if (!g_display) {
        fprintf(stderr, "wlbar: cannot connect to Wayland display\n");
        return -1;
    }
    g_registry = wl_display_get_registry(g_display);
    wl_registry_add_listener(g_registry, &registry_listener, NULL);
    /* First round-trip: receive globals. */
    wl_display_roundtrip(g_display);
    /* Second round-trip: each wl_output sends its 'name' event. */
    wl_display_roundtrip(g_display);

    if (!g_compositor || !g_shm || !g_layer_shell) {
        fprintf(stderr,
                "wlbar: missing globals (compositor=%p shm=%p layer-shell=%p);"
                " compositor probably lacks wlr-layer-shell\n",
                (void*)g_compositor, (void*)g_shm, (void*)g_layer_shell);
        wlbar_shutdown();
        return -1;
    }
    if (g_output_count == 0) {
        fprintf(stderr, "wlbar: no wl_outputs advertised by compositor\n");
        wlbar_shutdown();
        return -1;
    }

    for (int i = 0; i < g_output_count; i++) {
        if (bar_output_open(&g_outputs[i]) < 0) {
            fprintf(stderr, "wlbar: bar_output_open failed for output %d\n", i);
        }
    }
    /* Wait for configure events on every surface. */
    wl_display_roundtrip(g_display);
    wl_display_roundtrip(g_display);

    int configured = 0;
    for (int i = 0; i < g_output_count; i++)
        if (g_outputs[i].configured) configured++;

    if (!configured) {
        fprintf(stderr, "wlbar: no surface received configure event\n");
        wlbar_shutdown();
        return -1;
    }
    return g_output_count;
}

void wlbar_shutdown(void) {
    for (int i = 0; i < g_output_count; i++) bar_output_close(&g_outputs[i]);
    g_output_count = 0;
    if (g_layer_shell)   { zwlr_layer_shell_v1_destroy(g_layer_shell);     g_layer_shell = NULL; }
    if (g_shm)           { wl_shm_destroy(g_shm);                          g_shm = NULL; }
    if (g_compositor)    { wl_compositor_destroy(g_compositor);            g_compositor = NULL; }
    if (g_registry)      { wl_registry_destroy(g_registry);                g_registry = NULL; }
    if (g_display)       { wl_display_disconnect(g_display);               g_display = NULL; }
}

int       wlbar_output_count (void) { return g_output_count; }

int       wlbar_output_width   (int i) {
    return (i >= 0 && i < g_output_count) ? g_outputs[i].width  : 0;
}
int       wlbar_output_height  (int i) {
    return (i >= 0 && i < g_output_count) ? g_outputs[i].height : 0;
}
uint32_t *wlbar_output_pixels  (int i) {
    return (i >= 0 && i < g_output_count) ? g_outputs[i].pixels : NULL;
}
int       wlbar_output_stride  (int i) {
    return (i >= 0 && i < g_output_count) ? g_outputs[i].width * 4 : 0;
}
const char *wlbar_output_name  (int i) {
    return (i >= 0 && i < g_output_count) ? g_outputs[i].name : NULL;
}

void wlbar_output_commit(int i) {
    if (i < 0 || i >= g_output_count) return;
    struct bar_output *bo = &g_outputs[i];
    if (!bo->buffer || !bo->surface) return;
    wl_surface_attach(bo->surface, bo->buffer, 0, 0);
    wl_surface_damage_buffer(bo->surface, 0, 0, bo->width, bo->height);
    wl_surface_commit(bo->surface);
    wl_display_flush(g_display);
}

/* Legacy single-output aliases. */
int       wlbar_width  (void) { return wlbar_output_width (0); }
int       wlbar_height (void) { return wlbar_output_height(0); }
uint32_t *wlbar_pixels (void) { return wlbar_output_pixels(0); }
int       wlbar_stride (void) { return wlbar_output_stride(0); }
void      wlbar_commit (void) { wlbar_output_commit(0); }

int wlbar_fd(void) { return g_display ? wl_display_get_fd(g_display) : -1; }

int wlbar_closed(void) {
    if (g_output_count == 0) return 1;
    for (int i = 0; i < g_output_count; i++)
        if (!g_outputs[i].closed) return 0;
    return 1;
}

int wlbar_poll(int timeout_ms) {
    if (!g_display) return -1;
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
