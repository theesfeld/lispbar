/* wltray.c - StatusNotifierItem host for Lispbar.
 *
 * Owns org.kde.StatusNotifierWatcher on the session bus, tracks
 * registered items, and exposes a flat snapshot API to Lisp.  We
 * intentionally implement only what a bar needs:
 *
 *   * Watcher service: RegisterStatusNotifierItem / Unregister /
 *     RegisteredStatusNotifierItems property / signals
 *   * Per-item: Id, Title, Status, IconName, IconPixmap, ToolTip
 *   * Per-item: Activate / SecondaryActivate / ContextMenu calls
 *
 * We rely on Lisp polling wltray_poll() between bar render ticks;
 * tracked metadata is refreshed each poll via async-y GetAll calls
 * that are dispatched on the same connection.
 */

#include <ctype.h>
#include <dbus/dbus.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "wltray.h"

#define WATCHER_BUS  "org.kde.StatusNotifierWatcher"
#define WATCHER_PATH "/StatusNotifierWatcher"
#define WATCHER_IFACE "org.kde.StatusNotifierWatcher"
#define ITEM_IFACE   "org.kde.StatusNotifierItem"

#define MAX_ITEMS 32

struct item {
    char     *bus;        /* "org.kde.StatusNotifierItem-PID-N" or ":1.X" */
    char     *path;       /* usually "/StatusNotifierItem" */
    char     *id;
    char     *title;
    char     *status;
    char     *icon_name;
    char     *tooltip;
    int       has_pixmap;
    int       pixmap_w;
    int       pixmap_h;
    uint32_t *pixmap;     /* ARGB32, host byte order */
    /* Resolved absolute path to a PNG/SVG on disk for icon_name,
     * found via freedesktop icon-theme search.  NULL when no match
     * or when has_pixmap supersedes it. */
    char     *icon_path;
};

static DBusConnection *g_conn          = NULL;
static int             g_own_watcher   = 0;
static struct item     g_items[MAX_ITEMS];
static int             g_count         = 0;
static unsigned        g_revision      = 0;

/* ---- utility ---- */

static char *xstrdup(const char *s) {
    if (!s) return NULL;
    char *r = strdup(s);
    return r;
}
static void xfree(void *p) { if (p) free(p); }

static int find_item(const char *bus) {
    for (int i = 0; i < g_count; i++)
        if (g_items[i].bus && !strcmp(g_items[i].bus, bus)) return i;
    return -1;
}

static void item_clear(struct item *it) {
    xfree(it->bus);       it->bus = NULL;
    xfree(it->path);      it->path = NULL;
    xfree(it->id);        it->id = NULL;
    xfree(it->title);     it->title = NULL;
    xfree(it->status);    it->status = NULL;
    xfree(it->icon_name); it->icon_name = NULL;
    xfree(it->tooltip);   it->tooltip = NULL;
    xfree(it->pixmap);    it->pixmap = NULL;
    xfree(it->icon_path); it->icon_path = NULL;
    it->has_pixmap = it->pixmap_w = it->pixmap_h = 0;
}

static void item_remove(int idx) {
    if (idx < 0 || idx >= g_count) return;
    item_clear(&g_items[idx]);
    memmove(&g_items[idx], &g_items[idx + 1],
            (g_count - idx - 1) * sizeof(struct item));
    g_count--;
    g_revision++;
}

/* Split SNI's "RegisterStatusNotifierItem" argument.  Some callers
 * pass a bus name, others a path on the caller's bus.  Normalise to
 * (bus, path). */
static void normalise_target(const char *sender, const char *arg,
                              char **out_bus, char **out_path) {
    if (arg && arg[0] == '/') {
        *out_bus  = xstrdup(sender);
        *out_path = xstrdup(arg);
    } else if (arg && *arg) {
        *out_bus  = xstrdup(arg);
        *out_path = xstrdup("/StatusNotifierItem");
    } else {
        *out_bus  = xstrdup(sender);
        *out_path = xstrdup("/StatusNotifierItem");
    }
}

/* ---- D-Bus marshaling helpers ---- */

static int variant_get_string(DBusMessageIter *iter, char **out) {
    DBusMessageIter sub;
    if (dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_VARIANT) return 0;
    dbus_message_iter_recurse(iter, &sub);
    int t = dbus_message_iter_get_arg_type(&sub);
    if (t == DBUS_TYPE_STRING || t == DBUS_TYPE_OBJECT_PATH) {
        const char *s = NULL;
        dbus_message_iter_get_basic(&sub, &s);
        *out = xstrdup(s);
        return 1;
    }
    return 0;
}

/* IconPixmap is a(iiay) - array of (width, height, bytes).  We take
 * the largest pixmap.  Bytes are ARGB32 in network byte order; we
 * byteswap into host order. */
static void variant_get_iconpixmap(DBusMessageIter *iter, struct item *it) {
    DBusMessageIter v_iter;
    if (dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_VARIANT) return;
    dbus_message_iter_recurse(iter, &v_iter);
    if (dbus_message_iter_get_arg_type(&v_iter) != DBUS_TYPE_ARRAY) return;
    DBusMessageIter arr;
    dbus_message_iter_recurse(&v_iter, &arr);

    int best_w = 0, best_h = 0;
    uint32_t *best_px = NULL;

    while (dbus_message_iter_get_arg_type(&arr) == DBUS_TYPE_STRUCT) {
        DBusMessageIter st;
        dbus_message_iter_recurse(&arr, &st);
        int w = 0, h = 0;
        if (dbus_message_iter_get_arg_type(&st) == DBUS_TYPE_INT32)
            dbus_message_iter_get_basic(&st, &w);
        dbus_message_iter_next(&st);
        if (dbus_message_iter_get_arg_type(&st) == DBUS_TYPE_INT32)
            dbus_message_iter_get_basic(&st, &h);
        dbus_message_iter_next(&st);
        if (dbus_message_iter_get_arg_type(&st) == DBUS_TYPE_ARRAY) {
            DBusMessageIter ay;
            dbus_message_iter_recurse(&st, &ay);
            const uint8_t *bytes = NULL;
            int nbytes = 0;
            dbus_message_iter_get_fixed_array(&ay, &bytes, &nbytes);
            if (w > 0 && h > 0 && nbytes >= w * h * 4 &&
                w * h > best_w * best_h) {
                if (best_px) free(best_px);
                best_w  = w;
                best_h  = h;
                best_px = (uint32_t *)malloc(w * h * 4);
                if (best_px) {
                    /* Byteswap ARGB-net -> ARGB-host (little endian on x86). */
                    for (int i = 0; i < w * h; i++) {
                        uint32_t b0 = bytes[i * 4 + 0];
                        uint32_t b1 = bytes[i * 4 + 1];
                        uint32_t b2 = bytes[i * 4 + 2];
                        uint32_t b3 = bytes[i * 4 + 3];
                        best_px[i] = (b0 << 24) | (b1 << 16)
                                   | (b2 << 8)  | b3;
                    }
                }
            }
        }
        dbus_message_iter_next(&arr);
    }

    if (best_px) {
        xfree(it->pixmap);
        it->pixmap     = best_px;
        it->pixmap_w   = best_w;
        it->pixmap_h   = best_h;
        it->has_pixmap = 1;
    }
}

/* ToolTip is (s, a(iiay), s, s) - icon-name, icon-pixmaps, title, desc.
 * We only care about the title. */
static void variant_get_tooltip(DBusMessageIter *iter, struct item *it) {
    DBusMessageIter v_iter;
    if (dbus_message_iter_get_arg_type(iter) != DBUS_TYPE_VARIANT) return;
    dbus_message_iter_recurse(iter, &v_iter);
    if (dbus_message_iter_get_arg_type(&v_iter) != DBUS_TYPE_STRUCT) return;
    DBusMessageIter st;
    dbus_message_iter_recurse(&v_iter, &st);
    /* skip icon-name */
    dbus_message_iter_next(&st);
    /* skip icon-pixmaps */
    dbus_message_iter_next(&st);
    /* title */
    if (dbus_message_iter_get_arg_type(&st) == DBUS_TYPE_STRING) {
        const char *s = NULL;
        dbus_message_iter_get_basic(&st, &s);
        xfree(it->tooltip);
        it->tooltip = (s && *s) ? xstrdup(s) : NULL;
    }
}

/* ---- freedesktop icon-theme lookup ---- */
/*
 * Minimal but practical implementation: walk a fixed list of
 * common search roots and look for `NAME.png' or `NAME.svg' under
 * any size directory.  Larger sizes win, then `scalable' (SVG),
 * then `hicolor' as a last resort.  This covers ~99% of real-
 * world apps without depending on libxdg-icon-cache or shipping
 * a full icon-theme parser.
 */

static const char *icon_search_roots[] = {
    NULL,                         /* placeholder: $HOME/.icons */
    NULL,                         /* placeholder: $XDG_DATA_HOME/icons */
    "/usr/local/share/icons",
    "/usr/share/icons",
    "/usr/share/pixmaps",
    NULL,
};

static char *make_path(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    char *r = malloc(la + 1 + lb + 1);
    if (!r) return NULL;
    memcpy(r, a, la);
    r[la] = '/';
    memcpy(r + la + 1, b, lb);
    r[la + 1 + lb] = 0;
    return r;
}

static char *file_if_exists(const char *path) {
    if (access(path, R_OK) == 0) return strdup(path);
    return NULL;
}

static char *svg_to_cached_png(const char *svg_path, int size_px);

/* Try `root/theme/SIZE/CATEGORY/name.EXT' for a few sensible sizes
 * and extensions, returning a heap-allocated path or NULL. */
/* Test theme_dir/A/B/NAME.{png,svg}, returning a heap path or NULL.
 * `a' and `b' may be size-then-cat or cat-then-size depending on
 * the theme; the spec allows both layouts. */
static char *try_theme_layout(const char *theme_dir,
                                const char *a, const char *b,
                                const char *name) {
    static const char *exts[] = { ".png", ".svg", NULL };
    char *ab = make_path(theme_dir, a);
    if (!ab) return NULL;
    char *abc = make_path(ab, b);
    free(ab);
    if (!abc) return NULL;
    char *result = NULL;
    for (int e = 0; exts[e]; e++) {
        size_t len = strlen(abc) + 1 + strlen(name) + strlen(exts[e]) + 1;
        char *full = malloc(len);
        if (!full) continue;
        snprintf(full, len, "%s/%s%s", abc, name, exts[e]);
        char *hit = file_if_exists(full);
        free(full);
        if (hit) {
            size_t hl = strlen(hit);
            if (hl > 4 && !strcmp(hit + hl - 4, ".svg")) {
                char *png = svg_to_cached_png(hit, 32);
                free(hit);
                if (png) { result = png; goto out; }
            } else {
                result = hit; goto out;
            }
        }
    }
out:
    free(abc);
    return result;
}

static char *try_theme(const char *root, const char *theme, const char *name) {
    /* Largest sizes first - they tend to scale down nicely. */
    static const char *sizes[] = {
        "48", "48x48", "32", "32x32", "24", "24x24",
        "22", "22x22", "16", "16x16", "64", "64x64",
        "128", "128x128", "scalable", "symbolic", NULL
    };
    static const char *cats[] = {
        "apps", "status", "actions", "devices", "categories",
        "places", "mimetypes", "panel", NULL
    };
    char *theme_dir = make_path(root, theme);
    if (!theme_dir) return NULL;
    if (access(theme_dir, R_OK) != 0) { free(theme_dir); return NULL; }

    /* size/category/name (Adwaita / hicolor convention) */
    for (int s = 0; sizes[s]; s++)
        for (int c = 0; cats[c]; c++) {
            char *r = try_theme_layout(theme_dir, sizes[s], cats[c], name);
            if (r) { free(theme_dir); return r; }
        }
    /* category/size/name (Humanity / some KDE themes) */
    for (int c = 0; cats[c]; c++)
        for (int s = 0; sizes[s]; s++) {
            char *r = try_theme_layout(theme_dir, cats[c], sizes[s], name);
            if (r) { free(theme_dir); return r; }
        }
    free(theme_dir);
    return NULL;
}

/* Convert an SVG to a PNG via the system's `rsvg-convert' if available,
 * caching the output under $XDG_CACHE_HOME/lispbar/icons/.  Returns
 * the PNG path on success, NULL on failure.  The caller owns the
 * returned string. */
static char *svg_to_cached_png(const char *svg_path, int size_px) {
    if (!svg_path) return NULL;
    /* Look up rsvg-convert just once per process. */
    static int rsvg_checked = 0;
    static int rsvg_ok      = 0;
    if (!rsvg_checked) {
        rsvg_checked = 1;
        rsvg_ok = (system("command -v rsvg-convert >/dev/null 2>&1") == 0);
    }
    if (!rsvg_ok) return NULL;

    /* Build a cache key from the source path + size; SHA-ish via
     * a simple FNV1a hash is enough for de-duplication.  We assume
     * paths don't change after first encounter; if a file does
     * change, the cache becomes stale until removed manually. */
    uint64_t h = 1469598103934665603ULL;
    for (const char *p = svg_path; *p; p++) {
        h ^= (uint8_t)*p; h *= 1099511628211ULL;
    }
    h ^= (uint64_t)size_px; h *= 1099511628211ULL;

    char *cache_root = NULL;
    const char *xdg = getenv("XDG_CACHE_HOME");
    const char *home = getenv("HOME");
    if (xdg && *xdg) {
        size_t n = strlen(xdg) + strlen("/lispbar/icons") + 1;
        cache_root = malloc(n);
        snprintf(cache_root, n, "%s/lispbar/icons", xdg);
    } else if (home) {
        size_t n = strlen(home) + strlen("/.cache/lispbar/icons") + 1;
        cache_root = malloc(n);
        snprintf(cache_root, n, "%s/.cache/lispbar/icons", home);
    } else {
        return NULL;
    }
    /* mkdir -p */
    {
        char mkbuf[1024];
        snprintf(mkbuf, sizeof mkbuf, "mkdir -p '%s' 2>/dev/null", cache_root);
        /* If the dir already exists or `mkdir' isn't on PATH, the
         * subsequent file write will simply fail and we fall back to
         * text. */
        (void)!system(mkbuf);
    }

    size_t out_n = strlen(cache_root) + 64;
    char *out = malloc(out_n);
    snprintf(out, out_n, "%s/%016lx-%d.png", cache_root,
             (unsigned long)h, size_px);
    free(cache_root);

    if (access(out, R_OK) == 0) return out;        /* hit */

    /* Convert.  rsvg-convert returns non-zero on failure, in which
     * case we don't have a PNG at OUT and return NULL. */
    char cmd[2048];
    snprintf(cmd, sizeof cmd,
             "rsvg-convert -w %d -h %d -o '%s' '%s' >/dev/null 2>&1",
             size_px, size_px, out, svg_path);
    if (system(cmd) != 0 || access(out, R_OK) != 0) {
        free(out);
        return NULL;
    }
    return out;
}

static char *resolve_icon_path(const char *name) {
    if (!name || !*name)         return NULL;
    /* Absolute path? */
    if (name[0] == '/' && access(name, R_OK) == 0) {
        size_t len = strlen(name);
        if (len > 4 && !strcmp(name + len - 4, ".svg")) {
            char *png = svg_to_cached_png(name, 32);
            if (png) return png;
        }
        return strdup(name);
    }

    /* Populate placeholders for $HOME / $XDG_DATA_HOME. */
    char home_icons[1024]   = {0};
    char xdg_data[1024]     = {0};
    const char *home = getenv("HOME");
    if (home) snprintf(home_icons, sizeof home_icons, "%s/.icons", home);
    const char *xdg = getenv("XDG_DATA_HOME");
    if (xdg && *xdg) snprintf(xdg_data, sizeof xdg_data, "%s/icons", xdg);
    else if (home)   snprintf(xdg_data, sizeof xdg_data, "%s/.local/share/icons", home);
    icon_search_roots[0] = home_icons;
    icon_search_roots[1] = xdg_data;

    /* Walk every search root and try every theme directory we
     * find.  This is more thorough than a hard-coded theme list
     * and works for whatever icon set the user has installed. */
    for (int r = 0; icon_search_roots[r]; r++) {
        DIR *dir = opendir(icon_search_roots[r]);
        if (!dir) continue;
        struct dirent *de;
        while ((de = readdir(dir)) != NULL) {
            if (de->d_name[0] == '.') continue;
            /* Skip if no theme index file - probably not a theme. */
            char *theme_dir = make_path(icon_search_roots[r], de->d_name);
            if (!theme_dir) continue;
            char idx_path[2048];
            snprintf(idx_path, sizeof idx_path, "%s/index.theme", theme_dir);
            int has_index = (access(idx_path, R_OK) == 0);
            free(theme_dir);
            if (!has_index) continue;
            char *p = try_theme(icon_search_roots[r], de->d_name, name);
            if (p) { closedir(dir); return p; }
            /* For symbolic-only themes (Adwaita's symbolic/categories
             * subtree), the file is NAME-symbolic.{svg,png}. */
            if (!strstr(name, "-symbolic")) {
                size_t sn = strlen(name) + strlen("-symbolic") + 1;
                char *sym = malloc(sn);
                snprintf(sym, sn, "%s-symbolic", name);
                p = try_theme(icon_search_roots[r], de->d_name, sym);
                free(sym);
                if (p) { closedir(dir); return p; }
            }
        }
        closedir(dir);
    }
    /* Also probe /usr/share/pixmaps/NAME.{png,svg} directly. */
    for (const char *ext = ".png"; ext; ext = (ext[1] == 'p' ? ".svg" : NULL)) {
        size_t len = strlen("/usr/share/pixmaps/") + strlen(name) + strlen(ext) + 1;
        char *full = malloc(len);
        if (!full) continue;
        snprintf(full, len, "/usr/share/pixmaps/%s%s", name, ext);
        char *hit = file_if_exists(full);
        free(full);
        if (hit) {
            size_t hl = strlen(hit);
            if (hl > 4 && !strcmp(hit + hl - 4, ".svg")) {
                char *png = svg_to_cached_png(hit, 32);
                free(hit);
                if (png) return png;
            } else {
                return hit;
            }
        }
    }
    return NULL;
}

/* Fetch (synchronously) every property we care about for ITEM. */
static void item_refresh(struct item *it) {
    if (!it->bus || !it->path) return;
    DBusMessage *msg = dbus_message_new_method_call(
        it->bus, it->path,
        "org.freedesktop.DBus.Properties", "GetAll");
    if (!msg) return;
    const char *iface = ITEM_IFACE;
    dbus_message_append_args(msg, DBUS_TYPE_STRING, &iface, DBUS_TYPE_INVALID);

    DBusError err;
    dbus_error_init(&err);
    DBusMessage *reply = dbus_connection_send_with_reply_and_block(
        g_conn, msg, 500, &err);
    dbus_message_unref(msg);
    if (!reply) {
        if (dbus_error_is_set(&err)) dbus_error_free(&err);
        return;
    }
    DBusMessageIter top;
    if (!dbus_message_iter_init(reply, &top) ||
        dbus_message_iter_get_arg_type(&top) != DBUS_TYPE_ARRAY) {
        dbus_message_unref(reply);
        return;
    }
    DBusMessageIter dict;
    dbus_message_iter_recurse(&top, &dict);
    while (dbus_message_iter_get_arg_type(&dict) == DBUS_TYPE_DICT_ENTRY) {
        DBusMessageIter entry;
        dbus_message_iter_recurse(&dict, &entry);
        const char *key = NULL;
        if (dbus_message_iter_get_arg_type(&entry) == DBUS_TYPE_STRING)
            dbus_message_iter_get_basic(&entry, &key);
        dbus_message_iter_next(&entry);
        if (key) {
            if      (!strcmp(key, "Id"))         {
                xfree(it->id);        it->id = NULL;
                variant_get_string(&entry, &it->id);
            } else if (!strcmp(key, "Title"))    {
                xfree(it->title);     it->title = NULL;
                variant_get_string(&entry, &it->title);
            } else if (!strcmp(key, "Status"))   {
                xfree(it->status);    it->status = NULL;
                variant_get_string(&entry, &it->status);
            } else if (!strcmp(key, "IconName")) {
                xfree(it->icon_name); it->icon_name = NULL;
                variant_get_string(&entry, &it->icon_name);
            } else if (!strcmp(key, "IconPixmap")) {
                variant_get_iconpixmap(&entry, it);
            } else if (!strcmp(key, "ToolTip"))  {
                variant_get_tooltip(&entry, it);
            }
        }
        dbus_message_iter_next(&dict);
    }
    dbus_message_unref(reply);

    /* If the item didn't ship an inline pixmap but did publish an
     * IconName, resolve it against the icon-theme path so Lisp can
     * paint a real icon instead of falling back to text. */
    if (it->icon_name && !it->has_pixmap) {
        if (it->icon_path) { free(it->icon_path); it->icon_path = NULL; }
        it->icon_path = resolve_icon_path(it->icon_name);
    } else if (it->has_pixmap && it->icon_path) {
        /* Pixmap supersedes icon-theme; clear the cached path. */
        free(it->icon_path);
        it->icon_path = NULL;
    }
    g_revision++;
}

static void item_register(const char *bus, const char *path) {
    if (find_item(bus) >= 0) return;
    if (g_count >= MAX_ITEMS) return;
    struct item *it = &g_items[g_count++];
    memset(it, 0, sizeof *it);
    it->bus  = xstrdup(bus);
    it->path = xstrdup(path);
    item_refresh(it);
    g_revision++;
}

/* ---- Watcher service: method dispatch ---- */

static DBusHandlerResult watcher_filter(DBusConnection *conn,
                                        DBusMessage *msg, void *data) {
    (void)data;
    /* React to NameOwnerChanged so we can drop items whose owner
     * disappeared. */
    if (dbus_message_is_signal(msg,
                                "org.freedesktop.DBus", "NameOwnerChanged")) {
        const char *name = NULL, *old = NULL, *new = NULL;
        dbus_message_get_args(msg, NULL,
                              DBUS_TYPE_STRING, &name,
                              DBUS_TYPE_STRING, &old,
                              DBUS_TYPE_STRING, &new,
                              DBUS_TYPE_INVALID);
        if (name && (!new || !*new)) {
            int idx = find_item(name);
            if (idx >= 0) item_remove(idx);
        }
        return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }

    /* SNI per-item update signals.  D-Bus delivers these from the
     * sender's unique name (e.g. :1.42), not its well-known bus
     * name (e.g. org.kde.StatusNotifierItem-PID-1) which is what
     * we stored at registration time.  Mapping unique->well-known
     * would require tracking NameOwnerChanged for every well-known
     * name we care about; cheaper to just refresh every tracked
     * item on any item-signal.  There are rarely more than a few. */
    if (dbus_message_is_signal(msg, ITEM_IFACE, "NewTitle") ||
        dbus_message_is_signal(msg, ITEM_IFACE, "NewIcon")  ||
        dbus_message_is_signal(msg, ITEM_IFACE, "NewAttentionIcon") ||
        dbus_message_is_signal(msg, ITEM_IFACE, "NewOverlayIcon")   ||
        dbus_message_is_signal(msg, ITEM_IFACE, "NewToolTip") ||
        dbus_message_is_signal(msg, ITEM_IFACE, "NewStatus")) {
        for (int i = 0; i < g_count; i++) item_refresh(&g_items[i]);
        return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    }
    /* Watcher interface methods. */
    if (dbus_message_is_method_call(msg, WATCHER_IFACE,
                                     "RegisterStatusNotifierItem")) {
        const char *arg = NULL;
        dbus_message_get_args(msg, NULL,
                              DBUS_TYPE_STRING, &arg, DBUS_TYPE_INVALID);
        char *bus = NULL, *path = NULL;
        normalise_target(dbus_message_get_sender(msg), arg, &bus, &path);
        if (bus) {
            item_register(bus, path);
            xfree(bus); xfree(path);
        }
        DBusMessage *reply = dbus_message_new_method_return(msg);
        dbus_connection_send(conn, reply, NULL);
        dbus_message_unref(reply);
        return DBUS_HANDLER_RESULT_HANDLED;
    }
    if (dbus_message_is_method_call(msg, WATCHER_IFACE,
                                     "RegisterStatusNotifierHost")) {
        DBusMessage *reply = dbus_message_new_method_return(msg);
        dbus_connection_send(conn, reply, NULL);
        dbus_message_unref(reply);
        return DBUS_HANDLER_RESULT_HANDLED;
    }
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

/* Append a property value (as a variant) for the given NAME into ITER.
 * Returns 1 if the property is known, 0 otherwise. */
static int append_watcher_prop(DBusMessageIter *iter, const char *name) {
    if (!strcmp(name, "RegisteredStatusNotifierItems")) {
        DBusMessageIter var, arr;
        dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT, "as", &var);
        dbus_message_iter_open_container(&var, DBUS_TYPE_ARRAY, "s", &arr);
        for (int i = 0; i < g_count; i++)
            if (g_items[i].bus)
                dbus_message_iter_append_basic(&arr, DBUS_TYPE_STRING,
                                                &g_items[i].bus);
        dbus_message_iter_close_container(&var, &arr);
        dbus_message_iter_close_container(iter, &var);
        return 1;
    }
    if (!strcmp(name, "IsStatusNotifierHostRegistered")) {
        DBusMessageIter var;
        dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT, "b", &var);
        dbus_bool_t v = TRUE;
        dbus_message_iter_append_basic(&var, DBUS_TYPE_BOOLEAN, &v);
        dbus_message_iter_close_container(iter, &var);
        return 1;
    }
    if (!strcmp(name, "ProtocolVersion")) {
        DBusMessageIter var;
        dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT, "i", &var);
        int32_t v = 0;
        dbus_message_iter_append_basic(&var, DBUS_TYPE_INT32, &v);
        dbus_message_iter_close_container(iter, &var);
        return 1;
    }
    return 0;
}

/* Filter for Properties.Get / GetAll on our Watcher object. */
static DBusHandlerResult watcher_props_filter(DBusConnection *conn,
                                              DBusMessage *msg, void *data) {
    (void)data;
    const char *path = dbus_message_get_path(msg);
    if (!path || strcmp(path, WATCHER_PATH) != 0)
        return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
    if (dbus_message_is_method_call(msg,
                                     "org.freedesktop.DBus.Properties",
                                     "Get")) {
        const char *iface = NULL, *name = NULL;
        DBusError err; dbus_error_init(&err);
        if (!dbus_message_get_args(msg, &err,
                                    DBUS_TYPE_STRING, &iface,
                                    DBUS_TYPE_STRING, &name,
                                    DBUS_TYPE_INVALID)) {
            if (dbus_error_is_set(&err)) dbus_error_free(&err);
            return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
        }
        DBusMessage *reply = dbus_message_new_method_return(msg);
        DBusMessageIter it;
        dbus_message_iter_init_append(reply, &it);
        if (append_watcher_prop(&it, name)) {
            dbus_connection_send(conn, reply, NULL);
        } else {
            dbus_message_unref(reply);
            reply = dbus_message_new_error_printf(msg,
                "org.freedesktop.DBus.Error.UnknownProperty",
                "No such property %s on %s", name, WATCHER_IFACE);
            dbus_connection_send(conn, reply, NULL);
        }
        dbus_message_unref(reply);
        return DBUS_HANDLER_RESULT_HANDLED;
    }
    if (dbus_message_is_method_call(msg,
                                     "org.freedesktop.DBus.Properties",
                                     "GetAll")) {
        DBusMessage *reply = dbus_message_new_method_return(msg);
        DBusMessageIter it, dict;
        dbus_message_iter_init_append(reply, &it);
        dbus_message_iter_open_container(&it, DBUS_TYPE_ARRAY, "{sv}", &dict);
        static const char *names[] = {
            "RegisteredStatusNotifierItems",
            "IsStatusNotifierHostRegistered",
            "ProtocolVersion", NULL
        };
        for (int i = 0; names[i]; i++) {
            DBusMessageIter entry;
            dbus_message_iter_open_container(&dict, DBUS_TYPE_DICT_ENTRY,
                                              NULL, &entry);
            dbus_message_iter_append_basic(&entry, DBUS_TYPE_STRING, &names[i]);
            append_watcher_prop(&entry, names[i]);
            dbus_message_iter_close_container(&dict, &entry);
        }
        dbus_message_iter_close_container(&it, &dict);
        dbus_connection_send(conn, reply, NULL);
        dbus_message_unref(reply);
        return DBUS_HANDLER_RESULT_HANDLED;
    }
    return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

/* ---- Public API ---- */

int wltray_init(void) {
    DBusError err;
    dbus_error_init(&err);
    g_conn = dbus_bus_get_private(DBUS_BUS_SESSION, &err);
    if (!g_conn) {
        if (dbus_error_is_set(&err)) {
            fprintf(stderr, "wltray: cannot connect to session bus: %s\n",
                    err.message);
            dbus_error_free(&err);
        }
        return -1;
    }
    dbus_connection_set_exit_on_disconnect(g_conn, FALSE);

    /* Try to own the watcher service name.  Failure is non-fatal -
     * another watcher may already exist (waybar, plasma); items
     * registered with it won't reach us, but the bar shouldn't
     * crash. */
    int r = dbus_bus_request_name(g_conn, WATCHER_BUS,
                                  DBUS_NAME_FLAG_REPLACE_EXISTING |
                                  DBUS_NAME_FLAG_DO_NOT_QUEUE,
                                  &err);
    if (r == DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER ||
        r == DBUS_REQUEST_NAME_REPLY_ALREADY_OWNER) {
        g_own_watcher = 1;
    } else {
        fprintf(stderr, "wltray: another StatusNotifierWatcher is "
                        "running; lispbar tray will be empty.\n");
        if (dbus_error_is_set(&err)) dbus_error_free(&err);
    }

    /* Add our filters; we always want NameOwnerChanged for the items
     * we end up tracking. */
    dbus_connection_add_filter(g_conn, watcher_filter, NULL, NULL);
    dbus_connection_add_filter(g_conn, watcher_props_filter, NULL, NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='org.freedesktop.DBus',"
                       "member='NameOwnerChanged'",
                       NULL);
    /* Subscribe to all item-update signals across the bus.  The
     * filter routes them to the right item by sender. */
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewTitle'", NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewIcon'", NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewAttentionIcon'", NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewOverlayIcon'", NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewToolTip'", NULL);
    dbus_bus_add_match(g_conn,
                       "type='signal',interface='" ITEM_IFACE "',"
                       "member='NewStatus'", NULL);

    return 0;
}

void wltray_shutdown(void) {
    for (int i = 0; i < g_count; i++) item_clear(&g_items[i]);
    g_count = 0;
    if (g_conn) {
        dbus_connection_close(g_conn);
        dbus_connection_unref(g_conn);
        g_conn = NULL;
    }
    g_own_watcher = 0;
}

int wltray_fd(void) {
    if (!g_conn) return -1;
    int fd = -1;
    dbus_connection_get_unix_fd(g_conn, &fd);
    return fd;
}

int wltray_poll(int timeout_ms) {
    if (!g_conn) return 0;
    int n = 0;
    /* Drain anything pending without blocking. */
    while (dbus_connection_dispatch(g_conn) ==
           DBUS_DISPATCH_DATA_REMAINS) n++;
    dbus_connection_read_write(g_conn, timeout_ms);
    while (dbus_connection_dispatch(g_conn) ==
           DBUS_DISPATCH_DATA_REMAINS) n++;
    /* Per-item metadata is refreshed on demand by the New* signal
     * handlers above; no need to poll every tick. */
    return n;
}

int wltray_item_count(void) { return g_count; }

int wltray_item_get(int i, struct wltray_item *out) {
    if (!out || i < 0 || i >= g_count) return 0;
    struct item *s = &g_items[i];
    out->id         = s->id;
    out->title      = s->title;
    out->status     = s->status;
    out->icon_name  = s->icon_name;
    out->tooltip    = s->tooltip;
    out->has_pixmap = s->has_pixmap;
    out->pixmap_w   = s->pixmap_w;
    out->pixmap_h   = s->pixmap_h;
    out->pixmap     = s->pixmap;
    out->icon_path  = s->icon_path;
    return 1;
}

unsigned wltray_revision(void) { return g_revision; }

void wltray_invoke(int i, int button, int x, int y) {
    if (i < 0 || i >= g_count || !g_conn) return;
    struct item *s = &g_items[i];
    if (!s->bus || !s->path) return;
    const char *method = NULL;
    switch (button) {
        case 0: method = "Activate";          break;
        case 1: method = "SecondaryActivate"; break;
        case 2: method = "ContextMenu";       break;
        default: return;
    }
    DBusMessage *msg = dbus_message_new_method_call(
        s->bus, s->path, ITEM_IFACE, method);
    if (!msg) return;
    int32_t ix = x, iy = y;
    dbus_message_append_args(msg,
                              DBUS_TYPE_INT32, &ix,
                              DBUS_TYPE_INT32, &iy,
                              DBUS_TYPE_INVALID);
    dbus_connection_send(g_conn, msg, NULL);
    dbus_message_unref(msg);
    dbus_connection_flush(g_conn);
}
