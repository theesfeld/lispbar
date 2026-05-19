/* wltray.h - minimal StatusNotifierItem host for Lispbar.
 *
 * Owns the org.kde.StatusNotifierWatcher name on the session bus,
 * tracks registered items, and exposes a flat snapshot API to Lisp.
 * Pure C; everything visible to Lisp is plain pointers + ints.
 *
 * Threading: single-threaded.  Calls into libdbus run on whatever
 * thread invokes wltray_poll().
 */

#ifndef LISPBAR_WLTRAY_H
#define LISPBAR_WLTRAY_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Lifecycle ------------------------------------------------------- */

/* Connect to the session bus and (try to) claim
 * org.kde.StatusNotifierWatcher.  Returns 0 on success, -1 if the
 * bus is unreachable.  Failing to acquire the Watcher name is NOT
 * fatal - we still discover items via an existing watcher if one is
 * running. */
int  wltray_init(void);
void wltray_shutdown(void);

/* The bus fd; useful if the main loop wants to multiplex.  Returns
 * -1 if init() hasn't been called yet. */
int  wltray_fd(void);

/* Pump pending D-Bus traffic; returns the number of messages
 * dispatched.  Call this between bar render ticks. */
int  wltray_poll(int timeout_ms);

/* Inventory ------------------------------------------------------- */

struct wltray_item {
    const char *id;          /* SNI Id property (stable per app) */
    const char *title;       /* Title property (human-readable) */
    const char *status;      /* "Active" | "Passive" | "NeedsAttention" */
    const char *icon_name;   /* freedesktop icon name, NULL if absent */
    const char *tooltip;     /* ToolTip title field, NULL if absent */
    int   has_pixmap;
    int   pixmap_w;
    int   pixmap_h;
    /* ARGB32 ints, host byte order.  Owned by wltray; valid until
     * the next wltray_poll() or wltray_shutdown(). */
    const uint32_t *pixmap;
};

int  wltray_item_count(void);

/* Fill OUT with item I; returns 1 on success, 0 if I is out of range. */
int  wltray_item_get(int i, struct wltray_item *out);

/* Bumped any time the item set or any item's metadata changes.  Used
 * by Lisp to short-circuit redraws / re-layout when nothing changed. */
unsigned wltray_revision(void);

/* Actions --------------------------------------------------------- */

/* button: 0 = Activate (left), 1 = SecondaryActivate (middle),
 * 2 = ContextMenu (right).  x,y are surface coordinates in screen
 * pixels (best-effort; SNI doesn't really care). */
void wltray_invoke(int i, int button, int x, int y);

#ifdef __cplusplus
}
#endif

#endif /* LISPBAR_WLTRAY_H */
