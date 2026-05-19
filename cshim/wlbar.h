/* wlbar.h - C shim that hides Wayland protocol details from SBCL.
 *
 * Multi-output: this shim creates one layer-shell surface per
 * wl_output the compositor exposes (so the bar appears on every
 * monitor) and lets the Lisp side address each by index.  The
 * legacy single-output accessors are aliases for index 0.
 */

#ifndef LISPBAR_WLBAR_H
#define LISPBAR_WLBAR_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Lifecycle ------------------------------------------------------- */

/* Bar placement, passed to wlbar_init(). */
#define WLBAR_POSITION_TOP    0
#define WLBAR_POSITION_BOTTOM 1

/* Connect to the compositor and create one layer-shell surface per
 * connected wl_output, anchored to POSITION (WLBAR_POSITION_TOP or
 * _BOTTOM) with HEIGHT pixels of exclusive zone.  The four margin
 * arguments are in pixels (top, right, bottom, left) and gap the bar
 * away from its anchor edges (use 0 for a flush bar).  Returns the
 * number of surfaces created (>= 1) on success or -1 on failure. */
int   wlbar_init(int height, int position,
                 int margin_top, int margin_right,
                 int margin_bottom, int margin_left);

/* Release every Wayland object and disconnect.  Safe to call
 * repeatedly. */
void  wlbar_shutdown(void);

/* Per-output API -------------------------------------------------- */

/* How many surfaces are currently mapped.  Stable between init and
 * shutdown except when the compositor adds/removes outputs at run
 * time (in which case wlbar_poll() refreshes the count). */
int   wlbar_output_count(void);

int       wlbar_output_width   (int i);
int       wlbar_output_height  (int i);
uint32_t *wlbar_output_pixels  (int i);
int       wlbar_output_stride  (int i);
void      wlbar_output_commit  (int i);

/* Returns the wl_output name string (e.g. "DP-1"), or NULL if the
 * compositor never told us. */
const char *wlbar_output_name  (int i);

/* Legacy single-output aliases for wlbar_output_*(0). */
int       wlbar_width  (void);
int       wlbar_height (void);
uint32_t *wlbar_pixels (void);
int       wlbar_stride (void);
void      wlbar_commit (void);

/* Event loop ------------------------------------------------------ */

/* Dispatch any pending Wayland events.  Blocks up to TIMEOUT_MS
 * milliseconds (-1 = forever).  Returns 1 if events were processed,
 * 0 on timeout, -1 on display error. */
int   wlbar_poll  (int timeout_ms);

/* Compositor file descriptor (for poll/select multiplexing). */
int   wlbar_fd    (void);

/* Returns 1 once every layer surface has been closed. */
int   wlbar_closed(void);

/* Pointer input -------------------------------------------------- */

/* A queued pointer event.  Drained one-at-a-time by Lisp via
 * wlbar_poll_pointer_event(); see linux/input-event-codes.h for
 * button values (BTN_LEFT == 272, BTN_RIGHT == 273, BTN_MIDDLE == 274). */
struct wlbar_pointer_event {
    int    output_idx;   /* surface index the pointer was over */
    double x, y;         /* last-known surface-local coordinates */
    int    button;
    int    pressed;      /* 1 = press, 0 = release */
};

/* Dequeue one pointer event into *OUT.  Returns 1 on success, 0 if
 * the queue is empty.  Press events are queued; release events are
 * dropped (consumers care about clicks, not motion). */
int   wlbar_poll_pointer_event(struct wlbar_pointer_event *out);

/* Current hover state.  Returns 1 and fills OUTPUT_IDX + X + Y when
 * the pointer is currently inside one of our surfaces.  Returns 0
 * when the pointer is elsewhere or no pointer exists.  Useful for
 * driving tooltip overlays. */
int   wlbar_pointer_hover(int *output_idx, double *x, double *y);

/* Tooltip surfaces ------------------------------------------------ */
/*
 * Each output gets a secondary layer-shell surface used for tooltip
 * overlays.  The tooltip surface is created lazily on first show()
 * and reused thereafter (cheaper than create/destroy on every
 * hover).  Surfaces are anchored to the same edge the bar uses
 * (TOP or BOTTOM, configured by wlbar_init) and positioned by
 * margin.
 *
 * Lifecycle:
 *   wlbar_tooltip_show()    create/resize/move and become visible
 *   wlbar_tooltip_pixels()  pointer to ARGB32 buffer to paint
 *   wlbar_tooltip_commit()  push the painted frame
 *   wlbar_tooltip_hide()    move offscreen + zero buffer
 */

/* Configure tooltip surface for OUTPUT to WIDTH x HEIGHT at
 * horizontal position ANCHOR_X (pixels from the output's left edge).
 * The vertical position is automatic: just below the bar when the
 * bar is on top, just above when it's on bottom.  Returns 1 if the
 * surface is ready to paint, 0 on failure or if the configure
 * roundtrip hasn't completed yet (call again on the next tick). */
int       wlbar_tooltip_show   (int output, int anchor_x, int width, int height);

uint32_t *wlbar_tooltip_pixels (int output);
int       wlbar_tooltip_stride (int output);
int       wlbar_tooltip_width  (int output);
int       wlbar_tooltip_height (int output);
void      wlbar_tooltip_commit (int output);
void      wlbar_tooltip_hide   (int output);

#ifdef __cplusplus
}
#endif

#endif /* LISPBAR_WLBAR_H */
