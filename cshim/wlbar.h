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
 * _BOTTOM) with HEIGHT pixels of exclusive zone.  Returns the
 * number of surfaces created (>= 1) on success or -1 on failure. */
int   wlbar_init(int height, int position);

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

#ifdef __cplusplus
}
#endif

#endif /* LISPBAR_WLBAR_H */
