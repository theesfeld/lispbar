/* wlbar.h - C shim that hides Wayland protocol details from SBCL.
 *
 * The bar binary is a Common Lisp program; this shim takes care of
 * the bits that need codegen against the wlr-layer-shell XML, namely
 * binding global objects, creating the layer surface, allocating a
 * wl_shm buffer, and dispatching events.  SBCL FFIs in through CFFI.
 */

#ifndef LISPBAR_WLBAR_H
#define LISPBAR_WLBAR_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Lifecycle ------------------------------------------------------- */

/* Connect to the compositor and create a layer-shell surface anchored
 * to the top edge of the default output with HEIGHT pixels of
 * exclusive zone.  Returns 0 on success, -1 on failure.  Must be the
 * first call.  After a successful return, wlbar_width() gives the
 * surface width in pixels. */
int   wlbar_init(int height);

/* Release every Wayland object and disconnect.  Safe to call multiple
 * times.  After this returns, the bar is no longer mapped. */
void  wlbar_shutdown(void);

/* Width / height (pixels) of the current surface, valid only after
 * the first wlbar_init() succeeds and the first configure event has
 * been received (which happens during wlbar_init). */
int   wlbar_width(void);
int   wlbar_height(void);

/* Buffer access --------------------------------------------------- */

/* Pointer to a wlbar_width() * wlbar_height() ARGB32 pixel buffer.
 * The caller (Lisp) draws into this with cairo, then calls
 * wlbar_commit() to push the frame.  Returns NULL on failure. */
uint32_t *wlbar_pixels(void);

/* The stride in bytes between successive rows of wlbar_pixels(),
 * always wlbar_width()*4 in the current implementation. */
int   wlbar_stride(void);

/* Submit the current contents of wlbar_pixels() to the compositor.
 * Damages the whole surface. */
void  wlbar_commit(void);

/* Event loop ------------------------------------------------------ */

/* Dispatch any pending Wayland events.  Blocks up to TIMEOUT_MS
 * milliseconds (-1 = forever).  Returns 1 if events were processed,
 * 0 on timeout, -1 on display error (caller should call shutdown). */
int   wlbar_poll(int timeout_ms);

/* The compositor file descriptor; suitable for poll()/select() if
 * the caller wants to multiplex with other fds. */
int   wlbar_fd(void);

/* Returns 1 once the compositor has sent the closed event for the
 * layer surface (e.g. monitor disconnect).  Caller should exit. */
int   wlbar_closed(void);

#ifdef __cplusplus
}
#endif

#endif /* LISPBAR_WLBAR_H */
