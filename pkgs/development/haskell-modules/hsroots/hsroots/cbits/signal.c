#include <wayland-server.h>

void
c_signal_add(struct wl_signal *signal, struct wl_listener *listener)
{
	wl_signal_add(signal, listener);
}
