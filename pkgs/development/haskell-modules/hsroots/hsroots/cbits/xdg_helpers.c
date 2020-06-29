#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_box.h>
#include <wlr/types/wlr_xdg_shell_v6.h>
#include <wlr/types/wlr_xdg_shell.h>

void wlr_xdg_positioner_v6_get_geometry_c(struct wlr_xdg_positioner_v6 *positioner,
                                          struct wlr_box *box)
{
	*box = wlr_xdg_positioner_v6_get_geometry(positioner);
}

//void wlr_xdg_positioner_get_geometry_c(struct wlr_xdg_positioner *positioner,
//                                       struct wlr_box *box)
//{
//	*box = wlr_xdg_positioner_get_geometry(positioner);
//}
