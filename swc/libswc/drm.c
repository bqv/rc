/* swc: drm.c
 *
 * Copyright (c) 2013-2020 Michael Forney
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "drm.h"
#include "dmabuf.h"
#include "event.h"
#include "internal.h"
#include "launch.h"
#include "output.h"
#include "plane.h"
#include "screen.h"
#include "util.h"
#include "wayland_buffer.h"

#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>
#include <drm.h>
#include <xf86drm.h>
#include <wld/wld.h>
#include <wld/drm.h>
#include <wayland-server.h>
#include "wayland-drm-server-protocol.h"

struct swc_drm swc_drm;

static struct {
        char *path;

        struct wl_global *global;
        struct wl_global *dmabuf;
        struct wl_event_source *event_source;
} drm;

static void
authenticate(struct wl_client *client, struct wl_resource *resource, uint32_t magic)
{
        wl_drm_send_authenticated(resource);
}

static void
                        output->screen = screen_new(resources->crtcs[crtc_index], output, cursor_plane);
                        output->screen->id = crtc_index;
                        taken_crtcs |= 1 << crtc_index;

                        wl_list_insert(screens, &output->screen->link);
                }
        }
        drmModeFreeResources(resources);

        return true;
}

enum {
        WLD_USER_OBJECT_FRAMEBUFFER = WLD_USER_ID
};

struct framebuffer {
        struct wld_exporter exporter;
        struct wld_destructor destructor;
        uint32_t id;
};

static bool
framebuffer_export(struct wld_exporter *exporter, struct wld_buffer *buffer, uint32_t type, union wld_object *object)
{
        struct framebuffer *framebuffer = wl_container_of(exporter, framebuffer, exporter);

        switch (type) {
        case WLD_USER_OBJECT_FRAMEBUFFER:
                object->u32 = framebuffer->id;
                break;
        default:
                return false;
        }

        return true;
}

static void
framebuffer_destroy(struct wld_destructor *destructor)
{
        struct framebuffer *framebuffer = wl_container_of(destructor, framebuffer, destructor);

        drmModeRmFB(swc.drm->fd, framebuffer->id);
        free(framebuffer);
}

uint32_t
drm_get_framebuffer(struct wld_buffer *buffer)
{
        struct framebuffer *framebuffer;
        union wld_object object;
        int ret;

        if (!buffer)
                return 0;

        if (wld_export(buffer, WLD_USER_OBJECT_FRAMEBUFFER, &object))
                return object.u32;

        if (!wld_export(buffer, WLD_DRM_OBJECT_HANDLE, &object)) {
                ERROR("Could not get buffer handle\n");
                return 0;
        }

        if (!(framebuffer = malloc(sizeof(*framebuffer))))
                return 0;

        ret = drmModeAddFB2(swc.drm->fd, buffer->width, buffer->height, buffer->format,
                            (uint32_t[4]){object.u32}, (uint32_t[4]){buffer->pitch}, (uint32_t[4]){0},
                            &framebuffer->id, 0);
        if (ret < 0) {
                free(framebuffer);
                return 0;
        }

        framebuffer->exporter.export = &framebuffer_export;
        wld_buffer_add_exporter(buffer, &framebuffer->exporter);
        framebuffer->destructor.destroy = &framebuffer_destroy;
        wld_buffer_add_destructor(buffer, &framebuffer->destructor);

        return framebuffer->id;
}
