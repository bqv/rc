module Graphics.Egl
    ( Platform(..)
    , getPlatform
    )
where

#include <EGL/egl.h>
#include <EGL/eglext.h>

data Platform
    = DeviceExt
    | AndroidKhr
    | X11Ext
    | X11Khr
    | X11ScreenExt
    | X11ScreenKhr
    | GBMKhr
    | GBMMesa
    | WaylandExt
    | WaylandKhr
    | SurfacelessMesa

getPlatform :: Num a => Platform -> a
getPlatform DeviceExt       = #{const EGL_PLATFORM_DEVICE_EXT}
getPlatform AndroidKhr      = #{const EGL_PLATFORM_ANDROID_KHR}
getPlatform X11Ext          = #{const EGL_PLATFORM_X11_EXT}
getPlatform X11Khr          = #{const EGL_PLATFORM_X11_KHR}
getPlatform X11ScreenExt    = #{const EGL_PLATFORM_X11_SCREEN_EXT}
getPlatform X11ScreenKhr    = #{const EGL_PLATFORM_X11_SCREEN_KHR}
getPlatform GBMKhr          = #{const EGL_PLATFORM_GBM_KHR}
getPlatform GBMMesa         = #{const EGL_PLATFORM_GBM_MESA}
getPlatform WaylandExt      = #{const EGL_PLATFORM_WAYLAND_EXT}
getPlatform WaylandKhr      = #{const EGL_PLATFORM_WAYLAND_KHR}
getPlatform SurfacelessMesa = #{const EGL_PLATFORM_SURFACELESS_MESA}
