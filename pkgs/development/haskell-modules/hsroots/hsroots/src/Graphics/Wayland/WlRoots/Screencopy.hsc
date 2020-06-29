module Graphics.Wayland.WlRoots.Screencopy
    ( WlrScreencopy
    , screencopyCreate
    , getScreencopyGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_screencopy_v1.h>

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.Global (WlGlobal)

data WlrScreencopy

foreign import ccall "wlr_screencopy_manager_v1" c_create :: Ptr DisplayServer -> IO (Ptr WlrScreencopy)

screencopyCreate :: DisplayServer -> IO (Ptr WlrScreencopy)
screencopyCreate (DisplayServer dsp) = c_create dsp

getScreencopyGlobal :: Ptr WlrScreencopy -> IO (Ptr WlGlobal)
getScreencopyGlobal = #{peek struct wlr_screencopy_manager_v1, global}
