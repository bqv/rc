module Graphics.Wayland.WlRoots.GammaControl
    ( WlrGammaManager
    , createGammaManager
    , getGammaGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_gamma_control_v1.h>

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.Global (WlGlobal)

data WlrGammaManager

foreign import ccall "wlr_gamma_control_manager_v1_create" c_create :: Ptr DisplayServer -> IO (Ptr WlrGammaManager)

createGammaManager :: DisplayServer -> IO (Ptr WlrGammaManager)
createGammaManager (DisplayServer ptr) = c_create ptr

getGammaGlobal :: Ptr WlrGammaManager -> IO (Ptr WlGlobal)
getGammaGlobal = #{peek struct wlr_gamma_control_manager_v1, global}
