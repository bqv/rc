module Graphics.Wayland.WlRoots.PrimarySelection
    ( PrimarySelectionManager (..)
    , createPrimaryDeviceManager
    , destroyPrimaryDeviceManager
    , getPrimaryGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_primary_selection.h>

import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.Global (WlGlobal)

newtype PrimarySelectionManager = PrimarySelectionManager { unPSM :: Ptr PrimarySelectionManager}

foreign import ccall unsafe "wlr_primary_selection_device_manager_create" c_create :: Ptr DisplayServer -> IO (Ptr PrimarySelectionManager)

createPrimaryDeviceManager :: DisplayServer -> IO PrimarySelectionManager
createPrimaryDeviceManager (DisplayServer ptr) = fmap PrimarySelectionManager .
    throwErrnoIfNull "createPrimaryDeviceManager" $ c_create ptr

foreign import ccall safe "wlr_primary_selection_device_manager_destroy" c_destroy :: Ptr PrimarySelectionManager -> IO ()

destroyPrimaryDeviceManager :: PrimarySelectionManager -> IO ()
destroyPrimaryDeviceManager = c_destroy . unPSM

getPrimaryGlobal :: PrimarySelectionManager -> IO (Ptr WlGlobal)
getPrimaryGlobal = #{peek struct wlr_primary_selection_device_manager, global} . unPSM
