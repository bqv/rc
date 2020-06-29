{-# LANGUAGE OverloadedStrings #-}
module Graphics.Wayland.WlRoots.DataControl
    ( DataControlManager (..)
    , dataControlManagerCreate
    , dataControlManagerDestroy
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_data_device.h>

import Foreign.Ptr (Ptr)
import Graphics.Wayland.Server (DisplayServer(..))

import Foreign.C.Error (throwErrnoIfNull)

newtype DataControlManager = DataControlManager {unDCM :: Ptr DataControlManager}

foreign import ccall unsafe "wlr_data_control_manager_v1_create" c_create :: Ptr DisplayServer -> IO (Ptr DataControlManager)
dataControlManagerCreate :: DisplayServer -> IO DataControlManager
dataControlManagerCreate (DisplayServer dsp) = fmap DataControlManager .  throwErrnoIfNull "dataControlManagerCreate" $ c_create dsp

foreign import ccall unsafe "wlr_data_control_manager_v1_destroy" c_destroy :: Ptr DataControlManager -> IO ()
dataControlManagerDestroy :: DataControlManager -> IO ()
dataControlManagerDestroy = c_destroy . unDCM
