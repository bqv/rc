module Graphics.Wayland.WlRoots.Backend.Headless
    ( backendIsHeadless
    , inputDeviceIsHeadless
    , addHeadlessInput
    , createHeadlessBackend
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/backend/headless.h>

import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt (..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr, nullPtr)

import Graphics.Wayland.Server (DisplayServer (..))

import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.WlRoots.Input

foreign import ccall unsafe "wlr_backend_is_headless" c_is_headless :: Ptr Backend -> IO Word8

backendIsHeadless :: Ptr Backend -> IO Bool
backendIsHeadless = fmap (/= 0) . c_is_headless

foreign import ccall unsafe "wlr_input_device_is_headless" c_input_is_headless :: Ptr InputDevice -> IO Word8

inputDeviceIsHeadless :: Ptr InputDevice -> IO Bool
inputDeviceIsHeadless = fmap (/= 0) . c_input_is_headless

foreign import ccall "wlr_headless_add_input_device" c_add_input :: Ptr Backend -> CInt -> IO (Ptr InputDevice)

addHeadlessInput :: Ptr Backend -> (Ptr a -> DeviceType) -> IO (Maybe (Ptr InputDevice))
addHeadlessInput backend devType = do
    isHeadless <- backendIsHeadless backend
    if not isHeadless
        then pure Nothing
        else Just <$> c_add_input backend (deviceTypeToInt $ devType nullPtr)

foreign import ccall unsafe "wlr_headless_backend_create" c_headless_create :: Ptr DisplayServer -> IO (Ptr Backend)

createHeadlessBackend :: DisplayServer -> IO (Ptr Backend)
createHeadlessBackend (DisplayServer ptr) = 
    throwErrnoIfNull "createHeadlessBackend" $ c_headless_create ptr
