module Graphics.Wayland.WlRoots.Backend.Libinput
    ( backendIsLibinput
    , inputDeviceIsLibinput
    , getDeviceHandle
    )
where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Backend
import Graphics.Wayland.WlRoots.Input

import qualified System.InputDevice as LI

foreign import ccall unsafe "wlr_backend_is_libinput" c_is_libinput :: Ptr Backend -> IO Word8

backendIsLibinput :: Ptr Backend -> IO Bool
backendIsLibinput = fmap (/= 0) . c_is_libinput

foreign import ccall unsafe "wlr_input_device_is_libinput" c_input_is_libinput :: Ptr InputDevice -> IO Word8

inputDeviceIsLibinput :: Ptr InputDevice -> IO Bool
inputDeviceIsLibinput = fmap (/= 0) . c_input_is_libinput

foreign import ccall unsafe "wlr_libinput_get_device_handle" c_get_handle :: Ptr InputDevice -> IO LI.InputDevice

getDeviceHandle :: Ptr InputDevice -> IO (Maybe LI.InputDevice)
getDeviceHandle ptr = do
    isLibinput <- inputDeviceIsLibinput ptr
    if isLibinput
        then Just <$> c_get_handle ptr
        else pure Nothing
