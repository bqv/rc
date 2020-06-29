{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input
    ( DeviceType(..)
    , deviceTypeToInt
    , intToDeviceType

    , ButtonState(..)
    , buttonStateToInt
    , intToButtonState

    , InputDevice
    , inputDeviceType
    , getDestroySignal
    , getDeviceName
    , getCleanDeviceName
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_input_device.h>

import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.Signal (WlSignal)

import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard)
import Graphics.Wayland.WlRoots.Input.Pointer (WlrPointer)
import Graphics.Wayland.WlRoots.Input.TabletPad (WlrTabletPad (..))
import Graphics.Wayland.WlRoots.Input.Tablet (WlrTablet (..))
import Graphics.Wayland.WlRoots.Input.Touch (WlrTouch)
import Graphics.Wayland.WlRoots.Input.Buttons

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

newtype WlrSwitch = WlrSwitch (Ptr WlrSwitch) deriving (Eq, Show)

data DeviceType
    = DeviceKeyboard   !(Ptr WlrKeyboard)
    | DevicePointer    !(Ptr WlrPointer)
    | DeviceTouch      !(Ptr WlrTouch)
    | DeviceTablet     !WlrTablet
    | DeviceTabletPad  !WlrTabletPad
    | DeviceSwitch  !WlrSwitch
    deriving (Eq, Show)

deviceTypeToInt :: Num a => DeviceType -> a
deviceTypeToInt (DeviceKeyboard   _) = #{const WLR_INPUT_DEVICE_KEYBOARD}
deviceTypeToInt (DevicePointer    _) = #{const WLR_INPUT_DEVICE_POINTER}
deviceTypeToInt (DeviceTouch      _) = #{const WLR_INPUT_DEVICE_TOUCH}
deviceTypeToInt (DeviceTablet     _) = #{const WLR_INPUT_DEVICE_TABLET_TOOL}
deviceTypeToInt (DeviceTabletPad  _) = #{const WLR_INPUT_DEVICE_TABLET_PAD}
deviceTypeToInt (DeviceSwitch  _) = #{const WLR_INPUT_DEVICE_SWITCH}

intToDeviceType :: (Eq a, Num a, Show a) => a -> Ptr b -> DeviceType
intToDeviceType #{const WLR_INPUT_DEVICE_KEYBOARD}    = DeviceKeyboard . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_POINTER}     = DevicePointer . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TOUCH}       = DeviceTouch . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_TOOL} = DeviceTablet . WlrTablet . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_PAD}  = DeviceTabletPad . WlrTabletPad . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_SWITCH}  = DeviceSwitch . WlrSwitch . castPtr
intToDeviceType x = error $ "Got an unknown DeviceType: " ++ show x

data InputDevice

inputDeviceType :: Ptr InputDevice -> IO DeviceType
inputDeviceType ptr = do
    int :: CInt <- #{peek struct wlr_input_device, type} ptr
    devptr <- #{peek struct wlr_input_device, _device} ptr
    pure $ intToDeviceType int devptr

getDestroySignal :: Ptr InputDevice -> Ptr (WlSignal (InputDevice))
getDestroySignal = #{ptr struct wlr_input_device, events.destroy}

-- | Get device name + hexadecimal value pointer. This enforces that every
-- device has a unique name for logging/IPC, but isn't deterministic or pretty
getDeviceName :: Ptr InputDevice -> IO Text
getDeviceName ptr = do
    name <- fmap E.decodeUtf8 . unsafePackCString =<< #{peek struct wlr_input_device, name} ptr
    let pos = T.pack $ ' ':show ptr
    pure $ name `T.append` pos

-- | Get clean device name
getCleanDeviceName :: Ptr InputDevice -> IO Text
getCleanDeviceName ptr =
    fmap E.decodeUtf8 . unsafePackCString =<< #{peek struct wlr_input_device, name} ptr
