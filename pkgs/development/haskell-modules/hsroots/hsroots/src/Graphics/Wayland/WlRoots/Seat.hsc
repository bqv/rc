module Graphics.Wayland.WlRoots.Seat
    ( WlrSeat
    , createSeat
    , destroySeat
    , handleForClient
    , setSeatCapabilities

    , pointerNotifyEnter
    , pointerNotifyMotion

    , keyboardNotifyEnter
    , getSeatKeyboard

    , pointerClearFocus
    , pointerNotifyButton
    , pointerNotifyAxis

    , keyboardNotifyKey
    , keyboardNotifyModifiers
    , seatSetKeyboard

    , keyboardClearFocus

    , SetCursorEvent (..)
    , SeatRequestSetSelectionEvent(..)
    , SeatSignals (..)

    , setSelection

    , WlrSeatClient (..)
    , seatGetSignals
    , seatClientGetClient
    , clientHasTouch

    , WlrSeatKeyboardState
    , getKeyboardState
    , getKeyboardFocus

    , WlrSeatPointerState
    , getPointerState
    , getPointerFocus

    , touchNotifyMotion
    , touchNotifyUp
    , touchNotifyDown
    , touchPointFocus
    , touchClearFocus
    , getSelectionSource

    , WlrPointerGrabInterface (..)
    , setSeatPointerGrab
    , endSeatPointerGrab
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_seat.h>

import Data.Bits ((.|.))
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CSize (..))
import Foreign.Ptr (Ptr, plusPtr, nullPtr, FunPtr)
import Foreign.StablePtr (castPtrToStablePtr, deRefStablePtr, newStablePtr, castStablePtrToPtr, freeStablePtr)
import Foreign.Storable(Storable(..))
import Foreign.Marshal.Alloc (free, callocBytes)

import Graphics.Wayland.List (isListEmpty)
import Graphics.Wayland.Server (DisplayServer(..), Client (..), SeatCapability(..))
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.DeviceManager (WlrDataSource (..))
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard, KeyState(..), keyStateToInt, KeyboardModifiers)
import Graphics.Wayland.WlRoots.Input.Pointer (axisOToInt, AxisOrientation)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

data WlrSeat

foreign import ccall "wlr_seat_create" c_create :: Ptr DisplayServer -> CString -> IO (Ptr WlrSeat)

createSeat :: DisplayServer -> String -> IO (Ptr WlrSeat)
createSeat (DisplayServer ptr) name = throwErrnoIfNull "createSeat" $ withCString name $ c_create ptr


foreign import ccall "wlr_seat_destroy" c_destroy :: Ptr WlrSeat -> IO ()

destroySeat :: Ptr WlrSeat -> IO ()
destroySeat = c_destroy


newtype WlrSeatClient = WlrSeatClient { unSeatClient :: Ptr WlrSeatClient }

foreign import ccall "wlr_seat_client_for_wl_client" c_handle_for_client :: Ptr WlrSeat -> Ptr Client -> IO (Ptr WlrSeatClient)

handleForClient :: Ptr WlrSeat -> Client -> IO (Maybe WlrSeatClient)
handleForClient seat (Client client) = do
    ret <-  c_handle_for_client seat client
    if ret == nullPtr
        then pure Nothing
        else pure . Just $ WlrSeatClient ret

clientHasTouch :: WlrSeatClient -> IO Bool
clientHasTouch =
    fmap not . isListEmpty . #{ptr struct wlr_seat_client, touches} . unSeatClient

foreign import ccall "wlr_seat_set_capabilities" c_set_caps :: Ptr WlrSeat -> CInt -> IO ()

setSeatCapabilities :: Ptr WlrSeat -> [SeatCapability] -> IO ()
setSeatCapabilities seat xs =
    c_set_caps seat (fromIntegral $ foldr ((.|.) . unCap) 0 xs)
    where unCap :: SeatCapability -> Int
          unCap (SeatCapability x) = x


foreign import ccall "wlr_seat_pointer_notify_enter" c_pointer_enter :: Ptr WlrSeat -> Ptr WlrSurface -> Double -> Double -> IO ()

pointerNotifyEnter :: Ptr WlrSeat -> Ptr WlrSurface -> Double -> Double -> IO ()
pointerNotifyEnter = c_pointer_enter


foreign import ccall "wlr_seat_keyboard_notify_enter" c_keyboard_enter :: Ptr WlrSeat -> Ptr WlrSurface -> Ptr Word32 -> CSize -> Ptr KeyboardModifiers -> IO ()

keyboardNotifyEnter :: Ptr WlrSeat -> Ptr WlrSurface -> Ptr Word32 -> CSize -> Ptr KeyboardModifiers -> IO ()
keyboardNotifyEnter = c_keyboard_enter

-- struct wlr_keyboard *wlr_seat_get_keyboard(struct wlr_seat *seat);
foreign import ccall unsafe "wlr_seat_get_keyboard" c_get_keyboard :: Ptr WlrSeat -> IO (Ptr WlrKeyboard)

getSeatKeyboard :: Ptr WlrSeat -> IO (Maybe (Ptr WlrKeyboard))
getSeatKeyboard seat = do
    ret <- c_get_keyboard seat
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

foreign import ccall "wlr_seat_pointer_notify_motion" c_pointer_motion :: Ptr WlrSeat -> Word32 -> Double -> Double -> IO ()

pointerNotifyMotion :: Ptr WlrSeat -> Word32 -> Double -> Double -> IO ()
pointerNotifyMotion = c_pointer_motion


foreign import ccall "wlr_seat_pointer_clear_focus" c_pointer_clear_focus :: Ptr WlrSeat -> IO ()

pointerClearFocus :: Ptr WlrSeat -> IO ()
pointerClearFocus = c_pointer_clear_focus


foreign import ccall "wlr_seat_pointer_notify_button" c_notify_button :: Ptr WlrSeat -> Word32 -> Word32 -> Word32 -> IO ()

pointerNotifyButton :: Ptr WlrSeat -> Word32 -> Word32 -> ButtonState -> IO ()
pointerNotifyButton seat time button state =
    c_notify_button seat time button (buttonStateToInt state)

foreign import ccall "wlr_seat_pointer_notify_axis" c_pointer_notify_axis :: Ptr WlrSeat -> Word32 -> CInt -> Double -> Int32 -> IO ()

pointerNotifyAxis :: Ptr WlrSeat -> Word32 -> AxisOrientation -> Double -> Int32 -> IO ()
pointerNotifyAxis seat time orientation value discrete =
    c_pointer_notify_axis seat time (axisOToInt orientation) value discrete

foreign import ccall "wlr_seat_keyboard_notify_key" c_notify_key :: Ptr WlrSeat -> Word32 -> Word32 -> Word32 -> IO ()

keyboardNotifyKey :: Ptr WlrSeat -> Word32 -> Word32 -> KeyState -> IO ()
keyboardNotifyKey seat time key state = c_notify_key seat time key (keyStateToInt state)

foreign import ccall "wlr_seat_keyboard_notify_modifiers" c_notify_modifiers :: Ptr WlrSeat -> Ptr KeyboardModifiers -> IO ()

keyboardNotifyModifiers :: Ptr WlrSeat -> Ptr KeyboardModifiers -> IO ()
keyboardNotifyModifiers = c_notify_modifiers

foreign import ccall unsafe "wlr_seat_set_keyboard" c_set_keyboard :: Ptr WlrSeat  -> Ptr InputDevice -> IO ()

seatSetKeyboard :: Ptr WlrSeat -> Ptr InputDevice -> IO ()
seatSetKeyboard = c_set_keyboard

foreign import ccall "wlr_seat_keyboard_clear_focus" c_keyboard_clear_focus :: Ptr WlrSeat -> IO ()

keyboardClearFocus :: Ptr WlrSeat -> IO ()
keyboardClearFocus = c_keyboard_clear_focus

seatClientGetClient :: WlrSeatClient -> IO Client
seatClientGetClient = fmap Client . #{peek struct wlr_seat_client, client} . unSeatClient

data SetCursorEvent = SetCursorEvent
    { seatCursorSurfaceClient   :: WlrSeatClient
    , seatCursorSurfaceSurface  :: Ptr WlrSurface
    , seatCursorSurfaceSerial   :: Word32
    , seatCursorSurfaceHotspotX :: Int32
    , seatCursorSurfaceHotspotY :: Int32
    }

instance Storable SetCursorEvent where
    sizeOf _ = #{size struct wlr_seat_pointer_request_set_cursor_event}
    alignment _ = #{alignment struct wlr_seat_pointer_request_set_cursor_event}
    peek ptr = SetCursorEvent . WlrSeatClient
        <$> #{peek struct wlr_seat_pointer_request_set_cursor_event, seat_client} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, surface} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, serial} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, hotspot_x} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, hotspot_y} ptr
    poke ptr evt = do
        #{poke struct wlr_seat_pointer_request_set_cursor_event, seat_client} ptr . unSeatClient $ seatCursorSurfaceClient evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, surface} ptr $ seatCursorSurfaceSurface evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, serial} ptr $ seatCursorSurfaceSerial evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, hotspot_x} ptr $ seatCursorSurfaceHotspotX evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, hotspot_y} ptr $ seatCursorSurfaceHotspotY evt

data SeatRequestSetSelectionEvent = SeatRequestSetSelectionEvent
    { seatRequestSetSelectionEventSource :: WlrDataSource
    , seatRequestSetSelectionEventSerial :: Word32
    } deriving (Show, Eq)

instance Storable SeatRequestSetSelectionEvent where
    sizeOf _ = #{size struct wlr_seat_request_set_selection_event}
    alignment _ = #{alignment struct wlr_seat_request_set_selection_event}
    peek ptr = SeatRequestSetSelectionEvent . WlrDataSource
        <$> #{peek struct wlr_seat_request_set_selection_event, source} ptr
        <*> #{peek struct wlr_seat_request_set_selection_event, serial} ptr
    poke ptr evt = do
        #{poke struct wlr_seat_request_set_selection_event, source} ptr . unDS $ seatRequestSetSelectionEventSource evt
        #{poke struct wlr_seat_request_set_selection_event, serial} ptr $ seatRequestSetSelectionEventSerial evt

data SeatSignals = SeatSignals
    { seatSignalSetCursor :: Ptr (WlSignal SetCursorEvent)
    , seatSignalRequestSetSelection :: Ptr (WlSignal SeatRequestSetSelectionEvent)
    , seatSignalSetSelection :: Ptr (WlSignal SeatRequestSetSelectionEvent)
    }

seatGetSignals :: Ptr WlrSeat -> SeatSignals
seatGetSignals ptr = SeatSignals
    { seatSignalSetCursor = #{ptr struct wlr_seat, events.request_set_cursor} ptr
    , seatSignalRequestSetSelection = #{ptr struct wlr_seat, events.request_set_selection} ptr
    , seatSignalSetSelection = #{ptr struct wlr_seat, events.set_selection} ptr
    }

foreign import ccall safe "wlr_seat_set_selection" c_set_selection :: Ptr WlrSeat -> Ptr WlrDataSource -> Word32 -> IO ()
setSelection :: Ptr WlrSeat -> WlrDataSource -> Word32 -> IO ()
setSelection seat (WlrDataSource dev) serial = c_set_selection seat dev serial

data WlrSeatKeyboardState

getKeyboardState :: Ptr WlrSeat -> Ptr WlrSeatKeyboardState
getKeyboardState = #{ptr struct wlr_seat, keyboard_state}

getKeyboardFocus :: Ptr WlrSeatKeyboardState -> IO (Ptr WlrSurface)
getKeyboardFocus = #{peek struct wlr_seat_keyboard_state, focused_surface}

data WlrSeatPointerState

getPointerState :: Ptr WlrSeat -> Ptr WlrSeatPointerState
getPointerState = #{ptr struct wlr_seat, pointer_state}

getPointerFocus :: Ptr WlrSeatPointerState -> IO (Ptr WlrSurface)
getPointerFocus = #{peek struct wlr_seat_pointer_state, focused_surface}


foreign import ccall "wlr_seat_touch_notify_down" c_touch_notify_down :: Ptr WlrSeat -> Ptr WlrSurface -> Word32 -> Int32 -> Double -> Double -> IO Word32
touchNotifyDown :: Ptr WlrSeat -> Ptr WlrSurface -> Word32 -> Int32 -> Double -> Double -> IO Word32
touchNotifyDown = c_touch_notify_down


foreign import ccall "wlr_seat_touch_notify_up" c_touch_notify_up :: Ptr WlrSeat -> Word32 -> Int32 -> IO ()
touchNotifyUp :: Ptr WlrSeat -> Word32 -> Int32 -> IO ()
touchNotifyUp = c_touch_notify_up

foreign import ccall "wlr_seat_touch_notify_motion" c_touch_notify_motion :: Ptr WlrSeat -> Word32 -> Int32 -> Double -> Double -> IO ()
touchNotifyMotion :: Ptr WlrSeat -> Word32 -> Int32 -> Double -> Double -> IO ()
touchNotifyMotion = c_touch_notify_motion

foreign import ccall "wlr_seat_touch_point_focus" c_touch_point_focus :: Ptr WlrSeat -> Ptr WlrSurface -> Word32 -> Int32 -> Double -> Double -> IO ()
touchPointFocus :: Ptr WlrSeat -> Ptr WlrSurface -> Word32 -> Int32 -> Double -> Double -> IO ()
touchPointFocus = c_touch_point_focus

foreign import ccall "wlr_seat_touch_point_clear_focus" c_touch_point_clear_focus :: Ptr WlrSeat -> Word32 -> Int32 -> IO ()
touchClearFocus :: Ptr WlrSeat -> Word32 -> Int32 -> IO ()
touchClearFocus = c_touch_point_clear_focus

getSelectionSource :: Ptr WlrSeat -> IO (Maybe WlrDataSource)
getSelectionSource ptr = do
    ret <- #{peek struct wlr_seat, selection_source} ptr
    pure $ if ret /= nullPtr
        then Just $ WlrDataSource ret
        else Nothing

data WlrSeatPointerGrab

data WlrPointerGrabInterface = WlrPointerGrabInterface
    { pointerGrabEnter  :: Ptr WlrSurface -> Double -> Double -> IO ()
    , pointerGrabMotion :: Word32 -> Double -> Double -> IO ()
    , pointerGrabButton :: Word32 -> Word32 -> Word32 -> IO Word32
    , pointerGrabAxis   :: Word32 -> CInt -> Double -> IO ()
    , pointerGrabCancel :: IO ()
    }

getPointerGrabData :: Ptr WlrSeatPointerGrab -> IO (Ptr a)
getPointerGrabData = #{peek struct wlr_seat_pointer_grab, data}

pointerGrabEnterImpl :: Ptr WlrSeatPointerGrab -> Ptr WlrSurface -> Double -> Double -> IO ()
pointerGrabEnterImpl grab surf x y = do
    ifacePtr <- getPointerGrabData grab
    iface <- deRefStablePtr $ castPtrToStablePtr ifacePtr
    pointerGrabEnter iface surf x y

foreign export ccall "pointerGrabEnterImpl" pointerGrabEnterImpl :: Ptr WlrSeatPointerGrab -> Ptr WlrSurface -> Double -> Double -> IO ()
foreign import ccall "&pointerGrabEnterImpl" pointerGrabEnterPtr :: FunPtr (Ptr WlrSeatPointerGrab -> Ptr WlrSurface -> Double -> Double -> IO ())


pointerGrabMotionImpl :: Ptr WlrSeatPointerGrab -> Word32 -> Double -> Double -> IO ()
pointerGrabMotionImpl grab time x y = do
    ifacePtr <- getPointerGrabData grab
    iface <- deRefStablePtr $ castPtrToStablePtr ifacePtr
    pointerGrabMotion iface time x y

foreign export ccall "pointerGrabMotionImpl" pointerGrabMotionImpl :: Ptr WlrSeatPointerGrab -> Word32 -> Double -> Double -> IO ()
foreign import ccall "&pointerGrabMotionImpl" pointerGrabMotionPtr :: FunPtr (Ptr WlrSeatPointerGrab -> Word32 -> Double -> Double -> IO ())


pointerGrabButtonImpl :: Ptr WlrSeatPointerGrab -> Word32 -> Word32 -> Word32 -> IO Word32
pointerGrabButtonImpl grab time button state = do
    ifacePtr <- getPointerGrabData grab
    iface <- deRefStablePtr $ castPtrToStablePtr ifacePtr
    pointerGrabButton iface time button state

foreign export ccall "pointerGrabButtonImpl" pointerGrabButtonImpl :: Ptr WlrSeatPointerGrab -> Word32 -> Word32 -> Word32 -> IO Word32
foreign import ccall "&pointerGrabButtonImpl" pointerGrabButtonPtr :: FunPtr (Ptr WlrSeatPointerGrab -> Word32 -> Word32 -> Word32 -> IO Word32)


pointerGrabAxisImpl :: Ptr WlrSeatPointerGrab -> Word32 -> CInt -> Double -> IO ()
pointerGrabAxisImpl grab time orientation value = do
    ifacePtr <- getPointerGrabData grab
    iface <- deRefStablePtr $ castPtrToStablePtr ifacePtr
    pointerGrabAxis iface time orientation value

foreign export ccall "pointerGrabAxisImpl" pointerGrabAxisImpl :: Ptr WlrSeatPointerGrab -> Word32 -> CInt -> Double -> IO ()
foreign import ccall "&pointerGrabAxisImpl" pointerGrabAxisPtr :: FunPtr (Ptr WlrSeatPointerGrab -> Word32 -> CInt -> Double -> IO ())


pointerGrabCancelImpl :: Ptr WlrSeatPointerGrab -> IO ()
pointerGrabCancelImpl grab = do
    ifacePtr <- getPointerGrabData grab
    iface <- deRefStablePtr $ castPtrToStablePtr ifacePtr
    pointerGrabCancel iface
    freeStablePtr $ castPtrToStablePtr ifacePtr
    free grab

foreign export ccall "pointerGrabCancelImpl" pointerGrabCancelImpl :: Ptr WlrSeatPointerGrab -> IO ()
foreign import ccall "&pointerGrabCancelImpl" pointerGrabCancelPtr :: FunPtr (Ptr WlrSeatPointerGrab -> IO ())

foreign import ccall "wlr_seat_pointer_start_grab" c_pointer_start_grab :: Ptr WlrSeat -> Ptr WlrSeatPointerGrab -> IO ()

setSeatPointerGrab :: Ptr WlrSeat -> WlrPointerGrabInterface -> IO ()
setSeatPointerGrab seat iface = do
    grab <- callocBytes $ #{size struct wlr_seat_pointer_grab} + #{size struct wlr_pointer_grab_interface}
    let cIface = plusPtr grab #{size struct wlr_seat_pointer_grab}
    sPtr <- newStablePtr iface
    #{poke struct wlr_seat_pointer_grab, data} grab $ castStablePtrToPtr sPtr
    #{poke struct wlr_seat_pointer_grab, interface} grab $ cIface

    #{poke struct wlr_pointer_grab_interface, enter} cIface pointerGrabEnterPtr
    #{poke struct wlr_pointer_grab_interface, motion} cIface pointerGrabMotionPtr
    #{poke struct wlr_pointer_grab_interface, button} cIface pointerGrabButtonPtr
    #{poke struct wlr_pointer_grab_interface, axis} cIface pointerGrabAxisPtr
    #{poke struct wlr_pointer_grab_interface, cancel} cIface pointerGrabCancelPtr

    c_pointer_start_grab seat grab

foreign import ccall "wlr_seat_pointer_end_grab" c_pointer_end_grab :: Ptr WlrSeat -> IO ()

endSeatPointerGrab :: Ptr WlrSeat -> IO ()
endSeatPointerGrab = c_pointer_end_grab
