{-# LANGUAGE TupleSections #-}
module Graphics.Wayland.WlRoots.Cursor
    ( WlrCursor
    , createCursor
    , destroyCursor

    , getCursorX
    , getCursorY

    , setXCursor
    , warpCursor
    , warpCursorAbs
    , moveCursor
    , attachInputDevice
    , detachInputDevice
    , attachOutputLayout

    , mapToOutput
    , mapInputToOutput

    , mapToRegion

    , CursorEvents (..)
    , cursorGetEvents
    , setCursorImage
    , setCursorSurface
    , absCoordsToGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_cursor.h>

import Data.Word (Word32, Word8)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Pointer
    (WlrEventPointerButton, WlrEventPointerMotion, WlrEventPointerAbsMotion, WlrEventPointerAxis)
import Graphics.Wayland.WlRoots.Input.Tablet
    (ToolAxisEvent, ToolProximityEvent, ToolTipEvent, ToolButtonEvent)
import Graphics.Wayland.WlRoots.Input.Touch
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.XCursor (WlrXCursor)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

data CursorEvents = CursorEvents
    { cursorButton    :: !(Ptr (WlSignal WlrEventPointerButton))
    , cursorMotion    :: !(Ptr (WlSignal WlrEventPointerMotion))
    , cursorMotionAbs :: !(Ptr (WlSignal WlrEventPointerAbsMotion))
    , cursorAxis      :: !(Ptr (WlSignal WlrEventPointerAxis))

    , cursorTouchDown   :: !(Ptr (WlSignal WlrTouchDown))
    , cursorTouchUp     :: !(Ptr (WlSignal WlrTouchUp))
    , cursorTouchMotion :: !(Ptr (WlSignal WlrTouchMotion))
    , cursorTouchCancel :: !(Ptr (WlSignal WlrTouchCancel))

    , cursorToolAxis       :: !(Ptr (WlSignal ToolAxisEvent))
    , cursorToolProximity  :: !(Ptr (WlSignal ToolProximityEvent))
    , cursorToolTip        :: !(Ptr (WlSignal ToolTipEvent))
    , cursorToolButton     :: !(Ptr (WlSignal ToolButtonEvent))
    }

cursorGetEvents :: Ptr WlrCursor -> CursorEvents
cursorGetEvents ptr = CursorEvents
    { cursorButton    = #{ptr struct wlr_cursor, events.button} ptr
    , cursorMotion    = #{ptr struct wlr_cursor, events.motion} ptr
    , cursorMotionAbs = #{ptr struct wlr_cursor, events.motion_absolute} ptr
    , cursorAxis      = #{ptr struct wlr_cursor, events.axis} ptr

    , cursorTouchDown   = #{ptr struct wlr_cursor, events.touch_down} ptr
    , cursorTouchUp     = #{ptr struct wlr_cursor, events.touch_up} ptr
    , cursorTouchCancel = #{ptr struct wlr_cursor, events.touch_cancel} ptr
    , cursorTouchMotion = #{ptr struct wlr_cursor, events.touch_motion} ptr

    , cursorToolAxis       = #{ptr struct wlr_cursor, events.tablet_tool_axis} ptr
    , cursorToolProximity  = #{ptr struct wlr_cursor, events.tablet_tool_proximity} ptr
    , cursorToolTip        = #{ptr struct wlr_cursor, events.tablet_tool_tip} ptr
    , cursorToolButton     = #{ptr struct wlr_cursor, events.tablet_tool_button} ptr
    }

data WlrCursor

foreign import ccall "wlr_cursor_create" c_cursor_create :: IO (Ptr WlrCursor)

createCursor :: IO (Ptr WlrCursor)
createCursor = throwErrnoIfNull "createCursor" c_cursor_create

getCursorX :: Ptr WlrCursor -> IO Double
getCursorX =  #{peek struct wlr_cursor, x}

getCursorY :: Ptr WlrCursor -> IO Double
getCursorY =  #{peek struct wlr_cursor, y}


foreign import ccall "wlr_cursor_destroy" c_cursor_destroy :: Ptr WlrCursor -> IO ()

destroyCursor :: Ptr WlrCursor -> IO ()
destroyCursor = c_cursor_destroy


setXCursor :: Ptr WlrCursor -> Ptr WlrXCursor -> IO ()
setXCursor = \_ _ -> pure ()
    --c_set_xcursor


foreign import ccall "wlr_cursor_warp" c_cursor_warp :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO Bool

warpCursor :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Double -> Double -> IO Bool
warpCursor cursor Nothing x y = warpCursor cursor (Just nullPtr) x y
warpCursor cursor (Just dev) x y = c_cursor_warp cursor dev x y


foreign import ccall "wlr_cursor_warp_absolute" c_cursor_warp_abs :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO ()

warpCursorAbs :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Maybe Double -> Maybe Double -> IO ()
warpCursorAbs cursor Nothing x y = warpCursorAbs cursor (Just nullPtr) x y
warpCursorAbs cursor (Just dev) x y =
    c_cursor_warp_abs cursor dev (fromMaybe (-1) x) (fromMaybe (-1) y)


foreign import ccall "wlr_cursor_move" c_cursor_move :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO ()

moveCursor :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Double -> Double -> IO ()
moveCursor cursor Nothing x y = moveCursor cursor (Just nullPtr) x y
moveCursor cursor (Just dev) x y = c_cursor_move cursor dev x y


foreign import ccall "wlr_cursor_attach_input_device" c_attach_input_device :: Ptr WlrCursor -> Ptr InputDevice -> IO ()

attachInputDevice :: Ptr WlrCursor -> Ptr InputDevice -> IO ()
attachInputDevice = c_attach_input_device


foreign import ccall "wlr_cursor_detach_input_device" c_detach_input_device :: Ptr WlrCursor -> Ptr InputDevice -> IO ()

detachInputDevice :: Ptr WlrCursor -> Ptr InputDevice -> IO ()
detachInputDevice = c_detach_input_device


foreign import ccall "wlr_cursor_attach_output_layout" c_attach_layout :: Ptr WlrCursor -> Ptr WlrOutputLayout -> IO ()

attachOutputLayout :: Ptr WlrCursor -> Ptr WlrOutputLayout -> IO ()
attachOutputLayout = c_attach_layout


foreign import ccall "wlr_cursor_map_to_output" c_map_to_output :: Ptr WlrCursor -> Ptr WlrOutput -> IO ()

mapToOutput :: Ptr WlrCursor -> Ptr WlrOutput -> IO ()
mapToOutput = c_map_to_output


foreign import ccall "wlr_cursor_map_input_to_output" c_map_intput_to_output :: Ptr WlrCursor -> Ptr InputDevice -> Ptr WlrOutput -> IO ()

mapInputToOutput :: Ptr WlrCursor -> Ptr InputDevice -> Ptr WlrOutput -> IO ()
mapInputToOutput = c_map_intput_to_output


foreign import ccall "wlr_cursor_map_to_region" c_map_to_region :: Ptr WlrCursor -> Ptr WlrBox -> IO ()

mapToRegion :: Ptr WlrCursor -> Maybe (Ptr WlrBox) -> IO ()
mapToRegion cursor Nothing = mapToRegion cursor (Just nullPtr)
mapToRegion cursor (Just box) = c_map_to_region cursor box

--void wlr_cursor_set_image(struct wlr_cursor *cur, const uint8_t *pixels,
--    int32_t stride, uint32_t width, uint32_t height, int32_t hotspot_x,
--    int32_t hotspot_y);

foreign import ccall "wlr_cursor_set_image" c_set_cursor_image :: Ptr WlrCursor -> Ptr () -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Bool


setCursorImage :: Ptr WlrCursor -> Ptr () -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
setCursorImage cursor buffer stride width height hotspot_x hotspot_y =
    throwErrnoIf_ not "setCursorImage" $ c_set_cursor_image cursor buffer stride width height hotspot_x hotspot_y

foreign import ccall "wlr_cursor_set_surface" c_set_surface :: Ptr WlrCursor -> Ptr WlrSurface -> Int32 -> Int32 -> IO ()

setCursorSurface :: Integral a => Ptr WlrCursor -> Ptr WlrSurface -> a -> a -> IO ()
setCursorSurface cursor surface hotspotX hotspotY = c_set_surface cursor surface (fromIntegral hotspotX) (fromIntegral hotspotY)


-- bool wlr_cursor_absolute_to_layout_coords(struct wlr_cursor *cur,struct wlr_input_device *device, double x_mm, double y_mm,double width_mm, double height_mm, double *lx, double *ly) {

foreign import ccall unsafe "wlr_cursor_absolute_to_layout_coords" c_absolute_to_layout_coords :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> Ptr Double -> Ptr Double -> IO Word8

absCoordsToGlobal :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO (Double, Double)
absCoordsToGlobal cursor dev x y = alloca $ \xptr -> alloca $ \yptr -> do
    _ <- c_absolute_to_layout_coords cursor dev x y xptr yptr
    (,) <$> peek xptr <*> peek yptr
