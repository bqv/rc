{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Tablet
    ( WlrTablet (..)
    , TabletEvents (..)
    , getTabletEvents

    , peekTabletData
    , pokeTabletData

    , ToolAxis (..)
    , ToolAxisEvent (..)

    , ProximityState (..)
    , ToolProximityEvent (..)

    , TipState (..)
    , ToolTipEvent (..)
    , tipStateToButtonState

    , ToolButtonEvent (..)
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_tablet_tool.h>

import Data.Bits (bit, (.&.))
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable

import Graphics.Wayland.Signal (WlSignal)
import {-# SOURCE #-} Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons (ButtonState, intToButtonState)
import Graphics.Wayland.WlRoots.Input.TabletTool (WlrTabletTool (..))

newtype WlrTablet = WlrTablet (Ptr WlrTablet) deriving (Eq, Show)


peekTabletData :: WlrTablet -> IO (Ptr a)
peekTabletData (WlrTablet ptr) = #{peek struct wlr_tablet, data} ptr

pokeTabletData :: WlrTablet -> Ptr a -> IO ()
pokeTabletData (WlrTablet ptr) = #{poke struct wlr_tablet, data} ptr


data TabletEvents = TabletEvents
    { tabletEventAxis      :: Ptr (WlSignal ToolAxisEvent)
    , tabletEventProximity :: Ptr (WlSignal ToolProximityEvent)
    , tabletEventTip       :: Ptr (WlSignal ToolTipEvent)
    , tabletEventButton    :: Ptr (WlSignal ToolButtonEvent)
    }


getTabletEvents :: WlrTablet -> TabletEvents
getTabletEvents (WlrTablet ptr) = TabletEvents
    { tabletEventAxis = #{ptr struct wlr_tablet, events.axis} ptr
    , tabletEventProximity = #{ptr struct wlr_tablet, events.proximity} ptr
    , tabletEventTip = #{ptr struct wlr_tablet, events.tip} ptr
    , tabletEventButton = #{ptr struct wlr_tablet, events.button} ptr
    }

data ToolAxis
    = AxisX Double
    | AxisY Double
    | AxisDistance Double
    | AxisPressure Double
    | AxisTiltX Double
    | AxisTiltY Double
    | AxisRotation Double
    | AxisSlider Double
    | AxisWheel Double
    deriving (Eq, Show, Read)

_toolAxisToInt :: Num a => ToolAxis -> a
_toolAxisToInt (AxisX _)        = #{const WLR_TABLET_TOOL_AXIS_X}
_toolAxisToInt (AxisY _)        = #{const WLR_TABLET_TOOL_AXIS_Y}
_toolAxisToInt (AxisDistance _) = #{const WLR_TABLET_TOOL_AXIS_DISTANCE}
_toolAxisToInt (AxisPressure _) = #{const WLR_TABLET_TOOL_AXIS_PRESSURE}
_toolAxisToInt (AxisTiltX _)    = #{const WLR_TABLET_TOOL_AXIS_TILT_X}
_toolAxisToInt (AxisTiltY _)    = #{const WLR_TABLET_TOOL_AXIS_TILT_Y}
_toolAxisToInt (AxisRotation _) = #{const WLR_TABLET_TOOL_AXIS_ROTATION}
_toolAxisToInt (AxisSlider _)   = #{const WLR_TABLET_TOOL_AXIS_SLIDER}
_toolAxisToInt (AxisWheel _)    = #{const WLR_TABLET_TOOL_AXIS_WHEEL}

readToolAxis :: (Eq a, Num a) => a -> Ptr ToolAxisEvent -> Maybe (IO ToolAxis)
readToolAxis #{const WLR_TABLET_TOOL_AXIS_X} ptr = Just $ AxisX
    <$> #{peek struct wlr_event_tablet_tool_axis, x} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_Y} ptr = Just $ AxisY
    <$> #{peek struct wlr_event_tablet_tool_axis, y} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_DISTANCE} ptr = Just $ AxisDistance
    <$> #{peek struct wlr_event_tablet_tool_axis, distance} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_PRESSURE} ptr = Just $ AxisPressure
    <$> #{peek struct wlr_event_tablet_tool_axis, pressure} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_TILT_X} ptr = Just $ AxisTiltX
    <$> #{peek struct wlr_event_tablet_tool_axis, tilt_x} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_TILT_Y} ptr = Just $ AxisTiltY
    <$> #{peek struct wlr_event_tablet_tool_axis, tilt_y} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_ROTATION} ptr = Just $ AxisRotation
    <$> #{peek struct wlr_event_tablet_tool_axis, rotation} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_SLIDER} ptr = Just $ AxisSlider
    <$> #{peek struct wlr_event_tablet_tool_axis, slider} ptr
readToolAxis #{const WLR_TABLET_TOOL_AXIS_WHEEL} ptr = Just $ AxisWheel
    <$> #{peek struct wlr_event_tablet_tool_axis, wheel_delta} ptr
readToolAxis _ _ = Nothing

data ToolAxisEvent = ToolAxisEvent
    { toolAxisEvtTime   :: Word32
    , toolAxisEvtAxes   :: [ToolAxis]
    , toolAxisEvtDevice :: Ptr InputDevice
    , toolAxisEvtTool   :: WlrTabletTool
    } deriving (Show)

instance Storable ToolAxisEvent where
    sizeOf _ = #{size struct wlr_event_tablet_tool_axis}
    alignment _ = #{alignment struct wlr_event_tablet_tool_axis}
    peek ptr = do
        device <- #{peek struct wlr_event_tablet_tool_axis, device} ptr
        time <- #{peek struct wlr_event_tablet_tool_axis, time_msec} ptr
        axesEnum :: CInt <- #{peek struct wlr_event_tablet_tool_axis, updated_axes} ptr
        axes <- sequence . catMaybes . flip fmap [0..8] $ \index ->
            readToolAxis (bit index .&. axesEnum) ptr
        tool <- WlrTabletTool <$> #{peek struct wlr_event_tablet_tool_axis, tool} ptr
        pure $ ToolAxisEvent
            { toolAxisEvtTime = time
            , toolAxisEvtAxes = axes
            , toolAxisEvtDevice = device
            , toolAxisEvtTool = tool
            }
    poke _ _ = error "We don't poke ToolAxisEvents for now"

data ProximityState
    = ProximityIn
    | ProximityOut
    deriving (Show, Eq, Read)

proximityStateToInt :: Num a => ProximityState -> a
proximityStateToInt ProximityIn  = #{const WLR_TABLET_TOOL_PROXIMITY_OUT}
proximityStateToInt ProximityOut = #{const WLR_TABLET_TOOL_PROXIMITY_IN }

intToProximityState :: (Eq a, Num a, Show a) => a -> ProximityState
intToProximityState #{const WLR_TABLET_TOOL_PROXIMITY_OUT} = ProximityIn 
intToProximityState #{const WLR_TABLET_TOOL_PROXIMITY_IN } = ProximityOut
intToProximityState x = error $ "Got an an unknown PadRingSource: " ++ show x

instance Storable ProximityState where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToProximityState :: CInt -> ProximityState) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (proximityStateToInt val :: CInt)

data ToolProximityEvent = ToolProximityEvent
    { toolProximityEvtDevice :: Ptr InputDevice
    , toolProximityEvtTime   :: Word32
    , toolProximityEvtX      :: Double
    , toolProximityEvtY      :: Double
    , toolProximityEvtState  :: ProximityState
    , toolProximityEvtTool   :: WlrTabletTool
    } deriving (Show)

instance Storable ToolProximityEvent where
    sizeOf _ = #{size struct wlr_event_tablet_tool_proximity}
    alignment _ = #{alignment struct wlr_event_tablet_tool_proximity}
    peek ptr = ToolProximityEvent
        <$> #{peek struct wlr_event_tablet_tool_proximity, device} ptr
        <*> #{peek struct wlr_event_tablet_tool_proximity, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_tool_proximity, x} ptr
        <*> #{peek struct wlr_event_tablet_tool_proximity, y} ptr
        <*> #{peek struct wlr_event_tablet_tool_proximity, state} ptr
        <*> (WlrTabletTool <$> #{peek struct wlr_event_tablet_tool_proximity, tool} ptr)
    poke _ _ = error "We don't poke ToolProximityEvents for now"


data TipState
    = TipUp
    | TipDown
    deriving (Show, Eq, Read)

tipStateToInt :: Num a => TipState -> a
tipStateToInt TipUp   = #{const WLR_TABLET_TOOL_TIP_UP}
tipStateToInt TipDown = #{const WLR_TABLET_TOOL_TIP_DOWN }

intToTipState :: (Eq a, Num a, Show a) => a -> TipState
intToTipState #{const WLR_TABLET_TOOL_TIP_UP} = TipUp
intToTipState #{const WLR_TABLET_TOOL_TIP_DOWN} = TipDown
intToTipState x = error $ "Got an an unknown PadRingSource: " ++ show x

tipStateToButtonState :: TipState -> ButtonState
tipStateToButtonState state = intToButtonState (tipStateToInt state :: Int)

instance Storable TipState where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToTipState :: CInt -> TipState) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (tipStateToInt val :: CInt)


data ToolTipEvent = ToolTipEvent
    { toolTipEvtDevice :: Ptr InputDevice
    , toolTipEvtTime   :: Word32
    , toolTipEvtX      :: Double
    , toolTipEvtY      :: Double
    , toolTipEvtState  :: TipState
    , toolTipEvtTool   :: WlrTabletTool
    } deriving (Show)

instance Storable ToolTipEvent where
    sizeOf _ = #{size struct wlr_event_tablet_tool_tip}
    alignment _ = #{alignment struct wlr_event_tablet_tool_tip}
    peek ptr = ToolTipEvent
        <$> #{peek struct wlr_event_tablet_tool_tip, device} ptr
        <*> #{peek struct wlr_event_tablet_tool_tip, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_tool_tip, x} ptr
        <*> #{peek struct wlr_event_tablet_tool_tip, y} ptr
        <*> #{peek struct wlr_event_tablet_tool_tip, state} ptr
        <*> (WlrTabletTool <$> #{peek struct wlr_event_tablet_tool_tip, tool} ptr)
    poke _ _ = error "We don't poke ToolTipEvents for now"

data ToolButtonEvent = ToolButtonEvent
    { toolButtonEvtDevice :: Ptr InputDevice
    , toolButtonEvtTime   :: Word32
    , toolButtonEvtButton :: Word32
    , toolButtonEvtState  :: ButtonState
    , toolButtonEvtTool   :: WlrTabletTool
    } deriving (Show)

instance Storable ToolButtonEvent where
    sizeOf _ = #{size struct wlr_event_tablet_tool_button}
    alignment _ = #{alignment struct wlr_event_tablet_tool_button}
    peek ptr = ToolButtonEvent
        <$> #{peek struct wlr_event_tablet_tool_button, device} ptr
        <*> #{peek struct wlr_event_tablet_tool_button, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_tool_button, button} ptr
        <*> #{peek struct wlr_event_tablet_tool_button, state} ptr
        <*> (WlrTabletTool <$> #{peek struct wlr_event_tablet_tool_button, tool} ptr)
    poke ptr evt = do
        #{poke struct wlr_event_tablet_tool_button, device} ptr $    toolButtonEvtDevice evt
        #{poke struct wlr_event_tablet_tool_button, time_msec} ptr $ toolButtonEvtTime   evt
        #{poke struct wlr_event_tablet_tool_button, button} ptr $    toolButtonEvtButton evt
        #{poke struct wlr_event_tablet_tool_button, state} ptr $     toolButtonEvtState  evt
        #{poke struct wlr_event_tablet_tool_button, tool} ptr $ unWlrTabletTool $ toolButtonEvtTool  evt

