{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
module Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrPointer
    , WlrEventPointerButton (..)

    , pointerGetEvents
    , PointerEvents (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)

    , AxisSource (..)
    , AxisOrientation (..)
    , WlrEventPointerAxis (..)
    , axisOToInt
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_pointer.h>

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Input.Buttons
import {-# SOURCE #-} Graphics.Wayland.WlRoots.Input (InputDevice)

data PointerEvents = PointerEvents
    { pointerButton :: Ptr (WlSignal WlrEventPointerButton)
    , pointerMotion :: Ptr (WlSignal WlrEventPointerMotion)
    , pointerMotionAbs :: Ptr (WlSignal WlrEventPointerAbsMotion)
    , pointerAxis :: Ptr (WlSignal WlrEventPointerAxis)
    }

pointerGetEvents :: Ptr WlrPointer -> PointerEvents
pointerGetEvents ptr = PointerEvents
    { pointerButton = #{ptr struct wlr_pointer, events.button} ptr
    , pointerMotion = #{ptr struct wlr_pointer, events.motion} ptr
    , pointerMotionAbs = #{ptr struct wlr_pointer, events.motion_absolute} ptr
    , pointerAxis = #{ptr struct wlr_pointer, events.axis} ptr
    }

data WlrPointer

data WlrEventPointerButton = WlrEventPointerButton
    { eventPointerButtonDevice :: Ptr InputDevice
    , eventPointerButtonTime :: Word32
    , eventPointerButtonButton :: Word32
    , eventPointerButtonState :: ButtonState
    } deriving (Show, Eq)

instance Storable WlrEventPointerButton where
    sizeOf _ = #{size struct wlr_event_pointer_button}
    alignment _ = #{alignment struct wlr_event_pointer_button}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_button, device} ptr
        button <- #{peek struct wlr_event_pointer_button, button} ptr

        state :: CInt <- #{peek struct wlr_event_pointer_button, state} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_button, time_msec} ptr

        pure $ WlrEventPointerButton 
            dev
            (fromIntegral tsec)
            button
            (intToButtonState state)
    poke ptr event = do
        #{poke struct wlr_event_pointer_button, device} ptr $ eventPointerButtonDevice event
        #{poke struct wlr_event_pointer_button, button} ptr $ eventPointerButtonButton event
        let state :: CInt = buttonStateToInt $ eventPointerButtonState event
        #{poke struct wlr_event_pointer_button, state} ptr state
        let tsec :: Word32 = fromIntegral $ eventPointerButtonTime event
        #{poke struct wlr_event_pointer_button, time_msec} ptr tsec

data WlrEventPointerMotion = WlrEventPointerMotion
    { eventPointerMotionDevice :: Ptr InputDevice
    , eventPointerMotionTime :: Word32
    , eventPointerMotionDeltaX :: Double
    , eventPointerMotionDeltaY :: Double
    } deriving (Show, Eq)

instance Storable WlrEventPointerMotion where
    sizeOf _ = #{size struct wlr_event_pointer_motion}
    alignment _ = #{alignment struct wlr_event_pointer_motion}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_motion, device} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_motion, time_msec} ptr
        deltax <- #{peek struct wlr_event_pointer_motion, delta_x} ptr
        deltay <- #{peek struct wlr_event_pointer_motion, delta_y} ptr

        pure $ WlrEventPointerMotion
            dev
            (fromIntegral tsec)
            deltax
            deltay
    poke ptr event = do
        #{poke struct wlr_event_pointer_motion, device} ptr $ eventPointerMotionDevice event
        let tsec :: Word32 = fromIntegral $ eventPointerMotionTime event
        #{poke struct wlr_event_pointer_motion, time_msec} ptr tsec

        #{poke struct wlr_event_pointer_motion, delta_x} ptr $ eventPointerMotionDeltaX event
        #{poke struct wlr_event_pointer_motion, delta_y} ptr $ eventPointerMotionDeltaY event


data WlrEventPointerAbsMotion = WlrEventPointerAbsMotion
    { eventPointerAbsMotionDevice :: Ptr InputDevice
    , eventPointerAbsMotionTime :: Word32
    , eventPointerAbsMotionX :: Double
    , eventPointerAbsMotionY :: Double
    } deriving (Show, Eq)

instance Storable WlrEventPointerAbsMotion where
    sizeOf _ = #{size struct wlr_event_pointer_motion_absolute}
    alignment _ = #{alignment struct wlr_event_pointer_motion_absolute}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_motion_absolute, device} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_motion_absolute, time_msec} ptr
        x <- #{peek struct wlr_event_pointer_motion_absolute, x} ptr
        y <- #{peek struct wlr_event_pointer_motion_absolute, y} ptr

        pure $ WlrEventPointerAbsMotion
            dev
            (fromIntegral tsec)
            x
            y

    poke ptr event = do
        #{poke struct wlr_event_pointer_motion_absolute, device} ptr $ eventPointerAbsMotionDevice event
        let tsec :: Word32 = fromIntegral $ eventPointerAbsMotionTime event
        #{poke struct wlr_event_pointer_motion_absolute, time_msec} ptr tsec

        #{poke struct wlr_event_pointer_motion_absolute, x} ptr $ eventPointerAbsMotionX event
        #{poke struct wlr_event_pointer_motion_absolute, y} ptr $ eventPointerAbsMotionY event


data AxisSource
    = AxisWheel
    | AxisFinger
    | AxisContinuous
    | AxisWheelTilt
    deriving (Show, Eq, Read)

axisSToInt :: Num a => AxisSource -> a
axisSToInt AxisWheel      = #{const WLR_AXIS_SOURCE_WHEEL}
axisSToInt AxisFinger     = #{const WLR_AXIS_SOURCE_FINGER}
axisSToInt AxisContinuous = #{const WLR_AXIS_SOURCE_CONTINUOUS}
axisSToInt AxisWheelTilt  = #{const WLR_AXIS_SOURCE_WHEEL_TILT}

intToAxisS :: (Eq a, Num a, Show a) => a -> AxisSource
intToAxisS #{const WLR_AXIS_SOURCE_WHEEL}      = AxisWheel
intToAxisS #{const WLR_AXIS_SOURCE_FINGER}     = AxisFinger
intToAxisS #{const WLR_AXIS_SOURCE_CONTINUOUS}  = AxisContinuous
intToAxisS #{const WLR_AXIS_SOURCE_WHEEL_TILT} = AxisWheelTilt
intToAxisS x = error $ "Got an an unknown PadRingSource: " ++ show x

instance Storable AxisSource where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToAxisS :: CInt -> AxisSource) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (axisSToInt val :: CInt)


data AxisOrientation
    = AxisVertical
    | AxisHorizontal
    deriving (Show, Eq, Read)

axisOToInt :: Num a => AxisOrientation -> a
axisOToInt AxisVertical   = #{const WLR_AXIS_ORIENTATION_VERTICAL}
axisOToInt AxisHorizontal = #{const WLR_AXIS_ORIENTATION_HORIZONTAL}

intToAxisO :: (Eq a, Num a, Show a) => a -> AxisOrientation
intToAxisO #{const WLR_AXIS_ORIENTATION_VERTICAL}   = AxisVertical
intToAxisO #{const WLR_AXIS_ORIENTATION_HORIZONTAL} = AxisHorizontal
intToAxisO x = error $ "Got an an unknown PadRingSource: " ++ show x

instance Storable AxisOrientation where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToAxisO :: CInt -> AxisOrientation) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (axisOToInt val :: CInt)

data WlrEventPointerAxis = WlrEventPointerAxis
    { eventPointerAxisDevice      :: Ptr InputDevice
    , eventPointerAxisTime        :: Word32
    , eventPointerAxisSource      :: AxisSource
    , eventPointerAxisOrientation :: AxisOrientation
    , eventPointerAxisDelta       :: Double
    , eventPointerAxisDiscrete    :: Int32
    } deriving (Show, Eq)



instance Storable WlrEventPointerAxis where
    sizeOf _ = #{size struct wlr_event_pointer_axis}
    alignment _ = #{alignment struct wlr_event_pointer_axis}
    peek ptr = WlrEventPointerAxis
        <$> #{peek struct wlr_event_pointer_axis, device} ptr
        <*> #{peek struct wlr_event_pointer_axis, time_msec} ptr
        <*> #{peek struct wlr_event_pointer_axis, source} ptr
        <*> #{peek struct wlr_event_pointer_axis, orientation} ptr
        <*> #{peek struct wlr_event_pointer_axis, delta} ptr
        <*> #{peek struct wlr_event_pointer_axis, delta_discrete} ptr
    poke ptr event = do
        #{poke struct wlr_event_pointer_axis, device} ptr      $ eventPointerAxisDevice      event
        #{poke struct wlr_event_pointer_axis, time_msec} ptr   $ eventPointerAxisTime        event
        #{poke struct wlr_event_pointer_axis, source} ptr      $ eventPointerAxisSource      event
        #{poke struct wlr_event_pointer_axis, orientation} ptr $ eventPointerAxisOrientation event
        #{poke struct wlr_event_pointer_axis, delta} ptr       $ eventPointerAxisDelta       event
        #{poke struct wlr_event_pointer_axis, delta_discrete} ptr       $ eventPointerAxisDiscrete       event
