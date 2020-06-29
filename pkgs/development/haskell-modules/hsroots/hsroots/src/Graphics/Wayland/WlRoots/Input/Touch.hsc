module Graphics.Wayland.WlRoots.Input.Touch
    ( WlrTouch (..)
    , WlrTouchEvents (..)
    , getWlrTouchEvents

    , WlrTouchDown (..)
    , WlrTouchUp (..)
    , WlrTouchMotion (..)
    , WlrTouchCancel (..)
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_touch.h>

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Signal (WlSignal)

import {-# SOURCE #-} Graphics.Wayland.WlRoots.Input (InputDevice)

newtype WlrTouch = WlrTouch { unTouch :: Ptr WlrTouch }

data WlrTouchEvents = WlrTouchEvents
    { wlrTouchEvtDown   :: {-# UNPACK #-} !(Ptr (WlSignal WlrTouchDown))
    , wlrTouchEvtUp     :: {-# UNPACK #-} !(Ptr (WlSignal WlrTouchUp))
    , wlrTouchEvtMotion :: {-# UNPACK #-} !(Ptr (WlSignal WlrTouchMotion))
    , wlrTouchEvtCancel :: {-# UNPACK #-} !(Ptr (WlSignal WlrTouchCancel))
    }

getWlrTouchEvents :: WlrTouch -> WlrTouchEvents
getWlrTouchEvents (WlrTouch ptr) = WlrTouchEvents
    { wlrTouchEvtDown   = #{ptr struct wlr_touch, events.down} ptr
    , wlrTouchEvtUp     = #{ptr struct wlr_touch, events.up} ptr
    , wlrTouchEvtCancel = #{ptr struct wlr_touch, events.cancel} ptr
    , wlrTouchEvtMotion = #{ptr struct wlr_touch, events.motion} ptr
    }

data WlrTouchDown = WlrTouchDown
    { wlrTouchDownDev    :: {-# UNPACK #-} !(Ptr InputDevice)
    , wlrTouchDownMSec   :: {-# UNPACK #-} !Word32
    , wlrTouchDownId     :: {-# UNPACK #-} !Int32
    , wlrTouchDownX      :: {-# UNPACK #-} !Double
    , wlrTouchDownY      :: {-# UNPACK #-} !Double
    } deriving (Show)

instance Storable WlrTouchDown where
    sizeOf _ = #{size struct wlr_event_touch_down}
    alignment _ = #{alignment struct wlr_event_touch_down}
    peek ptr = WlrTouchDown
        <$> #{peek struct wlr_event_touch_down, device} ptr
        <*> #{peek struct wlr_event_touch_down, time_msec} ptr
        <*> #{peek struct wlr_event_touch_down, touch_id} ptr
        <*> #{peek struct wlr_event_touch_down, x} ptr
        <*> #{peek struct wlr_event_touch_down, y} ptr
    poke _ _ = error "We don't poke events (for now)"

data WlrTouchUp = WlrTouchUp
    { wlrTouchUpDev  :: {-# UNPACK #-} !(Ptr InputDevice)
    , wlrTouchUpMSec :: {-# UNPACK #-} !Word32
    , wlrTouchUpId   :: {-# UNPACK #-} !Int32
    } deriving (Show)

instance Storable WlrTouchUp where
    sizeOf _ = #{size struct wlr_event_touch_up}
    alignment _ = #{alignment struct wlr_event_touch_up}
    peek ptr = WlrTouchUp
        <$> #{peek struct wlr_event_touch_down, device} ptr
        <*> #{peek struct wlr_event_touch_down, time_msec} ptr
        <*> #{peek struct wlr_event_touch_down, touch_id} ptr
    poke _ _ = error "We don't poke events (for now)"

data WlrTouchMotion = WlrTouchMotion
    { wlrTouchMotionDev    :: {-# UNPACK #-} !(Ptr InputDevice)
    , wlrTouchMotionMSec   :: {-# UNPACK #-} !Word32
    , wlrTouchMotionId     :: {-# UNPACK #-} !Int32
    , wlrTouchMotionX      :: {-# UNPACK #-} !Double
    , wlrTouchMotionY      :: {-# UNPACK #-} !Double
    } deriving (Show)

instance Storable WlrTouchMotion where
    sizeOf _ = #{size struct wlr_event_touch_motion}
    alignment _ = #{alignment struct wlr_event_touch_motion}
    peek ptr = WlrTouchMotion
        <$> #{peek struct wlr_event_touch_motion, device} ptr
        <*> #{peek struct wlr_event_touch_motion, time_msec} ptr
        <*> #{peek struct wlr_event_touch_motion, touch_id} ptr
        <*> #{peek struct wlr_event_touch_motion, x} ptr
        <*> #{peek struct wlr_event_touch_motion, y} ptr
    poke _ _ = error "We don't poke events (for now)"


data WlrTouchCancel = WlrTouchCancel
    { wlrTouchCancelDev  :: {-# UNPACK #-} !(Ptr InputDevice)
    , wlrTouchCancelMSec :: {-# UNPACK #-} !Word32
    , wlrTouchCancelId   :: {-# UNPACK #-} !Int32
    } deriving (Show)

instance Storable WlrTouchCancel where
    sizeOf _ = #{size struct wlr_event_touch_cancel}
    alignment _ = #{alignment struct wlr_event_touch_cancel}
    peek ptr = WlrTouchCancel
        <$> #{peek struct wlr_event_touch_cancel, device} ptr
        <*> #{peek struct wlr_event_touch_cancel, time_msec} ptr
        <*> #{peek struct wlr_event_touch_cancel, touch_id} ptr
    poke _ _ = error "We don't poke events (for now)"
