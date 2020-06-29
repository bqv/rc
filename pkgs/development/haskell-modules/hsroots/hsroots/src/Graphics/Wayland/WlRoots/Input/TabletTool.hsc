{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.TabletTool
    ( WlrTabletTool (..)

    , peekTabletToolData
    , pokeTabletToolData

    , TabletToolEvents (..)
    , getTabletToolEvents
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_tablet_tool.h>

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable

import Graphics.Wayland.Signal (WlSignal)

newtype WlrTabletTool = WlrTabletTool {unWlrTabletTool :: Ptr WlrTabletTool} deriving (Eq, Show)


peekTabletToolData :: WlrTabletTool -> IO (Ptr a)
peekTabletToolData (WlrTabletTool ptr) = #{peek struct wlr_tablet_tool, data} ptr

pokeTabletToolData :: WlrTabletTool -> Ptr a -> IO ()
pokeTabletToolData (WlrTabletTool ptr) = #{poke struct wlr_tablet_tool, data} ptr


newtype TabletToolEvents = TabletToolEvents
    { tabletToolEventDestroy :: Ptr (WlSignal WlrTabletTool) }


getTabletToolEvents :: WlrTabletTool -> TabletToolEvents
getTabletToolEvents (WlrTabletTool ptr) = TabletToolEvents
    { tabletToolEventDestroy = #{ptr struct wlr_tablet_tool, events.destroy} ptr
    }
