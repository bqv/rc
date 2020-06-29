module Graphics.Wayland.WlRoots.Tabletv2
    ( TabletManagerv2 (..)
    , createTabletManagerv2

    , Tabletv2 (..)
    , createTabletv2

    , TabletPadv2 (..)
    , createTabletPadv2

    , TabletToolv2 (..)
    , createTabletToolv2

    , surfaceAcceptsTablet

    , sendTabletPadEnter
    , sendTabletPadLeave
    , sendTabletToolProximityIn
    , sendTabletToolProximityOut
    )
where


#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_tablet_v2.h>

import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.WlRoots.Global
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.TabletTool (WlrTabletTool (..))
import Graphics.Wayland.WlRoots.Seat (WlrSeat)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)


newtype TabletManagerv2 = TabletManagerv2 (Ptr TabletManagerv2)

foreign import ccall unsafe "wlr_tablet_v2_create" c_create_manager :: Ptr DisplayServer -> IO (Ptr TabletManagerv2)
foreign import ccall safe "wlr_tablet_v2_destroy" c_destroy_manager :: Ptr TabletManagerv2 -> IO ()

createTabletManagerv2 :: DisplayServer -> IO TabletManagerv2
createTabletManagerv2 (DisplayServer dsp) =
    TabletManagerv2 <$> throwErrnoIfNull "createTabletManagerv2" (c_create_manager dsp)

instance GlobalWrapper TabletManagerv2 where
    getGlobal (TabletManagerv2 ptr) = #{peek struct wlr_tablet_manager_v2, wl_global} ptr
    removeGlobal (TabletManagerv2 ptr) = c_destroy_manager ptr


newtype Tabletv2 = Tabletv2 (Ptr Tabletv2) deriving (Eq, Ord, Show)

foreign import ccall unsafe "wlr_tablet_create" c_create_tablet :: Ptr TabletManagerv2 -> Ptr WlrSeat -> Ptr InputDevice -> IO (Ptr Tabletv2)
createTabletv2 :: TabletManagerv2 -> Ptr WlrSeat -> Ptr InputDevice -> IO Tabletv2
createTabletv2 (TabletManagerv2 mgr) seat iDev =
    Tabletv2 <$> throwErrnoIfNull "createTabletv2" (c_create_tablet mgr seat iDev)


newtype TabletPadv2 = TabletPadv2 (Ptr TabletPadv2) deriving (Eq, Ord, Show)

foreign import ccall unsafe "wlr_tablet_pad_create" c_create_tablet_pad :: Ptr TabletManagerv2 -> Ptr WlrSeat -> Ptr InputDevice -> IO (Ptr TabletPadv2)
createTabletPadv2 :: TabletManagerv2 -> Ptr WlrSeat -> Ptr InputDevice -> IO TabletPadv2
createTabletPadv2 (TabletManagerv2 mgr) seat iDev =
    TabletPadv2 <$> throwErrnoIfNull "createTabletPadv2" (c_create_tablet_pad mgr seat iDev)

newtype TabletToolv2 = TabletToolv2 (Ptr TabletToolv2) deriving (Eq, Ord, Show)

foreign import ccall unsafe "wlr_tablet_tool_create" c_create_tablet_tool :: Ptr TabletManagerv2 -> Ptr WlrSeat -> Ptr WlrTabletTool -> IO (Ptr TabletToolv2)
createTabletToolv2 :: TabletManagerv2 -> Ptr WlrSeat -> WlrTabletTool -> IO TabletToolv2
createTabletToolv2 (TabletManagerv2 mgr) seat (WlrTabletTool tool) =
    TabletToolv2 <$> throwErrnoIfNull "createTabletToolv2" (c_create_tablet_tool mgr seat tool)


foreign import ccall unsafe "wlr_surface_accepts_tablet_v2" c_accepts_tablet :: Ptr Tabletv2 -> Ptr WlrSurface -> IO Word8
surfaceAcceptsTablet :: Tabletv2 -> Ptr WlrSurface -> IO Bool
surfaceAcceptsTablet (Tabletv2 tablet) surf = (/= 0) <$> c_accepts_tablet tablet surf

foreign import ccall "wlr_send_tablet_v2_tablet_pad_enter" c_send_pad_enter :: Ptr TabletPadv2 -> Ptr Tabletv2 -> Ptr WlrSurface -> IO Word32
sendTabletPadEnter :: TabletPadv2 -> Tabletv2 -> Ptr WlrSurface -> IO Word32
sendTabletPadEnter (TabletPadv2 pad) (Tabletv2 tablet) surf = c_send_pad_enter pad tablet surf

foreign import ccall "wlr_send_tablet_v2_tablet_pad_leave" c_send_pad_leave :: Ptr TabletPadv2 -> Ptr WlrSurface -> IO Word32
sendTabletPadLeave :: TabletPadv2 -> Ptr WlrSurface -> IO Word32
sendTabletPadLeave (TabletPadv2 pad) surf = c_send_pad_leave pad surf


foreign import ccall "wlr_send_tablet_v2_tablet_tool_proximity_in" c_send_proximity_in :: Ptr TabletToolv2 -> Ptr Tabletv2 -> Ptr WlrSurface -> IO ()
sendTabletToolProximityIn :: TabletToolv2 -> Tabletv2 -> Ptr WlrSurface -> IO ()
sendTabletToolProximityIn (TabletToolv2 tool) (Tabletv2 tablet) surf = c_send_proximity_in tool tablet surf

foreign import ccall "wlr_send_tablet_v2_tablet_tool_proximity_out" c_send_proximity_out :: Ptr TabletToolv2 -> IO ()
sendTabletToolProximityOut :: TabletToolv2 -> IO ()
sendTabletToolProximityOut (TabletToolv2 tool) = c_send_proximity_out tool
