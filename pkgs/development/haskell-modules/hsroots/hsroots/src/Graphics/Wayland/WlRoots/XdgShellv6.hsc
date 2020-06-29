{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XdgShellv6
    ( WlrXdgShell
    , xdgShellCreate
    , xdgShellDestroy

    , WlrXdgSurface
    , xdgSurfaceGetSurface
    , getXdgToplevel

    , WlrXdgToplevel
    , WlrXdgToplevelEvents
    , getXdgToplevelEvents

    , MoveEvent (..)
    , ResizeEvent (..)
    , MenuEvent (..)
    , FullscreenEvent (..)
    , WlrXdgSurfaceEvents (..)
    , getXdgSurfaceEvents
    , getXdgSurfaceDataPtr

    , sendClose
    , setSize
    , getGeometry
    , setActivated
    , setMaximized

    , getPopups
    , isXdgPopup
    , isConfigured

    , getPopupGeometry
    , xdgSurfaceAt

    , getTitle
    , getAppId

    , WlrXdgPopup
    , xdgPopupGetBase
    , xdgGetPopupSurfaces
    , getPopupPositioner

    , XdgPosAnchor (..), XdgPosGravity (..), XdgPosAdjust (..)
    , WlrXdgPositioner

    , getPositionerGeometry
    , getPopupAnchor
    , unconstrainPopup
    , getConfigureSerial
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_xdg_shell_v6.h>

import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    )

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Seat (WlrSeatClient)
import Graphics.Wayland.List (getListFromHead)

import Graphics.Wayland.Signal
import Utility (textFromNull)
import Control.Monad (when)

data WlrXdgShell

foreign import ccall unsafe "wlr_xdg_shell_v6_create" c_shell_create :: Ptr DisplayServer -> IO (Ptr WlrXdgShell)

xdgShellCreate :: (Ptr WlrXdgSurface -> IO ()) -> DisplayServer -> IO (Ptr WlrXdgShell)
xdgShellCreate new (DisplayServer ptr) = do
    shell <- throwErrnoIfNull "shellCreate" $ c_shell_create ptr

    let signal = #{ptr struct wlr_xdg_shell_v6, events.new_surface} shell
    handler <- addListener (WlListener new) signal
    sptr <- newStablePtr handler
    poke (#{ptr struct wlr_xdg_shell_v6, data} shell) (castStablePtrToPtr sptr)

    pure shell

foreign import ccall unsafe "wlr_xdg_shell_v6_destroy" c_shell_destroy :: Ptr WlrXdgShell -> IO ()

xdgShellDestroy :: Ptr WlrXdgShell -> IO ()
xdgShellDestroy = c_shell_destroy

data WlrXdgSurface
data WlrXdgToplevel

isConfigured :: Ptr WlrXdgSurface -> IO Bool
isConfigured = #{peek struct wlr_xdg_surface_v6, configured}

isXdgPopup :: Ptr WlrXdgSurface -> IO Bool
isXdgPopup surf = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    pure (role == #{const WLR_XDG_SURFACE_V6_ROLE_POPUP})

xdgSurfaceGetSurface :: Ptr WlrXdgSurface -> IO (Maybe (Ptr WlrSurface))
xdgSurfaceGetSurface ptr = do
    ret <- #{peek struct wlr_xdg_surface_v6, surface} ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

data MoveEvent = MoveEvent
    { moveEvtSurface :: Ptr WlrXdgSurface
    , moveEvtSeat    :: Ptr WlrSeatClient
    , moveEvtSerial  :: Word32
    }

instance Storable MoveEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_move_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_move_event}
    peek ptr = MoveEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_move_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_move_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_move_event, serial} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_move_event, surface} ptr $ moveEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_move_event, seat} ptr $ moveEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_move_event, serial} ptr $ moveEvtSerial evt


data ResizeEvent = ResizeEvent
    { resizeEvtSurface :: Ptr WlrXdgSurface
    , resizeEvtSeat    :: Ptr WlrSeatClient
    , resizeEvtSerial  :: Word32
    , resizeEvtEdges   :: Word32 -- TODO: Make this a [Edge]
    }

instance Storable ResizeEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_resize_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_resize_event}
    peek ptr = ResizeEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_resize_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, serial} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, edges} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_resize_event, surface} ptr $ resizeEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, seat} ptr $ resizeEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, serial} ptr $ resizeEvtSerial evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, edges} ptr $ resizeEvtEdges evt

data MenuEvent = MenuEvent
    { menuEvtSurface :: Ptr WlrXdgSurface
    , menuEvtSeat    :: Ptr WlrSeatClient
    , menuEvtSerial  :: Word32
    , menuEvtX       :: Word32
    , menuEvtY       :: Word32
    }

instance Storable MenuEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_show_window_menu_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_show_window_menu_event}
    peek ptr = MenuEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, serial} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, x} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, y} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, surface} ptr $ menuEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, seat} ptr $ menuEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, serial} ptr $ menuEvtSerial evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, x} ptr $ menuEvtX evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, y} ptr $ menuEvtY evt

data FullscreenEvent = FullscreenEvent
    { fullscreenEvtSurface :: Ptr WlrXdgSurface
    , fullscreenEvtFull    :: Bool
    , fullscreenEvtOutput  :: Ptr WlrOutput
    }

instance Storable FullscreenEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_set_fullscreen_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_set_fullscreen_event}
    peek ptr = FullscreenEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, fullscreen} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, output} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, surface} ptr $ fullscreenEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, fullscreen} ptr $ fullscreenEvtFull evt
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, output} ptr $ fullscreenEvtOutput evt

data WlrXdgSurfaceEvents = WlrXdgSurfaceEvents
    { xdgSurfaceEvtCommit  :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtDestroy :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtTimeout :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtPopup   :: Ptr (WlSignal WlrXdgPopup)
    , xdgSurfaceEvtMap     :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtUnmap   :: Ptr (WlSignal WlrXdgSurface)
    }

getXdgSurfaceEvents :: Ptr WlrXdgSurface -> WlrXdgSurfaceEvents
getXdgSurfaceEvents ptr = WlrXdgSurfaceEvents
    { xdgSurfaceEvtDestroy = #{ptr struct wlr_xdg_surface_v6, events.destroy} ptr
    , xdgSurfaceEvtCommit = #{ptr struct wlr_xdg_surface_v6, events.destroy} ptr
    , xdgSurfaceEvtTimeout = #{ptr struct wlr_xdg_surface_v6, events.ping_timeout} ptr
    , xdgSurfaceEvtPopup = #{ptr struct wlr_xdg_surface_v6, events.new_popup} ptr
    , xdgSurfaceEvtMap = #{ptr struct wlr_xdg_surface_v6, events.map} ptr
    , xdgSurfaceEvtUnmap = #{ptr struct wlr_xdg_surface_v6, events.unmap} ptr
    }

data WlrXdgToplevelEvents = WlrXdgToplevelEvents
    { xdgToplevelEvtMaximize   :: Ptr (WlSignal WlrXdgSurface)
    , xdgToplevelEvtFullscreen :: Ptr (WlSignal FullscreenEvent)
    , xdgToplevelEvtMinimize   :: Ptr (WlSignal WlrXdgSurface)

    , xdgToplevelEvtMove   :: Ptr (WlSignal MoveEvent)
    , xdgToplevelEvtResize :: Ptr (WlSignal ResizeEvent)
    , xdgToplevelEvtMenu   :: Ptr (WlSignal MenuEvent)
    }

getXdgToplevelEvents :: Ptr WlrXdgToplevel -> WlrXdgToplevelEvents
getXdgToplevelEvents ptr = WlrXdgToplevelEvents
    { xdgToplevelEvtMaximize = #{ptr struct wlr_xdg_toplevel_v6, events.request_maximize} ptr
    , xdgToplevelEvtFullscreen = #{ptr struct wlr_xdg_toplevel_v6, events.request_fullscreen} ptr
    , xdgToplevelEvtMinimize = #{ptr struct wlr_xdg_toplevel_v6, events.request_minimize} ptr

    , xdgToplevelEvtMove = #{ptr struct wlr_xdg_toplevel_v6, events.request_move} ptr
    , xdgToplevelEvtResize = #{ptr struct wlr_xdg_toplevel_v6, events.request_resize} ptr
    , xdgToplevelEvtMenu = #{ptr struct wlr_xdg_toplevel_v6, events.request_show_window_menu} ptr
    }

getXdgToplevel :: Ptr WlrXdgSurface -> IO (Maybe (Ptr WlrXdgToplevel))
getXdgToplevel ptr = do
    ret <- #{peek struct wlr_xdg_surface_v6, toplevel} ptr
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} ptr
    pure $ if role /= #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL} || ret == nullPtr
        then Nothing
        else Just ret

getXdgSurfaceDataPtr :: Ptr WlrXdgSurface -> Ptr (Ptr ())
getXdgSurfaceDataPtr = #{ptr struct wlr_xdg_surface_v6, data}

foreign import ccall "wlr_xdg_surface_v6_surface_at" c_surface_at :: Ptr WlrXdgSurface -> Double -> Double -> Ptr Double -> Ptr Double -> IO (Ptr WlrSurface)

xdgSurfaceAt :: Ptr WlrXdgSurface -> Double -> Double -> IO (Maybe (Ptr WlrSurface, Double, Double))
xdgSurfaceAt surf x y = alloca $ \xptr -> alloca $ \yptr -> do
    popup <- c_surface_at surf x y xptr yptr
    if popup == nullPtr
        then pure Nothing
        else do
            newx <- peek xptr
            newy <- peek yptr
            pure $ Just (popup, newx, newy)


foreign import ccall "wlr_xdg_surface_v6_send_close" c_close :: Ptr WlrXdgSurface -> IO ()

sendClose :: Ptr WlrXdgSurface -> IO ()
sendClose surf = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    case role of
        #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL} -> c_close surf
        _ -> pure ()

foreign import ccall "wlr_xdg_surface_v6_get_geometry" c_get_geometry :: Ptr WlrXdgSurface -> Ptr WlrBox -> IO ()

getGeometry :: Ptr WlrXdgSurface -> IO WlrBox
getGeometry ptr = alloca $ \bPtr -> do
    c_get_geometry ptr bPtr
    peek bPtr

foreign import ccall "wlr_xdg_toplevel_v6_set_size" c_set_size :: Ptr WlrXdgSurface -> Word32 -> Word32 -> IO Word32

setSize :: Ptr WlrXdgSurface -> Word32 -> Word32 -> IO Word32
setSize surf width height = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    case role of
        #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL} -> c_set_size surf width height
        _ -> pure 0

getConfigureSerial :: Ptr WlrXdgSurface -> IO Word32
getConfigureSerial = #{peek struct wlr_xdg_surface_v6, configure_serial}


foreign import ccall "wlr_xdg_toplevel_v6_set_activated" c_activate :: Ptr WlrXdgSurface -> Bool -> IO ()

setActivated :: Ptr WlrXdgSurface -> Bool -> IO ()
setActivated surf active = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_activate surf active)



foreign import ccall "wlr_xdg_toplevel_v6_set_maximized" c_maximize :: Ptr WlrXdgSurface -> Bool -> IO ()

setMaximized :: Ptr WlrXdgSurface -> Bool -> IO ()
setMaximized surf maximized = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_maximize surf maximized)


getPopups :: Ptr WlrXdgSurface -> IO [Ptr WlrXdgPopup]
getPopups surf = do
    let list = #{ptr struct wlr_xdg_surface_v6, popups} surf
    getListFromHead list #{offset struct wlr_xdg_popup_v6, link}

xdgPopupGetBase :: Ptr WlrXdgPopup -> IO (Ptr WlrXdgSurface)
xdgPopupGetBase = #{peek struct wlr_xdg_popup_v6, base}

xdgGetPopupSurfaces :: Ptr WlrXdgSurface -> IO [Ptr WlrXdgSurface]
xdgGetPopupSurfaces surf =
    mapM xdgPopupGetBase =<< getPopups surf

data WlrXdgPopup

getPopupState :: Ptr WlrXdgSurface -> IO (Maybe (Ptr WlrXdgPopup))
getPopupState surf = do
    ret <- #{peek struct wlr_xdg_surface_v6, popup} surf
    if ret == nullPtr
        then pure Nothing
        else pure $ Just ret


getPopupGeometry :: Ptr WlrXdgSurface -> IO (Maybe WlrBox)
getPopupGeometry surf = traverse #{peek struct wlr_xdg_popup_v6, geometry} =<< getPopupState surf

getPopupPositioner :: Ptr WlrXdgPopup -> Ptr WlrXdgPositioner
getPopupPositioner = #{ptr struct wlr_xdg_popup_v6, positioner}

getTitle :: Ptr WlrXdgToplevel -> IO (Maybe Text)
getTitle ptr = textFromNull =<< #{peek struct wlr_xdg_toplevel_v6, title} ptr

getAppId :: Ptr WlrXdgToplevel -> IO (Maybe Text)
getAppId ptr = textFromNull =<< #{peek struct wlr_xdg_toplevel_v6, app_id} ptr


data XdgPosAnchor
    = AnchorNone
    | AnchorTop
    | AnchorBottom
    | AnchorLeft
    | AnchorRight
    deriving (Eq, Show)

data XdgPosGravity
    = GravityNone
    | GravityTop
    | GravityBottom
    | GravityLeft
    | GravityRight
    deriving (Eq, Show)

data XdgPosAdjust
    = AdjustNone
    | AdjustSlideX
    | AdjustSlideY
    | AdjustFlipX
    | AdjustFlipY
    | AdjustResizeX
    | AdjustResizeY
    deriving (Eq, Show)

data WlrXdgPositioner

foreign import ccall unsafe "wlr_xdg_positioner_v6_get_geometry_c" c_positioner_get_geometry :: Ptr WlrXdgPositioner -> Ptr WlrBox -> IO ()

getPositionerGeometry :: Ptr WlrXdgPositioner -> IO WlrBox
getPositionerGeometry pos = alloca $ \bPtr -> do
    c_positioner_get_geometry pos bPtr
    peek bPtr


foreign import ccall unsafe "wlr_xdg_popup_v6_get_anchor_point" c_popup_get_anchor_point :: Ptr WlrXdgPopup -> Ptr CInt -> Ptr CInt -> IO ()

getPopupAnchor :: Ptr WlrXdgPopup -> IO (Int, Int)
getPopupAnchor pop = alloca $ \xPtr -> alloca $ \yPtr -> do
    c_popup_get_anchor_point pop xPtr yPtr
    x <- peek xPtr
    y <- peek yPtr
    pure (fromIntegral x, fromIntegral y)


foreign import ccall unsafe "wlr_xdg_popup_v6_unconstrain_from_box" c_popup_unconstrain_from_box :: Ptr WlrXdgPopup -> Ptr WlrBox -> IO ()

-- | Box in popups root toplevel coordinates
unconstrainPopup :: Ptr WlrXdgPopup -> WlrBox -> IO ()
unconstrainPopup pop box = with box $ c_popup_unconstrain_from_box pop
