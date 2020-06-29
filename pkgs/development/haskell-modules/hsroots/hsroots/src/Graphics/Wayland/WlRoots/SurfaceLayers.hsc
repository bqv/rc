{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.SurfaceLayers
    ( LayerShell (..)
    , LayerShellEvents (..)
    , SurfaceState (..)
    , LayerSurface (..)
    , LayerShellLayer (..)
    , getLayerShellEvents
    , layerShellCreate
    , layerShellDestroy

    , configureSurface
    , closeSurface

    , getLayerSurfaceLayer
    , LayerSurfaceEvents (..)
    , getLayerSurfaceEvents

    , getSurfaceState

    , Anchor (..)
    , getAnchorValue
    , getMainAnchor
    , useHeight, useWidth
    , getSurfaceOutput
    , setSurfaceOutput
    , getLayerSurfaceSurface

    , Corner (..), getAnchorCorner

    , getPopups
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_xdg_shell.h>

import Data.Bits (Bits((.&.)))
import Data.Word (Word32)
import Data.Int (Int32)
import Foreign.C.Types (CInt)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable

import Graphics.Wayland.Server (DisplayServer (..))

import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

import Graphics.Wayland.WlRoots.XdgShell (WlrXdgPopup)

data LayerShellLayer
    = LayerShellLayerBackground
    | LayerShellLayerBottom
    | LayerShellLayerTop
    | LayerShellLayerOverlay
    deriving (Eq, Show, Ord)

data Anchor
    = AnchorTop
    | AnchorBottom
    | AnchorLeft
    | AnchorRight
    deriving (Eq, Show)

useHeight :: SurfaceState -> Word32 -> Word32
useHeight state box =
    let full = box - surfaceStateMarginBottom state - surfaceStateMarginTop state
     in case surfaceStateAnchor state .&. #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP} of
        #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP} -> full
        _ -> min full (surfaceStateDesiredHeight state)

useWidth :: SurfaceState -> Word32 -> Word32
useWidth state box =
    let full = box - surfaceStateMarginLeft state - surfaceStateMarginTop state
     in case surfaceStateAnchor state .&. #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT} of
        #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT} -> full
        _ -> min full (surfaceStateDesiredWidth state)

getMainAnchor :: (Num a, Eq a) => a -> Maybe Anchor
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM} = Just AnchorBottom
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP} = Just AnchorTop
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT} = Just AnchorLeft
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT} = Just AnchorRight
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT | ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT} = Just AnchorBottom
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT | ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT} = Just AnchorTop
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT | ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP} = Just AnchorLeft
getMainAnchor #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT | ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP} = Just AnchorRight
getMainAnchor _ = Nothing

data Corner
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight
    deriving (Eq, Show)

getAnchorCorner :: (Num a, Eq a) => a -> Maybe Corner
getAnchorCorner #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP | ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT} = Just TopLeft
getAnchorCorner #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT} = Just TopRight
getAnchorCorner #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT} = Just BottomLeft
getAnchorCorner #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT} = Just BottomRight
getAnchorCorner _ = Nothing



getAnchorValue :: Num a => Anchor -> a
getAnchorValue AnchorBottom = #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM}
getAnchorValue AnchorTop = #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP}
getAnchorValue AnchorLeft = #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT}
getAnchorValue AnchorRight = #{const ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT}


data LayerShell = LayerShell { unLS :: Ptr LayerShell }
data LayerSurface = LayerSurface { unLSS :: Ptr LayerSurface } deriving (Eq, Show, Ord)

data LayerShellEvents = LayerShellEvents
    { layerShellEventsSurface :: Ptr (WlSignal LayerSurface)
    }

getLayerShellEvents :: LayerShell -> LayerShellEvents
getLayerShellEvents (LayerShell ptr) = LayerShellEvents
    { layerShellEventsSurface = #{ptr struct wlr_layer_shell_v1, events.new_surface} ptr
    }

data SurfaceState = SurfaceState
    { surfaceStateAnchor :: Word32 -- TODO: Make list of enum
    , surfaceStateExclusive :: Int32
    , surfaceStateMarginTop :: Word32
    , surfaceStateMarginBottom :: Word32
    , surfaceStateMarginLeft :: Word32
    , surfaceStateMarginRight :: Word32
    , surfaceStateKeyboard :: Bool
    , surfaceStateDesiredWidth :: Word32
    , surfaceStateDesiredHeight :: Word32
    , surfaceStateActualWidth :: Word32
    , surfaceStateActualHeight :: Word32
    , surfaceStateLayer :: LayerShellLayer
    }

foreign import ccall unsafe "wlr_layer_shell_v1_create" c_create :: Ptr DisplayServer -> IO (Ptr LayerShell)

layerShellCreate :: DisplayServer -> IO LayerShell
layerShellCreate (DisplayServer dsp) = LayerShell <$>
    throwErrnoIfNull "layerShellCreate" (c_create dsp)

foreign import ccall "wlr_layer_shell_v1_destroy" c_destroy :: Ptr LayerShell -> IO ()

layerShellDestroy :: LayerShell -> IO ()
layerShellDestroy = c_destroy . unLS

foreign import ccall unsafe "wlr_layer_surface_v1_configure" c_configure :: Ptr LayerSurface -> Word32 -> Word32 -> IO ()

configureSurface :: LayerSurface -> Word32 -> Word32 -> IO ()
configureSurface (LayerSurface ptr) width height = c_configure ptr width height

foreign import ccall unsafe "wlr_layer_surface_v1_close" c_close :: Ptr LayerSurface -> IO ()

closeSurface :: LayerSurface -> IO ()
closeSurface = c_close . unLSS

data LayerSurfaceEvents = LayerSurfaceEvents
    { layerSurfaceEventsDestroy :: Ptr (WlSignal LayerSurface)
    , layerSurfaceEventsMap     :: Ptr (WlSignal LayerSurface)
    , layerSurfaceEventsUnmap   :: Ptr (WlSignal LayerSurface)
    , layerSurfaceEventsPopup   :: Ptr (WlSignal WlrXdgPopup)
    }

getLayerSurfaceEvents :: LayerSurface -> LayerSurfaceEvents
getLayerSurfaceEvents (LayerSurface ptr) = LayerSurfaceEvents
    { layerSurfaceEventsDestroy = #{ptr struct wlr_layer_surface_v1, events.destroy} ptr
    , layerSurfaceEventsMap     = #{ptr struct wlr_layer_surface_v1, events.map} ptr
    , layerSurfaceEventsUnmap   = #{ptr struct wlr_layer_surface_v1, events.unmap} ptr
    , layerSurfaceEventsPopup   = #{ptr struct wlr_layer_surface_v1, events.new_popup} ptr
    }


getLayerSurfaceLayer :: LayerSurface -> IO LayerShellLayer
getLayerSurfaceLayer val = surfaceStateLayer <$> getSurfaceState val


instance Storable SurfaceState where
    sizeOf _ = #{size struct wlr_layer_surface_v1_state}
    alignment _ = #{alignment struct wlr_layer_surface_v1_state}
    peek ptr = SurfaceState
        <$> #{peek struct wlr_layer_surface_v1_state, anchor} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, exclusive_zone} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, margin.top} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, margin.bottom} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, margin.left} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, margin.right} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, keyboard_interactive} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, desired_width} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, desired_height} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, actual_width} ptr
        <*> #{peek struct wlr_layer_surface_v1_state, actual_height} ptr
        <*> getSurfaceStateLayer ptr
    poke = error "No reason to poke LayerShell SurfaceStates for now"

getSurfaceStateLayer :: Ptr SurfaceState -> IO LayerShellLayer
getSurfaceStateLayer ptr = do
    layer :: CInt <- #{peek struct wlr_layer_surface_v1_state, layer} ptr
    pure $ case layer of
        #{const ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND} -> LayerShellLayerBackground
        #{const ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM}     -> LayerShellLayerBottom
        #{const ZWLR_LAYER_SHELL_V1_LAYER_TOP}        -> LayerShellLayerTop
        #{const ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY}    -> LayerShellLayerOverlay
        _ -> LayerShellLayerBottom

getSurfaceState :: LayerSurface -> IO SurfaceState
getSurfaceState = #{peek struct wlr_layer_surface_v1, current} . unLSS


getSurfaceOutput :: LayerSurface -> IO (Ptr WlrOutput)
getSurfaceOutput = #{peek struct wlr_layer_surface_v1, output} . unLSS

setSurfaceOutput :: LayerSurface -> Ptr WlrOutput -> IO ()
setSurfaceOutput surf out = #{poke struct wlr_layer_surface_v1, output} (unLSS surf) out

getLayerSurfaceSurface :: LayerSurface -> IO (Maybe (Ptr WlrSurface))
getLayerSurfaceSurface (LayerSurface ptr) = do
    ret <- #{peek struct wlr_layer_surface_v1, surface} ptr
    pure $ case ret /= nullPtr of
        True -> Just ret
        False -> Nothing

getPopups :: LayerSurface -> IO [Ptr WlrXdgPopup]
getPopups (LayerSurface surf) = do
    let list = #{ptr struct wlr_layer_surface_v1, popups} surf
    getListFromHead list #{offset struct wlr_xdg_popup, link}
