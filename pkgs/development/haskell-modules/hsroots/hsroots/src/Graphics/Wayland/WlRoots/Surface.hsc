{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , surfaceGetTexture

    , createSurface
    , surfaceGetRoot

    , WlrSurfaceState

    , WlrFrameCallback
    , callbackGetResource
    , surfaceGetCallbacks
    , callbackGetCallback

    , getPendingState
    , getCurrentState

    , WlrSubSurface
    , subSurfaceGetSurface
    , surfaceGetSubs
    , subSurfaceGetBox
    , surfaceGetInputRegion

    , getSurfaceResource
    , surfaceAt
    , surfaceHasDamage

    , WlrSurfaceEvents (..)
    , getWlrSurfaceEvents

    , surfaceGetScale
    , surfaceGetSize
    , surfaceSendEnter
    , surfaceSendLeave
    , getSurfaceDamage
    , subSurfaceGetDestroyEvent
    , surfaceGetTransform

    , pokeSurfaceData
    , peekSurfaceData

    , surfaceFromResource
    , surfaceHasBuffer
    , surfaceGetBuffer
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_surface.h>

import Data.Composition ((.:))
import Data.Int (Int32)
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)

import Graphics.Pixman (PixmanRegion32 (..), pixmanRegionNotEmpty)
import Graphics.Wayland.Signal
import Graphics.Wayland.Server (Callback(..), OutputTransform (..))

import Graphics.Wayland.List (getListFromHead, getListElems, WlList)
import Graphics.Wayland.Resource (WlResource)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Render (Texture, Renderer)
import Graphics.Wayland.WlRoots.Buffer (WlrBuffer (..))

data WlrSurface

pokeSurfaceData :: Ptr WlrSurface -> Ptr a -> IO ()
pokeSurfaceData = #{poke struct wlr_surface, data}

peekSurfaceData :: Ptr WlrSurface -> IO (Ptr a)
peekSurfaceData = #{peek struct wlr_surface, data}

foreign import ccall unsafe "wlr_surface_create" c_create :: Ptr WlResource -> Ptr Renderer -> IO (Ptr WlrSurface)

createSurface :: Ptr WlResource -> Ptr Renderer -> IO (Ptr WlrSurface)
createSurface = throwErrnoIfNull "createSurface" .: c_create

getSurfaceResource :: Ptr WlrSurface -> IO (Ptr WlResource)
getSurfaceResource = #{peek struct wlr_surface, resource}

data WlrSurfaceEvents = WlrSurfaceEvents
    { wlrSurfaceEvtCommit  :: Ptr (WlSignal WlrSurface)
    , wlrSurfaceEvtSubSurf :: Ptr (WlSignal WlrSubSurface)
    , wlrSurfaceEvtDestroy :: Ptr (WlSignal WlrSurface)
    }

getWlrSurfaceEvents :: Ptr WlrSurface -> WlrSurfaceEvents
getWlrSurfaceEvents ptr = WlrSurfaceEvents
    { wlrSurfaceEvtDestroy = #{ptr struct wlr_surface, events.destroy} ptr
    , wlrSurfaceEvtSubSurf = #{ptr struct wlr_surface, events.new_subsurface} ptr
    , wlrSurfaceEvtCommit = #{ptr struct wlr_surface, events.commit} ptr
    }

foreign import ccall unsafe "wlr_surface_get_texture" c_get_texture :: Ptr WlrSurface -> IO (Ptr Texture)

surfaceGetTexture :: Ptr WlrSurface -> IO (Maybe (Ptr Texture))
surfaceGetTexture ptr = do
    ret <- c_get_texture ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

foreign import ccall unsafe "wlr_surface_get_root_surface" c_get_root_surface :: Ptr WlrSurface -> IO (Ptr WlrSurface)

surfaceGetRoot :: Ptr WlrSurface -> IO (Ptr WlrSurface)
surfaceGetRoot = c_get_root_surface


data WlrSurfaceState

stateGetTransform :: Ptr WlrSurfaceState -> IO OutputTransform
stateGetTransform = fmap OutputTransform . #{peek struct wlr_surface_state, transform}

surfaceGetTransform :: Ptr WlrSurface -> IO OutputTransform
surfaceGetTransform = stateGetTransform . getCurrentState

stateGetScale :: Ptr WlrSurfaceState -> IO Word32
stateGetScale = #{peek struct wlr_surface_state, scale}

surfaceGetScale :: Ptr WlrSurface -> IO Word32
surfaceGetScale = stateGetScale . getCurrentState

stateGetInputRegion :: Ptr WlrSurfaceState -> Ptr PixmanRegion32
stateGetInputRegion = #{ptr struct wlr_surface_state, input}

surfaceGetInputRegion :: Ptr WlrSurface -> Ptr PixmanRegion32
surfaceGetInputRegion = stateGetInputRegion . getCurrentState

stateGetSize :: Ptr WlrSurfaceState -> IO Point
stateGetSize state = do
    width :: CInt <- #{peek struct wlr_surface_state, width} state
    height :: CInt <- #{peek struct wlr_surface_state, height} state

    pure $ Point (fromIntegral width) (fromIntegral height)

surfaceGetSize :: Ptr WlrSurface -> IO Point
surfaceGetSize = stateGetSize . getCurrentState

newtype WlrFrameCallback = WlrFrameCallback (Ptr WlList)
foreign import ccall unsafe "wl_resource_from_link" c_resource_from_link :: Ptr WlList -> IO (Ptr WlResource)

callbackGetResource :: WlrFrameCallback -> IO (Ptr WlResource)
callbackGetResource (WlrFrameCallback ptr) =
    c_resource_from_link ptr

surfaceGetCallbacks :: Ptr WlrSurfaceState -> IO [WlrFrameCallback]
surfaceGetCallbacks ptr =
    let list = #{ptr struct wlr_surface_state, frame_callback_list} ptr
     in fmap WlrFrameCallback <$> getListElems list

callbackGetCallback :: WlrFrameCallback -> IO Callback
callbackGetCallback = fmap (Callback . castPtr) . callbackGetResource


getPendingState :: Ptr WlrSurface -> IO (Ptr WlrSurfaceState)
getPendingState = #{peek struct wlr_surface, pending}

getCurrentState :: Ptr WlrSurface -> Ptr WlrSurfaceState
getCurrentState = #{ptr struct wlr_surface, current}

stateHasDamage :: Ptr WlrSurfaceState -> IO Bool
stateHasDamage ptr = pixmanRegionNotEmpty . PixmanRegion32 $ #{ptr struct wlr_surface_state, surface_damage} ptr

surfaceHasDamage :: Ptr WlrSurface -> IO Bool
surfaceHasDamage = stateHasDamage . getCurrentState

getStateDamage :: Ptr WlrSurfaceState -> Ptr PixmanRegion32
getStateDamage = #{ptr struct wlr_surface_state, surface_damage}

getSurfaceDamage :: Ptr WlrSurface -> IO (Maybe PixmanRegion32)
getSurfaceDamage surf = do
    let state = getCurrentState surf
    hasDamage <- stateHasDamage state
    pure $ if hasDamage
        then Just . PixmanRegion32 $ getStateDamage state
        else Nothing

data WlrSubSurface

subSurfaceGetDestroyEvent :: Ptr WlrSubSurface -> Ptr (WlSignal WlrSubSurface)
subSurfaceGetDestroyEvent = #{ptr struct wlr_subsurface, events.destroy}

subSurfaceGetSurface :: Ptr WlrSubSurface -> IO (Ptr WlrSurface)
subSurfaceGetSurface = #{peek struct wlr_subsurface, surface}

surfaceGetSubs :: Ptr WlrSurface -> IO [Ptr WlrSubSurface]
surfaceGetSubs surf = do
    let list = #{ptr struct wlr_surface, subsurfaces} surf
    getListFromHead list #{offset struct wlr_subsurface, parent_link}

subSurfaceGetBox :: Ptr WlrSubSurface -> IO WlrBox
subSurfaceGetBox surf = do
    Point w h <- surfaceGetSize =<< subSurfaceGetSurface surf
    let subsurfState = #{ptr struct wlr_subsurface, current} surf
    x :: Int32 <- #{peek struct wlr_subsurface_state, x} subsurfState
    y :: Int32 <- #{peek struct wlr_subsurface_state, y} subsurfState

    pure $ WlrBox (fromIntegral x) (fromIntegral y) w h

foreign import ccall "wlr_surface_surface_at" c_subsurface_at :: Ptr WlrSurface -> Double -> Double -> Ptr Double -> Ptr Double -> IO (Ptr WlrSurface)

surfaceAt :: Ptr WlrSurface -> Double -> Double -> IO (Maybe (Ptr WlrSurface, Double, Double))
surfaceAt surf x y = alloca $ \xptr -> alloca $ \yptr -> do
    ret <- c_subsurface_at surf x y xptr yptr
    if ret == nullPtr
        then pure Nothing
        else do
            sX <- peek xptr
            sY <- peek yptr
            pure $ Just (ret, sX, sY)

foreign import ccall "wlr_surface_send_enter" c_send_enter :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()

surfaceSendEnter :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()
surfaceSendEnter = c_send_enter

foreign import ccall "wlr_surface_send_leave" c_send_leave :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()

surfaceSendLeave :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()
surfaceSendLeave = c_send_leave

foreign import ccall "wlr_surface_from_resource" c_from_resource :: Ptr WlResource -> IO (Ptr WlrSurface)

surfaceFromResource :: Ptr WlResource -> IO (Ptr WlrSurface)
surfaceFromResource = c_from_resource

foreign import ccall "wlr_surface_has_buffer" c_has_buffer :: Ptr WlrSurface -> IO Word8

surfaceHasBuffer :: Ptr WlrSurface -> IO Bool
surfaceHasBuffer =  fmap (/= 0) . c_has_buffer

surfaceGetBuffer :: Ptr WlrSurface -> IO (WlrBuffer)
surfaceGetBuffer = fmap WlrBuffer . #{peek struct wlr_surface, buffer}
