module Graphics.Wayland.WlRoots.Buffer
    ( WlrBuffer (..)
    , getBufferResource
    , getBuffer
    , putBuffer
    , getTexture
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_buffer.h>

import Foreign.Storable
import Foreign.Ptr (Ptr, nullPtr)

import Graphics.Wayland.Resource (WlResource)
import Graphics.Wayland.WlRoots.Render (Texture)

newtype WlrBuffer = WlrBuffer (Ptr WlrBuffer) 

getBufferResource :: WlrBuffer -> IO (Maybe (Ptr WlResource))
getBufferResource (WlrBuffer ptr) = do
    ret <- #{peek struct wlr_buffer, resource} ptr
    if ret == nullPtr
        then pure Nothing
        else pure $ Just ret

foreign import ccall unsafe "wlr_buffer_ref" c_ref :: Ptr WlrBuffer -> IO ()

getBuffer :: WlrBuffer -> IO WlrBuffer
getBuffer b@(WlrBuffer ptr) = c_ref ptr >> pure b

foreign import ccall unsafe "wlr_buffer_unref" c_unref :: Ptr WlrBuffer -> IO ()

putBuffer :: WlrBuffer -> IO ()
putBuffer (WlrBuffer ptr) = c_unref ptr

getTexture :: WlrBuffer -> IO (Maybe (Ptr Texture))
getTexture (WlrBuffer ptr) = do
    ret <- #{peek struct wlr_buffer, texture} ptr
    if ret == nullPtr
        then pure Nothing
        else pure $ Just ret
