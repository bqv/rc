{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Compositor
    ( WlrCompositor
    , compositorCreate
    , compositorDestroy
    )
where

import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull)
import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Render (Renderer)

data WlrCompositor

foreign import ccall "wlr_compositor_create" c_compositor_create :: Ptr DisplayServer -> Ptr Renderer -> IO (Ptr WlrCompositor)

compositorCreate :: DisplayServer -> Ptr Renderer -> IO (Ptr WlrCompositor)
compositorCreate (DisplayServer ptr) backend = 
    throwErrnoIfNull "compositorCreate" $ c_compositor_create ptr backend

foreign import ccall "wlr_compositor_destroy" c_compositor_destroy :: Ptr WlrCompositor -> IO ()

compositorDestroy :: Ptr WlrCompositor -> IO ()
compositorDestroy = c_compositor_destroy
