module Graphics.Wayland.WlRoots.Render.Gles2
    ( rendererCreate
    )
where

import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Render (Renderer)
import Graphics.Wayland.WlRoots.Backend (Backend)

foreign import ccall unsafe "wlr_gles2_renderer_create" c_renderer_create :: Ptr Backend -> IO (Ptr Renderer)

rendererCreate :: Ptr Backend -> IO (Ptr Renderer)
rendererCreate = throwErrnoIfNull "rendererCreate" . c_renderer_create
