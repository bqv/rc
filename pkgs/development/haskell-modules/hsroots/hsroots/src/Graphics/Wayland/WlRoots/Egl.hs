{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Egl
    ( EGL
    , eglInit
    , eglBindDisplay
    )
where

import Data.Composition ((.:))
import Graphics.Wayland.Server (Display)
import Graphics.Egl (Platform, getPlatform)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Foreign.C.Error (throwErrnoIf_)

data EGL


foreign import ccall unsafe "wlr_egl_init" c_egl_init :: Ptr EGL -> CInt -> Ptr a -> IO Bool

eglInit :: Ptr EGL -> Platform -> Ptr a -> IO ()
eglInit e p d = let num = getPlatform p in
    throwErrnoIf_ not "eglInit" (c_egl_init e num d)


foreign import ccall unsafe "wlr_egl_bind_display" c_egl_bind_display :: Ptr EGL -> Ptr Display -> IO Bool

eglBindDisplay :: Ptr EGL -> Ptr Display -> IO ()
eglBindDisplay =
    throwErrnoIf_ not "eglBindDisplay" .: c_egl_bind_display

-- TODO: wlr_egl_query_buffer



