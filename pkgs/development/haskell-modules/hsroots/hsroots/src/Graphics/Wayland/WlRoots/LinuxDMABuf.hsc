module Graphics.Wayland.WlRoots.LinuxDMABuf
    ( LinuxDMABuf (..)
    , createDMABuf
    , destroyDMABuf
    )
where

import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer(..))

import Graphics.Wayland.WlRoots.Render (Renderer)
import Graphics.Wayland.WlRoots.Backend (Backend, backendGetRenderer)

newtype LinuxDMABuf = LinuxDMABuf (Ptr LinuxDMABuf)

foreign import ccall unsafe "wlr_linux_dmabuf_v1_create" c_create :: Ptr DisplayServer -> Ptr Renderer -> IO (Ptr LinuxDMABuf)

createDMABuf :: DisplayServer -> Ptr Backend -> IO LinuxDMABuf
createDMABuf (DisplayServer dsp) backend =
    LinuxDMABuf <$> throwErrnoIfNull "creatELinuxDMABuf" (c_create dsp =<< backendGetRenderer backend)

foreign import ccall "wlr_linux_dmabuf_v1_destroy" c_destroy :: Ptr LinuxDMABuf -> IO ()

destroyDMABuf :: LinuxDMABuf -> IO ()
destroyDMABuf (LinuxDMABuf ptr) = c_destroy ptr
