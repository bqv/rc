{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Backend
    ( Backend
    , backendAutocreate
    , backendStart
    , backendDestroy
    , getSession

    , BackendSignals (..)
    , backendGetSignals
    , backendGetRenderer
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/backend.h>

import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Graphics.Wayland.Server (DisplayServer(..))
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Backend.Session (WlrSession)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Render (Renderer)

data Backend

foreign import ccall unsafe "wlr_backend_autocreate" c_backend_autocreate :: Ptr DisplayServer -> Ptr a -> IO (Ptr Backend)

backendAutocreate :: DisplayServer -> IO (Ptr Backend)
backendAutocreate (DisplayServer ptr) = throwErrnoIfNull "backendAutocreate" $ c_backend_autocreate ptr nullPtr


foreign import ccall safe "wlr_backend_start" c_backend_start :: Ptr Backend -> IO Bool

backendStart :: Ptr Backend -> IO ()
backendStart = throwErrnoIf_ not "backendStart" . c_backend_start


foreign import ccall safe "wlr_backend_destroy" c_backend_destroy :: Ptr Backend -> IO ()

backendDestroy :: Ptr Backend -> IO ()
backendDestroy = c_backend_destroy

data BackendSignals = BackendSignals
    { backendEvtInput   :: Ptr (WlSignal InputDevice)
    , backendEvtOutput  :: Ptr (WlSignal WlrOutput)
    , backendEvtDestroy :: Ptr (WlSignal Backend)
    }

backendGetSignals :: Ptr Backend -> BackendSignals
backendGetSignals ptr = 
    let input_add = #{ptr struct wlr_backend, events.new_input} ptr
        output_add = #{ptr struct wlr_backend, events.new_output} ptr
        destroy = #{ptr struct wlr_backend, events.destroy} ptr
     in BackendSignals
         { backendEvtInput = input_add
         , backendEvtOutput = output_add
         , backendEvtDestroy = destroy
         }

foreign import ccall "wlr_backend_get_renderer" c_get_renderer :: Ptr Backend -> IO (Ptr Renderer)

backendGetRenderer :: Ptr Backend -> IO (Ptr Renderer)
backendGetRenderer = c_get_renderer

foreign import ccall unsafe "wlr_backend_get_session" c_get_session :: Ptr Backend -> IO (Ptr WlrSession)

getSession :: Ptr Backend -> IO (Maybe (Ptr WlrSession))
getSession b = do
    s <- c_get_session b
    pure $ if s == nullPtr
        then Nothing
        else Just s
