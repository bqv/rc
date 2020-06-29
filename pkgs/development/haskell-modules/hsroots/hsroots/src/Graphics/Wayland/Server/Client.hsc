{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.Server.Client
where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr)

import Graphics.Wayland.Server (Client (..))
import Graphics.Wayland.Signal
    ( WlListener(..)
    , makeListenerPtr
    )

foreign import ccall unsafe "wl_client_add_destroy_listener" c_add_listener :: Ptr Client -> Ptr (WlListener Client) -> IO ()

addDestroyListener :: Client -> (Client -> IO ()) -> IO ()
addDestroyListener (Client c) fun = do
    ref :: IORef (StablePtr (ForeignPtr (WlListener Client))) <- newIORef undefined
    lptr <- makeListenerPtr . WlListener $ \client -> do
        fun (Client client)
        freeStablePtr =<< readIORef ref
    writeIORef ref =<< newStablePtr lptr
    withForeignPtr lptr $ c_add_listener c
