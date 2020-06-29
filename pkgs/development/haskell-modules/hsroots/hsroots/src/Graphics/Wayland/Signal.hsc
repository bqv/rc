{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Wayland.Signal
    ( WlSignal
    , WlListener (..)
    , ListenerToken

    , makeListenerPtr
    , addListener
    , removeListener
    , destroyListener
    , setSignalHandler
    , setDestroyHandler
    , addDestroyListener
    )
where

-- We need the wl_lisener in scope
#include <wayland-server.h>

import Control.Monad (when)
import Control.Concurrent.MVar
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, FunPtr, plusPtr, freeHaskellFunPtr, nullPtr, castFunPtrToPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)

data WlSignal a
data WlList

newtype WlListener a = WlListener (Ptr a -> IO ())
data ListenerToken = forall a. ListenerToken (ForeignPtr (WlListener a))

foreign import ccall unsafe "c_signal_add" c_signal_add :: Ptr (WlSignal a) -> Ptr (WlListener a) -> IO ()
foreign import ccall unsafe "wl_list_init" c_list_init :: Ptr WlList -> IO ()
foreign import ccall unsafe "wl_list_remove" c_list_remove :: Ptr WlList -> IO ()

destroyWlListener :: forall a. Ptr (WlListener a) -> IO ()
destroyWlListener ptr = do
    removeListener' ptr
    notify :: FunPtr (Ptr a -> IO ()) <- #{peek struct wl_listener, notify} ptr
    when (castFunPtrToPtr notify /= nullPtr) $ freeHaskellFunPtr notify
    #{poke struct wl_listener, notify} ptr nullPtr


freeWlListener :: forall a. Ptr (WlListener a) -> IO ()
freeWlListener ptr = do
    destroyWlListener ptr
    free ptr

foreign import ccall "wrapper" mkCbFun :: (Ptr (WlListener a) -> Ptr a -> IO ()) -> IO (FunPtr (Ptr (WlListener a) -> Ptr a -> IO ()))

makeListenerPtr :: forall a. WlListener a -> IO (ForeignPtr (WlListener a))
makeListenerPtr (WlListener fun) = do
    mem :: Ptr (WlListener a) <- mallocBytes #{size struct wl_listener}
    let link = #{ptr struct wl_listener, link} mem
    c_list_init link
    funPtr <- mkCbFun (\_ -> fun)
    #{poke struct wl_listener, notify} mem funPtr
    newForeignPtr mem (freeWlListener mem)

addListener :: WlListener a -> Ptr (WlSignal a) -> IO (ListenerToken)
addListener listener signal = do
    ptr <- makeListenerPtr listener
    withForeignPtr ptr $ c_signal_add signal
    pure (ListenerToken ptr)

destroyListener :: ListenerToken -> IO ()
destroyListener (ListenerToken ptr) = withForeignPtr ptr destroyWlListener

removeListener :: ListenerToken -> IO ()
removeListener (ListenerToken ptr) = withForeignPtr ptr removeListener'

removeListener' :: Ptr (WlListener a) -> IO ()
removeListener' ptr =
    let link = #{ptr struct wl_listener, link} ptr
     in do
         -- For some reason c_list_remove makes the elem point at null instead
         -- of itself, so we got to init it after, to *not* break things if we
         -- try to remove the element again
         c_list_remove link
         c_list_init link

-- | Set a 'Way' action as signal handler.
setSignalHandler :: Ptr (WlSignal a) -> (Ptr a -> IO ()) -> IO ListenerToken
setSignalHandler signal act = addListener (WlListener act) signal

-- | Set a signal handler that will remove itself after it's fired once. This
-- can be used for destroy handlers that don't have to be stored anywhere.
setDestroyHandler :: Ptr (WlSignal a)
                  -> (Ptr a -> IO ())
                  -> IO ()
setDestroyHandler signal handler = do
    var <- newEmptyMVar
    listener <- flip addListener signal . WlListener $ \ptr -> do
        handler ptr
        (destroyListener =<< takeMVar var)
    putMVar var listener


addDestroyListener :: (Ptr a -> IO ()) -> (Ptr (WlListener a) -> IO ()) -> IO ()
addDestroyListener fun adder = do
    var <- newEmptyMVar
    listener <- makeListenerPtr . WlListener $ \ptr -> do
        fun ptr
        (destroyListener =<< takeMVar var)
    withForeignPtr listener adder
    putMVar var (ListenerToken listener)
