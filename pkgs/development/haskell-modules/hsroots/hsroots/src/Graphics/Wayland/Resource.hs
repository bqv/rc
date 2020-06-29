{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.Resource
    ( WlResource
    , getUserData
    , resourceDestroy
    , resourceGetClient
    , resourceFromLink
    , addResourceDestroyListener
    )
where

import Foreign.Ptr (Ptr)

import Graphics.Wayland.List (WlList)
import Graphics.Wayland.Server (Client (..))
import Graphics.Wayland.Signal

data WlResource

foreign import ccall unsafe "wl_resource_get_user_data" c_get_user_data :: Ptr WlResource -> Ptr a

getUserData :: Ptr WlResource -> Ptr a
getUserData = c_get_user_data

foreign import ccall unsafe "wl_resource_destroy" c_resource_destroy :: Ptr WlResource -> IO ()

resourceDestroy :: Ptr WlResource -> IO ()
resourceDestroy = c_resource_destroy

foreign import ccall unsafe "wl_resource_get_client" c_get_client :: Ptr WlResource -> IO (Ptr Client)

resourceGetClient :: Ptr WlResource -> IO Client
resourceGetClient = fmap Client . c_get_client

foreign import ccall unsafe "wl_resource_from_link" c_from_link :: Ptr WlList -> Ptr WlResource

resourceFromLink :: Ptr WlList -> Ptr WlResource
resourceFromLink = c_from_link

foreign import ccall unsafe "wl_resource_add_destroy_listener" c_add_listener :: Ptr WlResource -> Ptr (WlListener WlResource) -> IO ()

addResourceDestroyListener :: Ptr WlResource -> (Ptr WlResource -> IO ()) -> IO ()
addResourceDestroyListener rs act = 
    addDestroyListener act (c_add_listener rs)
