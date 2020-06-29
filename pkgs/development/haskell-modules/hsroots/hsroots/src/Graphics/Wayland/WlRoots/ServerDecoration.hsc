{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.ServerDecoration
    ( ServerDecorationMode (..)
    , WlrServerDecorationManager
    , DecorationManagerEvents (..)
    , WlrServerDecoration
    , ServerDecorationEvents (..)

    , setDefaultDecorationMode
    , destroyServerDecorationManager
    , createServerDecorationManager
    , getDecorationManagerEvents
    , getServerDecorationMode
    , getServerDecorationEvents
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_server_decoration.h>

import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (..))
import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.Signal (WlSignal)

data ServerDecorationMode
    = SDModeNone
    | SDModeClient
    | SDModeServer
    deriving (Show, Eq)

sDModeToInt :: Num a => ServerDecorationMode -> a
sDModeToInt SDModeNone   = #{const WLR_SERVER_DECORATION_MANAGER_MODE_NONE}
sDModeToInt SDModeClient = #{const WLR_SERVER_DECORATION_MANAGER_MODE_CLIENT}
sDModeToInt SDModeServer = #{const WLR_SERVER_DECORATION_MANAGER_MODE_SERVER}

intToSDMode :: (Num a, Eq a, Show a) => a -> ServerDecorationMode
intToSDMode #{const WLR_SERVER_DECORATION_MANAGER_MODE_NONE}   = SDModeNone  
intToSDMode #{const WLR_SERVER_DECORATION_MANAGER_MODE_CLIENT} = SDModeClient
intToSDMode #{const WLR_SERVER_DECORATION_MANAGER_MODE_SERVER} = SDModeServer
intToSDMode x = error $ "Found invalid ServerDeocrationMode: " ++ show x

data WlrServerDecorationManager

data DecorationManagerEvents = DecorationManagerEvents
    { decorationManagerEvtNew :: Ptr (WlSignal WlrServerDecoration)
    }

getDecorationManagerEvents :: Ptr WlrServerDecorationManager -> DecorationManagerEvents
getDecorationManagerEvents ptr = DecorationManagerEvents
    { decorationManagerEvtNew = #{ptr struct wlr_server_decoration_manager, events.new_decoration} ptr
    }


foreign import ccall unsafe "wlr_server_decoration_manager_create" c_create :: Ptr DisplayServer -> IO (Ptr WlrServerDecorationManager)

createServerDecorationManager :: DisplayServer -> IO (Ptr WlrServerDecorationManager)
createServerDecorationManager (DisplayServer ptr) =
    throwErrnoIfNull "createServerDecorationManager" $ c_create ptr


foreign import ccall unsafe "wlr_server_decoration_manager_destroy" c_destroy :: Ptr WlrServerDecorationManager -> IO ()


destroyServerDecorationManager :: Ptr WlrServerDecorationManager -> IO ()
destroyServerDecorationManager = c_destroy

-- void wlr_server_decoration_manager_set_default_mode(
--  struct wlr_server_decoration_manager *manager, uint32_t default_mode);

foreign import ccall unsafe "wlr_server_decoration_manager_set_default_mode" c_set_default_mode :: Ptr WlrServerDecorationManager -> Word32 -> IO ()

setDefaultDecorationMode :: Ptr WlrServerDecorationManager -> ServerDecorationMode -> IO ()
setDefaultDecorationMode ptr mode = c_set_default_mode ptr $ sDModeToInt mode

data WlrServerDecoration

getServerDecorationMode :: Ptr WlrServerDecoration -> IO ServerDecorationMode
getServerDecorationMode ptr = do
    val :: Word32 <- #{peek struct wlr_server_decoration, mode} ptr
    pure $ intToSDMode val

data ServerDecorationEvents = ServerDecorationEvents
    { serverDecorationEvtDestroy :: Ptr (WlSignal WlrServerDecoration)
    , serverDecorationEvtMode    :: Ptr (WlSignal WlrServerDecoration)
    }

getServerDecorationEvents :: Ptr WlrServerDecoration -> ServerDecorationEvents
getServerDecorationEvents ptr = ServerDecorationEvents
    { serverDecorationEvtDestroy = #{ptr struct wlr_server_decoration, events.destroy} ptr
    , serverDecorationEvtMode    = #{ptr struct wlr_server_decoration, events.mode} ptr
    }

