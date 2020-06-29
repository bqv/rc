{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Wayland.WlRoots.DeviceManager
    ( WlrDeviceManager
    , managerCreate

    , WlrDataSource (..)
    , sendDataSend
    , getSelectionText
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_data_device.h>

import Data.ByteString (useAsCString)
import Data.Text (Text)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CChar, CInt (..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.Storable (Storable (..))
import System.Posix.IO (createPipe)
import System.Posix.Types (Fd (..))

import Graphics.Wayland.Server (DisplayServer(..))

-- import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data WlrDeviceManager

foreign import ccall unsafe "wlr_data_device_manager_create" c_manager_create :: Ptr DisplayServer -> IO (Ptr WlrDeviceManager)

managerCreate :: DisplayServer -> IO (Ptr WlrDeviceManager)
managerCreate (DisplayServer ptr) =
    throwErrnoIfNull "managerCreate" $ c_manager_create ptr


newtype WlrDataSource = WlrDataSource { unDS :: Ptr WlrDataSource } deriving (Show, Eq)

foreign import ccall "dynamic"
    mkSendFun :: FunPtr (Ptr WlrDataSource -> Ptr CChar -> Fd -> IO ())
              -> Ptr WlrDataSource -> Ptr CChar -> Fd -> IO ()

wrapSendFun :: WlrDataSource -> IO (Ptr WlrDataSource -> Ptr CChar -> Fd -> IO ())
wrapSendFun (WlrDataSource ptr) = do
    impl <- #{peek struct wlr_data_source, impl} ptr
    mkSendFun <$> #{peek struct wlr_data_source_impl, send} impl

getSendFun :: WlrDataSource -> IO (Text -> Fd -> IO ())
getSendFun source = do
    fun <- wrapSendFun source
    pure $ realFun (fun $ unDS source)
    where   realFun :: (Ptr CChar -> Fd -> IO ()) -> Text -> Fd -> IO ()
            realFun fun txt fd = useAsCString (E.encodeUtf8 txt) $ \cstr ->
                    fun cstr fd

sendDataSend :: WlrDataSource -> Text -> Fd -> IO ()
sendDataSend device txt fd = do
    fun <- getSendFun device
    fun txt fd

getSelection :: WlrDataSource -> Text -> IO Fd
getSelection source mime = do
    (rFd, wFd) <- createPipe
    sendDataSend source mime wFd

    pure rFd

-- WARNING: This may be moved out of here
getSelectionText :: WlrDataSource -> IO Fd
getSelectionText source = getSelection source "text/plain"
