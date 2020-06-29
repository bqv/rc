{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Backend.Session
    ( WlrSession
    , changeVT
    )
where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CUInt(..))

import Foreign.C.Error (throwErrnoIf_)

data WlrSession


foreign import ccall unsafe "wlr_session_change_vt" c_change_vt :: Ptr WlrSession -> CUInt -> IO Bool

changeVT :: Ptr WlrSession -> Word -> IO ()
changeVT ptr vt = throwErrnoIf_ not "changeVT" $ c_change_vt ptr (fromIntegral vt)
