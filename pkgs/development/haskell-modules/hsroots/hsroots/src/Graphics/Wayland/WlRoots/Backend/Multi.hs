module Graphics.Wayland.WlRoots.Backend.Multi
    ( isMulti

    , addBackend
    )
where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Backend (Backend)


foreign import ccall unsafe "wlr_backend_is_multi" c_is_multi :: Ptr Backend -> IO Word8

isMulti :: Ptr Backend -> IO Bool
isMulti = fmap (/= 0) . c_is_multi

foreign import ccall "wlr_multi_backend_add" c_multi_add :: Ptr Backend -> Ptr Backend -> IO ()

addBackend :: Ptr Backend -> Ptr Backend -> IO ()
addBackend = c_multi_add
