module Graphics.Wayland.Global
    ( WlGlobal
    , FilterFun
    , setGlobalFilter
    )
where

import Foreign.Ptr (Ptr, FunPtr, nullPtr, castPtrToFunPtr)
import Graphics.Wayland.Server (DisplayServer (..), Client (..))

data WlGlobal

type FilterFun = Ptr Client -> Ptr WlGlobal -> IO Bool
type FilterFunPtr a = Ptr Client -> Ptr WlGlobal -> Ptr a -> IO Bool

foreign import ccall "wl_display_set_global_filter" c_set_filter :: Ptr DisplayServer -> FunPtr (FilterFunPtr a) -> Ptr a -> IO ()

foreign import ccall "wrapper" mkCbFun :: (FilterFunPtr a) -> IO (FunPtr (FilterFunPtr a))

setGlobalFilter :: DisplayServer -> (Maybe FilterFun) -> IO ()
setGlobalFilter (DisplayServer ptr) Nothing = c_set_filter ptr (castPtrToFunPtr nullPtr) nullPtr
setGlobalFilter (DisplayServer ptr) (Just fun) = do
    cb <- mkCbFun $ \c g _ -> fun c g
    c_set_filter ptr cb nullPtr


