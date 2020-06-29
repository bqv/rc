module Graphics.Wayland.WlRoots.Util.Region
where

import Foreign.C.Types (CFloat (..))
import Foreign.Ptr (Ptr)

import Graphics.Pixman (PixmanRegion32, withRegion32)

foreign import ccall unsafe "wlr_region_scale" c_scale :: Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> CFloat -> IO ()

scaleRegion :: PixmanRegion32 -> Float -> IO ()
scaleRegion region scale = withRegion32 region $ \ptr -> 
    c_scale ptr ptr (CFloat scale)
