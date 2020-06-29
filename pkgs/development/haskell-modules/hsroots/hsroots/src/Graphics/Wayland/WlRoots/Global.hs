module Graphics.Wayland.WlRoots.Global
where

import Graphics.Wayland.Global (WlGlobal)
import Foreign.Ptr (Ptr)

class GlobalWrapper a where
    getGlobal :: a  -> IO (Ptr WlGlobal)
    removeGlobal :: a -> IO ()
