module Graphics.Wayland.WlRoots.XCursorManager
    ( WlrXCursorManager
    , xCursorManagerCreate
    , xCursorSetImage
    , xCursorLoad
    , xCursorManagerDestroy
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_xcursor_manager.h>

import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar, CInt(..))
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIfMinus1_)
import Foreign.C.String (withCString)

import Graphics.Wayland.WlRoots.Cursor (WlrCursor)

data WlrXCursorManager

foreign import ccall "wlr_xcursor_manager_create" c_manager_create :: Ptr CChar -> Word32 -> IO (Ptr WlrXCursorManager)

xCursorManagerCreate :: String -> Word -> IO (Ptr WlrXCursorManager)
xCursorManagerCreate name size = withCString name $ \str ->
    throwErrnoIfNull "xCursorManagerCreate" $ c_manager_create str (fromIntegral size)


foreign import ccall "wlr_xcursor_manager_destroy" c_manager_destroy :: Ptr WlrXCursorManager -> IO ()

xCursorManagerDestroy :: Ptr WlrXCursorManager -> IO ()
xCursorManagerDestroy = c_manager_destroy

foreign import ccall "wlr_xcursor_manager_set_cursor_image" c_set_cursor_image :: Ptr WlrXCursorManager -> Ptr CChar -> Ptr WlrCursor -> IO ()

xCursorSetImage :: Ptr WlrXCursorManager -> String -> Ptr WlrCursor -> IO ()
xCursorSetImage manager name cursor = withCString name $ \str ->
    c_set_cursor_image manager str cursor

foreign import ccall "wlr_xcursor_manager_load" c_load :: Ptr WlrXCursorManager -> Float -> IO CInt

xCursorLoad :: Ptr WlrXCursorManager -> Float -> IO ()
xCursorLoad manager scale =
    throwErrnoIfMinus1_ "xCursorLoad" $ c_load manager scale
