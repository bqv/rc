{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XCursor
    ( WlrXCursorTheme
    , loadCursorTheme
    , destroyCursorTheme

    , WlrXCursor
    , getCursor

    , cursorFrame

    , getImages
    , WlrXCursorImage (..)
    )
where

#include <wlr/xcursor.h>

import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word32)

import Foreign.C.Error (throwErrnoIfNull)

import Data.Composition ((.:))

data WlrXCursorTheme

foreign import ccall "wlr_xcursor_theme_load" c_theme_load :: CString -> CInt -> IO (Ptr WlrXCursorTheme)

loadCursorTheme :: String -> Word -> IO (Ptr WlrXCursorTheme)
loadCursorTheme name size = withCString name $ \str -> 
    throwErrnoIfNull "loadCursorTheme" $ c_theme_load str (fromIntegral size)

foreign import ccall "wlr_xcursor_theme_destroy" c_theme_destroy :: Ptr WlrXCursorTheme -> IO ()

destroyCursorTheme :: Ptr WlrXCursorTheme -> IO ()
destroyCursorTheme = c_theme_destroy


data WlrXCursor

foreign import ccall "wlr_xcursor_theme_get_cursor" c_get_cursor :: Ptr WlrXCursorTheme -> CString -> IO (Ptr WlrXCursor)

getCursor :: Ptr WlrXCursorTheme -> String -> IO (Ptr WlrXCursor)
getCursor theme name = withCString name $ \str -> 
    throwErrnoIfNull "getCursor" $ c_get_cursor theme str

foreign import ccall "wlr_xcursor_frame" c_cursor_frame :: Ptr WlrXCursor -> Word32 -> IO CInt

cursorFrame :: Ptr WlrXCursor -> Word32 -> IO Int
cursorFrame = fmap fromIntegral .: c_cursor_frame

getImages :: Ptr WlrXCursor -> IO [Ptr WlrXCursorImage]
getImages xcursor = do
    count :: CUInt <- #{peek struct wlr_xcursor, image_count} xcursor
    ptr <- #{peek struct wlr_xcursor, images} xcursor
    mapM (peekElemOff ptr . fromIntegral) [0 .. count - 1]

data WlrXCursorImage = WlrXCursorImage
    { xCursorImageWidth :: Word32
    , xCursorImageHeight :: Word32

    , xCursorImageHotspotX :: Word32
    , xCursorImageHotspotY :: Word32

    , xCursorImageDelay :: Word32

    , xCursorImageBuffer :: Ptr ()
    } deriving (Eq, Show)

instance Storable WlrXCursorImage where
    sizeOf _ = #{size struct wlr_xcursor_image}
    alignment _ = #{alignment struct wlr_xcursor_image}
    peek ptr = WlrXCursorImage
        <$> #{peek struct wlr_xcursor_image, width} ptr
        <*> #{peek struct wlr_xcursor_image, height} ptr
        <*> #{peek struct wlr_xcursor_image, hotspot_x} ptr
        <*> #{peek struct wlr_xcursor_image, hotspot_y} ptr
        <*> #{peek struct wlr_xcursor_image, delay} ptr
        <*> #{peek struct wlr_xcursor_image, buffer} ptr
    poke ptr image = do
        #{poke struct wlr_xcursor_image, width} ptr $ xCursorImageWidth image
        #{poke struct wlr_xcursor_image, height} ptr $ xCursorImageHeight image
        #{poke struct wlr_xcursor_image, hotspot_x} ptr $ xCursorImageHotspotX image
        #{poke struct wlr_xcursor_image, hotspot_y} ptr $ xCursorImageHotspotY image
        #{poke struct wlr_xcursor_image, delay} ptr $ xCursorImageDelay image
        #{poke struct wlr_xcursor_image, buffer} ptr $ xCursorImageBuffer image
