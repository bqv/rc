{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Render
    ( Renderer
    , Texture

    , renderWithMatrix
    , renderWithMatrixA
    , rendererDestroy

    , rendererBegin
    , rendererEnd

    , doRender
    , getTextureSize

    , renderColoredQuad

    , rendererScissor
    , rendererClear

    , initWlDisplay
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/render/wlr_renderer.h>

import Control.Exception (bracket_)
import Foreign.C.Error ({-throwErrnoIfNull, -}throwErrnoIf_)
import Foreign.C.Types (CFloat(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Server (DisplayServer (..))

import Graphics.Wayland.WlRoots.Output (WlrOutput, getWidth, getHeight)
import Graphics.Wayland.WlRoots.Render.Color (Color)
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))
import Graphics.Wayland.WlRoots.Box (WlrBox)

data Renderer
data Texture

foreign import ccall unsafe "wlr_renderer_begin" c_renderer_begin :: Ptr Renderer -> CInt -> CInt -> IO ()

rendererBegin :: Ptr Renderer -> CInt -> CInt -> IO ()
rendererBegin = c_renderer_begin


foreign import ccall unsafe "wlr_renderer_end" c_renderer_end :: Ptr Renderer -> IO ()

rendererEnd :: Ptr Renderer -> IO ()
rendererEnd = c_renderer_end


doRender :: Ptr Renderer -> Ptr WlrOutput -> IO a -> IO a
doRender renderer output act = do
    width <- getWidth output
    height <- getHeight output
    bracket_
        (rendererBegin renderer (fromIntegral width) (fromIntegral height))
        (rendererEnd renderer)
        act

foreign import ccall unsafe "wlr_render_texture_with_matrix" c_render_with_matrix :: Ptr Renderer -> Ptr Texture -> Ptr CFloat -> CFloat -> IO Bool

renderWithMatrix :: Ptr Renderer -> Ptr Texture -> Matrix -> IO ()
renderWithMatrix r t (Matrix m) = throwErrnoIf_ not "renderWithMatrix" $ c_render_with_matrix r t m 1.0

renderWithMatrixA :: Ptr Renderer -> Ptr Texture -> Matrix -> CFloat -> IO ()
renderWithMatrixA r t (Matrix m) a = throwErrnoIf_ not "renderWithMatrixA" $ c_render_with_matrix r t m a


foreign import ccall unsafe "wlr_renderer_destroy" c_renderer_destroy :: Ptr Renderer -> IO ()

rendererDestroy :: Ptr Renderer -> IO ()
rendererDestroy = c_renderer_destroy

foreign import ccall unsafe "wlr_render_quad_with_matrix" c_colored_quad :: Ptr Renderer -> Ptr Color -> Ptr CFloat -> IO ()

renderColoredQuad :: Ptr Renderer -> Color -> Matrix -> IO ()
renderColoredQuad rend col (Matrix m) = with col $ \cptr ->
    c_colored_quad rend cptr m

foreign import ccall unsafe "wlr_renderer_clear" c_clear :: Ptr Renderer -> Ptr Color -> IO ()

rendererClear :: Ptr Renderer -> Color -> IO ()
rendererClear rend col = with col $ c_clear rend

foreign import ccall unsafe "wlr_texture_get_size" c_get_size :: Ptr Texture -> Ptr CInt -> Ptr CInt -> IO ()

getTextureSize :: Ptr Texture -> IO (Int, Int)
getTextureSize ptr = do
    (width, height) <- alloca $ \widthPtr -> alloca $ \heightPtr -> c_get_size ptr widthPtr heightPtr >> peekTextureSize widthPtr heightPtr
    pure (fromIntegral width, fromIntegral height)
    where peekTextureSize :: Ptr CInt -> Ptr CInt -> IO (CInt, CInt)
          peekTextureSize widthPtr heightPtr = do
              width <- peek widthPtr
              height <- peek heightPtr
              pure (width, height)

foreign import ccall unsafe "wlr_renderer_scissor" c_scissor :: Ptr Renderer -> Ptr WlrBox -> IO ()

rendererScissor :: Ptr Renderer -> Maybe WlrBox -> IO ()
rendererScissor rend (Just box) = with box $ c_scissor rend
rendererScissor rend Nothing =  c_scissor rend nullPtr


foreign import ccall unsafe "wlr_renderer_init_wl_display" c_init_display :: Ptr Renderer -> Ptr DisplayServer -> IO ()

initWlDisplay :: DisplayServer -> Ptr Renderer -> IO ()
initWlDisplay (DisplayServer dsp) rend = c_init_display rend dsp
