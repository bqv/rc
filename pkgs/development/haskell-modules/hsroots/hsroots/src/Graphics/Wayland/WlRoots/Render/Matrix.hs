{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Render.Matrix
    ( Matrix(..)
-- | This entir emodule should probably replaced by types reimplemented in
-- Haskell to get out of IO for simple matrix calculations

    , withMatrix
    , withIdentity

    , printMatrix

-- | This is the low level interface exported by wlroots.
    , matrixIdentity
    , matrixTranslate
    , matrixScale
    , matrixRotate
    , matrixMul

    , matrixProjectBox
    )
where

import System.IO
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CFloat(..), CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)

import Graphics.Wayland.Server (OutputTransform(..))
import Graphics.Wayland.WlRoots.Box (WlrBox)

-- | This has to be a float[16]. The 'withMatrix' makes sure it is.
newtype Matrix = Matrix { unMatrix :: (Ptr CFloat) }

-- | Do something with a matrix. This needs to be IO for at least as long as we
-- keep the matrix type/operations from wlroots
withMatrix :: (Matrix -> IO a) -> IO a
withMatrix act = allocaBytes (9 * 4) $ act . Matrix

-- | Same as 'withMatrix' but make sure the matrix is the identity matrix.
withIdentity :: (Matrix -> IO a) -> IO a
withIdentity act = withMatrix $ \m -> do
    matrixIdentity m
    act m


foreign import ccall unsafe "wlr_matrix_identity" c_matrix_identity :: Ptr CFloat -> IO ()

matrixIdentity :: Matrix -> IO ()
matrixIdentity = c_matrix_identity . unMatrix


foreign import ccall unsafe "wlr_matrix_translate" c_matrix_translate :: Ptr CFloat -> CFloat -> CFloat -> IO ()

matrixTranslate :: Matrix -> Float -> Float -> IO ()
matrixTranslate (Matrix p) x y = c_matrix_translate p (CFloat x) (CFloat y)


foreign import ccall unsafe "wlr_matrix_scale" c_matrix_scale :: Ptr CFloat -> CFloat -> CFloat -> IO ()

matrixScale :: Matrix -> Float -> Float -> IO ()
matrixScale (Matrix p) x y = c_matrix_scale p (CFloat x) (CFloat y)


foreign import ccall unsafe "wlr_matrix_rotate" c_matrix_rotate :: Ptr CFloat -> CFloat -> IO ()

matrixRotate :: Matrix -> Float -> IO ()
matrixRotate (Matrix p) r = c_matrix_rotate p (CFloat r)


foreign import ccall unsafe "wlr_matrix_multiply" c_matrix_mul :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

matrixMul :: Matrix -> Matrix -> Matrix -> IO ()
matrixMul (Matrix x) (Matrix y) (Matrix o) = c_matrix_mul x y o


foreign import ccall unsafe "wlr_matrix_project_box" c_project_box :: Ptr CFloat -> Ptr WlrBox -> CInt -> CFloat -> Ptr CFloat -> IO ()

matrixProjectBox :: Matrix -> WlrBox -> OutputTransform -> Float -> Matrix -> IO ()
matrixProjectBox (Matrix mat) box (OutputTransform transform) rotation (Matrix projection) = with box $ \boxPtr -> 
    c_project_box mat boxPtr (fromIntegral transform) (CFloat rotation) projection


printMatrix :: Handle -> Matrix -> IO ()
printMatrix handle (Matrix p) = do
    values :: [CFloat] <- mapM (peekElemOff p) [0 .. 8]
    hPutStrLn handle . show $ take 3 $ drop 0  values
    hPutStrLn handle . show $ take 3 $ drop 3  values
    hPutStrLn handle . show $ take 3 $ drop 6  values
