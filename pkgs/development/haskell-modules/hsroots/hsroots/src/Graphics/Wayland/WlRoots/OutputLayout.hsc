{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , createOutputLayout
    , destroyOutputLayout

    , WlrOutputLayoutOutput
    , layoutOuputGetPosition
    , layoutGetOutput
    , layoutAtPos

    , layoutOutputGetOutput
    , layoutGetOutputs

    , addOutput
    , moveOutput
    , removeOutput

    , outputContainsPoint
    , outputIntersects

    , closestPoint
    , addOutputAuto
    , getOutputLayoutExtends
    , getOutputLayoutBox
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_output_layout.h>

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(peek, peekByteOff))
import Data.Composition ((.:))

import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Box (WlrBox, Point(..))
import Graphics.Wayland.List (getListFromHead)

data WlrOutputLayout

foreign import ccall "wlr_output_layout_create" c_layout_create :: IO (Ptr WlrOutputLayout)

createOutputLayout :: IO (Ptr WlrOutputLayout)
createOutputLayout = throwErrnoIfNull "createOutputLayout" c_layout_create


foreign import ccall "wlr_output_layout_destroy" c_layout_destroy :: Ptr WlrOutputLayout -> IO ()

destroyOutputLayout :: Ptr WlrOutputLayout -> IO ()
destroyOutputLayout = c_layout_destroy


data WlrOutputLayoutOutput

layoutOuputGetPosition :: Ptr WlrOutputLayoutOutput -> IO Point
layoutOuputGetPosition ptr = do
    x :: CInt <- #{peek struct wlr_output_layout_output, x} ptr
    y :: CInt <- #{peek struct wlr_output_layout_output, y} ptr
    pure $ Point (fromIntegral x) (fromIntegral y)

foreign import ccall "wlr_output_layout_get" c_layout_get :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO (Ptr WlrOutputLayoutOutput)

layoutGetOutput :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO (Ptr WlrOutputLayoutOutput)
layoutGetOutput = throwErrnoIfNull "layoutGetOutput" .: c_layout_get

layoutOutputGetOutput :: Ptr WlrOutputLayoutOutput -> IO (Ptr WlrOutput)
layoutOutputGetOutput = #{peek struct wlr_output_layout_output, output}

layoutGetOutputs :: Ptr WlrOutputLayout -> IO [Ptr WlrOutputLayoutOutput]
layoutGetOutputs layout =
    getListFromHead (#{ptr struct wlr_output_layout, outputs} layout) #{offset struct wlr_output_layout_output, link}


foreign import ccall "wlr_output_layout_output_at" c_layout_at :: Ptr WlrOutputLayout -> Double -> Double -> IO (Ptr WlrOutput)

layoutAtPos :: Ptr WlrOutputLayout -> Double -> Double -> IO (Maybe (Ptr WlrOutput))
layoutAtPos layout x y = do
    ret <- c_layout_at layout x y
    pure $ if ret == nullPtr
        then Nothing
        else Just ret


foreign import ccall "wlr_output_layout_add" c_output_add :: Ptr WlrOutputLayout -> Ptr WlrOutput -> CInt -> CInt -> IO ()

addOutput :: Ptr WlrOutputLayout -> Ptr WlrOutput -> Int -> Int -> IO ()
addOutput layout output x y =
    c_output_add layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_move" c_output_move :: Ptr WlrOutputLayout -> Ptr WlrOutput -> CInt -> CInt -> IO ()

moveOutput :: Ptr WlrOutputLayout -> Ptr WlrOutput -> Int -> Int -> IO ()
moveOutput layout output x y =
    c_output_move layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_remove" c_output_remove :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO ()

removeOutput :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO ()
removeOutput layout output =
    c_output_remove layout output

foreign import ccall unsafe "wlr_output_layout_get_box" c_get_box :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO (Ptr WlrBox)

getOutputLayoutBox :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO WlrBox
getOutputLayoutBox layout out = peek =<< c_get_box layout out

getOutputLayoutExtends :: Ptr WlrOutputLayout -> IO WlrBox
getOutputLayoutExtends layout = getOutputLayoutBox layout nullPtr

foreign import ccall "wlr_output_layout_contains_point" c_contains_point :: Ptr WlrOutputLayout -> Ptr WlrOutput -> CInt -> CInt -> IO Bool

outputContainsPoint :: Ptr WlrOutputLayout -> Ptr WlrOutput -> Int -> Int -> IO Bool
outputContainsPoint layout output x y = c_contains_point layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_intersects" c_intersects :: Ptr WlrOutputLayout -> Ptr WlrOutput -> Ptr WlrBox -> IO Bool

outputIntersects :: Ptr WlrOutputLayout -> Ptr WlrOutput -> WlrBox -> IO Bool
outputIntersects layout output box = with box $ c_intersects layout output


foreign import ccall "wlr_output_layout_closest_point" c_closest_point :: Ptr WlrOutputLayout -> Ptr WlrOutput -> Double -> Double -> Ptr Double -> Ptr Double -> IO ()

closestPoint :: Ptr WlrOutputLayout -> Maybe (Ptr WlrOutput) -> Double -> Double -> IO (Double, Double)
closestPoint layout Nothing x y = closestPoint layout (Just nullPtr) x y
closestPoint layout (Just output) x y = alloca $ \xptr -> alloca $ \yptr -> do
    c_closest_point layout output x y xptr yptr
    xret <- peek xptr
    yret <- peek yptr
    pure (xret, yret)

-- TODO: Box

foreign import ccall "wlr_output_layout_add_auto" c_add_auto :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO ()

addOutputAuto :: Ptr WlrOutputLayout -> Ptr WlrOutput -> IO ()
addOutputAuto = c_add_auto
