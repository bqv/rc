-- This should probably live in another module in future
module Graphics.Pixman
    ( pixmanRegionExtents
    , PixmanRegion32 (..)
    , PixmanBox32 (..)
    , pixmanRegionNotEmpty
    , pixmanRegionTranslate
    , withRegionCopy
    , withRegion
    , pixmanRegionBoxes
    , withRegion32

    , allocateRegion
    , resetRegion
    , pixmanRegionUnion
    , pixmanRegionIntersect
    , boxToWlrBox
    , copyRegion
    , pixmanRegionSubtract
    , withBoxRegion
    )
where

#include <pixman-1/pixman.h>

import Control.Exception (bracket_)
import Data.Int (Int32)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (allocaBytes, alloca, mallocBytes, free)
import Foreign.Marshal.Array (peekArray)

import Graphics.Wayland.WlRoots.Box (WlrBox (..))

data PixmanRegion32
    = PixmanRegion32 { unPR32 :: Ptr PixmanRegion32}
    | PixmanRegion32A { unPR32A :: ForeignPtr PixmanRegion32 }

withRegion32 :: PixmanRegion32 -> (Ptr PixmanRegion32 -> IO a) -> IO a
withRegion32 (PixmanRegion32 r) act = act r
withRegion32 (PixmanRegion32A fptr) act = withForeignPtr fptr $ act

boxToWlrBox :: PixmanBox32 -> WlrBox
boxToWlrBox (PixmanBox32 x1 y1 x2 y2) = WlrBox
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral $ x2 - x1)
    (fromIntegral $ y2 - y1)

withBoxRegion :: WlrBox -> (PixmanRegion32 -> IO a) -> IO a
withBoxRegion box fun = withRegion $ \region -> do
    resetRegion region $ Just box
    fun region

data PixmanBox32 = PixmanBox32
    { pBoxX1 :: Int32
    , pBoxY1 :: Int32
    , pBoxX2 :: Int32
    , pBoxY2 :: Int32
    } deriving (Show, Eq)

instance Storable PixmanBox32 where
    sizeOf _ = #{size struct pixman_box32}
    alignment _ = #{alignment struct pixman_box32}
    peek ptr = PixmanBox32
        <$> #{peek struct pixman_box32, x1} ptr
        <*> #{peek struct pixman_box32, y1} ptr
        <*> #{peek struct pixman_box32, x2} ptr
        <*> #{peek struct pixman_box32, y2} ptr
    poke ptr (PixmanBox32 x1 y1 x2 y2) = do
        #{poke struct pixman_box32, x1} ptr x1
        #{poke struct pixman_box32, y1} ptr y1
        #{poke struct pixman_box32, x2} ptr x2
        #{poke struct pixman_box32, y2} ptr y2

foreign import ccall unsafe "pixman_region32_extents" c_32_extends :: Ptr PixmanRegion32 -> IO (Ptr PixmanBox32)

pixmanRegionExtents :: PixmanRegion32 -> IO PixmanBox32
pixmanRegionExtents region = peek =<< withRegion32 region c_32_extends

foreign import ccall unsafe "pixman_region32_not_empty" c_32_not_empty :: Ptr PixmanRegion32 -> IO Bool

pixmanRegionNotEmpty :: PixmanRegion32 -> IO Bool
pixmanRegionNotEmpty = flip withRegion32 c_32_not_empty

foreign import ccall unsafe "pixman_region32_translate" c_32_translate :: Ptr PixmanRegion32 -> CInt -> CInt -> IO ()

pixmanRegionTranslate :: PixmanRegion32 -> Int -> Int -> IO ()
pixmanRegionTranslate region x y = withRegion32 region $ \ptr ->
    c_32_translate ptr (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "pixman_region32_copy" c_32_copy :: Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> IO ()

withRegionCopy :: PixmanRegion32 -> (PixmanRegion32 -> IO a) -> IO a
withRegionCopy orig act = withRegion32 orig $ \original -> withRegion $ \copy -> do
    c_32_copy (unPR32 copy) original
    act $ copy

copyRegion :: PixmanRegion32 -> PixmanRegion32 -> IO ()
copyRegion dst src = withRegion32 dst $ \dstPtr ->
    withRegion32 src $ \srcPtr -> 
        c_32_copy dstPtr srcPtr

foreign import ccall unsafe "pixman_region32_init" c_32_init :: Ptr PixmanRegion32 -> IO ()
foreign import ccall unsafe "pixman_region32_init_rect" c_32_init_rect :: Ptr PixmanRegion32 -> CInt -> CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "pixman_region32_clear" c_32_clear :: Ptr PixmanRegion32 -> IO ()
foreign import ccall unsafe "pixman_region32_fini" c_32_fini :: Ptr PixmanRegion32 -> IO ()

withRegion :: (PixmanRegion32 -> IO a) -> IO a
withRegion act = allocaBytes #{size struct pixman_region32} $ \reg -> bracket_
    (c_32_init reg)
    (c_32_fini reg)
    (act $ PixmanRegion32 reg)

allocateRegion :: IO PixmanRegion32
allocateRegion = do
    ret <- mallocBytes #{size struct pixman_region32}
    c_32_init ret
    fptr <- newForeignPtr ret (freeRegion ret)
    pure $ PixmanRegion32A fptr

freeRegion :: Ptr PixmanRegion32 -> IO ()
freeRegion ptr = do
    c_32_fini ptr
    free ptr

resetRegion :: PixmanRegion32 -> Maybe WlrBox -> IO ()
resetRegion reg box = withRegion32 reg $ \ptr -> do
    c_32_clear ptr
    case box of
        Nothing -> c_32_init ptr
        Just (WlrBox x y w h) ->
            c_32_init_rect ptr
                (fromIntegral x)
                (fromIntegral y)
                (fromIntegral w)
                (fromIntegral h)

foreign import ccall unsafe "pixman_region32_rectangles" c_32_rectangles :: Ptr PixmanRegion32 -> Ptr CInt -> IO (Ptr PixmanBox32)

pixmanRegionBoxes :: PixmanRegion32 -> IO [PixmanBox32]
pixmanRegionBoxes region = alloca $ \nPtr -> withRegion32 region $ \reg -> do
    ret <- c_32_rectangles reg nPtr
    num <- peek nPtr
    peekArray (fromIntegral num) ret

foreign import ccall unsafe "pixman_region32_union" c_32_union :: Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> IO ()

pixmanRegionUnion :: PixmanRegion32 -> PixmanRegion32 -> IO ()
pixmanRegionUnion dst src = withRegion32 dst $ \dstPtr ->
    withRegion32 src $ \srcPtr ->
        c_32_union dstPtr dstPtr srcPtr

foreign import ccall unsafe "pixman_region32_intersect" c_32_intersect :: Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> IO ()

pixmanRegionIntersect :: PixmanRegion32 -> PixmanRegion32 -> IO ()
pixmanRegionIntersect dst src = withRegion32 dst $ \dstPtr ->
    withRegion32 src $ \srcPtr ->
        c_32_intersect dstPtr dstPtr srcPtr

foreign import ccall unsafe "pixman_region32_subtract" c_32_subtract :: Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> Ptr PixmanRegion32 -> IO ()

pixmanRegionSubtract :: PixmanRegion32 -> PixmanRegion32 -> IO ()
pixmanRegionSubtract reg sub = withRegion32 reg $ \regPtr ->
    withRegion32 sub $ \subPtr ->
        c_32_subtract regPtr regPtr subPtr
