{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
module Graphics.Wayland.WlRoots.Output
    ( WlrOutput
    , outputEnable
    , outputDisable
    , isOutputEnabled
    , makeOutputCurrent
    , swapOutputBuffers
--  , getOutputPosition

    , effectiveResolution
    , destroyOutput

    , OutputMode(..)
    , setOutputMode

    , hasModes
    , getModes
    , getMode
    , getWidth
    , getHeight
    , getTransMatrix

    , OutputSignals(..)
    , getOutputSignals
    , getDataPtr

    , transformOutput
    , getOutputTransform

    , getEffectiveBox
    , getOutputBox
    , getOutputName
    , getOutputScale
    , setOutputScale

    , getMake
    , getModel
    , getSerial

    , destroyOutputGlobal
    , createOutputGlobal

    , scheduleOutputFrame
    , outputTransformedResolution
    , invertOutputTransform
    , composeOutputTransform
    , getOutputDamage
    , setOutputDamage
    , outputFromResource
    , outputResourceForClient
    , outputGetBackend
    )
where

#define WLR_USE_UNSTABLE
#include <time.h>
#include <wlr/types/wlr_output.h>

import Control.Monad (filterM)
import Data.ByteString.Unsafe (unsafePackCString)
import Data.Int (Int32)
--import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word32, Word8, Word64)
import Foreign.C.Error (throwErrnoIf_)
import Foreign.C.Types (CInt(..), CLong (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (Storable(..))

import Graphics.Pixman

import Graphics.Wayland.Resource (WlResource, resourceFromLink, resourceGetClient)
import Graphics.Wayland.Server (Client (..))
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.Server (OutputTransform(..))
import Graphics.Wayland.List (getListFromHead, isListEmpty, getListElems)

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data WlrOutput

getOutputName :: Ptr WlrOutput -> IO Text
getOutputName = fmap E.decodeUtf8 . unsafePackCString . #{ptr struct wlr_output, name}

makeMaybe :: Text -> Maybe Text
makeMaybe txt = if T.null txt then Nothing else Just txt

getMake :: Ptr WlrOutput -> IO (Maybe Text)
getMake = fmap (makeMaybe . E.decodeUtf8) . unsafePackCString . #{ptr struct wlr_output, make}

getModel :: Ptr WlrOutput -> IO (Maybe Text)
getModel = fmap (makeMaybe . E.decodeUtf8) . unsafePackCString . #{ptr struct wlr_output, model}

getSerial :: Ptr WlrOutput -> IO (Maybe Text)
getSerial = fmap (makeMaybe . E.decodeUtf8) . unsafePackCString . #{ptr struct wlr_output, serial}

{-
getOutputPosition :: Ptr WlrOutput -> IO Point
getOutputPosition ptr = do
    x :: Int32 <- #{peek struct wlr_output, lx} ptr
    y :: Int32 <- #{peek struct wlr_output, ly} ptr
    pure $ Point (fromIntegral x) (fromIntegral y)
-}

foreign import ccall unsafe "wlr_output_enable" c_output_enable :: Ptr WlrOutput -> Bool -> IO ()

outputEnable :: Ptr WlrOutput -> IO ()
outputEnable = flip c_output_enable True

outputDisable :: Ptr WlrOutput -> IO ()
outputDisable = flip c_output_enable False

isOutputEnabled :: Ptr WlrOutput -> IO Bool
isOutputEnabled = fmap (/= (0 :: Word8)) . #{peek struct wlr_output, enabled}

foreign import ccall unsafe "wlr_output_make_current" c_make_current :: Ptr WlrOutput -> Ptr CInt -> IO Word8
makeOutputCurrent :: Ptr WlrOutput -> IO (Maybe Int)
makeOutputCurrent out = alloca $ \ptr -> do
    ret <- c_make_current out ptr
    if ret == 0
        then pure Nothing
        else Just . fromIntegral <$> peek ptr


foreign import ccall unsafe "wlr_output_swap_buffers" c_swap_buffers :: Ptr WlrOutput -> Ptr () -> Ptr PixmanRegion32 -> IO Word8
swapOutputBuffers :: Ptr WlrOutput -> Maybe Integer -> Maybe (Ptr PixmanRegion32) -> IO Bool
swapOutputBuffers out time _ {-damage-} =
    let withTime = case time of
            Nothing -> ($ nullPtr)
            Just t -> \act -> allocaBytes #{size struct timespec} $ \ptr -> do
                    let secs :: Word64 = fromIntegral (t `div` 1e9)
                    let nsecs :: CLong = fromIntegral (t `mod` 1e9)
                    #{poke struct timespec, tv_sec} ptr secs
                    #{poke struct timespec, tv_nsec} ptr nsecs
                    act ptr
     in (/= 0) <$> withTime (\t -> c_swap_buffers out t {-(fromMaybe nullPtr damage)-} nullPtr)


foreign import ccall unsafe "wlr_output_destroy" c_output_destroy :: Ptr WlrOutput -> IO ()

destroyOutput :: Ptr WlrOutput -> IO ()
destroyOutput = c_output_destroy


foreign import ccall unsafe "wlr_output_effective_resolution" c_effective_resolution :: Ptr WlrOutput -> Ptr CInt -> Ptr CInt -> IO ()

effectiveResolution :: Ptr WlrOutput -> IO (Int, Int)
effectiveResolution output = alloca $ \width -> alloca $ \height -> do
    c_effective_resolution output width height
    width_val <- peek width
    height_val <- peek height
    pure (fromIntegral width_val, fromIntegral height_val)

getEffectiveBox :: Ptr WlrOutput -> IO WlrBox
getEffectiveBox ptr = do
    phys <- getOutputBox ptr
    (width, height) <- effectiveResolution ptr
    pure phys {boxWidth = width, boxHeight = height}

foreign import ccall "wlr_output_set_transform" c_output_transform :: Ptr WlrOutput -> CInt -> IO ()

transformOutput :: Ptr WlrOutput -> OutputTransform -> IO ()
transformOutput ptr (OutputTransform x) =
    c_output_transform ptr (fromIntegral x)

getOutputTransform :: Ptr WlrOutput -> IO OutputTransform
getOutputTransform ptr = do
    val :: CInt <- #{peek struct wlr_output, transform} ptr
    pure $ OutputTransform (fromIntegral val)

data OutputMode = OutputMode
    { modeWidth   :: Word32
    , modeHeight  :: Word32
    , modeRefresh :: Word32
    }
    deriving (Eq, Show)

instance Storable OutputMode where
    alignment _ = #{alignment struct wlr_output_mode}
    sizeOf _ = #{size struct wlr_output_mode}
    peek ptr = OutputMode
        <$> #{peek struct wlr_output_mode, width} ptr
        <*> #{peek struct wlr_output_mode, height} ptr
        <*> #{peek struct wlr_output_mode, refresh} ptr
    poke = error "We do not poke output modes"

foreign import ccall "wlr_output_set_mode" c_set_mode :: Ptr WlrOutput -> Ptr OutputMode -> IO Bool

setOutputMode :: Ptr OutputMode -> Ptr WlrOutput -> IO ()
setOutputMode mptr ptr =
    throwErrnoIf_ not "setOutputMode" $ c_set_mode ptr mptr


getWidth :: Ptr WlrOutput -> IO Int32
getWidth = #{peek struct wlr_output, width}

getHeight :: Ptr WlrOutput -> IO Int32
getHeight = #{peek struct wlr_output, height}

hasModes :: Ptr WlrOutput -> IO Bool
hasModes = fmap not . isListEmpty . #{ptr struct wlr_output, modes}

getModes :: Ptr WlrOutput -> IO [Ptr OutputMode]
getModes ptr = do
    let listptr = #{ptr struct wlr_output, modes} ptr
    getListFromHead listptr #{offset struct wlr_output_mode, link}

getMode :: Ptr WlrOutput -> IO (Maybe (Ptr OutputMode))
getMode ptr = do
    ret <- #{peek struct wlr_output, current_mode} ptr
    if ret == nullPtr
        then pure Nothing
        else pure $ Just ret

getTransMatrix :: Ptr WlrOutput -> Matrix
getTransMatrix =
    Matrix . #{ptr struct wlr_output, transform_matrix}

data OutputSignals = OutputSignals
    { outSignalFrame :: Ptr (WlSignal WlrOutput)
    , outSignalNeedsFrame :: Ptr (WlSignal WlrOutput)
    , outSignalPrecommit :: Ptr (WlSignal WlrOutput)
    , outSignalCommit :: Ptr (WlSignal WlrOutput)
    , outSignalPresent :: Ptr (WlSignal WlrOutput)
    , outSignalEnable :: Ptr (WlSignal WlrOutput)
    , outSignalMode :: Ptr (WlSignal WlrOutput)
    , outSignalScale :: Ptr (WlSignal WlrOutput)
    , outSignalTransform :: Ptr (WlSignal WlrOutput)
    , outSignalDescription :: Ptr (WlSignal WlrOutput)
    , outSignalDestroy :: Ptr (WlSignal WlrOutput)
    }

getOutputSignals :: Ptr WlrOutput -> OutputSignals
getOutputSignals ptr = OutputSignals
    { outSignalFrame = #{ptr struct wlr_output, events.frame} ptr
    , outSignalNeedsFrame = #{ptr struct wlr_output, events.needs_frame} ptr
    , outSignalPrecommit = #{ptr struct wlr_output, events.precommit} ptr
    , outSignalCommit = #{ptr struct wlr_output, events.commit} ptr
    , outSignalPresent = #{ptr struct wlr_output, events.present} ptr
    , outSignalEnable = #{ptr struct wlr_output, events.enable} ptr
    , outSignalMode = #{ptr struct wlr_output, events.mode} ptr
    , outSignalScale = #{ptr struct wlr_output, events.scale} ptr
    , outSignalTransform = #{ptr struct wlr_output, events.transform} ptr
    , outSignalDescription = #{ptr struct wlr_output, events.description} ptr
    , outSignalDestroy = #{ptr struct wlr_output, events.destroy} ptr
    }

getDataPtr :: Ptr WlrOutput -> Ptr (Ptr a)
getDataPtr = #{ptr struct wlr_output, data}


getOutputBox :: Ptr WlrOutput -> IO WlrBox
getOutputBox ptr = do
    width :: Word32 <- #{peek struct wlr_output, width} ptr
    height :: Word32 <- #{peek struct wlr_output, height} ptr
    pure $ WlrBox 0 0 (fromIntegral width) (fromIntegral height)

getOutputScale :: Ptr WlrOutput -> IO Float
getOutputScale = #{peek struct wlr_output, scale}

foreign import ccall "wlr_output_set_scale" c_set_scale :: Ptr WlrOutput -> Float -> IO ()

setOutputScale :: Ptr WlrOutput -> Float -> IO ()
setOutputScale = c_set_scale

foreign import ccall "wlr_output_create_global" c_create_global :: Ptr WlrOutput -> IO ()

createOutputGlobal :: Ptr WlrOutput -> IO ()
createOutputGlobal = c_create_global

foreign import ccall "wlr_output_destroy_global" c_destroy_global :: Ptr WlrOutput -> IO ()

destroyOutputGlobal :: Ptr WlrOutput -> IO ()
destroyOutputGlobal = c_destroy_global

foreign import ccall unsafe "wlr_output_transformed_resolution" c_transformed_resolution :: Ptr WlrOutput -> Ptr CInt -> Ptr CInt -> IO ()

outputTransformedResolution :: Ptr WlrOutput -> IO Point
outputTransformedResolution ptr = alloca $ \xptr -> alloca $ \yptr -> do
    c_transformed_resolution ptr xptr yptr
    x <- peek xptr
    y <- peek yptr
    pure $ Point (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "wlr_output_schedule_frame" c_schedule_frame :: Ptr WlrOutput -> IO ()

scheduleOutputFrame :: Ptr WlrOutput -> IO ()
scheduleOutputFrame = c_schedule_frame

foreign import ccall unsafe "wlr_output_transform_invert" c_transform_invert :: CInt -> CInt

invertOutputTransform :: OutputTransform -> OutputTransform
invertOutputTransform (OutputTransform val) = OutputTransform . fromIntegral $  c_transform_invert (fromIntegral val)

foreign import ccall unsafe "wlr_output_transform_compose" c_transform_compose :: CInt -> CInt -> CInt

composeOutputTransform :: OutputTransform -> OutputTransform -> OutputTransform
composeOutputTransform (OutputTransform l) (OutputTransform r) =
    OutputTransform . fromIntegral $ c_transform_compose (fromIntegral l) (fromIntegral r)

getOutputDamage :: Ptr WlrOutput -> PixmanRegion32
getOutputDamage = PixmanRegion32 . #{ptr struct wlr_output, damage}

setOutputDamage :: Ptr WlrOutput -> PixmanRegion32 -> IO ()
setOutputDamage ptr val = withRegion32 val $ #{poke struct wlr_output, damage} ptr

foreign import ccall unsafe "wlr_output_from_resource" c_from_resource :: Ptr WlResource -> IO (Ptr WlrOutput)

outputFromResource :: Ptr WlResource -> IO (Ptr WlrOutput)
outputFromResource = c_from_resource

outputResourceForClient :: Client -> Ptr WlrOutput -> IO (Ptr WlResource)
outputResourceForClient target output = do
    elems <- getListElems $ #{ptr struct wlr_output, resources} output
    ret <- flip filterM elems $ \link -> do
        client <- resourceGetClient $ resourceFromLink link
        pure (client == target)
    pure . resourceFromLink $ head ret

outputGetBackend :: Ptr WlrOutput -> IO (Ptr a)
outputGetBackend = #{peek struct wlr_output, backend}
