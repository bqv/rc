module Utility
where

import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, nullPtr)

import qualified Data.Text.Encoding as E


textFromPtr :: Ptr CChar -> IO Text
textFromPtr = fmap E.decodeUtf8 . unsafePackCString

textFromNull :: Ptr CChar -> IO (Maybe Text)
textFromNull ptr = if ptr == nullPtr
    then pure Nothing
    else Just <$> textFromPtr ptr
