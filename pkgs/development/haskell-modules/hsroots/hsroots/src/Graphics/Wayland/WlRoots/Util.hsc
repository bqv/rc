module Graphics.Wayland.WlRoots.Util
    ( LogPriority (..)
    , setLogPrio
    )
where

#include <wlr/util/log.h>

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)

data LogPriority
    = Silent
    | Error
    | Info
    | Debug
    deriving (Show, Eq)

logPrioToInt :: Num a => LogPriority -> a
logPrioToInt Silent = #{const WLR_SILENT}
logPrioToInt Error  = #{const WLR_ERROR}
logPrioToInt Info   = #{const WLR_INFO}
logPrioToInt Debug  = #{const WLR_DEBUG}

foreign import ccall unsafe "wlr_log_init" c_log_init :: CInt -> Ptr a -> IO ()

setLogPrio :: LogPriority -> IO ()
setLogPrio prio =
    c_log_init (logPrioToInt prio) nullPtr
