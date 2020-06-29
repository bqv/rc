module Graphics.Wayland.WlRoots.InputInhibitor
    ( WlrInputInhibitor (..)
    , createInputInhibitor
    , destroyInputInhibitor

    , getInhibitClient
    , WlrInputInhibitEvents (..)
    , getInputInhibitorEvents
    , getInputInhibitGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_input_inhibitor.h>

import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer (..), Client (..))
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.Global (WlGlobal)

data WlrInputInhibitor = WlrInputInhibitor (Ptr WlrInputInhibitor)

foreign import ccall unsafe "wlr_input_inhibit_manager_create" c_create :: Ptr DisplayServer -> IO (Ptr WlrInputInhibitor)

createInputInhibitor :: DisplayServer -> IO WlrInputInhibitor
createInputInhibitor (DisplayServer dsp) = WlrInputInhibitor <$>
    throwErrnoIfNull "createInputInhibitor" (c_create dsp)

foreign import ccall unsafe "wlr_input_inhibit_manager_destroy" c_destroy :: Ptr WlrInputInhibitor -> IO ()


destroyInputInhibitor :: WlrInputInhibitor -> IO ()
destroyInputInhibitor (WlrInputInhibitor ptr) = c_destroy ptr

getInhibitClient :: WlrInputInhibitor -> IO (Maybe Client)
getInhibitClient (WlrInputInhibitor ptr) = do
    ret <- #{peek struct wlr_input_inhibit_manager, active_client} ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just $ Client ret

data WlrInputInhibitEvents = WlrInputInhibitEvents
    { inputInhibitEventsActivate   :: Ptr (WlSignal WlrInputInhibitor)
    , inputInhibitEventsDeactivate :: Ptr (WlSignal WlrInputInhibitor)
    }

getInputInhibitorEvents :: WlrInputInhibitor -> WlrInputInhibitEvents
getInputInhibitorEvents (WlrInputInhibitor ptr) = WlrInputInhibitEvents
    { inputInhibitEventsActivate   = #{ptr struct wlr_input_inhibit_manager, events.activate} ptr
    , inputInhibitEventsDeactivate = #{ptr struct wlr_input_inhibit_manager, events.deactivate} ptr
    }

getInputInhibitGlobal :: WlrInputInhibitor -> IO (Ptr WlGlobal)
getInputInhibitGlobal (WlrInputInhibitor ptr) =
    #{peek struct wlr_input_inhibit_manager, global} ptr
