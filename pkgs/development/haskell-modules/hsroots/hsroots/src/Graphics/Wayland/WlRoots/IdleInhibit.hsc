module Graphics.Wayland.WlRoots.IdleInhibit
    ( IdleInhibitManager (..)

    , idleInhibitCreate
    , idleInhibitDestroy
    , getIdleInhibitGlobal
    , getIdleInhibitSignal

    , IdleInhibitor (..)
    , getInhibitorDestroy
    , getInhibitorSurface
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_idle_inhibit_v1.h>

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable (..))
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.Global (WlGlobal)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

newtype IdleInhibitManager = IdleInhibitManager { unIIM :: Ptr IdleInhibitManager}


foreign import ccall "wlr_idle_inhibit_v1_create" c_create :: Ptr DisplayServer -> IO (Ptr IdleInhibitManager)

idleInhibitCreate :: DisplayServer -> IO IdleInhibitManager
idleInhibitCreate (DisplayServer dsp) = IdleInhibitManager <$> 
    throwErrnoIfNull "idleInhibitCreate" (c_create dsp)

foreign import ccall "wlr_idle_inhibit_v1_destroy" c_destroy :: Ptr IdleInhibitManager -> IO ()

idleInhibitDestroy :: IdleInhibitManager -> IO ()
idleInhibitDestroy (IdleInhibitManager ptr) = c_destroy ptr

getIdleInhibitGlobal :: IdleInhibitManager -> IO (Ptr WlGlobal)
getIdleInhibitGlobal =
    #{peek struct wlr_idle_inhibit_manager_v1, global} . unIIM

getIdleInhibitSignal :: IdleInhibitManager -> Ptr (WlSignal IdleInhibitor)
getIdleInhibitSignal = #{ptr struct wlr_idle_inhibit_manager_v1, events.new_inhibitor} . unIIM

newtype IdleInhibitor = IdleInhibitor { unII :: Ptr IdleInhibitor } deriving (Eq, Ord, Show)

getInhibitorDestroy :: IdleInhibitor -> Ptr (WlSignal IdleInhibitor)
getInhibitorDestroy = #{ptr struct wlr_idle_inhibitor_v1, events.destroy} . unII

getInhibitorSurface :: IdleInhibitor -> IO (Ptr WlrSurface)
getInhibitorSurface = #{peek struct wlr_idle_inhibitor_v1, surface} . unII
