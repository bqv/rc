module Graphics.Wayland.WlRoots.ExportDMABuf
    ( ExportDMABufManager (..)

    , createDMAExporter
    , destroyDMAExporter
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_export_dmabuf_v1.h>

import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer (..))

newtype ExportDMABufManager = ExportDMABufManager (Ptr ExportDMABufManager)

foreign import ccall unsafe "wlr_export_dmabuf_manager_v1_create" c_create :: Ptr DisplayServer -> IO (Ptr ExportDMABufManager)

createDMAExporter :: DisplayServer -> IO ExportDMABufManager
createDMAExporter (DisplayServer ptr) = ExportDMABufManager <$>
    throwErrnoIfNull "createDMAExporter" (c_create ptr)

foreign import ccall unsafe "wlr_export_dmabuf_manager_v1_destroy" c_destroy :: Ptr ExportDMABufManager -> IO ()

destroyDMAExporter :: ExportDMABufManager -> IO ()
destroyDMAExporter (ExportDMABufManager ptr) = c_destroy ptr
