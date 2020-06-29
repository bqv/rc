{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.Scanner
where

import Foreign.Ptr (Ptr)
import Control.Monad.Fail ()
import Control.Monad.Trans (MonadTrans(lift))

import Graphics.Wayland.Scanner.Dispatcher
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Scanner.XML

import Utility

import qualified Language.Haskell.TH as TH

#if MIN_VERSION_template_haskell(2, 12, 0)
import System.Process (readProcess)
import qualified Language.Haskell.TH.Syntax as THS
#endif

makeInterfaceGetter :: String -> TH.Dec
makeInterfaceGetter iface =
    let ifaceName = iface ++ "_interface"
        funName = TH.mkName $ replaceUnder iface ++ "Interface"
        importType = TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlInterface)
     in TH.ForeignD $ TH.ImportF TH.CCall TH.Safe ('&':ifaceName) funName importType

makeInterfaceDecls :: (Monad m, MonadFail m) => (String, Interface, Int) -> Scanner m [TH.Dec]
makeInterfaceDecls (name, (Interface _ reqs evts), _) = do
    reqD <- if null reqs
        then pure []
        else makeDispatcher name $ map (\(n, WlRequest x) -> (n, map snd x)) reqs
    evtD <- mapM (\((n, WlEvent x), i) ->  makePostFun (TH.mkName $ replaceUnder name ++ "Post" ++ cleanName n) (map snd x) i) $ zip evts [0 ..]
    pure $ makeInterfaceGetter name : reqD ++ concat evtD

protocolFromFile :: String -> Scanner TH.Q [TH.Dec]
protocolFromFile file = do
    WlProtocol _ ifaces <- scannerIO $ protFromFile file
    ret <- mapM makeInterfaceDecls ifaces
    Scanner . lift $ generateInterface file
    pure $ concat ret

generateInterface :: String -> TH.Q ()
#if MIN_VERSION_template_haskell(2, 12, 0)
generateInterface file = do
    content <- TH.runIO $ readProcess "wayland-scanner" ["code", file, "/dev/stdout"] ""
    THS.addForeignSource THS.LangC content
#else
generateInterface _ = pure ()
#endif
