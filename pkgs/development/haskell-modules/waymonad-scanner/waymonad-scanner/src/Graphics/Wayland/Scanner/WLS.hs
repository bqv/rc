{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Scanner.WLS
    ( Scanner (..)
    , ScannerEnv (..)
    , getObjectConvert
    , scannerIO
    , runScanner
    )
where

import Control.Monad.Fail ()
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Resource (WlResource)

import qualified Data.Map as M
import qualified Language.Haskell.TH as TH

type ObjectMap = Map String (TH.Type, TH.Exp, TH.Exp)

data ScannerEnv = ScannerEnv
    { scannerObjectMap :: ObjectMap
    }

newtype Scanner m a = Scanner (ReaderT ScannerEnv m a)
    deriving (Functor, Applicative, Monad, MonadReader ScannerEnv, MonadFail)

getObjectMap :: Monad m => Scanner m ObjectMap
getObjectMap = scannerObjectMap <$> ask

getObjectConvert :: Monad m => String -> Scanner m (TH.Type, TH.Exp, TH.Exp)
getObjectConvert name = do
    oMap <- getObjectMap
    pure $ fromMaybe (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlResource), TH.VarE 'pure, TH.LamE [TH.WildP] (TH.VarE 'pure)) $ M.lookup name oMap

scannerIO :: IO a -> Scanner TH.Q a
scannerIO = Scanner . lift . TH.runIO

runScanner :: Scanner m a -> ScannerEnv -> m a
runScanner (Scanner act) env = runReaderT act env
