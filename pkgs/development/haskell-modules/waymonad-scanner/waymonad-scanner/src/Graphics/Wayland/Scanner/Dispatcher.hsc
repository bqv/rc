{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Wayland.Scanner.Dispatcher
where

import Graphics.Wayland.Server (Client (..))

import Control.Monad.Fail ()
import Data.IORef
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr, freeHaskellFunPtr)
import Foreign.StablePtr (castPtrToStablePtr, deRefStablePtr, castStablePtrToPtr, newStablePtr, freeStablePtr)

import Graphics.Wayland.Resource (WlResource)

import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.Marshal
import Utility

import qualified Language.Haskell.TH as TH

makeDispatchClause :: Monad m => (TH.Name, [TH.Name]) -> [[ArgumentType]] -> Scanner m TH.Clause
makeDispatchClause (dataTName, fs) xs = do
    let implName = TH.mkName "implPtr"
        opName = TH.mkName "opcode"
        argName = TH.mkName "wlArgs"
        dataName = TH.mkName "takers"
        makeMarshalName = TH.mkName . (++) "marshal" . show
        marshalNames = take (length xs) $ fmap makeMarshalName [0 ..]
    marshalDecs <- sequence $ zipWith makeMarshaller marshalNames xs
    let dataStmt = TH.BindS (TH.SigP (TH.VarP dataName) (TH.ConT dataTName)) $ TH.AppE (TH.VarE 'deRefStablePtr) $ TH.AppE (TH.VarE 'castPtrToStablePtr) (TH.VarE implName)
        makeMatch field index =
            let matchExp = TH.AppE (TH.AppE (TH.VarE $ makeMarshalName index) (TH.VarE argName)) (TH.AppE (TH.VarE field) (TH.VarE dataName))
             in TH.Match (TH.LitP (TH.IntegerL index)) (TH.NormalB matchExp) []
        failClause = TH.Match TH.WildP (TH.NormalB $ TH.AppE (TH.VarE 'pure) (TH.ConE '())) []
        clauses = zipWith makeMatch fs [0..] ++ [failClause]
        funExp = TH.DoE [dataStmt, TH.NoBindS (TH.CaseE (TH.VarE opName) clauses)]
    pure $ TH.Clause [TH.VarP implName, TH.WildP, TH.VarP opName, TH.WildP, TH.VarP argName] (TH.NormalB funExp) (concat marshalDecs)

makeDispatchRecord :: Monad m => String -> [(String, [ArgumentType])] -> Scanner m TH.Dec
makeDispatchRecord name xs = do
    let dataName = TH.mkName $ cleanName name ++ "Requests"
        bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
        makeField (field, args) = do
            tType <- takerType args
            pure $ (TH.mkName (replaceUnder name ++ "Request" ++ cleanName field), bang, tType)
    dataCon <- TH.RecC dataName <$> mapM makeField xs
    pure $ TH.DataD [] dataName [] Nothing [dataCon] []

dispatcherType :: TH.Type
dispatcherType =
    let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
        ptrType = TH.AppT (TH.ConT ''Ptr)
        thTypes = [ptrType (TH.TupleT 0), ptrType (TH.ConT ''WlResource), TH.ConT ''Word32, ptrType (TH.ConT ''WlMessage), ptrType (TH.ConT ''WlArgument)]
     in foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) resType thTypes

makeDispatcherForeigns :: String -> TH.Name -> [TH.Dec]
makeDispatcherForeigns str name =
    let cName = ("s_" ++ str ++ "Dispatcher")
        importType = TH.AppT (TH.ConT ''FunPtr) dispatcherType
        importName = TH.mkName $ str ++ "DispatcherPtr"
     in [ TH.ForeignD $ TH.ExportF TH.CCall cName name dispatcherType
        , TH.ForeignD $ TH.ImportF TH.CCall TH.Safe ('&':cName) importName importType
        ]

makeSetterType :: TH.Name -> TH.Type
makeSetterType name =
    let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
        ptrType = TH.AppT (TH.ConT ''Ptr)
        thTypes = [ptrType (TH.ConT ''WlResource), TH.ConT name, resType]
     in foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) resType thTypes

makeSetterBody :: TH.Name -> TH.Clause
makeSetterBody dispName =
    let resName = TH.mkName "resource"
        implName = TH.mkName "impl"
        destroyName = TH.mkName "destroy"
        body = TH.NormalB $
            TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.VarE 'setResourceDispatcher) (TH.VarE resName)) (TH.VarE implName)) (TH.VarE dispName)) (TH.VarE destroyName)
     in TH.Clause [TH.VarP resName, TH.VarP implName, TH.VarP destroyName] body []

makeDispatcher :: (Monad m, MonadFail m) => String -> [(String, [ArgumentType])] -> Scanner m [TH.Dec]
makeDispatcher name xs = do
    dataType@(TH.DataD _ dataName [] Nothing [TH.RecC _ fs] []) <- makeDispatchRecord name xs
    clause <- makeDispatchClause (dataName, map (\(n, _, _) -> n) fs) $ map snd xs
    let dispName = TH.mkName $ name ++ "Dispatcher"
        foreigns = makeDispatcherForeigns name dispName
        setterName = TH.mkName $ "set" ++ cleanName name ++ "Dispatcher"
        setterSig = TH.SigD setterName (makeSetterType dataName)
        setterFun = TH.FunD setterName [makeSetterBody $ TH.mkName $ name ++ "DispatcherPtr"]
        setter = [setterSig, setterFun]
        dispatcher = [TH.SigD dispName dispatcherType, TH.FunD dispName [clause]]
    pure $ dataType : setter ++ foreigns ++ dispatcher

foreign import ccall unsafe "wl_resource_set_dispatcher" c_set_dispatcher
    :: Ptr WlResource
    -> FunPtr (Ptr () -> Ptr WlResource -> Word32 -> Ptr WlMessage -> Ptr WlArgument -> IO ())
    -> Ptr () -- ^Implementation
    -> Ptr () -- ^Data
    -> FunPtr (Ptr WlResource -> IO ())
    -> IO ()

foreign import ccall "wrapper" mkDestroyHandler :: (Ptr WlResource -> IO ()) -> IO (FunPtr (Ptr WlResource -> IO ()))

setResourceDispatcher :: Ptr WlResource
                      -> a
                      -> FunPtr (Ptr () -> Ptr WlResource -> Word32 -> Ptr WlMessage -> Ptr WlArgument -> IO ())
                      -> IO ()
                      -> IO ()
setResourceDispatcher resource handlers dispatcher destroy = do
    sPtr <- newStablePtr handlers
    ref <- newIORef undefined
    destroyPointer <- mkDestroyHandler $ \_ -> do
        freeStablePtr sPtr
        freeHaskellFunPtr =<< readIORef ref
        destroy
    writeIORef ref destroyPointer
    c_set_dispatcher
        resource
        dispatcher
        (castStablePtrToPtr sPtr)
        nullPtr
        destroyPointer

data WlInterface

foreign import ccall "wl_resource_create" c_create :: Ptr Client -> Ptr WlInterface -> CInt -> Word32 -> IO (Ptr WlResource)

createResource :: Client -> Ptr WlInterface -> Int -> Word32 -> IO (Ptr WlResource)
createResource (Client cPtr) iface version name = c_create cPtr iface (fromIntegral version) name
