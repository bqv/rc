{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Wayland.Scanner.Marshal
where

#include <wayland-util.h>

import Data.ByteString (ByteString, packCStringLen, useAsCString)
import Data.ByteString.Unsafe (unsafePackCString, unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (Storable (..))
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.Types (Fd)

import Graphics.Wayland.Resource (WlResource, resourceGetClient)

import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Scanner.Types

import qualified Data.Text.Encoding as E
import qualified Language.Haskell.TH as TH

newtype WlArray = WlArray { unArray :: ByteString }

instance Storable WlArray where
    sizeOf _ = #{size struct wl_array}
    alignment _ = #{alignment struct wl_array}
    peek ptr = do
        size :: CSize <- #{peek struct wl_array, size} ptr
        ret <- packCStringLen . (,fromIntegral size) =<< #{peek struct wl_array, data} ptr
        pure $ WlArray ret
    poke ptr (WlArray bs) = unsafeUseAsCStringLen bs $ \(bsPtr, len) -> do
        #{poke struct wl_array, data} ptr bsPtr
        #{poke struct wl_array, size} ptr (fromIntegral len :: CSize)
        #{poke struct wl_array, alloc} ptr (fromIntegral len :: CSize)

data WlFixed

data WlMessage

argTypeToExp :: Monad m => ArgumentType -> Scanner m TH.Type
argTypeToExp IntArg = pure $ TH.ConT ''Int32
argTypeToExp UIntArg = pure $ TH.ConT ''Word32
argTypeToExp FixedArg = error "Can't handle fixed point arguments yet"
argTypeToExp StringArg = pure $ TH.ConT ''Text
argTypeToExp NullableStringArg = pure $ TH.AppT (TH.ConT ''Maybe) (TH.ConT ''Text)
argTypeToExp (ObjectArg str) = (\(v, _, _) -> v) <$> getObjectConvert str
argTypeToExp (NullableObjectArg t) = TH.AppT (TH.ConT ''Maybe) <$> argTypeToExp (ObjectArg t)
argTypeToExp (NewIdArg _) = pure $ TH.ConT ''Word32
argTypeToExp (NullableNewIdArg t) = TH.AppT (TH.ConT ''Maybe) <$> argTypeToExp (NewIdArg t)
argTypeToExp ArrayArg = pure $ TH.ConT ''ByteString
argTypeToExp NullableArrayArg = TH.AppT (TH.ConT ''Maybe) <$> argTypeToExp ArrayArg
argTypeToExp FdArg = pure $ TH.ConT ''Fd

decodePattern :: Monad m => ArgumentType -> TH.Name -> Scanner m TH.Pat
decodePattern arg name = TH.SigP (TH.VarP name) <$> argTypeToExp arg

withStringArg :: Ptr CString -> Maybe Text -> IO () -> IO ()
withStringArg ptr Nothing act = (poke ptr nullPtr) >> act
withStringArg ptr (Just txt) act =
    useAsCString (E.encodeUtf8 txt) $ \cStr -> do
        poke ptr cStr
        act

withArrayArg :: Ptr (Ptr WlArray) -> Maybe ByteString -> IO () -> IO ()
withArrayArg ptr Nothing act = (poke ptr nullPtr) >> act
withArrayArg ptr (Just array) act = with (WlArray array) $ \aPtr -> do
    poke ptr aPtr
    act

argTypeToDemarshalExp :: Monad m => ArgumentType -> TH.Exp -> TH.Exp -> TH.Exp -> Scanner m TH.Exp
argTypeToDemarshalExp FixedArg  _ _ _         = error "Can't encode FixdPoint values yet"
argTypeToDemarshalExp StringArg _ p e         = pure $ TH.AppE (TH.AppE (TH.VarE 'withStringArg) p) (TH.AppE (TH.ConE 'Just) e)
argTypeToDemarshalExp NullableStringArg _ p e = pure $ TH.AppE (TH.AppE (TH.VarE 'withStringArg) p) e
argTypeToDemarshalExp ArrayArg _ p e          = pure $ TH.AppE (TH.AppE (TH.VarE 'withArrayArg) p) (TH.AppE (TH.ConE 'Just) e)
argTypeToDemarshalExp NullableArrayArg _ p e  = pure $ TH.AppE (TH.AppE (TH.VarE 'withArrayArg) p) e
argTypeToDemarshalExp (ObjectArg str) t p e   = do
    convert <- (\(_, _, v) -> v) <$> getObjectConvert str
    let rpName = TH.mkName "resourcePtr"
        cName = TH.mkName "tClient"
        actName = TH.mkName "act"
    pure . TH.LamE [TH.VarP actName] $ TH.DoE
        [ TH.BindS (TH.VarP cName) $ TH.AppE (TH.VarE 'resourceGetClient) t
        , TH.BindS (TH.VarP rpName) $ TH.AppE (TH.AppE convert (TH.VarE cName)) e
        , TH.NoBindS $ TH.AppE (TH.AppE (TH.VarE '(>>)) (TH.AppE (TH.AppE (TH.VarE 'poke) p) (TH.VarE rpName))) (TH.VarE actName)
        ]
argTypeToDemarshalExp (NullableObjectArg str) _ p e   = do
    convert <- (\(_, _, v) -> v) <$> getObjectConvert str
    let rpName = TH.mkName "resourcePtr"
        unMaybe = TH.AppE (TH.VarE 'fromMaybe) (TH.VarE 'nullPtr)
    pure $ TH.DoE
        [ TH.BindS (TH.VarP rpName) $ TH.AppE (TH.AppE (TH.VarE 'traverse) convert) e
        , TH.NoBindS $ TH.AppE (TH.VarE '(>>)) (TH.AppE (TH.AppE (TH.VarE 'poke) p) (TH.AppE unMaybe $ TH.VarE rpName))
        ]
argTypeToDemarshalExp _ _ p e                 = pure $ TH.AppE (TH.VarE '(>>)) (TH.AppE (TH.AppE (TH.VarE 'poke) p) e)

foreign import ccall "wl_resource_post_event_array" c_post :: Ptr WlResource -> Word32 -> Ptr WlArgument -> IO ()

makePostClause :: Monad m => [ArgumentType] -> Integer -> Scanner m TH.Clause
makePostClause xs opcode = do
    let apName = TH.mkName "argumentsPtr"
        rpName = TH.mkName "targetPtr"
        argNames = take (length xs) $ map (TH.mkName . (++) "arg" . show) [0 :: Int ..]
        callocExp = (TH.AppE (TH.VarE 'allocaBytes) (TH.LitE (TH.IntegerL $ 8 * fromIntegral (length xs))))
        deMarshalExps (arg, i) argName = argTypeToDemarshalExp arg (TH.VarE rpName) (TH.AppE (TH.AppE (TH.VarE 'plusPtr) (TH.VarE apName)) (TH.LitE . TH.IntegerL $ i * 8)) (TH.VarE argName)
    exps <- sequence $ zipWith deMarshalExps (zip xs [0..]) argNames
    let act = TH.AppE (TH.AppE (TH.AppE (TH.VarE 'c_post) (TH.VarE rpName)) (TH.LitE $ TH.IntegerL opcode)) (TH.VarE apName)
        lam = TH.LamE [TH.VarP apName] $ foldr TH.AppE act exps
    pure $ TH.Clause (TH.VarP rpName: fmap TH.VarP argNames) (TH.NormalB (TH.AppE callocExp lam)) []

makePostFun :: Monad m => TH.Name -> [ArgumentType] -> Integer -> Scanner m [TH.Dec]
makePostFun name xs opcode = do
    clause <- makePostClause xs opcode
    tType <- takerType xs
    let rpType = TH.AppT TH.ArrowT (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlResource))
        funType = TH.AppT rpType tType
    pure [TH.SigD name  funType, TH.FunD name [clause]]


argTypeToMarshalExp :: Monad m => ArgumentType -> TH.Exp -> Scanner m TH.Exp
argTypeToMarshalExp FixedArg  _       = error "Can't decode FixdPoint values yet"
argTypeToMarshalExp StringArg e       = pure $
    let ptrName = TH.mkName "strPtr"
        bsName = TH.mkName "bs"
     in TH.DoE
        [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e
        , TH.BindS (TH.VarP bsName) $ TH.AppE (TH.VarE 'unsafePackCString) (TH.VarE ptrName)
        , TH.NoBindS (TH.AppE (TH.VarE 'pure) (TH.AppE (TH.VarE 'E.decodeUtf8) (TH.VarE bsName)))
        ]
argTypeToMarshalExp NullableStringArg e       = pure $
    let ptrName = TH.mkName "strPtr"
        bsName = TH.mkName "bs"
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
        falseStmt = TH.DoE
            [ TH.BindS (TH.VarP bsName) $ TH.AppE (TH.VarE 'unsafePackCString) (TH.VarE ptrName)
            , TH.NoBindS (TH.AppE (TH.VarE 'pure) (TH.AppE (TH.ConE 'Just) $ TH.AppE (TH.VarE 'E.decodeUtf8) (TH.VarE bsName)))
            ]
     in TH.DoE
        [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e
        , TH.NoBindS $ TH.CaseE (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE ptrName))
            [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) []
            , TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
            ]
        ]
argTypeToMarshalExp ArrayArg e    = pure $
    let ptrName = TH.mkName "arrayPtr"
        arrName = TH.mkName "array"
     in TH.DoE
        [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e
        , TH.BindS (TH.VarP arrName) $ TH.AppE (TH.VarE 'peek) (TH.VarE ptrName)
        , TH.NoBindS (TH.AppE (TH.VarE 'pure) (TH.AppE (TH.VarE 'unArray) (TH.VarE arrName )))
        ]
argTypeToMarshalExp NullableArrayArg e    = pure $
    let ptrName = TH.mkName "arrayPtr"
        arrName = TH.mkName "array"
        falseStmt = TH.DoE
            [ TH.BindS (TH.VarP arrName) $ TH.AppE (TH.VarE 'peek) (TH.VarE ptrName)
            , TH.NoBindS (TH.AppE (TH.VarE 'pure) $ TH.AppE (TH.ConE 'Just) (TH.AppE (TH.VarE 'unArray) (TH.VarE arrName )))
            ]
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
     in TH.DoE
        [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e
        , TH.NoBindS $ TH.CaseE (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE ptrName))
            [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) []
            , TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
            ]
        ]
argTypeToMarshalExp (ObjectArg str) e = do
    convert <- (\(_, v, _) -> v) <$> getObjectConvert str
    let rpName = TH.mkName "resourcePtr"
    pure $ TH.DoE
        [ TH.BindS (TH.SigP (TH.VarP rpName) (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlResource))) (TH.AppE (TH.VarE 'peek) e)
        , TH.NoBindS (TH.AppE convert (TH.VarE rpName))
        ]
argTypeToMarshalExp (NullableObjectArg str) e = do
    convert <- (\(_, v, _) -> v) <$> getObjectConvert str
    let objPtr = TH.mkName "ptrName"
        falseStmt = TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.ConE 'Just)) $ TH.AppE convert (TH.VarE objPtr)
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
    pure $ TH.DoE
        [ TH.BindS (TH.SigP (TH.VarP objPtr) (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlResource))) $ TH.AppE (TH.VarE 'peek) e
        , TH.NoBindS $ TH.CaseE (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE objPtr))
            [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) []
            , TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
            ]
        ]
argTypeToMarshalExp (NullableNewIdArg _) e = pure $
    let objPtr = TH.mkName "ptrName"
        falseStmt = TH.AppE (TH.VarE 'pure) $ TH.AppE (TH.ConE 'Just) (TH.VarE objPtr)
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
     in TH.DoE
        [ TH.BindS (TH.VarP objPtr) $ TH.AppE (TH.VarE 'peek) e
        , TH.NoBindS $ TH.CaseE (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.LitE $ TH.IntegerL 0))) (TH.VarE objPtr))
            [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) []
            , TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
            ]
        ]
argTypeToMarshalExp _ e               = pure $ TH.AppE (TH.VarE 'peek) e

argTypeToMarshal :: Monad m => ArgumentType -> TH.Name -> TH.Exp -> Scanner m TH.Stmt
argTypeToMarshal at name e = TH.BindS <$> decodePattern at name <*> argTypeToMarshalExp at e

data WlArgument

takerType :: Monad m => [ArgumentType] -> Scanner m TH.Type
takerType xs = do
    thTypes <- mapM argTypeToExp xs
    let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
    pure $ foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) resType thTypes

makeMarshalBody :: Monad m => TH.Name -> TH.Name -> [ArgumentType] -> Scanner m TH.Body
makeMarshalBody dataPtr funName xs = do
    let argNames = take (length xs) $ map (TH.mkName . (++) "arg" . show) [0 :: Int ..]
    let dataExp i = TH.AppE (TH.AppE (TH.VarE 'plusPtr) (TH.VarE dataPtr)) (TH.LitE . TH.IntegerL $ i * 8)
    let makeArgStmt (argtype, i) name = argTypeToMarshal argtype name (dataExp i)
    argStmts <- sequence $ zipWith makeArgStmt (zip xs [0..]) argNames
    let applyExp = foldl TH.AppE (TH.VarE funName) $ fmap TH.VarE argNames
    let successCase = TH.NoBindS applyExp
    pure $ TH.NormalB $ TH.DoE (argStmts ++ [successCase])

makeMarshalClause :: Monad m => [ArgumentType] -> Scanner m TH.Clause
makeMarshalClause xs = do
    let mpName = TH.mkName "messagePtr"
    let funName = TH.mkName "takerFun"
    body <- makeMarshalBody mpName funName xs
    pure $ TH.Clause [if null xs then TH.WildP else TH.VarP mpName, TH.VarP funName] body []

makeMarshaller :: Monad m => TH.Name -> [ArgumentType] -> Scanner m [TH.Dec]
makeMarshaller name xs = do
    let mpType = TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlArgument)
    let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
    tType <- takerType xs
    let funType = TH.AppT (TH.AppT TH.ArrowT mpType) $ TH.AppT (TH.AppT TH.ArrowT tType) resType
    clause <- makeMarshalClause xs
    pure [TH.SigD name funType, TH.FunD name [clause]]

