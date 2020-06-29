module Graphics.Wayland.Scanner.Types
where

data ArgumentType
    = IntArg
    | UIntArg
    | FixedArg
    | StringArg
    | NullableStringArg
    | ObjectArg String
    | NullableObjectArg String
    | NewIdArg String
    | NullableNewIdArg String
    | ArrayArg
    | NullableArrayArg
    | FdArg
    deriving (Show)

argTypeFromData :: String -> Bool -> Maybe String -> ArgumentType
argTypeFromData "int" _ _ = IntArg
argTypeFromData "uint" _ _ = UIntArg
argTypeFromData "fixed" _ _ = FixedArg
argTypeFromData "string" False _ = StringArg
argTypeFromData "string" True _ = NullableStringArg
argTypeFromData "object" False (Just s) = ObjectArg s
argTypeFromData "object" True (Just s) = NullableObjectArg s
argTypeFromData "new_id" False (Just s) = NewIdArg s
argTypeFromData "new_id" True (Just s) = NullableNewIdArg s
argTypeFromData "array" False _ = ArrayArg
argTypeFromData "array" True _ = NullableArrayArg
argTypeFromData "fd" _ _ = FdArg
argTypeFromData x _ _ = error $ "Can't decode " ++ x ++ " as argument type"

newtype WlEnum = WlEnum [(String, Int)]
newtype WlRequest = WlRequest [(String, ArgumentType)]
newtype WlEvent = WlEvent [(String, ArgumentType)]

data Interface = Interface [(String, WlEnum)] [(String, WlRequest)] [(String, WlEvent)]

data WlProtocol = WlProtocol String [(String, Interface, Int)]
