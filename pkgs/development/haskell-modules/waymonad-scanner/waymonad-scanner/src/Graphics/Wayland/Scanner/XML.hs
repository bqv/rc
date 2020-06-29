module Graphics.Wayland.Scanner.XML
    ( parseProtocol
    , protFromFile
    )
where

import Data.Maybe (fromMaybe, mapMaybe)
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Types
import Text.XML.Light.Proc

import Graphics.Wayland.Scanner.Types

--data Interface = Interface [(String, WlEnum)] [(String, WlRequest)] [(String, WlEvent)]

argTypeFromXml :: Element -> ArgumentType
argTypeFromXml e =
    let tStr = findAttr (QName "type" Nothing Nothing) e
        nullAttr = findAttr (QName "allow-null" Nothing Nothing) e
        nullable = maybe False ("true" ==) nullAttr
        tName = fromMaybe (error "Couldn't find type attribute") tStr
        iface = findAttr (QName "interface" Nothing Nothing) e
     in argTypeFromData tName nullable iface


argFromElem :: Element -> Maybe (String, ArgumentType)
argFromElem el =  case elName el  == QName "arg" Nothing Nothing of
    False -> Nothing
    True ->
        let name = fromMaybe (error "Can't find argument name") $ findAttr (QName "name" Nothing Nothing) el
            aType = argTypeFromXml el
         in Just (name, aType)

eventsFromElem :: Element -> [(String, WlEvent)]
eventsFromElem el =
    let getReq el' = case elName el'  == QName "event" Nothing Nothing of
            False -> Nothing
            True ->
                let name = fromMaybe (error "Can't find request name") $ findAttr (QName "name" Nothing Nothing) el'
                    args = mapMaybe argFromElem $ elChildren el'
                 in Just (name, WlEvent args)
     in mapMaybe getReq $ elChildren el

requestsFromElem :: Element -> [(String, WlRequest)]
requestsFromElem el =
    let getReq el' = case elName el'  == QName "request" Nothing Nothing of
            False -> Nothing
            True ->
                let name = fromMaybe (error "Can't find request name") $ findAttr (QName "name" Nothing Nothing) el'
                    args = mapMaybe argFromElem $ elChildren el'
                 in Just (name, WlRequest args)
     in mapMaybe getReq $ elChildren el

interfaceFromElem :: Element -> Maybe (String, Interface, Int)
interfaceFromElem el = case elName el == QName "interface" Nothing Nothing of
    False -> Nothing
    True ->
        let name = fromMaybe (error "Can't find interface name") $ findAttr (QName "name" Nothing Nothing) el
            version = fromMaybe (error "Can't find interface version") $ findAttr (QName "version" Nothing Nothing) el
         in Just (name, Interface [] (requestsFromElem el) (eventsFromElem el), read version)

protFromElem :: Element -> WlProtocol
protFromElem el =
    let name = fromMaybe (error "Can't find protocol name") $ findAttr (QName "name" Nothing Nothing) el
        elems = mapMaybe interfaceFromElem $ elChildren el
     in WlProtocol name elems

parseProtocol :: XmlSource s => s -> WlProtocol
parseProtocol src = protFromElem $
    fromMaybe (error "Failed to parse XML document") $ parseXMLDoc  src

protFromFile :: String -> IO WlProtocol
protFromFile file = do
    content <- readFile file
    pure $ parseProtocol content
