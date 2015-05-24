module Text.RSS.Export.Utils where

import Text.RSS.Export
import Text.XML.Light as XML

createContent :: String -> Content
createContent txt = Text blank_cdata { cdData = txt }

createQName :: String -> QName
createQName txt = XML.QName{ qName = txt, qURI = Nothing, qPrefix = Nothing }