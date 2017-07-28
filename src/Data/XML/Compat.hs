-- | Compatibility interface between `xml` and `xml-types`.

module Data.XML.Compat

where

import Data.Text
import qualified Data.Text as T

import Data.XML.Types

type Attr = (Name, [Content])

mkAttr :: Text -> Text -> Attr
mkAttr a b = (Name a Nothing Nothing, [ContentText b])

attrKey :: Attr -> Name
attrKey = fst

strContent :: Element -> Text
strContent = T.concat . elementText

unqual = id

unode = error "TODO unode is not implemented"

findChildren :: Name -> Element -> [Element]
findChildren n el = Prelude.filter ((n ==) . elementName) $ elementChildren el
