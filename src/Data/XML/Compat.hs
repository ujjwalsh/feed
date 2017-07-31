-- Compatibility junk from `xml` types to `xml-types`

module Data.XML.Compat

where

import Data.Text
import Data.XML.Types

type Attr = (Name, [Content])

mkAttr :: Text -> Text -> Attr
mkAttr a b = (Name a Nothing Nothing, [ContentText b])

attrKey :: Attr -> Name
attrKey = fst

unqual = id

unode = error "TODO unode is not implemented"
