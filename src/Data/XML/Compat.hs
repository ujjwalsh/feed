{-# LANGUAGE FlexibleInstances #-}

-- | Compatibility interface between `xml` and `xml-types`.

module Data.XML.Compat

where

import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types

import Safe

type Attr = (Name, [Content])

mkAttr :: Text -> Text -> Attr
mkAttr a b = (Name a Nothing Nothing, [ContentText b])

attrKey :: Attr -> Name
attrKey = fst

strContent :: Element -> Text
strContent = T.concat . elementText

class ToNode t where
  unode :: Name -> t -> Element

instance ToNode [Attr] where
  unode n as = Element n as []

instance ToNode [Element] where
  unode n = Element n [] . map NodeElement

instance ToNode ([Attr], Text) where
  unode n (as, t) = Element n as [NodeContent $ ContentText t]

instance ToNode Text where
  unode n t = unode n ([] :: [Attr], t)

findChildren :: Name -> Element -> [Element]
findChildren n el = Prelude.filter ((n ==) . elementName) $ elementChildren el

findChild :: Name -> Element -> Maybe Element
findChild = (headMay .) <$> findChildren

