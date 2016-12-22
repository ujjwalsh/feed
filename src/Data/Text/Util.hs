module Data.Text.Util
  ( readInt
  ) where

import Data.Text
import Data.Text.Read

readInt :: Text -> Maybe Integer
readInt s =
  case decimal s of
    Right (x, _) -> Just x
    _ -> Nothing
