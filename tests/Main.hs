module Main
  ( main
  ) where

import Example (exampleTests)
import Test.Framework (defaultMain)
import Text.Atom.Tests (atomTests)
import Text.Feed.Util.Tests (feedUtilTests)
import Text.RSS.Tests (rssTests)

main :: IO ()
main = defaultMain [rssTests, atomTests, feedUtilTests, exampleTests]
