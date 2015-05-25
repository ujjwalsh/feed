module Main (main) where

import Test.Framework (defaultMain)
import Text.RSS.Tests (rssTests)


main :: IO ()
main = defaultMain [
    rssTests
    ]
