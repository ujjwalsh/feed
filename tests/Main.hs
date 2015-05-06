module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)
import Text.Feed.Export
import Text.Feed.Import
import Text.XML.Light

import Paths_feed

main :: IO ()
main = defaultMain [testCase "rss20" testRss20]

testRss20 :: Assertion
testRss20 = do
  putStrLn . ppTopElement . xmlFeed =<< parseFeedFromFile =<< getDataFileName "tests/files/rss20.xml"
  assertBool "OK" True
