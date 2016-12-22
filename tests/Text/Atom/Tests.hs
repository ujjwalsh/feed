module Text.Atom.Tests where

import Data.Maybe (isJust)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool)
import Text.XML

import Text.Atom.Feed
import Text.Feed.Export
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Utils

import Paths_feed

atomTests :: Test
atomTests =
  testGroup
    "Text.Atom"
    [mutuallyExclusive $ testGroup "Atom" [testFullAtomParse, testAtomAlternate]]

testFullAtomParse :: Test
testFullAtomParse = testCase "parse a complete atom file" testAtom
  where
    testAtom :: Assertion
    testAtom = do
      print . (fmap (renderText def)) . elementToDoc . xmlFeed =<<
        parseFeedFromFile =<< getDataFileName "tests/files/atom.xml"
      assertBool "OK" True

testAtomAlternate :: Test
testAtomAlternate = testCase "*unspecified* link relation means 'alternate'" testAlt
  where
    testAlt :: Assertion
    testAlt =
      let nullent = nullEntry "" (TextString "") ""
          item = AtomItem nullent {entryLinks = [nullLink ""]}
      in assertBool "unspecified means alternate" $ isJust $ getItemLink item
