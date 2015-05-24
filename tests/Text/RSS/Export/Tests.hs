{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.RSS.Export.Tests where

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.RSS.Export
import Text.XML.Light as XML
import Text.RSS.Export.Utils (createContent, createQName)
import Text.RSS.Export.Equals

rssExportTests :: Test
rssExportTests = testGroup "Text.RSS.Export"
    [ mutuallyExclusive $ testGroup "RSS export"
        [ testCreateEmptySkipHours
        , testCreateSkipHours
        , testCreateEmptySkipDays
        , testCreateSkipDays
        , testCreateXMLAttr
        , testCreateXMLLeaf
        ]
    ]

testCreateEmptySkipHours :: Test
testCreateEmptySkipHours = testCase "empty list of skip hours" emptySkipHours
  where
    emptySkipHours :: Assertion
    emptySkipHours = do
        let skipHours = []
        let expected = XML.Element {
            elName = createQName "skipHours"
            , elAttribs = [] :: [ Attr ]
            , elContent = [] :: [ Content ]
            , elLine = Nothing
        }

        assertEqual "empty skip hours" expected (xmlSkipHours skipHours)


testCreateSkipHours :: Test
testCreateSkipHours = testCase "skip hours" skipHours
  where
    skipHours :: Assertion
    skipHours = do
        let skipHours = [ 1, 2, 3 ]
        let expected = XML.Element {
            elName = createQName "skipHours"
            , elAttribs = [] :: [ Attr ]
            , elContent = [ hourElem 0, hourElem 1, hourElem 2 ] :: [ Content ]
            , elLine = Nothing
        } where hourElem id = Elem (xmlLeaf "hour" $ show $ skipHours !! id)

        assertEqual "skip hours" expected (xmlSkipHours skipHours)



testCreateEmptySkipDays :: Test
testCreateEmptySkipDays = testCase "empty list of skip days" emptySkipDays
  where
    emptySkipDays :: Assertion
    emptySkipDays = do
        let skipDays = []
        let expected = XML.Element {
            elName = createQName "skipDayss"
            , elAttribs = [] :: [ Attr ]
            , elContent = [] :: [ Content ]
            , elLine = Nothing
        }

        assertEqual "empty skip days" expected (xmlSkipDays skipDays)


testCreateSkipDays :: Test
testCreateSkipDays = testCase "skip days" skipDays
  where
    skipDays :: Assertion
    skipDays = do
        let skipDays = [ "first day", "second day", "third day" ]
        let expected = XML.Element {
            elName = createQName "skipDayss"
            , elAttribs = [] :: [ Attr ]
            , elContent = [ dayElem 0, dayElem 1, dayElem 2 ] :: [ Content ]
            , elLine = Nothing
        } where dayElem id = Elem (xmlLeaf "day" $ skipDays !! id)

        assertEqual "skip days" expected (xmlSkipDays skipDays)



testCreateXMLAttr :: Test
testCreateXMLAttr = testCase "create xml attr" createXMLAttr
  where
    createXMLAttr :: Assertion
    createXMLAttr = do
        let tg = "attr"
        let txt = "example of attr value"
        let expected = XML.Attr {
            attrKey = createQName tg
            , attrVal = txt
        }

        assertEqual "create a leaf" expected (xmlAttr tg txt)


testCreateXMLLeaf :: Test
testCreateXMLLeaf = testCase "create xml leaf" createXMLLeaf
  where
    createXMLLeaf :: Assertion
    createXMLLeaf = do
        let tg = "leaf"
        let txt = "example of leaf text"
        let expected = XML.Element {
                               elName = createQName tg
                               , elAttribs = [] :: [ Attr ]
                               , elContent = [ createContent txt ] :: [ Content ]
                               , elLine = Nothing
                               }

        assertEqual "create a leaf" expected (xmlLeaf tg txt)