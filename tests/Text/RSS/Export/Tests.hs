module Text.RSS.Export.Tests where

import Test.HUnit (Assertion, assertEqual)
import Test.Framework (Test, mutuallyExclusive, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.RSS.Export
import Text.XML.Light as XML
import Text.RSS.Export.Utils (createContent, createQName)
import Text.RSS.Export.Equals ()


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
        let hoursToSkip = []
        let expected = XML.Element {
            elName = createQName "skipHours"
            , elAttribs = [] :: [ Attr ]
            , elContent = [] :: [ Content ]
            , elLine = Nothing
        }

        assertEqual "empty skip hours" expected (xmlSkipHours hoursToSkip)



testCreateSkipHours :: Test
testCreateSkipHours = testCase "skip hours" skipHours
  where
    skipHours :: Assertion
    skipHours = do
        let hoursToSkip = [ 1, 2, 3 ]
        let expected = XML.Element {
            elName = createQName "skipHours"
            , elAttribs = [] :: [ Attr ]
            , elContent = [ hourElem 0, hourElem 1, hourElem 2 ] :: [ Content ]
            , elLine = Nothing
        } where hourElem ind = Elem (xmlLeaf "hour" $ show $ hoursToSkip !! ind)

        assertEqual "skip hours" expected (xmlSkipHours hoursToSkip)



testCreateEmptySkipDays :: Test
testCreateEmptySkipDays = testCase "empty list of skip days" emptySkipDays
  where
    emptySkipDays :: Assertion
    emptySkipDays = do
        let daysToSkip = []
        let expected = XML.Element {
            elName = createQName "skipDays"
            , elAttribs = [] :: [ Attr ]
            , elContent = [] :: [ Content ]
            , elLine = Nothing
        }

        assertEqual "empty skip days" expected (xmlSkipDays daysToSkip)



testCreateSkipDays :: Test
testCreateSkipDays = testCase "skip days" skipDays
  where
    skipDays :: Assertion
    skipDays = do
        let daysToSkip = [ "first day", "second day", "third day" ]
        let expected = XML.Element {
            elName = createQName "skipDays"
            , elAttribs = [] :: [ Attr ]
            , elContent = [ dayElem 0, dayElem 1, dayElem 2 ] :: [ Content ]
            , elLine = Nothing
        } where dayElem ind = Elem (xmlLeaf "day" $ daysToSkip !! ind)

        assertEqual "skip days" expected (xmlSkipDays daysToSkip)



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
