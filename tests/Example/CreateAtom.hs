module Example.CreateAtom
  ( createAtom
  ) where

import Prelude ()
import Prelude.Compat hiding (take)

import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import Text.RSS.Utils

import Data.Text
import Data.Text.Lazy (toStrict)
import Text.XML

createAtom :: Maybe Text
createAtom = feed examplePosts

examplePosts :: [(Text, Text, Text)] -- Date, URL, Content
examplePosts =
  [ ("2000-02-02T18:30:00Z", "http://example.com/2", "Bar.")
  , ("2000-01-01T18:30:00Z", "http://example.com/1", "Foo.")
  ]

feed :: [(Text, Text, Text)] -> Maybe Text
feed posts =
  fmap (toStrict . renderText def) $
  elementToDoc $
  Export.xmlFeed $
  fd {Atom.feedEntries = fmap toEntry posts, Atom.feedLinks = [Atom.nullLink "http://example.com/"]}
  where
    fd :: Atom.Feed
    fd =
      Atom.nullFeed
        "http://example.com/atom.xml" -- ID
        (Atom.TextString "Example Website") -- Title
        (case posts -- Updated
               of
           (latestPostDate, _, _):_ -> latestPostDate
           _ -> "")
    toEntry :: (Text, Text, Text) -> Atom.Entry
    toEntry (date, url, content) =
      (Atom.nullEntry
         url -- The ID field. Must be a link to validate.
         (Atom.TextString (take 20 content)) -- Title
         date)
      { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "J. Smith"}]
      , Atom.entryLinks = [Atom.nullLink url]
      , Atom.entryContent = Just (Atom.HTMLContent content)
      }
