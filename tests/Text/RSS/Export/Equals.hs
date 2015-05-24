module Text.RSS.Export.Equals where

import Text.RSS.Export
import Text.XML.Light as XML

instance Eq XML.Element where
   (XML.Element name1 attribs1 content1 line1) == (XML.Element name2 attribs2 content2 line2) =
        (name1 == name2) && (attribs1 == attribs2) && (content1 == content2) && (line1 == line2)

instance Eq Content where
    (Text data1) == (Text data2) = (data1 == data2)
    (CRef text1) == (CRef text2) = (text1 == text2)
    (Elem elem1) == (Elem elem2) = (elem1 == elem2)
    (_) == (_) = False

instance Eq CData where
    (CData verbatim1 data1 line1) == (CData verbatim2 data2 line2) =
        (verbatim1 == verbatim2) && (data1 == data2) && (line1 == line2)