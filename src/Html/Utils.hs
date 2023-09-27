module Html.Utils where

import Data.List (intersperse)
import Text.Blaze.Html5 (Attribute, AttributeValue)
import Text.Blaze.Html5.Attributes qualified as Attr

classes :: [AttributeValue] -> Attribute
classes = Attr.class_ . mconcat . intersperse " "
