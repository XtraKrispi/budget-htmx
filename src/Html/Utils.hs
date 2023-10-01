module Html.Utils where

import Data.List (intersperse)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html, (!))
import Text.Blaze.Html5.Attributes qualified as Attr

classes :: [AttributeValue] -> Attribute
classes = Attr.class_ . mconcat . intersperse " "

classesPred :: [(AttributeValue, Bool)] -> Attribute
classesPred = classes . fmap fst . filter snd

withAttributes :: [Attribute] -> Html -> Html
withAttributes attrs content = foldr (flip (!)) content attrs
