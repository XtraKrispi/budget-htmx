module Html.Utils where

import Data.List (intersperse)
import Database (Key)
import Database.Persist (ToBackendKey)
import Database.Persist.Postgresql (SqlBackend, fromSqlKey)
import Text.Blaze.Html5 (Attribute, AttributeValue, Html, stringValue, (!))
import Text.Blaze.Html5.Attributes qualified as Attr

classes :: [AttributeValue] -> Attribute
classes = Attr.class_ . mconcat . intersperse " "

classesPred :: [(AttributeValue, Bool)] -> Attribute
classesPred = classes . fmap fst . filter snd

withAttributes :: [Attribute] -> Html -> Html
withAttributes attrs content = foldr (flip (!)) content attrs

keyAttribute :: (ToBackendKey SqlBackend record) => Key record -> AttributeValue
keyAttribute = stringValue . show . fromSqlKey