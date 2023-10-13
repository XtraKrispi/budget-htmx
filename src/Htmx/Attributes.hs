module Htmx.Attributes where

import Data.Text (Text, intercalate)
import Text.Blaze.Html5 (Attribute, AttributeValue, ToValue (toValue), customAttribute)

hxGet :: AttributeValue -> Attribute
hxGet = customAttribute "hx-get"

hxPost :: AttributeValue -> Attribute
hxPost = customAttribute "hx-post"

hxDelete :: AttributeValue -> Attribute
hxDelete = customAttribute "hx-delete"

hxPut :: AttributeValue -> Attribute
hxPut = customAttribute "hx-put"

hxSwap :: AttributeValue -> Attribute
hxSwap = customAttribute "hx-swap"

hxTrigger :: AttributeValue -> Attribute
hxTrigger = customAttribute "hx-trigger"

hxTarget :: AttributeValue -> Attribute
hxTarget = customAttribute "hx-target"

hxSwapOob :: Attribute
hxSwapOob = customAttribute "hx-swap-oob" "true"

hxVals :: [(Text, Text)] -> Attribute
hxVals vals = customAttribute "hx-vals" $ toValue $ (\t -> "{" <> t <> "}") $ intercalate "," $ (\(key, val) -> "\"" <> key <> "\":" <> "\"" <> val <> "\"") <$> vals