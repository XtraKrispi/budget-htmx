module Htmx.Attributes where

import Text.Blaze.Html5 (Attribute, AttributeValue, customAttribute)

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