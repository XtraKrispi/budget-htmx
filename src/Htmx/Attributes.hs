module Htmx.Attributes where

import Text.Blaze.Html5 (Attribute, AttributeValue, customAttribute)

hxGet :: AttributeValue -> Attribute
hxGet = customAttribute "hx-get"

hxTrigger :: AttributeValue -> Attribute
hxTrigger = customAttribute "hx-trigger"