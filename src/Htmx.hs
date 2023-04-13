{-# LANGUAGE OverloadedStrings #-}

module Htmx where

import Data.Text (Text)
import Lucid.Base (Attribute, makeAttribute)

hxGet :: Text -> Attribute
hxGet = makeAttribute "hx-get"

hxPost :: Text -> Attribute
hxPost = makeAttribute "hx-post"

hxPut :: Text -> Attribute
hxPut = makeAttribute "hx-put"

hxPatch :: Text -> Attribute
hxPatch = makeAttribute "hx-patch"

hxDelete :: Text -> Attribute
hxDelete = makeAttribute "hx-delete"

hxSwap :: Text -> Attribute
hxSwap = makeAttribute "hx-swap"

hxTrigger :: Text -> Attribute
hxTrigger = makeAttribute "hx-trigger"

hxSwapOob :: Attribute
hxSwapOob = makeAttribute "hx-swap-oob" "true"

hxTarget :: Text -> Attribute
hxTarget = makeAttribute "hx-target"

hxBoost :: Attribute
hxBoost = makeAttribute "hx-boost" "true"

hxInclude :: Text -> Attribute
hxInclude = makeAttribute "hx-include"

hxParams :: Text -> Attribute
hxParams = makeAttribute "hx-params"