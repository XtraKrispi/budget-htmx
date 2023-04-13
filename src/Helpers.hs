{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Data.Text (Text, pack)
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Lucid (Attribute)
import Lucid.Base (makeAttribute)

tshow :: Show a => a -> Text
tshow = pack . show

emptyAttribute :: Attribute
emptyAttribute = makeAttribute "" ""

asCurrency :: Double -> Text
asCurrency = prettyF (PrettyCfg 2 (Just ',') '.')
