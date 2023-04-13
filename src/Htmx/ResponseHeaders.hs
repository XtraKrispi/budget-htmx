{-# LANGUAGE OverloadedStrings #-}

module Htmx.ResponseHeaders where

import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, addHeader)

hxTriggerResponse :: Text -> ActionM ()
hxTriggerResponse = addHeader "HX-Trigger"