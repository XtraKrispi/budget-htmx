{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Home where

import Data.Time (Day, addDays)
import Helpers (tshow)
import Htmx (hxGet, hxInclude, hxParams, hxTarget, hxTrigger)
import Lucid

renderHome :: Day -> Html ()
renderHome date = div_ [class_ "flex flex-col space-y-4"] do
  div_ [] do
    h1_ [class_ "text-2xl"] do
      "Home"
  div_ do
    input_
      [ type_ "date"
      , name_ "endDate"
      , value_ (tshow (addDays 20 date))
      , hxGet "/api/items"
      , hxTarget "#items"
      , hxParams "*"
      ]
  div_ [class_ "flex gap-4"] do
    div_
      [ id_ "items"
      , hxGet "/api/items"
      , hxInclude "[name='endDate']"
      , hxParams "*"
      , hxTrigger "load, load-items"
      , class_ "grow"
      ]
      ""
    div_ [class_ "grow"] "extra"
