{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Home.Items where

import Data.Foldable (traverse_)
import Data.List qualified as L
import Helpers (asCurrency, tshow)
import Htmx (hxPut)
import Lucid
import Types

renderItem :: Item -> Html ()
renderItem i = div_ [class_ "p-4 border rounded-lg drop-shadow-lg bg-white flex flex-col"] do
  div_ [class_ "flex gap-2 justify-between"] do
    a_
      [ href_ ""
      , class_ "hover:underline"
      ]
      (strong_ $ toHtml i.description)
    span_ $ strong_ (toHtml $ "$" <> asCurrency i.amount)
  div_ [class_ "flex justify-between"] do
    span_ (toHtml $ tshow i.date)
    div_ [class_ "flex space-x-2"] do
      button_ [hxPut ("/api/items/" <> tshow i.definitionId <> "/" <> tshow i.date <> "/pay"), class_ "hover:bg-blue-700 bg-blue-500 py-1 px-2 text-white rounded-lg"] "Pay"
      button_ [hxPut ("/api/items/" <> tshow i.definitionId <> "/" <> tshow i.date <> "/skip"), class_ "hover:bg-blue-700 bg-blue-500 py-1 px-2 text-white rounded-lg"] "Skip"

renderItems :: [Item] -> Html ()
renderItems xs = div_ [class_ "space-y-4"] do
  traverse_ renderItem (L.sortOn (.date) xs)