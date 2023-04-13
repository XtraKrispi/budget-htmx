{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Archive.Items where

import Data.Foldable (traverse_)
import Data.List qualified as L
import Data.Text (Text)
import Helpers (asCurrency, tshow)
import Lucid
import Types

renderItem :: Archive -> Html ()
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
    span_ $
      em_
        ( toHtml $ case i.archiveType of
            Paid -> "Paid" :: Text
            Skipped -> "Skipped" :: Text
        )

renderItems :: [Archive] -> Html ()
renderItems xs = div_ [class_ "space-y-4"] do
  traverse_ renderItem (L.sortOn (.date) xs)