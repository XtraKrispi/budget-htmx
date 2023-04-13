{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Helpers (tshow)
import Htmx (hxGet, hxTrigger)
import Lucid
import Types (Archive)

renderArchive :: Html ()
renderArchive = div_ [class_ "flex flex-col space-y-4"] do
  div_ [] do
    h1_ [class_ "text-2xl"] do
      "Archive"

  div_ [hxGet "/api/archive", hxTrigger "load"] ""
