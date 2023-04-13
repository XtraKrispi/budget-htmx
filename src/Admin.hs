{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Admin where

import Admin.Definitions (editCreatePanel)
import Htmx (hxGet, hxTrigger)
import Lucid

renderAdmin :: Html ()
renderAdmin = div_ [class_ "flex flex-col space-y-4"] do
  div_ [] do
    h1_ [class_ "text-2xl"] do
      "Admin"
  div_ [id_ "edit-create-panel", class_ "bg-white border rounded-lg p-4 drop-shadow-lg"] do
    editCreatePanel Nothing
  div_ [hxGet "/api/definitions", hxTrigger "load, reload-definitions from:body"] ""