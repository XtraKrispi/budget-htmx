module Html.FullPage.Entries where

import Html.Components (spinner)
import Html.Layout qualified as Layout
import Html.Partials.Entries qualified as Partial
import Html.Utils qualified as Utils
import Htmx.Attributes qualified as Htmx
import Route (PartialRoute (..), Route (EntriesR), partialRouteToPath)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

render :: Html
render = Layout.withLayout EntriesR do
  Html.h1
    ! Utils.classes
      [ "text-2xl"
      , "font-bold"
      ]
    $ "Entries"
  Html.div
    ! Utils.classes
      [ "flex"
      , "justify-between"
      ]
    $ do
      Html.div
        ! Utils.classes ["w-[45%]"]
        ! Htmx.hxGet (partialRouteToPath EntriesP)
        ! Htmx.hxTarget "#entries"
        ! Htmx.hxTrigger "load"
        $ do
          Html.div
            ! Attr.id "entries"
            $ Html.div
            ! Attr.class_ "hx-indicator"
            $ spinner
      Html.div
        ! Utils.classes
          [ "w-[45%]"
          , "flex"
          , "flex-col"
          , "space-y-4"
          , "p-4"
          , "border"
          , "border-gray-200"
          , "rounded-md"
          , "shadow-md"
          ]
        $ do
          Html.div "New Entry"
          Partial.entryForm Nothing
