module Html.FullPage.Entries where

import Html.Components (spinner)
import Html.Layout qualified as Layout
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
  Html.div $ do
    "New Entry Section"
    Html.form mempty

  Html.div
    ! Htmx.hxGet (partialRouteToPath EntriesP)
    ! Htmx.hxTrigger "load"
    $ do
      Html.div
        ! Attr.class_ "hx-indicator"
        $ spinner