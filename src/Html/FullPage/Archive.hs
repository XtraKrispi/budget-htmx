module Html.FullPage.Archive where

import Html.Components (spinner)
import Html.Layout qualified as Layout
import Html.Utils qualified
import Htmx.Attributes qualified as Htmx
import Route (PartialRoute (..), Route (ArchiveR), partialRouteToPath)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

render :: Html
render = Layout.withLayout ArchiveR do
  Html.div
    ! Html.Utils.classes ["flex", "flex-col", "space-y-4"]
    $ do
      Html.div
        ! Attr.id "archive"
        ! Htmx.hxTrigger "load"
        ! Htmx.hxGet (partialRouteToPath ArchiveP)
        $ Html.div
        ! Attr.class_ "hx-indicator"
        $ spinner
