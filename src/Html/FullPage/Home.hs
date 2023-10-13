module Html.FullPage.Home where

import Html.Components (spinner)
import Html.Layout qualified as Layout
import Html.Utils qualified
import Htmx.Attributes qualified as Htmx
import Route (PartialRoute (..), Route (..), partialRouteToPath)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

render :: Html
render = Layout.withLayout HomeR do
  Html.div
    ! Html.Utils.classes ["flex", "flex-col", "space-y-4"]
    $ do
      Html.div
        ! Attr.id "scratch"
        ! Htmx.hxTrigger "load, refreshScratch from:body"
        ! Htmx.hxGet (partialRouteToPath ScratchP)
        $ Html.div
        ! Attr.class_ "hx-indicator"
        $ spinner
      Html.div
        ! Attr.id "instances"
        ! Htmx.hxGet (partialRouteToPath InstancesP)
        ! Htmx.hxTrigger "load, refreshInstances from:body"
        ! Html.Utils.classes
          [ "grow"
          ]
        $ Html.div
        ! Attr.class_ "hx-indicator"
        $ spinner
