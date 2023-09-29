module Html.FullPage.Home where

import Html.Layout qualified as Layout
import Route (Route (..))
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Html

render :: Html
render = Layout.withLayout HomeR do
  Html.h1 "Home"