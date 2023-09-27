module Html.FullPage.Entries where

import Html.Layout qualified as Layout
import Route (Route)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Html

render :: Route -> Html
render activeRoute = Layout.withLayout activeRoute do
  Html.h1 "Hello"