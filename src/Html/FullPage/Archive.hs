module Html.FullPage.Archive where

import Html.Layout qualified as Layout
import Route (Route (ArchiveR))
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Html

render :: Html
render = Layout.withLayout ArchiveR do
  Html.h1 "Hello"