module Html.Partials.Scratch where

import Data.Text.Format.Numbers (PrettyCfg (..), prettyF)
import Html.Utils qualified
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

render :: Double -> Html
render amt =
  Html.div
    ! Attr.id "entries"
    $ Html.ul
    ! Html.Utils.classes ["space-y-4"]
    $ Html.span
    $ do
      "Amount until next payday: "
      Html.text
        $ "$"
        <> prettyF (PrettyCfg 2 (Just ',') '.') amt
