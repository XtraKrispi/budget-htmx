module Html.Partials.Entries where

import Database (Entry)
import Database.Persist (Entity)
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as Html

render :: [Entity Entry] -> Html
render _ = Html.div $ Html.text "Hello"