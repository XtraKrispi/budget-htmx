module Html.Partials.Archive where

import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (..), prettyF)
import Database (Archive (..))
import Database.Persist (Entity (..))
import Html.Utils qualified
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Types (EntryType (..), frequencyDisplay)
import Utils qualified

render :: [Entity Archive] -> Html
render archive = Html.div do
  Html.ul
    ! Html.Utils.classes ["space-y-2"]
    $ mconcat (renderArchive <$> archive)

renderArchive :: Entity Archive -> Html
renderArchive (Entity _ archive) = Html.li
  ! Html.Utils.classes
    [ "border"
    , "p-4"
    , "rounded-md"
    , "shadow-sm"
    , "flex"
    , "flex-col"
    , "space-y-2"
    ]
  $ do
    Html.div ! Html.Utils.classes ["flex", "justify-between"] $ do
      Html.span
        ! Html.Utils.classesPred
          [ ("font-bold", True)
          , ("text-green-500", archive.archiveEntryType == Payday)
          , ("text-red-500", archive.archiveEntryType == Expense)
          ]
        $ Html.text archive.archiveDescription
      Html.span
        ! Html.Utils.classesPred
          [ ("hidden", archive.archiveEntryType == Payday)
          , ("font-bold", True)
          ]
        $ Html.text
        $ "$"
        <> prettyF (PrettyCfg 2 (Just ',') '.') archive.archiveAmount
    Html.div ! Html.Utils.classes ["flex", "justify-between"] $ do
      Html.span $ Html.text $ frequencyDisplay archive.archiveFrequency
      Html.span $ Html.text $ T.pack $ Utils.formatDate archive.archiveDate
