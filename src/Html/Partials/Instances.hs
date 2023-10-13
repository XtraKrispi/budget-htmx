module Html.Partials.Instances where

import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (..), prettyF)
import Html.Utils qualified
import Htmx.Attributes qualified as Htmx
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Types (ArchiveAction (Paid, Skipped), EntryType (..), Instance (..), frequencyDisplay)
import Utils qualified

render :: [Instance] -> Html
render instances = Html.div do
  Html.ul
    ! Html.Utils.classes ["space-y-2"]
    $ mconcat (renderInstance <$> instances)

renderInstance :: Instance -> Html
renderInstance i = Html.li
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
          , ("text-green-500", i.instanceEntryType == Payday)
          , ("text-red-500", i.instanceEntryType == Expense)
          ]
        $ Html.text i.instanceDescription
      Html.span
        ! Html.Utils.classesPred
          [ ("hidden", i.instanceEntryType == Payday)
          , ("font-bold", True)
          ]
        $ Html.text
        $ "$"
        <> prettyF (PrettyCfg 2 (Just ',') '.') i.instanceAmount
    Html.div ! Html.Utils.classes ["flex", "justify-between"] $ do
      Html.span $ Html.text $ frequencyDisplay i.instanceFrequency
      Html.span $ Html.text $ T.pack $ Utils.formatDate i.instanceDate
    Html.div $ do
      if i.instanceEntryType == Payday
        then pure ()
        else
          Html.button
            ! Html.Utils.classes
              [ "text-white"
              , "bg-blue-700"
              , "hover:bg-blue-800"
              , "focus:ring-4"
              , "focus:ring-blue-300"
              , "font-medium"
              , "rounded-lg"
              , "text-sm"
              , "px-5"
              , "py-2.5"
              , "mr-2"
              , "mb-2"
              , "focus:outline-none"
              , "transition-colors"
              ]
            ! Htmx.hxPost "/archive"
            ! Htmx.hxVals
              [ ("entryId", Utils.tShow i.instanceEntryId)
              , ("action", Utils.tShow Paid)
              , ("date", T.pack $ Utils.formatDate i.instanceDate)
              ]
            $ "Pay"
      if i.instanceEntryType == Payday
        then pure ()
        else
          Html.button
            ! Htmx.hxPost "/archive"
            ! Htmx.hxVals
              [ ("entryId", Utils.tShow i.instanceEntryId)
              , ("action", Utils.tShow Skipped)
              , ("date", T.pack $ Utils.formatDate i.instanceDate)
              ]
            ! Html.Utils.classes
              [ "text-white"
              , "bg-green-700"
              , "hover:bg-green-800"
              , "focus:ring-4"
              , "focus:ring-green-300"
              , "font-medium"
              , "rounded-lg"
              , "text-sm"
              , "px-5"
              , "py-2.5"
              , "mr-2"
              , "mb-2"
              , "focus:outline-none"
              , "transition-colors"
              ]
            $ "Skip"
