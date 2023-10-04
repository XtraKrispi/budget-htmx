module Html.Partials.Entries (render, entryForm) where

import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Database (Entry (..))
import Database.Persist (Entity (..))
import Html.Form (FormInput (..), FormSelect (..), FormSelectOption (..), FormSubmit (..), input, renderForm, select, submitButton)
import Html.Utils qualified as Utils
import Htmx.Attributes (hxDelete)
import Htmx.Attributes qualified as Htmx
import Text.Blaze.Html5 (Html, ToValue (toValue), stringValue, textValue, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import Types (EntryType (..), Frequency (..), entryTypeDisplay, frequencyDisplay)

renderEntry :: Entity Entry -> Html
renderEntry (Entity key entry) = Html.li
  ! Utils.classes
    [ "border"
    , "p-4"
    , "rounded-md"
    , "shadow-sm"
    , "flex"
    , "flex-col"
    , "space-y-2"
    , "hover:bg-gray-300"
    , "hover:cursor-pointer"
    ]
  ! Htmx.hxGet
    ( "/partial/entries/"
        <> Utils.keyAttribute key
    )
  ! Htmx.hxTarget "#entry-form"
  ! Htmx.hxSwap "outerHTML"
  $ do
    Html.div ! Utils.classes ["flex", "justify-between"] $ do
      Html.span
        ! Utils.classesPred
          [ ("font-bold", True)
          , ("text-green-500", entry.entryEntryType == Payday)
          , ("text-red-500", entry.entryEntryType == Expense)
          ]
        $ Html.text entry.entryDescription
      Html.span
        ! Utils.classesPred
          [ ("hidden", entry.entryEntryType == Payday)
          ]
        $ Html.text
        $ "$"
        <> prettyF (PrettyCfg 2 (Just ',') '.') entry.entryAmount
    Html.div ! Utils.classes ["flex", "justify-between"] $ do
      Html.span $ Html.text $ frequencyDisplay entry.entryFrequency
      Html.span $ Html.text $ T.pack $ formatShow iso8601Format entry.entryStartDate
    Html.div $ do
      Html.button ! hxDelete ("/partial/entries/" <> Utils.keyAttribute key) $ "Delete"

render :: [Entity Entry] -> Html
render entries =
  Html.div
    ! Attr.id "entries"
    ! Htmx.hxSwapOob
    $ Html.ul
    ! Utils.classes ["space-y-4"]
    $ mconcat
    $ renderEntry
    <$> entries

entryForm :: Maybe (Entity Entry) -> Html
entryForm mEntry =
  renderForm
    [ maybe
        (Htmx.hxPost "/partial/entries")
        (\e -> Htmx.hxPut $ "/partial/entries/" <> Utils.keyAttribute (entityKey e))
        mEntry
    , Htmx.hxSwap "outerHTML"
    , Attr.id "entry-form"
    ]
    [ input
        ( FormInput
            { formInputInputType = "text"
            , formInputId = Just "new-description"
            , formInputName = Just "new-description"
            , formInputLabel = "Description"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            , formInputValue =
                textValue
                  . (.entryDescription)
                  . entityVal
                  <$> mEntry
            }
        )
    , input
        ( FormInput
            { formInputInputType = "text"
            , formInputId = Just "new-amount"
            , formInputName = Just "new-amount"
            , formInputLabel = "Amount"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            , formInputValue =
                stringValue
                  . show
                  . (.entryAmount)
                  . entityVal
                  <$> mEntry
            }
        )
    , input
        ( FormInput
            { formInputInputType = "date"
            , formInputId = Just "new-start-date"
            , formInputName = Just "new-start-date"
            , formInputLabel = "Start Date"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            , formInputValue =
                stringValue
                  . formatShow iso8601Format
                  . (.entryStartDate)
                  . entityVal
                  <$> mEntry
            }
        )
    , select
        ( FormSelect
            { formSelectId = Just "new-frequency"
            , formSelectName = Just "new-frequency"
            , formSelectLabel = "Frequency"
            , formSelectOptions =
                ( \f ->
                    FormSelectOption
                      { formSelectOptionValue = toValue $ show f
                      , formSelectOptionText = frequencyDisplay f
                      , formSelectOptionSelected = maybe False ((== f) . (.entryFrequency) . entityVal) mEntry
                      }
                )
                  <$> [OneTime .. Monthly]
            }
        )
    , select
        ( FormSelect
            { formSelectId = Just "new-entry-type"
            , formSelectName = Just "new-entry-type"
            , formSelectLabel = "Entry Type"
            , formSelectOptions =
                ( \e ->
                    FormSelectOption
                      { formSelectOptionValue = entryTypeDisplay e
                      , formSelectOptionText = entryTypeDisplay e
                      , formSelectOptionSelected = maybe False ((== e) . (.entryEntryType) . entityVal) mEntry
                      }
                )
                  <$> [Expense, Payday]
            }
        )
    , submitButton (FormSubmit "Save" False)
    ]
