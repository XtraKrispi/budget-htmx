module Html.Partials.Entries (render, entryForm) where

import Data.Text qualified as T
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Database (Entry (..))
import Database.Persist (Entity (..))
import Html.Form (FormButton (..), FormInput (..), FormSelect (..), FormSelectOption (..), input, otherButton, renderForm, select, submitButton)
import Html.Form.Types (FormData (..))
import Html.Utils qualified
import Htmx.Attributes qualified as Htmx
import Text.Blaze.Html5 (Html, ToValue (toValue), stringValue, textValue, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import Types (EntryType (..), Frequency (..), entryTypeDisplay, frequencyDisplay)
import Utils qualified

renderEntry :: Entity Entry -> Html
renderEntry (Entity key entry) = Html.li
  ! Html.Utils.classes
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
        <> Html.Utils.keyAttribute key
    )
  ! Htmx.hxTarget "#entry-form"
  ! Htmx.hxSwap "outerHTML"
  $ do
    Html.div ! Html.Utils.classes ["flex", "justify-between"] $ do
      Html.span
        ! Html.Utils.classesPred
          [ ("font-bold", True)
          , ("text-green-500", entry.entryEntryType == Payday)
          , ("text-red-500", entry.entryEntryType == Expense)
          ]
        $ Html.text entry.entryDescription
      Html.span
        ! Html.Utils.classesPred
          [ ("hidden", entry.entryEntryType == Payday)
          ]
        $ Html.text
        $ "$"
        <> prettyF (PrettyCfg 2 (Just ',') '.') entry.entryAmount
    Html.div ! Html.Utils.classes ["flex", "justify-between"] $ do
      Html.span $ Html.text $ frequencyDisplay entry.entryFrequency
      Html.span $ Html.text $ T.pack $ Utils.formatDate entry.entryStartDate
    Html.div $ do
      Html.button
        ! Html.Utils.classes
          [ "focus:outline-none"
          , "text-white"
          , "bg-red-700"
          , "hover:bg-red-800"
          , "focus:ring-4"
          , "focus:ring-red-300"
          , "font-medium"
          , "rounded-lg"
          , "text-sm"
          , "px-5"
          , "py-2.5"
          , "mr-2"
          , "mb-2"
          , "transition-colors"
          ]
        ! Htmx.hxDelete ("/entries/" <> Html.Utils.keyAttribute key)
        ! Htmx.hxTrigger "click consume"
        ! Htmx.hxTarget "#entries"
        $ "Delete"

render :: [Entity Entry] -> Html
render entries =
  Html.div
    ! Attr.id "entries"
    $ Html.ul
    ! Html.Utils.classes ["space-y-4"]
    $ mconcat
    $ renderEntry
    <$> entries

entryForm :: Maybe (Entity Entry) -> Html
entryForm mEntry =
  renderForm
    [ maybe
        (Htmx.hxPost "/entries")
        (\e -> Htmx.hxPut $ "/entries/" <> Html.Utils.keyAttribute (entityKey e))
        mEntry
    , Htmx.hxSwap "outerHTML"
    , Attr.id "entry-form"
    ]
    ( VBox
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
                      . Utils.formatDate
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
        , HBox
            [ submitButton (FormButton "Save" False mempty)
            , otherButton (FormButton "Clear" False (Htmx.hxGet "/partial/entries/new-form" <> Htmx.hxTarget "#entry-form"))
            ]
        ]
    )
