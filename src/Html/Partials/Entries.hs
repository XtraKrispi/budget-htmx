module Html.Partials.Entries where

import Database (Entry (..))
import Database.Persist (Entity (..))
import Html.Form (FormInput (..), FormSelect (..), FormSelectOption (..), FormSubmit (..), input, renderForm, select, submitButton)
import Htmx.Attributes qualified as Htmx
import Text.Blaze.Html5 (Html, ToValue (toValue), toMarkup, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import Types (EntryType (..), Frequency (..), entryTypeDisplay, frequencyDisplay)

render :: [Entity Entry] -> Html
render entries =
  let e = mconcat $ (\(Entity _ entry) -> Html.li $ toMarkup entry.entryDescription) <$> entries
   in Html.div
        ! Attr.id "entries"
        ! Htmx.hxSwapOob
        $ Html.ul e

newEntryForm :: Html
newEntryForm =
  renderForm
    [Htmx.hxPost "/partial/entries", Htmx.hxSwap "outerHTML"]
    [ input
        ( FormInput
            { formInputInputType = "text"
            , formInputId = Nothing
            , formInputName = Just "new-description"
            , formInputLabel = "Description"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            }
        )
    , input
        ( FormInput
            { formInputInputType = "text"
            , formInputId = Nothing
            , formInputName = Just "new-amount"
            , formInputLabel = "Amount"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            }
        )
    , input
        ( FormInput
            { formInputInputType = "date"
            , formInputId = Nothing
            , formInputName = Just "new-start-date"
            , formInputLabel = "Start Date"
            , formInputIsReadonly = False
            , formInputIsRequired = True
            }
        )
    , select
        ( FormSelect
            { formSelectId = Nothing
            , formSelectName = Just "new-frequency"
            , formSelectLabel = "Frequency"
            , formSelectOptions =
                ( \f ->
                    FormSelectOption
                      { formSelectOptionValue = toValue $ show f
                      , formSelectOptionText = frequencyDisplay f
                      }
                )
                  <$> [OneTime .. Monthly]
            }
        )
    , select
        ( FormSelect
            { formSelectId = Nothing
            , formSelectName = Just "new-entry-type"
            , formSelectLabel = "Entry Type"
            , formSelectOptions =
                ( \e ->
                    FormSelectOption
                      { formSelectOptionValue = entryTypeDisplay e
                      , formSelectOptionText = entryTypeDisplay e
                      }
                )
                  <$> [Expense, Payday]
            }
        )
    , submitButton (FormSubmit "Save" False)
    ]
