module Html.Form.Rendering where

import Data.Maybe (fromMaybe, isJust)
import Html.Form.Types (
  Form,
  FormButton (..),
  FormData (..),
  FormField (..),
  FormInput (..),
  FormSelect (..),
  FormSelectOption (..),
 )
import Html.Utils qualified as Utils
import Text.Blaze.Html5 (Attribute, Html, (!), (!?))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

renderForm :: [Attribute] -> Form -> Html
renderForm attrs form =
  Utils.withAttributes attrs
    $ Html.form
    $ Html.div
    ! Utils.classes ["flex", "flex-col", "space-y-6"]
    $ renderFormData form

renderFormData :: FormData -> Html
renderFormData (Field formField) = renderFormField formField
renderFormData (HBox hs) =
  Html.div
    ! Utils.classes
      [ "flex"
      , "space-x-4"
      ]
    $ mconcat (renderFormData <$> hs)
renderFormData (VBox vs) =
  Html.div
    ! Utils.classes
      [ "flex"
      , "flex-col"
      , "space-y-4"
      ]
    $ mconcat (renderFormData <$> vs)

renderFormField :: FormField -> Html
renderFormField (Input formInput) =
  Html.div
    ! Utils.classes
      [ "relative"
      , "z-0"
      , "w-full"
      , "group"
      ]
    $ do
      Html.input
        ! Attr.type_ formInput.formInputInputType
        !? (isJust formInput.formInputName, Attr.name $ fromMaybe "" formInput.formInputName)
        !? (isJust formInput.formInputId, Attr.id $ fromMaybe "" formInput.formInputId)
        !? (formInput.formInputIsReadonly, Attr.readonly "readonly")
        !? (formInput.formInputIsRequired, Attr.required "required")
        !? (isJust formInput.formInputValue, Attr.value $ fromMaybe "" formInput.formInputValue)
        ! Attr.placeholder ""
        ! Utils.classes
          [ "block"
          , "py-2.5"
          , "px-0"
          , "w-full"
          , "text-sm"
          , "text-gray-900"
          , "bg-transparent"
          , "border-0"
          , "border-b-2"
          , "border-gray-300"
          , "appearance-none"
          , "focus:outline-none"
          , "focus:ring-0"
          , "focus:border-blue-600"
          , "peer"
          ]
      Html.label
        !? (isJust formInput.formInputId, Attr.for $ fromMaybe "" formInput.formInputId)
        ! Utils.classes
          [ "peer-focus:font-medium"
          , "absolute"
          , "text-sm"
          , "text-gray-500"
          , "duration-300"
          , "transform"
          , "-translate-y-6"
          , "scale-75"
          , "top-3"
          , "-z-10"
          , "origin-[0]"
          , "peer-focus:left-0"
          , "peer-focus:text-blue-600"
          , "peer-placeholder-shown:scale-100"
          , "peer-placeholder-shown:translate-y-0"
          , "peer-focus:scale-75"
          , "peer-focus:-translate-y-6"
          ]
        $ formInput.formInputLabel
renderFormField (Select s) =
  Html.div
    $ do
      Html.label
        !? (isJust s.formSelectId, Attr.for $ fromMaybe "" s.formSelectId)
        ! Utils.classes
          [ "block"
          , "mb-2"
          , "text-sm"
          , "font-medium"
          , "text-gray-900"
          ]
        $ s.formSelectLabel
      Html.select
        !? (isJust s.formSelectName, Attr.name $ fromMaybe "" s.formSelectName)
        !? (isJust s.formSelectId, Attr.id $ fromMaybe "" s.formSelectId)
        ! Utils.classes
          [ "bg-gray-50"
          , "border"
          , "border-gray-300"
          , "text-gray-900"
          , "text-sm rounded-lg"
          , "focus:ring-blue-500"
          , "focus:border-blue-500"
          , "block"
          , "w-full"
          , "p-2.5"
          ]
        $ mconcat
          ( ( \o ->
                Html.option
                  ! Attr.value o.formSelectOptionValue
                  !? (o.formSelectOptionSelected, Attr.selected "selected")
                  $ o.formSelectOptionText
            )
              <$> s.formSelectOptions
          )
renderFormField (SubmitButton button) =
  Html.button
    ! Attr.type_ "submit"
    !? (button.formButtonIsDisabled, Attr.disabled "disabled")
    ! button.formButtonAttribute
    ! Utils.classes
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
    $ button.formButtonLabel
renderFormField (OtherButton button) =
  Html.button
    ! Attr.type_ "button"
    !? (button.formButtonIsDisabled, Attr.disabled "disabled")
    ! button.formButtonAttribute
    ! Utils.classes
      [ "py-2.5"
      , "px-5"
      , "mr-2"
      , "mb-2"
      , "text-sm"
      , "font-medium"
      , "text-gray-900"
      , "focus:outline-none"
      , "bg-white"
      , "rounded-lg"
      , "border"
      , "border-gray-200"
      , "hover:bg-gray-100"
      , "hover:text-blue-700"
      , "focus:z-10"
      , "focus:ring-4"
      , "focus:ring-gray-200"
      , "transition-colors"
      ]
    $ button.formButtonLabel